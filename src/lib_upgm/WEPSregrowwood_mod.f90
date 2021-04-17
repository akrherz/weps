!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSregrowwood_mod
  use Preprocess_mod
  use constants, only: dp, check_return, u_mgtokg
  use plant_mod
  use WEPSCrop_util_mod, only: shootnum, shoot_delay, shoot_flg, per_release, stage_release
  implicit none

  type, extends(preprocess) :: WEPSregrowwood
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => regrowwood_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSregrowwood

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPSregrowwood), intent(inout) :: self
      type(hash_state), intent(inout) :: processState
      ! Body of loadState
      ! load processState into my state:
      self%processState = hash_state()
      call self%processState%init()
      call self%processState%clone(processState)
    end subroutine load_state

    subroutine proc_register(self, req_input, prod_output)
      ! Variables
      implicit none
      class(WEPSregrowwood), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine regrowwood_proc(self, plnt, env)
      implicit none
      class(WEPSregrowwood), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.
      integer(int32) :: lay ! soil layer loop index

      ! plant database
      real(dp) :: bcdpop ! Number of plants per unit area (#/m^2)
                     ! Note: bcdstm/bcdpop gives the number of stems per plant
      real(dp) :: bc0shoot ! mass from root storage required for each shoot (mg/shoot)
      real(dp) :: bcdmaxshoot ! maximum number of shoots possible from each plant
      real(dp) :: bc0hue ! relative heat unit for emergence (fraction)
      real(dp) :: bczloc_regrow ! location of regrowth point (+ on stem, 0 or negative from crown at or below surface) (m)

      ! environment
      integer(int32) :: bnslay ! number of soil layers

      ! plant state
      real(dp) :: bcmstandstem ! crop standing stem mass (kg/m^2)
      real(dp) :: bcmstandleaflive ! crop live standing leaf mass (kg/m^2)
      real(dp) :: bcmstandleafdead ! crop dead standing leaf mass (kg/m^2)
      real(dp) :: bcmstandstore ! crop standing storage mass (kg/m^2) (head with seed, or vegetative head (cabbage, pineapple))
      real(dp) :: bcmflatstem  ! crop flat stem mass (kg/m^2)
      real(dp) :: bcmflatleaf  ! crop flat leaf mass (kg/m^2)
      real(dp) :: bcmflatstore ! crop flat storage mass (kg/m^2)
      real(dp) :: bcmshoot ! crop shoot mass grown from root storage (kg/m^2)
                           ! this is a "breakout" mass and does not represent a unique pool
                           ! since this mass is destributed into below ground stem and
                           ! standing stem as each increment of the shoot is added
      real(dp) :: bcmtotshoot ! total mass released from root storage biomass (kg/m^2)
                              ! in the period from beginning to completion of emegence heat units
      real(dp), dimension(:), allocatable :: bcmbgstemz ! crop stem mass below soil surface by layer (kg/m^2)
      real(dp), dimension(:), allocatable :: bcmrootstorez ! crop root storage mass by soil layer (kg/m^2)
                                                       ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      real(dp) :: bczht  ! Crop height (m)
      real(dp) :: bprevht ! Crop height (m) from previous day
      real(dp) :: bcdstm ! Number of crop stems per unit area (#/m^2)
      real(dp) :: bcthu_shoot_beg ! heat unit index (fraction) for beginning of shoot grow from root storage period
      real(dp) :: bcthu_shoot_end ! heat unit index (fraction) for end of shoot grow from root storage period
      real(dp) :: bcgrainf ! internally computed grain fraction of reproductive mass
      real(dp) :: bctwarmdays ! number of consecutive days that the temperature has been above the minimum growth temperature
      ! above ground residue from plant being forced to regrow (cutting, defoliation)
      real(dp) :: bgmstandstem
      real(dp) :: bgmstandleaf
      real(dp) :: bgmstandstore
      real(dp) :: bgmflatstem
      real(dp) :: bgmflatleaf
      real(dp) :: bgmflatstore
      real(dp), dimension(:), allocatable :: bgmbgstemz
      real(dp) :: bggrainf
      real(dp) :: bgzht
      real(dp) :: bgdstm
      logical :: shoot_growing ! flag set to indicate that shoot growth is occuring
      logical :: can_regrow ! flag set to indicate that crop is able to regrow (past bc0hue, partition to root store)
      logical :: do_regrow  ! flag set to indicate that regrow has been triggered

      ! locally computed values
      real(dp) :: root_store_rel ! root storage which could be released for regrowth
      real(dp) :: pot_stems ! potential number of stems which could be released for regrowth
      real(dp) :: pot_leaf_mass ! potential leaf mass which could be released for regrowth.
      integer(int32) :: regrowth_flg

      ! Body of regrowwood

      ! retrieve required inputs

      ! plant state
      call plnt%state%get("can_regrow", can_regrow, succ)
      if( .not. check_return( trim(self%processName) , "can_regrow", succ ) ) return

      ! check for regrowth from a stump
      regrowth_flg = 0
      do_regrow = .false.
      if( can_regrow ) then
        ! Stump regrowth is possible

        ! plant state
        call plnt%state%get("prevheight", bprevht, succ)
        if( .not. check_return( trim(self%processName) , "prevheight", succ ) ) return

        if( bczht .lt. (0.1*bprevht) ) then
          ! tree has been cut close to the ground

          ! plant state
          call plnt%state%get("warmdays", bctwarmdays, succ)
          if( .not. check_return( trim(self%processName) , "warmdays", succ ) ) return

          regrowth_flg = 1
          if( bctwarmdays .ge. shoot_delay ) then
            ! enough warm days to start regrowth

            ! plant state
            call plnt%state%get("shoot_growing", shoot_growing, succ)
            if( .not. check_return( trim(self%processName) , "shoot_growing", succ ) ) return

            regrowth_flg = 2
            if( .not. shoot_growing ) then
              ! heat units past emergence

              ! plant database
              call plnt%pars%get("plantpop", bcdpop, succ)
              if( .not. check_return( trim(self%processName) , "plantpop", succ ) ) return
              call plnt%pars%get("regrmshoot", bc0shoot, succ)
              if( .not. check_return( trim(self%processName) , "regrmshoot", succ ) ) return
              call plnt%pars%get("dmaxshoot", bcdmaxshoot, succ)
              if( .not. check_return( trim(self%processName) , "dmaxshoot", succ ) ) return
              call plnt%pars%get("huie", bc0hue, succ)
              if( .not. check_return( trim(self%processName) , "huie", succ ) ) return
              call plnt%pars%get("zloc_regrow", bczloc_regrow, succ)
              if( .not. check_return( trim(self%processName) , "zloc_regrow", succ ) ) return

              ! plant state
              call plnt%state%get("mrootstorez", bcmrootstorez, succ)
              if( .not. check_return( trim(self%processName) , "mrootstorez", succ ) ) return
              bnslay = size(bcmrootstorez)

              regrowth_flg = 3
              ! find out how much root store can be released for regrowth
              call shootnum(shoot_flg, bnslay, 0.9d0, bcdpop, bc0shoot, &
                 bcdmaxshoot, root_store_rel, bcmrootstorez, pot_stems)
              ! reset growth clock 
              do_regrow = .true.
              bcthu_shoot_beg = 0.0d0
              bcthu_shoot_end = bc0hue
              ! reset shoot grow configuration
              if ( bczloc_regrow .le. 0.0 ) then
                ! regrows from crown, stem becomes residue

                ! plant state
                call plnt%state%get("mstandstem", bcmstandstem, succ)
                if( .not. check_return( trim(self%processName) , "mstandstem", succ ) ) return
                call plnt%state%get("mstandleaflive", bcmstandleaflive, succ)
                if( .not. check_return( trim(self%processName) , "mstandleaflive", succ ) ) return
                call plnt%state%get("mstandleafdead", bcmstandleafdead, succ)
                if( .not. check_return( trim(self%processName) , "mstandleafdead", succ ) ) return
                call plnt%state%get("mstandstore", bcmstandstore, succ)
                if( .not. check_return( trim(self%processName) , "mstandstore", succ ) ) return
                call plnt%state%get("mflatstem", bcmflatstem, succ)
                if( .not. check_return( trim(self%processName) , "mflatstem", succ ) ) return
                call plnt%state%get("mflatleaf", bcmflatleaf, succ)
                if( .not. check_return( trim(self%processName) , "mflatleaf", succ ) ) return
                call plnt%state%get("mflatstore", bcmflatstore, succ)
                if( .not. check_return( trim(self%processName) , "mflatstore", succ ) ) return
                call plnt%state%get("mbgstemz", bcmbgstemz, succ)
                if( .not. check_return( trim(self%processName) , "mbgstemz", succ ) ) return
                call plnt%state%get("grainf", bcgrainf, succ)
                if( .not. check_return( trim(self%processName) , "grainf", succ ) ) return
                call plnt%state%get("height", bczht, succ)
                if( .not. check_return( trim(self%processName) , "height", succ ) ) return
                call plnt%state%get("dstm", bcdstm, succ)
                if( .not. check_return( trim(self%processName) , "dstm", succ ) ) return

                bgmstandstem = bcmstandstem
                bgmstandleaf = bcmstandleaflive + bcmstandleafdead
                bgmstandstore = bcmstandstore
                bgmflatstem = bcmflatstem
                bgmflatleaf = bcmflatleaf
                bgmflatstore = bcmflatstore
                do lay = 1, bnslay
                    bgmbgstemz(lay) = bcmbgstemz(lay)
                end do
                bggrainf = bcgrainf
                bgzht = bczht
                bgdstm = bcdstm
                ! reset crop values to indicate new growth cycle
                bcmstandstem = 0.0
                bcmstandleaflive = 0.0
                bcmstandleafdead = 0.0
                bcmstandstore = 0.0
                bcmflatstem = 0.0
                bcmflatleaf = 0.0
                bcmflatstore = 0.0
                do lay = 1, bnslay
                    bcmbgstemz(lay) = 0.0
                end do
                bcgrainf = 0.0
                bczht = 0.0

                call plnt%state%replace("res_standstem", bgmstandstem, succ)
                if( .not. check_return( trim(self%processName) , "res_standstem", succ ) ) return
                call plnt%state%replace("res_standleaf", bgmstandleaf, succ)
                if( .not. check_return( trim(self%processName) , "res_standleaf", succ ) ) return
                call plnt%state%replace("res_standstore", bgmstandstore, succ)
                if( .not. check_return( trim(self%processName) , "res_standstore", succ ) ) return
                call plnt%state%replace("res_flatstem", bgmflatstem, succ)
                if( .not. check_return( trim(self%processName) , "res_flatstem", succ ) ) return
                call plnt%state%replace("res_flatleaf", bgmflatleaf, succ)
                if( .not. check_return( trim(self%processName) , "res_flatleaf", succ ) ) return
                call plnt%state%replace("res_flatstore", bgmflatstore, succ)
                if( .not. check_return( trim(self%processName) , "res_flatstore", succ ) ) return
                call plnt%state%replace("res_bgstemz", bgmbgstemz, succ)
                if( .not. check_return( trim(self%processName) , "res_bgstemz", succ ) ) return
                call plnt%state%replace("res_grainf", bggrainf, succ)
                if( .not. check_return( trim(self%processName) , "res_grainf", succ ) ) return
                call plnt%state%replace("res_zht", bgzht, succ)
                if( .not. check_return( trim(self%processName) , "res_zht", succ ) ) return
                call plnt%state%replace("res_dstm", bgdstm, succ)
                if( .not. check_return( trim(self%processName) , "res_dstm", succ ) ) return

                call plnt%state%replace("mstandstem", bcmstandstem, succ)
                if( .not. check_return( trim(self%processName) , "mstandstem", succ ) ) return
                call plnt%state%replace("mstandleaflive", bcmstandleaflive, succ)
                if( .not. check_return( trim(self%processName) , "mstandleaflive", succ ) ) return
                call plnt%state%replace("mstandleafdead", bcmstandleafdead, succ)
                if( .not. check_return( trim(self%processName) , "mstandleafdead", succ ) ) return
                call plnt%state%replace("mstandstore", bcmstandstore, succ)
                if( .not. check_return( trim(self%processName) , "mstandstore", succ ) ) return
                call plnt%state%replace("mflatstem", bcmflatstem, succ)
                if( .not. check_return( trim(self%processName) , "mflatstem", succ ) ) return
                call plnt%state%replace("mflatleaf", bcmflatleaf, succ)
                if( .not. check_return( trim(self%processName) , "mflatleaf", succ ) ) return
                call plnt%state%replace("mflatstore", bcmflatstore, succ)
                if( .not. check_return( trim(self%processName) , "mflatstore", succ ) ) return
                call plnt%state%replace("mbgstemz", bcmbgstemz, succ)
                if( .not. check_return( trim(self%processName) , "mbgstemz", succ ) ) return
                call plnt%state%replace("grainf", bcgrainf, succ)
                if( .not. check_return( trim(self%processName) , "grainf", succ ) ) return
                call plnt%state%replace("height", bczht, succ)
                if( .not. check_return( trim(self%processName) , "height", succ ) ) return

              end if

              ! if it regrows from stem, stem does not become residue
              ! starting mass of shoot is always zero
              bcmshoot = 0.0

              bcmtotshoot = root_store_rel
              bcdstm = pot_stems

              ! update values for return
              call plnt%state%replace("thu_shoot_beg", bcthu_shoot_beg, succ)
              if( .not. check_return( trim(self%processName) , "thu_shoot_beg", succ ) ) return
              call plnt%state%replace("thu_shoot_end", bcthu_shoot_end, succ)
              if( .not. check_return( trim(self%processName) , "thu_shoot_end", succ ) ) return
              call plnt%state%replace("masshoot", bcmshoot, succ)
              if( .not. check_return( trim(self%processName) , "masshoot", succ ) ) return
              call plnt%state%replace("mtotshoot", bcmtotshoot, succ)
              if( .not. check_return( trim(self%processName) , "mtotshoot", succ ) ) return
              call plnt%state%replace("dstm", bcdstm, succ)
              if( .not. check_return( trim(self%processName) , "dstm", succ ) ) return

            end if
          end if
        end if
      end if

      ! update plant state values
      call plnt%state%replace("regrowth_flg", regrowth_flg, succ)
      if( .not. check_return( trim(self%processName) , "regrowth_flg", succ ) ) return
      call plnt%state%replace("do_regrow", do_regrow, succ)
      if( .not. check_return( trim(self%processName) , "do_regrow", succ ) ) return

    end subroutine regrowwood_proc

end module WEPSregrowwood_mod
