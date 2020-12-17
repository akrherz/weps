!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSDeciduousWood_mod
    use phases_mod
    use constants, only: dp, int32, check_return, u_max_arg_exp, u_max_real
    use WEPSCrop_util_mod, only: total_leaf
   implicit none

    type, extends(phase) :: WEPS_DeciduousWood
    contains
    procedure, pass(self) :: load => dummyload
    procedure, pass(self) :: doPhase => deciduouswood ! may not need to pass self
    procedure, pass(self) :: register => dummyregister
    end type WEPS_DeciduousWood

  contains

    subroutine dummyload(self, phaseState)
      implicit none
      class(WEPS_DeciduousWood), intent(inout) :: self
      type(hash_state), intent(inout) :: phaseState
    end subroutine dummyload

    subroutine dummyregister(self, req_input, prod_output)
      implicit none
      class(WEPS_DeciduousWood), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
    end subroutine dummyregister

    subroutine deciduouswood(self, plnt, env)
      implicit none
      class(WEPS_DeciduousWood), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.

      ! phase parmeter
      real(dp) :: leafhuie ! relative heat unit for leaf emergence (fraction)

      ! plant database
      real(dp) :: bcthum ! potential heat units for crop maturity (deg. C)
      real(dp) :: alf  ! leaf partitioning s-curve coefficient a
      real(dp) :: blf  ! leaf partitioning s-curve coefficient b
      real(dp) :: clf  ! leaf partitioning s-curve coefficient c
      real(dp) :: dlf  ! leaf partitioning s-curve coefficient d
      real(dp) :: arp  ! reproductive partitioning s-curve coefficient a
      real(dp) :: brp  ! reproductive partitioning s-curve coefficient b
      real(dp) :: crp  ! reproductive partitioning s-curve coefficient c
      real(dp) :: drp  ! reproductive partitioning s-curve coefficient d
      real(dp) :: aht  ! height (and rooting depth) s-curve coefficient a
      real(dp) :: bht  ! height (and rooting depth) s-curve coefficient b
      real(dp) :: zmxc ! maximum plant height
      real(dp) :: zmrt ! maximum plant rooting depth
      real(dp) :: ehu0 ! heat unit fraction where senescence starts

      ! environment
      integer(int32) :: bnslay ! number of soil layers

      ! plant state
      real(dp) :: bcmtotleaf  ! total mass released from root storage biomass (kg/m^2)
                              ! in the period from beginning to completion of leaf emergence heat units
      real(dp), dimension(:), allocatable :: bcmrootstorez ! crop root storage mass by soil layer (kg/m^2)
                                                       ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      real(dp) :: bcdstm ! Number of crop stems per unit area (#/m^2)
      real(dp) :: bctrthucum ! accumulated root growth heat units (degree-days)
      real(dp) :: bcthu_leaf_beg
      real(dp) :: bcthu_leaf_end
      real(dp) :: bcthu_shoot_beg ! heat unit index (fraction) for beginning of shoot grow from root storage period
      real(dp) :: bcthu_shoot_end ! heat unit index (fraction) for end of shoot grow from root storage period
      real(dp) :: bctcolddays ! number of days that the daily average temperature has been below the minimum growth temperature with decay
      real(dp) :: bctwarmdays ! number of consecutive days that the temperature has been above the minimum growth temperature
      logical :: can_regrow ! flag set to indicate that crop is able to regrow
      logical :: do_leafon
      logical :: do_leafoff

      ! stage state
      real(dp) :: bcthucum ! plant accumulated heat units (degree-days)
      real(dp) :: daygdd   ! plant heat units for this day (degree-days)

      ! locally computed values
      real(dp) :: hui          ! heat unit index (ratio of acthucum to acthum)
      real(dp) :: huiy         ! heat unit index (ratio of acthucum to acthum) on day (i-1)
      real(dp) :: huirt        ! heat unit index for root expansion (ratio of actrthucum to acthum)
      real(dp) :: huirty       ! heat unit index for root expansion (ratio of actrthucum to acthum) on day (i-1)
      real(dp) :: ffa  ! leaf senescence factor (ratio)
      real(dp) :: ffw  ! leaf weight reduction factor (ratio)
      real(dp) :: ffr  ! root weight reduction factor (ratio)
      real(dp) :: gif  ! grain index accounting for development of chaff before grain fill
      real(dp) :: shoot_hui    ! today fraction of heat unit shoot growth index accumulation
      real(dp) :: shoot_huiy   ! previous day fraction of heat unit shoot growth index accumulation
      real(dp) :: leaf_hui    ! today fraction of heat unit leaf growth index accumulation
      real(dp) :: leaf_huiy   ! previous day fraction of heat unit leaf growth index accumulation
      real(dp) :: p_rw ! fibrous root partitioning ratio
      real(dp) :: p_st ! stem partitioning ratio
      real(dp) :: p_lf ! leaf partitioning ratio
      real(dp) :: p_rp ! reproductive partitioning ratio
      real(dp) :: pdht ! increment in potential height (m)'
      real(dp) :: pdrd ! potential increment in root length (m)
      real(dp) :: arg_exp    ! argument calculated for exponential function (to test for validity)
      real(dp) :: p_lf_rp    ! sum of leaf and reproductive partitioning fractions
      real(dp) :: huf        ! heat unit factor for driving root depth, plant height development
      real(dp) :: hufy       ! value of huf on day (i-1)
      real(dp) :: pchty      ! potential plant height from previous day
      real(dp) :: pcht       ! potential plant height for today
      real(dp) :: prdy       ! potential root depth from previous day
      real(dp) :: prd        ! potential root depth today
      real(dp) :: hui0f      ! relative gdd at start of scenescence
      real(dp) :: ff         ! senescence factor (ratio)
      real(dp) :: hux        ! relative gdd offset to start at scenescence

      integer(int32) :: tmp
      integer(int32), parameter :: winter_ann_root = 1
      logical :: lastday

      ! Body of deciduouswood

      ! retrieve required inputs
      ! plant database
      call plnt%pars%get("thum", bcthum, succ)
      if( .not. check_return( trim(self%phaseName) , "thum", succ ) ) return
      call plnt%pars%get("alf", alf, succ)
      if( .not. check_return( trim(self%phaseName) , "alf", succ ) ) return
      call plnt%pars%get("blf", blf, succ)
      if( .not. check_return( trim(self%phaseName) , "blf", succ ) ) return
      call plnt%pars%get("clf", clf, succ)
      if( .not. check_return( trim(self%phaseName) , "clf", succ ) ) return
      call plnt%pars%get("dlf", dlf, succ)
      if( .not. check_return( trim(self%phaseName) , "dlf", succ ) ) return
      call plnt%pars%get("arp", arp, succ)
      if( .not. check_return( trim(self%phaseName) , "arp", succ ) ) return
      call plnt%pars%get("brp", brp, succ)
      if( .not. check_return( trim(self%phaseName) , "brp", succ ) ) return
      call plnt%pars%get("crp", crp, succ)
      if( .not. check_return( trim(self%phaseName) , "crp", succ ) ) return
      call plnt%pars%get("drp", drp, succ)
      if( .not. check_return( trim(self%phaseName) , "drp", succ ) ) return
      call plnt%pars%get("aht", aht, succ)
      if( .not. check_return( trim(self%phaseName) , "aht", succ ) ) return
      call plnt%pars%get("bht", bht, succ)
      if( .not. check_return( trim(self%phaseName) , "bht", succ ) ) return
      call plnt%pars%get("zmxc", zmxc, succ)
      if( .not. check_return( trim(self%phaseName) , "zmxc", succ ) ) return
      call plnt%pars%get("zmrt", zmrt, succ)
      if( .not. check_return( trim(self%phaseName) , "zmrt", succ ) ) return
      call plnt%pars%get("ehu0", ehu0, succ)
      if( .not. check_return( trim(self%phaseName) , "ehu0", succ ) ) return

      ! environment variables

      ! plant state
      call plnt%state%get("dstm", bcdstm, succ)
      if( .not. check_return( trim(self%phaseName) , "dstm", succ ) ) return
      call plnt%state%get("trthucum", bctrthucum, succ)
      if( .not. check_return( trim(self%phaseName) , "trthucum", succ ) ) return
      call plnt%state%get("can_regrow", can_regrow, succ)
      if( .not. check_return( trim(self%phaseName) , "can_regrow", succ ) ) return
      call plnt%state%get("daygdd", daygdd, succ)
      if( .not. check_return( trim(self%phaseName) , "daygdd", succ ) ) return

      ! stage state
      call self%phaseState%get("stagegdd", bcthucum, succ)
      if( .not. check_return( trim(self%phaseName) , "stagegdd", succ ) ) return

      ! Set growth / no growth conditions depending on leaf actions
      if( can_regrow ) then

        ! plant state
        call plnt%state%get("do_leafon", do_leafon, succ)
        if( .not. check_return( trim(self%phaseName) , "do_leafon", succ ) ) return
        call plnt%state%get("do_leafoff", do_leafoff, succ)
        if( .not. check_return( trim(self%phaseName) , "do_leafoff", succ ) ) return

        if( do_leafon ) then
          ! phase parameter
          call self%phasePars%get("leafhuie", leafhuie, succ)
          if( .not. check_return( trim(self%phaseName) , "leafhuie", succ ) ) return
          ! plant state
          call plnt%state%get("mrootstorez", bcmrootstorez, succ)
          if( .not. check_return( trim(self%phaseName) , "mrootstorez", succ ) ) return
          bnslay = size(bcmrootstorez)

          ! new leaves start to appear
          bcmtotleaf = total_leaf( bnslay, bcmrootstorez )
          ! reset heat units
          bcthucum = 0.0d0
          bcthu_leaf_beg = bcthucum / bcthum
          bcthu_leaf_end = bcthucum + leafhuie
          bctcolddays = 0.0d0

          ! update plant state values
          call plnt%state%replace("colddays", bctcolddays, succ)
          if( .not. check_return( trim(self%phaseName) , "colddays", succ ) ) return

        else if( do_leafoff ) then
          ! leaf drop has occurred
          ! reset heat units to mature
          bcthucum = bcthum
          lastday = .true.  ! this triggers no growth, non-transpiring condition in UPGM like in WEPS crop_growth
          ! reset spring trigger values
          bcmtotleaf = 0.0d0
          bcthu_leaf_beg = 0.0d0
          bcthu_leaf_end = 0.0d0
          ! no shoot grow
          bcthu_shoot_beg = 0.0d0
          bcthu_shoot_end = 0.0d0
          bctwarmdays = 0.0d0

          ! update plant state values
          call plnt%state%replace("lastday", lastday, succ)
          if( .not. check_return( trim(self%phaseName) , "lastday", succ ) ) return
          call plnt%state%replace("thu_shoot_beg", bcthu_shoot_beg, succ)
          if( .not. check_return( trim(self%phaseName) , "thu_shoot_beg", succ ) ) return
          call plnt%state%replace("thu_shoot_end", bcthu_shoot_end, succ)
          if( .not. check_return( trim(self%phaseName) , "thu_shoot_end", succ ) ) return
          call plnt%state%replace("warmdays", bctwarmdays, succ)
          if( .not. check_return( trim(self%phaseName) , "warmdays", succ ) ) return

        end if
        if( do_leafon .or. do_leafoff ) then
          ! update plant state values
          call self%phaseState%replace("stagegdd", bcthucum, succ)
          if( .not. check_return( trim(self%phaseName) , "stagegdd", succ ) ) return
          call plnt%state%replace("mtotleaf", bcmtotleaf, succ)
          if( .not. check_return( trim(self%phaseName) , "mtotleaf", succ ) ) return
          call plnt%state%replace("thu_leaf_beg", bcthu_leaf_beg, succ)
          if( .not. check_return( trim(self%phaseName) , "thu_leaf_beg", succ ) ) return
          call plnt%state%replace("thu_leaf_end", bcthu_leaf_end, succ)
          if( .not. check_return( trim(self%phaseName) , "thu_leaf_end", succ ) ) return

        end if
      end if

      ! accumulate growing degree days
      if( (bcthum .le. 0.0_dp) .or. (bcdstm .le. 0.0_dp) ) then
          ! always keep this invalid plant in first stage growth
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow
          huiy = 0.0_dp
          hui = 0.0_dp
      else
          ! previous day heat unit index
          huiy = min(1.0_dp, bcthucum / bcthum)
          huirty = bctrthucum / bcthum
          ! check for growth completion
          if( huiy .lt. 1.0_dp ) then
              ! accumulate additional for today
              bcthucum = bcthucum + daygdd
              ! root depth growth heat units
              bctrthucum = bctrthucum + daygdd
              ! calculate heat unit index
              hui = min(1.0_dp, bcthucum / bcthum)
              huirt = bctrthucum / bcthum
              tmp = 0  ! clear nextstage, this phase never transitions
              call plnt%state%replace("nextstage", tmp, succ)
              call self%phaseState%replace("stagegdd", bcthucum, succ)
              if( .not. check_return( trim(self%phaseName) , "stagegdd", succ ) ) return

              ! update plant state values
              call plnt%state%replace("trthucum", bctrthucum, succ)
              if( .not. check_return( trim(self%phaseName) , "trthucum", succ ) ) return

          else
              hui = huiy
              huirt = huirty
          end if

      end if

      ! find partitioning between fibrous roots and all other biomass
      ! root partition done using root heat unit index, which is not reset
      ! when a harvest removes all the leaves. This index also is not delayed
      ! in prevernalization winter annuals. Made to parallel winter annual
      ! rooting depth flag as well.
      if( winter_ann_root .eq. 0 ) then
          p_rw = (0.4_dp-0.2_dp*hui)
      else
          p_rw = max(0.05_dp, (0.4_dp-0.2_dp*huirt) )
      end if

      ! find partitioning factors of the remaining biomass (not fibrous root)
      ! calculate leaf partitioning.
      arg_exp = -(hui - clf) / dlf
      if( arg_exp .ge. u_max_arg_exp ) then
          p_lf = alf + blf / u_max_real
      else
          p_lf = alf + blf / (1.0_dp + exp(arg_exp))
      end if
      p_lf = max( 0.0_dp, min( 1.0_dp, p_lf ))

      ! calculate reproductive partitioning based on partioning curve
      arg_exp = -(hui - crp) / drp
      if( arg_exp .ge. u_max_arg_exp ) then
          p_rp = arp + brp / u_max_real
      else
          p_rp = arp + brp / (1.0_dp + exp(arg_exp))
      end if
      p_rp = max( 0.0_dp, min( 1.0_dp, p_rp ))

      ! normalize leaf and reproductive fractions so sum never greater than 1.0
      p_lf_rp = p_lf + p_rp
      if( p_lf_rp .gt. 1.0_dp ) then
          p_lf = p_lf / p_lf_rp
          p_rp = p_rp / p_lf_rp
          ! set stem partitioning parameter.
          p_st = 0.0_dp
      else
          ! set stem partitioning parameter.
          p_st = 1.0_dp - p_lf_rp
      end if

      ! added method (different from EPIC) of calculating plant height
      ! pht=cummulated potential height,pdht=daily potential height
      ! aczht(am0csr) = cummulated actual height
      ! adht=daily actual height, plant%database%aht,plant%database%bht are
      ! height-scurve parameters (formerly lai parameters)
      ! previous day
      hufy = 0.01_dp + 1.0_dp / (1.0_dp + exp((huiy - aht) / bht))
      ! today
      huf = 0.01_dp + 1.0_dp / (1.0_dp + exp((hui - aht) / bht))

      pchty = min(zmxc, zmxc * hufy)
      pcht = min(zmxc, zmxc * huf)
      pdht = pcht - pchty

      ! calculate rooting depth (eq. 2.203) and check that it is not deeper
      ! than the maximum potential depth, and the depth of the root zone.
      ! This change from the EPIC method is undocumented!! It says that root depth
      ! starts at 10cm and increases from there at the rate determined by huf.
      ! the 10 cm assumption was prevously removed from elsewhere in the code
      ! and is subsequently removed here. The initial depth is now set in 
      ! crop record seeding depth, and  the function just increases it.
      ! This is now based on a no delay heat unit accumulation to allow
      ! rapid root depth development by winter annuals.
      if( winter_ann_root .eq. 0 ) then
          prdy = min(zmrt, zmrt * hufy + 0.1_dp)
          prd = min(zmrt, zmrt * huf + 0.1_dp)
      else
          prdy = zmrt *(0.01_dp + 1.0_dp / (1.0_dp + exp((huirty - aht) / bht)))
          prd = zmrt * (0.01_dp + 1.0_dp / (1.0_dp + exp((huirt - aht) / bht)))
      end if
      pdrd = max(0.0_dp, prd - prdy)

      ! senescence is done on a whole plant mass basis not incremental mass
      ! This starts senescence before the entered heat unit index for
      ! the start of senscence. For most leaf partitioning functions
      ! the coefficients draw a curve that approaches 1 around -0.5 but
      ! the value at zero, raised to fractional powers is still very small
      hui0f = ehu0 - ehu0 * 0.1_dp
      if (hui.ge.hui0f) then
          hux = hui - ehu0
          ff = 1.0_dp / (1.0_dp + exp(-(hux - clf / 2.0_dp) / dlf))
          ffa = ff**0.125_dp
          ffw = ff**0.0625_dp
          ffr = 0.98_dp
      else
          ! set a value to be written out
          ffa = 1.0_dp
          ffw = 1.0_dp
          ffr = 1.0_dp
      endif

      ! this factor prorates the grain reproductive fraction (grf) defined
      ! in the database for crop type 1, grains. Compensates for the
      ! development of chaff before grain filling, ie., grain is not
      ! uniformly a fixed fraction of reproductive mass during the entire 
      ! reproductive development stage.
      gif=1.0_dp / (1.0_dp + exp(-(hui - 0.64_dp) / 0.05_dp))

      if( (huiy .lt. 1.0_dp) .and. (bcdstm .gt. 0.0_dp)) then
        ! crop growth not yet complete
        ! stem count can be set to zero by harvest, but not reset by
        ! regrowth early in spring, causing divide by zero in shoot_grow

        if( hui .ge. 1.0_dp ) then
          lastday = .true.
        else
          lastday = .false.
        end if

        ! plant state
        call plnt%state%get("thu_shoot_end", bcthu_shoot_end, succ)
        if( .not. check_return( trim(self%phaseName) , "thu_shoot_end", succ ) ) return

        if( huiy .lt. bcthu_shoot_end ) then

          ! plant state
          call plnt%state%get("thu_shoot_beg", bcthu_shoot_beg, succ)
          if( .not. check_return( trim(self%phaseName) , "thu_shoot_beg", succ ) ) return

          if( hui .gt. bcthu_shoot_beg ) then

            ! fraction of shoot growth from stored reserves (today and yesterday)
            shoot_huiy = max( 0.0_dp, (huiy - bcthu_shoot_beg) / (bcthu_shoot_end - bcthu_shoot_beg) )
            shoot_hui = (hui - bcthu_shoot_beg) / (bcthu_shoot_end - bcthu_shoot_beg)
            if( shoot_hui .ge. 1.0_dp ) then
              ! shoot growth completes on this day
              bcthu_shoot_beg = 0.0d0
              bcthu_shoot_end = 0.0d0

              ! update plant state values
              call plnt%state%replace("thu_shoot_beg", bcthu_shoot_beg, succ)
              if( .not. check_return( trim(self%phaseName) , "thu_shoot_beg", succ ) ) return
              call plnt%state%replace("thu_shoot_end", bcthu_shoot_end, succ)
              if( .not. check_return( trim(self%phaseName) , "thu_shoot_end", succ ) ) return
            end if
            shoot_hui = min( 1.0_dp, shoot_hui )

          else
            shoot_hui = 0.0_dp
            shoot_huiy = 0.0_dp
          end if

        else
          shoot_hui = 1.0_dp
          shoot_huiy = 1.0_dp
        end if

        ! plant state
        call plnt%state%get("thu_leaf_end", bcthu_leaf_end, succ)
        if( .not. check_return( trim(self%phaseName) , "thu_leaf_end", succ ) ) return

        if( huiy .lt. bcthu_leaf_end ) then

          ! plant state
          call plnt%state%get("thu_leaf_beg", bcthu_leaf_beg, succ)
          if( .not. check_return( trim(self%phaseName) , "thu_leaf_beg", succ ) ) return

          if( hui .gt. bcthu_leaf_beg ) then

            ! fraction of leaf growth from stored reserves (today and yesterday)
            leaf_hui = min( 1.0d0, (hui - bcthu_leaf_beg) / (bcthu_leaf_end - bcthu_leaf_beg) )
            leaf_huiy = max( 0.0d0, (huiy - bcthu_leaf_beg) / (bcthu_leaf_end - bcthu_leaf_beg) )

          else
            leaf_hui = 0.0_dp
            leaf_huiy = 0.0_dp
          end if

        else
          leaf_hui = 1.0_dp
          leaf_huiy = 1.0_dp
        end if

        ! update plant state values
        call plnt%state%replace("lastday", lastday, succ)
        if( .not. check_return( trim(self%phaseName) , "lastday", succ ) ) return
        call plnt%state%replace("shoot_hui", shoot_hui, succ)
        if( .not. check_return( trim(self%phaseName) , "shoot_hui", succ ) ) return
        call plnt%state%replace("shoot_huiy", shoot_huiy, succ)
        if( .not. check_return( trim(self%phaseName) , "shoot_huiy", succ ) ) return
        call plnt%state%replace("leaf_hui", leaf_hui, succ)
        if( .not. check_return( trim(self%phaseName) , "leaf_hui", succ ) ) return
        call plnt%state%replace("leaf_huiy", leaf_huiy, succ)
        if( .not. check_return( trim(self%phaseName) , "leaf_huiy", succ ) ) return

      end if

      ! update plant state values
      call plnt%state%replace("ffa", ffa, succ)
      if( .not. check_return( trim(self%phaseName) , "ffa", succ ) ) return
      call plnt%state%replace("ffw", ffw, succ)
      if( .not. check_return( trim(self%phaseName) , "ffw", succ ) ) return
      call plnt%state%replace("ffr", ffr, succ)
      if( .not. check_return( trim(self%phaseName) , "ffr", succ ) ) return
      call plnt%state%replace("gif", gif, succ)
      if( .not. check_return( trim(self%phaseName) , "gif", succ ) ) return
      call plnt%state%replace("p_rw", p_rw, succ)
      if( .not. check_return( trim(self%phaseName) , "p_rw", succ ) ) return
      call plnt%state%replace("p_st", p_st, succ)
      if( .not. check_return( trim(self%phaseName) , "p_st", succ ) ) return
      call plnt%state%replace("p_lf", p_lf, succ)
      if( .not. check_return( trim(self%phaseName) , "p_lf", succ ) ) return
      call plnt%state%replace("p_rp", p_rp, succ)
      if( .not. check_return( trim(self%phaseName) , "p_rp", succ ) ) return
      call plnt%state%replace("pdht", pdht, succ)
      if( .not. check_return( trim(self%phaseName) , "pdht", succ ) ) return
      call plnt%state%replace("pdrd", pdrd, succ)
      if( .not. check_return( trim(self%phaseName) , "pdrd", succ ) ) return

      ! update phase state
      call self%phaseState%replace("phase_rel_gdd", hui, succ)
      if( .not. check_return( trim(self%phaseName) , "phase_rel_gdd", succ ) ) return


    end subroutine deciduouswood

end module WEPSDeciduousWood_mod
