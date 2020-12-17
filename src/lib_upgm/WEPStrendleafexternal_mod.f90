!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPStrendleafexternal_mod
  use Preprocess_mod
  use constants, only: dp, check_return, u_mgtokg
  use plant_mod
  use WEPSCrop_util_mod, only: shootnum, shoot_delay, shoot_flg, per_release, stage_release
  implicit none

  type, extends(preprocess) :: WEPStrendleafexternal
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => trend_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPStrendleafexternal

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPStrendleafexternal), intent(inout) :: self
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
      class(WEPStrendleafexternal), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine trend_proc(self, plnt, env)
      implicit none
      class(WEPStrendleafexternal), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.

      ! plant state
      real(dp) :: bcmstandleaf ! crop standing leaf mass (kg/m^2)
      real(dp) :: bcfliveleaf ! fraction of standing plant leaf which is living (transpiring)

      real(dp) :: bcleafareatrend ! direction in which leaf area is trending.
                                  ! Saves trend even if leaf area is static for long periods.
      real(dp) :: bprevliveleaf
      real(dp) :: bprevstandleaf

      ! locally computed values
      real(dp) :: trend ! test computation for trend direction of living leaf area

      ! Body of regrowth

      ! retrieve required inputs

      ! plant state
      call plnt%state%get("mstandleaf", bcmstandleaf, succ)
      if( .not. check_return( trim(self%processName) , "mstandleaf", succ ) ) return
      call plnt%state%get("fliveleaf", bcfliveleaf, succ)
      if( .not. check_return( trim(self%processName) , "fliveleaf", succ ) ) return
      call plnt%state%get("prevliveleaf", bprevliveleaf, succ)
      if( .not. check_return( trim(self%processName) , "prevliveleaf", succ ) ) return
      call plnt%state%get("prevstandleaf", bprevstandleaf, succ)
      if( .not. check_return( trim(self%processName) , "prevstandleaf", succ ) ) return

      ! set trend direction for living leaf area from external forces
      trend = (bcfliveleaf*bcmstandleaf) - (bprevliveleaf*bprevstandleaf)
      if( trend .ne. 0.0_dp ) then
        ! trend non-zero and (heat units past emergence or staged crown release crop)
        bcleafareatrend = trend

        ! plant state
        call plnt%state%replace("leafareatrend", bcleafareatrend, succ)
        if( .not. check_return( trim(self%processName) , "leafareatrend", succ ) ) return

      end if

    end subroutine trend_proc

end module WEPStrendleafexternal_mod
