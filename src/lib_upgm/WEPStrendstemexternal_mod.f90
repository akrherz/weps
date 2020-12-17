!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPStrendstemexternal_mod
  use Preprocess_mod
  use constants, only: dp, check_return, u_mgtokg
  use plant_mod
  use WEPSCrop_util_mod, only: shootnum, shoot_delay, shoot_flg, per_release, stage_release
  implicit none

  type, extends(preprocess) :: WEPStrendstemexternal
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => trend_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPStrendstemexternal

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPStrendstemexternal), intent(inout) :: self
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
      class(WEPStrendstemexternal), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine trend_proc(self, plnt, env)
      implicit none
      class(WEPStrendstemexternal), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.

      ! plant state
      real(dp) :: bcmstandstem ! crop standing stem mass (kg/m^2)
      real(dp) :: bcmflatstem  ! crop flat stem mass (kg/m^2)

      real(dp) :: bcstemmasstrend ! direction in which stem mass is trending.
                                  ! Saves trend even if stem mass is static for long periods.
      real(dp) :: bprevstandstem
      real(dp) :: bprevflatstem

      ! locally computed values
      real(dp) :: trend ! test computation for trend direction of living stem mass

      ! Body of regrowth

      ! retrieve required inputs

      ! plant state
      call plnt%state%get("mstandstem", bcmstandstem, succ)
      if( .not. check_return( trim(self%processName) , "mstandstem", succ ) ) return
      call plnt%state%get("mflatstem", bcmflatstem, succ)
      if( .not. check_return( trim(self%processName) , "mflatstem", succ ) ) return
      call plnt%state%get("prevstandstem", bprevstandstem, succ)
      if( .not. check_return( trim(self%processName) , "prevstandstem", succ ) ) return
      call plnt%state%get("prevflatstem", bprevflatstem, succ)
      if( .not. check_return( trim(self%processName) , "prevflatstem", succ ) ) return

      ! set trend direction for above ground stem mass from external forces
      trend = bcmstandstem + bcmflatstem - bprevstandstem - bprevflatstem
      if( trend .ne. 0.0_dp ) then
        ! trend non-zero and (heat units past emergence or staged crown release crop)
        bcstemmasstrend = trend

        ! plant state
        call plnt%state%replace("stemmasstrend", bcstemmasstrend, succ)
        if( .not. check_return( trim(self%processName) , "stemmasstrend", succ ) ) return

      end if

    end subroutine trend_proc

end module WEPStrendstemexternal_mod
