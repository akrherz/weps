!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPScolddays_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  use WEPSCrop_util_mod, only: coldday_cum
  implicit none

  type, extends(preprocess) :: WEPScolddays
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => coldday_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPScolddays

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPScolddays), intent(inout) :: self
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
      class(WEPScolddays), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine coldday_proc(self, plnt, env)
      implicit none
      class(WEPScolddays), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: colddays       ! accumulated warm days
      real(dp) :: tbase          ! minimum growth temperature
      real(dp) :: tmax           ! Maximum temperature for this growth day
      real(dp) :: tmin           ! Minimum temperature for this growth day
      logical :: succ = .false.

      ! get Parameters
      call self%processPars%get("tbas", tbase, succ)
      if( .not. check_return( trim(self%processName) , "tbas", succ ) ) return
      ! get current state
      call plnt%state%get("colddays", colddays, succ)
      if( .not. check_return( trim(self%processName) , "colddays", succ ) ) return
      call env%state%get("tmax", tmax, succ)
      if( .not. check_return( trim(self%processName) , "tmax", succ ) ) return
      call env%state%get("tmin", tmin, succ)
      if( .not. check_return( trim(self%processName) , "tmin", succ ) ) return

      call coldday_cum(colddays, tbase, tmax, tmin)

      call plnt%state%replace("colddays", colddays, succ)
      if( .not. check_return( trim(self%processName) , "colddays", succ ) ) return

    end subroutine coldday_proc

end module WEPScolddays_mod
