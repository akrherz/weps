!$Author$
!$Date$
!$Revision$
!$HeadURL$

module ritchieVernalization_mod
  use Preprocess_mod
  use constants, only: dp, int32, check_return
  use WEPSCrop_util_mod, only: chillunit_cum
  implicit none

  type, extends(preprocess) :: ritchieVernalization
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => Vernalization ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type ritchieVernalization

  contains

    subroutine load_state(self, processState)
      implicit none
      class(ritchieVernalization), intent(inout) :: self
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
      class(ritchieVernalization), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of stage_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine Vernalization(self, plnt, env)
      implicit none
      class(ritchieVernalization), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: chill_unit_cum  ! accumulated chill units for vernalization
      real(dp) :: tmax           ! Maximum temperature for this growth day
      real(dp) :: tmin           ! Minimum temperature for this growth day
      integer(int32) :: tmp
      logical :: succ = .false.

      ! initialized to zero at process beginning
      call plnt%state%get("chill_unit_cum", chill_unit_cum, succ)
      if( .not. check_return( trim(self%processName) , "chill_unit_cum", succ ) ) return
      call env%state%get("tmax", tmax, succ)
      if( .not. check_return( trim(self%processName) , "tmax", succ ) ) return
      call env%state%get("tmin", tmin, succ)
      if( .not. check_return( trim(self%processName) , "tmin", succ ) ) return

      call chillunit_cum(chill_unit_cum, tmax, tmin)

      !write(*,*) 'Chill Units: ', chill_unit_cum, tmax, tmin

      call plnt%state%replace("chill_unit_cum", chill_unit_cum, succ)
      if( .not. check_return( trim(self%processName) , "chill_unit_cum", succ ) ) return

    end subroutine Vernalization

end module ritchieVernalization_mod
