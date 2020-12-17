!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSFreezeDamage_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  use WEPSCrop_util_mod, only: freeze_damage
  implicit none

  type, extends(preprocess) :: WEPSFreezeDamage
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => FreezeDamage ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSFreezeDamage

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPSFreezeDamage), intent(inout) :: self
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
      class(WEPSFreezeDamage), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine FreezeDamage(self, plnt, env)
      implicit none
      class(WEPSFreezeDamage), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: a_fr   ! parameter in the frost damage s-curve
      real(dp) :: b_fr   ! parameter in the frost damage s-curve
      real(dp) :: ffa    ! fraction of live leaf senescence occuring
      real(dp) :: tsmn1  ! minimum temperature of surface soil layer
      real(dp) :: mstandleaf ! mass of standing leaf
      real(dp) :: fliveleaf ! mass of standing leaf
      real(dp) :: frst   ! the fraction of living leaf killed by freezing
      real(dp) :: lost_mass ! the amount of mass lost due to freeze damage
      logical :: succ = .false.

      ! get Parameters
      call self%processPars%get("a_fr", a_fr, succ)
      if( .not. check_return( trim(self%processName) , "a_fr", succ ) ) return
      call self%processPars%get("b_fr", b_fr, succ)
      if( .not. check_return( trim(self%processName) , "b_fr", succ ) ) return

      ! get current state
      call plnt%state%get("ffa", ffa, succ)
      if( .not. check_return( trim(self%processName) , "ffa", succ ) ) return
      call plnt%state%get("mstandleaf", mstandleaf, succ)
      if( .not. check_return( trim(self%processName) , "mstandleaf", succ ) ) return
      call plnt%state%get("fliveleaf", fliveleaf, succ)
      if( .not. check_return( trim(self%processName) , "fliveleaf", succ ) ) return

      ! get environment variables
      call env%state%get("tsmn1", tsmn1, succ)
      if( .not. check_return( trim(self%processName) , "tsmn1", succ ) ) return

      call freeze_damage( ffa, tsmn1, a_fr, b_fr, mstandleaf, fliveleaf, frst, lost_mass )

      call plnt%state%replace("mstandleaf", mstandleaf, succ)
      if( .not. check_return( trim(self%processName) , "mstandleaf", succ ) ) return
      call plnt%state%replace("fliveleaf", fliveleaf, succ)
      if( .not. check_return( trim(self%processName) , "fliveleaf", succ ) ) return
      call plnt%state%replace("frst", frst, succ)
      if( .not. check_return( trim(self%processName) , "frst", succ ) ) return
      call plnt%state%replace("lost_mass", lost_mass, succ)
      if( .not. check_return( trim(self%processName) , "lost_mass", succ ) ) return

    end subroutine FreezeDamage

end module WEPSFreezeDamage_mod
