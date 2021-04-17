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
      real(dp) :: frsx1  ! warmer frost damage temperature
      real(dp) :: frsx2  ! colder frost damage temperature
      real(dp) :: frsy1  ! fraction leaf death at warmer frost damage temperature
      real(dp) :: frsy2  ! fraction leaf death at colder frost damage temperature
      real(dp) :: ffa    ! fraction of live leaf senescence occuring
      real(dp) :: tsmn1  ! minimum temperature of surface soil layer
      real(dp) :: mstandleaflive ! mass of live standing leaf
      real(dp) :: mstandleafdead ! mass of dead standing leaf
      real(dp) :: frst   ! the fraction of living leaf killed by freezing
      real(dp) :: lost_mass ! the amount of mass lost due to freeze damage
      logical :: succ = .false.

      ! get Parameters
      call self%processPars%get("frsx1", frsx1, succ)
      if( .not. check_return( trim(self%processName) , "frsx1", succ ) ) return
      call self%processPars%get("frsx2", frsx2, succ)
      if( .not. check_return( trim(self%processName) , "frsx2", succ ) ) return
      call self%processPars%get("frsy1", frsy1, succ)
      if( .not. check_return( trim(self%processName) , "frsy1", succ ) ) return
      call self%processPars%get("frsy2", frsy2, succ)
      if( .not. check_return( trim(self%processName) , "frsy2", succ ) ) return

      ! get current state
      call plnt%state%get("ffa", ffa, succ)
      if( .not. check_return( trim(self%processName) , "ffa", succ ) ) return
      call plnt%state%get("mstandleaflive", mstandleaflive, succ)
      if( .not. check_return( trim(self%processName) , "mstandleaflive", succ ) ) return
      call plnt%state%get("mstandleafdead", mstandleafdead, succ)
      if( .not. check_return( trim(self%processName) , "mstandleafdead", succ ) ) return

      ! get environment variables
      call env%state%get("tsmn1", tsmn1, succ)
      if( .not. check_return( trim(self%processName) , "tsmn1", succ ) ) return

      call freeze_damage( ffa, tsmn1, frsx1, frsx2, frsy1, frsy2, mstandleaflive, mstandleafdead, frst, lost_mass )

      call plnt%state%replace("mstandleaflive", mstandleaflive, succ)
      if( .not. check_return( trim(self%processName) , "mstandleaflive", succ ) ) return
      call plnt%state%replace("mstandleafdead", mstandleafdead, succ)
      if( .not. check_return( trim(self%processName) , "mstandleafdead", succ ) ) return
      call plnt%state%replace("frst", frst, succ)
      if( .not. check_return( trim(self%processName) , "frst", succ ) ) return
      call plnt%state%replace("lost_mass", lost_mass, succ)
      if( .not. check_return( trim(self%processName) , "lost_mass", succ ) ) return

    end subroutine FreezeDamage

end module WEPSFreezeDamage_mod
