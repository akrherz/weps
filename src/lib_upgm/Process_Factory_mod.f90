!$Author$
!$Date$
!$Revision$
!$HeadURL$

module Process_Factory
    use Preprocess_mod
    use gddmethod1_mod
    use gddmethodWEPS_mod
    use ritchieVernalization_mod
    use ritchieHardening_mod
    use WEPSwarmdays_mod
    use WEPStempstress_mod
    use WEPSFreezeDamage_mod
    use WEPSregrowth_mod

  contains
    
    function create_process(processName, processLabel) result(processPtr)
      class(preprocess), pointer :: processPtr
      character(len=*), intent(in) :: processName ! please trim, all lower case.
      character(len=*), intent(in) :: processLabel ! please trim, all lower case.

      nullify(processPtr)
    
      if (processName == "gddmethod1") then
        allocate(gdd1_method :: processPtr)
      elseif (processName == "gddweps_method") then
        allocate(gddWEPS_method :: processPtr)
      elseif (processName == "ritchie_vernalization") then
        allocate(ritchieVernalization :: processPtr)
      elseif (processName == "ritchie_winterhardening") then
        allocate(ritchieHardening :: processPtr)
      elseif (processName == "weps_warmdays") then
        allocate(WEPSwarmdays :: processPtr)
      elseif (processName == "weps_tempstress") then
        allocate(WEPSTempStress :: processPtr)
      elseif (processName == "weps_freezedamage") then
        allocate(WEPSFreezeDamage :: processPtr)
      elseif (processName == "weps_regrowth") then
        allocate(WEPSregrowth :: processPtr)
      endif

      if( associated(processPtr) ) then
        processPtr%processName = processName
        processPtr%processLabel = processLabel
        call processPtr%processPars%init()
        call processPtr%processState%init()
        nullify( processPtr%processNext )
    end if

    end function create_process
    
end module Process_Factory
