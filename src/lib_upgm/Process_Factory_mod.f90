!$Author$
!$Date$
!$Revision$
!$HeadURL$

module Process_Factory
    use Preprocess_mod
    use gddmethod1_mod
    use gddmethodWEPS_mod
    use ritchieHardening_mod
    use ritchieVernalization_mod
    use WEPScolddays_mod
    use WEPSFreezeDamage_mod
    use WEPSleafoff_mod
    use WEPSleafon_mod
    use WEPSregrowth_mod
    use WEPSregrowwood_mod
    use WEPStempstress_mod
    use WEPStrendleafexternal_mod
    use WEPStrendstemexternal_mod
    use WEPSwarmdays_mod
    use WEPSwinterAnnSpring_mod

  contains
    
    function create_process(processName, processLabel) result(processPtr)
      class(preprocess), pointer :: processPtr
      character(len=*), intent(in) :: processName ! please trim, all lower case.
      character(len=*), intent(in) :: processLabel ! please trim, all lower case.

      nullify(processPtr)
    
      if (processName == "gdd1_method") then
        allocate(gdd1_method :: processPtr)
      elseif (processName == "gddWEPS_method") then
        allocate(gddWEPS_method :: processPtr)
      elseif (processName == "ritchieHardening") then
        allocate(ritchieHardening :: processPtr)
      elseif (processName == "ritchieVernalization") then
        allocate(ritchieVernalization :: processPtr)
      elseif (processName == "WEPScolddays") then
        allocate(WEPScolddays :: processPtr)
      elseif (processName == "WEPSFreezeDamage") then
        allocate(WEPSFreezeDamage :: processPtr)
      elseif (processName == "WEPSleafoff") then
        allocate(WEPSleafoff :: processPtr)
      elseif (processName == "WEPSleafon") then
        allocate(WEPSleafon :: processPtr)
      elseif (processName == "WEPSregrowthannual") then
        allocate(WEPSregrowth :: processPtr)
      elseif (processName == "WEPSregrowthperen") then
        allocate(WEPSregrowth :: processPtr)
      elseif (processName == "WEPSregrowthstaged") then
        allocate(WEPSregrowth :: processPtr)
      elseif (processName == "WEPSregrowwood") then
        allocate(WEPSregrowwood :: processPtr)
      elseif (processName == "WEPSTempStress") then
        allocate(WEPSTempStress :: processPtr)
      elseif (processName == "WEPStrendleafexternal") then
        allocate(WEPStrendleafexternal :: processPtr)
      elseif (processName == "WEPStrendstemexternal") then
        allocate(WEPStrendstemexternal :: processPtr)
      elseif (processName == "WEPSwarmdays") then
        allocate(WEPSwarmdays :: processPtr)
      elseif (processName == "WEPSwinterAnnSpring") then
        allocate(WEPSwinterAnnSpring :: processPtr)
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
