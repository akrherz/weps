!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sim_area_average_mod
    use Polygons_Mod, only: polygon
    use hydro_data_struct_defs, only: hydro_derived_et
    use report_hydrobal_mod, only: hydro_balance
    use erosion_data_struct_defs, only: subregionsurfacestate
    use soil_data_struct_defs, only: soil_def
    use biomaterial, only: biototal

    implicit none

  contains

    subroutine sim_area_average( h1et, h1bal, subrsurf, soil, croptot, restot, biotot )
       type(hydro_derived_et), dimension(0:), intent(inout) :: h1et
       type(hydro_balance), dimension(0:), intent(inout) :: h1bal
       type(subregionsurfacestate), dimension(0:), intent(inout) :: subrsurf  ! subregion surface conditions
       type(soil_def), dimension(0:), intent(inout) :: soil  ! contains:
                                ! aslagm, as0ags, aslagn, aslagx (ASD parms)
                                ! aseags (agg stability), asdagd (agg density)
                                ! asfcr (crust fraction)
                                ! aszcr (crust thickness)
                                ! asmlos (mass of loose material on crusted surface)
                                ! asflos (fraction of crusted surface with loose material)
                                ! asdcr (density of crust)
                                ! asecr (stability of crust)
                                ! acancr(crust coeff. of abrasion)
                                ! acanag (agg. coeff. of abrasion)
                                ! aslrr Allmaras RR values
                                ! aszrgh Ridge height
                                ! asxrgs Ridge spacing
                                ! asargo Ridge dir
       type(biototal), dimension(0:), intent(inout) :: restot  ! contains:
                                ! adftcvtot(isr)  total dead flat cover
                                ! adrcdtot(isr)   total effective silhouette
                                ! admftot(isr)    total dead flat mass
                                ! admsttot(isr)   total dead standing mass
       type(biototal), dimension(0:), intent(inout) :: croptot  ! contains:
                                ! acfcancov(isr)  crop canopy cover
                                ! acftcv(isr)     crop flat cover
                                ! acrcd(isr)      crop effective silhouette
                                ! acmf(isr)       crop flat mass
                                ! acmst(isr)      crop standing mass
                                ! acmstandstore(isr)      crop standing repr mass
                                ! acmflatstore(isr)      crop flat repr mass
       type(biototal), dimension(0:), intent(inout) :: biotot  ! contains:
                                ! abftcv(isr)     all flat cover
                                ! abrcd           all effective silhouette
                                ! abmf(isr)       all flat mass
                                ! abmst(isr)      all standing mass

       integer :: isr, nsubr
       real :: tot_area
       real :: frac_area
       REAL, PARAMETER :: snow_depth_thresh = 20.0

       nsubr = size(subrsurf) - 1

       ! sum up all subregion areas
       tot_area = subrsurf(0)%cntcells

       h1et(0)%zea  = 0.0
       h1et(0)%zep  = 0.0
       h1et(0)%zeta = 0.0
       h1et(0)%zetp = 0.0
       h1et(0)%zpta = 0.0
       h1et(0)%zptp = 0.0
       h1et(0)%drat = 0.0
       h1et(0)%zsnd = 0.0
       h1et(0)%snow_protect = 0.0
       h1et(0)%zirr = 0.0
       h1et(0)%zper = 0.0
       h1et(0)%zrun = 0.0

       h1bal(0)%presswc = 0.0

       subrsurf(0)%acanag = 0.0  ! Ag Coeff. of abrasion (1/m)
       subrsurf(0)%asfcr = 0.0   ! Surface Crust fraction
       subrsurf(0)%asecr = 0.0   ! Surface Crust stability (J/m^2)
       subrsurf(0)%asmlos = 0.0  ! Surface Crust loose material (Mg/m^2)
       subrsurf(0)%aszcr = 0.0   ! Surface Crust thickness (mm)
       subrsurf(0)%asdcr = 0.0   ! Surface Crust density (Mg/m^3)
       subrsurf(0)%asflos = 0.0  ! Surface Crust - fraction of loose material (m^2/m^2)
       subrsurf(0)%acancr = 0.0  ! Surface Crust Coeff. of abrasion (1/m)

       croptot(0)%ftcancov = 0.0 ! Crop canopy cover fraction
       croptot(0)%ftcvtot = 0.0  ! Crop flat cover fraction
       croptot(0)%rcdtot = 0.0   ! Crop Standing silhouette (m^2/m^2)
       croptot(0)%msttot = 0.0      ! Crop standing mass (kg/m^2)
       croptot(0)%mftot = 0.0       ! Crop Flat masss (kg/m^2)
       croptot(0)%mstandstore = 0.0 ! Crop standing storage mass (kg/m^2)
       croptot(0)%mflatstore = 0.0  ! Crop flat storage mass (kg/m^2)
       croptot(0)%mrttot = 0.0
       croptot(0)%zht_ave = 0.0
       croptot(0)%dstmtot = 0.0

       restot(0)%ftcvtot = 0.0
       restot(0)%rcdtot = 0.0
       restot(0)%mftot = 0.0
       restot(0)%msttot = 0.0
       restot(0)%mbgtot = 0.0
       restot(0)%mrttot = 0.0
       restot(0)%zht_ave = 0.0
       restot(0)%dstmtot = 0.0

       biotot(0)%ftcvtot = 0.0
       biotot(0)%rcdtot = 0.0
       biotot(0)%mftot = 0.0
       biotot(0)%msttot = 0.0

       soil(0)%aslrr = 0.0
       soil(0)%aszrgh = 0.0
       soil(0)%asxrgs = 0.0
       soil(0)%asargo = 0.0
       soil(0)%aslagm(1) = 0.0
       soil(0)%as0ags(1) = 0.0
       soil(0)%aslagn(1) = 0.0
       soil(0)%aslagx(1) = 0.0
       soil(0)%aseags(1) = 0.0
       soil(0)%asdagd(1) = 0.0
       soil(0)%acanag = 0.0
       soil(0)%asfcr = 0.0
       soil(0)%asecr = 0.0
       soil(0)%asmlos = 0.0
       soil(0)%aszcr = 0.0
       soil(0)%asdcr = 0.0
       soil(0)%asflos = 0.0
       soil(0)%acancr = 0.0

       do isr = 1, nsubr
          frac_area = subrsurf(isr)%cntcells / tot_area
          h1et(0)%zea  = h1et(0)%zea  + h1et(isr)%zea  * frac_area
          h1et(0)%zep  = h1et(0)%zep  + h1et(isr)%zep  * frac_area
          h1et(0)%zeta = h1et(0)%zeta + h1et(isr)%zeta * frac_area
          h1et(0)%zetp = h1et(0)%zetp + h1et(isr)%zetp * frac_area
          h1et(0)%zpta = h1et(0)%zpta + h1et(isr)%zpta * frac_area
          h1et(0)%zptp = h1et(0)%zptp + h1et(isr)%zptp * frac_area
          h1et(0)%drat = h1et(0)%drat + h1et(isr)%drat * frac_area
          h1et(0)%zsnd = h1et(0)%zsnd + h1et(isr)%zsnd * frac_area
          ! Note that the 20mm depth should be a global parameter
          ! It is currently stuck in erosion.for as a local parameter there
          ! this makes the 0 element of the snow cover array the fraction of the total area 
          ! which is protected from erosion by snow cover (the intent of the reporting code?)
          IF (h1et(isr)%zsnd > snow_depth_thresh) THEN
             h1et(isr)%snow_protect = 1.0
          else
             h1et(isr)%snow_protect = 0.0
          end if
          h1et(0)%snow_protect = h1et(0)%snow_protect + h1et(isr)%snow_protect * frac_area
          h1et(0)%zirr = h1et(0)%zirr + h1et(isr)%zirr * frac_area
          h1et(0)%zper = h1et(0)%zper + h1et(isr)%zper * frac_area
          h1et(0)%zrun = h1et(0)%zrun + h1et(isr)%zrun * frac_area

          h1bal(0)%presswc = h1bal(0)%presswc + h1bal(isr)%presswc * frac_area

          subrsurf(0)%acanag = subrsurf(0)%acanag + subrsurf(isr)%acanag * frac_area
          subrsurf(0)%asfcr = subrsurf(0)%asfcr + subrsurf(isr)%asfcr * frac_area
          subrsurf(0)%asecr = subrsurf(0)%asecr + subrsurf(isr)%asecr * frac_area
          subrsurf(0)%asmlos = subrsurf(0)%asmlos + subrsurf(isr)%asmlos * frac_area
          subrsurf(0)%aszcr = subrsurf(0)%aszcr + subrsurf(isr)%aszcr * frac_area
          subrsurf(0)%asdcr = subrsurf(0)%asdcr + subrsurf(isr)%asdcr * frac_area
          subrsurf(0)%asflos = subrsurf(0)%asflos + subrsurf(isr)%asflos * frac_area
          subrsurf(0)%acancr = subrsurf(0)%acancr + subrsurf(isr)%acancr * frac_area

          croptot(0)%ftcancov = croptot(0)%ftcancov + croptot(isr)%ftcancov * frac_area
          croptot(0)%ftcvtot = croptot(0)%ftcvtot + croptot(isr)%ftcvtot * frac_area
          croptot(0)%rcdtot = croptot(0)%rcdtot + croptot(isr)%rcdtot * frac_area
          croptot(0)%msttot = croptot(0)%msttot + croptot(isr)%msttot * frac_area
          croptot(0)%mftot = croptot(0)%mftot + croptot(isr)%mftot * frac_area
          croptot(0)%mstandstore = croptot(0)%mstandstore + croptot(isr)%mstandstore * frac_area
          croptot(0)%mflatstore = croptot(0)%mflatstore + croptot(isr)%mflatstore * frac_area
          croptot(0)%mrttot = croptot(0)%mrttot + croptot(isr)%mrttot * frac_area
          croptot(0)%zht_ave = croptot(0)%zht_ave + croptot(isr)%zht_ave * frac_area
          croptot(0)%dstmtot = croptot(0)%dstmtot + croptot(isr)%dstmtot * frac_area

          restot(0)%ftcvtot = restot(0)%ftcvtot + restot(isr)%ftcvtot * frac_area
          restot(0)%rcdtot = restot(0)%rcdtot + restot(isr)%rcdtot * frac_area
          restot(0)%mftot = restot(0)%mftot + restot(isr)%mftot * frac_area
          restot(0)%msttot = restot(0)%msttot + restot(isr)%msttot * frac_area
          restot(0)%mbgtot = restot(0)%mbgtot + restot(isr)%mbgtot * frac_area
          restot(0)%mrttot = restot(0)%mrttot + restot(isr)%mrttot * frac_area
          restot(0)%zht_ave = restot(0)%zht_ave + restot(isr)%zht_ave * frac_area
          restot(0)%dstmtot = restot(0)%dstmtot + restot(isr)%dstmtot * frac_area

          biotot(0)%ftcvtot = biotot(0)%ftcvtot + biotot(isr)%ftcvtot * frac_area
          biotot(0)%rcdtot = biotot(0)%rcdtot + biotot(isr)%rcdtot * frac_area
          biotot(0)%mftot = biotot(0)%mftot + biotot(isr)%mftot * frac_area
          biotot(0)%msttot = biotot(0)%msttot + biotot(isr)%msttot * frac_area

          soil(0)%aslrr = soil(0)%aslrr + soil(isr)%aslrr * frac_area
          soil(0)%aszrgh = soil(0)%aszrgh + soil(isr)%aszrgh * frac_area
          soil(0)%asxrgs = soil(0)%asxrgs + soil(isr)%asxrgs * frac_area
          soil(0)%asargo = soil(0)%asargo + soil(isr)%asargo * frac_area
          soil(0)%aslagm(1) = soil(0)%aslagm(1) + soil(isr)%aslagm(1) * frac_area
          soil(0)%as0ags(1) = soil(0)%as0ags(1) + soil(isr)%as0ags(1) * frac_area
          soil(0)%aslagn(1) = soil(0)%aslagn(1) + soil(isr)%aslagn(1) * frac_area
          soil(0)%aslagx(1) = soil(0)%aslagx(1) + soil(isr)%aslagx(1) * frac_area
          soil(0)%aseags(1) = soil(0)%aseags(1) + soil(isr)%aseags(1) * frac_area
          soil(0)%asdagd(1) = soil(0)%asdagd(1) + soil(isr)%asdagd(1) * frac_area
          soil(0)%acanag = soil(0)%acanag + soil(isr)%acanag * frac_area
          soil(0)%asfcr = soil(0)%asfcr + soil(isr)%asfcr * frac_area
          soil(0)%asecr = soil(0)%asecr + soil(isr)%asecr * frac_area
          soil(0)%asmlos = soil(0)%asmlos + soil(isr)%asmlos * frac_area
          soil(0)%aszcr = soil(0)%aszcr + soil(isr)%aszcr * frac_area
          soil(0)%asdcr = soil(0)%asdcr + soil(isr)%asdcr * frac_area
          soil(0)%asflos = soil(0)%asflos + soil(isr)%asflos * frac_area
          soil(0)%acancr = soil(0)%acancr + soil(isr)%acancr * frac_area

       end do

    end subroutine sim_area_average

end module sim_area_average_mod

