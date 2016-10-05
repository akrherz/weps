!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sim_area_average_mod
    use Polygons_Mod, only: polygon
    use hydro_data_struct_defs, only: hydro_derived_et
    use erosion_data_struct_defs, only: subregionsurfacestate

    implicit none

  contains

    subroutine sim_area_average( subr_poly, h1et, subrsurf )
       type(polygon), dimension(:), intent(in) :: subr_poly
       type(hydro_derived_et), dimension(0:), intent(inout) :: h1et
       type(subregionsurfacestate), dimension(0:), intent(inout) :: subrsurf  ! subregion surface conditions

       integer :: isr, nsubr
       real :: tot_area
       REAL, PARAMETER :: snow_depth_thresh = 20.0

       nsubr = size(subr_poly)

       ! sum up all subregion areas
       tot_area = 0.0
       do isr = 1, nsubr
          tot_area = tot_area + subr_poly(isr)%area
       end do

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

       subrsurf(0)%acanag = 0.0  ! Ag Coeff. of abrasion (1/m)
       subrsurf(0)%asfcr = 0.0   ! Surface Crust fraction
       subrsurf(0)%asecr = 0.0   ! Surface Crust stability (J/m^2)
       subrsurf(0)%asmlos = 0.0  ! Surface Crust loose material (Mg/m^2)
       subrsurf(0)%aszcr = 0.0   ! Surface Crust thickness (mm)
       subrsurf(0)%asdcr = 0.0   ! Surface Crust density (Mg/m^3)
       subrsurf(0)%asflos = 0.0  ! Surface Crust - fraction of loose material (m^2/m^2)
       subrsurf(0)%acancr = 0.0  ! Surface Crust Coeff. of abrasion (1/m)

       do isr = 1, nsubr
          h1et(0)%zea  = h1et(0)%zea  + h1et(isr)%zea  * subr_poly(isr)%area / tot_area
          h1et(0)%zep  = h1et(0)%zep  + h1et(isr)%zep  * subr_poly(isr)%area / tot_area
          h1et(0)%zeta = h1et(0)%zeta + h1et(isr)%zeta * subr_poly(isr)%area / tot_area
          h1et(0)%zetp = h1et(0)%zetp + h1et(isr)%zetp * subr_poly(isr)%area / tot_area
          h1et(0)%zpta = h1et(0)%zpta + h1et(isr)%zpta * subr_poly(isr)%area / tot_area
          h1et(0)%zptp = h1et(0)%zptp + h1et(isr)%zptp * subr_poly(isr)%area / tot_area
          h1et(0)%drat = h1et(0)%drat + h1et(isr)%drat * subr_poly(isr)%area / tot_area
          h1et(0)%zsnd = h1et(0)%zsnd + h1et(isr)%zsnd * subr_poly(isr)%area / tot_area
          ! Note that the 20mm depth should be a global parameter
          ! It is currently stuck in erosion.for as a local parameter there
          ! this makes the 0 element of the snow cover array the fraction of the total area 
          ! which is protected from erosion by snow cover (the intent of the reporting code?)
          IF (h1et(isr)%zsnd > snow_depth_thresh) THEN
             h1et(isr)%snow_protect = 1.0
          else
             h1et(isr)%snow_protect = 0.0
          end if
          h1et(0)%snow_protect = h1et(0)%snow_protect + h1et(isr)%snow_protect * subr_poly(isr)%area / tot_area
          h1et(0)%zirr = h1et(0)%zirr + h1et(isr)%zirr * subr_poly(isr)%area / tot_area
          h1et(0)%zper = h1et(0)%zper + h1et(isr)%zper * subr_poly(isr)%area / tot_area
          h1et(0)%zrun = h1et(0)%zrun + h1et(isr)%zrun * subr_poly(isr)%area / tot_area

          subrsurf(0)%acanag = subrsurf(0)%acanag + subrsurf(isr)%acanag * subr_poly(isr)%area / tot_area
          subrsurf(0)%asfcr = subrsurf(0)%asfcr + subrsurf(isr)%asfcr * subr_poly(isr)%area / tot_area
          subrsurf(0)%asecr = subrsurf(0)%asecr + subrsurf(isr)%asecr * subr_poly(isr)%area / tot_area
          subrsurf(0)%asmlos = subrsurf(0)%asmlos + subrsurf(isr)%asmlos * subr_poly(isr)%area / tot_area
          subrsurf(0)%aszcr = subrsurf(0)%aszcr + subrsurf(isr)%aszcr * subr_poly(isr)%area / tot_area
          subrsurf(0)%asdcr = subrsurf(0)%asdcr + subrsurf(isr)%asdcr * subr_poly(isr)%area / tot_area
          subrsurf(0)%asflos = subrsurf(0)%asflos + subrsurf(isr)%asflos * subr_poly(isr)%area / tot_area
          subrsurf(0)%acancr = subrsurf(0)%acancr + subrsurf(isr)%acancr * subr_poly(isr)%area / tot_area

       end do

    end subroutine sim_area_average

end module sim_area_average_mod

