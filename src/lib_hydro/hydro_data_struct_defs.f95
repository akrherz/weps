!$Author$
!$Date$
!$Revision$
!$HeadURL$

module hydro_data_struct_defs
    use Polygons_Mod, only: polygon
    implicit none

    integer, dimension(:), allocatable :: am0hfl    ! flag to print HYDROlogy output
                                                    ! 0 = no output
                                                    ! 1 = daily
                                                    ! 2 = hourly
                                                    ! 3 = daily and hourly
                                                    ! 4 = soil temperature
                                                    ! 5 = daily and soil temperature
                                                    ! 6 = hourly and soil temperature
                                                    ! 7 = daily, hourly, and soil temperature
    integer, dimension(:), allocatable :: am0hdb    ! flag to print HYDROlogy variables before and after the call to HYDRO
                                                    ! 0 = no output
                                                    ! 1 = output


    type hydro_derived_et
       real :: zea  ! Actual bare soil evaporation (mm/day)
       real :: zep  ! Potential bare soil evaporation (mm/day)
       real :: zeta ! Actual evapotranspiration (mm/day)
       real :: zetp ! potential evapotranspiration (mm/day)
       real :: zpta ! Actual plant transpiration (mm/day)
       real :: zptp ! potential plant transpiration (mm/day)
       real :: drat ! dryness ratio
       real :: zsnd ! snow depth (mm)
       real :: snow_protect ! snow cover greater than snow_depth_thresh
       real :: zirr ! Single day irrigation water applied (mm)
       real :: zper ! daily deep percolation (mm/day)
       real :: zrun ! daily surface runoff (mm/day)
    end type hydro_derived_et

end module hydro_data_struct_defs

