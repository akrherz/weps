!$Author$
!$Date$
!$Revision$
!$HeadURL$

module subregions_mod
    use Polygons_Mod

    type(polygon), dimension(:), allocatable :: subr_poly  ! array of subregion polygons
    type(polygon), dimension(:), allocatable :: acct_poly  ! array of accounting region polygons

end module subregions_mod
