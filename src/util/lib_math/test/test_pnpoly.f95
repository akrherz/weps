!$Author$
!$Date$
!$Revision$
!$HeadURL$

program test_pnpoly

    use Points_Mod
    use Polygons_Mod
    use pnpoly_mod
    implicit none

    ! variable declaration
    integer :: sr, nsubr
    integer :: i, j, imax, jmax
    real :: ix, jy
    integer :: csr(0:6,0:10)

    ! variable definitions
    ! sr - subregion counter
    ! nsubr - number of subregions
    ! xx - array of subregion x coordinates
    ! yy - array of subregion y coordinates
    ! i - grid x index counter
    ! j - grid y index counter
    ! imax - maximum x index
    ! jmax - maximum y index
    ! ix - grid cell length in x direction
    ! iy - grid cell length in y direction
 
    character(len=16), dimension(4) :: names
    type(polygon), dimension(4) :: polys
    type(point) :: pnt
 
    nsubr = 4

    ! pnpoly requires a point and a polygon

    ! first subregion, south, 120x60
    names(1) = "South"
    polys(1) = create_polygon(4)

    polys(1)%points(1)%x = 0.0
    polys(1)%points(1)%y = 0.0

    polys(1)%points(2)%x = 120.0
    polys(1)%points(2)%y = 0.0

    polys(1)%points(3)%x = 120.0
    polys(1)%points(3)%y = 60.0

    polys(1)%points(4)%x = 0.0
    polys(1)%points(4)%y = 60.0

    ! second subregion, north, 120x140
    names(2) = "North"
    polys(2) = create_polygon(4)

    polys(2)%points(1)%x = 0.0
    polys(2)%points(1)%y = 60.0

    polys(2)%points(2)%x = 120.0
    polys(2)%points(2)%y = 60.0

    polys(2)%points(3)%x = 120.0
    polys(2)%points(3)%y = 200.0

    polys(2)%points(4)%x = 0.0
    polys(2)%points(4)%y = 200.0

    ! third subregion, western strips, 30x200
    names(3) = "Western Strips"
    polys(3) = create_polygon(10)

    polys(3)%points(1)%x = 0.0
    polys(3)%points(1)%y = 0.0

    polys(3)%points(2)%x = 30.0
    polys(3)%points(2)%y = 0.0

    polys(3)%points(3)%x = 30.0
    polys(3)%points(3)%y = 200.0

    polys(3)%points(4)%x = 0.0
    polys(3)%points(4)%y = 200.0

    polys(3)%points(5)%x = 0.0
    polys(3)%points(5)%y = 0.0

    polys(3)%points(6)%x = 60.0
    polys(3)%points(6)%y = 0.0

    polys(3)%points(7)%x = 90.0
    polys(3)%points(7)%y = 0.0

    polys(3)%points(8)%x = 90.0
    polys(3)%points(8)%y = 200.0

    polys(3)%points(9)%x = 60.0
    polys(3)%points(9)%y = 200.0

    polys(3)%points(10)%x = 60.0
    polys(3)%points(10)%y = 0.0

    ! fourth subregion, eastern strips, 30x200
    names(4) = "Eastern Strips"
    polys(4) = create_polygon(10)

    polys(4)%points(1)%x = 30.0
    polys(4)%points(1)%y = 0.0

    polys(4)%points(2)%x = 60.0
    polys(4)%points(2)%y = 0.0

    polys(4)%points(3)%x = 60.0
    polys(4)%points(3)%y = 200.0

    polys(4)%points(4)%x = 30.0
    polys(4)%points(4)%y = 200.0

    polys(4)%points(5)%x = 30.0
    polys(4)%points(5)%y = 0.0

    polys(4)%points(6)%x = 90.0
    polys(4)%points(6)%y = 0.0

    polys(4)%points(7)%x = 120.0
    polys(4)%points(7)%y = 0.0

    polys(4)%points(8)%x = 120.0
    polys(4)%points(8)%y = 200.0

    polys(4)%points(9)%x = 90.0
    polys(4)%points(9)%y = 200.0

    polys(4)%points(10)%x = 90.0
    polys(4)%points(10)%y = 0.0

    ! set up grid
    imax = 7
    jmax = 11
    ix = 120.0 / (imax-1)
    jy = 200.0 / (jmax-1)


    do j = 1, jmax-1
      do i = 1, imax-1
         ! The grid cell is assumed rectangular. Use centroid of grid cell
         ! with subregion polygon to select grid cell subregion
         pnt%x = 0.5 * (i-1+i) * ix
         pnt%y = 0.5 * (j-1+j) * jy
         do sr = 1,nsubr
           ! Check if it is inside subregion polygon
           if( pnpoly(pnt, polys(sr)).ge.0) then
              ! centroid of grid cell is inside or on edge of subregion polygon
              ! set subregion index
              !csr(i,j) = sr
              write(*,*) 'point ', pnt%x, ':', pnt%y, ' is in subregion ', names(sr)
           end if
         end do
         !write(*,*) 'point ', centrx, ':', centry, ' is in subregion ', csr(i,j)
      end do          
    end do
 
    do i = 1, 4
       call free_polygon(polys(i))
    end do

end program test_pnpoly
