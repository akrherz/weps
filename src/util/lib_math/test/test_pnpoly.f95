!$Author$
!$Date$
!$Revision$
!$HeadURL$

program test_pnpoly

    ! variable declaration
    integer :: sr, nsubr
    real :: xx(4,4), yy(4,4)
    integer :: i, j, imax, jmax
    real :: ix, jy
    integer :: csr(0:6,0:10)
    real :: centrx, centry

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
    ! centrx - x coordinate of grid cell centroid
    ! centry - y coordinate of grid cell centroid
 
    ! function declaration
    integer pnpoly

    nsubr = 2

    ! pnpoly requires an array of x values and an array of y values

    ! first subregion, south, 120x60
    xx(1,1) = 0.0
    yy(1,1) = 0.0

    xx(2,1) = 120.0
    yy(2,1) = 0.0

    xx(3,1) = 120.0
    yy(3,1) = 60.0

    xx(4,1) = 0.0
    yy(4,1) = 60.0

    ! second subregion, north, 120x140
    xx(1,2) = 0.0
    yy(1,2) = 60.0

    xx(2,2) = 120.0
    yy(2,2) = 60.0

    xx(3,2) = 120.0
    yy(3,2) = 200.0

    xx(4,2) = 0.0
    yy(4,2) = 200.0

    imax = 7
    jmax = 11
    ix = 120.0 / (imax-1)
    jy = 200.0 / (jmax-1)


    do j = 1, jmax-1
      do i = 1, imax-1
         ! The grid cell is assumed rectangular. Use centroid of grid cell
         ! with subregion polygon to select grid cell subregion
         centrx = 0.5 * (i-1+i) * ix
         centry = 0.5 * (j-1+j) * jy
         do sr = 1,nsubr
           ! Check if it is inside subregion polygon
           if( pnpoly(centrx,centry,xx(1,sr),yy(1,sr),4).ge.0) then
              ! centroid of grid cell is inside or on edge of subregion polygon
              ! set subregion index
              csr(i,j) = sr
              write(*,*) 'point ', centrx, ':', centry, ' is in subregion ', sr
              ! default to first polygon if on edge by exiting the subregion do loop
              exit
           end if
         end do
         write(*,*) 'point ', centrx, ':', centry, ' is in subregion ', csr(i,j)
      end do          
    end do

    return
end
