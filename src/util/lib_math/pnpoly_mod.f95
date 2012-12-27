!$Author$
!$Date$
!$Revision$
!$HeadURL$

! taken from:
! www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
! and heavily modified for current coding convention.
! Note: original comments by Author are below for reference

! purpose
! to determine whether a point is inside a polygon

! usage
! call pnpoly (px, py, xx, yy, n, inout )

! description of the parameters
! px - x-coordinate of point in question.
! py - y-coordinate of point in question.
! xx - n long vector containing x-coordinates of c vertices of polygon.
! yy - n long vector containg y-coordinates of c vertices of polygon.
! n - number of vertices in the polygon.
! inout - the signal returned:
! -1 if the point is outside of the polygon,
! 0 if the point is on an edge or at a vertex,
! 1 if the point is inside of the polygon.

! remarks
! the vertices may be listed clockwise or anticlockwise.
! the first may optionally be repeated, if so n may
! optionally be increased by 1.
! the input polygon may be a compound polygon consisting
! of several separate subpolygons. if so, the first vertex
! of each subpolygon must be repeated, and when calculating
! n, these first vertices must be counted twice.
! inout is the only parameter whose value is changed.
! the size of the arrays must be increased if n > maxdim
! written by randolph franklin, university of ottawa, 7/70.

! subroutines and function subprograms required
! none

! method
! a vertical(really horizontal, see web page)
! line is drawn thru the point in question. if it
! crosses the polygon an odd number of times, then the
! point is inside of the polygon.

Module pnpoly_mod
  use Points_Mod
  use Polygons_Mod

contains

  function pnpoly(pnt, ppol) result(pinout)
    ! arguments
    type(point), intent(in) :: pnt
    type(polygon), intent(in) :: ppol
    integer pinout  ! -1 - outside, 0 - on line or vertex, 1 - inside 

    ! local variables
    integer :: i, j
    real :: temp
    logical :: mx, my, nx, ny
    type(point), dimension(1:ppol%np) :: pnt0 ! polygon array adjusted to pnt origin


    do i = 1, ppol%np
        pnt0(i)=ppol%points(i)-pnt
    end do
    pinout = -1
    do i = 1, ppol%np
       j = 1 + mod(i,ppol%np)
       mx = pnt0(i)%x .ge. 0.0
       nx = pnt0(j)%x .ge. 0.0
       my = pnt0(i)%y .ge. 0.0
       ny = pnt0(j)%y .ge. 0.0
       if( .not.((my.or.ny).and.(mx.or.nx)) .or. (mx.and.nx) ) then
           continue
       else
           if( .not.(my.and.ny.and.(mx.or.nx).and. .not.(mx.and.nx)) ) then
               temp = (pnt0(i)%y*pnt0(j)%x-pnt0(i)%x*pnt0(j)%y)/(pnt0(j)%x-pnt0(i)%x)
               if(temp .eq. 0.0 ) then
                   pinout = 0
                   return
               else if( temp .gt. 0.0 ) then
                   pinout = -pinout
               end if
           else
              pinout = -pinout
           end if
       end if
     end do
     return
   end function pnpoly

end module pnpoly_mod

