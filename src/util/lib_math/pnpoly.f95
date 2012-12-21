!$Author$
!$Date$
!$Revision$
!$HeadURL$

! taken from:
! www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
! and heavily modified for current coding convention.
! Note: This does not require selecting a point know to be outside the polygon

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

integer function pnpoly(px, py, xx, yy, n)

! returns "inout" value as the signal described above

! arguments
real :: px, py
real :: xx(*), yy(*)
integer :: n

! local variables
integer :: i, j
real :: x, y, temp
logical :: mx, my, nx, ny
dimension x(n), y(n)

do i = 1, n
    x(i)=xx(i)-px
    y(i)=yy(i)-py
end do
pnpoly = -1
do i = 1, n
   j = 1 + mod(i,n)
   mx = x(i) .ge. 0.0
   nx = x(j) .ge. 0.0
   my = y(i) .ge. 0.0
   ny = y(j) .ge. 0.0
   if( .not.((my.or.ny).and.(mx.or.nx)) .or. (mx.and.nx) ) then
       continue
   else
       if( .not.(my.and.ny.and.(mx.or.nx).and. .not.(mx.and.nx)) ) then
           temp = (y(i)*x(j)-x(i)*y(j))/(x(j)-x(i))
           if(temp .eq. 0.0 ) then
               pnpoly = 0
               return
           else if( temp .gt. 0.0 ) then
               pnpoly = -pnpoly
           end if
       else
          pnpoly = -pnpoly
       end if
   end if
 end do
 return
 end 

