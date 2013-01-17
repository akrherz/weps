!$Author$
!$Date$
!$Revision$
!$HeadURL$

! started with code from:
! http://rosettacode.org/wiki/Ray-casting_algorithm#Fortran

! heavily modified to eliminate vertex index functionality.
! A polygon is now simply and ordered set of points.
! Routines define whether the last point must be joined to
! the first point in order to close the polygon


module Polygons_Mod
  use Points_Mod
  implicit none
 
  type polygon
     integer :: np   ! number of points in polygon point array
     type(point), dimension(:), allocatable :: points  ! the polygon points
  end type polygon
 
contains
 
  ! allocates a polygon structure which can contain np points
  function create_polygon(nump) result(ppol)
    integer, intent(in) :: nump  ! number of points in polygon created
    type(polygon) :: ppol

    ! local variable
    integer :: alloc_stat

    allocate(ppol%points(nump), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to allocate memory for Polygon"
      ppol%np = 0
    else
      ppol%np = nump
    end if 
  end function create_polygon
 
  ! deallocates a polygon structure
  subroutine destroy_polygon(pol)
    type(polygon), intent(inout) :: pol

    ! local variable
    integer :: dealloc_stat

    deallocate(pol%points, stat=dealloc_stat)
    if( dealloc_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to deallocate memory for Polygon"
    end if
  end subroutine destroy_polygon

end module Polygons_Mod
