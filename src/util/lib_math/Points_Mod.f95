!$Author$
!$Date$
!$Revision$
!$HeadURL$

! http://rosettacode.org/wiki/Closest_pair_problem/Fortran

module Points_Mod
  implicit none
 
  type point
     real :: x, y
  end type point
 
  interface operator (-)
     module procedure pt_sub
  end interface
 
  interface len
     module procedure pt_len
  end interface
 
  public :: point
  private :: pt_sub, pt_len
 
contains
 
  function pt_sub(a, b) result(c)
    type(point), intent(in) :: a, b
    type(point) :: c
 
    c = point(a%x - b%x, a%y - b%y)
  end function pt_sub
 
  function pt_len(a) result(l)
    type(point), intent(in) :: a
    real :: l
 
    l = sqrt((a%x)**2 + (a%y)**2)
  end function pt_len
 
end module Points_Mod
