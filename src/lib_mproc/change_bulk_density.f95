!$Author$
!$Date$
!$Revision$
!$HeadURL$

subroutine change_bulk_density( u,tillf,nlay,density,sbd,laythk )

  ! + + + PURPOSE + + +
     
  ! This subroutine reads in the array(s) containing the components 
  ! that need to be loosen/compact(ed). 

  ! + + + KEYWORDS + + +
  ! loosen/compact, tillage 

  include 'p1werm.inc'

  ! + + + ARGUMENT DECLARATIONS + + +
  integer :: nlay      ! number of soil layers used
  real :: u            ! loosening coefficient
  real :: tillf        ! fraction of soil area tilled by the machine
  real :: density(mnsz) ! present soil bulk density
  real :: laythk(mnsz) ! layer thickness
  real :: sbd(mnsz)    ! settled soil bulk density

  ! + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
  ! mnsz        - max number of soil layers

  ! + + + LOCAL VARIABLES + + +
  integer :: i       ! loop variable on layers 
  real :: dum(mnsz)  ! dummy variable used in calculating the mass in a subregion

  ! + + + END SPECIFICATIONS + + + 

  ! perform the compact process on the layers in a subregion 

  do i=1,nlay
    dum(i)= density(i)-((density(i)-(2.0/3.0)*sbd(i))*u*tillf)
    laythk(i)=laythk(i)*(density(i)/dum(i))
    density(i)=dum(i)
  end do
end subroutine change_bulk_density
