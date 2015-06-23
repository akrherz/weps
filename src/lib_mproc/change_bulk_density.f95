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
  integer :: nlay       ! number of soil layers used
  real :: u             ! Change coefficient: positive values loosen, negative values compact
  real :: tillf         ! fraction of soil area tilled by the machine
  real :: density(mnsz) ! present soil bulk density
  real :: sbd(mnsz)     ! settled soil bulk density
  real :: proc_bd_wc(mnsz) ! proctor soil bulk density adjusted for water content
  real :: laythk(mnsz)  ! layer thickness

  ! + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
  ! mnsz        - max number of soil layers

  ! + + + LOCAL VARIABLES + + +
  integer :: i       ! loop variable on layers 
  real :: dum(mnsz)  ! dummy variable used in calculating the adjusted density

  ! + + + END SPECIFICATIONS + + + 

  ! perform the compact process on the layers in a subregion 

  do i=1,nlay
    if( u .gt. 0.0 ) then
      ! loosening - performed equally on all depths
      dum(i)= density(i)-((density(i)-(2.0/3.0)*sbd(i))*u*tillf)
    else if( u .lt. 0.0 ) then
      ! compaction - linear decrease to depth of influence
      dum(i)= density(i)+((proc_bd_wc(i)-density(i))*u*tillf)
    end if
    laythk(i)=laythk(i)*(density(i)/dum(i))
    density(i)=dum(i)
  end do
end subroutine change_bulk_density
