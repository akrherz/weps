!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine updres(isr, residue, restot)

      use weps_interface_defs, only: poolupdate
      use biomaterial, only: biomatter, biototal

!     + + +   ARGUMENT DECLARATIONS + + +
      integer isr
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot

!     Update geometric properties of the decomp residue pools

      include 'p1werm.inc'
      include 's1layr.inc'

!     + + + END SPECIFICATIONS + + +

      ! update derived globals for all decomposition pools
      call poolupdate(nslay(isr), aszlyd(1:size(aszlyd,1),isr), residue, restot)

      return
      end
