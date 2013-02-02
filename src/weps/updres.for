!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine updres(isr, residue, restot)

      use biomaterial, only: biomatter, biototal

!     + + +   ARGUMENT DECLARATIONS + + +
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot

!     Update geometric properties of the decomp residue pools

      include 'p1werm.inc'
      include 'p1const.inc'
      include 's1layr.inc'
      include 'd1glob.inc'
      include 'd1gen.inc'

!     + + + ARGUMENT DECLARATIONS + + +

      integer isr

!     + + + END SPECIFICATIONS + + +

      ! update derived globals for all decomposition pools
      call poolupdate(nslay(isr), aszlyd(1,isr), residue, restot)

      return
      end
