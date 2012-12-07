!$Author: jcgao $
!$Date: 2012-08-22 14:59:37 -0500 (Wed, 22 Aug 2012) $
!$Revision: 12359 $
!$HeadURL: https://wind.eweru.ksu.edu/svn/code/weps1/branches/weps.src.subregion/src/lib_erosion/sbgrid.for $
!**********************************************************************
!     subroutine subr_dim
!**********************************************************************
      subroutine subr_dim(isr)
!
!     +++ PURPOSE +++
!     to calculate the dimension of subregion with imax_sub and jmax_sub
!     A max 'interior' square grid of 29X29 is assigned-no barriers
!     A max 'interior' rectangular grid of 59X59 is assigned barriers
!     to assign subregion index no. to each grid point.
!
!     +++ ARGUMENT DECLARATION +++

      integer isr
!     +++ LOCAL DEFINITIONS +++

!     ix    - grid interval in x-direction (m)
!     jy    - grid interval in y-direction (m)
!     dxmin - minimum grid interval (m)
!     csr   - current subr. index at grid point i,j
!     icsr  - same as csr but not an array
!     i,j   - do loop indexes
!
!     + + + GLOBAL COMMON BLOCKS + + +
!     imax_sub  - no. grid intervals in x-direction
!     jmax_sub  - no. grid intervals in y-direction.
      include  'p1werm.inc'
      include  'm1geo.inc'
      include  'm1subr.inc'
      include 'subglobe.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +
      include  'erosion/m2geo.inc'
      include  'erosion/e2grid.inc'
!
!     +++ LOCAL VARIABLES +++
      integer  ngdpt
      integer  i, j
      real     dxmin, lx, ly
!
!     +++ END SPECIFICATIONS +++
!
!     set min grid spacing
       dxmin = MIN_GRID_SP
!     set max no. of grid points with no barrier

       ngdpt = N_G_DPT
!     barriers?
       if (nbr .gt. 0) then
!        find shortest barrier to determine dxmin
         do 5 i=1,nbr
            if (amzbr(i) > 0.0) then    !Check for zero height barriers
               dxmin = min(dxmin, 5.0*amzbr(i))
            endif
    5    continue
         ngdpt = B_G_DPT  !default to this value if a barrier exists
       endif

     
! Change lx and ly into subregion length here by JG 
      lx = amxsr(1,2,isr)-amxsr(1,1,isr)
      ly = amxsr(2,2,isr)-amxsr(2,1,isr)

!
! change imax and jmax size into subregion size by JG
!        case where lx > ly
      if ( lx .gt. ly)then
        imax_sub(isr)  = int ( lx / dxmin)
        imax_sub(isr) = min(imax,ngdpt)
        imax_sub(isr) = max(imax,2)
!     calculate spacing for square or with barriers a rectangular grid
!        ix  = lx / (imax_sub(isr) - 1)

         if (nbr .gt. 0) then
           jmax_sub(isr)  = int (ly / dxmin)
           jmax_sub(isr)  = min(jmax_sub(isr), ngdpt)
         else
           jmax_sub(isr) = anint(ly/ix) + 1
         endif

        jmax_sub(isr) = max(jmax_sub(isr),2)
!        jy   = ly/(jmax_sub(isr) - 1)

!        case where lx = ly or lx < ly
      else
        jmax_sub(isr)  = int (ly / dxmin)
        jmax_sub(isr) = min(jmax_sub(isr),ngdpt)
        jmax_sub(isr) = max(jmax_sub(isr),2)
        jy   = ly / (jmax_sub(isr) - 1)

        if (nbr .gt. 0) then
           imax_sub(isr)  = int (lx / dxmin)
           imax_sub(isr)  = min(imax_sub(isr),ngdpt)
        else
           imax_sub(isr) = anint(lx/jy) + 1
        endif
        imax_sub(isr) = max(imax_sub(isr),2)
        ix = lx/(imax_sub(isr)-1)

      endif
    
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
