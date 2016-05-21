!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains variables representing the crop
!     properties for each subregion from the last time the crop growth
!     routines were called. When the crop growth model is called
!     again these values represent the previous day. These are compared
!     to the state variables to determine any operations done on the
!     crop and the resetting of the crop state required to properly
!     simulate the crop response to the operation.

!     Parameter include files required:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      real             prevcancov(mnsub)

      common / prevderiv /                                              &
     &           prevcancov

      save :: /prevderiv/

!     + + + VARIABLE DEFINITIONS + + +

!     prevcancov - crop canopy cover (fraction)

