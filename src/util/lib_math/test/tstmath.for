!
!$Author$
!$Date$
!$Revision$
!$HeadURL$

      program tstmath

      integer i

      real erf,z

C     -------------------------------------------------------------------------
C
      do 20 i=0, 1500 
         z = z + .001
         print*, z, erf(z)
20    continue

      stop
      end
