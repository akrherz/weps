!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'm1subr.inc'

!     This common block contains the state variables relating to
!     the subregion parameters for WERM.

!     Parameter include files:c:\main\inc\p1werm.inc


!     + + + VARIABLE DECLARATIONS + + +

      integer          nsubr
      integer          am0csr
      integer          amnryr(mnsub)
      real             amrslp(mnsub)
      real             SFCov(mnsub)
      real             WaterErosion(mnsub)
      real             SoilRockFragments(mnsub)
      character*160    am0sid(mnsub)
      character*80     am0tax(mnsub)
      character*80     am0localphase(mnsub)
      character*20     SoilLossTol(mnsub)

      common   / m1subr /                                               &
     &                   nsubr,                                         &
     &                   am0csr,                                        &
     &                   amnryr,                                        &
     &                   amrslp,                                        &
     &                   SFCov,                                         &
     &                   WaterErosion,                                  & 
     &                   SoilRockFragments,                             & 
     &                   am0sid,                                        &
     &                   am0tax,                                        &
     &                   am0localphase,                                 &
     &                   SoilLossTol

      save :: /m1subr/

!     + + + VARIABLE DEFINITIONS + + +

!    nsubr  - Number of subregions
!    am0csr - Current subregion
!    amnryr - Counter for current rotation year

!    amrslp - Average subregion slope (m/m)
!    SFCov - NRCS "Surface Fragment Cover" or "Surface Layer Fragment" fraction (%)
!    WaterErosion - water erosion soil loss
!    SoilRockFragments - fraction of soil volume that is soil rock fragments (m^3/m^3)

!    am0sid  - soil identification
!    am0tax  - soil taxonomic order
!    am0localphase  - soil local phase
!    SoilLossTol  - NRCS assigned "soil loss tolerance" value


