! Defintion of subregion variable
     
      integer subidx(mnsub)
!      integer subgrid(imax*jmax)
      character*124 soilName(mnsub)      
      character*124 cmzName(mnsub)
      
      integer imax_sub(mnsub)
      integer jmax_sub(mnsub)
! subgrid - grids in a region with i * j  
!  regionID - subregion id read in from file
!  soilName - Name of soil file 
! cmzName - Name of crop management file

      common /subglobe/subidx,soilName,cmzName,imax_sub,jmax_sub

