!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
      subroutine erodinit
!
!     +++ PURPOSE +++
!
!     Controls calls to subroutines that:
!       create the Erosion submodel grid (sbgrid)    
!       initialize Erosion submodel output array to zero (sbigrd).
!       calculate normalized effect of hills on friction velocity 
!        on grid for each wind direction (not activated)
!       calculate normalized effect of barriers on friction velocity
!        on grid for each wind direction (sbbr)
!       initialize reporting variables that need to have a value even
!        when erosion is not being called.

!     + + + Modules Used + + +
      use Points_Mod
      use Polygons_Mod
      use pnpoly_mod
      use subregions_mod

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  'm1flag.inc'
      include  'm1geo.inc'
      include  'm1subr.inc'
      include  'erosion/m2geo.inc'
      include  'erosion/e2grid.inc'  !needed for initialization of csr(*,*)
      include  'erosion/threshold.inc'
      include  's1surf.inc'
      
!     +++ SUBROUTINES CALLED +++
!     sbgrid
!     sbigrd
!     sbhill (not activated)
!     sbbr

!     +++ LOCAL VARIABLES +++
      integer i, j, sr
      type(point) :: centroid

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     nbr  = number of barriers (from m1geo.inc)

!     +++ END SPECIFICATIONS +++

      ! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         ! check to see if grid dimensions specified via cmdline args
         if ((xgdpt > 0) .and. (ygdpt > 0)) then
           imax = xgdpt + 1
           jmax = ygdpt + 1
           ix = (amxsim(1,2) - amxsim(1,1)) / xgdpt
           jy = (amxsim(2,2) - amxsim(2,1)) / ygdpt
         else          !use Hagen's grid dimensioning as the default
           call sbgrid
         endif

         ! assign subregion number to each grid cell
         ! code lifted from sbgrid because it is initialized there - LEW
         do j = 1, jmax-1
           do i = 1, imax-1
             ! The grid cell is assumed rectangular. Use centroid of grid cell
             ! with subregion polygon to select grid cell subregion
             centroid%x = 0.5 * (i-1+i) * ix
             centroid%y = 0.5 * (j-1+j) * jy
             do sr = 1,nsubr
               ! Check if it is inside subregion polygon
               if( pnpoly(centroid, subr_poly(sr)) .ge. 0) then
                  ! centroid of grid cell is inside or on edge of subregion polygon
                  ! set subregion index
                  csr(i,j) = sr
                  ! default to first polygon if on edge by exiting the subregion do loop
                  exit
               end if
             end do
             ! check final status
             if( csr(i,j) .eq. 0 ) then
                 ! this grid cell not assigned to a subregion
                 write(*,*) 'ERROR: no subregion for grid cell ',i,':',j
                 write(*,*) 'Subregion coverage is not complete'
                 stop
             end if
           end do          
        end do

         ! set grid cell output arrays to zero
         call sbigrd

         ! check for hills - sbhill not implemented
!        if (nhill .gt. 0) then
!        call sbhill
!        endif

         ! check for barriers
         if (nbr .gt. 0) then
         call sbbr
         endif

         ! Turn off grid creation flag
         am0eif = .false.
      endif

      do sr = 1, nsubr
           ! initalize erosion threshold trigger variables
           ne_erosion(sr) = 0
           ne_snowdepth(sr) = 0

           ne_wus_anemom(sr) = 0
           ne_wus_random(sr) = 0
           ne_wus_ridge(sr) = 0
           ne_wus_biodrag(sr) = 0
           ne_wus(sr) = 0

           ne_bare(sr) = 0
           ne_flat_cov(sr) = 0
           ne_surf_wet(sr) = 0
           ne_ag_den(sr) = 0
           ne_wust(sr) = 0

           ne_sfd84(sr) = 0
           ne_asvroc(sr) = 0
           ne_wzzo(sr) = 0
           ne_sfcv(sr) = 0

           ! initialize surface condition reporting values
           acanag(sr) = 0
           acancr(sr) = 0
      end do

      return
      end

