!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbinit
!**********************************************************************

      subroutine sbinit( subrsurf )

!     +++ purpose +++
!     Input subregion values of variables from other submodels
!     to the grid points of the erosion submodel which erosion changes
!     Initialize output grid array
!     Calc. soil fraction of 4 dia. from asd, & rr shelter angles

      use weps_interface_defs
      use erosion_data_struct_defs

!     + + + ARGUEMENT DECLARATIONS + + +
      type(subregionsurfacestate), dimension(:), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'w1clig.inc'
!
!     + + +  LOCAL COMMON BLOCKS + + +
      include 'erosion/p1erode.inc'
      include 'erosion/m2geo.inc'
      include 'erosion/e2grid.inc'
      include 'erosion/s2agg.inc'
      include 'erosion/s2surf.inc'
      include 'erosion/s2sgeo.inc'
      include 'erosion/e2erod.inc'
!
!
!     + + + LOCAL VARIABLES + + +
      integer  icsr, i, j

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     icsr  = index of current subregion
!     i,j   = grid cell x,y coordinates

!     + + + SUBROUTINES CALLED + + +
!     sbsfdi
!     sbpm10
!     + + + END SPECIFICATION + + +

!     calculate abrasion and pm10 parameters    edit LH 3-4-05
      do icsr = 1, nsubr
         call sbpm10( subrsurf(icsr)%bsl(1)%aseags, subrsurf(icsr)%asecr, subrsurf(icsr)%bsl(1)%asfcla, &
              subrsurf(icsr)%bsl(1)%asfsan, awzypt, subrsurf(icsr)%acanag, subrsurf(icsr)%acancr, &
              subrsurf(icsr)%asf10an, subrsurf(icsr)%asf10en, subrsurf(icsr)%asf10bk )

         ! calculate fraction less than diameter from asd
         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 0.01, subrsurf(icsr)%sfd1 )
         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 0.1, subrsurf(icsr)%sfd10 )
         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 0.84, subrsurf(icsr)%sfd84 )
         ! store initial sf84
         sf84ic = subrsurf(icsr)%sfd84
         sf84ic = min(0.9999, max(sf84ic,0.0001))            !set limits
         ! store initial sf10
         sf10ic = subrsurf(icsr)%sfd10

         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 2.0, subrsurf(icsr)%sfd200 )
      end do

      do 20 j = 1, jmax-1
      do 10 i = 1, imax-1

!     determine subregion
      icsr = csr(i,j)
!     input variables to grid cells
      sf1  (i,j) = subrsurf(icsr)%sfd1
      sf10 (i,j) = subrsurf(icsr)%sfd10
      sf84 (i,j) = subrsurf(icsr)%sfd84
      sf200(i,j) = subrsurf(icsr)%sfd200
!     edit ljh - 1-22-04
      svroc(i,j) = subrsurf(icsr)%bsl(1)%asvroc    ! if ifc has surface rock, 1st index maybe 0.
!
      szcr(i,j)  = subrsurf(icsr)%aszcr
      sfcr(i,j)  = subrsurf(icsr)%asfcr
      smlos(i,j) = subrsurf(icsr)%asmlos
      sflos(i,j) = subrsurf(icsr)%asflos
!
      szrgh(i,j) = subrsurf(icsr)%aszrgh

      !initialize RR values for each grid cell
      slrr(i,j)  = subrsurf(icsr)%aslrr

      if (slrr(i,j) < SLRR_MIN) then
          slrr(i,j) = SLRR_MIN
      else if (slrr(i,j) > SLRR_MAX) then
          slrr(i,j) = SLRR_MAX
      endif

      dmlos(i,j) = 0.0
      smaglos(i,j) = 0.0
      smaglosmx(i,j) = 0.0
      sf84mn(i,j) = 0.0
!
!     initialize output array- now in sbigrd
!      egt(i,j)    = 0
!      egtss(i,j)  = 0
!      egt10(i,j)  = 0
!
   10 continue
   20 continue
!
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
