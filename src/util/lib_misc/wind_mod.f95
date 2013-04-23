!$Author$
!$Date$
!$Revision$
!$HeadURL$

module wind_mod

  contains

    subroutine sbwind( wustfl, awu, ntstep, intstep, rusust, subrsurf, cellstate)

!     +++ PURPOSE +++
!     to update wzzo at each grid point;
!     To update  soil friction velocity on each grid point
!     and modify it for barriers and hills;
!     To initialize en thresh. and cp thresh. fr. velocites on grid;
!     To calculate max ratios of friction velocity to threshold
!     friction velocity

      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate, anemht, awzzo, wzoflg
      use grid_mod, only: kbr, imax, jmax
      use barriers_mod, only: barrier

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(in) :: wustfl
      integer, intent(in) :: intstep  ! current index of ntstep thru time
      integer, intent(in) :: ntstep   ! max. no. of time steps in day
      real, intent(in) :: awu       ! input wind speed driving EROSION submodel (m/s).
      real, intent(out) :: rusust    ! max ratio of friction velocity to thresh. friction vel.
      type(subregionsurfacestate), dimension(:), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     +++ LOCAL VARIABLES +++
      integer i,j, k
      integer :: icsr     ! index of current subregion.
      real wzorg, wzorr, wzzo, wzzov
      real at, rintstep, brcd
      real wubsts, wucsts, wucwts, wucdts, sfcv ! these are placeholders in call to sbwust are are not used anywhere else.

!     + + + END SPECIFICATIONS + + +

      rusust = 0.1

      ! loop through grid interior to update
      do i = 1, imax-1
        do j = 1, jmax-1

          ! assign subregion index for grid point
          icsr = cellstate(i,j)%csr

          ! calculate "effective" biomass drag coefficient
          brcd = biodrag( subrsurf(icsr)%adrlaitot, subrsurf(icsr)%adrsaitot, subrsurf(icsr)%acrlai, subrsurf(icsr)%acrsai, &
                          subrsurf(icsr)%ac0rg, subrsurf(icsr)%acxrow, subrsurf(icsr)%aczht, cellstate(i,j)%szrgh )

          call sbzo( subrsurf(icsr)%sxprg, cellstate(i,j)%szrgh, cellstate(i,j)%slrr, subrsurf(icsr)%abzht, brcd, &
                     wzoflg, wzorg, wzorr, wzzo, wzzov, awzzo )

          ! update surface (below canopy) friction velocity
          cellstate(i,j)%wus = sbwus( anemht, awzzo, awu, wzzov, brcd )

          ! correct friction velocity for hills
          ! if (nhill .ne. 0 ) then
          !   cellstate(i,j)%wus = cellstate(i,j)%wus * w0hill(i,j,kbr)
          ! endif

          ! correct friction velocity for barriers
          if ( allocated(barrier) ) then
            cellstate(i,j)%wus = cellstate(i,j)%wus * cellstate(i,j)%w0br(1)
          endif

          if (wustfl .eq. 1) then
            ! update threshold friction velocities and loose erodible material
            ! calculate hour k for surface water content
            rintstep = intstep
            k = int(rintstep*23.75/ntstep) + 1

            call sbwust( cellstate(i,j)%sf84, subrsurf(icsr)%bsl(1)%asdagd, cellstate(i,j)%sfcr, cellstate(i,j)%svroc, &
                 cellstate(i,j)%sflos, subrsurf(icsr)%abffcv, wzzo, subrsurf(icsr)%ahrwc0(k), subrsurf(icsr)%bsl(1)%ahrwcw, &
                 subrsurf(icsr)%sf84ic, subrsurf(icsr)%bsl(1)%asvroc, & 
                 cellstate(i,j)%wust, cellstate(i,j)%wusp, cellstate(i,j)%wusto, &
                 wubsts, wucsts, wucwts, wucdts, sfcv)

            ! calculate: smaglosmx; update: smaglos, sf84mn
            call sbaglos( cellstate(i,j)%wus, cellstate(i,j)%wust, cellstate(i,j)%wusto, &
                          subrsurf(icsr)%sf84ic, subrsurf(icsr)%bsl(1)%asvroc, &
                          cellstate(i,j)%smaglosmx, cellstate(i,j)%smaglos, cellstate(i,j)%sf84mn, cellstate(i,j)%sf84 )

          endif

          at = cellstate(i,j)%wus/cellstate(i,j)%wust
          rusust = amax1(rusust, at)

        end do
      end do

    end subroutine sbwind

    subroutine sbzo (sxprg, szrgh, slrr, bbzht, brcd, wzoflg, wzorg, wzorr, wzzo, wzzov, awzzo)

!     +++ PURPOSE +++
!     Calc. aerodynamic roughness parm., wzzo, with no standing biomass
!           wzzo is used by sbwust

!     Calc. aerodynamic roughness parm. as wzzov, if standing biomass
!              else let wzzov = wzzo
!         wzzov is used by sbwus

!     set anem aero. roughness and field roughness equal when anem. at
!         the field site, ie. wzoflg = 1
!     to calculate aerodynamic roughness of vegetation canopy.
!     Ref. Trans ASAE 31(3):769-775, Armbrust and Bilbro, 1995

      use p1erode_def, only: WZZO_MIN, WZZO_MAX
      use p1unconv_mod, only: mtomm

!     +++ ARGUMENT DECLARATIONS +++
      real, intent(in) :: sxprg  ! row/dike spacing parallel the wind (mm)
      real, intent(in) :: szrgh  ! ridge height (mm)
      real, intent(in) :: slrr   ! random roughness (mm)
      real, intent(in) :: bbzht  ! composite average biomass height (m)
      real, intent(in) :: brcd   ! biomass drag coefficient
      integer, intent(in) :: wzoflg ! flag=0 - anemometer at station
                                    ! flag=1 - anemometer at field
      real, intent(out) :: wzzo   ! aerodynamic roughness of surface below canopy (mm)
      real, intent(out) :: wzorg  ! aerodynamic roughness of ridge
      real, intent(out) :: wzorr  ! aerodynamic roughness of random roughness
      real, intent(out) :: wzzov  ! aerodynamic roughness length of canopy (mm)
      real, intent(out) :: awzzo  ! aerodynamic roughness at anemom. site (mm)

!     +++ LOCAL VARIABLES +++
      real :: hl    ! ratio of ridge height to parallel ridge spacing
      real :: bht   ! biomass height (mm)

!     +++ END SPECIFICATIONS +++
      ! Note: wzoflg should be set to 1 and anemomht changed if the anemomenter is at the field site
      ! to obtain correct values from SBZO

      ! calc. for ridge aerodynamic roughness
      if (szrgh .gt. 5.0) then
        hl   = szrgh / sxprg
        ! winds are never continually normal to ridges, so restrict hl.
        hl = min(0.20,hl)
        wzorg = szrgh * 1/(-64.1+135.5*hl+(20.84/sqrt(hl)))
      else
        wzorg = 0.
      endif

      ! calculation for random aerodynamic roughness
      wzorr = slrr*0.3
      !set upper and lower limits on aerodynamic roughness
      wzorr = min(WZZO_MAX, wzorr)   ! RR <= ~100.0mm
      wzorr = max(wzorr, WZZO_MIN)   ! RR >= ~1.67mm

      ! estimate combined ridge and random aerodynamic roughness
      ! (later- no data sets at present) chose the largest of the two.
      wzzo = max (wzorg, wzorr)

      ! calculate aerodynamic roughness of vegetation, if present

      ! convert biomass height to mm
      bht = bbzht * mtomm

      ! calculate roughness length of canopy ( in mm)
      if (brcd .gt. 0.1) then
        wzzov = bht * 1/(17.27-(1.254*alog(brcd)/brcd)-(3.714/brcd))
      else if( (bht .gt. 5.0) .and. (brcd .gt. 0.001) ) then
          ! wzzov = bht*exp(alog(wzzo/bht) + (alog(0.11*bht/wzzo) * alog(brcd/0.01))/2.3) ! caused Simon's instability
        wzzov = bht*(wzzo/bht+((0.11-wzzo/bht)/4.60517)*alog(brcd/0.001))
      else
          wzzov = 0.0
      endif

      ! choose the maximum of canopy or surface roughness
      wzzov = max(wzzov, wzzo)

      if (wzoflg .eq. 1) then
         ! anemom. in field set awzzo to wzzov
         awzzo = wzzov
      endif

    end subroutine sbzo

    function sbwus( anemht, awzzo, awu, wzzov, brcd ) result( wus )

!     +++ PURPOSE +++
!     To calculate subregion, friction velocity, given station
!     anemometer height, surface roughness, wind speed; and subregion
!     aerodynamic roughness.

!     if standing biomass present, then calculate friction velocity
!     at surface below the canopy (wus).

!     +++ ARGUMENT DECLARATIONS +++
      real, intent(in) :: anemht ! parameter, anemometer height of input wind speed (m).
      real, intent(in) :: awzzo  ! parameter, surface aerodynamic roughness at input wind speed location (mm).
      real, intent(in) :: awu    ! input wind speed driving EROSION submodel (m/s).
      real, intent(in) :: wzzov  ! subregion aerodynamic roughness (mm).
      real, intent(in) :: brcd   ! biomass drag coefficient
      real :: wus    ! subregion soil surface friction velocity (m/s) i.e. below canopy, if one exists.

!     +++ LOCAL VARIABLES +++
      real :: wusst  ! station (ie. anemomter location friction velocity)
      real :: wusv   ! friction veolocity value retained for check against below canopy value

!     +++ END SPECIFICATIONS +++

!     note:  wzoflg should be set to 1 and anemht set to correct height if anemometer is at field site
!             to obtain correct values from SBWUS or read as input data in stand-alone EROSION.

      ! Calc station (input wind speed location) friction velocity
      wusst = awu*0.4/alog(anemht*1000./awzzo)

      ! calc subregion friction velocity
      wus = wusst * (wzzov/awzzo)**0.067

      ! if standing biomass, calculate wus below canopy
      if (brcd .gt. 0.0001 ) then
         wusv = wus

        ! calculate friction velocity below canopy
        if( brcd.gt.2.56) then       !check to avoid underflow
            wus = wusv * 0.25*exp(-brcd/0.356)
        else
            wus = wusv*(0.86*exp(-brcd/0.0298)+0.25*exp(-brcd/0.356))
        endif
        wus = min(wus,wusv)
      endif

    end function sbwus

    subroutine sbwust (sf84, sdagd, sfcr, svroc, sflos, bffcv, wzzo, hrwc, hrwcw, sf84ic, asvroc, &
                       wust, wusp, wusto, wubsts, wucsts, wucwts, wucdts, sfcv)

!     + + + PURPOSE + + +
!     Calculate threshold soil surface friction velocity
!     as a function of ag size dist., aerodynamic roughness,
!   	  crust, rock, & flat biomass cover,and soil surface wetness

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: sf84   ! soil mass fraction in surface layer < 0.84 mm
      real, intent(in) :: sdagd  ! aggregate density (Mg/m^3)
      real, intent(in) :: sfcr   ! fraction of crust cover.
      real, intent(in) :: svroc  ! updated surface vol. rock > 2.0 mm (m^3/m^3).
      real, intent(in) :: sflos  ! soil fraction loose material cover on crust (m^3/m^3)
      real, intent(in) :: bffcv  ! biomass fraction of flat cover (m^2/m^2)
      real, intent(in) :: wzzo   ! aerodynamic roughness length of surface below canopy(mm)
      real, intent(in) :: hrwc   ! soil water content on mass basis (at surface) (kg/kg).
      real, intent(in) :: hrwcw  ! soil water content on mass basis, at -1.5 MPa (kg/kg)
      real, intent(in) :: sf84ic ! surface soil fraction <0.84 mm initial condition
      real, intent(in) :: asvroc ! initial surface soil volume roc fraction
      real, intent(out) :: wust   ! friction velocity theshold for en (m/s)
      real, intent(out) :: wusp   ! friction velocity threshold of tp and trans. cap.(m/s)
      real, intent(out) :: wusto  ! friction velocity threshold of bare smooth surface with sf84ic (for in sbaglos) (m/s)
      real, intent(out) :: wubsts ! bare soil threshold friction velocity
      real, intent(out) :: wucsts ! surface cover addition to bare soil threshold friction velocity
      real, intent(out) :: wucwts ! surface wetness addition to bare soil threshold friction velocity
      real, intent(out) :: wucdts ! aggregate density addition to bare soil threshold friction velocity
      real, intent(out) :: sfcv   ! fraction bare surface that does not emit

!     + + + LOCAL VARIABLES + + +
      real  b1, b2, wet_rat

!     + + + END SPECIFICATIONS + + +

      ! calculate threshold (wusto) of bare, smooth surface with sf84ic for use in sbaglos edit 6-27-06 LH
      b1 = -0.179 + 0.225*(log(1.5))**0.891  ! approx -0.078337
      b2 = 0.3 + 0.06*0.5**1.2               ! approx  0.3261165
      wusto = 1.7 - 1.35 * exp( -b1 - b2*( (1-sf84ic)*(1-asvroc) + asvroc )**2 )

      ! calc fraction bare surface that does not emit
      sfcv = ((1 - sfcr)*(1 - sf84) + sfcr - sfcr*sflos)*(1 - svroc) + svroc

      ! to avoid a zero value
      sfcv = sfcv + 0.0001
      ! check for total cover.
      !  if (sfcv < 1.0) then
      !   calculate bare surface static threshold friction velocity
      b1 = -0.179 + 0.225*(alog(1 + wzzo))**0.891
      b2 = 0.3 + 0.06*wzzo**1.2
      wubsts = 1.7 - 1.35*exp(-b1-b2*sfcv**2)
      wusp   = 1.7 - 1.35*exp(-b1-b2*0.4**2)
      ! else
      !    wubsts = 1.85
      !    wusp   = 1.80
      ! endif

      ! edit 07-17-01
      ! calc change in threshold with flat cover
      if (bffcv .gt. 0) then
         wucsts = (1 - exp(-1.2*bffcv))*(exp(-0.3*sfcv))
      else
         wucsts = 0.
      endif

      ! calc change in threshold vel with wetness
      wet_rat = hrwc / hrwcw
      !  if ( wet_rat .gt. 0.3) then
      !  if ( wet_rat .gt. 0.25) then  ! triggers at 1/4 of the wilting pt
      if ( wet_rat .ge. 0.0) then
          ! wucwts = 0.48 * wet_rat
          ! this modified curve closely matches the previous linear realationship
          ! in the 0.25 to 0.8 range, which is where the measured data are.
          ! (make sure the reference to the measured data is in the Tech Doc.)
          ! It is apparently not possible to measure threshold effects below
          ! 0.25 wetness ratio so whether the relationship should go smoothly
          ! through zero for a wetness ratio below 0.25 has not been determined.
          !!!!!! this function has a singularity and goes negative for
          ! values of wet_rat > 1.307674238
          ! wucwts = 1.0 / (11.906541 - 10.41204 * wet_rat**0.5)
          !!!! this function is better behaved
          wucwts = 0.58 * (exp(wet_rat) - 1.0 - 0.7*wet_rat*wet_rat)
      else
          wucwts = 0.
      endif

      ! After consultation with LH, it was decided that the adjustment
      ! to the friction velocity terms due to Agg. Density would be
      ! retained in the erosion code, but WEPS would default it to a
      ! value of 1.8 at this time.  The standalone code would then
      ! be able to modify that value if desired (with 1.8 being the
      ! suggested default value) - Mar. 15, 2006 - LEW

      !correct for ag density, (use constant sdagd=1.8, 5/28/03 LH)
      ! wucdts = -0.05275  !adjustment value if sdagd == 1.8

      wucdts = 0.3*(sqrt(sdagd/2.65)-1.0)


      ! calc final static threshold friction velocity
      wust = wubsts + wucsts + wucwts + wucdts
      wusp = wusp   + wucsts + wucwts + wucdts

    end subroutine sbwust

    subroutine sbzdisp( wzoflg, brcd, bbzht, bczht, awzdisp, wzdisp )

!     +++ PURPOSE +++
!     Calc. zero plane displacement (mm)

!     set field zero plane displacement equal to Anemometer zero plane
!     displacement when anem. at the field site, ie. wzoflg = 1

      ! using equation from: Raupach, M.R. 1994. Simplified Expressions for
      ! Vegetation Roughness Length and Zero-Plane Displacement as functions
      ! of Canopy Height and Area index. Boundary Layer meteorology 71:211-216.

      use p1unconv_mod, only: mtomm

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(in) :: wzoflg ! flag=0 - weather measurements are from a distant station
                                    ! flag=1 - weather measurements are in field
      real, intent(in) :: brcd      ! biomass drag coefficient (or "effective" biomass silhouette area index)
      real, intent(in) :: bbzht     ! composite average residue height (m)
      real, intent(in) :: bczht     ! crop height (m)
      real, intent(out) :: awzdisp  ! zero plane displacement at weather location (mm)
      real, intent(out) :: wzdisp   ! zero plane displacement at location (mm)

!     +++ LOCAL VARIABLES +++
      real  bht

!     +++ LOCAL VARIABLE DEFINITIONS +++
!     bht   - biomass height (mm)

!     +++ END SPECIFICATIONS +++

      ! find maximum biomass height and convert to mm
      bht = max(bbzht, bczht) * mtomm

      ! use silhouette area index and biomass height to find zero plane displacement
      if( brcd .gt. 1.0e-10 ) then
          wzdisp = bht * (1.0 - (1.0 - exp( -(15.0*brcd)**0.5) ) / (15.0*brcd)**0.5)
      else
          wzdisp = 0.0
      end if

      if (wzoflg .eq. 1) then
         ! anemom. in field, set weather displacement to field displacement
         awzdisp = wzdisp
      endif

    end subroutine sbzdisp

    function biodrag (bdrlai, bdrsai, bcrlai, bcrsai, bc0rg, bcxrow, bczht, bszrgh) result( bio_drag )

!     + + + PURPOSE + + +
!     BIODRAG: combine effects of leaves and stems on drag coef.

!     Leaves are less effective at reducing the wind speed than
!     stems.  Three effects are simulated: 1. streamlining of leaves,
!     2. leaf sheltered in furrow, and
!     3.leaf area confined in wide rows that act as wind barriers.
!     This function combines these effects into a single
!     value for use by other routines. May still be too large.

      use p1unconv_mod, only: mmtom

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: bdrlai   ! residue leaf area index (sum of all pools)(m^2/m^2)
      real, intent(in) :: bdrsai   ! residue stem silhouette area index (sum of all pools)(m^2/m^2)
      real, intent(in) :: bcrlai   ! crop leaf area index (m^2/m^2)
      real, intent(in) :: bcrsai   ! crop stem silhouette area index (m^2/m^2)
      integer, intent(in) :: bc0rg    ! crop seed location flag (0 = in furrow, 1 = on ridge)
      real, intent(in) :: bcxrow   ! crop row spacing (m)(0 = broadcast)
      real, intent(in) :: bczht    ! crop biomass height (m)
      real, intent(in) :: bszrgh   ! ridge height (mm)

      real :: bio_drag  ! drag coefficient (no units)

!     + + + PARAMETERS + + +
      real fur_dis      ! coefficient for discounting drag of plant in furrow bottom
      parameter( fur_dis = 0.5 )

!     + + + LOCAL VARIABLES + + +
      real :: red_lai     ! reduced leaf area index (m^2/m^2)
      real :: red_sai     ! reduced stem area index (m^2/m^2)
      real :: red_fac     ! reduction factor

!     + + + END SPECIFICATIONS + + +

      ! place crop values in temporary variables
      red_lai = bcrlai
      red_sai = bcrsai

      ! check for crop biomass position with respect to the ridge
      if(bc0rg .eq. 0) then
          ! biomass in furrow
          ! test plant height and ridge height for minimums
          if( bczht .gt. (fur_dis * bszrgh * mmtom) ) then
              ! sufficient height for some effect
              red_fac = (1.0 - fur_dis * bszrgh * mmtom / bczht)
              red_lai = red_lai * red_fac
              red_sai = red_sai * red_fac

              ! check for row width effect
              if( bcxrow .gt. bczht*5.0 ) then
                  red_fac = 1.0/(0.92 + 0.021 * bcxrow / (bczht - fur_dis * bszrgh * mmtom) )
                  red_lai = red_lai * red_fac
              end if

          else
              ! not tall enough to do anything
              red_lai = 0.0
              red_sai = 0.0
          endif
      else
          ! biomass not in furrow
          ! test plant height and ridge height for minimums
          if( bczht .gt. 0.0 ) then
              ! check for row width effect
              if( bcxrow .gt. bczht*5.0 ) then
                  red_fac = 1.0 / (0.92 + 0.021 * bcxrow / bczht)
                  red_lai = red_lai * red_fac
              end if
          else
              ! not tall enough to do anything
              red_lai = 0.0
              red_sai = 0.0
          endif
      end if

      ! add discounted crop values to biomass values
      red_lai = red_lai + bdrlai
      red_sai = red_sai + bdrsai

      ! streamline effect for total leaf area
      red_lai = red_lai * 0.2 * (1.0 - exp(-red_lai))

      ! final result
      bio_drag = red_lai + red_sai

    end function biodrag

    subroutine anemometer_init

! + + +  PURPOSE + + +
!     To provide initial default values to wx station variables

!     The anemom. ht. and awwzo may be changed by read inputs in the
!     stand-alone erosion code. If anem. at the field  i.e flag =1,
!     then awwzo is set equal to the field zo in sbwus.

! + + + VARIABLE DEFINITIONS + + +
!     anemht = anemometer height (m)
!     awzzo  = aerodynamic roughness at anemometer (mm)
!     awzdisp - Weather station zero plane displacement height (mm)
!     wzoflg = flag = 0 for anem. and fixed awwzo at wx station
!              flag = 1 for anem. and variable awzzo at field

! + + + END SPECIFICATIONS + + +

      use erosion_data_struct_defs, only: anemht, awzzo, awzdisp, wzoflg

      ! set the default data values
      anemht =  10.0
      awzzo = 25.0
      awzdisp = 0.0
      wzoflg = 0

   end subroutine anemometer_init

end module wind_mod

