!$Author$
!$Date$
!$Revision$
!$HeadURL$

module soil_mod

  contains

    subroutine callsoil(daysim, isr, croptot, biotot, h1et, subrsurf)
! Wrapper to call soil

      use biomaterial, only: biototal
      use timer_mod, only: timer, TIMSOIL, TIMSTART, TIMSTOP
      use soil_data_struct_defs, only: am0sdb
      use hydro_data_struct_defs, only: hydro_derived_et
      use erosion_data_struct_defs, only: subregionsurfacestate

! Arguments
      integer daysim
      integer isr                   
      type(biototal), intent(in) :: croptot, biotot
      type(hydro_derived_et), intent(inout) :: h1et
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

! Includes
      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 's1agg.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'

      call timer(TIMSOIL,TIMSTART)      

            if (am0sdb(isr) .eq. 1) then
               call sdbug(isr, nslay(isr), croptot, biotot, h1et, subrsurf)
            end if
            call soil(isr,daysim,ahlocirr(isr),h1et%zirr, ahzsmt(isr),  &
     &                 ahtsmx(1,isr), ahtsmn(1,isr),                    &
     &                 ahrwc(1,isr), ahrwcdmx(1,isr), ahrwca(1,isr),    &
     &                 ahrwcw(1,isr), ahrwcs(1,isr),                    &
     &                 aszlyt(1,isr), nslay(isr),                       &
     &                 asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),     &
     &                 asfom(1,isr), asvroc(1,isr),                     &
     &                 asdsblk(1,isr), asdwblk(1,isr),                  &
     &                 asdblk(0,isr), asdagd(0,isr),                    &
     &                 aslagm(0,isr), aslagn(0,isr),                    &
     &                 as0ags(0,isr), aslagx(0,isr), aseags(0,isr),     &
     &                 aseagm(1,isr), aseagmn(1,isr), aseagmx(1,isr),   &
     &                 ask4d(1,isr), aslmin(1,isr), aslmax(1,isr),      &
     &                 biotot%ffcvtot, biotot%fscvtot,                  &
     &                 asfcce(1,isr), asfcec(1,isr),                    &
     &                 ahzinf(isr), ahzwid(isr), subrsurf)
            if (am0sdb(isr) .eq. 1) then
               call sdbug(isr, nslay(isr), croptot, biotot, h1et, subrsurf)
            end if

      ! recalculate  depth to bottom of soil layer
      call depthini( nslay(isr), aszlyt(1,isr), aszlyd(1,isr) )

      call timer(TIMSOIL,TIMSTOP)      

    end subroutine callsoil

    subroutine soil (isr, daysim, bhlocirr, bhzirr, bhzsmt,           &
     &                 bhtsmx, bhtsmn,                                  &
     &                 bhrwc, bhrwcdmx, bhrwca,                         &
     &                 bhrwcw, bhrwcs, bszlyt, bslay,                   &
     &                 bsfsan, bsfsil, bsfcla, bsfom, bsvroc,           &
     &                 bsdsblk, bsdwblk,                                &
     &                 bsdblk, bsdagd,                                  &
     &                 bslagm, bslagn,                                  &
     &                 bs0ags, bslagx, bseags,                          &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bsk4d, bslmin, bslmax,                           &
     &                 bbffcv, bbfscv,                                  &
     &                 bsfcce, bsfcec, bhzinf, bhzwid, subrsurf)


!     + + + PURPOSE + + +
! SOIL submodel for the Wind Erosion Prediction System model.
! update the SOIL (SURFACE: roughness, ridges, crust, and erodible material,
! and the LAYERS: aggregate size distribution, agg stability, and density).
! for more details on equations and processes, see SOIL SUBMODEL TECHNICAL
! DESCRIPTION.

!     + + + CONTRIBUTORS to CODE + + +
!     Imam Elminyawi,  Erik Monson, L. Hagen, Andy Hawkins, T. Zobeck

!     + + + KEY WORDS + + +
!     wind erosion, soil processes, surface process, layer process

!     + + + GLOBAL COMMON BLOCKS + + +

      use datetime_mod, only: get_simdate_doy, get_simdate_year
      use file_io_mod, only: luosoilsurf, luosoillay
      use soil_data_struct_defs, only: am0sfl
      use climate_input_mod, only: cli_today
      use erosion_data_struct_defs, only: subregionsurfacestate
      use process_mod, only: coef_abrasion

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'soil/cumulat.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer daysim
      real bhlocirr, bhzirr, bhzsmt
      real bhtsmx(mnsz), bhtsmn(mnsz)
      real bhrwc(mnsz), bhrwcdmx(mnsz), bhrwca(mnsz)
      real bhrwcw(mnsz), bhrwcs(mnsz), bszlyt(mnsz)
      integer bslay
      real bsfsan(1:mnsz), bsfsil(1:mnsz), bsfcla(1:mnsz)
      real bsfom(1:mnsz), bsvroc(1:mnsz)
      real bsdsblk(mnsz), bsdwblk(mnsz)
      real bsdblk(0:mnsz), bsdagd(0:mnsz)
      real bslagm(0:mnsz), bslagn(0:mnsz)
      real bs0ags(0:mnsz), bslagx(0:mnsz), bseags(0:mnsz)
      real bseagm(mnsz), bseagmn(mnsz), bseagmx(mnsz)
      real bsk4d(mnsz), bslmin(mnsz), bslmax(mnsz)
      real bbffcv, bbfscv
      real bsfcce(1:mnsz), bsfcec(1:mnsz)
      real bhzinf, bhzwid
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

!     + + + ARGUMENT DEFINITIONS + + +
!   daysim    - an index for the day of simulation.
!   bhlocirr  - location of irrigation application
!               + means above the soil surface
!               - means below the soil surface
!               soil surface reference is the bottom of the furrow 
!   bhzirr    - irrigation water applied, mm/day.
!   bhzsmt    - snowmelt, mm/day.
!   bhtsmx    - layer maximum temperature of today in C.
!   bhtsmn    - layer minimum temperature of today in C.
!   bhrwc     - soil water content for today, kg/kg.
!   bhrwcdmx  - daily maximum soil water content for today, kg/kg.
!   bhrwca    - soil avaiable water content on mass basis kg water/kg soil.
!   bhrwcw    - wilting point = 15 bar-grav. soil water content, kg/kg
!   bszlyt    - layer thickness, mm.
!   bslay     - number of soil layers
!   bsfsan    - layer fraction of sand.
!   bsfsil    - layer fraction of silt.
!   bsfcla    - layer fraction of clay.
!   bsfom     - layer fraction of organic matter.
!   bsvroc    - soil volume fraction of rock in each layer
!   bsdsblk    - consolidated soil bulk density by layer, Mg/m^3
!   bsdwblk    - Bulk Density of soil measured at 1/3 bar, Mg/m^3
!   bsdblk    - current layer density may be different from bsdsblk.
!   bsdagd    - aggregate density.
!   bslagm    - aggregate geometric mean diameter, mm.
!   bslagn    - minimum geometric diameter for aggregates in each
!               layer, mm.
!   bs0ags    - aggregate geometric standard deviation.
!   bslagx    - maximum value of aggregate size (mm)
!               (that aggregate may reach)
!   bseags    - agg stability, ln(J/kg).
!   bseagm    - mean agg stability, ln(J/kg)
!   bseagmn   - minimum agg stability, ln(J/kg)
!   bseagmx   - maximum agg stability, ln(J/kg)
!   bsk4d     - drying process coef. to calc. aggregate stability
!   bslmin    - min value of aggregate gmd
!   bslmax    - max value of aggregate gmd
!   bbffcv    - biomass fraction flat cover
!   bbfscv    - biomass fraction standing cover
!   bsfcce    - soil fraction calcium carbonate equivalent
!   bsfcec    - soil cation exchange capacity (cmol/kg)
!   bhzinf    - daily water infiltration depth (mm of water)
!   bhzwid    - water infiltration depth (mm of soil)

!     + + + LOCAL VARIABLES + + +
! Retain the values of these variables for the next day
      include 'soil/prevday.inc'
! the 0 at the end of bhtmx0, bhrwc0, bszrr0, bszrh0 refer to
! prior day values of:
! max temperature, soil water content, random roughnes & ridge height

      real rain, snow, sprink
      real cumpa
      real cf2cov
      real szlyd(0:mnsz), laycenter(mnsz)
      real bsmls0
      real dcump
      integer yr, idoy
      integer ldx, trigger(bslay)

!     + + + LOCAL DEFINITIONS + + +
!   rain      - water added to soil as rain.
!   snow      - water equivalent added to soil surface as snow, mm.
!   sprink    - water added to soil as sprinkler irrigation, mm.
!   cumpa     - apparent (rain + sprinkler + snow-metl) to current
!               day from time of last tillage
!   cf2cov    - a plant cover correction factor for ridge height
!               and random roughness decrease as a result of rain.
!   szlyd     - depth to bottom of each soil layer, mm
!   laycenter - depth to middle of each soil layer, mm
!   bsmls0    - prior value of bsmlos before update by SOIL, kg/m^2
!   dcump     - total rain + sprinkler + snow-melt for current day.
!   yr        - current year of simulation for output.
!   idoy      - day of year for output
!   ldx       - index for layers
!   trigger   - bitmapped integer showing the state of soil property change
!               condition triggers for output into the layer detail file
!               This is the same as the value of the integer being set in 
!               powers of two
!               BIT - representative condition
!               0   - freeze
!               1   - freeze_thaw
!               2   - thaw
!               3   - frozen
!               4   - wetting
!               5   - drying
!               6   - warm_puddling
!               7   - wet_bulk_den

!     + + + END SPECIFICATIONS + + +

!     + + + INITIALIZATION  SECTION + + +


! call daily initialization
      call sinit (daysim,                                               &
     &                 bhtsmx, bhrwc, bsfom, bszlyt,                    &
     &                 bslay, bsfsan, bsfsil, bsfcla,                   &
                       subrsurf%aszrgh, subrsurf%aslrr, bsfcce, bsfcec, &
     &                 cump(isr), dcump, bsk4d,                         &
     &                 bhtmx0(1,isr), bhrwc0(1,isr), szlyd(0),          &
     &                 bszrr0(isr), bszrh0(isr),                        &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bslmin, bslmax,                                  &
     &                 rain, snow, sprink,                              &
                       bhzirr, subrsurf%aszrho, &
                       bhlocirr, bhzsmt, subrsurf%aslrro, &
     &                 bsdsblk, cli_today%zdpt, cli_today%tdav, trigger)
!
!  UPDATE SURFACE
!     do surface processes if (rain+sprinkler+snowmelt>0)

      if (dcump .gt. 0.0) then

!  RIDGE SECTION:
        call rid(cf2cov, bbfscv, bbffcv, subrsurf%aszrgh, &
          subrsurf%asxrgs, subrsurf%aszrho, cumpa, dcump, bsvroc)

!
!  RANDOM ROUGHNESS SECTION:
        call ranrou(bsfsil(1), bsfsan(1), subrsurf%aslrr, subrsurf%aslrro, &
     &    cumpa, dcump, cf2cov, bsvroc(1))

!
!  CRUST SECTION:
        call  cru(subrsurf%aszcr, cumpa, bsfcla(1), dcump, &
          subrsurf%asfcr, bhzsmt, subrsurf%asmlos, bsfom(1), bsfcce(1), &
          bsfsan(1), bsmls0, subrsurf%aszrgh, subrsurf%aslrr, subrsurf%asflos)
      endif

!  skip layer update on first simulation day
      if (daysim .ge. 2)                                                &
     &  call updlay( daysim, szlyd,                                     &
     &  bhrwc0(1,isr), bhrwc, bhrwcdmx,                                 &
     &  bseagmx, bseagmn, bseags,                                       &
     &  bhrwca, bhrwcw, bhrwcs,                                         &
     &  bhtsmn, bhtmx0(1,isr), bhtsmx,                                  &
     &  bsk4d, bslmin, bslmax,                                          &
     &  bslagm,                                                         &
     &  bs0ags, bslagx, bsdblk,                                         &
     &  bszlyt, bsdagd, bslay,                                   &
     &  bsdsblk, bsdwblk, bhzinf, bhzwid, trigger)

      ! update surface properties based on surface layer properties
      ! crust stability
      subrsurf%asecr = bseags(1)
      ! crust density
      subrsurf%asdcr = 0.576 + 0.603 * bsdsblk(1)
      ! crust coefficient of abrasion
      subrsurf%acancr = coef_abrasion(subrsurf%asecr)
      ! aggregate coefficient of abrasion
      subrsurf%acanag = coef_abrasion(bseags(1))

!     Assign today's values to 'yesterday storage'
      do ldx = 1,bslay
          bhtmx0(ldx,isr) = bhtsmx(ldx)
          bhrwc0(ldx,isr) = bhrwc(ldx)
      end do

      bszrr0(isr) = subrsurf%aslrr
      bszrh0(isr) = subrsurf%aszrgh

!     + + + OUTPUT FORMATS + + +
 2100 format('#daysim idoy yr cump dcump bszrgh bsxrgs bszrr bszcr bsfcr&
     & bsecr bsmlos bsflos')
 2200 format( 3(1x,i4), 10(1x,f8.4) )
 2300 format('#daysim idoy yr layer depth bszlyt bsdblk bseags bseagmn b&
     &seagm bseagmx bslagn bslmin bslagm bslmax bslagx bs0ags bsdagd rel&
     &_ag_stab rel_geo_mean freeze freeze_thaw thaw frozen wetting dryin&
     &g warm_puddling wet_bulk_den ')
 2400 format( i6, 1x,i3, 1x,i4, 1x,i3, 16(1x,f10.4), 8(1x,b1) )

!  + + +  OUTPUT SECTION  + + +

      if ((am0sfl(isr) .eq. 1)) then
         ! get some date, day variables
         yr = get_simdate_year()
         idoy = get_simdate_doy()

         ! write output headers
         if( daysim .eq. 1 ) then
             write(luosoilsurf(isr),2100)
             write(luosoillay(isr),2300)
         end if
         ! insert single blank line to break layer blocks for graphing
         write(luosoillay(isr),*)
         ! insert additional blank line (make double) to break years into blocks for graphing
         if( idoy .eq. 1 ) then
             write(luosoilsurf(isr),*)
             write(luosoilsurf(isr),*)
             write(luosoillay(isr),*)
         end if

         write(luosoilsurf(isr), 2200) daysim,idoy,yr, cump(isr), dcump, &
              subrsurf%aszrgh, subrsurf%asxrgs, subrsurf%aslrr, subrsurf%aszcr, &
              subrsurf%asfcr, subrsurf%asecr, subrsurf%asmlos, subrsurf%asflos

! output new values by layer to the soil output file.
         do ldx = 1,bslay
            laycenter(ldx) = 0.5 * ( szlyd(ldx-1) + szlyd(ldx) )
            write (luosoillay(isr),2400) daysim, idoy, yr, ldx,         &
     &          laycenter(ldx), bszlyt(ldx), bsdblk(ldx),               &
     &          bseags(ldx), bseagmn(ldx), bseagm(ldx), bseagmx(ldx),   &
     &          bslagn(ldx), bslmin(ldx), bslagm(ldx), bslmax(ldx),     &
     &          bslagx(ldx), bs0ags(ldx), bsdagd(ldx),                  &
     &          (bseags(ldx)-bseagmn(ldx))/(bseagmx(ldx)-bseagmn(ldx)), &
     &          (bslagm(ldx) - bslmin(ldx))/(bslmax(ldx) - bslmin(ldx)),&
     &          ibits(trigger(ldx),0,1), ibits(trigger(ldx),1,1),       &
     &          ibits(trigger(ldx),2,1), ibits(trigger(ldx),3,1),       &
     &          ibits(trigger(ldx),4,1), ibits(trigger(ldx),5,1),       &
     &          ibits(trigger(ldx),6,1), ibits(trigger(ldx),7,1)
         end do
      endif

      return
    end subroutine soil

    subroutine depthini(nlay, bszlyt, bszlyd)

      integer nlay
      real    bszlyt(*), bszlyd(*)

      integer idx

!     nlay - number of soil layers
!     bszlyt - soil layer thickness (mm)
!     bszlyd - depth to bottom of soil layer (mm)

      bszlyd(1) = bszlyt(1)
      do idx = 2, nlay
        bszlyd(idx) = bszlyt(idx) + bszlyd(idx-1)
      end do

      return
    end subroutine depthini

    subroutine sinit (daysim,                                         &
     &                 bhtsmx, bhrwc, bsfom, bszlyt,                    &
     &                 bslay, bsfsan, bsfsil, bsfcla,                   &
     &                 bszrgh, bszrr, bsfcce, bsfcec,                   &
     &                 cump, dcump, bsk4d,                              &
     &                 bhtmx0, bhrwc0, szlyd,                           &
     &                 bszrr0, bszrh0,                                  &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bslmin, bslmax,                                  &
     &                 rain, snow, sprink,                              &
     &                 bhzirr, bszrho,                                  &
     &                 bhlocirr, bhzsmt, bszrro,                        &
     &                 bsdsblk, bwzdpt, bwtdav, trigger)

!     + + + PURPOSE + + +
! SOIL submodel for the Wind Erosion Prediction System model.
! daily initialization of soil properties
! (SURFACE: roughness, ridges, crust, and erodible material,
! and the LAYERS: aggregate size distribution, agg stability, and density).
! for more details on equations and processes, see SOIL SUBMODEL TECHNICAL
! DESCRIPTION.

!     + + + KEY WORDS + + +
!     wind erosion, soil processes, surface process, layer process

!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      real bhtsmx(mnsz), bhrwc(mnsz), bsfom(1:mnsz), bszlyt(mnsz)
      integer bslay
      real bsfsan(1:mnsz), bsfsil(1:mnsz), bsfcla(1:mnsz)
      real bszrgh, bszrr, bsfcce(1:mnsz), bsfcec(1:mnsz)
      real cump, dcump, bsk4d(mnsz)
      real bhtmx0(mnsz), bhrwc0(mnsz), szlyd(mnsz)
      real bszrr0, bszrh0
      real bseagm(mnsz), bseagmn(mnsz), bseagmx(mnsz)
      real bslmin(mnsz),bslmax(mnsz)
      real rain, snow, sprink
      real bhzirr, bszrho
      real bhlocirr, bhzsmt, bszrro
      real bsdsblk(mnsz), bwzdpt, bwtdav
      integer trigger(bslay)

!     + + + ARGUMENT DEFINITIONS + + +
!   daysim    - an index for the day of simulation.
!   bhtsmx    - layer maximum temperature of today in C.
!   bhrwc     - soil water content for today, kg/kg.
!   bsfom     - layer fraction of organic matter.
!   bszlyt    - layer thickness, mm.
!   bslay     - number of soil layers
!   bsfsan    - layer fraction of sand.
!   bsfsil    - layer fraction of silt.
!   bsfcla    - layer fraction of clay.
!   bszrgh    - ridge height, mm.
!   bszrr     - random roughness height, mm
!   bsfcce    - soil fraction calcium carbonate equivalent
!   bsfcec    - soil cation exchange capacity (cmol/kg)
!   cump      - cumulative (rain + sprinkler + snow-melt) to current
!               day from day 1 or time of last tillage
!   dcump     - total rain + sprinkler + snow-melt for current day.
!   bsk4d     - drying process coef. to calc. aggregate stability
!   bhtmx0    - layer maximum temperature of yesterday. in C
!   bhrwc0    - soil water content for yesterday. mass basis kg/kg.
!   szlyd     - depth to bottom of each soil layer, mm
!   bszrr0    - prior day random roughness, mm
!   bszrh0    - prior day ridge height, mm
!   bseagm    - mean agg stability, ln(J/kg)
!   bseagmn   - minimum agg stability, ln(J/kg)
!   bseagmx   - maximum agg stability, ln(J/kg)
!   bslmin    - min value of aggregate gmd
!   bslmax    - max value of aggregate gmd
!   rain      - water added to soil as rain.
!   snow      - water equivalent added to soil surface as snow, mm.
!   sprink    - water added to soil as sprinkler irrigation, mm.
!   bhzirr    - irrigation water applied, mm/day.
!   bszrho    - ridge height right after tillage, mm.
!   bhlocirr  - location of irrigation application, mm.
!               + means above the soil surface
!               - means below the soil surface
!               soil surface reference is the bottom of the furrow
!   bhzsmt    - snowmelt, mm/day.
!   bszrro    - random roughness height right after tillage, mm.
!   bsdsblk    - consolidated soil bulk density by layer, Mg/m^3
!   bwzdpt    - rainfall depth (mm)
!   bwtdav    - Average daily air temperature (deg C)
!   trigger   - bitmapped integer showing the state of soil property change
!               condition triggers for output into the layer detail file

! the 0 at the end of bhtmx0, bhrwc0, bszrr0, bszrh0 refer to
! prior day values of:
! max temperature, soil water content, random roughnes & ridge height
! bszrro , bszrho are right-after-tillage

!     + + + LOCAL VARIABLES + + +
      real sf84m(mnsz), sf84sd(mnsz), scecr
      real tsfom, tsfcce, tsfsacl
      integer ldx

!     + + + LOCAL DEFINITIONS + + +
!   sf84m     - mean of fraction agg. < 0.84 mm
!   sf84sd    - standard deviation of fraction agg. < 0.84 mm
!   scecr     - ratio of clay fraction cation exchange capacity
!               to percent clay
!   tsfom     - temporary layer fraction of organic matter.
!   tsfcce    - temporary soil fraction calcium carbonate equivalent
!   tsfsacl   - temporary layer fraction of clay.
!   ldx       - layer index

!     + + + FUNCTIONS CALLED + + +

!     + + + SUBROUTINES CALLED + + +

!     + + + END SPECIFICATIONS + + +

      ! check for first day
      if (daysim .eq. 1) then
         ! set up tillage check
         bszrr0 = - 1.0
         bszrh0 = - 1.0
         ! initialize previous day temperature and water content
         do ldx = 1, bslay
             bhtmx0(ldx) = bhtsmx(ldx)
             bhrwc0(ldx) = bhrwc(ldx)
         end do
      endif

      szlyd(1) = bszlyt(1)
      trigger(1) = 0
      do ldx = 2, bslay
          ! calc. depth to bottom of each layer
          szlyd(ldx) = szlyd(ldx-1) + bszlyt(ldx)
          ! zero out trigger condition array
          trigger(ldx) = 0
      end do

      ! if tillage (or anything else outside of soil submodel)
      ! changed roughness or ridge height then update
      if ((bszrr0.ne.bszrr).or.(bszrh0.ne.bszrgh)) then

!        store initial or after tillage surface roughness
         bszrro = bszrr
         bszrho = bszrgh

!        set cumulative precip to zero
         cump = 0.0

!        store/calculate initial layer values
         do 10 ldx = 1, bslay
!            store initial water content & yesterday's temperature
             bhtmx0(ldx) = bhtsmx(ldx)
             bhrwc0(ldx) = bhrwc(ldx)
!           calc. mean, min, and max agg. stability
!           (eq. S-26, S-27, S-28, *** sb S-25,26,27)
            if (bsfcla(ldx) .gt. 0.6) then
               bseagm(ldx) = 3.484
            else
              bseagm(ldx) = -16.73 - 46.629*bsfcla(ldx)**2              &
     &                    + 23.514*bsfcla(ldx)**3                       &
     &                    + 17.519*exp(bsfcla(ldx))
            endif
            bseagmn(ldx) = bseagm(ldx) - 2*(0.16)*bseagm(ldx)
            bseagmx(ldx) = bseagm(ldx) + 2*(0.16)*bseagm(ldx)
!           calc. mean and standard deviation of fraction agg. < 0.84 mm
!           (eq. S-42, S-43, *** sb S-37, S-38)

! ***            sf84m(ldx)  = 0.2902 + 0.31 * bsfsan(ldx) + 0.17 * bsfsil(ldx) +
! ***     &       0.0033*bsfsan(ldx)/bsfcla(ldx) - 4.66*bsfom(ldx) - 0.95*bsfcce(ldx)
! *** debugging fix, 1st try
! ***            sf84m(ldx)  = 0.2902 + 0.31 * bsfsan(ldx) + 0.17 * bsfsil(ldx) +
! ***     &       0.0033*bsfsan(ldx)/bsfcla(ldx) - 4.66*bsfom(ldx)
! eodf
! *** debugging fix, 2nd try
! clamping upper limits on variables to keep them from forcing sf84m negative
! note that this needs correcting by a more robust regression equation
            tsfom = bsfom(ldx)
            if (tsfom.gt.0.03) tsfom = 0.03
            tsfcce = bsfcce(ldx)
            if (tsfcce.gt.0.2) tsfcce = 0.2
            if (bsfcla(ldx).eq.0) tsfsacl = 40.
            if (bsfcla(ldx).ne.0) tsfsacl = bsfsan(ldx) / bsfcla(ldx)
            if (tsfsacl.gt.40) tsfsacl = 40.
! *** convert organic carbon to organic matter by dividing by 1.724
            if ((bsfsan(ldx) .ge. .15).and.(bsfcla(ldx) .le. 0.25)) then
              sf84m(ldx)  = 0.2909 + 0.31*bsfsan(ldx) + 0.17*bsfsil(ldx) &
     &                + 0.01*tsfsacl - 4.66*tsfom/1.724 - 0.95*tsfcce
            else
              sf84m(ldx)  = 0.2909 + 0.31*bsfsan(ldx) + 0.17*bsfsil(ldx) &
     &                + 0.0033*tsfsacl - 4.66*tsfom/1.724 - 0.95*tsfcce
            end if
! *** eodf
            sf84sd(ldx) = (0.41 - 0.22*bsfsan(ldx))*sf84m(ldx)
! ***       write(*,*) ' sf84m(ldx), sf84sd(ldx) ', ldx, sf84m(ldx), sf84sd(ldx)

!           calc. min and max values of geom. mean agg. diameter (eq. S-45, S-46)
            bslmin(ldx) = exp(3.44 - 7.21*(sf84m(ldx)+ 2.0*sf84sd(ldx)))
            if (bslmin(ldx) .lt. 0.025) bslmin(ldx) = 0.025
            bslmax(ldx) = exp(3.44 - 7.21*(sf84m(ldx)- 2.0*sf84sd(ldx)))
            if (bslmax(ldx) .gt. 31.0) bslmax(ldx) = 31.0
            if(bslmin(ldx).ge.bslmax(ldx)) write(*,*) 'sinit:min.gt.max'

! ***       write(*,*) 'bslmin(ldx),bslmax(ldx)',ldx,bslmin(ldx),bslmax(ldx)
!           calc. ratio of clay cation exchange capacity to percent clay (eq. S-53)
            scecr = (bsfcec(ldx) - bsfom(ldx) * (142. + 0.17 *          &
     &        szlyd(ldx)))/ (bsfcla(ldx) * 100.0 + 0.0001)
            if (scecr .lt. 0.15) scecr = 0.15
            if (scecr .gt. 0.65) scecr = 0.65

! *** remove calculation of cbd; replace with original cbd calc from inpsub
! ***            sdbko(ldx) = bsdsblk(ldx)
! ***c           calc. consolidated bulk density (eq. S-52)
! ***            sdbko(ldx) = 1.514 + 0.25*bsfsan(ldx) -
! ***     *        13.*bsfsan(ldx)*bsfom(ldx) -6.0*bsfcla(ldx)*
! ***     *        bsfom(ldx) - 0.48*bsfcla(ldx)*scecr

! ***            if (sdbko(ldx) .gt. 2.2) sdbko(ldx) = 2.2
! ***            if (sdbko(ldx) .lt. 0.5) sdbko(ldx) = 0.5
! ***c           calc. increase in consolidated bulk density with depth
! ***c           (note the depths change slightly with time, but current
! ***c           update only occurs with tillage.)
! ***c           (eq. S-54)
! *** debugging fix
! ***            sdbko(ldx) = sdbko(ldx)*(0.975+ 1.931/
! ***     *        (1+exp(-(szlyd(ldx)-506.8)/118.5)))
! *** eodf

!           set stability drying process coefficient:
            if( ldx .eq. 1) then
              bsk4d(ldx) = 0.46 - 0.23 * exp(-(szlyd(ldx)/2.0)/88.57)
            else
              bsk4d(ldx) = 0.46 - 0.23 * exp(-(szlyd(ldx-1) +           &
     &               (szlyd(ldx) - szlyd(ldx-1))/2.0)/88.57)
            end if

 10      continue

      endif
!23456789*23456789*23456789*23456789*23456789*23456789*23456789*2345
!
!     set stability freezing and wetting process coefficients:
!
!     initialize rain and snow variables
      rain = 0.0
      snow = 0.0
!     Determine if precip
      if (bwzdpt .gt. 0.0) then
!     Determine if precip. is rain or snow
!     (note HYDROLOGY may do this in future using ELS results)
         if (bwtdav .ge. 0.0) rain = bwzdpt
         if (bwtdav .lt. 0.0) snow = bwzdpt
      endif

      ! add irrigation to cumulative precipitation based on application
      ! height with respect to ridge height
      if (bhlocirr .ge. bszrgh ) then
         ! irrigation is being applied from above ridge height
         ! add full amount to degrade ridge height and random roughness
         sprink = bhzirr
      else if (bhlocirr .ge. 0.0 ) then
         ! irrigation application is below ridge height
         ! partially include reducing degradation (like furrow irrigation)
         sprink = bhzirr * bhlocirr / bszrgh
      else
         ! irrigation application underground
         ! no degradation of ridge height or randowm roughness
         sprink = 0.0
      endif
!     Calc. daily and cumulative (rain + sprinkler irrigation + snowmelt)
      dcump = rain + sprink + bhzsmt
!     (note: cump not used in calc., but useful as ouptput for validation)
      cump = cump + dcump
      end subroutine sinit

    subroutine soilinit(isr)
! ***************************************************************** wjr
! Contains init code from main
!
!       Edit History
!       04-Mar-99       wjr     created

      include 'p1werm.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
!
      integer isr
!
      ! recalculate  depth to bottom of soil layer
      call depthini( nslay(isr), aszlyt(1,isr), aszlyd(1,isr) )
!
! This should go away, possibly becoming a data statement   
      asmno3(isr) = 0.

    end subroutine soilinit

end module soil_mod

