!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! print out many of the biomass pool components (used for debugging purposes)

! These files use the following columnar format.  Some are filled with zeros
! to make it easier to select specific columns for comparisons between the
! crop and individual biomass pools (not all pools have the same variables)


      subroutine bpools (cd,cm,cy,isr)

      use file_io_mod, only: luocrp1, luobio1, luodec

      integer cd,cm,cy,isr
      real total, saitotal !added by Simon

      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'b1glob.inc'
      include 'd1gen.inc'
      include 'd1glob.inc'
      include 'c1glob.inc'
      include 'c1db1.inc'
      include 'decomp/decomp.inc'
      include 'main/main.inc'   ! daysim

! statements below added by Simon

      include 'w1clig.inc'
!      include 's1layr.inc'
!      include 'p1const.inc'
!      include 'm1dbug.inc'

!   These hydrology common blocks provide soil temp, moisture and irrigation

!      include 'h1temp.inc'
!      include 'h1db1.inc'
!      include 'h1hydro.inc'

!     + + + LOCAL VARIABLES + + +
      integer doy, idx

!     + + + FUNCTIONS CALLED + + +
      integer dayear

!     + + + END OF SPECIFICATIONS + + +

      doy = dayear (cd, cm, cy)

      if ((am0dfl .eq. 1).or.(am0dfl.eq.3)) then

        ! day, month, year
        ! flat crop cover, standing crop cover, total crop cover
        ! crop cover fract, crop SAI, crop LAI
        ! total crop biomass, 0.0, standing crop mass
        ! (no "flat crop biomass")
        ! crop root mass, 0.0, crop yield mass
        ! (no "buried crop biomass")
        ! qty crop stems per area, crop height, crop root depth, repr stem dia

        ! Dead Crop Biomass Pool
        ! write file header if still initializing
        if (am0ifl .eqv. .true.) then
          write(luocrp1,*) '#daysim doy yy Tmin Tmax Tavg Tfacabove',   &
     &        ' Water Wfacstand Wfacflat Ddaystand Ddayflat Mstand1',   &
     &        ' Mstand2 Mstand3 MstandAll Mflat1 Mflat2 Mflat3',        &
     &        ' MflatAll MaboveAll Mburied1 Mburied2 Mburied3',         &
     &        ' MburiedAll Mroot1 Mroot2 Mroot3 MrootAll Cstand1',      &
     &        ' Cstand2 Cstand3 CstandAll Cflat1 Cflat2 Cflat3',        &
     &        ' CflatAll Cstand+flat1 Cstand+flat2 Cstand+flat3',       &
     &        ' Cstand+flatAll SAI1 SAI2 SAI3 SAIAll LAI1 LAI2 LAI3',   &
     &        ' LAIAll Biodrag #stem1 #stem2 #stem3 #stemAll Hstem1',   &
     &        ' Hstem2 Hstem3 HstemAll Mrt4all'

        else

          total = admsttot(1) + admftot(1)   !sum of standing and flat residue mass, all pools

          ! insert double blank lines to demarcate years
          if( doy .eq. 1 ) then
              write (luocrp1,*)
              write (luocrp1,*)
          end if

          write(luocrp1,2222) daysim, doy, cy,                          & !simulation day, day of year, year
     &    awtdmn, awtdmx, awtdav, ditca,                                & !tmin, tmax, tavg, tf  
     &    aqua, diwcs, diwcf, didds, diddf,                             & !precip, wf standing, wf flat, dd standing, dd flat
     &    admst(1,1), admst(2,1), admst(3,1), admsttot(1),              & !mass, standing
     &    admf(1,1), admf(2,1), admf(3,1), admftot(1),                  & !mass, flat
     &    total,                                                        & !sum of standing and flat residue mass, all pools
     &    admbg(1,1), admbg(2,1), admbg(3,1), admbgtot(1),              & !mass, below ground
     &    admrt(1,1), admrt(2,1), admrt(3,1), admrttot(1),              & !mass, roots
     &    adfscv(1,1), adfscv(2,1), adfscv(3,1), adfscvtot(1),          & !cover provided by standing residue (fraction)
     &    adffcv(1,1), adffcv(2,1), adffcv(3,1), adffcvtot(1),          & !cover provided by flat residue (fraction)
     &    adftcv(1,1), adftcv(2,1), adftcv(3,1), adftcvtot(1),          & !cover provided by standing+flat residue (fraction)
     &    adrsai(1,1), adrsai(2,1), adrsai(3,1), adrsaitot(1),          & !stem area index 
     &    adrlai(1,1), adrlai(2,1), adrlai(3,1), adrlaitot(1),          & !leaf area index
     &    adrcdtot(1),                                                  & !biodrag
     &    addstm(1,1), addstm(2,1), addstm(3,1), addstmtot(1),          & !stems (no/m2) 
     &    adzht(1,1), adzht(2,1), adzht(3,1), adzht_ave(1),             & !stem height for each residue pool
     &    admrttotto4(1)                                                  !root mass to 4 inches

! tf=temperature factor, wf=water factor, dd=decomposition day
   
2222     format (' ',i6,' ',i3,' ',i4,' ',3f7.1,f7.3,f7.2,4f7.3,17f7.4, &
     &           24f7.4,4f7.1,3f7.2)  !added by Simon

!     &    acffcv(isr), acfscv(isr), acftcv(isr),
!     &    accovfact(isr), acrsai(isr), acrlai(isr),
!     &    acm(isr), 0.0, acmst(isr),
!     &    acmrt(isr), 0.0, acmyld(isr),
!     &    acdstm(isr), aczht(isr), aczrtd(isr), acxstmrep(isr)

          ! day, month, year
          ! flat residue cover, standing residue cover, total residue cover
          ! residue cover fract, residue SAI, residue LAI
          ! total residue biomass, flat residue mass, standing residue mass
          ! residue root mass, below gnd residue mass
          ! qty residue stems per area, "ave" residue height, 0.0, 0.0
          ! (no "ave" root depth or stem dia computed across residue pools)

2345     format (i6,i4,i5,3f10.5,13f10.3)

          ! All Residue Pools Combined
          write(luobio1,2345) daysim, doy, cy,                          &
     &    abffcv(isr), abfscv(isr), abftcv(isr),                        &
     &    0.0, abrsai(isr), abrlai(isr),                                &
     &    abm(isr), abmf(isr), abmst(isr),                              &
     &    abmrt(isr), abmbg(isr),                                       &
     &    abdstm(isr), abzht(isr), 0.0, 0.0
        endif
      endif

      if ((am0dfl .eq. 2).or.(am0dfl.eq.3)) then

          ! day, month, year
          ! flat residue cover, standing residue cover, total residue cover
          ! residue cover fract, residue SAI, residue LAI
          ! total residue biomass, flat residue mass, standing residue mass
          ! residue root mass, below gnd residue mass, 0.0
          ! (no residue yield mass)
          ! qty residue stems per area, residue height, 0.0, rep stem dia
          ! (no root depth for residue pools)

        do idx=1,mnbpls

          ! write file header if still initializing
         if (am0ifl .eqv. .true.) then
           write(luodec(idx),*) '#daysim resday resyear doy yy pool#',  &
     &          ' cumddysta cumddyflat cumddybg10 flatcov standcov',    &
     &          ' totalcov covfact silhoutte leafarea totalmass',       &
     &          ' flatmass standmass bgrootmass bgshootmass stemnumb',  &
     &          ' height repstemdia stemstandm leafstandm storstandm',  &
     &          ' stemflatm leafflatm storflatm rstorflatm rfiberflatm',&
     &          ' stembgm leafbgm storbgm rstorgbm rfibergbm name'

         else

2355       format (i6,1x,i5,1x,i4,1x,i3,1x,i4,1x,i2,30(1x,f10.5),1x,a30)

           ! Residue Pool #idx
           write(luodec(idx),2355) daysim,                              &
     &     resday(idx,isr), resyear(idx,isr), doy, cy, idx,             &
     &     cumdds(idx,isr), cumddf(idx,isr), cumddg(10,idx,isr),        &
     &     adffcv(idx,isr), adfscv(idx,isr), adftcv(idx,isr),           &
     &     covfact(idx,isr), adrsai(idx,isr), adrlai(idx,isr),          &
     &     adm(idx,isr), admf(idx,isr), admst(idx,isr),                 &
     &     admrt(idx,isr), admbg(idx,isr),                              &
     &     addstm(idx,isr), adzht(idx,isr), adxstmrep(idx,isr),         &
     &     admstandstem(idx,isr), admstandleaf(idx,isr),                &
     &     admstandstore(idx,isr), admflatstem(idx,isr),                &
     &     admflatleaf(idx,isr), admflatstore(idx,isr),                 &
     &     admflatrootstore(idx,isr), admflatrootfiber(idx,isr),        &
     &     admbgstem(idx,isr), admbgleaf(idx,isr),                      &
     &     admbgstore(idx,isr), admbgrootstore(idx,isr),                &
     &     admbgrootfiber(idx,isr),                                     &
     &     ad0nam(idx,isr)

         endif
        end do
      endif

      end
