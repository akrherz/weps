!$Author$
!$Date$
!$Revision$
!$HeadURL$

program hpdn_to_cligen

  use Running_Stats

  implicit none

  ! defines input column values for High Plains Data Network files
  type HighPlainsData
    integer month  ! month of year
    integer day    ! day of month
    integer year   ! year
    real    tmax   ! daily maximum temperature (C)
    real    tmin   ! daily minimum temperature (C)
    real    rh     ! daily average relative humidity (%)
    real    tsoil  ! daily average soil temperature at 10cm (C)
    real    wind   ! daily average wind speed (m/s)
    real    solar  ! daily total horizontal solar radiation (MJ/m^2)
    real    precip ! daily total precipitation (mm)
    real    et     ! daily total evapo-transpiration (mm)
  end type HighPlainsData

  ! define output column values for cligen format files
  type CligenData
    integer day    ! day of month
    integer month  ! month of year
    integer year   ! year
    real    precip ! daily total precipitation (mm)
    real    duration ! duration of daily precipitation event (hours)
    real    timetopeak ! time of peak rainfall intensity (fraction of duration)
    real    peakintensity ! intensity of the peak rainfall (mm/h)
    real    tmax   ! daily maximum temperature (C)
    real    tmin   ! daily minimum temperature (C)
    real    solar  ! daily total horizontal solar radiation (langleys/day)
    real    wind   ! daily average wind speed (m/s)
    real    dirwind ! wind direction (degrees from N)
    real    tdew   ! dew point temperature (C)
  end type CligenData

  type(HighPlainsData) :: hpdn
  type(CligenData) :: cligen

  ! variables
  character*256 :: line
  integer       :: ioc
  integer       :: linecnt

  integer       :: lui
  parameter (lui=5)  ! stdin
  integer       :: luo
  parameter (luo=6)  ! stdout

  ! inputs from second line of hpdn file (manually added to header for this purpose)
  character*20  :: station_id
  character*20  :: station_name
  integer       :: lat_deg
  integer       :: lat_min
  integer       :: lon_deg
  integer       :: lon_min
  real          :: elevation

  ! tracking of years
  integer year_min
  integer year_max        

  ! Monthly averages (and VAR if needed)
  type(statistics), dimension(1:12) :: rs_tmax
  type(statistics), dimension(1:12)  :: rs_tmin
  type(statistics), dimension(1:12)  :: rs_solar
  type(statistics), dimension(1:12)  :: rs_precip
  real             :: precip_mo_sum   ! monthly
  integer          :: month_curr      ! the current month

  integer       :: idx

  linecnt = 0
  year_min = 32000
  year_max = -32000
  precip_mo_sum = 0
  month_curr = 0

  ! initialize statistics structures
  do idx = 1, 12
    call rs_initial( rs_tmax(idx) )
    call rs_initial( rs_tmin(idx) )
    call rs_initial( rs_solar(idx) )
  end do


  ! first line, text only, skip
  read (lui,'(a)',iostat=ioc) line
  linecnt = linecnt + 1
  if (ioc .eq. 0) then
    ! second line, ID, lat, lon, elevation
    read (lui,'(a)',iostat=ioc) line
    linecnt = linecnt + 1
    if (ioc .eq. 0) then
      ! parse the line
      read(line, *, iostat=ioc) station_id, station_name, lat_deg, lat_min, lon_deg, lon_min, elevation
      if (ioc .eq. 0) then
        ! third line, text only, skip
        read (lui,'(a)',iostat=ioc) line
        linecnt = linecnt + 1
        if (ioc .ne. 0) then
          write(luo,*) 'Read failed at line: ', linecnt
          stop
        end if
      else
        write(luo,*) 'Parse failed at line: ', linecnt
        stop
      end if
    else
      write(luo,*) 'Read failed at line: ', linecnt
      stop
    end if
  else
    write(luo,*) 'Read failed at line: ', linecnt
    stop
  end if

  ! first line of data
  read (lui,'(a)',iostat=ioc) line
  linecnt = linecnt + 1
  if (ioc .lt. 0) then
    write(luo,*) 'No Data in File. Read failed at line: ', linecnt
    stop
  end if

  do while (ioc .eq. 0)

    ! parse the line
    read(line, *, iostat=ioc) hpdn

    if (ioc .ne. 0) then
      ! We have a failure parsing the line
      write(luo,*) 'Parse failed at line: ', linecnt
      stop
    else
      ! assign values to CligenData
      cligen%day = hpdn%day
      cligen%month = hpdn%month
      cligen%year = hpdn%year
      cligen%precip = hpdn%precip
      cligen%duration = min( 24.0, 2.0 * hpdn%precip / 10.0)
      cligen%timetopeak = 0.5
      cligen%peakintensity = max( 10.0, 2.0 * hpdn%precip/cligen%duration )
      cligen%tmax = hpdn%tmax
      cligen%tmin = hpdn%tmin
      cligen%solar = hpdn%solar / 0.04186  ! convert MJ/m^2 to langleys/day
      cligen%wind = hpdn%wind
      cligen%dirwind = 0
      cligen%tdew = rh_to_dewpoint( hpdn%tmax, hpdn%tmin, hpdn%rh )

      ! print out cligen data line
      write (luo, * ) cligen

      ! capture first and last year
      year_min = min( cligen%year, year_min)
      year_max = max( cligen%year, year_max)

      ! add new values to running statistics
      call rs_newnum( rs_tmax(cligen%month), cligen%tmax )
      call rs_newnum( rs_tmin(cligen%month), cligen%tmin )
      call rs_newnum( rs_solar(cligen%month), cligen%solar )

      ! sum rainfall for month
      if( month_curr .eq. 0 ) then
        ! this is the first month
        month_curr = cligen%month
      else if( month_curr .ne. cligen%month ) then
        ! the month has changed, add sum to statistics, reset
        ! this assumes date order data
        call rs_newnum( rs_precip(month_curr), precip_mo_sum )
        month_curr = cligen%month
        precip_mo_sum = cligen%precip
      else
        precip_mo_sum = precip_mo_sum + cligen%precip
      end if

    end if

   read (lui,'(a)',iostat=ioc) line
   if (ioc .eq. 0) then
     linecnt = linecnt + 1
   else if (ioc .lt. 0) then
    write(luo,*) 'Read and processed ', linecnt, ' records.'
   else
     write(luo,*) 'Read failed at line: ', linecnt
     stop
   end if

  end do

  write(luo,*) linecnt, " lines of data processed."
  write(luo,*) "Delete this and previous 2 lines, and move remaining lines to head of file"
  ! write out cligen header
  write(luo,*) "5.30001"
  write(luo,*) "   1   0   0"
  write(luo,*) "  Station:  HPDN ", station_name, " ", station_id, " Measured data"
  write(luo,*) " Latitude Longitude Elevation (m) Obs. Years   Beginning year  Years simulated Command Line:"
  write(luo,*) lat_deg+(lat_min/60.0), lat_deg+(lat_min/60.0), elevation, year_max-year_min+1, year_min, year_max          
  write(luo,*) " Observed monthly ave max temperature (C)"
  write(luo,*) rs_mean(rs_tmax(1)), rs_mean(rs_tmax(2)), rs_mean(rs_tmax(3)), rs_mean(rs_tmax(4)), &
               rs_mean(rs_tmax(5)), rs_mean(rs_tmax(6)), rs_mean(rs_tmax(7)), rs_mean(rs_tmax(8)), &
               rs_mean(rs_tmax(9)), rs_mean(rs_tmax(10)), rs_mean(rs_tmax(11)), rs_mean(rs_tmax(12))
  write(luo,*) " Observed monthly ave min temperature (C)"
  write(luo,*) rs_mean(rs_tmin(1)), rs_mean(rs_tmin(2)), rs_mean(rs_tmin(3)), rs_mean(rs_tmin(4)), &
               rs_mean(rs_tmin(5)), rs_mean(rs_tmin(6)), rs_mean(rs_tmin(7)), rs_mean(rs_tmin(8)), &
               rs_mean(rs_tmin(9)), rs_mean(rs_tmin(10)), rs_mean(rs_tmin(11)), rs_mean(rs_tmin(12))
  write(luo,*) " Observed monthly ave solar radiation (Langleys/day)"
  write(luo,*) rs_mean(rs_solar(1)), rs_mean(rs_solar(2)), rs_mean(rs_solar(3)), rs_mean(rs_solar(4)), &
               rs_mean(rs_solar(5)), rs_mean(rs_solar(6)), rs_mean(rs_solar(7)), rs_mean(rs_solar(8)), &
               rs_mean(rs_solar(9)), rs_mean(rs_solar(10)), rs_mean(rs_solar(11)), rs_mean(rs_solar(12))
  write(luo,*) " Observed monthly ave precipitation (mm)"
  write(luo,*) rs_mean(rs_precip(1)), rs_mean(rs_precip(2)), rs_mean(rs_precip(3)), rs_mean(rs_precip(4)), &
               rs_mean(rs_precip(5)), rs_mean(rs_precip(6)), rs_mean(rs_precip(7)), rs_mean(rs_precip(8)), &
               rs_mean(rs_precip(9)), rs_mean(rs_precip(10)), rs_mean(rs_precip(11)), rs_mean(rs_precip(12))
  write(luo,*) " da mo year  prcp  dur   tp     ip  tmax  tmin  rad  w-vl w-dir  tdew"
  write(luo,*) "             (mm)  (h)               (C)   (C) (l/d) (m/s)(Deg)   (C)"

  !call test_rh2dp

contains

  ! returns the dewpoint temperature (C) back calculated from the 
  ! approximation from Jensen ASCE manual 70 evapotranspiration
  ! referenced to Tetens (1930), and transformed by Murray (1966)
  ! Converted here to use temperature in (C) and made to be consistent
  ! with usage of dewpoint in WEPS et.for routines
  function rh_to_dewpoint( tmax, tmin, rh ) result( tdp )
    real :: tdp     ! the calculated dew point temperature (C)

    real :: tmax    ! maximum daily air temperature (C)
    real :: tmin    ! minimum daily air temperature (C)
    real :: rh      ! daily average relative humidity (%)

    ! + + + PARAMETERS + + +
    real c1, c2, c3
    ! c1, c2, c3 -  coefficients for saturated equation
    parameter (c1 = 0.611, c2 = 17.27, c3 = 237.3)

    ! + + + FUNCTIONS CALLED + + +
    real satvappres

    ! + + + LOCAL VARIABLES + + +
    real vpact, logvpc1

    vpact = 0.5 * (rh/100.0) * ( satvappres(tmax) + satvappres(tmin) )

    logvpc1 = log(vpact/c1)

    tdp = c3 * logvpc1 / ( c2 - logvpc1 )

    return
  end function rh_to_dewpoint

  subroutine test_rh2dp

    ! variables
    real :: tmax, tmin, rh_in, tdew
    real vpa, vpsmn, vpsmx, vps, rh_chk
    integer idx, jdx, kdx

    ! function
    real satvappres

    ! test rh code
    do idx = 0, 100
      rh_in = idx
      do jdx = -15, 40
        tmax = jdx
        do kdx = -40, jdx
          tmin = kdx
          tdew = rh_to_dewpoint( tmax, tmin, rh_in )

          ! calculate in reverse
          vpa = satvappres( tdew )
          vpsmn = satvappres( tmin )
          vpsmx = satvappres( tmax )
          vps = 0.5 * (vpsmn + vpsmx)
          rh_chk = 100.0 * vpa / vps

          if ( abs( rh_in - rh_chk ) .gt. 0.00005 ) then
            write(*,*) 'input: tmax, tmin, rh: output tdew, rh:', tmin, tmax, rh_in, tdew, rh_chk
            ! single precision testing of this condition shows that for rh_in of 100% results in some
            ! some rh_chk values greater than 100%, but no more than 0.000053
          end if

        end do
      end do
    end do
    return
  end subroutine test_rh2dp

end program hpdn_to_cligen

