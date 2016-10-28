!$Author$
!$Date$
!$Revision$
!$HeadURL$

program adjust_wind_to_cli

  ! usage: adjust_win_to_cli cligen_file_name windgen_file_name

  ! Outputs to stdout, the adjusted windgen file data

  use datetime_mod, only: julday
  use f2kcli, only: COMMAND_ARGUMENT_COUNT
  use Running_Stats

  implicit none

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

  type WindgenData
    integer :: day    ! day of month
    integer :: month  ! month of year
    integer :: year   ! year
    real :: dirwind   ! wind direction (degrees from N)
    real, dimension(1:24) :: hrwind   ! Hourly wind speed (m/s)
  end type WindgenData

  type(CligenData) :: cligen
  type(WindgenData) :: windgen
  type(statistics) :: daywind

  integer :: julian_cli
  integer :: julian_win

  ! variables
  character*256 :: line
  integer       :: ioc
  integer       :: linecnt

  character*128 :: file_cli
  integer       :: lui_cli
  parameter (lui_cli=100)
  character*128 :: file_win
  integer       :: lui_win
  parameter (lui_win=101)
  integer       :: lui
  parameter (lui=5)  ! stdin
  integer       :: luo
  parameter (luo=6)  ! stdout

  integer       :: idx

  character*512 argv    ! For Fortran 2k commandline parsing
  integer       numarg
  integer       ll,ss

  real :: factor

  linecnt = 0

  ! read cligen and windgen file names from command line
  numarg = COMMAND_ARGUMENT_COUNT()  !Fortran 2k compatible call
  do idx = 1, numarg
    call GET_COMMAND_ARGUMENT(idx,argv,ll,ss)  !Fortran 2k compatible call
    if( idx .eq. 1 ) then
      file_cli = trim(argv)
    end if
    if( idx .eq. 2 ) then
      file_win = trim(argv)
    end if
  end do

  ! open cligen file
  open( lui_cli, FILE=trim(file_cli), POSITION='REWIND', IOSTAT=ioc)
  if( ioc .ne. 0 ) then
    ! unsucessful open
    write(*,*) 'unable to open CLIGEN file: ', trim(file_cli)
    stop
  end if

  ! open cligen file
  open( lui_win, FILE=trim(file_win), POSITION='REWIND', IOSTAT=ioc)
  if( ioc .ne. 0 ) then
    ! unsucessful open
    write(*,*) 'unable to open WINDGEN file: ', trim(file_win)
    stop
  end if

  ! read and skip cligen header
  do idx = 1, 15
    read (lui_cli,'(a)',iostat=ioc) line
  end do

  ! read and echo windgen header
  do idx = 1, 7
    read (lui_win,'(a)',iostat=ioc) line
    if( idx .eq. 2 ) then
      write(luo,'(2a)') trim(line), ' Adjusted to match Average daily cligen file wind speed'
    else
      write(luo,'(a)') trim(line)
    end if
  end do

  ! read the first CLIGEN line
  call read_cli()

  ! read first WINDGEN line
  call read_win()

  do while( ioc .eq. 0 )
    if( julian_cli .lt. julian_win ) then
      ! CLIGEN data is earlier than WINDGEN data
      call read_cli()
    else if( julian_cli .gt. julian_win ) then
      ! WINDGEN data is earlier than CLIGEN data
      call read_win()
    else
      ! correct day, find average wind
      call rs_initial(daywind)
      do idx = 1, 24
        call rs_newnum(daywind, windgen%hrwind(idx) )
      end do
      ! check and correct hourly wind values
      if( rs_mean(daywind) .gt. 0 ) then
        factor = cligen%wind / rs_mean(daywind)
        do idx = 1, 24
          windgen%hrwind(idx) = windgen%hrwind(idx) * factor
        end do
      else
        do idx = 1, 24
          windgen%hrwind(idx) = cligen%wind
        end do
      end if
      ! write WINGEN line to stdout
      write(luo,'(2i3,i5,25f6.1)') windgen
      linecnt = linecnt + 1
      ! get next day for both
      call read_cli()
      call read_win()
    end if

  end do

  ! finished reading file
  write(*,*) 'Number of days where WINDGEN Data was adjusted: ', linecnt

contains

  subroutine read_cli()
    read (lui_cli,'(a)',iostat=ioc) line
    if (ioc .eq. 0) then
      ! sucessful read from file, parse line
      read(line, *, iostat=ioc) cligen
      if (ioc .ne. 0) then
        ! We have a failure parsing the data line
        write(*,*) 'Cligen Data line parsing failed at line ', linecnt
        stop
      else
        ! sucessful parsing of line
        julian_cli = julday( cligen%day, cligen%month, cligen%year )
      end if
    end if
  end subroutine read_cli

  subroutine read_win()
    read (lui_win,'(a)',iostat=ioc) line
    if (ioc .eq. 0) then
      ! sucessful read from file, parse line
      read(line, *, iostat=ioc) windgen
      if( ioc .ne. 0 ) then
        ! parse failed
        write(*,*) 'Windgen Data line parsing failed with input line: ', trim(line)
        stop
      else
        ! sucessful parsing of line
        julian_win = julday( windgen%day, windgen%month, windgen%year )
      end if
    end if
  end subroutine read_win

end program adjust_wind_to_cli

