!$Author$
!$Date$
!$Revision$
!$HeadURL$

      program test_date

!     + + + PURPOSE + + +
!     Test date routine

      use datetime_mod, only: julday, caldat
      use f2kcli, only: COMMAND_ARGUMENT_COUNT

!     + + + LOCAL VARIABLES + + +
      integer cd, cm, cy, jday, begin_jday, end_jday
      integer begin_yr, nyears

!   cd        - The calendar day of month
!   cm        - The calendar month of year
!   cy        - The calendar year
!   jday      - julian day
!   begin_jday - The first julian day
!   end_jday  - The last julian day
!   begin_yr  - the first year
!   nyears    - number of years to test

      ! declarations for f2k commandline functions
      integer cmd_iarg      ! Temporary var for retrieving integer cmdline args
      character*512 argv    ! For Fortran 2k commandline parsing
      integer       idx
      integer       numarg
      integer       ll,ss

!   idx         - Generic loop counter.
!     argv    - a specified arg from the list of command line arguments.
!   numarg    - number of arguments passed on the command line.
!     + + + DATA INITIALIZATIONS + + +

      begin_yr = 1
      nyears = 1

!     + + + END SPECIFICATIONS + + +

      ! Read command line arguments and options
      ! Will now use the Fortran 2k commandline parsing support - LEW
      ! There cannot be any space between the option and any arguments,
      ! e.g. '-i#' is ok but '-i #' is not.
      ! Any option arguments that have any spaces in them must be quoted,
      ! e.g. '-i"C:\Program Files"' is ok but '-iC:\Program Files' is not.

      numarg = COMMAND_ARGUMENT_COUNT()  !Fortran 2k compatible call

      if (numarg .gt. 0) then
        do 09 idx = 1, numarg
          call GET_COMMAND_ARGUMENT(idx,argv,ll,ss)  !Fortran 2k compatible call
!         write(6,*) 'argv ',i,' is: ', trim(argv)

          if(argv(1:1) .ne. '-') then   !make sure all options start with '-'
            write(*,*) 'Option ignored, no option flag: ', argv
            goto 9  !Go get next arg
          endif

          !command line help prompt
          if( (argv(2:2).eq.'?').or.(argv(2:2).eq.'h')) then
              write(*,*) 'Valid command line options:'
              write(*,*) '-?  Display this help screen'
              write(*,*) '-h  Display this help screen'
              write(*,*)

              write(*,*) '-b#  Specify beginning year for test'
              write(*,*) '    # = beginning year (default 1)'

              write(*,*) '-y#  Specify number of years to test'
              write(*,*) '    # = number of years (default 1)'

              call exit(1)

          !specify beginning year for test
          else if(argv(2:2) .eq. 'b') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. -4713) .or. (cmd_iarg .gt. 3267) ) then
              write(*,*)                                                &
     &         'Beginning year is outside of current Julian Epoch, using 1', trim(argv)
            else
              begin_yr = cmd_iarg
            endif

          !specify beginning year for test
          else if(argv(2:2) .eq. 'y') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 1 ) then
              write(*,*)                                                &
     &         'Number of years must be 1 or greater, using 1, you used:', trim(argv)
            else if( cmd_iarg+begin_yr .gt. 3267 ) then
              write(*,*)                                                &
     &         'Number of years is outside of current Julian Epoch, using 1, you used:', trim(argv)
            else
              nyears = cmd_iarg
            endif

          else
              write(*,*) 'Ignoring unknown option: ', trim(argv)
          endif
 09     continue
      endif

      begin_jday = julday(1, 1, begin_yr)
      end_jday = julday(31, 12, begin_yr+nyears-1)

      do jday = begin_jday, end_jday 
          call caldat (jday, cd, cm, cy)

          if( (cm .eq. 2) .and. (cd .eq. 29) ) then
              write(*,*) jday, cd, cm, cy
          end if

      end do

      stop
      end program test_date

