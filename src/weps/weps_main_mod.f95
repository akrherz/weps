!$Author$
!$Date$
!$Revision$
!$HeadURL$

module weps_main_mod

    logical :: old_run_file
    character*512 :: clifil  ! climate file name
    character*512 :: runfil  ! run file name
    character*512 :: subfil  ! subdaily wind file name
    character*512 :: winfil  ! wind file name
    character*256 :: usrnam  ! user name
    character*256 :: farmid  ! Farm identifier
    character*256 :: tractid ! Tract identifier
    character*256 :: fieldid ! Field identifier

    integer :: run_rot_cycles ! number of rotation cycles

    integer :: id     ! initial simulation day of month
    integer :: im     ! initial simulation month of year
    integer :: iy     ! initial simulation year
    integer :: ld     ! final (last) simulation day of month
    integer :: lm     ! final (last) simulation month of year
    integer :: ly     ! final (last) simulation year

    character*512 :: rootp*512  ! the root path from which the weps command was started.

    integer :: daysim  ! current day number of the simulation run
    integer :: ijday   ! This variable contains the initial julian day of the simulation run.
    integer :: ljday   ! This variable contains the last julian day of the simulation run.
    integer :: maxper  ! The maximum number of years in a rotation cycle of all subregions.
                       ! All subregion rotation cycle period lengths (in years) must be a factor
                       ! in this value.  For example, 3 subregions with individual rotation
                       ! periods of 2, 3, and 4 years each would have a "maxper" value of 12
                       ! years.  Note that each of the individual subregion rotation periods can
                       ! divide evenly into the "maxper" value.
    integer :: ncycles ! a count of the number of maxper cycles that have been completed in the simulation run.

  contains

    subroutine wepsinit

      ! Initializes variables in common blocks

      use erosion_data_struct_defs, only: am0eif

      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'm1subr.inc'

      integer idx

      ! main/weps.for
      do idx = 1, mnsub
          amnryr(idx) = 1        ! m1subr.inc
      end do
      daysim = 0
      maxper = 1

      ! set initialization flags
      am0dif = .true.            ! m1flag.inc
      am0eif = .true.            ! m1flag.inc
      am0sif = .true.            ! m1flag.inc
      am0ifl = .true.            ! m1flag.inc

      ! set grid flag until first gridding is done
      am0gdf = .false.           ! m1flag.inc

      ! set output flag to initialize output arrays
      am0oif = .true.            ! m1flag.inc

      ! set initialization, calibration, and report loop flags
      init_loop = .false.        ! m1flag.inc
      calib_loop = .false.       ! m1flag.inc
      report_loop = .false.      ! m1flag.inc

      return
    end subroutine wepsinit


end module weps_main_mod

