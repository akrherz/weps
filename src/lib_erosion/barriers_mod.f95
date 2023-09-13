  !$Author$
!$Date$
!$Revision$
!$HeadURL$

! A polyline is simply an ordered set of points.

module barriers_mod
  use Points_Mod, only: point, slen
  implicit none
 
  type barrier_params
    real :: amzbr          ! Height in meters.
    real :: amxbrw         ! Width in meters.
    real :: ampbr          ! Porosity as fraction of the outline of each barrier, silhouette area
  end type barrier_params

  type barrier_data
     character*80 :: amzbt  ! Barrier type
     integer :: np   ! number of points in barrier_params and polyline point array
     type(point), dimension(:), allocatable :: points  ! the polyline points
     type( barrier_params), dimension(:), allocatable :: param
  end type barrier_data

  type barrier_day_state
     character*80 :: st_desc ! text description of the barrier state on this day
     integer :: doy          ! day of year the state occurs
  end type barrier_day_state

  type barrier_climate_transition       ! used with season flag type 2 and defines parameters for climate based triggers for 
                             ! transition from one barrier state to another such as leaf off to leaf on, or low height to high height (grass trap strip?)
     integer :: beg_flg ! multilevel flag defining the climate data type which will trigger beginning of transition
                             ! 0 - number of days temperature is above base trigger temperature since previous known state date
                             ! 1 - number of days temperature is below base trigger temperature since previous known state date
                             ! 2 - accumulation of Growing degree days (GDD) since previous known state date
                             ! 3 - accumulation of Cooling degree days (CDD) since previous known state date
                             ! 4 - accumulation of rainfall depth (above minimum depth) since previous known state date
                             ! 5 - accumulation of days with rainfall below minimum depth (rainfall above minimum depth
                             !     resets accumulation) since  previous known state date
                             ! 6 - accumulation of humidity levels above base humidity since previous known state date
                             ! 7 - accumulation of humidity levels below base humidity since previous known state date
                             ! 8 - day length past a threshold, and either increasing or decreasing
     real :: beg_thresh  ! accumulation threshold value for each of the methods above
     real :: beg_base    ! base value above/below which accumulation occurs
     real :: beg_accum   ! total accumulation of beg_flg specified quantity since given day of year
     integer :: end_flg ! multilevel flag defining the method for determining completion of leaf emergence/drop
                             ! 0 - days to complete transition are specified
                             ! 1 - Growing Degree Days (GDD) to full transition are specified
                             ! 2 - Cooling Degree Days (CDD) to full transition are specified
     real :: end_thresh ! Accumulation threshold value where full transition occurs
     real :: end_base   ! base value above/below which accumulation occurs
     real :: end_accum  ! total accumlation of end_flg specified quantity since beg_threshold exceeded
  end type barrier_climate_transition

  type barrier_climate
     integer :: trig_logic  ! logic for combination of multiple thresholds in phase transitions initiation
                            ! 0 - OR logic, phase transition begins when any threshold is reached
                            ! 1 - AND logic, phase transition begins when all thresholds are reached
     integer :: nthresh     ! number of threshold methods
     type(barrier_climate_transition), dimension(:), allocatable :: cli_tran
  end type barrier_climate

  type barrier_seasonal
     character*80 :: amzbt  ! Barrier type
     integer :: seas_flg    ! multi level flag defining implementation of barrier seasons
                            ! 0 - use seasonal data as given with time interpolation
                            ! 1 - set barrier data on doy given. Value remains constant until the next doy given, ie. no time interpolation
                            ! 2 - Manage the timing of barrier state (seasonal) transitions internally with a climate based model
     integer :: ntm         ! number of time marks specified for barrier
     integer :: np          ! number of points in barrier_params and polyline point array
     integer :: tran_days   ! days since beginning of barrier state transition
     type(point), dimension(:), allocatable :: points  ! the polyline points
     type(barrier_day_state), dimension(:), allocatable :: dst  ! label and day of year for time marks
     type(barrier_params), dimension(:,:), allocatable :: param
     type(barrier_climate), dimension(:), allocatable :: clim
  end type barrier_seasonal

  interface create_barrier
      module procedure create_barrier_fixed
      module procedure create_barrier_seasonal
      module procedure create_barrier_transition
  end interface create_barrier

  interface destroy_barrier
      module procedure destroy_barrier_fixed
      module procedure destroy_barrier_seasonal
      module procedure destroy_barrier_transition
  end interface destroy_barrier

  public :: create_barrier
  public :: destroy_barrier

  type(barrier_data), dimension(:), allocatable :: barrier
  type(barrier_seasonal), dimension(:), allocatable :: barseas

contains
 
  ! allocates a barrier_data structure which can contain nump points
  subroutine create_barrier_fixed(barr, nump)
    type(barrier_data), intent(inout) :: barr   ! barrier to be allocated
    integer, intent(in) :: nump  ! number of points in barrier_params and polyline created

    ! local variable
    integer :: sum_stat
    integer :: alloc_stat

    sum_stat = 0
    allocate(barr%points(nump), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(barr%param(nump), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    if( sum_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to allocate memory for barrier"
      barr%np = 0
    else
      barr%np = nump
    end if 
  end subroutine create_barrier_fixed
 
  ! deallocates a barrier_data structure
  subroutine destroy_barrier_fixed(barr)
    type(barrier_data), intent(inout) :: barr

    ! local variable
    integer :: sum_stat
    integer :: dealloc_stat

    sum_stat = 0
    deallocate(barr%points, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    deallocate(barr%param, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    if( sum_stat .gt. 0 ) then
      ! deallocation failed
      write(*,*) "ERROR: unable to deallocate memory for Polygon"
    end if
  end subroutine destroy_barrier_fixed

  ! allocates a barrier_data structure which can contain nump points
  subroutine create_barrier_seasonal(barr, nump, numtm, sflg)
    type(barrier_seasonal), intent(inout) :: barr ! barrier to be allocated
    integer, intent(in) :: nump  ! number of points in barrier_params and polyline created
    integer, intent(in) :: numtm ! number of time marks in barrier_params
    integer, intent(in) :: sflg  ! flag which selects type of internal season transition

    ! local variable
    integer :: sum_stat
    integer :: alloc_stat
    integer :: iseas

    sum_stat = 0
    allocate(barr%points(nump), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(barr%dst(numtm), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(barr%param(nump,numtm), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    if( sflg .eq. 2 ) then
       allocate(barr%clim(numtm), stat=alloc_stat)
       sum_stat = sum_stat + alloc_stat
    else
       allocate(barr%clim(0), stat=alloc_stat)
       sum_stat = sum_stat + alloc_stat
    end if

    if( sum_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to allocate memory for barrier"
      barr%np = 0
      barr%ntm = 0
      barr%tran_days = 0
      barr%seas_flg = 0
    else
      barr%np = nump
      barr%ntm = numtm
      barr%tran_days = 0
      barr%seas_flg = sflg
      do iseas = 1, barr%ntm
        barr%dst(iseas)%st_desc = 'fixed'
        barr%dst(iseas)%doy = 0
        if( barr%seas_flg .eq. 2 ) then
          barr%clim(iseas)%trig_logic = 0
          barr%clim(iseas)%nthresh = 0
        end if
      end do
    end if 
  end subroutine create_barrier_seasonal
 
  ! deallocates a barrier_data structure
  subroutine destroy_barrier_seasonal(barr)
    type(barrier_seasonal), intent(inout) :: barr

    ! local variable
    integer :: sum_stat
    integer :: dealloc_stat

    sum_stat = 0
    deallocate(barr%points, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    deallocate(barr%dst, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    deallocate(barr%param, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    deallocate(barr%clim, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    if( sum_stat .gt. 0 ) then
      ! deallocation failed
      write(*,*) "ERROR: unable to deallocate memory for Polygon"
    end if
  end subroutine destroy_barrier_seasonal

  subroutine create_barrier_transition(clim, nthresh, trig_logic)
    type(barrier_climate), intent(inout) :: clim ! barrier climate array element to be allocated
    integer, intent(in) :: nthresh    ! number of Threshold methods
    integer, intent(in) :: trig_logic ! Time mark to be allocated
  
    ! local variable
    integer :: sum_stat
    integer :: alloc_stat
    integer :: ithresh

    sum_stat = 0
    allocate(clim%cli_tran(nthresh), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat

    if( sum_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to allocate memory for barrier threshold"
      clim%nthresh = 0
      clim%trig_logic = 0
    else
      clim%nthresh = nthresh
      clim%trig_logic = trig_logic
      do ithresh = 1, clim%nthresh
        clim%cli_tran(ithresh)%beg_flg = -1
        clim%cli_tran(ithresh)%beg_thresh = 0.0
        clim%cli_tran(ithresh)%beg_base = 0.0
        clim%cli_tran(ithresh)%beg_accum = 0.0
        clim%cli_tran(ithresh)%end_flg = -1
        clim%cli_tran(ithresh)%end_thresh = 0.0
        clim%cli_tran(ithresh)%end_base = 0.0
        clim%cli_tran(ithresh)%end_accum = 0.0
      end do
    end if 
  end subroutine create_barrier_transition

  ! deallocates a barrier_data structure
  subroutine destroy_barrier_transition(cli_tran)
    type(barrier_climate_transition), dimension(:), allocatable, intent(inout) :: cli_tran ! barrier climate array element to be deallocated

    ! local variable
    integer :: sum_stat
    integer :: dealloc_stat

    sum_stat = 0
    deallocate(cli_tran, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    if( sum_stat .gt. 0 ) then
      ! deallocation failed
      write(*,*) "ERROR: unable to deallocate memory for barrier threshold array"
    end if
  end subroutine destroy_barrier_transition

  ! Given the day of year, sets the present barrier characteristics for all barriers
  subroutine set_barrier_season(doy)

    use lin_interp_mod, only: lin_interp
    use file_io_mod, only: luobarr
    use datetime_mod, only: get_simdate_daysim, get_simdate_year, isleap
    use string_mod, only: space2hyphen
    use erosion_data_struct_defs, only: am0efl

    ! argument declarations
    integer, intent(in) :: doy  ! day of year for setting barrier season

    ! local variables
    integer :: bdx    ! barrier loop variable
    integer :: tdx    ! time mark loop variable
    integer :: pdx    ! point loop variable
    integer :: ndx    ! threshold loop variable
    integer :: low_tm ! low time mark index
    integer :: hi_tm  ! high time mark index
    real :: frac_tm   ! fraction of time into bracketed time interval
    integer :: doy_adj  ! adjustment to day of year based on begin/end of year location of actual doy
    integer :: sgn_adj  ! adjustment to sign of calculation based on begin/end of year location of actual doy
    real :: delta_x

    ! loop over all barriers
    do bdx = 1, size(barrier)
      ! check number of time marks in seasonal barrier
      if( barseas(bdx)%ntm .gt. 1 ) then
        ! this barrier contains seasons
        ! find location in time mark array
        ! NOTE: This assumes that the time marks are in DAY OF YEAR Order!!
        if( (doy .lt. barseas(bdx)%dst(1)%doy) .or. (doy .ge. barseas(bdx)%dst(barseas(bdx)%ntm)%doy) ) then
          low_tm = barseas(bdx)%ntm
        else
          do tdx = 1, barseas(bdx)%ntm-1
            ! search for low time mark index
            if( (doy .ge. barseas(bdx)%dst(tdx)%doy) .and. (doy .lt. barseas(bdx)%dst(tdx+1)%doy) ) then
              low_tm = tdx
              exit
            end if
          end do
        end if

        ! set high time mark index
        if( low_tm .lt. barseas(bdx)%ntm ) then
          ! no wrapping required for bracketing index
          hi_tm = low_tm + 1
        else
          ! low_tm was at end of year, wrap to bracketing index
          hi_tm = 1
        end if

        if( real(doy) .eq. barseas(bdx)%dst(low_tm)%doy) then
            barseas(bdx)%tran_days = 0
        end if

        select case (barseas(bdx)%seas_flg)
        case (0)  ! do interpolation between all time points

          if( low_tm .lt. hi_tm ) then
            ! find fraction of distance in time between time marks
            frac_tm = (real(doy) - barseas(bdx)%dst(low_tm)%doy) &
                    / (barseas(bdx)%dst(hi_tm)%doy - barseas(bdx)%dst(low_tm)%doy)
          else
            ! adjust calculation of location in time
            if( doy .ge. barseas(bdx)%dst(low_tm)%doy ) then
              doy_adj = 0
              sgn_adj = -1
            else
              if( isleap(get_simdate_year()) ) then
                doy_adj = 366
              else
                doy_adj = 365
              end if
              sgn_adj = 1
            end if
            ! find fraction of distance in time between time marks adjusted for wrapping
            frac_tm = sgn_adj * (real(doy) + doy_adj - barseas(bdx)%dst(low_tm)%doy) &
                    / (barseas(bdx)%dst(hi_tm)%doy + doy_adj - barseas(bdx)%dst(low_tm)%doy)
          end if

          if( frac_tm .gt. 0 ) then
              barseas(bdx)%tran_days = barseas(bdx)%tran_days + 1
          end if

          ! interpolate barrier params in time, copying into fixed barrier structure
          do pdx = 1, barseas(bdx)%np
            barrier(bdx)%param(pdx)%amzbr = lin_interp(frac_tm, barseas(bdx)%param(pdx,low_tm)%amzbr, &
                                                                barseas(bdx)%param(pdx,hi_tm)%amzbr)
            barrier(bdx)%param(pdx)%amxbrw = lin_interp(frac_tm, barseas(bdx)%param(pdx,low_tm)%amxbrw, &
                                                                 barseas(bdx)%param(pdx,hi_tm)%amxbrw)
            barrier(bdx)%param(pdx)%ampbr = lin_interp(frac_tm, barseas(bdx)%param(pdx,low_tm)%ampbr, &
                                                                barseas(bdx)%param(pdx,hi_tm)%ampbr)
          end do

        case (1)  ! set barrier to value at previous time mark until next time mark

          barseas(bdx)%tran_days = barseas(bdx)%tran_days + 1

          do pdx = 1, barseas(bdx)%np
            barrier(bdx)%param(pdx)%amzbr = barseas(bdx)%param(pdx,low_tm)%amzbr
            barrier(bdx)%param(pdx)%amxbrw = barseas(bdx)%param(pdx,low_tm)%amxbrw
            barrier(bdx)%param(pdx)%ampbr = barseas(bdx)%param(pdx,low_tm)%ampbr
          end do

        case (2)  ! determine time based on climatic information and interpolate
          if( doy .eq. barseas(bdx)%dst(low_tm)%doy ) then
            ! reset accumulators to start this process
            do ndx = 1, barseas(bdx)%clim(hi_tm)%nthresh
              barseas(bdx)%clim(hi_tm)%cli_tran(ndx)%beg_accum = 0.0
            end do
          end if
          ! find fraction of state transition
          frac_tm = state_transition( barseas(bdx)%clim(hi_tm), doy )

          if( frac_tm .gt. 0 ) then
              barseas(bdx)%tran_days = barseas(bdx)%tran_days + 1
          end if

          ! interpolate barrier params in time, copying into fixed barrier structure
          do pdx = 1, barseas(bdx)%np
            barrier(bdx)%param(pdx)%amzbr = lin_interp(frac_tm, barseas(bdx)%param(pdx,low_tm)%amzbr, &
                                                                barseas(bdx)%param(pdx,hi_tm)%amzbr)
            barrier(bdx)%param(pdx)%amxbrw = lin_interp(frac_tm, barseas(bdx)%param(pdx,low_tm)%amxbrw, &
                                                                 barseas(bdx)%param(pdx,hi_tm)%amxbrw)
            barrier(bdx)%param(pdx)%ampbr = lin_interp(frac_tm, barseas(bdx)%param(pdx,low_tm)%ampbr, &
                                                                barseas(bdx)%param(pdx,hi_tm)%ampbr)
          end do

        end select

      else  ! this barrier does not have seasons, copy into fixed barrier structure
        low_tm = 0
        hi_tm = 0
        do pdx = 1, barseas(bdx)%np
          barrier(bdx)%param(pdx)%amzbr = barseas(bdx)%param(pdx,1)%amzbr
          barrier(bdx)%param(pdx)%amxbrw = barseas(bdx)%param(pdx,1)%amxbrw
          barrier(bdx)%param(pdx)%ampbr = barseas(bdx)%param(pdx,1)%ampbr
        end do
      end if

      if( (am0efl .gt. 0) .and. (size(barseas) .gt. 0) ) then
        if( get_simdate_daysim() .eq. 1 ) then
          ! write header to barrier daily output file
          write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') '#simday doy yr  Barrier_Description npt'
          do pdx = 1, barrier(bdx)%np
            write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') ' delta_x height  width porosi'
          end do
          ! write transition days
          write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') ' tran_days'
          ! write time mark
          write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') ' hi_tm'
          ! write barrier season type
          write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') ' seas_flg'
          if( barseas(bdx)%seas_flg .eq. 2 ) then
            ! these values are populated
            write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') ' logic'
            write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') ' nthresh'
            do ndx = 1, barseas(bdx)%clim(hi_tm)%nthresh
              write(UNIT=luobarr(bdx),FMT='(a)',advance='NO') ' b_flg beg_accu beg_thre e_flg end_accu end_thre'
            end do
          end if
          write(UNIT=luobarr(bdx),FMT='(a)') ''
        end if

        ! insert double blank lines to demarcate years
        if( doy .eq. 1 ) then
          write (luobarr(bdx),'(a)')
          write (luobarr(bdx),'(a)')
        end if
    
        ! write data to barrier daily output file
        write(UNIT=luobarr(bdx),FMT='(1x,i6,1x,i3,1x,i4,1x,a20)',advance='NO') &
               get_simdate_daysim(), doy, get_simdate_year(), space2hyphen( trim(barrier(bdx)%amzbt) )
        write(UNIT=luobarr(bdx),FMT='(1x,i3)',advance='NO') barrier(bdx)%np
        delta_x = 0.0
        do pdx = 1, barrier(bdx)%np
          if( pdx .gt. 1 ) then
            delta_x = delta_x + slen(barrier(bdx)%points(pdx-1), barrier(bdx)%points(pdx))
          end if
          write(UNIT=luobarr(bdx),FMT='(1x,f7.1)',advance='NO') delta_x
          write(UNIT=luobarr(bdx),FMT='(3(1x,f7.4))',advance='NO') &
              barrier(bdx)%param(pdx)%amzbr, barrier(bdx)%param(pdx)%amxbrw, barrier(bdx)%param(pdx)%ampbr
        end do
        ! write transition days
        write(UNIT=luobarr(bdx),FMT='(1x,i0)',advance='NO') barseas(bdx)%tran_days
        ! write time mark
        write(UNIT=luobarr(bdx),FMT='(1x,i0)',advance='NO') hi_tm
        ! write barrier season type
        write(UNIT=luobarr(bdx),FMT='(1x,i2)',advance='NO') barseas(bdx)%seas_flg
        if( barseas(bdx)%seas_flg .eq. 2 ) then
          ! these values are populated
          write(UNIT=luobarr(bdx),FMT='(1x,i2)',advance='NO') barseas(bdx)%clim(hi_tm)%trig_logic
          write(UNIT=luobarr(bdx),FMT='(1x,i0)',advance='NO') barseas(bdx)%clim(hi_tm)%nthresh
          do ndx = 1, barseas(bdx)%clim(hi_tm)%nthresh
            write(UNIT=luobarr(bdx),FMT='(1x,i2)',advance='NO') barseas(bdx)%clim(hi_tm)%cli_tran(ndx)%beg_flg
            write(UNIT=luobarr(bdx),FMT='(2(1x,f8.4))',advance='NO') &
                barseas(bdx)%clim(hi_tm)%cli_tran(ndx)%beg_accum, barseas(bdx)%clim(hi_tm)%cli_tran(ndx)%beg_thresh
            write(UNIT=luobarr(bdx),FMT='(1x,i2)',advance='NO') barseas(bdx)%clim(hi_tm)%cli_tran(ndx)%end_flg
            write(UNIT=luobarr(bdx),FMT='(2(1x,f8.4))',advance='NO') &
                barseas(bdx)%clim(hi_tm)%cli_tran(ndx)%end_accum, barseas(bdx)%clim(hi_tm)%cli_tran(ndx)%end_thresh
          end do
        end if
        ! write newline character
        write(UNIT=luobarr(bdx),FMT='(a)') ''
      end if
    end do
    
  end subroutine set_barrier_season

  ! finds accumulation of climate factors which will determine when barrier state changes
  ! Most commonly represents deciduous trees, but can be adapted to other plants
  ! A useful resource for developing barrier parameters could be:
  ! https://www.usanpn.org (National Phenology Network)
  ! Options 2-3 see:
  ! Caroline A. Polgar and Richard B. Primack. Leaf-out phenology of temperate woody plants:
  ! from trees to ecosystems. New Phytologist (2011) 191: 926–941 doi: 10.1111/j.1469-8137.2011.03803.x
  ! Options 4-7 may best apply to tropical / savannah ecosystems
  ! see:
  ! February EC, Higgins SI (2016) Rapid Leaf Deployment Strategies in a Deciduous Savanna.
  ! PLoS ONE 11(6): e0157833. doi:10.1371/journal.
  ! and:
  ! Niles J. Hasselquist, Michael F. Allen,Louis S. Santiago. Water relations of evergreen
  ! and drought-deciduous trees along a seasonally dry tropical forest chronosequence.
  ! Oecologia (2010) 164:881–890 DOI 10.1007/s00442-010-1725-y pone.0157833
  ! Additional research may be required to refine the procedures specified here.
  function state_transition( clim, doy ) result(frac_tm)
    use crop_climate_mod, only: warmday_cum, heatunit, coldunit, coldday_cum
    use air_water_mod, only: precip_cum, no_precip_cum, high_humid_cum, low_humid_cum
    use solar_mod, only: amalat, civilrise, daylen
    type(barrier_climate), intent(inout) :: clim
    integer, intent(in) :: doy  ! day of year
    real :: frac_tm

    integer :: idx
    integer :: alloc_stat
    real, dimension(:), allocatable :: frac_test
    real :: hrlt    ! daylength
    real :: hrlty   ! daylength yesterday
    
    allocate( frac_test(clim%nthresh), stat = alloc_stat )
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'Unable to allocate memory for frac_test.'
    end if

    select case ( clim%trig_logic )
    case (0) ! OR logic, phase transition begins when any threshold is reached
      frac_tm = 0.0
    case (1) ! AND logic, phase transition begins when all thresholds are reached
      frac_tm = 1.0
    case (2) ! Second method starts to accumulate after first threshold is met
      frac_tm = 1.0
    end select

    do idx = 1, clim%nthresh

      ! test if threshold had been crossed. Prevent returning to previous state 
      if( clim%cli_tran(idx)%beg_accum .le. clim%cli_tran(idx)%beg_thresh ) then
        ! find current accumulation for when transition begins
        select case (clim%cli_tran(idx)%beg_flg)
        case (0)  ! number of consecutive days temperature is above base trigger temperature
          call warmday_cum( clim%cli_tran(idx)%beg_accum, clim%cli_tran(idx)%beg_base )
        case (1)  ! number of consecutive days temperature is below base trigger temperature
          call coldday_cum( clim%cli_tran(idx)%beg_accum, clim%cli_tran(idx)%beg_base )
        case (2)  ! accumulation of growing degree days (no optimum temperature specified)
          clim%cli_tran(idx)%beg_accum = clim%cli_tran(idx)%beg_accum + heatunit( clim%cli_tran(idx)%beg_base )
        case (3)  ! accumulation of cold degree days
          clim%cli_tran(idx)%beg_accum = clim%cli_tran(idx)%beg_accum + coldunit( clim%cli_tran(idx)%beg_base )
        case (4)  ! accumulation of rainfall depth above minimum
          clim%cli_tran(idx)%beg_accum = precip_cum( clim%cli_tran(idx)%beg_accum, clim%cli_tran(idx)%beg_base )
        case (5)  ! accumulation of period with no rainfall above minimum value
          clim%cli_tran(idx)%beg_accum = no_precip_cum( clim%cli_tran(idx)%beg_accum, clim%cli_tran(idx)%beg_base )
        case (6)  ! number of consecutive days humidity is above base humidity
          clim%cli_tran(idx)%beg_accum = high_humid_cum( clim%cli_tran(idx)%beg_accum, clim%cli_tran(idx)%beg_base )
        case (7)  ! number of consecutive days humidity is below base humidity
          clim%cli_tran(idx)%beg_accum = low_humid_cum( clim%cli_tran(idx)%beg_accum, clim%cli_tran(idx)%beg_base )
        case (8)  ! daylength is past threshold value either increasing or decreasing
          hrlty = daylen(amalat, doy-1, civilrise)
          hrlt = daylen(amalat, doy, civilrise)
          if( clim%cli_tran(idx)%beg_base .lt. clim%cli_tran(idx)%beg_thresh ) then
            ! increasing daylength required
            if( hrlty .lt. hrlt ) then
              ! increasing daylength found
              clim%cli_tran(idx)%beg_accum = hrlt
            end if
          else
            ! decreasing daylength
            if( hrlty .gt. hrlt ) then
              ! increasing daylength found
              clim%cli_tran(idx)%beg_accum = hrlt
            end if
          end if
        end select
      end if

      if( clim%cli_tran(idx)%beg_accum .le. clim%cli_tran(idx)%beg_thresh ) then
        ! no transition yet
        frac_test(idx) = 0.0
        clim%cli_tran(idx)%end_accum = 0.0
      else
        ! transition has begun
        ! test if threshold had been crossed. Prevent returning to previous state 
        if( clim%cli_tran(idx)%end_accum .lt. clim%cli_tran(idx)%end_thresh ) then
          ! find current accumulation for when transition is complete
          select case (clim%cli_tran(idx)%end_flg)
          case (0)  ! days to complete transition are specified
            clim%cli_tran(idx)%end_accum = clim%cli_tran(idx)%end_accum + 1
          case (1)  ! Growing Degree Days (GDD) to complete transition are specified
            clim%cli_tran(idx)%end_accum = clim%cli_tran(idx)%end_accum + heatunit( clim%cli_tran(idx)%end_base )
          case (2)  ! Cold Degree Days (CDD) to complete transition are specified
            clim%cli_tran(idx)%end_accum = clim%cli_tran(idx)%end_accum + coldunit( clim%cli_tran(idx)%end_base )
          end select
        end if

        if( clim%cli_tran(idx)%end_accum .ge. clim%cli_tran(idx)%end_thresh ) then
          ! transition is complete
          frac_test(idx) = 1.0
        else
          ! in transition
          frac_test(idx) = clim%cli_tran(idx)%end_accum / clim%cli_tran(idx)%end_thresh
        end if
      end if

      select case( clim%trig_logic )
      case (0) ! OR logic, phase transition begins when any threshold is reached
        frac_tm = max(frac_tm, frac_test(idx))
      case (1) ! AND logic, phase transition begins when all thresholds are reached
        frac_tm = min(frac_tm, frac_test(idx))
      case (2) ! Subsequent threshold starts to accumulate after previous threshold is met
        if( idx .gt. 1 ) then
          ! check for threshold and reset subsequent accum if necessary
          if( frac_test(idx-1) .le. 0.0 ) then
            ! previous transition threshold has not been met, reset accum to 0.0
            clim%cli_tran(idx)%beg_accum = 0.0
            frac_test(idx) = 0.0
          end if
        end if
        frac_tm = min(frac_tm, frac_test(idx))
      end select

    end do
    
    deallocate( frac_test, stat = alloc_stat )
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'Unable to deallocate memory for frac_test.'
    end if

  end function state_transition

  subroutine sbbr( cellstate )

!     + + + PURPOSE + + +
!     to calculate the fraction of open field friction velocity
!     from up wind and down wind sources of shelter at all interior nodes

      use erosion_data_struct_defs, only: cellsurfacestate
      use grid_mod, only: imax, jmax, lencell_x, lencell_y, amxsim, awa
      use Points_Mod, only: point
      use pnt_polyline_mod, only: location_intersect, pl_intersect
      use lin_interp_mod, only: lin_interp

!     + + + ARGUMENT DECLARATIONS + + +
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + LOCAL VARIABLES + + +
      integer i, j, n   ! do-loop indices
      integer :: nbr    ! number of barriers
      type(point) ::  pnt_grid  ! point form of grid coordinate
      type(location_intersect) ::  loc_intersect  ! point where upwind direction meets barrier, index in polyline and fraction distance between indexes
      real :: dist   ! distance from grid cell centroid to barrier
      real :: w0br_min   ! minimum value of sheltering effect (fraction of open field fric. vel) for this point
      integer :: npt     ! number of points along the barrier
      real :: zbr_interp  ! value of barrier height interpolated along barrier
      real :: pbr_interp  ! value of barrier porosity interpolated along barrier
      real :: xbrw_interp  ! value of barrier width interpolated along barrier
      real, dimension(:), allocatable :: bar_tint ! temporary array for call to lin_interp
      integer :: alloc_stat  ! return status of memory allocation, deallocation

!     + + + END SPECIFICATIONS + + +

      ! discover number of barriers
      nbr = size(barrier)

      ! update interior nodes
      do i = 1, imax-1
        do j = 1, jmax-1
          ! calculate distance to middle of grid cell (maybe offset from origin)
          pnt_grid%x = (i-0.5)*lencell_x + amxsim(1)%x
          pnt_grid%y = (j-0.5)*lencell_y + amxsim(1)%y

          ! barrier sweep
          w0br_min = 1.0   ! maximum value for parameter
          do n = 1, nbr
            ! find number of points in barrier for interpolations
            npt = size(barrier(n)%param)

            allocate( bar_tint(npt), stat=alloc_stat )
            if ( alloc_stat .gt. 0 ) then
              write(*,*) 'Unable to allocate memory for Barrier interpolation'
              call exit(1)
            end if

            ! look for barrier up wind
            if( pl_intersect( pnt_grid, awa, barrier(n)%points, loc_intersect ) ) then
              ! intersection point found (it is minimum distance for this barrier)
              dist = slen(pnt_grid, loc_intersect%pnt)

              ! barrier influence calculated down wind of barrier
              ! interpolate height along barrier segment
              bar_tint = barrier(n)%param(1:npt)%amzbr
              zbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, bar_tint )
              if (dist .le. 35*zbr_interp) then
                ! distance is close enough for effect
                ! interpolate parameters along barrier segment
                bar_tint = barrier(n)%param(1:npt)%amxbrw
                xbrw_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, bar_tint )
                bar_tint = barrier(n)%param(1:npt)%ampbr
                pbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, bar_tint )

                ! find shelter effect
                w0br_min = min(w0br_min, fu( dist, zbr_interp, xbrw_interp, pbr_interp ) )
              end if
            end if

            ! look for barrier down wind
            if( pl_intersect( pnt_grid, awa-180.0, barrier(n)%points, loc_intersect ) ) then
              ! intersection point found (it is minimum distance for this barrier)
              dist = slen(pnt_grid, loc_intersect%pnt)

              ! barrier influence calculated down wind of barrier
              ! interpolate height along barrier segment
              bar_tint = barrier(n)%param(1:npt)%amzbr
              zbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, bar_tint )
              if (dist .lt. 5*zbr_interp) then
                ! distance is close enough for effect
                ! interpolate parameters along barrier segment
                bar_tint = barrier(n)%param(1:npt)%amxbrw
                xbrw_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, bar_tint )
                bar_tint = barrier(n)%param(1:npt)%ampbr
                pbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, bar_tint )

                ! find shelter effect (on upwind side of barrier use negative distance for correct function value)
                w0br_min = min(w0br_min, fu( -dist, zbr_interp, xbrw_interp, pbr_interp ) )
              end if
            end if

            deallocate( bar_tint, stat=alloc_stat )
            if ( alloc_stat .gt. 0 ) then
              write(*,*) 'Unable to deallocate memory for Barrier interpolation'
              call exit(1)
            end if

          end do

          ! assign minimum value to grid cell
          cellstate(i,j)%w0br = w0br_min

        end do
      end do

  end subroutine sbbr

  function fu (xh, zbr, xbrw, pbr) result(fufv)
      real, intent(in) :: xh     ! distance from barrier (range: -5*zbr to 50*zbr)
      real, intent(in) :: zbr    ! barrier height
      real, intent(in) :: xbrw   ! barrier width
      real, intent(in) :: pbr    ! barrier optical porosity (through entire width) (range: 0 to 0.9)

      real :: fufv  ! fraction of upwind fric. velocity near the  barrier

      ! local variables
      real a, b, c, d    ! intermediate result values
      real :: x, xw, pb  ! scaled values for distance, width and porosity

      ! scale distance & width by barrier height
      x = xh/zbr
      xw = xbrw/zbr

      ! increase effective porosity with barrier width
      pb = pbr + (1 - exp(-0.5*xw))*0.3*(1-pbr)

      ! calculate coef. as fn of porosity
      a = 0.008-0.17*pb+0.17*pb**1.05
      b = 1.35*exp(-0.5*pb**0.2)
      c = 10*(1-0.5*pb)
      d = 3 - pb

      ! calc. frac. of fric. vel.
      fufv = 1 - exp(-a*x**2) + b*exp(-0.003*(x+c)**d)

      ! Cap fu at 1.0
      if (fufv > 1.0) then
          fufv = 1.0
      endif

  end function fu

  function minht_barriers() result( minht )

    real :: minht  ! minimum barrier height

    integer :: i, j, k   ! do loop indexes

    if( size(barseas) .gt. 0 ) then
       ! when barriers exist, find shortest barrier height
       minht = 9999.9
       do i = 1, size(barseas)
          do j = 1, barseas(i)%np
             do k = 1, barseas(i)%ntm
                minht = min(minht, barseas(i)%param(j,k)%amzbr)
             end do
          end do
       end do
    else
       ! default no barriers, so set height to zero
       minht = 0.0
    end if

  end function minht_barriers

end module barriers_mod
