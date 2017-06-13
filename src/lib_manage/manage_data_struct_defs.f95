!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_data_struct_defs

  type operation_date
    integer :: day
    integer :: month
    integer :: year
  end type operation_date

  type integer_param
    integer :: p_value
    logical :: p_acquired
  end type integer_param

  type real_param
    real :: p_value
    logical :: p_acquired
  end type real_param

  type string_param
    character(len=80) :: p_value
    logical :: p_acquired
  end type string_param

  type process
    character(len=3) :: procID
    integer :: procType
    type(process), pointer :: procNext
    type(integer_param), dimension(:), allocatable :: i_param
    type(real_param), dimension(:), allocatable :: r_param
    type(string_param), dimension(:), allocatable :: s_param
  end type process

  type group
    character(len=3) :: grpID
    integer :: grpType
    type(group), pointer :: grpNext
    type(process), pointer :: procFirst
    type(integer_param), dimension(:), allocatable :: i_param
    type(real_param), dimension(:), allocatable :: r_param
    type(string_param), dimension(:), allocatable :: s_param
  end type group

  type operation
    type(operation_date) :: operDate
    character(len=80) :: operName
    character(len=3) :: operID
    integer :: operType
    type(operation), pointer :: operNext
    type(group), pointer :: grpFirst
    type(integer_param), dimension(:), allocatable :: i_param
    type(real_param), dimension(:), allocatable :: r_param
    type(string_param), dimension(:), allocatable :: s_param
  end type operation

  type man_file_struct
    integer :: isub      ! subregion index
    character(len=512) :: tinfil  ! management file name
    real :: mversion     ! management version number
    integer :: mperod    ! length of management of rotation
    integer :: am0tfl    ! flag to print MANAGEment (TILLAGE) output
                         ! 0 = no output
                         ! 1 = detailed output file created
                         ! 2 = ASD output file(s) created
    integer :: am0tdb    ! flag to print MANAGEment variables before and after the call to MANAGE
                         ! 0 = no output
                         ! 1 = output
    integer :: asdhflag  ! flag to control printing ASD header info
                         ! 0 = ASD header line not yet printed
                         ! 1 = ASD header (first) line now printed
    integer :: wchflag   ! flag to control printing WC header info
                         ! 0 = WC header line not yet printed
                         ! 1 = WC header (first) line now printed
    type(operation), pointer :: operFirst, oper
    type(group), pointer :: grp
    type(process), pointer :: proc
  end type man_file_struct

  type(man_file_struct), dimension(:), allocatable :: manFile

  type last_operation
    integer  ::    day       ! The day of the last operation.
    integer  ::    mon       ! The month, and year of the last operation.
    integer  ::    yr        ! The year of the last operation.
    integer  ::    code       ! code indicating operation type
                              ! 0 - indicates an operation that will be run only mcount times
                              !     (normally used for initialzation)
                              ! 1 - triggers a read of tillage related operation parameters
                              !     (speed and direction)
    integer  ::    skip       ! used to skip all groups and processes in an operation that
                              ! has already completed mcount invocations
                              ! 0 - do not skip
                              ! 1 - skip
    character*80 :: name       ! name of current operation read from management file
    character*80 :: fuel       ! name of fuel used for operation
    real     ::    energyarea  ! diesel fuel equivalent energy required for operation Liters per hectare
    real     ::    stir        ! Operation Stir value (assigned from RUSLE2)

    character*80 grname       ! name of group read from management file
    integer  ::    grcode       ! group code designating which parameters will follow name
                                ! 1 - soil distrubance parameters
                                ! 2 - biomass manipulation
                                ! 3 - crop growth
                                ! 4 - ammendments
    real     ::    cutht        ! read from process as fraction or distance (flag controlled).
                                ! Converted to distance from ground up in meters by cut.for
  end type last_operation

  type(last_operation), dimension(:), allocatable :: lastoper 

  interface elemCreate
    module procedure operCreate
    module procedure grpCreate
    module procedure procCreate
  end interface elemCreate

contains

  subroutine manFileAlloc( nsubr )
    integer, intent(in) :: nsubr

    integer :: alloc_stat
    integer idx

    allocate( manFile(nsubr), stat=alloc_stat )
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'Unable to allocate memory for manFile structure.'
    end if

    ! initialize values
    do idx = 1, nsubr
      manFile(idx)%isub = idx
      manFile(idx)%mperod = 0
      manFile(idx)%am0tfl = 0
      manFile(idx)%am0tdb = 0
      manFile(idx)%asdhflag = 0
      manFile(idx)%wchflag = 0
      nullify(manFile(idx)%operFirst)
      nullify(manFile(idx)%oper)
      nullify(manFile(idx)%grp)
      nullify(manFile(idx)%proc)
    end do
  end subroutine manFileAlloc

  function operCreate(operPntr, operID, int_cnt, real_cnt, str_cnt) result(operNew)
    type(operation), pointer :: operPntr
    character(len=*), intent(in) :: operID
    integer, intent(in) :: int_cnt
    integer, intent(in) :: real_cnt
    integer, intent(in) :: str_cnt
    type(operation), pointer :: operNew

    integer :: alloc_stat
    integer :: sum_stat
    integer :: idx

    allocate(operPntr, stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Operation pointer: P ', operID
    end if
    operPntr%operID = operID
    read(operID, *) operPntr%operType
    sum_stat = 0
    allocate(operPntr%i_param(int_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(operPntr%r_param(real_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(operPntr%s_param(str_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    if( sum_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Operation params: P ', operID
    end if

    ! initialize pointers to NULL
    nullify(operPntr%operNext)
    nullify(operPntr%grpFirst)

    ! initialize acquisition flags to .false.
    do idx = 1, size(operPntr%i_param)
      operPntr%i_param(idx)%p_acquired = .false.
    end do
    do idx = 1, size(operPntr%r_param)
      operPntr%r_param(idx)%p_acquired = .false.
    end do
    do idx = 1, size(operPntr%s_param)
      operPntr%s_param(idx)%p_acquired = .false.
    end do

    operNew =>operPntr
        
  end function operCreate

  function grpCreate(grpPntr, grpID, int_cnt, real_cnt, str_cnt) result(grpNew)
    type(group), pointer :: grpPntr
    character(len=*), intent(in) :: grpID
    integer, intent(in) :: int_cnt
    integer, intent(in) :: real_cnt
    integer, intent(in) :: str_cnt
    type(group), pointer :: grpNew

    integer :: alloc_stat
    integer :: sum_stat
    integer :: idx

    allocate(grpPntr, stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Group pointer: G ', grpID
    end if
    grpPntr%grpID = grpID
    read(grpID, *) grpPntr%grpType
    sum_stat = 0
    allocate(grpPntr%i_param(int_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(grpPntr%r_param(real_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(grpPntr%s_param(str_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    if( sum_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Group params: G ', grpID
    end if

    ! initialize pointers to NULL
    nullify(grpPntr%grpNext)
    nullify(grpPntr%procFirst)

    ! initialize acquisition flags to .false.
    do idx = 1, size(grpPntr%i_param)
      grpPntr%i_param(idx)%p_acquired = .false.
    end do
    do idx = 1, size(grpPntr%r_param)
      grpPntr%r_param(idx)%p_acquired = .false.
    end do
    do idx = 1, size(grpPntr%s_param)
      grpPntr%s_param(idx)%p_acquired = .false.
    end do

    grpNew =>grpPntr
        
  end function grpCreate

  function procCreate(procPntr, procID, int_cnt, real_cnt, str_cnt) result(procNew)
    type(process), pointer :: procPntr
    character(len=*), intent(in) :: procID
    integer, intent(in) :: int_cnt
    integer, intent(in) :: real_cnt
    integer, intent(in) :: str_cnt
    type(process), pointer :: procNew

    integer :: alloc_stat
    integer :: sum_stat
    integer :: idx

    allocate(procPntr, stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Process pointer: P ', procID
    end if
    procPntr%procID = procID
    read(procID, *) procPntr%procType
    sum_stat = 0
    allocate(procPntr%i_param(int_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(procPntr%r_param(real_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(procPntr%s_param(str_cnt), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    if( sum_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Process params: P ', procID
    end if

    ! initialize pointer to NULL
    nullify(procPntr%procNext)

    ! initialize acquisition flags to .false.
    do idx = 1, size(procPntr%i_param)
      procPntr%i_param(idx)%p_acquired = .false.
    end do
    do idx = 1, size(procPntr%r_param)
      procPntr%r_param(idx)%p_acquired = .false.
    end do
    do idx = 1, size(procPntr%s_param)
      procPntr%s_param(idx)%p_acquired = .false.
    end do

    procNew =>procPntr
        
  end function procCreate

end module manage_data_struct_defs

