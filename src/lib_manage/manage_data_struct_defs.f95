!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_data_struct_defs

  integer, parameter :: MAX_NAME_LEN = 40
  integer, parameter :: MAX_TYPE_LEN = 10

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
    integer :: OGPidx
    character(len=80) :: procName
    type(process), pointer :: procNext
    type(integer_param), dimension(:), allocatable :: i_param
    type(real_param), dimension(:), allocatable :: r_param
    type(string_param), dimension(:), allocatable :: s_param
  end type process

  type group
    character(len=3) :: grpID
    integer :: grpType
    integer :: OGPidx
    character(len=80) :: grpName
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
    integer :: OGPidx
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

  type :: name_type
    character(len=1) :: ogp  ! identifies whether this is and operation, group or process parameter list
    character(len=3) :: id   ! the id "number" of the operation, group or process
    character(len=MAX_NAME_LEN), dimension(:), allocatable  :: i_name   ! integer parameter names
    character(len=MAX_NAME_LEN), dimension(:), allocatable  :: r_name   ! real parameter names
    character(len=MAX_NAME_LEN), dimension(:), allocatable  :: s_name   ! string parameter names
  end type name_type

  integer :: max_ogp   ! the total number of operations, groups, and processes
  type(name_type), dimension(:), allocatable :: param_nt ! array of the list of operations, groups, processes

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

end module manage_data_struct_defs

