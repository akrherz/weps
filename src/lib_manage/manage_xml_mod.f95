!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_xml_mod

  use flib_sax
  use manage_data_struct_defs, only: manFile, operation_date, elemCreate

  integer, parameter :: MAX_NAME_LEN = 40
  integer, parameter :: MAX_TYPE_LEN = 10

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def

  type(tag_def), dimension(:), allocatable :: man_tag
  integer :: max_tags

  type :: name_type
    character(len=1) :: ogp  ! identifies whether this is and operation, group or process parameter list
    character(len=3) :: id   ! the id "number" of the operation, group or process
    character(len=MAX_NAME_LEN), dimension(:), allocatable  :: i_name   ! integer parameter names
    character(len=MAX_NAME_LEN), dimension(:), allocatable  :: r_name   ! real parameter names
    character(len=MAX_NAME_LEN), dimension(:), allocatable  :: s_name   ! string parameter names
  end type name_type

  integer :: max_ogp   ! the total number of operations, groups, and processes
  type(name_type), dimension(:), allocatable :: param_nt ! array of the list of operations, groups, processes

  integer, parameter, public :: rotationyears = 1
  integer, parameter, public :: wepsmanvalue = 2
  integer, parameter, public :: date = 3
  integer, parameter, public :: operationDB = 4
  integer, parameter, public :: operationname = 5
  integer, parameter, public :: actionvalue = 6
  integer, parameter, public :: identity = 7
  integer, parameter, public :: code = 8
  integer, parameter, public :: id = 9
  integer, parameter, public :: param = 10
  integer, parameter, public :: p_name = 11
  integer, parameter, public :: value = 12
  integer, parameter, public :: version = 13
  integer, parameter, public :: wepsmanDB = 14

  integer :: int_cnt     ! count of integer values to be read into an operation, group or process, for allocation
  integer :: real_cnt    ! count of real values to be read into an operation, group or process, for allocation
  integer :: str_cnt     ! count of string values to be read into an operation, group or process, for allocation

  integer :: i_cnt       ! count of integer values acutally read
  integer :: r_cnt       ! count of real values acutally read
  integer :: s_cnt       ! count of string values acutally read

  integer :: isub ! current subregion number used in a routines in this module
  type(operation_date) :: t_operDate
  character(len=80) :: t_operName
  character(len=3) :: t_code
  integer :: t_id
  integer :: ogp_id_idx
  character(len=4) :: p_type
  integer :: p_idx

  character(len=3) :: operID
  character(len=3) :: grpID
  character(len=3) :: procID

  logical :: all_wepsmanvalues
  logical :: all_operationDBs
  logical :: all_actionvalues
  logical :: all_params
  logical :: manfile_complete ! indicator that a complete manfile was read

  interface check_params
    module procedure oper_check_params
    module procedure grp_check_params
    module procedure proc_check_params
  end interface check_params

contains

  subroutine init_man_xml( isubr )
    integer, intent(in) :: isubr

    integer :: idx
    integer :: sum_stat
    integer :: alloc_stat

    ! set subregion index used with manFile
    isub = isubr

    max_tags = 14   ! count of unique tags needed from management files
    allocate( man_tag(max_tags), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., input_tag'
    end if

    ! assign defaults to flag status values
    do idx = 1, max_tags
      man_tag(idx)%acquired = .false.
      man_tag(idx)%in_tag = .false.
    end do

   ! assign tag names
    man_tag(1)%name = "rotationyears"
    man_tag(2)%name = "wepsmanvalue"
    man_tag(3)%name = "date"
    man_tag(4)%name = "operationDB"
    man_tag(5)%name = "operationname"
    man_tag(6)%name = "actionvalue"
    man_tag(7)%name = "identity"
    man_tag(8)%name = "code"
    man_tag(9)%name = "id"
    man_tag(10)%name = "param"
    man_tag(11)%name = "name"
    man_tag(12)%name = "value"
    man_tag(13)%name = "version"
    man_tag(14)%name = "wepsmanDB"

    max_ogp = 42   ! count of total number of operations, groups, and processes
    sum_stat = 0
    allocate( param_nt(max_ogp), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    param_nt(1)%ogp="O"
    param_nt(1)%id="00"
    allocate( param_nt(1)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(1)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(1)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(2)%ogp="O"
    param_nt(2)%id="01"
    allocate( param_nt(2)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(2)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(2)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(2)%r_name(1)="ospeed"
    param_nt(2)%r_name(2)="odirect"
    param_nt(2)%r_name(3)="ostdspeed"
    param_nt(2)%r_name(4)="ominspeed"
    param_nt(2)%r_name(5)="omaxspeed"
    param_nt(3)%ogp="O"
    param_nt(3)%id="02"
    allocate( param_nt(3)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(3)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(3)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(4)%ogp="O"
    param_nt(4)%id="03"
    allocate( param_nt(4)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(4)%r_name(7), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(4)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(4)%r_name(1)="oenergyarea"
    param_nt(4)%r_name(2)="ostir"
    param_nt(4)%r_name(3)="ospeed"
    param_nt(4)%r_name(4)="odirect"
    param_nt(4)%r_name(5)="ostdspeed"
    param_nt(4)%r_name(6)="ominspeed"
    param_nt(4)%r_name(7)="omaxspeed"
    param_nt(4)%s_name(1)="ofuel"
    param_nt(5)%ogp="O"
    param_nt(5)%id="04"
    allocate( param_nt(5)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(5)%r_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(5)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(5)%r_name(1)="oenergyarea"
    param_nt(5)%r_name(2)="ostir"
    param_nt(5)%s_name(1)="ofuel"
    param_nt(6)%ogp="G"
    param_nt(6)%id="01"
    allocate( param_nt(6)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(6)%r_name(6), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(6)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(6)%r_name(1)="gtdepth"
    param_nt(6)%r_name(2)="gtilint"
    param_nt(6)%r_name(3)="gtilArea"
    param_nt(6)%r_name(4)="gtstddepth"
    param_nt(6)%r_name(5)="gtmindepth"
    param_nt(6)%r_name(6)="gtmaxdepth"
    param_nt(7)%ogp="G"
    param_nt(7)%id="02"
    allocate( param_nt(7)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(7)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(7)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(7)%r_name(1)="gbioarea"
    param_nt(8)%ogp="G"
    param_nt(8)%id="03"
    allocate( param_nt(8)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(8)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(8)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(8)%s_name(1)="gcropname"
    param_nt(9)%ogp="G"
    param_nt(9)%id="04"
    allocate( param_nt(9)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(9)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(9)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(9)%s_name(1)="gamdname"
    param_nt(10)%ogp="P"
    param_nt(10)%id="01"
    allocate( param_nt(10)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(10)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(10)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(11)%ogp="P"
    param_nt(11)%id="02"
    allocate( param_nt(11)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(11)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(11)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(11)%i_name(1)="rroughflag"
    param_nt(11)%r_name(1)="rrough"
    param_nt(12)%ogp="P"
    param_nt(12)%id="05"
    allocate( param_nt(12)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(12)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(12)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(12)%i_name(1)="rdgflag"
    param_nt(12)%r_name(1)="rdghit"
    param_nt(12)%r_name(2)="rdgspac"
    param_nt(12)%r_name(3)="rdgwidth"
    param_nt(12)%r_name(4)="dkhit"
    param_nt(12)%r_name(5)="dkspac"
    param_nt(13)%ogp="P"
    param_nt(13)%id="11"
    allocate( param_nt(13)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(13)%r_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(13)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(13)%r_name(1)="asdf"
    param_nt(13)%r_name(2)="crif"
    param_nt(14)%ogp="P"
    param_nt(14)%id="12"
    allocate( param_nt(14)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(14)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(14)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(14)%r_name(1)="soilos"
    param_nt(15)%ogp="P"
    param_nt(15)%id="13"
    allocate( param_nt(15)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(15)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(15)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(15)%r_name(1)="laymix"
    param_nt(16)%ogp="P"
    param_nt(16)%id="14"
    allocate( param_nt(16)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(16)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(16)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(17)%ogp="P"
    param_nt(17)%id="24"
    allocate( param_nt(17)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(17)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(17)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(17)%i_name(1)="fbioflagvt"
    param_nt(17)%r_name(1)="massflatvt1"
    param_nt(17)%r_name(2)="massflatvt2"
    param_nt(17)%r_name(3)="massflatvt3"
    param_nt(17)%r_name(4)="massflatvt4"
    param_nt(17)%r_name(5)="massflatvt5"
    param_nt(18)%ogp="P"
    param_nt(18)%id="25"
    allocate( param_nt(18)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(18)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(18)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(18)%i_name(1)="burydist"
    param_nt(18)%r_name(1)="massburyvt1"
    param_nt(18)%r_name(2)="massburyvt2"
    param_nt(18)%r_name(3)="massburyvt3"
    param_nt(18)%r_name(4)="massburyvt4"
    param_nt(18)%r_name(5)="massburyvt5"
    param_nt(19)%ogp="P"
    param_nt(19)%id="26"
    allocate( param_nt(19)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(19)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(19)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(19)%r_name(1)="massresurvt1"
    param_nt(19)%r_name(2)="massresurvt2"
    param_nt(19)%r_name(3)="massresurvt3"
    param_nt(19)%r_name(4)="massresurvt4"
    param_nt(19)%r_name(5)="massresurvt5"
    param_nt(20)%ogp="P"
    param_nt(20)%id="30"
    allocate( param_nt(20)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(20)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(20)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(20)%i_name(1)="defoliateflag"
    param_nt(21)%ogp="P"
    param_nt(21)%id="31"
    allocate( param_nt(21)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(21)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(21)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(21)%i_name(1)="kilflag"
    param_nt(22)%ogp="P"
    param_nt(22)%id="32"
    allocate( param_nt(22)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(22)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(22)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(22)%i_name(1)="cutflag"
    param_nt(22)%r_name(1)="cutvalh"
    param_nt(22)%r_name(2)="cyldrmh"
    param_nt(22)%r_name(3)="cplrmh"
    param_nt(22)%r_name(4)="cstrmh"
    param_nt(23)%ogp="P"
    param_nt(23)%id="33"
    allocate( param_nt(23)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(23)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(23)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(23)%r_name(1)="cutvalf"
    param_nt(23)%r_name(2)="cyldrmf"
    param_nt(23)%r_name(3)="cplrmf"
    param_nt(23)%r_name(4)="cstrmf"
    param_nt(24)%ogp="P"
    param_nt(24)%id="34"
    allocate( param_nt(24)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(24)%r_name(10), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(24)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(24)%i_name(1)="frselpool"
    param_nt(24)%r_name(1)="ratemultvt1"
    param_nt(24)%r_name(2)="ratemultvt2"
    param_nt(24)%r_name(3)="ratemultvt3"
    param_nt(24)%r_name(4)="ratemultvt4"
    param_nt(24)%r_name(5)="ratemultvt5"
    param_nt(24)%r_name(6)="threshmultvt1"
    param_nt(24)%r_name(7)="threshmultvt2"
    param_nt(24)%r_name(8)="threshmultvt3"
    param_nt(24)%r_name(9)="threshmultvt4"
    param_nt(24)%r_name(10)="threshmultvt5"
    param_nt(25)%ogp="P"
    param_nt(25)%id="37"
    allocate( param_nt(25)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(25)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(25)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(25)%r_name(1)="thinvalp"
    param_nt(25)%r_name(2)="tyldrmp"
    param_nt(25)%r_name(3)="tplrmp"
    param_nt(25)%r_name(4)="tstrmp"
    param_nt(26)%ogp="P"
    param_nt(26)%id="38"
    allocate( param_nt(26)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(26)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(26)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(26)%r_name(1)="thinvalf"
    param_nt(26)%r_name(2)="tyldrmf"
    param_nt(26)%r_name(3)="tplrmf"
    param_nt(26)%r_name(4)="tstrmf"
    param_nt(27)%ogp="P"
    param_nt(27)%id="40"
    allocate( param_nt(27)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(27)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(27)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(28)%ogp="P"
    param_nt(28)%id="42"
    allocate( param_nt(28)%i_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(28)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(28)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(28)%i_name(1)="harv_report_flg"
    param_nt(28)%i_name(2)="harv_calib_flg"
    param_nt(28)%i_name(3)="harv_unit_flg"
    param_nt(28)%i_name(4)="mature_warn_flg"
    param_nt(28)%i_name(5)="cutflag"
    param_nt(28)%r_name(1)="cutvalh"
    param_nt(28)%r_name(2)="cyldrmh"
    param_nt(28)%r_name(3)="cplrmh"
    param_nt(28)%r_name(4)="cstrmh"
    param_nt(29)%ogp="P"
    param_nt(29)%id="43"
    allocate( param_nt(29)%i_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(29)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(29)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(29)%i_name(1)="harv_report_flg"
    param_nt(29)%i_name(2)="harv_calib_flg"
    param_nt(29)%i_name(3)="harv_unit_flg"
    param_nt(29)%i_name(4)="mature_warn_flg"
    param_nt(29)%r_name(1)="cutvalf"
    param_nt(29)%r_name(2)="cyldrmf"
    param_nt(29)%r_name(3)="cplrmf"
    param_nt(29)%r_name(4)="cstrmf"
    param_nt(30)%ogp="P"
    param_nt(30)%id="47"
    allocate( param_nt(30)%i_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(30)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(30)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(30)%i_name(1)="harv_report_flg"
    param_nt(30)%i_name(2)="harv_calib_flg"
    param_nt(30)%i_name(3)="harv_unit_flg"
    param_nt(30)%i_name(4)="mature_warn_flg"
    param_nt(30)%r_name(1)="thinvalp"
    param_nt(30)%r_name(2)="tyldrmp"
    param_nt(30)%r_name(3)="tplrmp"
    param_nt(30)%r_name(4)="tstrmp"
    param_nt(31)%ogp="P"
    param_nt(31)%id="48"
    allocate( param_nt(31)%i_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(31)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(31)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(31)%i_name(1)="harv_report_flg"
    param_nt(31)%i_name(2)="harv_calib_flg"
    param_nt(31)%i_name(3)="harv_unit_flg"
    param_nt(31)%i_name(4)="mature_warn_flg"
    param_nt(31)%r_name(1)="thinvalf"
    param_nt(31)%r_name(2)="tyldrmf"
    param_nt(31)%r_name(3)="tplrmf"
    param_nt(31)%r_name(4)="tstrmf"
    param_nt(32)%ogp="P"
    param_nt(32)%id="50"
    allocate( param_nt(32)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(32)%r_name(18), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(32)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(32)%r_name(1)="numst"
    param_nt(32)%r_name(2)="rstandht"
    param_nt(32)%r_name(3)="rstandmass"
    param_nt(32)%r_name(4)="rflatmass"
    param_nt(32)%i_name(1)="rbc"
    param_nt(32)%r_name(5)="rburiedmass"
    param_nt(32)%r_name(6)="rburieddepth"
    param_nt(32)%r_name(7)="rrootmass"
    param_nt(32)%r_name(8)="rrootdepth"
    param_nt(32)%r_name(9)="standdk"
    param_nt(32)%r_name(10)="surfdk"
    param_nt(32)%r_name(11)="burieddk"
    param_nt(32)%r_name(12)="rootdk"
    param_nt(32)%r_name(13)="stemnodk"
    param_nt(32)%r_name(14)="stemdia"
    param_nt(32)%r_name(15)="thrddys"
    param_nt(32)%r_name(16)="covfact"
    param_nt(32)%r_name(17)="resevapa"
    param_nt(32)%r_name(18)="resevapb"
    param_nt(33)%ogp="P"
    param_nt(33)%id="51"
    allocate( param_nt(33)%i_name(9), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(33)%r_name(61), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(33)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(33)%i_name(1)="rowflag"
    param_nt(33)%r_name(1)="rowspac"
    param_nt(33)%i_name(2)="rowridge"
    param_nt(33)%r_name(2)="plantpop"
    param_nt(33)%r_name(3)="dmaxshoot"
    param_nt(33)%i_name(3)="cbaflag"
    param_nt(33)%r_name(4)="tgtyield"
    param_nt(33)%r_name(5)="cbafact"
    param_nt(33)%r_name(6)="cyrafact"
    param_nt(33)%i_name(4)="hyldflag"
    param_nt(33)%s_name(1)="hyldunits"
    param_nt(33)%r_name(7)="hyldwater"
    param_nt(33)%r_name(8)="hyconfact"
    param_nt(33)%i_name(5)="idc"
    param_nt(33)%r_name(9)="grf"
    param_nt(33)%r_name(10)="ck"
    param_nt(33)%r_name(11)="hui0"
    param_nt(33)%r_name(12)="hmx"
    param_nt(33)%r_name(13)="growdepth"
    param_nt(33)%r_name(14)="rdmx"
    param_nt(33)%r_name(15)="tbas"
    param_nt(33)%r_name(16)="topt"
    param_nt(33)%i_name(6)="thudf"
    param_nt(33)%i_name(7)="dtm"
    param_nt(33)%r_name(17)="thum"
    param_nt(33)%r_name(18)="frsx1"
    param_nt(33)%r_name(19)="frsx2"
    param_nt(33)%r_name(20)="frsy1"
    param_nt(33)%r_name(21)="frsy2"
    param_nt(33)%r_name(22)="verndel"
    param_nt(33)%r_name(23)="bceff"
    param_nt(33)%r_name(24)="a_lf"
    param_nt(33)%r_name(25)="b_lf"
    param_nt(33)%r_name(26)="c_lf"
    param_nt(33)%r_name(27)="d_lf"
    param_nt(33)%r_name(28)="a_rp"
    param_nt(33)%r_name(29)="b_rp"
    param_nt(33)%r_name(30)="c_rp"
    param_nt(33)%r_name(31)="d_rp"
    param_nt(33)%r_name(32)="a_ht"
    param_nt(33)%r_name(33)="b_ht"
    param_nt(33)%r_name(34)="ssaa"
    param_nt(33)%r_name(35)="ssab"
    param_nt(33)%r_name(36)="sla"
    param_nt(33)%r_name(37)="huie"
    param_nt(33)%i_name(8)="transf"
    param_nt(33)%r_name(38)="diammax"
    param_nt(33)%r_name(39)="storeinit"
    param_nt(33)%r_name(40)="mshoot"
    param_nt(33)%r_name(41)="leafstem"
    param_nt(33)%r_name(42)="fshoot"
    param_nt(33)%r_name(43)="leaf2stor"
    param_nt(33)%r_name(44)="stem2stor"
    param_nt(33)%r_name(45)="stor2stor"
    param_nt(33)%i_name(9)="rbc"
    param_nt(33)%r_name(46)="standdk"
    param_nt(33)%r_name(47)="surfdk"
    param_nt(33)%r_name(48)="burieddk"
    param_nt(33)%r_name(49)="rootdk"
    param_nt(33)%r_name(50)="stemnodk"
    param_nt(33)%r_name(51)="stemdia"
    param_nt(33)%r_name(52)="thrddys"
    param_nt(33)%r_name(53)="covfact"
    param_nt(33)%r_name(54)="resevapa"
    param_nt(33)%r_name(55)="resevapb"
    param_nt(33)%r_name(56)="yield_coefficient"
    param_nt(33)%r_name(57)="residue_intercept"
    param_nt(33)%r_name(58)="regrow_location"
    param_nt(33)%r_name(59)="noparam3"
    param_nt(33)%r_name(60)="noparam2"
    param_nt(33)%r_name(61)="noparam1"
    param_nt(34)%ogp="P"
    param_nt(34)%id="61"
    allocate( param_nt(34)%i_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(34)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(34)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(34)%i_name(1)="selpos"
    param_nt(34)%i_name(2)="selpool"
    param_nt(34)%r_name(1)="rstore"
    param_nt(34)%r_name(2)="rleaf"
    param_nt(34)%r_name(3)="rstem"
    param_nt(34)%r_name(4)="rrootstore"
    param_nt(34)%r_name(5)="rrootfiber"
    param_nt(35)%ogp="P"
    param_nt(35)%id="62"
    allocate( param_nt(35)%i_name(7), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(35)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(35)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(35)%i_name(1)="harv_report_flg"
    param_nt(35)%i_name(2)="harv_calib_flg"
    param_nt(35)%i_name(3)="harv_unit_flg"
    param_nt(35)%i_name(4)="mature_warn_flg"
    param_nt(35)%i_name(5)="selpos"
    param_nt(35)%i_name(6)="selpool"
    param_nt(35)%i_name(7)="selagepool"
    param_nt(35)%r_name(1)="rstore"
    param_nt(35)%r_name(2)="rleaf"
    param_nt(35)%r_name(3)="rstem"
    param_nt(35)%r_name(4)="rrootstore"
    param_nt(35)%r_name(5)="rrootfiber"
    param_nt(36)%ogp="P"
    param_nt(36)%id="65"
    allocate( param_nt(36)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(36)%r_name(18), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(36)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(36)%r_name(1)="numst"
    param_nt(36)%r_name(2)="rstandht"
    param_nt(36)%r_name(3)="rstandmass"
    param_nt(36)%r_name(4)="rflatmass"
    param_nt(36)%i_name(1)="rbc"
    param_nt(36)%r_name(5)="rburiedmass"
    param_nt(36)%r_name(6)="rburieddepth"
    param_nt(36)%r_name(7)="rrootmass"
    param_nt(36)%r_name(8)="rrootdepth"
    param_nt(36)%r_name(9)="standdk"
    param_nt(36)%r_name(10)="surfdk"
    param_nt(36)%r_name(11)="burieddk"
    param_nt(36)%r_name(12)="rootdk"
    param_nt(36)%r_name(13)="stemnodk"
    param_nt(36)%r_name(14)="stemdia"
    param_nt(36)%r_name(15)="thrddys"
    param_nt(36)%r_name(16)="covfact"
    param_nt(36)%r_name(17)="resevapa"
    param_nt(36)%r_name(18)="resevapb"
    param_nt(37)%ogp="P"
    param_nt(37)%id="66"
    allocate( param_nt(37)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(37)%r_name(20), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(37)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(37)%r_name(1)="M_numst"
    param_nt(37)%r_name(2)="M_rstandht"
    param_nt(37)%r_name(3)="M_rstandmass"
    param_nt(37)%r_name(4)="M_rflatmass"
    param_nt(37)%i_name(1)="rbc"
    param_nt(37)%r_name(5)="M_rburiedmass"
    param_nt(37)%r_name(6)="M_rburieddepth"
    param_nt(37)%r_name(7)="M_rrootmass"
    param_nt(37)%r_name(8)="M_rrootdepth"
    param_nt(37)%r_name(9)="manure_total_mass"
    param_nt(37)%r_name(10)="manure_buried_ratio"
    param_nt(37)%r_name(11)="standdk"
    param_nt(37)%r_name(12)="surfdk"
    param_nt(37)%r_name(13)="burieddk"
    param_nt(37)%r_name(14)="rootdk"
    param_nt(37)%r_name(15)="stemnodk"
    param_nt(37)%r_name(16)="stemdia"
    param_nt(37)%r_name(17)="thrddys"
    param_nt(37)%r_name(18)="covfact"
    param_nt(37)%r_name(19)="resevapa"
    param_nt(37)%r_name(20)="resevapb"
    param_nt(38)%ogp="P"
    param_nt(38)%id="71"
    allocate( param_nt(38)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(38)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(38)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(38)%i_name(1)="irrtype"
    param_nt(38)%r_name(1)="irrdepth"
    param_nt(39)%ogp="P"
    param_nt(39)%id="72"
    allocate( param_nt(39)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(39)%r_name(7), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(39)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(39)%i_name(1)="irrmonflag"
    param_nt(39)%r_name(1)="irrmaxapp"
    param_nt(39)%r_name(2)="irrrate"
    param_nt(39)%r_name(3)="irrduration"
    param_nt(39)%r_name(4)="irrapploc"
    param_nt(39)%r_name(5)="irrminapp"
    param_nt(39)%r_name(6)="irrmad"
    param_nt(39)%r_name(7)="irrminint"
    param_nt(40)%ogp="P"
    param_nt(40)%id="73"
    allocate( param_nt(40)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(40)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(40)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(40)%r_name(1)="irrdepth"
    param_nt(40)%r_name(2)="irrrate"
    param_nt(40)%r_name(3)="irrduration"
    param_nt(40)%r_name(4)="irrapploc"
    param_nt(41)%ogp="P"
    param_nt(41)%id="74"
    allocate( param_nt(41)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(41)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(41)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(42)%ogp="P"
    param_nt(42)%id="91"
    allocate( param_nt(42)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(42)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(42)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(42)%r_name(1)="asddepth"
    param_nt(42)%r_name(2)="gmdx"
    param_nt(42)%r_name(3)="gsdx"
    param_nt(42)%r_name(4)="mnot"
    param_nt(42)%r_name(5)="minf"
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., parameter names reference'
    end if

    all_wepsmanvalues = .true.  ! .true. indicates that no values are required
    all_operationDBs = .true.  ! .false. indicates that a value is required
    all_actionvalues = .true.
    all_params = .true.

  end subroutine init_man_xml

  subroutine get_value_cnts ( OGP, ID, int_cnt, real_cnt, str_cnt, code_id_index )
    character(len=*), intent(in) :: OGP
    character(len=*), intent(in) :: ID
    integer, intent(out) :: int_cnt
    integer, intent(out) :: real_cnt
    integer, intent(out) :: str_cnt
    integer, intent(out) :: code_id_index

    integer :: idx

    do idx = 1, max_ogp
      if (    OGP .eq. param_nt(idx)%ogp &
        .and. ID .eq. param_nt(idx)%id ) then
        int_cnt = size(param_nt(idx)%i_name)
        real_cnt = size(param_nt(idx)%r_name)
        str_cnt = size(param_nt(idx)%s_name)
        exit   ! found so exit and return index number
      end if
    end do
    code_id_index = idx
  end subroutine get_value_cnts

  subroutine get_value_type_index ( code_id_index, val_name, val_type, val_index )
    integer, intent(in) :: code_id_index
    character(len=*), intent(in) :: val_name
    character(len=4), intent(out) :: val_type
    integer , intent(out) :: val_index

    integer idx

    do idx = 1, size(param_nt(code_id_index)%i_name)
      if ( val_name .eq. param_nt(code_id_index)%i_name(idx) ) then
        val_type = 'int'
        exit  ! found the name
      end if
    end do
    if ( idx .gt. size(param_nt(code_id_index)%i_name) ) then
      val_index = 0
    else
      val_index = idx
      return  ! found the name
    end if

    do idx = 1, size(param_nt(code_id_index)%r_name)
      if ( val_name .eq. param_nt(code_id_index)%r_name(idx) ) then
        val_type = 'real'
        exit  ! found the name
      end if
    end do
    if ( idx .gt. size(param_nt(code_id_index)%r_name) ) then
      val_index = 0
    else
      val_index = idx
      return  ! found the name
    end if

    do idx = 1, size(param_nt(code_id_index)%s_name)
      if ( val_name .eq. param_nt(code_id_index)%s_name(idx) ) then
        val_type = 'str'
        exit  ! found the name
      end if
    end do
    if ( idx .gt. size(param_nt(code_id_index)%s_name) ) then
      val_index = 0
    else
      val_index = idx
      return  ! found the name
    end if

  end subroutine get_value_type_index

  function oper_check_params( operPtr ) result(acquired)
    use manage_data_struct_defs, only: operation
    type(operation), intent(in) :: operPtr
    logical :: acquired

    integer :: idx
    character(len=10) :: date_str

    ! set as default in case of no parameter action
    acquired = .true.

    do idx = 1, size(operPtr%i_param)
      if ( operPtr%i_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%i_name(idx)), ' Parameter in: O ', trim(operPtr%operID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(operPtr%r_param)
      if ( operPtr%r_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%r_name(idx)), ' Parameter in: O ', trim(operPtr%operID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(operPtr%s_param)
      if ( operPtr%s_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%s_name(idx)), ' Parameter in: O ', trim(operPtr%operID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
  end function oper_check_params

  function grp_check_params( operPtr, grpPtr ) result(acquired)
    use manage_data_struct_defs, only: operation, group
    type(operation), intent(in) :: operPtr
    type(group), intent(in) :: grpPtr
    logical :: acquired

    integer :: idx
    character(len=10) :: date_str

    ! set as default in case of no parameter action
    acquired = .true.

    do idx = 1, size(grpPtr%i_param)
      if ( grpPtr%i_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%i_name(idx)), ' Parameter in: G ', trim(grpPtr%grpID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(grpPtr%r_param)
      if ( grpPtr%r_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%r_name(idx)), ' Parameter in: G ', trim(grpPtr%grpID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(grpPtr%s_param)
      if ( grpPtr%s_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%s_name(idx)), ' Parameter in: G ', trim(grpPtr%grpID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
  end function grp_check_params

  function proc_check_params( operPtr, procPtr ) result(acquired)
    use manage_data_struct_defs, only: operation, process
    type(operation), intent(in) :: operPtr
    type(process), intent(in) :: procPtr
    logical :: acquired

    integer :: idx
    character(len=10) :: date_str

    ! set as default in case of no parameter action
    acquired = .true.

    do idx = 1, size(procPtr%i_param)
      if ( procPtr%i_param(idx)%p_acquired ) then
        acquired = .true.
      else
        acquired = .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%i_name(idx)), ' Parameter in: P ', trim(procPtr%procID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(procPtr%r_param)
      if ( procPtr%r_param(idx)%p_acquired ) then
        acquired = .true.
      else
        acquired = .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%r_name(idx)), ' Parameter in: P ', trim(procPtr%procID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(procPtr%s_param)
      if ( procPtr%s_param(idx)%p_acquired ) then
        acquired = .true.
      else
        acquired = .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(ogp_id_idx)%s_name(idx)), ' Parameter in: P ', trim(procPtr%procID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
  end function proc_check_params

!  subroutine read_manage_xml()

!    operType = 0
!    operFirst => elemCreate( operFirst, operType, real_cnt )
!    oper => operFirst

!    grpType = 0
!    oper%grpFirst => elemCreate( oper%grpFirst, grpType, real_cnt )
!    grp => oper%grpFirst
!    procType = 0
!    grp%procFirst => elemCreate( grp%procFirst, procType, int_cnt, real_cnt )
!    proc => grp%procFirst
!    do procType = 1, 5
!      proc%procNext => elemCreate( proc%procNext, procType, int_cnt, real_cnt )
!      proc => proc%procNext
!    end do
!    nullify( proc%procNext )
!    do grpType = 1, 5
!      grp => elemCreate( grp%grpNext, grpType )
!      procType = 0
!      grp%procFirst => elemCreate( grp%procFirst, procType )
!      proc => grp%procFirst
!      do procType = 1, 5
!        proc%procNext => elemCreate( proc%procNext, procType )
!        proc => proc%procNext
!      end do
!      nullify( proc%procNext )
!    end do
!    nullify( grp%grpNext )

!    do operType = 1, 5
!      oper => elemCreate( oper%operNext, operType )
!      grpType = 0
!      oper%grpFirst => elemCreate( oper%grpFirst, grpType )
!      grp => oper%grpFirst
!      procType = 0
!      grp%procFirst => elemCreate( grp%procFirst, procType )
!      proc => grp%procFirst
!      do procType = 1, 5
!        proc%procNext => elemCreate( proc%procNext, procType )
!        proc => proc%procNext
!      end do
!      nullify( proc%procNext )
!      do grpType = 1, 5
!        grp => elemCreate( grp%grpNext, grpType )
!        procType = 0
!        grp%procFirst => elemCreate( grp%procFirst, procType )
!        proc => grp%procFirst
!        do procType = 1, 5
!          proc%procNext => elemCreate( proc%procNext, procType )
!          proc => proc%procNext
!        end do
!        nullify( proc%procNext )
!      end do
!      nullify( grp%grpNext )
!    end do

!    oper%operNext => operFirst

!    oper => operFirst
!    operType = 0
!    do while( operType .lt. 12 )
!      write(*,'(a,i0)') 'OPER: ', oper%operType
!      grp => oper%grpFirst
!      do while( associated(grp) )
!        write(*,'(a,i0)') '  GRP: ', grp%grpType
!        proc => grp%procFirst
!        do while( associated(proc) )
!          write(*,'(a,i0)') '    PROC: ', proc%procType
!          proc => proc%procNext
!        end do
!        grp => grp%grpNext
!      end do
!      oper => oper%operNext
!      operType = operType + 1
!    end do

!  end subroutine read_manage_xml

  subroutine begin_man_element_handler(name,attributes)
    character(len=*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx

    do idx = 1, size(man_tag)
      if( man_tag(idx)%name .eq. name ) then
        man_tag(idx)%in_tag = .true.
        !write(*,*) 'In tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

  end subroutine begin_man_element_handler

  subroutine end_man_element_handler(name)
    character(len=*), intent(in) :: name

    integer :: idx

    do idx = 1, size(man_tag)
      if( man_tag(idx)%name .eq. name ) then
        man_tag(idx)%in_tag = .false.
        !write(*,*) 'Out tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

    if (idx .eq. wepsmanDB) then

      if ( man_tag(rotationyears)%acquired &
        .and. man_tag(version)%acquired &
        .and. all_wepsmanvalues ) then
        manfile_complete = .true.
      else
        manfile_complete = .false.
      end if

    else if (idx .eq. wepsmanvalue) then
      if( man_tag(date)%acquired &
        .and. all_operationDBs ) then
        man_tag(date)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_wepsmanvalues = (all_wepsmanvalues .and. .true. )
      else
        all_wepsmanvalues = .false.
        if ( .not. man_tag(date)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(date)%name), ' is missing from Management file.'
        end if
      end if

    else if (idx .eq. date) then

    else if (idx .eq. operationDB) then
      if( man_tag(operationname)%acquired &
        .and. all_actionvalues ) then
        man_tag(operationname)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_operationDBs = (all_operationDBs .and. .true. )
      else
        all_operationDBs = .false.
        if ( .not. man_tag(operationname)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(operationname)%name), ' is missing from Management file.'
        end if
      end if

    else if (idx .eq. actionvalue) then
      ! check if all parameters acquired for this action value
      if ( operID .ne. '' ) then
        all_params = check_params( manFile(isub)%oper )
      else if ( grpID .ne. '' ) then
        all_params = check_params( manFile(isub)%oper, manFile(isub)%grp )
      else if ( procID .ne. '' ) then
        all_params = check_params( manFile(isub)%oper, manFile(isub)%proc )
      end if
      if( man_tag(identity)%acquired &
        .and. all_params &
        ) then
        man_tag(identity)%acquired = .false.
        man_tag(actionvalue)%acquired = .true.
        ! stays .true. if all previous values have been true
        all_actionvalues = (all_actionvalues .and. .true. )
      else
        all_actionvalues = .false.
        if ( .not. man_tag(identity)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(identity)%name), ' is missing from Management file.'
        end if
      end if
      ! write(*,*) 'ALLACTIONVALUES', all_actionvalues

    else if (idx .eq. identity) then
      if( man_tag(code)%acquired &
        .and. man_tag(id)%acquired &
        ) then
        man_tag(code)%acquired = .false.
        man_tag(id)%acquired = .false.
        man_tag(identity)%acquired = .true.
      else
        if ( .not. man_tag(code)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(code)%name), ' is missing from Management file.'
        else if ( .not. man_tag(id)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(id)%name), ' is missing from Management file.'
        end if
      end if

    else if (idx .eq. param) then
      if( man_tag(p_name)%acquired &
        .and. man_tag(value)%acquired &
        ) then
        man_tag(p_name)%acquired = .false.
        man_tag(value)%acquired = .false.
        man_tag(param)%acquired = .true.
      end if

    end if

    !if ( idx .le. size(man_tag) ) then
    !  if ( man_tag(idx)%acquired ) then
    !    write(*,*) 'ACQUIRED: ', man_tag(idx)%name!, man_tag(idx)%acquired
    !  end if
    !end if

  end subroutine end_man_element_handler

  subroutine pcdata_man_chunk_handler(chunk)
    use read_write_xml_mod, only: read_param
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value

    param_value = trim(chunk)

    !write(*,*) 'CHUNK: ', trim(chunk)

    if (man_tag(wepsmanDB)%in_tag) then
      if (man_tag(version)%in_tag) then
        call read_param(man_tag(version)%name, param_value, manFile(isub)%mversion)
        man_tag(version)%acquired = .true.
      else if (man_tag(rotationyears)%in_tag) then
        call read_param(man_tag(rotationyears)%name, param_value, manFile(isub)%mperod)
        man_tag(rotationyears)%acquired = .true.
      else if (man_tag(wepsmanvalue)%in_tag) then
        if (man_tag(date)%in_tag) then
          call read_param(man_tag(date)%name, param_value, t_operDate)
          man_tag(date)%acquired = .true.
        else if (man_tag(operationDB)%in_tag ) then
          if (man_tag(operationname)%in_tag ) then
            t_operName = trim(param_value)
            man_tag(operationname)%acquired = .true.
          else if (man_tag(actionvalue)%in_tag ) then
            if (man_tag(identity)%in_tag ) then
              if (man_tag(code)%in_tag ) then
                t_code = trim(param_value)
                if (   t_code .eq. 'O' &
                  .or. t_code .eq. 'G' &
                  .or. t_code .eq. 'P' &
                  ) then
                  man_tag(code)%acquired = .true.
                  operID = ''
                  grpID = ''
                  procID = ''
                else
                  write(*,*) 'Unknown Identity code: "', trim(t_code), '" found in ', trim(manFile(isub)%tinfil)
                  call exit(1)
                end if
              else if (man_tag(id)%in_tag ) then
                man_tag(id)%acquired = .true.
                i_cnt = 0
                r_cnt = 0
                s_cnt = 0
                select case (t_code)
                case ('O')
                  operID = trim(param_value)
                  call get_value_cnts ( t_code, operID, int_cnt, real_cnt, str_cnt, ogp_id_idx )
                  if ( .not. associated(manFile(isub)%operFirst) ) then
                    manFile(isub)%operFirst => elemCreate( manFile(isub)%operFirst, operID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%oper => manFile(isub)%operFirst
                  else
                    manFile(isub)%oper%operNext => elemCreate( manFile(isub)%oper%operNext, operID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%oper => manFile(isub)%oper%operNext
                  end if
                  manFile(isub)%oper%operDate = t_operDate
                  manFile(isub)%oper%operName = trim(t_operName)
                  ! new operation, so nullify current group and process
                  nullify(manFile(isub)%grp)
                  nullify(manFile(isub)%proc)
                case ('G')
                  grpID = trim(param_value)
                  call get_value_cnts ( t_code, grpID, int_cnt, real_cnt, str_cnt, ogp_id_idx )
                  if ( .not. associated(manFile(isub)%oper) ) then
                    write(*,*) 'Group appears before Operation in Management File: ', trim(manFile(isub)%tinfil)
                    call exit(1)
                  else if ( .not. associated(manFile(isub)%oper%grpFirst) ) then
                    manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, grpID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%grp => manFile(isub)%oper%grpFirst
                  else
                    manFile(isub)%grp%grpNext => elemCreate( manFile(isub)%grp%grpNext, grpID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%grp => manFile(isub)%grp%grpNext
                  end if
                case ('P')
                  procID = trim(param_value)
                  call get_value_cnts ( t_code, procID, int_cnt, real_cnt, str_cnt, ogp_id_idx )
                  if ( .not. associated(manFile(isub)%grp) ) then
                    ! Operation has process without group preceeding, create null group to support structure.
                    manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, '00', 0, 0, 0 )
                    manFile(isub)%grp => manFile(isub)%oper%grpFirst
                  end if
                  if ( .not. associated(manFile(isub)%grp%procFirst) ) then
                    manFile(isub)%grp%procFirst => elemCreate( manFile(isub)%grp%procFirst, procID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%proc => manFile(isub)%grp%procFirst
                  else
                    manFile(isub)%proc%procNext => elemCreate( manFile(isub)%proc%procNext, procID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%proc => manFile(isub)%proc%procNext
                  end if
                end select
              end if
            else if (man_tag(param)%in_tag ) then
              if (man_tag(p_name)%in_tag ) then
                ! sets index for placement of value into type array
                call get_value_type_index ( ogp_id_idx, param_value, p_type, p_idx)
                if ( p_idx .gt. 0 ) then
                  man_tag(p_name)%acquired = .true.
                end if
              else if (man_tag(value)%in_tag ) then
                if ( man_tag(p_name)%acquired ) then
                  select case (p_type)
                  case ('int')
                    if ( operID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%oper%i_param(p_idx)%p_value )
                      manFile(isub)%oper%i_param(p_idx)%p_acquired = .true.
                    else if ( grpID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%grp%i_param(p_idx)%p_value )
                      manFile(isub)%grp%i_param(p_idx)%p_acquired = .true.
                    else if ( procID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%proc%i_param(p_idx)%p_value )
                      manFile(isub)%proc%i_param(p_idx)%p_acquired = .true.
                    end if
                  case ('real')
                    if ( operID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%oper%r_param(p_idx)%p_value )
                      manFile(isub)%oper%r_param(p_idx)%p_acquired = .true.
                    else if ( grpID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%grp%r_param(p_idx)%p_value )
                      manFile(isub)%grp%r_param(p_idx)%p_acquired = .true.
                    else if ( procID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%proc%r_param(p_idx)%p_value )
                      manFile(isub)%proc%r_param(p_idx)%p_acquired = .true.
                    end if
                  case ('str')
                    if ( operID .ne. '' ) then
                      manFile(isub)%oper%s_param(p_idx)%p_value = trim(param_value)
                      manFile(isub)%oper%s_param(p_idx)%p_acquired = .true.
                    else if ( grpID .ne. '' ) then
                      manFile(isub)%grp%s_param(p_idx)%p_value = trim(param_value)
                      manFile(isub)%grp%s_param(p_idx)%p_acquired = .true.
                    else if ( procID .ne. '' ) then
                      manFile(isub)%proc%s_param(p_idx)%p_value = trim(param_value)
                      manFile(isub)%proc%s_param(p_idx)%p_acquired = .true.
                    end if
                  end select
                  man_tag(value)%acquired = .true.
                end if
              end if
            end if
          end if
        end if

      end if
    end if

  end subroutine pcdata_man_chunk_handler

end module manage_xml_mod

