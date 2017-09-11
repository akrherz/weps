!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_data_struct_mod

  use manage_data_struct_defs, only: operation, group, process
  use manage_data_struct_defs, only: max_ogp, param_nt

  interface elemCreate
    module procedure operCreate
    module procedure grpCreate
    module procedure procCreate
  end interface elemCreate

  interface getManVal
    module procedure getManVal_oper_int
    module procedure getManVal_oper_real
    module procedure getManVal_oper_str
    module procedure getManVal_grp_int
    module procedure getManVal_grp_real
    module procedure getManVal_grp_str
    module procedure getManVal_proc_int
    module procedure getManVal_proc_real
    module procedure getManVal_proc_str
  end interface getManVal
contains

  function operCreate(operPntr, operID) result(operNew)
    type(operation), pointer :: operPntr
    character(len=*), intent(in) :: operID
    type(operation), pointer :: operNew

    integer :: int_cnt
    integer :: real_cnt
    integer :: str_cnt
    integer :: alloc_stat
    integer :: sum_stat
    integer :: idx

    allocate(operPntr, stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Operation pointer: P ', operID
    end if
    operPntr%operID = operID
    call get_value_cnts ( 'O', operID, int_cnt, real_cnt, str_cnt, operPntr%OGPidx ) 
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

  function grpCreate(grpPntr, grpID) result(grpNew)
    type(group), pointer :: grpPntr
    character(len=*), intent(in) :: grpID
    type(group), pointer :: grpNew

    integer :: int_cnt
    integer :: real_cnt
    integer :: str_cnt
    integer :: alloc_stat
    integer :: sum_stat
    integer :: idx

    allocate(grpPntr, stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Group pointer: G ', grpID
    end if
    grpPntr%grpID = grpID
    call get_value_cnts ( 'G', grpID, int_cnt, real_cnt, str_cnt, grpPntr%OGPidx ) 
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

  function procCreate(procPntr, procID) result(procNew)
    type(process), pointer :: procPntr
    character(len=*), intent(in) :: procID
    type(process), pointer :: procNew

    integer :: int_cnt
    integer :: real_cnt
    integer :: str_cnt
    integer :: alloc_stat
    integer :: sum_stat
    integer :: idx

    allocate(procPntr, stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,'(a,i0)') 'Unable to allocate Process pointer: P ', procID
    end if
    procPntr%procID = procID
    call get_value_cnts ( 'P', procID, int_cnt, real_cnt, str_cnt, procPntr%OGPidx ) 
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

  subroutine get_value_cnts ( OGP, ID, int_cnt, real_cnt, str_cnt, code_id_index )
    character(len=*), intent(in) :: OGP
    character(len=*), intent(in) :: ID
    integer, intent(out) :: int_cnt
    integer, intent(out) :: real_cnt
    integer, intent(out) :: str_cnt
    integer, intent(out) :: code_id_index

    integer :: idx

    ! set default values (covers dummy Group 0)
    int_cnt = 0
    real_cnt = 0
    str_cnt = 0
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

  function get_code_id_index (OGP, ID) result(code_id_index)
    character(len=*), intent(in) :: OGP
    character(len=*), intent(in) :: ID
    integer :: code_id_index

    integer :: idx

    do idx = 1, max_ogp
      if (    OGP .eq. param_nt(idx)%ogp &
        .and. ID .eq. param_nt(idx)%id ) then
        exit   ! found so exit and return index number
      end if
    end do
    code_id_index = idx
  end function get_code_id_index

  subroutine get_value_type_index ( code_id_index, val_name, val_type, val_index )
    integer, intent(in) :: code_id_index
    character(len=*), intent(in) :: val_name
    character(len=4), intent(out) :: val_type
    integer, intent(out) :: val_index

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

  function get_value_index ( code_id_index, val_name ) result(val_index)
    integer, intent(in) :: code_id_index
    character(len=*), intent(in) :: val_name
    integer :: val_index

    integer idx

    do idx = 1, size(param_nt(code_id_index)%i_name)
      if ( val_name .eq. param_nt(code_id_index)%i_name(idx) ) then
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
        exit  ! found the name
      end if
    end do
    if ( idx .gt. size(param_nt(code_id_index)%s_name) ) then
      val_index = 0
    else
      val_index = idx
      return  ! found the name
    end if

  end function get_value_index

  subroutine getManVal_oper_int(operPtr, nameV, manVal)
    use manage_data_struct_defs, only: operation
    type(operation), pointer :: operPtr
    character(len=*), intent(in) :: nameV
    integer, intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( operPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = operPtr%i_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_oper_int

  subroutine getManVal_oper_real(operPtr, nameV, manVal)
    use manage_data_struct_defs, only: operation
    type(operation), pointer :: operPtr
    character(len=*), intent(in) :: nameV
    real, intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( operPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = operPtr%r_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_oper_real

  subroutine getManVal_oper_str(operPtr, nameV, manVal)
    use manage_data_struct_defs, only: operation
    type(operation), pointer :: operPtr
    character(len=*), intent(in) :: nameV
    character(len=*), intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( operPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = operPtr%s_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_oper_str

  subroutine getManVal_grp_int(grpPtr, nameV, manVal)
    use manage_data_struct_defs, only: operation
    type(group), pointer :: grpPtr
    character(len=*), intent(in) :: nameV
    integer, intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( grpPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = grpPtr%i_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_grp_int

  subroutine getManVal_grp_real(grpPtr, nameV, manVal)
    use manage_data_struct_defs, only: operation
    type(group), pointer :: grpPtr
    character(len=*), intent(in) :: nameV
    real, intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( grpPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = grpPtr%r_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_grp_real

  subroutine getManVal_grp_str(grpPtr, nameV, manVal)
    use manage_data_struct_defs, only: operation
    type(group), pointer :: grpPtr
    character(len=*), intent(in) :: nameV
    character(len=*), intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( grpPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = grpPtr%s_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_grp_str

  subroutine getManVal_proc_int(procPtr, nameV, manVal)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: nameV
    integer, intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( procPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = procPtr%i_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_proc_int

  subroutine getManVal_proc_real(procPtr, nameV, manVal)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: nameV
    real, intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( procPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = procPtr%r_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_proc_real

  subroutine getManVal_proc_str(procPtr, nameV, manVal)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: nameV
    character(len=*), intent(out) :: manVal

    integer :: idx

    idx = get_value_index ( procPtr%OGPidx, nameV )
    if ( idx .gt. 0 ) then
      manVal = procPtr%s_param(idx)%p_value
    else
      write(*,*) 'Name: ', trim(nameV), ' is not properly specified in getManVal request.'
      call exit(1)
    end if

  end subroutine getManVal_proc_str

end module manage_data_struct_mod

