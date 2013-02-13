!$Author$
!$Date$
!$Revision$
!$HeadURL$

module debug_mod
  implicit none

  type decomp_debug
     integer :: tday
     integer :: tmo
     integer :: tyr

  end type decomp_debug

  type(decomp_debug), dimension(:), allocatable :: tddbug

contains

  subroutine create_decomp_debug(nsubr, tddbug)
     integer, intent(in) :: nsubr
     type(decomp_debug), dimension(:), allocatable, intent(inout) :: tddbug
     integer :: alloc_stat
     allocate( tddbug(nsubr), stat=alloc_stat )
     if( alloc_stat .gt. 0 ) then
        Write(*,*) 'ERROR: unable to allocate decomp_debug'
     end if
  end subroutine create_decomp_debug

  subroutine destroy_decomp_debug(tddbug)
     type(decomp_debug), dimension(:), allocatable, intent(inout) :: tddbug
     integer :: dealloc_stat
     deallocate( tddbug, stat=dealloc_stat )
     if( dealloc_stat .gt. 0 ) then
        Write(*,*) 'ERROR: unable to deallocate decomp_debug'
     end if
  end subroutine destroy_decomp_debug

end module debug_mod
