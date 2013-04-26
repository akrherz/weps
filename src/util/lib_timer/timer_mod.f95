!$Author$
!$Date$
!$Revision$
!$HeadURL$

module timer_mod

      integer   TIMSTART
      parameter (TIMSTART = 1)

      integer   TIMSTOP
      parameter (TIMSTOP = 2)

      integer   TIMPRINT
      parameter (TIMPRINT = 3)

      integer   TIMRESET
      parameter (TIMRESET = 4)

      integer   TIMCROP
      parameter (TIMCROP = 1)

      integer   TIMHYDR
      parameter (TIMHYDR = 2)

      integer   TIMSOIL
      parameter (TIMSOIL = 3)

      integer   TIMDECO
      parameter (TIMDECO = 4)

      integer   TIMMANG
      parameter (TIMMANG = 5)

      integer   TIMDARC
      parameter (TIMDARC = 6)

      integer   TIMEROS
      parameter (TIMEROS = 7)

      integer   TIMSBWIND
      parameter (TIMSBWIND = 8)

      integer   TIMSBEROD
      parameter (TIMSBEROD = 9)

      integer   TIMSBQOUT
      parameter (TIMSBQOUT = 10)

      integer   TIMWEPS
      parameter (TIMWEPS = 11)

      character*8  timnam(0:11)
      data timnam /'system', 'crop', 'hydro', 'soil', 'decomp',         &
     &  'manage', 'darcy', 'erosion', 'sbwind','sberod','sbqout','weps'/

  contains

    subroutine timer(timnum,timact)
! ****************************************************************** wjr
!
! Provides benchmark data
!
!       Edit History
!       05-Feb-99       wjr     Original coding

      integer     timnum                        !# of timer being used
      integer     timact                        !action: 1==start, 2==end, 3==print, 4==reset
      integer     idx

      real        timarr(0:11)
      real        tim
      integer     lsttim
    

      data timarr /12*0.0/
      data lsttim / 0 /

      save timarr

      call cpu_time(tim)

      select case (timact)
      case (1)                                  ! start a timer
        if (timnum .ne. TIMWEPS) then
            timarr(0) = timarr(0) + tim
        endif
        timarr(timnum) = timarr(timnum) - tim
      case (2)                                  ! stop a timer
        if (timnum .ne. TIMWEPS) then
            timarr(0) = timarr(0) - tim
        endif
        timarr(timnum) = timarr(timnum) + tim
      case (3)                                  ! print out timers
        timarr(0) = timarr(0) + tim
        do idx = 0,11
          write(*,fmt="(' ',a8,2x,f8.2)") timnam(idx),timarr(idx)
        end do
        timarr(0) = timarr(0) - tim
      case (4)                                  ! reset a timer
        timarr(timnum) = 0.0
      end select

    end subroutine timer     

end module timer_mod
