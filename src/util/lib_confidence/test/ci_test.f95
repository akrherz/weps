!$Author$
!$Date$
!$Revision$
!$HeadURL$

program ci_test

use ci_select_mod, only: ci_select
use precision_mod, only: precision_init

! opens a file of values and computes the mean and confidence intervals
! starting with the first three values, then 4 then 5 ... till all are included

! input consists of a list of integer, real number pairs. integer is # years in rotation

! output is a ci.out style listing of year values and rotation average values and confidence intervals

integer :: idx   ! loop counter
integer :: nval  ! the cound the count of numbers to be read
integer :: ios   ! iostat return value
real, allocatable :: value(:) ! values read in
real, allocatable :: a_value(:) ! average values for each rotation
real :: prob  ! confidence interval probability level, 90% interval specified as 0.9
real :: mean, ci_hi, ci_low   ! mean and confidence interval values
integer :: i_temp
real :: r_temp
real :: sum_temp

call precision_init()

! count number of data values on stdin
nval = 0
read(*,*, iostat=ios) i_temp, r_temp
do while(ios .eq. 0)
    nval = nval + 1
    read(*,*, iostat=ios)  i_temp, r_temp
end do
!write(*,*) "nval = ", nval

! reset stdin to beginning
rewind(5)

! allocate array for values
allocate (value(1:nval))
allocate (a_value(1:(nval/i_temp)))

sum_temp = 0.0
! read in values
do idx = 1, nval
    read(*,*) i_temp, value(idx)
    if( i_temp .gt. 1 ) then
        ! multi year rotations, find averages for statistics
        ! add to sum
        sum_temp = sum_temp + value(idx)
        if( mod(idx, i_temp) .eq. 0 ) then
            ! last year of rotation cycle
            ! compute annual rotation average
            a_value(idx/i_temp) = sum_temp / i_temp
            sum_temp = 0.0
        end if
    else
        ! value and a_value are the same
        a_value(idx) = value(idx)
    end if
end do

! test various probability levels
prob = 0.9
!do while (prob .lt. 0.999)
    !write(*,*) "# confidence level = ", prob
    !Separate levels with blank lines
    !write(*,*)
    !write(*,*)
    ! write ci.out header
    write(*,*) '  yr_total  |   yrly_ave   |  Low_90.00% | High_90.00% |'

    sum_temp = 0.0
    ! loop through annual values
    do idx = 1, nval
        if( mod(idx, i_temp) .gt. 0 ) then
            ! write individual year value
            write(UNIT=*,FMT="(f10.5,a3)",ADVANCE="YES") value(idx)," | "
        else
            sum_temp = sum_temp + a_value(idx/i_temp)
            ! end of rotation
            if( idx/i_temp .le. 3 ) then
                ! write individual year value
                write(UNIT=*,FMT="(f10.5,a3, f10.5)",ADVANCE="YES") value(idx)," | "
            else
                ! write individual year value
                write(UNIT=*,FMT="(f10.5,a3, f10.5)",ADVANCE="NO") value(idx)," | "
                ! find confidence intervals
                call ci_select(a_value, (idx/i_temp), prob, mean, ci_hi, ci_low)
                write(UNIT=*,FMT="(f12.5,a,f12.5,a,g16.5,a)",ADVANCE="YES") (sum_temp/idx)*i_temp," | ",ci_low," | ",ci_hi," | "
            end if
        end if
    end do
!    prob = prob + 0.01
!end do

end program ci_test
