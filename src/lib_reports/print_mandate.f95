!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
SUBROUTINE print_mandate_output(lun, mperod, mandate)

    use mandate_mod, only: opercrop_date

    IMPLICIT NONE

    INTEGER :: lun             ! output file unit number
    integer :: mperod          ! number of years in man rotation file
    type (opercrop_date), dimension(:), intent(in) :: mandate

    INTEGER :: i               ! local loop variable


    WRITE (UNIT=lun,FMT="(i4,(A))",ADVANCE="YES")                            &
          mperod, '  Number of years in WEPS management rotation file'

! Removed header lines to make it easier for the WEPS 1.0 interface
! to parse the mandate output file.  Shouldn't be a big issue as it
! is pretty easy to deduce what the file contents and format is - LEW
!   WRITE (UNIT=lun,FMT="(1(A))",ADVANCE="NO")                               &
!          'dd/mo/ry|'
!   WRITE (UNIT=lun,FMT="(2(A))",ADVANCE="YES")                              &
!          ' operation                                                   |', &
!          ' crop                                                        |'

    DO i = 1, size(mandate)
       WRITE (UNIT=lun,FMT="(2(i2,'/'),i2,'| ')",ADVANCE="NO")               &
             mandate(i)%d, mandate(i)%m, mandate(i)%y

       WRITE (UNIT=lun,FMT="(A80,'| ',A80,'|')",ADVANCE="YES")               &
             mandate(i)%opname, mandate(i)%cropname
    END DO

END SUBROUTINE print_mandate_output

