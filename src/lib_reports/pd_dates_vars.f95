!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! The purpose of the derived-type "dates" variables is to
! to keep track of the time period (duration) that a variable
! is actively being tracked (summed, averaged, etc.)

MODULE pd_dates_vars

    USE pd_dates_type_def

    IMPLICIT NONE

    type :: reporting_dates
       ! "pd_dates" structures used by "pd_update" structures
       TYPE (pd_dates_type),DIMENSION(:), ALLOCATABLE :: yrly
       TYPE (pd_dates_type),DIMENSION(:,:), ALLOCATABLE :: monthly
       TYPE (pd_dates_type),DIMENSION(:,:), ALLOCATABLE :: hmonth
       TYPE (pd_dates_type),DIMENSION(:), ALLOCATABLE :: period
       TYPE (pd_dates_type),DIMENSION(:), ALLOCATABLE :: yr
    end type reporting_dates

END MODULE pd_dates_vars
