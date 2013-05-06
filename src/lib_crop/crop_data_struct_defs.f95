!$Author$
!$Date$
!$Revision$
!$HeadURL$

module crop_data_struct_defs

     integer, dimension(:), allocatable :: am0cfl    ! flag to print CROP output
                                                     ! 0 = no output
                                                     ! 1 = detailed output file created
     integer, dimension(:), allocatable :: am0cdb    ! flag to print CROP variables before and after the call to CROP
                                                     ! 0 = no output
                                                     ! 1 = output

!  contains


end module crop_data_struct_defs

