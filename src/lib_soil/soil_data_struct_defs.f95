!$Author$
!$Date$
!$Revision$
!$HeadURL$

module soil_data_struct_defs

     integer, dimension(:), allocatable :: am0sfl    ! flag to print SOIL output
                                                     ! 0 = no output
                                                     ! 1 = detailed output file created
     integer, dimension(:), allocatable :: am0sdb    ! flag to print SOIL variables before and after the call to SOIL
                                                     ! 0 = no output
                                                     ! 1 = output

!  contains


end module soil_data_struct_defs

