!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_data_struct_defs

     integer, dimension(:), allocatable :: am0tfl    ! flag to print MANAGEment (TILLAGE) output
                                                     ! 0 = no output
                                                     ! 1 = detailed output file created
     integer, dimension(:), allocatable :: am0tdb    ! flag to print MANAGEment variables before and after the call to MANAGE
                                                     ! 0 = no output
                                                     ! 1 = output

contains


end module manage_data_struct_defs

