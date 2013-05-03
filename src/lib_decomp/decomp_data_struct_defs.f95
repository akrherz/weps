!$Author$
!$Date$
!$Revision$
!$HeadURL$

module decomp_data_struct_defs

     integer, dimension(:), allocatable :: am0dfl    ! flag to print DECOMP output
                                                     !  0 then print no output
                                                     !  1 then print detailed output for above ground residue
                                                     !  2 then print detailed output for below ground residue and pool files
                                                     !  3 then print detailed output for both above and below ground residue and pool files

     integer, dimension(:), allocatable :: am0ddb    ! flag to print DECOMPosition variables before and after the call to DECOMP
                                                     ! 0 = no output
                                                     ! 1 = output

contains


end module decomp_data_struct_defs

