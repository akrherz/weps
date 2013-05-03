!$Author$
!$Date$
!$Revision$
!$HeadURL$

module hydro_data_struct_defs

     integer, dimension(:), allocatable :: am0hfl    ! flag to print HYDROlogy output
                                                     ! 0 = no output
                                                     ! 1 = daily
                                                     ! 2 = hourly
                                                     ! 3 = daily and hourly
                                                     ! 4 = soil temperature
                                                     ! 5 = daily and soil temperature
                                                     ! 6 = hourly and soil temperature
                                                     ! 7 = daily, hourly, and soil temperature
     integer, dimension(:), allocatable :: am0hdb    ! flag to print HYDROlogy variables before and after the call to HYDRO
                                                     ! 0 = no output
                                                     ! 1 = output

contains


end module hydro_data_struct_defs

