!$Author$
!$Date$
!$Revision$
!$HeadURL$

module precision_mod

  ! defined variables for portable checking of precision related numbers
  ! variables are initialized in main to make available to all subprograms

  real :: max_arg_exp  !  maximum real value allowed for argument to exponential function without overflowing
  real :: max_real     !  maximum real number allowed

  double precision :: max_arg_exp_dp  !  maximum double precison allowed for argument to exponential function without overflowing
  double precision :: max_real_dp     !  maximum double precision number allowed

contains

  subroutine precision_init()

    ! initialize math precision global variables
    ! the factor here is due to the implementation of the EXP function
    ! apparently, the limit is not the real number limit, but something else
    ! this works in Lahey, but I cannot attest to it's portability

    max_real = huge(1.0) * 0.999150
    max_arg_exp = log(max_real)

    max_real_dp = huge(1.0d0) * 0.999150d0
    max_arg_exp_dp = log(max_real_dp)

  end   subroutine precision_init

end module precision_mod
