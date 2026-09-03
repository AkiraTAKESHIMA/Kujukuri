module def_const
  use lib_const
  implicit none

  real(8), parameter :: GRAVITY = 9.8d0

  real(8), parameter :: ZS_MISS_THRESH = -1d2
  real(8), parameter :: ZS_MISS = -9999.d0
  real(8), parameter :: ZB_MISS = -9999.d0

  integer(4), parameter :: DOMAIN__OUTSIDE = 0
  integer(4), parameter :: DOMAIN__INSIDE  = 1
  integer(4), parameter :: DOMAIN__OUTLET  = 2


  integer, parameter :: SOLVER_METHOD__ADAPTIVE_RK45 = 1
  integer, parameter :: SOLVER_METHOD__TEST = 10

  integer, parameter :: RIVER_CRSSCT_METHOD__RECTANGULAR = 1
  integer, parameter :: RIVER_CRSSCT_METHOD__EXPLICIT = 2
end module def_const
