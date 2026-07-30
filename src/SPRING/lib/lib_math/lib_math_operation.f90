module lib_math_operation
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: add
  public :: add_cyclic
  public :: next_cyclic
  public :: prev_cyclic
  public :: mul
  public :: div
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface add
    module procedure add__int1
    module procedure add__int2
    module procedure add__int4
    module procedure add__int8
    module procedure add__real
    module procedure add__dble
    module procedure add__int1_int1
    module procedure add__int2_int2
    module procedure add__int4_int1
    module procedure add__int4_int2
    module procedure add__int4_int4
    module procedure add__int4_int8
    module procedure add__int8_int1
    module procedure add__int8_int2
    module procedure add__int8_int4
    module procedure add__int8_int8
    module procedure add__real_real
    module procedure add__dble_dble
  end interface

  interface add_cyclic
    module procedure add_cyclic__int1_int1
    module procedure add_cyclic__int2_int2
    module procedure add_cyclic__int4_int4
    module procedure add_cyclic__int4_int8
    module procedure add_cyclic__int8_int4
    module procedure add_cyclic__int8_int8
  end interface

  interface next_cyclic
    module procedure next_cyclic__int1
    module procedure next_cyclic__int2
    module procedure next_cyclic__int4
    module procedure next_cyclic__int8
  end interface

  interface prev_cyclic
    module procedure prev_cyclic__int1
    module procedure prev_cyclic__int2
    module procedure prev_cyclic__int4
    module procedure prev_cyclic__int8
  end interface

  interface mul
    module procedure mul__int4_int4
    module procedure mul__int4_int8
    module procedure mul__int4_dble
    module procedure mul__int8_int4
    module procedure mul__int8_int8
    module procedure mul__int8_dble
    module procedure mul__dble_int4
    module procedure mul__dble_int8
    module procedure mul__dble_dble
  end interface

  interface div
    module procedure div__dble_int4
    module procedure div__dble_int8
    module procedure div__dble_dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine add__int1(x)
  implicit none
  integer(1), intent(inout) :: x

  x = x + 1_1
end subroutine add__int1
!===============================================================
!
!===============================================================
subroutine add__int2(x)
  implicit none
  integer(2), intent(inout) :: x

  x = x + 1_2
end subroutine add__int2
!===============================================================
!
!===============================================================
subroutine add__int4(x)
  implicit none
  integer(4), intent(inout) :: x

  x = x + 1_4
end subroutine add__int4
!===============================================================
!
!===============================================================
subroutine add__int8(x)
  implicit none
  integer(8), intent(inout) :: x

  x = x + 1_8
end subroutine add__int8
!===============================================================
!
!===============================================================
subroutine add__real(x)
  implicit none
  real(4), intent(inout) :: x

  x = x + 1.0
end subroutine add__real
!===============================================================
!
!===============================================================
subroutine add__dble(x)
  implicit none
  real(8), intent(inout) :: x

  x = x + 1.d0
end subroutine add__dble
!===============================================================
!
!===============================================================
subroutine add__int1_int1(x, y)
  implicit none
  integer(1), intent(inout) :: x
  integer(1), intent(in)    :: y

  x = x + y
end subroutine add__int1_int1
!===============================================================
!
!===============================================================
subroutine add__int2_int2(x, y)
  implicit none
  integer(2), intent(inout) :: x
  integer(2), intent(in)    :: y

  x = x + y
end subroutine add__int2_int2
!===============================================================
!
!===============================================================
subroutine add__int4_int1(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(1), intent(in)    :: y

  x = x + y
end subroutine add__int4_int1
!===============================================================
!
!===============================================================
subroutine add__int4_int2(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(2), intent(in)    :: y

  x = x + y
end subroutine add__int4_int2
!===============================================================
!
!===============================================================
subroutine add__int4_int4(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x + y
end subroutine add__int4_int4
!===============================================================
!
!===============================================================
subroutine add__int4_int8(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x + int(y,4)
end subroutine add__int4_int8
!===============================================================
!
!===============================================================
subroutine add__int8_int1(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(1), intent(in)    :: y

  x = x + y
end subroutine add__int8_int1
!===============================================================
!
!===============================================================
subroutine add__int8_int2(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(2), intent(in)    :: y

  x = x + y
end subroutine add__int8_int2
!===============================================================
!
!===============================================================
subroutine add__int8_int4(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x + y
end subroutine add__int8_int4
!===============================================================
!
!===============================================================
subroutine add__int8_int8(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x + y
end subroutine add__int8_int8
!===============================================================
!
!===============================================================
subroutine add__real_real(x, y)
  implicit none
  real(4), intent(inout) :: x
  real(4), intent(in)    :: y

  x = x + y
end subroutine add__real_real
!===============================================================
!
!===============================================================
subroutine add__dble_dble(x, y)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: y

  x = x + y
end subroutine add__dble_dble
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine add_cyclic__int1_int1(x, inc, xmax)
  implicit none
  integer(1), intent(inout) :: x
  integer(1), intent(in)    :: inc
  integer(1), intent(in)    :: xmax

  x = x+inc - (x+inc-1_1)/xmax*xmax
  if( x <= 0_1 ) x = x + xmax
end subroutine add_cyclic__int1_int1
!===============================================================
!
!===============================================================
subroutine add_cyclic__int2_int2(x, inc, xmax)
  implicit none
  integer(2), intent(inout) :: x
  integer(2), intent(in)    :: inc
  integer(2), intent(in)    :: xmax

  x = x+inc - (x+inc-1_2)/xmax*xmax
  if( x <= 0_2 ) x = x + xmax
end subroutine add_cyclic__int2_int2
!===============================================================
!
!===============================================================
subroutine add_cyclic__int4_int4(x, inc, xmax)
  implicit none
  integer(4), intent(inout) :: x
  integer(4), intent(in)    :: inc
  integer(4), intent(in)    :: xmax

  x = x+inc - (x+inc-1)/xmax*xmax
  if( x <= 0 ) x = x + xmax
end subroutine add_cyclic__int4_int4
!===============================================================
!
!===============================================================
subroutine add_cyclic__int4_int8(x, inc, xmax)
  implicit none
  integer(4), intent(inout) :: x
  integer(8), intent(in)    :: inc
  integer(4), intent(in)    :: xmax

  x = x+int(inc,4) - (x+int(inc,4)-1)/xmax*xmax
  if( x <= 0 ) x = x + xmax
end subroutine add_cyclic__int4_int8
!===============================================================
!
!===============================================================
subroutine add_cyclic__int8_int4(x, inc, xmax)
  implicit none
  integer(8), intent(inout) :: x
  integer(4), intent(in)    :: inc
  integer(8), intent(in)    :: xmax

  x = x+inc - (x+inc-1)/xmax*xmax
  if( x <= 0_8 ) x = x + xmax
end subroutine add_cyclic__int8_int4
!===============================================================
!
!===============================================================
subroutine add_cyclic__int8_int8(x, inc, xmax)
  implicit none
  integer(8), intent(inout) :: x
  integer(8), intent(in)    :: inc
  integer(8), intent(in)    :: xmax

  x = x+inc - (x+inc-1)/xmax*xmax
  if( x <= 0_8 ) x = x + xmax
end subroutine add_cyclic__int8_int8
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer(1) function next_cyclic__int1(n, nmax) result(res)
  implicit none
  integer(1), intent(in) :: n, nmax

  res = n
  call add_cyclic__int1_int1(res, 1_1, nmax)
end function next_cyclic__int1
!===============================================================
!
!===============================================================
integer(2) function next_cyclic__int2(n, nmax) result(res)
  implicit none
  integer(2), intent(in) :: n, nmax

  res = n
  call add_cyclic__int2_int2(res, 1_2, nmax)
end function next_cyclic__int2
!===============================================================
!
!===============================================================
integer(4) function next_cyclic__int4(n, nmax) result(res)
  implicit none
  integer(4), intent(in) :: n, nmax

  res = n
  call add_cyclic__int4_int4(res, 1_4, nmax)
end function next_cyclic__int4
!===============================================================
!
!===============================================================
integer(8) function next_cyclic__int8(n, nmax) result(res)
  implicit none
  integer(8), intent(in) :: n, nmax

  res = n
  call add_cyclic__int8_int8(res, 1_8, nmax)
end function next_cyclic__int8
!===============================================================
!
!===============================================================
integer(1) function prev_cyclic__int1(n, nmax) result(res)
  implicit none
  integer(1), intent(in) :: n, nmax

  res = n
  call add_cyclic__int1_int1(res, -1_1, nmax)
end function prev_cyclic__int1
!===============================================================
!
!===============================================================
integer(2) function prev_cyclic__int2(n, nmax) result(res)
  implicit none
  integer(2), intent(in) :: n, nmax

  res = n
  call add_cyclic__int2_int2(res, -1_2, nmax)
end function prev_cyclic__int2
!===============================================================
!
!===============================================================
integer(4) function prev_cyclic__int4(n, nmax) result(res)
  implicit none
  integer(4), intent(in) :: n, nmax

  res = n
  call add_cyclic__int4_int4(res, -1_4, nmax)
end function prev_cyclic__int4
!===============================================================
!
!===============================================================
integer(8) function prev_cyclic__int8(n, nmax) result(res)
  implicit none
  integer(8), intent(in) :: n, nmax

  res = n
  call add_cyclic__int8_int8(res, -1_8, nmax)
end function prev_cyclic__int8
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine mul__int4_int4(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x * y
end subroutine mul__int4_int4
!===============================================================
!
!===============================================================
subroutine mul__int4_int8(x, y)
  implicit none
  integer(4), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = int(real(x,8) * y,4)
end subroutine mul__int4_int8
!===============================================================
!
!===============================================================
subroutine mul__int4_dble(x, y)
  implicit none
  integer(4), intent(inout) :: x
  real(8)   , intent(in)    :: y

  x = int(real(x,8) * y,4)
end subroutine mul__int4_dble
!===============================================================
!
!===============================================================
subroutine mul__int8_int4(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x * y
end subroutine mul__int8_int4
!===============================================================
!
!===============================================================
subroutine mul__int8_int8(x, y)
  implicit none
  integer(8), intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x * y
end subroutine mul__int8_int8
!===============================================================
!
!===============================================================
subroutine mul__int8_dble(x, y)
  implicit none
  integer(8), intent(inout) :: x
  real(8)   , intent(in)    :: y

  x = int(real(x,8) * y,8)
end subroutine mul__int8_dble
!===============================================================
!
!===============================================================
subroutine mul__dble_int4(x, y)
  implicit none
  real(8)   , intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x * y
end subroutine mul__dble_int4
!===============================================================
!
!===============================================================
subroutine mul__dble_int8(x, y)
  implicit none
  real(8)   , intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x * y
end subroutine mul__dble_int8
!===============================================================
!
!===============================================================
subroutine mul__dble_dble(x, y)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: y

  x = x * y
end subroutine mul__dble_dble
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine div__dble_int4(x, a)
  implicit none
  real(8)   , intent(inout) :: x
  integer(4), intent(in)    :: a

  x = x / a
end subroutine div__dble_int4
!===============================================================
!
!===============================================================
subroutine div__dble_int8(x, a)
  implicit none
  real(8)   , intent(inout) :: x
  integer(8), intent(in)    :: a

  x = x / a
end subroutine div__dble_int8
!===============================================================
!
!===============================================================
subroutine div__dble_dble(x, a)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: a

  x = x / a
end subroutine div__dble_dble
!===============================================================
!
!===============================================================
end module lib_math_operation
