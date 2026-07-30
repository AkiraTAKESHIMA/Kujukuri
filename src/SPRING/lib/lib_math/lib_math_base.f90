module lib_math_base
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: is_int
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface is_int
    module procedure is_int__real
    module procedure is_int__dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
logical function is_int__real(x) result(res)
  implicit none
  real(4), intent(in) :: x

  res = int(x) == x
end function is_int__real
!===============================================================
!
!===============================================================
logical function is_int__dble(x) result(res)
  implicit none
  real(8), intent(in) :: x

  res = int(x) == x
end function is_int__dble
!===============================================================
!
!===============================================================
end module lib_math_base
