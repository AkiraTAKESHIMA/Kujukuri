module ls_log
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: spring_print_error_message
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine spring_print_error_message()
  implicit none 

  call errend()
end subroutine spring_print_error_message
!===============================================================
!
!===============================================================
end module ls_log
