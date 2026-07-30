module c1_opt_ctrl
  use lib_const
  use lib_log
  use c1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_opt_sys
  public :: get_opt_sys

  public :: set_opt_log
  public :: get_opt_log

  public :: set_opt_earth
  public :: get_opt_earth
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_opt_ctrl'

  type(opt_sys_)   :: sys
  type(opt_log_)   :: log
  type(opt_earth_) :: earth
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_opt_sys(opt_sys)
  implicit none
  type(opt_sys_), intent(in) :: opt_sys

  sys = opt_sys
end subroutine set_opt_sys
!===============================================================
!
!===============================================================
subroutine get_opt_sys(opt_sys)
  implicit none
  type(opt_sys_), intent(out) :: opt_sys

  opt_sys = sys
end subroutine get_opt_sys
!===============================================================
!
!===============================================================
subroutine set_opt_log(opt_log)
  implicit none
  type(opt_log_), intent(in) :: opt_log

  log = opt_log
end subroutine set_opt_log
!===============================================================
!
!===============================================================
subroutine get_opt_log(opt_log)
  implicit none
  type(opt_log_), intent(out) :: opt_log

  opt_log = log
end subroutine get_opt_log
!===============================================================
!
!===============================================================
subroutine set_opt_earth(opt_earth)
  implicit none
  type(opt_earth_), intent(in) :: opt_earth

  earth = opt_earth
end subroutine set_opt_earth
!===============================================================
!
!===============================================================
subroutine get_opt_earth(opt_earth)
  implicit none
  type(opt_earth_), intent(out) :: opt_earth

  opt_earth = earth
end subroutine get_opt_earth
!===============================================================
!
!===============================================================

!===============================================================
!
!===============================================================
end module c1_opt_ctrl
