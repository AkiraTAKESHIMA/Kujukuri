module def_type
  use c1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: opt_
  !-------------------------------------------------------------
  type opt_
    type(opt_sys_)   :: sys
    type(opt_log_)   :: log
    type(opt_earth_) :: earth
  end type
  !-------------------------------------------------------------
end module def_type
