module def_type
  use lib_const
  use lib_io
  use c1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: agcm_
  public :: lsm_
  public :: opt_
  public :: opt_ext_
  !-------------------------------------------------------------
  type agcm_
    character(CLEN_VAR) :: id
    character(CLEN_VAR) :: nam

    type(file_) :: f_grdidx
    type(file_) :: f_grdara
    type(file_) :: f_grdlon
    type(file_) :: f_grdlat

    integer(8) :: nij
    integer(8), pointer :: idx(:)
    integer(8), pointer :: idxarg(:)
    real(8)   , pointer :: ara(:)
    real(8)   , pointer :: lon(:)
    real(8)   , pointer :: lat(:)
    real(8)   , pointer :: lndfrc(:)
    integer, pointer :: lx(:), ly(:)  ! layer
    integer(8) :: idx_miss
  end type

  type lsm_
    character(CLEN_VAR) :: id
    character(CLEN_VAR) :: nam

    type(file_) :: f_grdidx
    type(file_) :: f_grdara
    type(file_) :: f_grdlon
    type(file_) :: f_grdlat

    integer(8) :: nij
    integer(8), pointer :: idx(:)
    integer(8), pointer :: idxarg(:)
    real(8)   , pointer :: ara(:)
    real(8)   , pointer :: lon(:)
    real(8)   , pointer :: lat(:)
    integer, pointer :: lx(:), ly(:)  ! layer
    integer(8) :: idx_miss
  end type

  type opt_ext_
    character(CLEN_KEY) :: method_rivwat
  end type

  type opt_
    type(opt_sys_) :: sys
    type(opt_log_) :: log
    type(opt_earth_) :: earth
  end type
end module def_type
