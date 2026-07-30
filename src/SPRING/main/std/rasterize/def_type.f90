module def_type
  use lib_const
  use lib_io
  use c1_type_opt
  use c2_type_rst
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: output_
  public :: opt_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type output_
    type(rst_thresh_) :: thresh
    type(file_) :: f_iarea_sum
    type(file_) :: f_iratio_sum
    type(file_) :: f_mask
    type(file_) :: f_idx
    real(8) :: val_miss
  end type

  type opt_
    type(opt_sys_)   :: sys
    type(opt_log_)   :: log
    type(opt_earth_) :: earth
  end type
  !-------------------------------------------------------------
end module def_type
