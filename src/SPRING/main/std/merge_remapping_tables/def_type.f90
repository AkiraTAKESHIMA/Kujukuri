module def_type
  use lib_const
  use lib_io
  use c1_type_opt
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: f_rt_
  public :: f_grid_
  public :: input_
  public :: output_
  public :: opt_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type f_rt_
    integer(8)  :: nij
    type(file_) :: f_sidx, f_tidx, f_area, f_coef
  end type

  type f_grid_
    integer(8)  :: nmax
    integer(8)  :: nmax_valid
    integer(8)  :: idxmin, idxmax
    type(file_) :: f_idx, f_ara
  end type

  type input_
    integer :: nFiles_rt
    type(f_rt_), pointer :: list_f_rt(:)
    integer :: nFiles_grid
    type(f_grid_), pointer :: list_f_grid(:)
    character(clen_key) :: opt_idx_dup
    integer(8) :: idx_miss
  end type

  type output_
    type(rt_) :: rt

    type(f_grid_) :: f_grid
    character(clen_path) :: path_grid_im
  end type

  type opt_
    type(opt_sys_) :: sys
    type(opt_log_) :: log
  end type
end module def_type
