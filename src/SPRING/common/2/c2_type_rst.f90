module c2_type_rst
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: rst_thresh_
  public :: iarea_max_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type rst_thresh_
    character(CLEN_KEY) :: ineq_iratio_min, ineq_iratio_max
    real(8) :: iratio_min, iratio_max
    real(8) :: iratio_ignored
    real(8) :: iratio_min_idx
  end type

  type iarea_max_
    integer(8) :: idx_single
    integer :: nij
    integer(8), pointer :: list_idx(:)
    real(8) :: val
  end type
end module c2_type_rst
