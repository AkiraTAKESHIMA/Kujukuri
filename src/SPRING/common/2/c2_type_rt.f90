module c2_type_rt
  use lib_const
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Remapping table
  !-------------------------------------------------------------
  ! rt_
  !  |--- (rt_main_) main
  !  |      |--- (file_rt_main_) f(:)
  !  |--- (rt_vrf_) vrf_source
  !  |      |--- (file_rt_vrf_) f(:)
  !  |--- (rt_vrf_) vrf_target
  !  |      |--- (file_rt_vrf_) f(:)
  !  |--- (file_rt_im_) f_im(:)
  !         |--- (file_rt_main_) f_main
  !         |--- (file_rt_vrf_) f_vrf_source
  !         |--- (file_rt_vrf_) f_vrf_target
  !-------------------------------------------------------------
  public :: opt_rt_coef_
  public :: opt_rt_area_

  public :: file_rt_main_

  public :: rt_main_

  public :: file_rt_vrf_
  public :: rt_vrf_

  public :: rt_

  public :: rt1d_
  !-------------------------------------------------------------
  type opt_rt_coef_
    logical :: is_sum_modify_enabled
    real(8) :: sum_modify

    logical :: is_sum_modify_ulim_enabled
    real(8) :: sum_modify_ulim

    logical :: is_zero_positive_enabled
    logical :: is_zero_negative_enabled
    real(8) :: zero_positive  !(0,$self) is modified to zero
    real(8) :: zero_negative  !($self,0) is modified to zero

    logical :: is_error_excess_enabled
    real(8) :: error_excess

    logical :: is_sum_error_excess_enabled
    real(8) :: sum_error_excess
  end type

  type opt_rt_area_
    logical :: is_ratio_zero_negative_enabled
    real(8) :: ratio_zero_negative  ! ($self,0) is modified to zero
    logical :: allow_le_ratio_zero_negative
  end type

  type file_rt_main_
    type(file_) :: sidx, &
                   tidx, &
                   area, &
                   coef
  end type

  type rt_main_
    character(CLEN_VAR) :: id

    character(CLEN_VAR) :: mode
    character(CLEN_KEY) :: mesh_coef
    character(CLEN_KEY) :: mesh_sort
    logical :: allow_empty

    logical :: is_sorted_by_sidx
    logical :: is_sorted_by_tidx
    integer(8) :: ijsize
    integer(8), pointer :: sidx(:), & !(ijsize)
                           tidx(:)
    real(8)   , pointer :: area(:), & !(ijsize)
                           coef(:)
    integer(8) :: sidx_vmin, sidx_vmax, tidx_vmin, tidx_vmax
    real(8)    :: area_vmin, area_vmax, coef_vmin, coef_vmax
    integer(8) :: sidx_imin, sidx_imax, tidx_imin, tidx_imax, &
                  area_imin, area_imax, coef_imin, coef_imax

    integer(8) :: nij

    type(file_rt_main_) :: f

    type(opt_rt_coef_) :: opt_coef
    type(opt_rt_area_) :: opt_area
  end type

  type file_rt_vrf_
    character(CLEN_VAR) :: id
    type(file_) :: out_grdidx     , &
                   out_grdara_true, &
                   out_grdara_rt  , &
                   out_rerr_grdara, &
                   out_grdnum     , &
                   out_iarea_sum  , &  ! raster
                   out_iratio_sum
  end type

  type rt_vrf_
    character(CLEN_VAR) :: id

    integer(8) :: idx_miss
    real(8)    :: dval_miss
    integer(8) :: ival_miss

    type(file_rt_vrf_) :: f

    integer(8), pointer :: grdidx(:)
    real(8)   , pointer :: grdara_true(:)
    real(8)   , pointer :: grdara_rt(:)
    real(8)   , pointer :: rerr_grdara(:)
    integer(8), pointer :: grdnum(:)
    real(8)   , pointer :: iarea_sum(:,:)
    real(8)   , pointer :: iratio_sum(:,:)
    real(8)    :: grdara_true_min, grdara_true_max, &
                  grdara_rt_min  , grdara_rt_max  , &
                  rerr_grdara_min, rerr_grdara_max
    integer(8) :: grdnum_min     , grdnum_max
    integer(8) :: idx_grdara_true_min, idx_grdara_true_max, &
                  idx_grdara_rt_min  , idx_grdara_rt_max  , &
                  idx_rerr_grdara_min, idx_rerr_grdara_max, &
                  idx_grdnum_min     , idx_grdnum_max
  end type

  type rt_
    character(CLEN_VAR) :: id
    character(CLEN_VAR*2+4) :: nam
    character(CLEN_VAR)     :: snam, tnam
    type(rt_main_) :: main
    type(rt_vrf_)  :: vrf_src
    type(rt_vrf_)  :: vrf_tgt
    character(CLEN_KEY) :: status  !c1 RT_STATUS__*
  end type

  type rt1d_
    integer(8)          :: ijsize
    integer(8)          :: mij
    integer(8)          :: idx_self
    integer(8), pointer :: idx(:)
    real(8)   , pointer :: ara(:)
  end type rt1d_
end module c2_type_rt
