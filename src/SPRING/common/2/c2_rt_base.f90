module c2_rt_base
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use c1_const
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_rt
  public :: init_rt_main
  public :: init_rt_main_data
  public :: init_file_rt_main
  public :: init_opt_rt_area
  public :: init_opt_rt_coef
  public :: init_rt_vrf
  public :: init_rt_vrf_grid
  public :: init_rt_vrf_raster

  public :: free_rt
  public :: free_rt_main
  public :: free_rt_vrf
  public :: free_rt_vrf_grid
  public :: free_rt_vrf_raster

  public :: clear_rt
  public :: clear_rt_main_data
  public :: clear_rt_vrf_grid
  public :: clear_rt_vrf_raster

  public :: set_default_values_rt
  public :: set_default_values_rt_main
  public :: set_default_values_rt_main_file
  public :: set_default_values_opt_rt_area
  public :: set_default_values_opt_rt_coef
  public :: set_default_values_rt_vrf

  public :: set_endian_rt_main_file
  public :: set_status_rt_main_file
  public :: set_action_rt_main_file
  public :: apply_oldfiles_rt_main_file
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_base'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function init_rt(rt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_rt'
  type(rt_), intent(out) :: rt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  rt%id = ''
  rt%nam = ''
  rt%snam = ''
  rt%tnam = ''

  if( init_rt_main(rt%main) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_rt_vrf(rt%vrf_src) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_rt_vrf(rt%vrf_tgt) /= 0 )then
    info = 1; call errret(); return
  endif

  rt%status = RT_STATUS__MAKE
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_rt
!===============================================================
!
!===============================================================
integer(4) function init_rt_main(rtm) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_rt_main'
  type(rt_main_), intent(out) :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  rtm%id = ''

  rtm%mesh_coef = MESH__NONE
  rtm%mesh_sort = MESH__NONE
  rtm%allow_empty = .true.

  if( init_rt_main_data(rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  rtm%nij = 0_8

  if( init_file_rt_main(rtm%f) /= 0 )then
    info = 1; call errret(); return
  endif

  if( init_opt_rt_area(rtm%opt_area) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_opt_rt_coef(rtm%opt_coef) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_rt_main
!===============================================================
!
!===============================================================
integer(4) function init_rt_main_data(rtm) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_rt_main_data'
  type(rt_main_), intent(inout) :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm%is_sorted_by_sidx = .false.
  rtm%is_sorted_by_tidx = .false.

  rtm%ijsize = 0_8
  nullify(rtm%sidx)
  nullify(rtm%tidx)
  nullify(rtm%area)
  nullify(rtm%coef)

  rtm%sidx_vmin = 0_8
  rtm%sidx_vmax = 0_8
  rtm%tidx_vmin = 0_8
  rtm%tidx_vmax = 0_8
  rtm%area_vmin = 0.d0
  rtm%area_vmax = 0.d0
  rtm%coef_vmin = 0.d0
  rtm%coef_vmax = 0.d0

  rtm%sidx_imin = 0_8
  rtm%sidx_imax = 0_8
  rtm%tidx_imin = 0_8
  rtm%tidx_imax = 0_8
  rtm%area_imin = 0_8
  rtm%area_imax = 0_8
  rtm%coef_imin = 0_8
  rtm%coef_imax = 0_8
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_rt_main_data
!===============================================================
!
!===============================================================
integer(4) function init_file_rt_main(f) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_file_rt_main'
  type(file_rt_main_), intent(out) :: f

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  f%sidx = file('')
  f%tidx = file('')
  f%area = file('')
  f%coef = file('')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_file_rt_main
!===============================================================
!
!===============================================================
integer(4) function init_opt_rt_area(opt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_opt_rt_area'
  type(opt_rt_area_), intent(out) :: opt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  opt%is_ratio_zero_negative_enabled = .true.
  opt%ratio_zero_negative = 0.d0
  opt%allow_le_ratio_zero_negative = .true.
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_opt_rt_area
!===============================================================
!
!===============================================================
integer(4) function init_opt_rt_coef(opt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_opt_rt_coef'
  type(opt_rt_coef_), intent(out) :: opt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  opt%is_sum_modify_enabled = .true.
  opt%sum_modify = 0.d0

  opt%is_sum_modify_ulim_enabled = .true.
  opt%sum_modify_ulim = 0.d0

  opt%is_zero_positive_enabled = .true.
  opt%is_zero_negative_enabled = .true.
  opt%zero_positive = 0.d0
  opt%zero_negative = 0.d0

  opt%is_error_excess_enabled = .true.
  opt%error_excess = 0.d0

  opt%is_sum_error_excess_enabled = .true.
  opt%sum_error_excess = 0.d0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_opt_rt_coef
!===============================================================
!
!===============================================================
integer(4) function init_rt_vrf(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_rt_vrf'
  type(rt_vrf_), intent(out) :: rtv

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  rtv%id = ''

  rtv%idx_miss  = 0_8
  rtv%dval_miss = 0.d0
  rtv%ival_miss = 0_8

  if( init_rt_vrf_grid(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_rt_vrf_raster(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_rt_vrf
!===============================================================
!
!===============================================================
integer(4) function init_rt_vrf_grid(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_rt_vrf_grid'
  type(rt_vrf_), intent(inout) :: rtv

  info = 0

  nullify(rtv%grdidx)
  nullify(rtv%grdara_true)
  nullify(rtv%grdara_rt)
  nullify(rtv%rerr_grdara)
  nullify(rtv%grdnum)
  rtv%grdara_true_min = 0.d0
  rtv%grdara_true_max = 0.d0
  rtv%grdara_rt_min   = 0.d0
  rtv%grdara_rt_max   = 0.d0
  rtv%rerr_grdara_min = 0.d0
  rtv%rerr_grdara_max = 0.d0
  rtv%grdnum_min      = 0_8
  rtv%grdnum_max      = 0_8
  rtv%idx_grdara_true_min = 0_8
  rtv%idx_grdara_true_max = 0_8
  rtv%idx_grdara_rt_min   = 0_8
  rtv%idx_grdara_rt_max   = 0_8
  rtv%idx_rerr_grdara_min = 0_8
  rtv%idx_rerr_grdara_max = 0_8
  rtv%idx_grdnum_min      = 0_8
  rtv%idx_grdnum_max      = 0_8
end function init_rt_vrf_grid
!===============================================================
!
!===============================================================
integer(4) function init_rt_vrf_raster(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_rt_vrf_raster'
  type(rt_vrf_), intent(inout) :: rtv

  info = 0

  nullify(rtv%iarea_sum)
  nullify(rtv%iratio_sum)
end function init_rt_vrf_raster
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
integer(4) function free_rt(rt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_rt'
  type(rt_), intent(inout) :: rt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( free_rt_main(rt%main) /= 0 )then
    info = 1; call errret(); return
  endif
  if( free_rt_vrf(rt%vrf_src) /= 0 )then
    info = 1; call errret(); return
  endif
  if( free_rt_vrf(rt%vrf_tgt) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_rt
!===============================================================
!
!===============================================================
integer(4) function free_rt_main(rtm) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_rt_main'
  type(rt_main_), intent(inout) :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  rtm%is_sorted_by_sidx = .false.
  rtm%is_sorted_by_tidx = .false.
  rtm%ijsize = 0_8
  call realloc(rtm%sidx, 0)
  call realloc(rtm%tidx, 0)
  call realloc(rtm%area, 0)
  call realloc(rtm%coef, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_rt_main
!===============================================================
!
!===============================================================
integer(4) function free_rt_vrf(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_rt_vrf'
  type(rt_vrf_), intent(inout) :: rtv

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( free_rt_vrf_grid(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  if( free_rt_vrf_raster(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_rt_vrf
!===============================================================
!
!===============================================================
integer(4) function free_rt_vrf_grid(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_rt_vrf_grid'
  type(rt_vrf_), intent(inout) :: rtv

  info = 0

  call realloc(rtv%grdidx     , 0)
  call realloc(rtv%grdara_true, 0)
  call realloc(rtv%grdara_rt  , 0)
  call realloc(rtv%rerr_grdara, 0)
  call realloc(rtv%grdnum     , 0)
end function free_rt_vrf_grid
!===============================================================
!
!===============================================================
integer(4) function free_rt_vrf_raster(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_rt_vrf_raster'
  type(rt_vrf_), intent(inout) :: rtv

  info = 0

  call realloc(rtv%iarea_sum , 0)
  call realloc(rtv%iratio_sum, 0)
end function free_rt_vrf_raster
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
integer(4) function clear_rt(rt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_rt'
  type(rt_), intent(inout) :: rt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( free_rt(rt) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_rt(rt) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function clear_rt
!===============================================================
!
!===============================================================
integer(4) function clear_rt_main_data(rtm) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_rt_main_data'
  type(rt_main_), intent(inout) :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( free_rt_main(rtm) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_rt_main(rtm) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function clear_rt_main_data
!===============================================================
!
!===============================================================
integer(4) function clear_rt_vrf_grid(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_rt_vrf_grid'
  type(rt_vrf_), intent(inout) :: rtv

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( free_rt_vrf_grid(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_rt_vrf_grid(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function clear_rt_vrf_grid
!===============================================================
!
!===============================================================
integer(4) function clear_rt_vrf_raster(rtv) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_rt_vrf_raster'
  type(rt_vrf_), intent(inout) :: rtv

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( free_rt_vrf_raster(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  if( init_rt_vrf_raster(rtv) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function clear_rt_vrf_raster
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
integer(4) function set_default_values_rt(&
    rt, &
    status) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_rt'
  type(rt_), intent(inout) :: rt
  character(*), intent(in), optional :: status

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( set_default_values_rt_main(rt%main, rt%id) /= 0 )then
    info = 1; call errret(); return
  endif

  if( present(status) ) rt%status = status

  if( set_default_values_rt_vrf(rt%vrf_src, .true. , rt%id) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_rt_vrf(rt%vrf_tgt, .false., rt%id) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_rt
!===============================================================
!
!===============================================================
integer(4) function set_default_values_rt_main(&
    rtm, &
    id, &
    mode, mesh_coef, mesh_sort, allow_empty, &
    mesh_sorted, nij) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_rt_main'
  type(rt_main_), intent(inout) :: rtm
  character(*), intent(in), optional :: id
  character(*), intent(in), optional :: mode
  character(*), intent(in), optional :: mesh_coef
  character(*), intent(in), optional :: mesh_sort
  logical     , intent(in), optional :: allow_empty
  character(*), intent(in), optional :: mesh_sorted
  integer(8)  , intent(in), optional :: nij

  character(CLEN_VAR) :: id_
  character(CLEN_KEY) :: mode_
  character(CLEN_KEY) :: mesh_coef_
  character(CLEN_KEY) :: mesh_sort_
  logical             :: allow_empty_
  character(CLEN_KEY) :: mesh_sorted_
  integer(8)          :: nij_

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  id_ = 'rt'
  mode_ = REMAP_MODE_1ST_ORDER_CONSERVATIVE
  mesh_coef_ = MESH__TARGET
  mesh_sort_ = MESH__TARGET
  allow_empty_ = .false.
  mesh_sorted_ = MESH__NONE
  nij_ = 0_8

  if( present(id) ) id_ = id
  if( present(mode) ) mode_ = mode
  if( present(mesh_coef) ) mesh_coef_ = mesh_coef
  if( present(mesh_sort) ) mesh_sort_ = mesh_sort
  if( present(allow_empty) ) allow_empty_ = allow_empty
  if( present(mesh_sorted) ) mesh_sorted_ = mesh_sorted
  if( present(nij) ) nij_ = nij

  rtm%id = trim(id_)//'%main'

  rtm%mode = mode_

  rtm%mesh_coef = mesh_coef_
  rtm%mesh_sort = mesh_sort_

  rtm%allow_empty = allow_empty_

  selectcase( mesh_sorted_ )
  case( MESH__SOURCE )
    rtm%is_sorted_by_sidx = .true.
    rtm%is_sorted_by_tidx = .false.
  case( MESH__TARGET )
    rtm%is_sorted_by_sidx = .false.
    rtm%is_sorted_by_tidx = .true.
  case( MESH__NONE )
    rtm%is_sorted_by_sidx = .false.
    rtm%is_sorted_by_tidx = .false.
  case default
    info = 1
    call errret(msg_invalid_value('mesh_sorted', mesh_sorted_))
    return
  endselect

  rtm%ijsize = 0_8
  rtm%nij = nij_

  nullify(rtm%sidx)
  nullify(rtm%tidx)
  nullify(rtm%area)
  nullify(rtm%coef)

  rtm%sidx_vmin = 0_8
  rtm%sidx_vmax = 0_8
  rtm%tidx_vmin = 0_8
  rtm%tidx_vmax = 0_8
  rtm%area_vmin = 0.d0
  rtm%area_vmax = 0.d0
  rtm%coef_vmin = 0.d0
  rtm%coef_vmax = 0.d0

  rtm%sidx_imin = 0_8
  rtm%sidx_imax = 0_8
  rtm%tidx_imin = 0_8
  rtm%tidx_imax = 0_8
  rtm%area_imin = 0_8
  rtm%area_imax = 0_8
  rtm%coef_imin = 0_8
  rtm%coef_imax = 0_8

  if( set_default_values_rt_main_file(rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  if( set_default_values_opt_rt_area(rtm%opt_area) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_opt_rt_coef(rtm%opt_coef) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_rt_main
!===============================================================
!
!===============================================================
integer(4) function set_default_values_rt_main_file(rtm) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_rt_main_file'
  type(rt_main_), intent(inout) :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm%f%sidx = file(dtype=DTYPE_INT4, rec=1, id=trim(rtm%id)//'%f%sidx')
  rtm%f%tidx = file(dtype=DTYPE_INT4, rec=2, id=trim(rtm%id)//'%f%tidx')
  rtm%f%area = file(dtype=DTYPE_DBLE, rec=1, id=trim(rtm%id)//'%f%area')
  rtm%f%coef = file(dtype=DTYPE_DBLE, rec=1, id=trim(rtm%id)//'%f%coef')
  call reset_file_default()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_rt_main_file
!===============================================================
!
!===============================================================
integer(4) function set_default_values_opt_rt_area(opt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_rt_area'
  type(opt_rt_area_), intent(inout) :: opt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  opt%is_ratio_zero_negative_enabled = .true.
  opt%ratio_zero_negative = -1d-16
  opt%allow_le_ratio_zero_negative = .true.
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_opt_rt_area
!===============================================================
!
!===============================================================
integer(4) function set_default_values_opt_rt_coef(opt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_rt_coef'
  type(opt_rt_coef_), intent(inout) :: opt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  opt%is_sum_modify_enabled = .false.
  opt%sum_modify = 0.d0

  opt%is_sum_modify_ulim_enabled = .false.
  opt%sum_modify_ulim = 0.d0

  opt%is_zero_positive_enabled = .false.
  opt%is_zero_negative_enabled = .false.
  opt%zero_positive = 0.d0
  opt%zero_negative = 0.d0

  opt%is_error_excess_enabled = .false.
  opt%error_excess = 0.d0

  opt%is_sum_error_excess_enabled = .false.
  opt%sum_error_excess = 0.d0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_opt_rt_coef
!===============================================================
!
!===============================================================
integer(4) function set_default_values_rt_vrf(rtv, is_source, id_rt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_rt_vrf'
  type(rt_vrf_), intent(inout), target :: rtv
  logical      , intent(in)            :: is_source
  character(*) , intent(in)            :: id_rt

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_source )then
    rtv%id = trim(id_rt)//'%vrf_src'
  else
    rtv%id = trim(id_rt)//'%vrf_tgt'
  endif

  rtv%idx_miss  = idx_miss_default
  rtv%dval_miss = dval_miss_default
  rtv%ival_miss = ival_miss_default

  rtv%f%id = trim(rtv%id)//'%f'
  call set_file_default(action=ACTION_WRITE)
  rtv%f%out_grdidx      = file(dtype=DTYPE_INT4, id=trim(rtv%f%id)//'%out_grdidx')
  rtv%f%out_grdara_true = file(dtype=DTYPE_DBLE, id=trim(rtv%f%id)//'%out_grdara_true')
  rtv%f%out_grdara_rt   = file(dtype=DTYPE_DBLE, id=trim(rtv%f%id)//'%out_grdara_rt')
  rtv%f%out_rerr_grdara = file(dtype=DTYPE_DBLE, id=trim(rtv%f%id)//'%out_rerr_grdara')
  rtv%f%out_grdnum      = file(dtype=DTYPE_INT4, id=trim(rtv%f%id)//'%out_grdnum')
  rtv%f%out_iarea_sum   = file(dtype=DTYPE_DBLE, id=trim(rtv%f%id)//'%out_iarea_sum')
  rtv%f%out_iratio_sum  = file(dtype=DTYPE_DBLE, id=trim(rtv%f%id)//'%out_iratio_sum')
  call reset_file_default()

  nullify(rtv%grdidx)
  nullify(rtv%grdara_true)
  nullify(rtv%grdara_rt)
  nullify(rtv%rerr_grdara)
  nullify(rtv%grdnum)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_rt_vrf
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
integer(4) function set_endian_rt_main_file(&
    f_rtm, endian) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_endian_rt_main_file'
  type(file_rt_main_), intent(inout) :: f_rtm
  character(*), intent(in) :: endian

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_rtm%sidx%endian = endian
  f_rtm%tidx%endian = endian
  f_rtm%area%endian = endian
  f_rtm%coef%endian = endian
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_endian_rt_main_file
!===============================================================
!
!===============================================================
integer(4) function set_status_rt_main_file(&
    f_rtm, status) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_status_rt_main_file'
  type(file_rt_main_), intent(inout) :: f_rtm
  character(*), intent(in) :: status

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_rtm%sidx%status = status
  f_rtm%tidx%status = status
  f_rtm%area%status = status
  f_rtm%coef%status = status
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_status_rt_main_file
!===============================================================
!
!===============================================================
integer(4) function set_action_rt_main_file(&
    f_rtm, action) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_action_rt_main_file'
  type(file_rt_main_), intent(inout) :: f_rtm
  character(*), intent(in) :: action

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_rtm%sidx%action = action
  f_rtm%tidx%action = action
  f_rtm%area%action = action
  f_rtm%coef%action = action
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_action_rt_main_file
!===============================================================
!
!===============================================================
integer(4) function apply_oldfiles_rt_main_file(&
    rt, opt_old_files) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'apply_oldfiles_rt_main_file'
  type(rt_), intent(inout), target :: rt
  character(*), intent(in), optional :: opt_old_files

  type(file_rt_main_), pointer :: f_rtm
  character(CLEN_KEY) :: status, action

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rt%status )
  case( RT_STATUS__MAKE )
    selectcase( opt_old_files )
    case( OPT_OLD_FILES_STOP )
      status = STATUS_NEW
      action = ACTION_WRITE
    case( OPT_OLD_FILES_REMOVE )
      status = STATUS_REPLACE
      action = ACTION_READWRITE
    case( OPT_OLD_FILES_OVERWRITE )
      status = STATUS_UNKNOWN
      action = ACTION_READWRITE
    case default
      call errret(msg_invalid_value('opt_old_files', opt_old_files))
    endselect
  case( RT_STATUS__READ )
    status = STATUS_OLD
    action = ACTION_READ
  case default
    call errret(msg_invalid_value('rt%status', rt%status))
  endselect

  f_rtm => rt%main%f

  f_rtm%sidx%status = status
  f_rtm%tidx%status = status
  f_rtm%area%status = status
  f_rtm%coef%status = status

  f_rtm%sidx%action = action
  f_rtm%tidx%action = action
  f_rtm%area%action = action
  f_rtm%coef%action = action

  nullify(f_rtm)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function apply_oldfiles_rt_main_file
!===============================================================
!
!===============================================================
end module c2_rt_base
