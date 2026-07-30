module c1_const
  use lib_const
  implicit none
  public
  !-------------------------------------------------------------
  ! Mesh
  !-------------------------------------------------------------
  integer, parameter :: MESH_STATUS__UNDEFINED   = 0
  integer, parameter :: MESH_STATUS__INITIALIZED = 1
  integer, parameter :: MESH_STATUS__MODIFIED    = 2

  character(CLEN_KEY), parameter :: MESHTYPE__LATLON  = 'latlon'
  character(CLEN_KEY), parameter :: MESHTYPE__RASTER  = 'raster'
  character(CLEN_KEY), parameter :: MESHTYPE__POLYGON = 'polygon'
  character(CLEN_KEY), parameter :: MESHTYPE__UNSPEC  = 'unspecified'
  character(CLEN_KEY), parameter :: MESHTYPE__UNDEF   = 'undefined'

  character(CLEN_KEY), parameter :: MESH__NONE   = 'none'
  character(CLEN_KEY), parameter :: MESH__SOURCE = 'source'
  character(CLEN_KEY), parameter :: MESH__TARGET = 'target'

  integer(8), parameter :: IDX_MISS_DEFAULT    = -9999_8
  real(8)   , parameter :: UWA_MISS_DEFAULT    = -1d20
  real(8)   , parameter :: ARA_MISS_DEFAULT    = -1d20
  real(8)   , parameter :: WGT_MISS_DEFAULT    = -1d20
  real(8)   , parameter :: XYZ_MISS_DEFAULT    = -1d20
  real(8)   , parameter :: LONLAT_MISS_DEFAULT = -1d20
  real(8)   , parameter :: DVAL_MISS_DEFAULT   = -1d20
  integer(8), parameter :: IVAL_MISS_DEFAULT   = -9999_8

  real(8), parameter :: COORD_MISS_C_DEFAULT = -1d20
  real(8), parameter :: COORD_MISS_S_DEFAULT = -1d20

  character(CLEN_KEY), parameter :: ORIGIN_SOUTH = 'south'
  character(CLEN_KEY), parameter :: ORIGIN_NORTH = 'north'

  character(CLEN_KEY), parameter :: GRID_STATUS__UNDEF          = 'undef'
  character(CLEN_KEY), parameter :: GRID_STATUS__PREPARED       = 'prepared'
  character(CLEN_KEY), parameter :: GRID_STATUS__TO_BE_PREPARED = 'to_be_prepared'
  character(CLEN_KEY), parameter :: GRID_STATUS__NOT_USED       = 'not_uesd'

  character(CLEN_KEY), parameter :: IDX_CONDITION__UNDEF      = 'undef'
  character(CLEN_KEY), parameter :: IDX_CONDITION__MATCH      = 'match'
  character(CLEN_KEY), parameter :: IDX_CONDITION__GRD_IN_RST = 'grid_in_raster'
  character(CLEN_KEY), parameter :: IDX_CONDITION__RST_IN_GRD = 'raster_in_grid'
  character(CLEN_KEY), parameter :: IDX_CONDITION__NONE       = 'none'

  integer(4), parameter :: INPUTFORM__NOT_GIVEN = -9
  integer(4), parameter :: INPUTFORM__FILE_BIN = 0
  integer(4), parameter :: INPUTFORM__INT1 = 1
  integer(4), parameter :: INPUTFORM__INT2 = 2
  integer(4), parameter :: INPUTFORM__INT4 = 4
  integer(4), parameter :: INPUTFORM__INT8 = 8
  integer(4), parameter :: INPUTFORM__REAL = 8
  integer(4), parameter :: INPUTFORM__DBLE = 8

!  integer, parameter :: rec_im_idx = 1
!  integer, parameter :: rec_im_msk = 2
!  integer, parameter :: rec_im_uwa = 3
!  integer, parameter :: rec_im_ara = 4
!  integer, parameter :: rec_im_wgt = 5
!  integer, parameter :: rec_im_x   = 6
!  integer, parameter :: rec_im_y   = 7
!  integer, parameter :: rec_im_z   = 8
!  integer, parameter :: rec_im_lon = 9
!  integer, parameter :: rec_im_lat = 10

  character(CLEN_VAR), parameter :: VARNAME_IDX = 'idx'
  character(CLEN_VAR), parameter :: VARNAME_MSK = 'msk'
  character(CLEN_VAR), parameter :: VARNAME_UWA = 'uwa'
  character(CLEN_VAR), parameter :: VARNAME_ARA = 'ara'
  character(CLEN_VAR), parameter :: VARNAME_WGT = 'wgt'
  character(CLEN_VAR), parameter :: VARNAME_X   = 'x'
  character(CLEN_VAR), parameter :: VARNAME_Y   = 'y'
  character(CLEN_VAR), parameter :: VARNAME_Z   = 'z'
  character(CLEN_VAR), parameter :: VARNAME_LON = 'lon'
  character(CLEN_VAR), parameter :: VARNAME_LAT = 'lat'

  character(CLEN_VAR), parameter :: VARNAME_GRDIDX    = 'grdidx'
  character(CLEN_VAR), parameter :: VARNAME_GRDMSK    = 'grdmsk'
  character(CLEN_VAR), parameter :: VARNAME_GRDUWA    = 'grduwa'
  character(CLEN_VAR), parameter :: VARNAME_GRDARA    = 'grdara'
  character(CLEN_VAR), parameter :: VARNAME_GRDWGT    = 'grdwgt'
  character(CLEN_VAR), parameter :: VARNAME_GRDX      = 'grdx'
  character(CLEN_VAR), parameter :: VARNAME_GRDY      = 'grdy'
  character(CLEN_VAR), parameter :: VARNAME_GRDZ      = 'grdz'
  character(CLEN_VAR), parameter :: VARNAME_GRDXYZ    = 'grdxyz'
  character(CLEN_VAR), parameter :: VARNAME_GRDLON    = 'grdlon'
  character(CLEN_VAR), parameter :: VARNAME_GRDLAT    = 'grdlat'
  character(CLEN_VAR), parameter :: VARNAME_GRDLONLAT = 'grdlonlat'

  character(CLEN_VAR), parameter :: VARNAME_RSTIDX    = 'rstidx'
  character(CLEN_VAR), parameter :: VARNAME_RSTARA    = 'rstara'
  character(CLEN_VAR), parameter :: VARNAME_RSTWGT    = 'rstwgt'

  character(CLEN_VAR), parameter :: VARNAME_IDXMAP    = 'idxmap'
  character(CLEN_VAR), parameter :: VARNAME_ARAMAP    = 'aramap'
  character(CLEN_VAR), parameter :: VARNAME_WGTMAP    = 'wgtmap'

  character(CLEN_VAR), parameter :: VARNAME_POLYGON   = 'polygon'
  !-------------------------------------------------------------
  ! Rasterization
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: VARNAME_IAREA_SUM  = 'iarea_sum'
  character(CLEN_VAR), parameter :: VARNAME_IRATIO_SUM = 'iratio_sum'
  character(CLEN_VAR), parameter :: VARNAME_MASK       = 'mask'
  !character(CLEN_VAR), parameter :: VARNAME_IDX      = 'idx'
  !-------------------------------------------------------------
  ! Remapping
  !-------------------------------------------------------------
  character(CLEN_KEY), parameter :: REMAP_MODE_1ST_ORDER_CONSERVATIVE = '1st_order_conservative'
  !-------------------------------------------------------------
  ! Remapping table
  !-------------------------------------------------------------
  integer(8), parameter :: RTM_IJSIZE_INIT = 1024_8
  integer(8), parameter :: RT1D_IJSIZE_INIT = 8_8
  real(8)   , parameter :: RT1D_EXTEND_RATE = 2.d0

  character(CLEN_KEY), parameter :: RT_STATUS__UNDEF  = 'undef'
  character(CLEN_KEY), parameter :: RT_STATUS__MAKE   = 'make'
  character(CLEN_KEY), parameter :: RT_STATUS__READ   = 'read'
  character(CLEN_KEY), parameter :: RT_STATUS__NONE   = 'none'

  character(CLEN_VAR), parameter :: GRID_FORM_AUTO   = 'auto'
  character(CLEN_VAR), parameter :: GRID_FORM_INDEX  = 'index'
  character(CLEN_VAR), parameter :: GRID_FORM_RASTER = 'raster'

  !character(CLEN_VAR), parameter :: VARNAME_GRDIDX      = 'grdidx'
  character(CLEN_VAR), parameter :: VARNAME_GRDARA_TRUE = 'grdara_true'
  character(CLEN_VAR), parameter :: VARNAME_GRDARA_RT   = 'grdara_rt'
  character(CLEN_VAR), parameter :: VARNAME_RERR_GRDARA = 'rerr_grdara'
  character(CLEN_VAR), parameter :: VARNAME_GRDNUM      = 'grdnum'
  integer, parameter :: CL_VARNAME_VRF = 11
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: OPT_OLD_FILES_STOP      = 'stop'
  character(CLEN_VAR), parameter :: OPT_OLD_FILES_REMOVE    = 'remove'
  character(CLEN_VAR), parameter :: OPT_OLD_FILES_OVERWRITE = 'overwrite'

  character(CLEN_KEY), parameter :: INPUT_OPT_IDX_DUP_SUM  = 'sum'
  character(CLEN_KEY), parameter :: INPUT_OPT_IDX_DUP_STOP = 'stop'
  !-----------------------------------------------------------
  ! Formatting
  !-----------------------------------------------------------
  character(CLEN_WFMT), parameter :: WFMT_MEM = 'es10.3'

  character(2), parameter :: HUT_COMMAND = '+ '
end module c1_const
