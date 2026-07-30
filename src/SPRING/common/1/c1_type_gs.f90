module c1_type_gs
  use lib_const
  use lib_io
  use c1_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Grid system
  !-------------------------------------------------------------
  public :: file_grid_in_
  public :: file_grid_out_
  public :: grid_

  public :: file_latlon_in_
  public :: file_raster_in_
  public :: file_polygon_in_

  public :: raster_zone_

  public :: polygon_

  public :: hrel_
  public :: vrel_

  public :: gs_latlon_
  public :: gs_raster_
  public :: gs_polygon_
  public :: gs_common_
  public :: gs_
  !-------------------------------------------------------------
  ! Grid system
  !-------------------------------------------------------------

  ! Grid
  !-------------------------------------------------------------
  type file_grid_in_
    character(:), allocatable :: id

    type(file_) :: idx
    type(file_) :: ara
    type(file_) :: wgt
    type(file_) :: x, y, z
    type(file_) :: lon, lat

    integer :: nFiles_val
    type(file_), pointer :: val(:) !(nFiles_val)

    integer(8), pointer :: sz(:), lb(:), ub(:)
    integer(8) :: nx, ny
    integer(8) :: nij

    integer(8) :: idx_bgn

    integer(8) :: idx_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss
    character(CLEN_KEY) :: unit_ara
    character(CLEN_KEY) :: unit_xyz
    character(CLEN_KEY) :: unit_lonlat
  end type

  type file_grid_out_
    character(:), allocatable :: id
    character(CLEN_KEY) :: form

    logical :: save_idx
    logical :: save_msk
    logical :: save_uwa
    logical :: save_ara
    logical :: save_wgt
    logical :: save_xyz
    logical :: save_lonlat

    type(file_) :: idx
    type(file_) :: msk
    type(file_) :: uwa
    type(file_) :: ara
    type(file_) :: wgt
    type(file_) :: x, y, z
    type(file_) :: lon, lat
    integer :: nFiles_val
    type(file_), pointer :: val(:) !(nFiles_val)

    integer(8), pointer :: sz(:), lb(:), ub(:)
    integer(8) :: nx, ny
    integer(8) :: nij

    integer(8) :: idxmin, idxmax

    integer(8) :: idx_miss
    real(8)    :: uwa_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss

    character(CLEN_KEY) :: unit_ara
    character(CLEN_KEY) :: unit_xyz
    character(CLEN_KEY) :: unit_lonlat
  end type

  type grid_
    character(:), allocatable :: id
    integer(8) :: nij
    integer(8) :: idxmin, idxmax
    integer(8), pointer :: idx(:)
    integer(8), pointer :: idxarg(:)
    logical(1), pointer :: msk(:)
    real(8)   , pointer :: uwa(:)  ! unweighted area
    real(8)   , pointer :: ara(:)  ! weighted area
    real(8)   , pointer :: wgt(:)  ! weight
    real(8)   , pointer :: x(:), y(:), z(:)
    real(8)   , pointer :: lon(:), lat(:)
    character(CLEN_KEY) :: status_idx, status_msk, &
                           status_uwa, status_ara, status_wgt, &
                           status_xyz, status_lonlat
    integer(8) :: ij_debug
  end type

  ! File of gs
  !-------------------------------------------------------------
  type file_latlon_in_
    character(:), allocatable :: id
    type(file_) :: lon
    type(file_) :: lat
    integer(8), pointer :: sz(:), lb(:), ub(:)
  end type

  type file_raster_in_
    character(:), allocatable :: id
    type(file_) :: idx
    type(file_) :: ara
    type(file_) :: wgt
    integer(8), pointer :: sz(:), lb(:), ub(:)
    character(CLEN_KEY) :: unit_ara
  end type

  type file_polygon_in_
    character(:), allocatable :: id
    type(file_) :: x, y, z
    type(file_) :: lon, lat
    type(file_) :: arctyp
    integer(8), pointer :: sz(:), lb(:), ub(:)
  end type

  ! Zone of raster
  !-------------------------------------------------------------
  type raster_zone_
    logical(4) :: is_valid
    integer(1) :: region_type
    integer(8) :: nx, xi, xf, ny, yi, yf
    integer(8) :: nh, hi, hf, nv, vi, vf
    real(8) :: west , east   ! lon(hi-1:hf)
    real(8) :: south, north  ! lat(vi-1:vf)
    integer(8), pointer :: idxmap(:,:)
    logical(1), pointer :: mskmap(:,:)
    real(8)   , pointer :: wgtmap(:,:)
    integer(8) :: idxmin, idxmax
  end type

  ! Polygon
  !-------------------------------------------------------------
  type polygon_
    integer(8) :: idx
    real(8)    :: val
    integer(4) :: n
    real(8), pointer :: lon(:), lat(:)
    real(8), pointer :: x(:), y(:), z(:)
    real(8)    :: west, east, south, north
    integer(4) :: n_west, n_east
    integer(4) :: n_pole
    integer(1) :: pos
    integer(1), pointer :: arctyp(:)
    integer(1), pointer :: arcpos(:)
    real(8)   , pointer :: a(:), b(:), c(:)
    integer(1), pointer :: convex(:)
    real(8)   , pointer :: lontop(:), lattop(:)
  end type

  ! Relations of latlon bounds
  !-------------------------------------------------------------
  type hrel_
    integer          :: nr
    integer(8)       :: hi(2), hf(2), mh
    real(8), pointer :: west(:), east(:)  !(mh)
    real(8), pointer :: lonwidth(:)       !(mh)
  end type

  type vrel_
    integer(8)       :: vi, vf, mv
    real(8), pointer :: south(:), north(:)  !(mv)
    real(8), pointer :: latwidth(:)         !(mv)
    real(8), pointer :: lapara_1rad(:)      !(mv)
  end type

  ! Grid system
  !-------------------------------------------------------------
  type gs_latlon_
    character(:), allocatable :: id
    character(:), pointer :: nam ! => gs_%nam
    logical(4)  , pointer :: is_valid  !=> gs_%is_valid

    type(file_latlon_in_), pointer :: f_latlon_in

    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out

    integer(8)  :: nij
    type(grid_) :: grid

    integer(8) :: nx, ny
    integer(8) :: nh, hi, hf, nv, vi, vf
    real(8)    :: west, east, south, north
    logical(4) :: is_cyclic
    logical(4) :: is_south_to_north
    integer(1) :: region_type
    character(CLEN_KEY) :: coord_unit

    real(8)   , pointer  :: lon(:)  !(hi-1:hf)
    real(8)   , pointer  :: lat(:)  !(vi-1:vf)
    real(8)   , pointer  :: lonwidth(:)  !(hi:hf)
    real(8)   , pointer  :: latwidth(:)  !(vi:vf)
    logical(4), pointer  :: lon0(:)

    integer(8), pointer :: idxmap(:,:) !(hi:hf,vi:vf)
    logical(1), pointer :: mskmap(:,:)
    real(8)   , pointer :: wgtmap(:,:)
    character(CLEN_KEY) :: status_idxmap, &
                           status_mskmap, &
                           status_wgtmap
    integer(8) :: idxmin, idxmax

    integer(8) :: idx_miss
    real(8)    :: uwa_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss

    ! For making remapping tables
    logical(4) , pointer :: is_source
    type(hrel_), pointer :: hrel(:)
    type(vrel_), pointer :: vrel(:)

    ! For debugging
    logical(4) :: debug
    integer(8) :: idx_debug
  end type

  type gs_raster_
    character(:), allocatable :: id
    character(:), pointer :: nam ! => gs_%nam
    logical(4)  , pointer :: is_valid  !=> gs_%is_valid

    type(file_raster_in_), pointer :: f_raster_in

    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out

    integer(8) :: nij
    type(grid_) :: grid

    integer(8) :: nx, xi, xf, ny, yi, yf
    integer(8) :: nh, hi, hf, nv, vi, vf
    real(8)    :: west, east, south, north
    logical(4) :: is_cyclic
    logical(4) :: is_south_to_north
    integer(1) :: region_type

    real(8)   , pointer :: lon(:)  !(hi-1:hf)
    real(8)   , pointer :: lat(:)  !(vi-1:vf)
    real(8)   , pointer :: lonwidth(:)  !(hi:hf)
    real(8)   , pointer :: latwidth(:)  !(vi:vf)
    logical(4), pointer :: lon0(:)

    integer :: nZone
    type(raster_zone_), pointer :: zone(:)
    character(CLEN_KEY) :: status_idxmap, &
                           status_mskmap, &
                           status_wgtmap
    integer(8) :: idxmin, idxmax
    character(CLEN_KEY) :: idx_condition

    integer(8) :: idx_miss
    real(8)    :: ara_miss
    real(8)    :: uwa_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss

    ! For making regmapping tables
    logical(4) , pointer :: is_source
    type(hrel_), pointer :: hrel(:)
    type(vrel_), pointer :: vrel(:)

    ! For debugging
    logical(4) :: debug
    integer(8) :: idx_debug
  end type

  type gs_polygon_
    character(:), allocatable :: id
    character(:), pointer :: nam ! => gs_%nam
    logical(4)  , pointer :: is_valid  !=> gs_%is_valid

    type(file_polygon_in_), pointer :: f_polygon_in

    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out

    integer(8) :: np
    integer(8) :: nij
    integer(8) :: ijs, ije

    type(grid_) :: grid
    type(polygon_), pointer :: polygon(:)

    character(CLEN_KEY) :: coord_sys
    character(CLEN_KEY) :: coord_unit
    real(8) :: coord_miss_s  ! spherical
    real(8) :: coord_miss_c  ! cartesian

    logical(4) :: allow_duplicated_vertex
    logical(4) :: arc_parallel

    integer(8) :: idxmin, idxmax

    integer(8) :: idx_miss
    real(8)    :: uwa_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss
    real(8)    :: xyz_miss
    real(8)    :: lonlat_miss
    real(8)    :: val_miss

    ! For making remapping tables
    logical(4), pointer :: is_source

    ! For debugging
    logical(4) :: debug
    integer(8) :: idx_debug
  end type

  type gs_common_
    character(:), allocatable :: id
    character(:)       , pointer :: nam
    logical(4)         , pointer :: is_valid
    character(CLEN_KEY), pointer :: typ
    logical(4)         , pointer :: is_source
    type(file_grid_in_) , pointer :: f_grid_in
    type(file_grid_out_), pointer :: f_grid_out
    type(grid_)         , pointer :: grid
    integer(8), pointer :: idx_miss
    real(8)   , pointer :: ara_miss
    real(8)   , pointer :: wgt_miss
    real(8)   , pointer :: xyz_miss
    real(8)   , pointer :: lonlat_miss
    real(8)   , pointer :: val_miss
    logical(4), pointer :: debug
    integer(8), pointer :: idx_debug
  end type

  type gs_
    integer :: status = MESH_STATUS__UNDEFINED
    character(:), allocatable :: id
    character(:), allocatable :: nam
    logical(4)          :: is_valid  = .false.
    character(CLEN_KEY) :: typ       = MESHTYPE__UNDEF
    logical(4)          :: is_source = .true.
    type(gs_latlon_) , pointer :: latlon
    type(gs_raster_) , pointer :: raster
    type(gs_polygon_), pointer :: polygon
    type(gs_common_) , pointer :: cmn
  end type
end module c1_type_gs
