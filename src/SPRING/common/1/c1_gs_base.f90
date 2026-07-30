module c1_gs_base
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  use c1_const
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_mesh
  public :: init_mesh_latlon
  public :: init_mesh_raster
  public :: init_mesh_polygon
  public :: init_mesh_raster_zone

  public :: alloc_file_grid_in_val
  public :: alloc_file_grid_out_val

  public :: set_bounds_file_latlon_in
  public :: set_bounds_file_raster_in
  public :: set_bounds_file_polygon_in

  public :: set_bounds_file_grid_in
  public :: set_bounds_file_grid_out

  public :: set_miss_file_grid_in
  public :: set_miss_file_grid_out
  public :: set_save_file_grid_out

  public :: set_mesh_common

  public :: clear_mesh
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_gs_base'
  !-------------------------------------------------------------
contains
!===============================================================
! Status of meshes
!   0: Undefined
!   1: Initialized
!   2: Modified
!
! Initialize
!   0 -> 1
! Put any value
!   [1,2] -> 2
! Clear
!   [2,1,0] -> 0
!===============================================================
!
!
!
!
!
!===============================================================
! Status "0. Undefined" -> "1. Initialized"
!===============================================================
integer(4) function init_mesh(a) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mesh'
  type(gs_), intent(out) :: a

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%status )
  case( MESH_STATUS__UNDEFINED )
    continue
  case( MESH_STATUS__INITIALIZED )
    call logmsg('Function `'//str(PRCNAM)//'` was called '//&
                'but the mesh has already been initialized. Nothing to do.'//&
               '\n  id : '//str(a%id)//&
               '\n  nam: '//str(a%nam))
    call logret(PRCNAM, MODNAM)
    return
  case( MESH_STATUS__MODIFIED )
    info = 1
    call errret(msg_unexpected_condition()//&
              '\n  a%status == MESH_STATUS__MODIFIED'//&
              '\nThe mesh has been modified.'//&
              '\n  id : '//str(a%id)//&
              '\n  nam: '//str(a%nam))
    return
  endselect
  a%status = MESH_STATUS__INITIALIZED
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: a%id)
  allocate(character(1) :: a%nam)
  a%id = ''
  a%nam = ''
  a%is_valid = .false.
  a%typ = MESHTYPE__UNSPEC
  a%is_source = .true.

  nullify(a%latlon)
  nullify(a%raster)
  nullify(a%polygon)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_mesh
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
!integer(4) function init_mesh_component(a, meshtype) result(info)
!  implicit none
!  type(gs_)   , intent(inout) :: a
!  character(*), intent(in)    :: meshtype
!
!  info = 0
!  call logbgn(PRCNAM, MODNAM, '-p')
!  !-------------------------------------------------------------
!  !
!  !-------------------------------------------------------------
!  selectcase( meshtype )
!  case( MESHTYPE__LATLON )
!    call init_mesh_latlon(a)
!  case( MESHTYPE__RASTER )
!    call init_mesh_raster(a)
!  case( MESHTYPE__POLYGON )
!    call init_mesh_polygon(a)
!  case default
!    info = 1
!    call errret(msg_invalid_value('meshtype', meshtype)//&
!              '\n  a%id: '//str(a%id)//&
!              '\n  a%nam: '//str(a%nam))
!    return
!  endselect
!  !-------------------------------------------------------------
!  call logret(PRCNAM, MODNAM)
!end function init_mesh_component
!===============================================================
!
!===============================================================
integer(4) function init_mesh_latlon(a) result(info)
  use c1_gs_grid_base, only: &
        init_grid
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mesh_latlon'
  type(gs_), intent(inout), target :: a

  type(gs_latlon_)     , pointer :: al
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  ! For avoiding maybe-uninitialized-error
  character(:), allocatable :: c

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  a%typ = MESHTYPE__LATLON

  allocate(a%latlon)
  al => a%latlon

  allocate(character(1) :: c)
  c = a%id

  allocate(character(1) :: al%id)
  al%id = c//'%latlon'
  al%nam => a%nam
  al%is_valid => a%is_valid
  al%is_source => a%is_source

  allocate(al%f_latlon_in)
  allocate(al%f_grid_in)
  allocate(al%f_grid_out)

  fl     => al%f_latlon_in
  fg_in  => al%f_grid_in
  fg_out => al%f_grid_out

  c = al%id
  allocate(character(1) :: al%grid%id, fl%id, fg_in%id, fg_out%id)
  al%grid%id = c//'%grid'
  fl%id      = c//'%f_latlon_in'
  fg_in%id   = c//'%f_grid_in'
  fg_out%id  = c//'%f_grid_out'

  if( set_default_values_file_latlon_in(fl) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_file_grid_in(fg_in) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_file_grid_out(fg_out) /= 0 )then
    info = 1; call errret(); return
  endif

  al%nij = 0_8
  if( init_grid(al%grid) /= 0 )then
    info = 1; call errret(); return
  endif

  al%nx = 0_8
  al%ny = 0_8
  al%nh = 0_8
  al%hi = 0_8
  al%hf = 0_8
  al%nv = 0_8
  al%vi = 0_8
  al%vf = 0_8

  al%west  = 0.d0
  al%east  = 0.d0
  al%south = 0.d0
  al%north = 0.d0
  al%is_cyclic = .true.
  al%is_south_to_north = .true.
  al%region_type = REGION_TYPE_UNDEF
  al%coord_unit = UNIT_DEGREE

  nullify(al%lon)
  nullify(al%lat)
  nullify(al%lonwidth)
  nullify(al%latwidth)
  nullify(al%lon0)

  nullify(al%idxmap)
  nullify(al%mskmap)
  nullify(al%wgtmap)
  al%status_idxmap = GRID_STATUS__TO_BE_PREPARED
  al%status_mskmap = GRID_STATUS__TO_BE_PREPARED
  al%status_wgtmap = GRID_STATUS__TO_BE_PREPARED
  al%idxmin = 0_8
  al%idxmax = 0_8

  al%idx_miss    = IDX_MISS_DEFAULT
  al%uwa_miss    = UWA_MISS_DEFAULT
  al%ara_miss    = ARA_MISS_DEFAULT
  al%wgt_miss    = WGT_MISS_DEFAULT
  al%xyz_miss    = XYZ_MISS_DEFAULT
  al%lonlat_miss = LONLAT_MISS_DEFAULT
  al%val_miss    = DVAL_MISS_DEFAULT

  nullify(al%hrel)
  nullify(al%vrel)

  al%debug = .false.
  al%idx_debug = IDX_MISS_DEFAULT

  deallocate(c)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_mesh_latlon
!===============================================================
!
!===============================================================
integer(4) function init_mesh_raster(a) result(info)
  use c1_gs_grid_base, only: &
        init_grid
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mesh_raster'
  type(gs_), intent(inout), target :: a

  type(gs_raster_)     , pointer :: ar
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  ! For avoiding maybe-uninitialized-error
  character(:), allocatable :: c

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  a%typ = MESHTYPE__RASTER

  allocate(a%raster)
  ar => a%raster

  allocate(character(1) :: c)
  c = a%id

  allocate(character(1) :: ar%id)
  ar%id = c//'%raster'
  ar%nam => a%nam
  ar%is_valid => a%is_valid
  ar%is_source => a%is_source

  allocate(ar%f_raster_in)
  allocate(ar%f_grid_in)
  allocate(ar%f_grid_out)

  fr     => ar%f_raster_in
  fg_in  => ar%f_grid_in
  fg_out => ar%f_grid_out

  c = ar%id
  ar%grid%id = c//'%grid'
  fr%id      = c//'%f_raster_in'
  fg_in%id   = c//'%f_grid_in'
  fg_out%id  = c//'%f_grid_out'

  if( set_default_values_file_raster_in(fr) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_file_grid_in(fg_in) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_file_grid_out(fg_out) /= 0 )then
    info = 1; call errret(); return
  endif

  ar%nij = 0_8
  if( init_grid(ar%grid) /= 0 )then
    info = 1; call errret(); return
  endif

  ar%nx = 0_8
  ar%ny = 0_8
  ar%xi = 0_8
  ar%xf = 0_8
  ar%yi = 0_8
  ar%yf = 0_8

  ar%nh = 0_8
  ar%hi = 0_8
  ar%hf = 0_8
  ar%nv = 0_8
  ar%vi = 0_8
  ar%vf = 0_8

  ar%west  = -1.8d2
  ar%east  =  1.8d2
  ar%south = -9.d1
  ar%north =  9.d1
  ar%is_cyclic = .true.
  ar%is_south_to_north = .true.
  ar%region_type = REGION_TYPE_UNDEF

  nullify(ar%lon)
  nullify(ar%lat)
  nullify(ar%lonwidth)
  nullify(ar%latwidth)
  nullify(ar%lon0)

  ar%nZone = 0
  nullify(ar%zone)
  ar%status_idxmap = GRID_STATUS__TO_BE_PREPARED
  ar%status_mskmap = GRID_STATUS__TO_BE_PREPARED
  ar%status_wgtmap = GRID_STATUS__TO_BE_PREPARED
  ar%idxmin = 0_8
  ar%idxmax = 0_8
  ar%idx_condition = IDX_CONDITION__MATCH

  ar%idx_miss    = IDX_MISS_DEFAULT
  ar%uwa_miss    = UWA_MISS_DEFAULT
  ar%ara_miss    = ARA_MISS_DEFAULT
  ar%wgt_miss    = WGT_MISS_DEFAULT
  ar%xyz_miss    = XYZ_MISS_DEFAULT
  ar%lonlat_miss = LONLAT_MISS_DEFAULT
  ar%val_miss    = DVAL_MISS_DEFAULT

  nullify(ar%hrel)
  nullify(ar%vrel)

  ar%debug = .false.
  ar%idx_debug = IDX_MISS_DEFAULT

  deallocate(c)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_mesh_raster
!===============================================================
!
!===============================================================
integer(4) function init_mesh_polygon(a) result(info)
  use c1_gs_grid_base, only: &
        init_grid
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mesh_polygon'
  type(gs_), intent(inout), target :: a

  type(gs_polygon_)     , pointer :: ap
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out

  ! For avoiding maybe-uninitialized-error
  character(:), allocatable :: c

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  a%typ = MESHTYPE__POLYGON

  allocate(a%polygon)
  ap => a%polygon

  allocate(character(1) :: c)
  c = a%id

  allocate(character(1) :: ap%id)
  ap%id = c//'%polygon'
  ap%nam => a%nam
  ap%is_valid => a%is_valid
  ap%is_source => a%is_source

  allocate(ap%f_polygon_in)
  allocate(ap%f_grid_in)
  allocate(ap%f_grid_out)

  fp     => ap%f_polygon_in
  fg_in  => ap%f_grid_in
  fg_out => ap%f_grid_out

  c = ap%id
  allocate(character(1) :: ap%grid%id, fp%id, fg_in%id, fg_out%id)
  ap%grid%id = c//'%grid'
  fp%id      = c//'%f_polygon_in'
  fg_in%id   = c//'%f_grid_in'
  fg_out%id  = c//'%f_grid_out'

  if( set_default_values_file_polygon_in(fp) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_file_grid_in(fg_in) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_default_values_file_grid_out(fg_out) /= 0 )then
    info = 1; call errret(); return
  endif

  ap%np = 0_8
  ap%nij = 0_8
  ap%ijs = 0_8
  ap%ije = 0_8

  if( init_grid(ap%grid) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(ap%polygon)

  ap%coord_sys  = ''
  ap%coord_unit = ''
  ap%coord_miss_s = COORD_MISS_S_DEFAULT
  ap%coord_miss_c = COORD_MISS_C_DEFAULT

  ap%allow_duplicated_vertex = .true.
  ap%arc_parallel = .false.

  ap%idxmin = 0_8
  ap%idxmax = 0_8

  ap%idx_miss    = IDX_MISS_DEFAULT
  ap%uwa_miss    = UWA_MISS_DEFAULT
  ap%ara_miss    = ARA_MISS_DEFAULT
  ap%wgt_miss    = WGT_MISS_DEFAULT
  ap%xyz_miss    = XYZ_MISS_DEFAULT
  ap%lonlat_miss = LONLAT_MISS_DEFAULT
  ap%val_miss    = DVAL_MISS_DEFAULT

  ap%debug = .false.
  ap%idx_debug = IDX_MISS_DEFAULT

  deallocate(c)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_mesh_polygon
!===============================================================
!
!===============================================================
integer(4) function init_mesh_raster_zone(arz) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mesh_raster_zone'
  type(raster_zone_), intent(inout) :: arz

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  nullify(arz%idxmap)
  nullify(arz%mskmap)
  nullify(arz%wgtmap)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_mesh_raster_zone
!===============================================================
!
!===============================================================
integer(4) function set_default_values_file_grid_in(fg) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_file_grid_in'
  type(file_grid_in_), intent(inout) :: fg

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call set_file_default(action=ACTION_READ)
  fg%idx = file(dtype=DTYPE_INT4, id=trim(fg%id)//'%idx')
  fg%ara = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%ara')
  fg%wgt = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%wgt')
  fg%x   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%x'  )
  fg%y   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%y'  )
  fg%z   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%z'  )
  fg%lon = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lon')
  fg%lat = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lat')
  call reset_file_default()

  fg%nFiles_val = 0
  nullify(fg%val)

  allocate(fg%sz(FILEDIM))
  allocate(fg%lb(FILEDIM))
  allocate(fg%ub(FILEDIM))
  fg%sz(:) = 0_8
  fg%lb(:) = 0_8
  fg%ub(:) = 0_8

  fg%nx  = 0_8
  fg%ny  = 0_8
  fg%nij = 0_8

  fg%idx_bgn = 1_8

  ! Missing values are updated by $gs
  fg%idx_miss    = 0_8
  fg%ara_miss    = 0.d0
  fg%wgt_miss    = 0.d0
  fg%xyz_miss    = 0.d0
  fg%lonlat_miss = 0.d0
  fg%val_miss    = 0.d0

  fg%unit_ara    = UNIT_SQUARE_METER
  fg%unit_xyz    = UNIT_METER
  fg%unit_lonlat = UNIT_DEGREE
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_file_grid_in
!===============================================================
!
!===============================================================
integer(4) function set_default_values_file_grid_out(fg) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_file_grid_out'
  type(file_grid_out_), intent(inout) :: fg

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  fg%form = GRID_FORM_AUTO

  fg%save_idx    = .false.
  fg%save_msk    = .false.
  fg%save_uwa    = .false.
  fg%save_ara    = .false.
  fg%save_wgt    = .false.
  fg%save_xyz    = .false.
  fg%save_lonlat = .false.

  call set_file_default(action=ACTION_WRITE)
  fg%idx = file(dtype=DTYPE_INT4, id=trim(fg%id)//'%idx')
  fg%msk = file(dtype=DTYPE_INT4, id=trim(fg%id)//'%msk')
  fg%uwa = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%uwa')
  fg%ara = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%ara')
  fg%wgt = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%wgt')
  fg%x   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%x'  )
  fg%y   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%y'  )
  fg%z   = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%z'  )
  fg%lon = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lon')
  fg%lat = file(dtype=DTYPE_DBLE, id=trim(fg%id)//'%lat')
  call reset_file_default()

  fg%nFiles_val = 0
  nullify(fg%val)

  allocate(fg%sz(filedim))
  allocate(fg%lb(filedim))
  allocate(fg%ub(filedim))
  fg%sz(:) = 0_8
  fg%lb(:) = 0_8
  fg%ub(:) = 0_8

  fg%nx  = 0_8
  fg%ny  = 0_8
  fg%nij = 0_8

  fg%idxmin = 0_8
  fg%idxmax = 0_8

  ! Missing values are updated by $gs
  fg%idx_miss    = 0_8
  fg%uwa_miss    = 0.d0
  fg%ara_miss    = 0.d0
  fg%wgt_miss    = 0.d0
  fg%xyz_miss    = 0.d0
  fg%lonlat_miss = 0.d0
  fg%val_miss    = 0.d0

  fg%unit_ara    = UNIT_SQUARE_METER
  fg%unit_xyz    = UNIT_METER
  fg%unit_lonlat = UNIT_DEGREE
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_file_grid_out
!===============================================================
!
!===============================================================
integer(4) function set_default_values_file_latlon_in(fl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_file_latlon_in'
  type(file_latlon_in_), intent(inout) :: fl

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call set_file_default(action=ACTION_READ)
  fl%lon = file(dtype=DTYPE_DBLE, id=trim(fl%id)//'%lon')
  fl%lat = file(dtype=DTYPE_DBLE, id=trim(fl%id)//'%lat')
  call reset_file_default()

  allocate(fl%sz(FILEDIM))
  allocate(fl%lb(FILEDIM))
  allocate(fl%ub(FILEDIM))
  fl%sz(:) = 0_8
  fl%lb(:) = 0_8
  fl%ub(:) = 0_8
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_file_latlon_in
!===============================================================
!
!===============================================================
integer(4) function set_default_values_file_raster_in(fr) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_file_raster_in'
  type(file_raster_in_), intent(inout) :: fr

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call set_file_default(action=ACTION_READ)
  fr%idx = file(dtype=DTYPE_INT4, id=trim(fr%id)//'%idx')
  fr%ara = file(dtype=DTYPE_DBLE, id=trim(fr%id)//'%ara')
  fr%wgt = file(dtype=DTYPE_DBLE, id=trim(fr%id)//'%wgt')
  call reset_file_default()

  allocate(fr%sz(FILEDIM))
  allocate(fr%lb(FILEDIM))
  allocate(fr%ub(FILEDIM))
  fr%sz(:) = 0_8
  fr%lb(:) = 0_8
  fr%ub(:) = 0_8

  fr%unit_ara = UNIT_SQUARE_METER
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_file_raster_in
!===============================================================
!
!===============================================================
integer(4) function set_default_values_file_polygon_in(fp) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_file_polygon_in'
  type(file_polygon_in_), intent(inout) :: fp

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call set_file_default(action=ACTION_READ)
  fp%x      = file(dtype=DTYPE_DBLE, id=trim(fp%id)//'%x')
  fp%y      = file(dtype=DTYPE_DBLE, id=trim(fp%id)//'%y')
  fp%z      = file(dtype=DTYPE_DBLE, id=trim(fp%id)//'%z')
  fp%lon    = file(dtype=DTYPE_DBLE, id=trim(fp%id)//'%lon')
  fp%lat    = file(dtype=DTYPE_DBLE, id=trim(fp%id)//'%lat')
  fp%arctyp = file(dtype=DTYPE_INT4, id=trim(fp%id)//'%arctyp')
  call reset_file_default()

  allocate(fp%sz(FILEDIM))
  allocate(fp%lb(FILEDIM))
  allocate(fp%ub(FILEDIM))
  fp%sz(:) = 0_8
  fp%lb(:) = 0_8
  fp%ub(:) = 0_8
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_default_values_file_polygon_in
!===============================================================
!
!===============================================================
integer(4) function alloc_file_grid_in_val(fg) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'alloc_file_grid_in_val'
  type(file_grid_in_), intent(inout) :: fg

  type(file_), pointer :: f
  integer :: iFile

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  allocate(fg%val(fg%nFiles_val))

  do iFile = 1, fg%nFiles_val
    f => fg%val(iFile)
    f = file('', DTYPE_DBLE, ENDIAN_DEFAULT, 1, &
             id=trim(fg%id)//'%val('//str(iFile)//')', &
             action=ACTION_READ)

    f%sz = fg%sz
    f%lb = fg%lb
    f%ub = fg%ub
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function alloc_file_grid_in_val
!===============================================================
!
!===============================================================
integer(4) function alloc_file_grid_out_val(fg) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'alloc_file_grid_out_val'
  type(file_grid_out_), intent(inout) :: fg

  type(file_), pointer :: f
  integer :: iFile

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  allocate(fg%val(fg%nFiles_val))

  do iFile = 1, fg%nFiles_val
    f => fg%val(iFile)
    f = file('', DTYPE_DBLE, ENDIAN_DEFAULT, 1, &
             id=trim(fg%id)//'%val('//str(iFile)//')', &
             action=ACTION_WRITE)

    f%sz = fg%sz
    f%lb = fg%lb
    f%ub = fg%ub
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function alloc_file_grid_out_val
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
integer(4) function set_bounds_file_grid_in(fg, sz1, sz2) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_bounds_file_grid_in'
  type(file_grid_in_), intent(inout), target :: fg
  integer(8), intent(in), optional :: sz1, sz2

  type(file_), pointer :: f

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( present(sz1) )then
    if( fg%sz(1) == 0_8 ) fg%sz(1) = sz1
    if( fg%sz(2) == 0_8 ) fg%sz(2) = sz2
  endif

  if( fg%lb(1) == 0_8 ) fg%lb(1) = 1_8
  if( fg%lb(2) == 0_8 ) fg%lb(2) = 1_8

  if( fg%ub(1) == 0_8 ) fg%ub(1) = fg%sz(1)
  if( fg%ub(2) == 0_8 ) fg%ub(2) = fg%sz(2)

  fg%nx = fg%ub(1) - fg%lb(1) + 1_8
  fg%ny = fg%ub(2) - fg%lb(2) + 1_8
  fg%nij = fg%nx * fg%ny

  f => fg%idx
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%ara
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%wgt
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_bounds_file_grid_in
!===============================================================
!
!===============================================================
integer(4) function set_bounds_file_grid_out(fg, sz1, sz2) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_bounds_file_grid_out'
  type(file_grid_out_), intent(inout), target :: fg
  integer(8), intent(in), optional ::sz1, sz2

  type(file_), pointer :: f

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( present(sz1) )then
    if( fg%sz(1) == 0_8 ) fg%sz(1) = sz1
    if( fg%sz(2) == 0_8 ) fg%sz(2) = sz2
  endif

  if( fg%lb(1) == 0_8 ) fg%lb(1) = 1_8
  if( fg%lb(2) == 0_8 ) fg%lb(2) = 1_8

  if( fg%ub(1) == 0_8 ) fg%ub(1) = fg%sz(1)
  if( fg%ub(2) == 0_8 ) fg%ub(2) = fg%sz(2)

  fg%nx = fg%ub(1) - fg%lb(1) + 1_8
  fg%ny = fg%ub(2) - fg%lb(2) + 1_8
  fg%nij = fg%nx * fg%ny

  f => fg%idx
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%ara
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%wgt
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%x
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%y
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%z
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%lon
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub

  f => fg%lat
  f%sz = fg%sz
  f%lb = fg%lb
  f%ub = fg%ub
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_bounds_file_grid_out
!===============================================================
!
!===============================================================
integer(4) function set_bounds_file_latlon_in(&
    fl,                    & ! inout
    nx, ny,                & ! in
    nh, hi, hf, nv, vi, vf & ! out
  ) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_bounds_file_latlon_in'
  type(file_latlon_in_), intent(inout), target :: fl
  integer(8), intent(in)  :: nx, ny
  integer(8), intent(out) :: nh, hi, hf, nv, vi, vf

  type(file_), pointer :: f

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  nh = nx
  hi = 1_8
  hf = nx
  nv = ny
  vi = 1_8
  vf = ny

  if( fl%sz(1) == 0_8 ) fl%sz(1) = nx
  if( fl%sz(2) == 0_8 ) fl%sz(2) = ny

  if( fl%lb(1) == 0_8 ) fl%lb(1) = 1_8
  if( fl%lb(2) == 0_8 ) fl%lb(2) = 1_8

  if( fl%ub(1) == 0_8 ) fl%ub(1) = fl%lb(1) + nx - 1_8
  if( fl%ub(2) == 0_8 ) fl%ub(2) = fl%lb(2) + ny - 1_8

  f => fl%lon
  f%sz(:) = fl%sz(:)
  f%lb(:) = fl%lb(:)
  f%ub(:) = fl%ub(:)

  f => fl%lat
  f%sz(:) = fl%sz(:)
  f%lb(:) = fl%lb(:)
  f%ub(:) = fl%ub(:)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_bounds_file_latlon_in
!===============================================================
!
!===============================================================
integer(4) function set_bounds_file_raster_in(&
    fr,                        & ! inout
    nx, ny, is_south_to_north, & ! in
    xi, xf, yi, yf,            & ! inout
    nh, hi, hf, nv, vi, vf     & ! out
  ) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_bounds_file_raster_in'
  type(file_raster_in_), intent(inout), target :: fr
  integer(8), intent(in)    :: nx, ny
  logical   , intent(in)    :: is_south_to_north
  integer(8), intent(inout) :: xi, xf, yi, yf
  integer(8), intent(out)   :: nh, hi, hf, &
                               nv, vi, vf

  type(file_), pointer :: f
  integer :: dgt_xy

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( xi == 0_8 ) xi = 1_8
  if( xf == 0_8 ) xf = nx
  if( yi == 0_8 ) yi = 1_8
  if( yf == 0_8 ) yf = ny

  nh = nx
  nv = ny
  hi = xi
  hf = xf
  if( is_south_to_north )then
    vi = yi
    vf = yf
  else
    vi = ny - yf + 1_8
    vf = ny - yi + 1_8
  endif

  if( fr%sz(1) == 0_8 ) fr%sz(1) = nx
  if( fr%sz(2) == 0_8 ) fr%sz(2) = ny
  if( fr%lb(1) == 0_8 ) fr%lb(1) = xi
  if( fr%lb(2) == 0_8 ) fr%lb(2) = yi
  if( fr%ub(1) == 0_8 ) fr%ub(1) = xf
  if( fr%ub(2) == 0_8 ) fr%ub(2) = yf
  !-------------------------------------------------------------
  if( fr%ub(1) - fr%lb(1) + 1_8 /= xf - xi + 1_8 .or. &
      fr%ub(2) - fr%lb(2) + 1_8 /= yf - yi + 1_8 )then
    info = 1
    dgt_xy = max(dgt(fr%sz(:2),dgt_opt_max), dgt(max(nx,ny)))
    call errret(msg_unexpected_condition()//&
              '\nShape of input and that of grid system mismatch.'//&
              '\ninput x: '//str(fr%ub(1)-fr%lb(1)+1_8)//&
                ' ('//str((/fr%lb(1),fr%ub(1)/),dgt_xy,':')//' in '//str(fr%sz(1),dgt_xy)//')'//&
              '\n      y: '//str(fr%ub(2)-fr%lb(2)+1_8)//&
                ' ('//str((/fr%lb(2),fr%ub(2)/),dgt_xy,':')//' in '//str(fr%sz(2),dgt_xy)//')'//&
              '\ngs    x: '//str(xf-xi+1_8)//&
                ' ('//str((/xi,xf/),dgt_xy,':')//' in '//str(nx,dgt_xy)//')'//&
              '\ngs    y: '//str(yf-yi+1_8)//&
                ' ('//str((/yi,yf/),dgt_xy,':')//' in '//str(ny,dgt_xy)//')')
    return
  endif
  !-------------------------------------------------------------
  f => fr%idx
  f%sz(:) = fr%sz(:)
  f%lb(:) = fr%lb(:)
  f%ub(:) = fr%ub(:)

  f => fr%ara
  f%sz(:) = fr%sz(:)
  f%lb(:) = fr%lb(:)
  f%ub(:) = fr%ub(:)

  f => fr%wgt
  f%sz(:) = fr%sz(:)
  f%lb(:) = fr%lb(:)
  f%ub(:) = fr%ub(:)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_bounds_file_raster_in
!===============================================================
!
!===============================================================
integer(4) function set_bounds_file_polygon_in(&
    fp, ijs, ije, np, nij) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_bounds_file_polygon_in'
  type(file_polygon_in_), intent(inout), target :: fp
  integer(8), intent(inout) :: ijs, ije
  integer(8), intent(in) :: np
  integer(8), intent(in) :: nij

  type(file_), pointer :: f

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( ijs == 0_8 ) ijs = 1_8
  if( ije == 0_8 ) ije = nij

  fp%sz(1) = np
  fp%lb(1) = 1_8
  fp%ub(1) = np

  if( fp%sz(2) == 0_8 ) fp%sz(2) = nij
  if( fp%lb(2) == 0_8 ) fp%lb(2) = 1_8
  if( fp%ub(2) == 0_8 ) fp%ub(2) = fp%lb(2) + nij - 1_8

  f => fp%x
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%y
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%z
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%lon
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%lat
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)

  f => fp%arctyp
  f%sz(:) = fp%sz(:)
  f%lb(:) = fp%lb(:)
  f%ub(:) = fp%ub(:)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_bounds_file_polygon_in
!===============================================================
!
!===============================================================
integer(4) function set_miss_file_grid_in(&
    fg_in,                           &
    idx_miss, ara_miss   , wgt_miss, &
    xyz_miss, lonlat_miss, val_miss  &
  ) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_miss_file_grid_in'
  type(file_grid_in_), intent(inout) :: fg_in
  integer(8), intent(in) :: idx_miss
  real(8)   , intent(in) :: ara_miss
  real(8)   , intent(in) :: wgt_miss
  real(8)   , intent(in) :: xyz_miss
  real(8)   , intent(in) :: lonlat_miss
  real(8)   , intent(in) :: val_miss

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  fg_in%idx_miss    = idx_miss
  fg_in%ara_miss    = ara_miss
  fg_in%wgt_miss    = wgt_miss
  fg_in%xyz_miss    = xyz_miss
  fg_in%lonlat_miss = lonlat_miss
  fg_in%val_miss    = val_miss
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_miss_file_grid_in
!===============================================================
!
!===============================================================
integer(4) function set_miss_file_grid_out(&
    fg_out,                          &
    idx_miss, ara_miss   , wgt_miss, &
    xyz_miss, lonlat_miss, val_miss  &
  ) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_miss_file_grid_out'
  type(file_grid_out_), intent(inout) :: fg_out
  integer(8), intent(in) :: idx_miss
  real(8)   , intent(in) :: ara_miss
  real(8)   , intent(in) :: wgt_miss
  real(8)   , intent(in) :: xyz_miss
  real(8)   , intent(in) :: lonlat_miss
  real(8)   , intent(in) :: val_miss

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  fg_out%idx_miss    = idx_miss
  fg_out%uwa_miss    = ara_miss
  fg_out%ara_miss    = ara_miss
  fg_out%wgt_miss    = wgt_miss
  fg_out%xyz_miss    = xyz_miss
  fg_out%lonlat_miss = lonlat_miss
  fg_out%val_miss    = val_miss
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_miss_file_grid_out
!===============================================================
! Public
!===============================================================
integer(4) function set_save_file_grid_out(fg_out) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_save_file_grid_out'
  type(file_grid_out_), intent(inout) :: fg_out

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out%save_idx    = fg_out%idx%path /= ''
  fg_out%save_msk    = fg_out%msk%path /= ''
  fg_out%save_ara    = fg_out%ara%path /= ''
  fg_out%save_wgt    = fg_out%wgt%path /= ''
  fg_out%save_xyz    = fg_out%x%path /= '' .or. fg_out%y%path /= '' .or. fg_out%z%path /= ''
  fg_out%save_lonlat = fg_out%lon%path /= '' .or. fg_out%lat%path /= ''
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_save_file_grid_out
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Requirements:
!   1. init_mesh_(meshtype) has been called.
!===============================================================
integer(4) function set_mesh_common(a) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_mesh_common'
  type(gs_), intent(inout), target :: a

  type(gs_common_) , pointer :: ac
  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap

  character(:), allocatable :: c

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(a%cmn)
  ac => a%cmn

  allocate(character(1) :: c)
  c = a%id

  allocate(character(1) :: ac%id)
  ac%id = c//'%cmn'
  ac%nam => a%nam
  ac%is_valid  => a%is_valid
  ac%typ       => a%typ
  ac%is_source => a%is_source

  selectcase( a%typ )
  case( MESHTYPE__LATLON )
    al => a%latlon
    ac%f_grid_in   => al%f_grid_in
    ac%f_grid_out  => al%f_grid_out
    ac%grid        => al%grid
    ac%idx_miss    => al%idx_miss
    ac%ara_miss    => al%ara_miss
    ac%wgt_miss    => al%wgt_miss
    ac%xyz_miss    => al%xyz_miss
    ac%lonlat_miss => al%lonlat_miss
    ac%val_miss    => al%val_miss
    ac%debug       => al%debug
    ac%idx_debug   => al%idx_debug
  case( MESHTYPE__RASTER )
    ar => a%raster
    ac%f_grid_in   => ar%f_grid_in
    ac%f_grid_out  => ar%f_grid_out
    ac%grid        => ar%grid
    ac%idx_miss    => ar%idx_miss
    ac%ara_miss    => ar%ara_miss
    ac%wgt_miss    => ar%wgt_miss
    ac%xyz_miss    => ar%xyz_miss
    ac%lonlat_miss => ar%lonlat_miss
    ac%val_miss    => ar%val_miss
    ac%debug       => ar%debug
    ac%idx_debug   => ar%idx_debug
  case( MESHTYPE__POLYGON )
    ap => a%polygon
    ac%f_grid_in   => ap%f_grid_in
    ac%f_grid_out  => ap%f_grid_out
    ac%grid        => ap%grid
    ac%idx_miss    => ap%idx_miss
    ac%ara_miss    => ap%ara_miss
    ac%wgt_miss    => ap%wgt_miss
    ac%xyz_miss    => ap%xyz_miss
    ac%lonlat_miss => ap%lonlat_miss
    ac%val_miss    => ap%val_miss
    ac%debug       => ap%debug
    ac%idx_debug   => ap%idx_debug
  case default
    info = 1
    call errret(msg_invalid_value('meshtype', a%typ)//&
              '\n  a%id : '//str(a%id)//&
              '\n  a%nam: '//str(a%nam))
    return
  endselect

  nullify(ac)

  deallocate(c)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_mesh_common
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Status "2. Modified"    -> "0. Undefined"
!     or "1. Initialized" -> "0. Undefined"
!===============================================================
integer(4) function clear_mesh(a) result(info)
  use c1_gs_grid_base, only: &
        free_grid
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_mesh'
  type(gs_), intent(inout), target :: a

  type(gs_common_) , pointer :: ac
  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap
  type(file_latlon_in_) , pointer :: fl
  type(file_raster_in_) , pointer :: fr
  type(file_polygon_in_), pointer :: fp
  integer :: iz

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%status )
  case( MESH_STATUS__UNDEFINED )
    call logmsg('Function `'//str(PRCNAM)//'` was called '//&
                'but the mesh is undefined. Nothing to do.'//&
               '\n  id : '//str(a%id)//&
               '\n  nam: '//str(a%nam))
    call logret(PRCNAM, MODNAM)
    return
  case( MESH_STATUS__INITIALIZED )
    continue
  case( MESH_STATUS__MODIFIED )
    continue
  case default
    info = 1
    call errret(msg_invalid_value('a%status', a%status)//&
              '\n  id : '//str(a%id)//&
              '\n  nam: '//str(a%nam))
    return
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ac => a%cmn

  deallocate(ac%id)
  nullify(ac%nam)
  nullify(ac%is_valid)
  nullify(ac%typ)
  nullify(ac%is_source)
  nullify(ac%f_grid_in)
  nullify(ac%f_grid_out)
  nullify(ac%grid)
  nullify(ac%idx_miss)
  nullify(ac%ara_miss)
  nullify(ac%wgt_miss)
  nullify(ac%xyz_miss)
  nullify(ac%lonlat_miss)
  nullify(ac%val_miss)
  nullify(ac%debug)
  nullify(ac%idx_debug)

  nullify(ac)
  nullify(a%cmn)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%typ )
  !-------------------------------------------------------------
  ! Case: LatLon
  case( MESHTYPE__LATLON )
    al => a%latlon

    deallocate(al%id)
    nullify(al%nam)

    if( associated(al%f_latlon_in) )then
      fl => al%f_latlon_in
      deallocate(fl%sz, fl%lb, fl%ub)
      nullify(fl)
      nullify(al%f_latlon_in)
    endif

    if( associated(al%f_grid_in) )then
      if( free_file_grid_in(al%f_grid_in) /= 0 )then
        info = 1; call errret(); return
      endif
      nullify(al%f_grid_in)
    endif

    if( associated(al%f_grid_out) )then
      if( free_file_grid_out(al%f_grid_out) /= 0 )then
        info = 1; call errret(); return
      endif
      nullify(al%f_grid_out)
    endif

    if( free_grid(al%grid) /= 0 )then
      info = 1; call errret(); return
    endif

    if( associated(al%lon) )then
      deallocate(al%lon, al%lat, al%lonwidth, al%latwidth, al%lon0)
      nullify(al%lon, al%lat, al%lonwidth, al%latwidth, al%lon0)
    endif

    if( associated(al%idxmap) ) deallocate(al%idxmap)
    if( associated(al%mskmap) ) deallocate(al%mskmap)
    if( associated(al%wgtmap) ) deallocate(al%wgtmap)
    al%status_idxmap = GRID_STATUS__TO_BE_PREPARED
    al%status_mskmap = GRID_STATUS__TO_BE_PREPARED
    al%status_wgtmap = GRID_STATUS__TO_BE_PREPARED

    if( associated(al%hrel) )then
      deallocate(al%hrel, al%vrel)
      nullify(al%hrel, al%vrel)
    endif

    nullify(al)
    nullify(a%latlon)
  !-------------------------------------------------------------
  ! Case: Raster
  case( MESHTYPE__RASTER )
    ar => a%raster

    deallocate(ar%id)
    nullify(ar%nam)

    if( associated(ar%f_raster_in) )then
      fr => ar%f_raster_in
      deallocate(fr%sz, fr%lb, fr%ub)
      nullify(fr)
      nullify(ar%f_raster_in)
    endif

    if( associated(ar%f_grid_in) )then
      if( free_file_grid_in(ar%f_grid_in) /= 0 )then
        info = 1; call errret(); return
      endif
      nullify(ar%f_grid_in)
    endif

    if( associated(ar%f_grid_out) )then
      if( free_file_grid_out(ar%f_grid_out) /= 0 )then
        info = 1; call errret(); return
      endif
      nullify(ar%f_grid_out)
    endif

    if( free_grid(ar%grid) /= 0 )then
      info = 1; call errret(); return
    endif

    if( associated(ar%lon) )then
      deallocate(ar%lon, ar%lat, ar%lonwidth, ar%latwidth, ar%lon0)
      nullify(ar%lon, ar%lat, ar%lonwidth, ar%latwidth, ar%lon0)
    endif

    do iz = 1, ar%nZone
      if( free_gs_raster_zone(ar%zone(iz)) /= 0 )then
        info = 1; call errret(); return
      endif
    enddo
    if( ar%nZone > 0 )then
      deallocate(ar%zone)
      nullify(ar%zone)
    endif
    ar%nZone = 0

    ar%status_idxmap = GRID_STATUS__TO_BE_PREPARED
    ar%status_mskmap = GRID_STATUS__TO_BE_PREPARED
    ar%status_wgtmap = GRID_STATUS__TO_BE_PREPARED

    if( associated(ar%hrel) )then
      deallocate(ar%hrel, ar%vrel)
      nullify(ar%hrel, ar%vrel)
    endif

    nullify(ar)
    nullify(a%raster)
  !-------------------------------------------------------------
  ! Case: Polygon
  case( MESHTYPE__POLYGON )
    ap => a%polygon

    deallocate(ap%id)
    nullify(ap%nam)

    if( associated(ap%f_polygon_in) )then
      fp => ap%f_polygon_in
      deallocate(fp%sz, fp%lb, fp%ub)
      nullify(fp)
      nullify(ap%f_polygon_in)
    endif

    if( associated(ap%f_grid_in) )then
      if( free_file_grid_in(ap%f_grid_in) /= 0 )then
        info = 1; call errret(); return
      endif
      nullify(ap%f_grid_in)
    endif

    if( associated(ap%f_grid_out) )then
      if( free_file_grid_out(ap%f_grid_out) /= 0 )then
        info = 1; call errret(); return
      endif
      nullify(ap%f_grid_out)
    endif

    if( free_grid(ap%grid) /= 0 )then
      info = 1; call errret(); return
    endif

    if( associated(ap%polygon) )then
      deallocate(ap%polygon)
      nullify(ap%polygon)
    endif

    nullify(ap)
    nullify(a%polygon)
  !-------------------------------------------------------------
  ! Case: Unspecified
  case( MESHTYPE__UNSPEC )
    continue
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('a%typ', a%typ))
    return
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  a%status = MESH_STATUS__UNDEFINED
  deallocate(a%id)
  deallocate(a%nam)
  a%is_valid = .false.
  a%typ = MESHTYPE__UNDEF
  a%is_source = .true.
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function clear_mesh
!===============================================================
!
!===============================================================
integer(4) function free_gs_raster_zone(arz) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_gs_raster_zone'
  type(raster_zone_), intent(inout) :: arz

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(arz%idxmap, 0)
  call realloc(arz%mskmap, 0)
  call realloc(arz%wgtmap, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_gs_raster_zone
!===============================================================
!
!===============================================================
integer(4) function free_file_grid_in(fg) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_file_grid_in'
  type(file_grid_in_), intent(inout) :: fg

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( fg%nFiles_val > 0 )then
    deallocate(fg%val)
    fg%nFiles_val = 0
  endif

  deallocate(fg%sz, fg%lb, fg%ub)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_file_grid_in
!===============================================================
!
!===============================================================
integer(4) function free_file_grid_out(fg) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_file_grid_out'
  type(file_grid_out_), intent(inout) :: fg

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( fg%nFiles_val > 0 )then
    deallocate(fg%val)
    fg%nFiles_val = 0
  endif

  deallocate(fg%sz, fg%lb, fg%ub)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_file_grid_out
!===============================================================
!
!===============================================================
end module c1_gs_base
