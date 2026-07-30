module ls_gs
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize

  public :: define_mesh_latlon
  public :: define_mesh_raster
  public :: point_mesh
  public :: clear_mesh

  public :: spring_define_mesh_latlon
  public :: spring_define_mesh_raster
  public :: spring_clear_mesh

  public :: spring_print_meshes_name
  public :: spring_print_mesh
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'ls_gs'

  ! List of meshes
  !-------------------------------------------------------------
  type(gs_), allocatable, target :: lst_gs(:)
  integer :: nmax_gs = 0
  logical :: is_initialized = .false.

  ! Missing values
  !-------------------------------------------------------------
  integer(1), parameter :: IDX1_MISS_DEFAULT = -99_1
  integer(2), parameter :: IDX2_MISS_DEFAULT = -999_2
  integer(4), parameter :: IDX4_MISS_DEFAULT = -9999
  integer(8), parameter :: IDX8_MISS_DEFAULT = -9999_8
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function initialize(size_lst_gs) result(info)
  use c1_opt_ctrl, only: &
        set_opt_sys, &
        set_opt_log, &
        set_opt_earth
  use c1_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log, &
        set_default_values_opt_earth
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'initialize'
  integer, intent(in) :: size_lst_gs

  type(opt_sys_)   :: opt_sys
  type(opt_log_)   :: opt_log
  type(opt_earth_) :: opt_earth
  integer :: i

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .false.) /= 0 )then
    info = 1; call errret(); return
  endif
  is_initialized = .true.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(lst_gs(size_lst_gs))
  do i = 1, size(lst_gs)
    lst_gs(i)%nam = ''
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_default_values_opt_sys(opt_sys)
  call set_default_values_opt_log(opt_log)
  call set_default_values_opt_earth(opt_earth)

  call set_opt_sys(opt_sys)
  call set_opt_log(opt_log)
  call set_opt_earth(opt_earth)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function initialize
!===============================================================
!
!===============================================================
integer(4) function finalize() result(info)
  use c1_gs_base, only: &
        clear_mesh
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize'

  integer :: i

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif
  is_initialized = .false.

  do i = 1, size(lst_gs)
    if( lst_gs(i)%nam == '' ) cycle
    if( clear_mesh(lst_gs(i)) /= 0 )then
      info = 1; call errret(); return
    endif
  enddo
  deallocate(lst_gs)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function finalize
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
integer(4) function define_mesh_latlon(&
    name, nx, ny, &
    west, east, south, north, &
    form_lon, form_lat, lon, lat, &
    path_lon, dtype_lon, endian_lon, rec_lon, &
    path_lat, dtype_lat, endian_lat, rec_lat, &
    form_idx, idx1, idx2, idx4, idx8, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    idx_miss, &
    origin) result(info)
  use c1_gs_base, only: &
        init_mesh_latlon         , &
        set_mesh_common          , &
        set_bounds_file_latlon_in, &
        set_bounds_file_grid_in  , &
        set_bounds_file_grid_out
  use c1_gs_define, only: &
        set_gs
  use c1_gs_grid_core, only: &
        make_idxmap, &
        make_grdidx
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'define_mesh_latlon'
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: west, east, south, north
  integer, intent(in) :: form_lon, form_lat
  real(8), intent(in) :: lon(:), lat(:)
  character(*), intent(in) :: path_lon
  character(*), intent(in) :: dtype_lon
  character(*), intent(in) :: endian_lon
  integer(4)  , intent(in) :: rec_lon
  character(*), intent(in) :: path_lat
  character(*), intent(in) :: dtype_lat
  character(*), intent(in) :: endian_lat
  integer(4)  , intent(in) :: rec_lat
  integer(4), intent(in) :: form_idx
  integer(1), intent(in) :: idx1(:,:)
  integer(2), intent(in) :: idx2(:,:)
  integer(4), intent(in) :: idx4(:,:)
  integer(8), intent(in) :: idx8(:,:)
  character(*), intent(in) :: path_idx
  character(*), intent(in) :: dtype_idx
  character(*), intent(in) :: endian_idx
  integer(4)  , intent(in) :: rec_idx
  integer(8), intent(in) :: idx_miss
  character(*), intent(in) :: origin

  type(gs_)            , pointer :: a
  type(gs_latlon_)     , pointer :: al
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  integer :: i_gs

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  ! Find an empty sloat for a grid system
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(a)
  do i_gs = 1, size(lst_gs)
    if( lst_gs(i_gs)%nam == '' )then
      a => lst_gs(i_gs)
      nmax_gs = max(i_gs, nmax_gs)
      exit
    endif
  enddo

  if( .not. associated(a) )then
    info = 1
    call errret('No slot for grid system is left.')
    return
  endif
  !-------------------------------------------------------------
  ! Check the inputs
  !-------------------------------------------------------------
  call logent('Checking the inputs', PRCNAM, MODNAM)

  selectcase( form_lon )
  case( INPUTFORM__DBLE )
    if( size(lon) /= nx+1 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(lon): ('//str(size(lon))//')'//&
                '\n  nx        : '//str(nx)//&
                '\nSize of $lon must be equal to $nx+1.')
      return
    endif
  endselect

  selectcase( form_lat )
  case( INPUTFORM__DBLE )
    if( size(lat) /= ny+1 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(lat): ('//str(size(lat))//')'//&
                '\n  ny        : '//str(ny)//&
                '\nSize of $lat must be equal to $ny+1.')
      return
    endif
  endselect

  call logext()
  !-------------------------------------------------------------
  ! Set the default values and pointers
  !-------------------------------------------------------------
  call logent('Setting the default values and pointers', PRCNAM, MODNAM)

  a%id = 'gs'//str(i_gs)
  a%nam = trim(name)

  if( init_mesh_latlon(a) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_mesh_common(a) /= 0 )then
    info = 1; call errret(); return
  endif

  al => a%latlon
  fl => al%f_latlon_in
  fg_in => al%f_grid_in

  call logext()
  !-------------------------------------------------------------
  ! Check the shape of raster index map
  !-------------------------------------------------------------
  call logent('Checking the shape of raster index map', PRCNAM, MODNAM)

  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    if( check_shape_2d('idx1', shape(idx1), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT2 )
    if( check_shape_2d('idx2', shape(idx2), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT4 )
    if( check_shape_2d('idx4', shape(idx4), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT8 )
    if( check_shape_2d('idx8', shape(idx8), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__FILE_BIN, &
        INPUTFORM__NOT_GIVEN )
    continue
  case default
    info = 1
    call errret(msg_invalid_value('form_idx', form_idx))
    return
  endselect

  call logext()
  !-------------------------------------------------------------
  ! Set some of the given values
  !-------------------------------------------------------------
  call logent('Setting some of the given values', PRCNAM, MODNAM)

  al%nx = nx
  al%ny = ny

  al%west = west
  al%east = east
  al%south = south
  al%north = north

  selectcase( form_lon )
  case( INPUTFORM__DBLE, &
        INPUTFORM__NOT_GIVEN )
    continue
  case( INPUTFORM__FILE_BIN )
    fl%lon = file(path_lon, dtype_lon, endian_lon, rec_lon)
  case default
    info = 1
    call errret(msg_invalid_value('form_lon', form_lon))
    return
  endselect

  selectcase( form_lat )
  case( INPUTFORM__DBLE, &
        INPUTFORM__NOT_GIVEN )
    continue
  case( INPUTFORM__FILE_BIN )
    fl%lat = file(path_lat, dtype_lat, endian_lat, rec_lat)
  case default
    info = 1
    call errret(msg_invalid_value('form_lat', form_lat))
    return
  endselect

  al%idx_miss = idx_miss

  selectcase( origin )
  case( ORIGIN_SOUTH )
    al%is_south_to_north = .true.
  case( ORIGIN_NORTH )
    al%is_south_to_north = .false.
  case default
    info = 1
    call errret(msg_invalid_value('origin', origin))
    return
  endselect

  call logext()
  !-------------------------------------------------------------
  ! Set bounds. of grid data
  !-------------------------------------------------------------
  call logent('Setting bounds. of grid data', PRCNAM, MODNAM)

  if( set_bounds_file_latlon_in(&
        al%f_latlon_in, al%nx, al%ny, &
        al%nh, al%hi, al%hf, al%nv, al%vi, al%vf) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_bounds_file_grid_in(al%f_grid_in, al%nx, al%ny) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_bounds_file_grid_out(al%f_grid_out, al%nx, al%ny) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Set the grid lines
  !-------------------------------------------------------------
  call logent('Setting the grid lines', PRCNAM, MODNAM)

  if( set_gs(al, lon, lat) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Set the raster index map and grid index
  !-------------------------------------------------------------
  call logent('Setting the index map and grid index', PRCNAM, MODNAM)

  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    if( make_idxmap(al, mi1=idx1) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT2 )
    if( make_idxmap(al, mi2=idx2) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT4 )
    if( make_idxmap(al, mi4=idx4) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT8 )
    if( make_idxmap(al, mi8=idx8) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__FILE_BIN )
    call update_file(&
           fg_in%idx, fg_in%idx%id, &
           path_idx, dtype_idx, endian_idx, rec_idx)
    if( make_idxmap(al) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__NOT_GIVEN )
    if( make_idxmap(al) /= 0 )then
      info = 1; call errret(); return
    endif
  case default
    info = 1
    call errret(msg_invalid_value('form_idx', form_idx))
    return
  endselect

  if( make_grdidx(al) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(fg_in)
  nullify(fl)
  nullify(al)
  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function define_mesh_latlon
!===============================================================
!
!===============================================================
integer(4) function define_mesh_raster(&
    name, nx, ny, west, east, south, north, &
    form_idx, idx1, idx2, idx4, idx8, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    idx_miss, &
    origin) result(info)
  use c1_gs_base, only: &
        init_mesh_raster         , &
        set_mesh_common          , &
        set_bounds_file_raster_in, &
        set_bounds_file_grid_in  , &
        set_bounds_file_grid_out
  use c1_gs_define, only: &
        set_gs
  use c1_gs_grid_core, only: &
        make_idxmap, &
        make_grdidx
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'define_mesh_raster'
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in) :: west, east, south, north
  integer(4), intent(in) :: form_idx
  integer(1), intent(in) :: idx1(:,:)
  integer(2), intent(in) :: idx2(:,:)
  integer(4), intent(in) :: idx4(:,:)
  integer(8), intent(in) :: idx8(:,:)
  character(*), intent(in) :: path_idx
  character(*), intent(in) :: dtype_idx
  character(*), intent(in) :: endian_idx
  integer(4)  , intent(in) :: rec_idx
  integer(8), intent(in) :: idx_miss
  character(*), intent(in) :: origin

  type(gs_)            , pointer :: a
  type(gs_raster_)     , pointer :: ar
  type(file_raster_in_), pointer :: fr
  integer :: i_gs

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  ! Find an empty sloat for a grid system
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(a)
  do i_gs = 1, size(lst_gs)
    if( lst_gs(i_gs)%nam == '' )then
      a => lst_gs(i_gs)
      nmax_gs = max(i_gs, nmax_gs)
      exit
    endif
  enddo

  if( .not. associated(a) )then
    info = 1
    call errret('No slot for grid system is left.')
    return
  endif

  a%id = 'gs'//str(i_gs)
  a%nam = trim(name)
  !-------------------------------------------------------------
  ! Check the shape of raster index map
  !-------------------------------------------------------------
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    if( check_shape_2d('idx1', shape(idx1), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT2 )
    if( check_shape_2d('idx2', shape(idx2), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT4 )
    if( check_shape_2d('idx4', shape(idx4), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT8 )
    if( check_shape_2d('idx8', shape(idx8), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__FILE_BIN )
    continue
  case default
    info = 1
    call errret(msg_invalid_value('form_idx', form_idx))
    return
  endselect
  !-------------------------------------------------------------
  ! Set the default values and pointers
  !-------------------------------------------------------------
  if( init_mesh_raster(a) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_mesh_common(a) /= 0 )then
    info = 1; call errret(); return
  endif

  ar => a%raster
  fr => ar%f_raster_in
  !-------------------------------------------------------------
  ! Set the given values (1)
  !-------------------------------------------------------------
  call logent('Setting the given values (1)', PRCNAM, MODNAM)

  ar%nx = nx
  ar%ny = ny
  ar%west = west
  ar%east = east
  ar%south = south
  ar%north = north

  ar%idx_miss = idx_miss

  selectcase( lower(origin) )
  case( ORIGIN_SOUTH )
    ar%is_south_to_north = .true.
  case( ORIGIN_NORTH )
    ar%is_south_to_north = .false.
  case default
    info = 1
    call errret(msg_invalid_value('origin', origin))
    return
  endselect

  call logext()
  !-------------------------------------------------------------
  ! Set bounds. of grid data
  !-------------------------------------------------------------
  call logent('Setting bounds. of grid data', PRCNAM, MODNAM)

  if( set_bounds_file_raster_in(&
        fr,                                     & ! inout
        ar%nx, ar%ny, ar%is_south_to_north,     & ! in
        ar%xi, ar%xf, ar%yi, ar%yf,             & ! out
        ar%nh, ar%hi, ar%hf, ar%nv, ar%vi, ar%vf& ! out
  ) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_bounds_file_grid_in(ar%f_grid_in) /= 0 )then
    info = 1; call errret(); return
  endif
  if( set_bounds_file_grid_out(ar%f_grid_out, ar%nx, ar%ny) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Set the pixel lines
  !-------------------------------------------------------------
  call logent('Setting the pixel lines', PRCNAM, MODNAM)

  if( set_gs(ar) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Set the raster index map and grid index
  !-------------------------------------------------------------
  call logent('Setting the index map and grid index', PRCNAM, MODNAM)

  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    if( make_idxmap(ar, mi1=idx1) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT2 )
    if( make_idxmap(ar, mi2=idx2) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT4 )
    if( make_idxmap(ar, mi4=idx4) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__INT8 )
    if( make_idxmap(ar, mi8=idx8) /= 0 )then
      info = 1; call errret(); return
    endif
  case( INPUTFORM__FILE_BIN )
    call update_file(&
           fr%idx, fr%idx%id, &
           path_idx, dtype_idx, endian_idx, rec_idx)
    if( make_idxmap(ar) /= 0 )then
      info = 1; call errret(); return
    endif
  case default
    info = 1
    call errret(msg_invalid_value('form_idx', form_idx))
    return
  endselect

  if( make_grdidx(ar) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(fr)
  nullify(ar)
  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function define_mesh_raster
!===============================================================
!
!===============================================================
integer(4) function point_mesh(name, a) result(info)
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'point_mesh'
  character(*), intent(in) :: name
  type(gs_)   , pointer    :: a    ! out

  integer :: i_gs

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif

  do i_gs = 1, nmax_gs
    if( lst_gs(i_gs)%nam == trim(name) )then
      a => lst_gs(i_gs)
      call logret(PRCNAM, MODNAM)
      return
    endif
  enddo

  info = 1
  call errret('The mesh "'//str(name)//'" is undefined.')
  return
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function point_mesh
!===============================================================
!
!===============================================================
integer(4) function clear_mesh(name) result(info)
  use c1_gs_base, only: &
        c1_clear_mesh => clear_mesh
  use ls_base, only: &
        logopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_mesh'
  character(*), intent(in) :: name

  type(gs_), pointer :: a

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( point_mesh(name, a) /= 0 )then
    info = 1; call errret(); return
  endif

  if( c1_clear_mesh(a) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function clear_mesh
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
integer(4) function spring_define_mesh_latlon(&
    name, nx, ny, &
    west, east, south, north, lon, lat, &
    path_lon, dtype_lon, endian_lon, rec_lon, &
    path_lat, dtype_lat, endian_lat, rec_lat, &
    idx , idx_miss , idx1, idx1_miss, &
    idx2, idx2_miss, idx8, idx8_miss, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    origin &
) result(info)
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_define_mesh_latlon'
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in), optional :: west, east, south, north
  real(8), intent(in), target, optional :: lon(:), lat(:)
  character(*), intent(in), optional :: path_lon
  character(*), intent(in), optional :: dtype_lon
  character(*), intent(in), optional :: endian_lon
  integer(4)  , intent(in), optional :: rec_lon
  character(*), intent(in), optional :: path_lat
  character(*), intent(in), optional :: dtype_lat
  character(*), intent(in), optional :: endian_lat
  integer(4)  , intent(in), optional :: rec_lat
  integer(1), intent(in), target, optional :: idx1(:,:)
  integer(2), intent(in), target, optional :: idx2(:,:)
  integer(4), intent(in), target, optional :: idx(:,:)
  integer(8), intent(in), target, optional :: idx8(:,:)
  integer(1), intent(in), optional :: idx1_miss
  integer(2), intent(in), optional :: idx2_miss
  integer(4), intent(in), optional :: idx_miss
  integer(8), intent(in), optional :: idx8_miss
  character(*), intent(in), optional :: path_idx
  character(*), intent(in), optional :: dtype_idx
  character(*), intent(in), optional :: endian_idx
  integer(4)  , intent(in), optional :: rec_idx
  character(*), intent(in), optional :: origin

  real(8) :: west_, east_, south_, north_
  real(8), pointer :: lon_(:), lat_(:)
  character(:), allocatable :: path_lon_
  character(CLEN_KEY)       :: dtype_lon_
  character(CLEN_KEY)       :: endian_lon_
  integer                   :: rec_lon_
  character(:), allocatable :: path_lat_
  character(CLEN_KEY)       :: dtype_lat_
  character(CLEN_KEY)       :: endian_lat_
  integer                   :: rec_lat_
  integer(1), pointer :: idx1_(:,:)
  integer(2), pointer :: idx2_(:,:)
  integer(4), pointer :: idx4_(:,:)
  integer(8), pointer :: idx8_(:,:)
  integer(1) :: idx1_miss_
  integer(2) :: idx2_miss_
  integer(4) :: idx4_miss_
  integer(8) :: idx8_miss_
  character(:), allocatable :: path_idx_
  character(CLEN_KEY)       :: dtype_idx_
  character(CLEN_KEY)       :: endian_idx_
  integer                   :: rec_idx_
  character(CLEN_KEY) :: origin_

  integer    :: n_lon, n_lat
  integer(4) :: form_lon, form_lat
  integer    :: n_idx
  integer(4) :: form_idx
  integer    :: n_idx_miss

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  west_ = -180.d0
  east_ =  180.d0
  south_ = -90.d0
  north_ =  90.d0

  idx1_miss_ = IDX1_MISS_DEFAULT
  idx2_miss_ = IDX2_MISS_DEFAULT
  idx4_miss_ = IDX4_MISS_DEFAULT
  idx8_miss_ = IDX8_MISS_DEFAULT

  allocate(character(1) :: path_lon_)
  path_lon_   = ''
  dtype_lon_  = DTYPE_DBLE
  endian_lon_ = ENDIAN_DEFAULT
  rec_lon_    = 1

  allocate(character(1) :: path_lat_)
  path_lat_   = ''
  dtype_lat_  = DTYPE_DBLE
  endian_lat_ = ENDIAN_DEFAULT
  rec_lat_    = 1

  allocate(character(1) :: path_idx_)
  path_idx_   = ''
  dtype_idx_  = DTYPE_INT4
  endian_idx_ = ENDIAN_DEFAULT
  rec_idx_    = 1
  origin_ = ORIGIN_SOUTH

  call logext()
  !-------------------------------------------------------------
  ! Set the given values
  !-------------------------------------------------------------
  call logent('Setting the given values', PRCNAM, MODNAM)

  ! west, east, south, north
  !-------------------------------------------------------------
  if( present(west) ) west_ = west
  if( present(east) ) east_ = east
  if( present(south) ) south_ = south
  if( present(north) ) north_ = north

  ! lon, lat
  !-------------------------------------------------------------
  n_lon = 0
  if( present(lon     ) ) call add(n_lon)
  if( present(path_lon) ) call add(n_lon)
  if( n_lon > 1 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nMultiple inputs for longit. of grid lines.')
    return
  endif

  n_lat = 0
  if( present(lat     ) ) call add(n_lat)
  if( present(path_lon) ) call add(n_lat)
  if( n_lat > 1 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nMultiple inputs for latit. of grid lines.')
    return
  endif

  form_lon = INPUTFORM__NOT_GIVEN
  if( present(lon     ) ) form_lon = INPUTFORM__DBLE
  if( present(path_lon) ) form_lon = INPUTFORM__FILE_BIN

  form_lat = INPUTFORM__NOT_GIVEN
  if( present(lat     ) ) form_lat = INPUTFORM__DBLE
  if( present(path_lat) ) form_lat = INPUTFORM__FILE_BIN

  selectcase( form_lon )
  case( INPUTFORM__DBLE )
    lon_ => lon
  case( INPUTFORM__FILE_BIN )
    path_lon_ = path_lon
    if( present(dtype_lon ) ) dtype_lon_  = dtype_lon
    if( present(endian_lon) ) endian_lon_ = endian_lon
    if( present(rec_lon   ) ) rec_lon_    = rec_lon
  case( INPUTFORM__NOT_GIVEN )
    allocate(lon_(1))
  case default
    info = 1
    call errret(msg_invalid_value('form_lon', form_lon))
    return
  endselect

  selectcase( form_lat )
  case( INPUTFORM__DBLE )
    lat_ => lat
  case( INPUTFORM__FILE_BIN )
    path_lat_ = path_lat
    if( present(dtype_lat ) ) dtype_lat_  = dtype_lat
    if( present(endian_lat) ) endian_lat_ = endian_lat
    if( present(rec_lat   ) ) rec_lat_    = rec_lat
  case( INPUTFORM__NOT_GIVEN )
    allocate(lat_(1))
  case default
    info = 1
    call errret(msg_invalid_value('form_lat', form_lat))
    return
  endselect

  ! idx
  !-------------------------------------------------------------
  n_idx = 0
  if( present(path_idx) ) call add(n_idx)
  if( present(idx1) ) call add(n_idx)
  if( present(idx2) ) call add(n_idx)
  if( present(idx ) ) call add(n_idx)
  if( present(idx8) ) call add(n_idx)
  if( n_idx > 1 )then
    info = 1
    call errret('Multiple index maps were given.')
    return
  endif

  form_idx = INPUTFORM__NOT_GIVEN
  if( present(idx1) ) form_idx = INPUTFORM__INT1
  if( present(idx2) ) form_idx = INPUTFORM__INT2
  if( present(idx ) ) form_idx = INPUTFORM__INT4
  if( present(idx8) ) form_idx = INPUTFORM__INT8
  if( present(path_idx) ) form_idx = INPUTFORM__FILE_BIN

  nullify(idx1_, idx2_, idx4_, idx8_)
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    if( check_shape_2d('idx1', shape(idx1), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx1_ => idx1
    if( present(idx1_miss) ) idx1_miss_ = idx1_miss
  case( INPUTFORM__INT2 )
    if( check_shape_2d('idx2', shape(idx2), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx2_ => idx2
    if( present(idx2_miss) ) idx2_miss_ = idx2_miss
  case( INPUTFORM__INT4 )
    if( check_shape_2d('idx', shape(idx), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx4_ => idx
    if( present(idx_miss ) ) idx4_miss_ = idx_miss
  case( INPUTFORM__INT8 )
    if( check_shape_2d('idx8', shape(idx8), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx8_ => idx8
    if( present(idx8_miss) ) idx8_miss_ = idx8_miss
  case( INPUTFORM__FILE_BIN )
    path_idx_ = path_idx
    if( present(dtype_idx ) ) dtype_idx_  = dtype_idx
    if( present(endian_idx) ) endian_idx_ = endian_idx
    if( present(rec_idx   ) ) rec_idx_    = rec_idx
  case( INPUTFORM__NOT_GIVEN )
    continue
  case default
    info = 1
    call errret(msg_invalid_value('form_idx', form_idx))
    return
  endselect

  if( .not. associated(idx1_) ) allocate(idx1_(1,1))
  if( .not. associated(idx2_) ) allocate(idx2_(1,1))
  if( .not. associated(idx4_) ) allocate(idx4_(1,1))
  if( .not. associated(idx8_) ) allocate(idx8_(1,1))

  ! idx_miss
  !-------------------------------------------------------------
  n_idx_miss = 0
  if( present(idx1_miss) ) call add(n_idx_miss)
  if( present(idx2_miss) ) call add(n_idx_miss)
  if( present(idx_miss ) ) call add(n_idx_miss)
  if( present(idx8_miss) ) call add(n_idx_miss)
  if( n_idx_miss > 1 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nMultiple inputs for missing value of index.')
    return
  endif

  if( present(idx1_miss) )then
    idx8_miss_ = int(idx1_miss,8)
  elseif( present(idx2_miss) )then
    idx8_miss_ = int(idx2_miss,8)
  elseif( present(idx_miss) )then
    idx8_miss_ = int(idx_miss,8)
  elseif( present(idx8_miss) )then
    idx8_miss_ = idx8_miss
  else
    selectcase( form_idx )
    case( INPUTFORM__INT1 )
      idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
    case( INPUTFORM__INT2 )
      idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
    case( INPUTFORM__INT4 )
      idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
    case( INPUTFORM__INT8 )
      idx8_miss_ = IDX8_MISS_DEFAULT
    case( INPUTFORM__FILE_BIN )
      selectcase( dtype_idx_ )
      case( DTYPE_INT1 )
        idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
      case( DTYPE_INT2 )
        idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
      case( DTYPE_INT4 )
        idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
      case( DTYPE_INT8 )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case( DTYPE_REAL, DTYPE_DBLE )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case default
        info = 1
        call errret(msg_invalid_value('dtype_idx_', dtype_idx_))
        return
      endselect
    case( INPUTFORM__NOT_GIVEN )
      idx8_miss_ = IDX8_MISS_DEFAULT
    case default
      info = 1
      call errret(msg_invalid_value('form_idx', form_idx))
      return
    endselect
  endif

  ! origin
  !-------------------------------------------------------------
  if( present(origin) ) origin_ = origin

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( define_mesh_latlon(&
        name, nx, ny, &
        west_, east_, south_, north_, &
        form_lon, form_lat, lon_, lat_, &
        path_lon_, dtype_lon_, endian_lon_, rec_lon_, &
        path_lat_, dtype_lat_, endian_lat_, rec_lat_, &
        form_idx, idx1_, idx2_, idx4_, idx8_, &
        path_idx_, dtype_idx_, endian_idx_, rec_idx_, &
        idx8_miss_, &
        origin_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_define_mesh_latlon
!===============================================================
!
!===============================================================
integer(4) function spring_define_mesh_raster(&
    name, nx, ny, west, east, south, north, &
    idx , idx_miss , idx1, idx1_miss, &
    idx2, idx2_miss, idx8, idx8_miss, &
    path_idx, dtype_idx, endian_idx, rec_idx, &
    origin &
) result(info)
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_define_mesh_raster'
  character(*), intent(in) :: name
  integer, intent(in) :: nx, ny
  real(8), intent(in), optional :: west, east, south, north
  integer(1), intent(in), target, optional :: idx1(:,:)
  integer(2), intent(in), target, optional :: idx2(:,:)
  integer(4), intent(in), target, optional :: idx(:,:)
  integer(8), intent(in), target, optional :: idx8(:,:)
  integer(1), intent(in), optional :: idx1_miss
  integer(2), intent(in), optional :: idx2_miss
  integer(4), intent(in), optional :: idx_miss
  integer(8), intent(in), optional :: idx8_miss
  character(*), intent(in), optional :: path_idx
  character(*), intent(in), optional :: dtype_idx
  character(*), intent(in), optional :: endian_idx
  integer(4)  , intent(in), optional :: rec_idx
  character(*), intent(in), optional :: origin

  real(8) :: west_, east_, south_, north_
  integer(1), pointer :: idx1_(:,:)
  integer(2), pointer :: idx2_(:,:)
  integer(4), pointer :: idx4_(:,:)
  integer(8), pointer :: idx8_(:,:)
  integer(8) :: idx8_miss_
  character(:), allocatable :: path_idx_
  character(CLEN_KEY)       :: dtype_idx_
  character(CLEN_KEY)       :: endian_idx_
  integer                   :: rec_idx_
  character(CLEN_KEY) :: origin_

  integer    :: n_idx
  integer(4) :: form_idx
  integer    :: n_idx_miss

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Set the defaules values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  west_ = -180.d0
  east_ =  180.d0
  south_ = -90.d0
  north_ =  90.d0

  allocate(character(1) :: path_idx_)
  path_idx_   = ''
  dtype_idx_  = DTYPE_INT4
  endian_idx_ = ENDIAN_DEFAULT
  rec_idx_    = 1

  origin_ = ORIGIN_SOUTH

  call logext()
  !-------------------------------------------------------------
  ! Set the given values
  !-------------------------------------------------------------
  call logent('Setting the given values', PRCNAM, MODNAM)

  ! west, east, south, north
  !-------------------------------------------------------------
  if( present(west) ) west_ = west
  if( present(east) ) east_ = east
  if( present(south) ) south_ = south
  if( present(north) ) north_ = north

  ! idx
  !-------------------------------------------------------------
  n_idx = 0
  if( present(idx1) ) call add(n_idx)
  if( present(idx2) ) call add(n_idx)
  if( present(idx ) ) call add(n_idx)
  if( present(idx8) ) call add(n_idx)
  if( present(path_idx) ) call add(n_idx)
  if( n_idx == 0 )then
    info = 1
    call errret('Index map was not given.')
    return
  elseif( n_idx > 1 )then
    info = 1
    call errret('Multiple index maps were given.')
    return
  endif

  form_idx = INPUTFORM__NOT_GIVEN
  if( present(idx1) ) form_idx = INPUTFORM__INT1
  if( present(idx2) ) form_idx = INPUTFORM__INT2
  if( present(idx ) ) form_idx = INPUTFORM__INT4
  if( present(idx8) ) form_idx = INPUTFORM__INT8
  if( present(path_idx) ) form_idx = INPUTFORM__FILE_BIN

  nullify(idx1_, idx2_, idx4_, idx8_)
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    if( check_shape_2d('idx1', shape(idx1), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx1_ => idx1
  case( INPUTFORM__INT2 )
    if( check_shape_2d('idx2', shape(idx2), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx2_ => idx2
  case( INPUTFORM__INT4 )
    if( check_shape_2d('idx', shape(idx), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx4_ => idx
  case( INPUTFORM__INT8 )
    if( check_shape_2d('idx8', shape(idx8), nx, ny) /= 0 )then
      info = 1; call errret(); return
    endif
    idx8_ => idx8
  case( INPUTFORM__FILE_BIN )
    path_idx_ = path_idx
    if( present(dtype_idx ) ) dtype_idx_  = dtype_idx
    if( present(endian_idx) ) endian_idx_ = endian_idx
    if( present(rec_idx   ) ) rec_idx_    = rec_idx
  case( INPUTFORM__NOT_GIVEN )
    info = 1
    call errret(msg_unexpected_condition()//&
              '\n  form_idx: '//str(form_idx))
    return
  case default
    info = 1
    call errret(msg_invalid_value('form_idx', form_idx))
    return
  endselect

  if( .not. associated(idx1_) ) allocate(idx1_(1,1))
  if( .not. associated(idx2_) ) allocate(idx2_(1,1))
  if( .not. associated(idx4_) ) allocate(idx4_(1,1))
  if( .not. associated(idx8_) ) allocate(idx8_(1,1))

  ! idx_miss
  !-------------------------------------------------------------
  n_idx_miss = 0
  if( present(idx1_miss) ) call add(n_idx_miss)
  if( present(idx2_miss) ) call add(n_idx_miss)
  if( present(idx_miss ) ) call add(n_idx_miss)
  if( present(idx8_miss) ) call add(n_idx_miss)
  if( n_idx_miss > 1 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nMultiple inputs for missing value of index.')
    return
  endif

  if( present(idx1_miss) )then
    idx8_miss_ = int(idx1_miss,8)
  elseif( present(idx2_miss) )then
    idx8_miss_ = int(idx2_miss,8)
  elseif( present(idx_miss) )then
    idx8_miss_ = int(idx_miss,8)
  elseif( present(idx8_miss) )then
    idx8_miss_ = idx8_miss
  else
    selectcase( form_idx )
    case( INPUTFORM__INT1 )
      idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
    case( INPUTFORM__INT2 )
      idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
    case( INPUTFORM__INT4 )
      idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
    case( INPUTFORM__INT8 )
      idx8_miss_ = IDX8_MISS_DEFAULT
    case( INPUTFORM__FILE_BIN )
      selectcase( dtype_idx_ )
      case( DTYPE_INT1 )
        idx8_miss_ = int(IDX1_MISS_DEFAULT,8)
      case( DTYPE_INT2 )
        idx8_miss_ = int(IDX2_MISS_DEFAULT,8)
      case( DTYPE_INT4 )
        idx8_miss_ = int(IDX4_MISS_DEFAULT,8)
      case( DTYPE_INT8 )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case( DTYPE_REAL, DTYPE_DBLE )
        idx8_miss_ = IDX8_MISS_DEFAULT
      case default
        info = 1
        call errret(msg_invalid_value('dtype_idx_', dtype_idx_))
        return
      endselect
    case default
      info = 1
      call errret(msg_invalid_value('form_idx', form_idx))
      return
    endselect
  endif

  ! origin
  !-------------------------------------------------------------
  if( present(origin) ) origin_ = origin
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( define_mesh_raster(&
        name, nx, ny, &
        west_, east_, south_, north_, &
        form_idx, idx1_, idx2_, idx4_, idx8_, &
        path_idx_, dtype_idx_, endian_idx_, rec_idx_, &
        idx8_miss_, &
        origin_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( form_idx )
  case( INPUTFORM__INT1 )
    nullify(idx1_)
    deallocate(idx2_, idx4_, idx8_)
  case( INPUTFORM__INT2 )
    nullify(idx2_)
    deallocate(idx1_, idx4_, idx8_)
  case( INPUTFORM__INT4 )
    nullify(idx4_)
    deallocate(idx1_, idx2_, idx8_)
  case( INPUTFORM__INT8 )
    nullify(idx8_)
    deallocate(idx1_, idx2_, idx4_)
  case( INPUTFORM__FILE_BIN )
    deallocate(idx1_, idx2_, idx4_, idx8_)
  case default
    info = 1
    call errret(msg_invalid_value('form_idx', form_idx))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_define_mesh_raster
!===============================================================
!
!===============================================================
integer(4) function spring_clear_mesh(name) result(info)
  use ls_base, only: &
        logopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_clear_mesh'
  character(*), intent(in) :: name

  type(gs_), pointer :: a

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( point_mesh(name, a) /= 0 )then
    info = 1; call errret(); return
  endif

  if( clear_mesh(name) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_clear_mesh
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
subroutine spring_print_meshes_name()
  use ls_base, only: &
        logopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_print_meshes_name'

  integer :: i

  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Meshes:')
  do i = 1, size(lst_gs)
    if( lst_gs(i)%nam == '' ) cycle
    call logmsg('  ('//str(i,dgt(size(lst_gs)))//') '//str(lst_gs(i)%nam))
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine spring_print_meshes_name
!===============================================================
!
!===============================================================
integer(4) function spring_print_mesh(name) result(info)
  use ls_base, only: &
        logopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_print_mesh'
  character(*), intent(in) :: name

  type(gs_), pointer :: a
  type(gs_latlon_) , pointer :: al
  type(gs_raster_) , pointer :: ar
  type(gs_polygon_), pointer :: ap

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( point_mesh(name, a) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('type: '//str(a%typ))

  selectcase( a%typ )
  case( MESHTYPE__LATLON )
    al => a%latlon
    call logmsg('  nx: '//str(al%nx)//', ny: '//str(al%ny))
  case( MESHTYPE__RASTER )
    ar => a%raster
    call logmsg('  nx: '//str(ar%nx)//', ny: '//str(ar%ny))
  case( MESHTYPE__POLYGON )
    ap => a%polygon
    call logmsg('  np: '//str(ap%np)//', nij: '//str(ap%nij))
  case default
    info = 1
    call errret(msg_invalid_value('a%typ', a%typ))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_print_mesh
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
integer(4) function check_shape_2d(varname, shp, nx, ny) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_shape_2d'
  character(*), intent(in) :: varname
  integer     , intent(in) :: shp(:)
  integer     , intent(in) :: nx, ny

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  if( shp(1) /= nx .or. shp(2) /= ny )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nShapes mismatch.'//&
              '\n  shape('//varname//'): '//str(shp,',')//&
              '\n  nx, ny: '//str((/nx,ny/),','))
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_shape_2d
!===============================================================
!
!===============================================================
end module ls_gs
