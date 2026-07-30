module c1_gs_define
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_util
  use lib_array
  use lib_io
  use lib_math
  use c1_const
  use c1_type_gs
  use c1_type_timer
  use c1_timer, only: &
        start_ctimer, &
        stop_ctimer
  use c1_gs_define_polygon, only: &
        set_gs__polygon
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_gs

  public :: check_bounds_lon
  public :: check_bounds_lat
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface set_gs
    module procedure set_mesh__any
    module procedure set_gs__latlon
    module procedure set_gs__raster
    module procedure set_gs__polygon
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_gs_define'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function set_mesh__any(a) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_mesh__any'
  type(gs_), intent(inout) :: a

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( a%typ )

  case( MESHTYPE__LATLON )
    if( set_gs__latlon(a%latlon) /= 0 )then
      info = 1; call errret(); return
    endif

  case( MESHTYPE__RASTER )
    if( set_gs__raster(a%raster) /= 0 )then
      info = 1; call errret(); return
    endif

  case( MESHTYPE__POLYGON )
    if( set_gs__polygon(a%polygon) /= 0 )then
      info = 1; call errret(); return
    endif

  case default
    info = 1
    call errret(msg_invalid_value('a%typ', a%typ)//&
              '\n  Mesh: '//str(a%nam))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_mesh__any
!===============================================================
!
!===============================================================
integer(4) function set_gs__latlon(ul, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_gs__latlon'
  type(gs_latlon_), intent(inout), target :: ul
  real(8), intent(in), optional :: lon(:), lat(:)

  type(file_latlon_in_), pointer :: fl
  type(file_), pointer :: f
  integer(8) :: ih
  real(8) :: coef
  logical :: lon_is_given, lat_is_given

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call start_ctimer('define_lattice')

  fl => ul%f_latlon_in

  allocate(ul%lon(0:ul%nh))
  allocate(ul%lat(0:ul%nv))

  allocate(ul%lonwidth(ul%nh))
  allocate(ul%latwidth(ul%nv))

  allocate(ul%lon0(ul%nh))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( ul%coord_unit )
  case( UNIT_DEGREE )
    coef = d2r
  case( UNIT_RADIAN )
    coef = 1.d0
  case default
    info = 1
    call errret(msg_invalid_value('ul%coord_unit', ul%coord_unit))
    return
  endselect

  lon_is_given = .false.
  lat_is_given = .false.
  if( present(lon) ) lon_is_given = size(lon) > 1
  if( present(lat) ) lat_is_given = size(lat) > 1
  !-------------------------------------------------------------
  ! Lon
  !-------------------------------------------------------------
  f => fl%lon
  !-------------------------------------------------------------
  ! Case: Given as an argument
  if( lon_is_given )then
    ul%lon(:) = lon(:)

    do ih = 0_8, ul%nh
      if( modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')') /= 0 )then
        info = 1; call errret(); return
      endif
    enddo

    ul%lon(:) = ul%lon(:) * coef

    do ih = 1_8, ul%nh
      ul%lonwidth(ih) = londiff_rad(ul%lon(ih-1_8), ul%lon(ih))
    enddo

    ul%west = ul%lon(0_8)
    ul%east = ul%lon(ul%nh)
  !-------------------------------------------------------------
  ! Case: Given via a file
  elseif( f%path /= '' )then
    call logmsg('Reading lon '//str(fileinfo(f)))
    if( rbin(ul%lon, f%path, f%dtype, f%endian, f%rec) /= 0 )then
      info = 1; call errret(); return
    endif

    do ih = 0_8, ul%nh
      if( modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')') /= 0 )then
        info = 1; call errret(); return
      endif
    enddo

    ul%lon(:) = ul%lon(:) * coef

    do ih = 1_8, ul%nh
      ul%lonwidth(ih) = londiff_rad(ul%lon(ih-1_8), ul%lon(ih))
    enddo

    ul%west = ul%lon(0_8)
    ul%east = ul%lon(ul%nh)
  !-------------------------------------------------------------
  ! Case: Not given
  else
    if( modify_lon_deg(ul%west, 'ul%west') /= 0 )then
      info = 1; call errret(); return
    endif
    if( modify_lon_deg(ul%east, 'ul%east') /= 0 )then
      info = 1; call errret(); return
    endif

    if( is_int(ul%west) .and. is_int(ul%east) )then
      if( calc_latlon_bounds_int(&
            ul%lon, ul%lonwidth, ul%west, ul%east, ul%nh) /= 0 )then
        info = 1; call errret(); return
      endif
    else
      if( calc_latlon_bounds_float(&
            ul%lon, ul%lonwidth, ul%west, ul%east) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    do ih = 0, ul%nh
      if( modify_lon_deg(ul%lon(ih), 'ul%lon('//str(ih)//')') /= 0 )then
        info = 1; call errret(); return
      endif
    enddo

    ul%west = ul%west * d2r
    ul%east = ul%east * d2r
    ul%lon(:) = ul%lon(:) * d2r
    ul%lonwidth(:) = ul%lonwidth(:) * d2r
  endif
  !-------------------------------------------------------------
  ! Lat
  !-------------------------------------------------------------
  f => fl%lat
  !-------------------------------------------------------------
  ! Case: Given as an argument
  if( lat_is_given )then
    ul%lat(:) = lat(:)

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  !-------------------------------------------------------------
  ! Case: Given via a file
  elseif( f%path /= '' )then
    call logmsg('Reading lat '//str(fileinfo(f)))
    if( rbin(ul%lat, f%path, f%dtype, f%endian, f%rec) /= 0 )then
      info = 1; call errret(); return
    endif

    if( ul%lat(0) > ul%lat(ul%nv) ) call reverse(ul%lat)

    ul%lat(:) = ul%lat(:) * coef

    ul%south = ul%lat(0)
    ul%north = ul%lat(ul%nv)
    ul%latwidth(:) = ul%lat(1:) - ul%lat(:ul%nv-1_8)
  !-------------------------------------------------------------
  ! Case: Not given
  else
    if( is_int(ul%south) .and. is_int(ul%north) )then
      if( calc_latlon_bounds_int(&
            ul%lat, ul%latwidth, ul%south, ul%north, ul%nv) /= 0 )then
        info = 1; call errret(); return
      endif
    else
      if( calc_latlon_bounds_float(&
            ul%lat, ul%latwidth, ul%south, ul%north) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    ul%south = ul%south * d2r
    ul%north = ul%north * d2r
    ul%lat(:) = ul%lat(:) * d2r
    ul%latwidth(:) = ul%latwidth(:) * d2r
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ul%is_cyclic = abs(ul%lon(0)-ul%lon(ul%nh)) < 1d-10 .or. &
                 abs(abs(ul%lon(0)-ul%lon(ul%nh))-3.6d2) < 1d-10

  if( ul%is_cyclic )then
    if( ul%lat(0) == -rad_90deg .and. ul%lat(ul%nv) == rad_90deg )then
      ul%region_type = REGION_TYPE_GLOBAL
    else
      ul%region_type = REGION_TYPE_CYCLIC
    endif
  else
    ul%region_type = REGION_TYPE_REGIONAL
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ih = 1, ul%nh
    ul%lon0(ih) = ul%lon(ih-1) > ul%lon(ih) .and. ul%lon(ih-1) /= rad_360deg
  enddo

  call stop_ctimer('define_lattice')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grids_latlon(ul%is_cyclic, ul%lon, ul%lat, ul%lon0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_gs__latlon
!===============================================================
!
!===============================================================
integer(4) function set_gs__raster(ar) result(info)
  use c1_gs_base, only: &
        init_mesh_raster_zone
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_gs__raster'
  type(gs_raster_), intent(inout), target :: ar

  type(raster_zone_), pointer :: arz
  integer(8) :: ih
  integer    :: iz

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call start_ctimer('define_lattice')

  allocate(ar%lon(ar%hi-1_8:ar%hf))
  allocate(ar%lat(ar%vi-1_8:ar%vf))

  allocate(ar%lonwidth(ar%hi:ar%hf))
  allocate(ar%latwidth(ar%vi:ar%vf))

  allocate(ar%lon0(ar%hi:ar%hf))
  !-------------------------------------------------------------
  ! Calc. coords. of the boundaries
  !-------------------------------------------------------------
  call logent('Calculating coords. of the boundaries', PRCNAM, MODNAM)

  if( modify_lon_deg(ar%west, 'ar%west') /= 0 )then
    info = 1; call errret(); return
  endif
  if( modify_lon_deg(ar%east, 'ar%east') /= 0 )then
    info = 1; call errret(); return
  endif

  if( is_int(ar%west) .and. is_int(ar%east) )then
    if( calc_latlon_bounds_int(&
          ar%lon, ar%lonwidth, ar%west, ar%east, ar%nh) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    if( calc_latlon_bounds_float(&
          ar%lon, ar%lonwidth, ar%west, ar%east) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( is_int(ar%south) .and. is_int(ar%north) )then
    if( calc_latlon_bounds_int(&
          ar%lat, ar%latwidth, ar%south, ar%north, ar%nv) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    if( calc_latlon_bounds_float(&
          ar%lat, ar%latwidth, ar%south, ar%north) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  do ih = ar%hi, ar%hf
    if( modify_lon_deg(ar%lon(ih), 'ar%lon('//str(ih)//')') /= 0 )then
      info = 1; call errret(); return
    endif
  enddo

  ar%west  = ar%west * d2r
  ar%east  = ar%east * d2r
  ar%south = ar%south * d2r
  ar%north = ar%north * d2r
  ar%lon(:) = ar%lon(:) * d2r
  ar%lat(:) = ar%lat(:) * d2r
  ar%lonwidth(:) = ar%lonwidth(:) * d2r
  ar%latwidth(:) = ar%latwidth(:) * d2r

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ar%is_cyclic = abs(ar%lon(ar%hi-1_8)-ar%lon(ar%hf)) < 1d-10 .or. &
                 abs(abs(ar%lon(ar%hi-1_8)-ar%lon(ar%hf))-3.6d2) < 1d-10

  if( ar%is_cyclic )then
    if( ar%lat(0) == -rad_90deg .and. ar%lat(ar%nv) == rad_90deg )then
      ar%region_type = REGION_TYPE_GLOBAL
    else
      ar%region_type = REGION_TYPE_CYCLIC
    endif
  else
    ar%region_type = REGION_TYPE_REGIONAL
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ar%lon0(:) = .false.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call print_grids_latlon(ar%is_cyclic, ar%lon, ar%lat, ar%lon0)
  !-------------------------------------------------------------
  ! Divide the grid system by the zero-longit. line
  !-------------------------------------------------------------
  if( .not. any(ar%lon(ar%hi:ar%hf-1_8) == rad_0deg) )then
    ar%nZone = 1
    allocate(ar%zone(1))
    arz => ar%zone(1)
    if( init_mesh_raster_zone(arz) /= 0 )then
      info = 1; call errret(); return
    endif

    arz%is_valid = .true.  ! not necessary

    arz%region_type = ar%region_type

    arz%nh = ar%nh
    arz%hi = ar%hi
    arz%hf = ar%hf
    arz%nv = ar%nv
    arz%vi = ar%vi
    arz%vf = ar%vf

    arz%nx = ar%nx
    arz%xi = ar%xi
    arz%xf = ar%xf
    arz%ny = ar%ny
    arz%yi = ar%yi
    arz%yf = ar%yf

    arz%west  = ar%lon(ar%hi-1_8)
    arz%east  = ar%lon(ar%hf)
    arz%south = ar%lat(ar%vi-1_8)
    arz%north = ar%lat(ar%vf)
  else
    call logmsg('Raster grid is divided by the zero-longit. line.')

    ar%nZone = 2
    allocate(ar%zone(ar%nZone))

    do iz = 1, ar%nZone
      arz => ar%zone(iz)

      if( init_mesh_raster_zone(arz) /= 0 )then
        info = 1; call errret(); return
      endif

      arz%is_valid = .true.  ! not necessary

      arz%region_type = REGION_TYPE_REGIONAL

      arz%ny = ar%ny
      arz%yi = ar%yi
      arz%yf = ar%yf
      arz%nv = ar%nv
      arz%vi = ar%vi
      arz%vf = ar%vf
      arz%south = ar%lat(ar%vi-1_8)
      arz%north = ar%lat(ar%vf)
    enddo

    do ih = ar%hi, ar%hf-1_8
      if( ar%lon(ih) == rad_0deg ) exit
    enddo

    arz => ar%zone(1)
    arz%hi = ar%hi
    arz%hf = ih
    arz%nh = ar%hf - ar%hi + 1_8
    arz%west = ar%lon(ar%hi-1_8)
    arz%east = rad_360deg

    arz => ar%zone(2)
    arz%hi = ih + 1_8
    arz%hf = ar%hf
    arz%nh = ar%hf - ar%hi + 1_8
    arz%west = rad_0deg
    arz%east = ar%lon(ar%hf)

    ar%zone(:)%nx = ar%zone(:)%nh
    ar%zone(:)%xi = ar%zone(:)%hi
    ar%zone(:)%xf = ar%zone(:)%hf
  endif

  call stop_ctimer('define_lattice')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_gs__raster
!===============================================================
!
!===============================================================
integer(4) function modify_lon_deg(lon, id) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_lon_deg'
  real(8), intent(inout) :: lon
  character(*), intent(in) :: id

  info = 0
  !-------------------------------------------------------------
  if( -1.8d2 <= lon .and. lon < 0.d0 )then
    lon = lon + 3.6d2
  elseif( lon == 3.6d2 )then
    lon = 0.d0
  elseif( 0.d0 <= lon .and. lon <= 3.6d2 )then
    continue
  else
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nlon < -180 .or. 360 < lon'//&
              '\n  id: '//str(id)//&
              '\n  lon: '//str(lon), &
                PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
end function modify_lon_deg
!===============================================================
!
!===============================================================
integer(4) function calc_latlon_bounds_float(&
    bnd, width, vmin, vmax) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_latlon_bounds_float'
  real(8), pointer :: bnd(:)    ! out (0:n)
  real(8), pointer :: width(:)  ! out (1:n)
  real(8), intent(in)  :: vmin, vmax

  real(8) :: vrange
  integer(8) :: n, i

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n = size(width)

  if( vmin < vmax )then
    vrange = vmax - vmin
  else
    vrange = vmax + 3.6d2 - vmin
  endif

  bnd(0) = vmin
  bnd(n) = vmax
  do i = 1_8, n-1_8
    bnd(i) = vmin + vrange * i / n
    if( bnd(i) > 3.6d2 ) bnd(i) = bnd(i) - 3.6d2
  enddo

  width(:) = vrange / n
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_latlon_bounds_float
!===============================================================
!
!===============================================================
integer(4) function calc_latlon_bounds_int(&
    bnd, width, vmin, vmax, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_latlon_bounds_int'
  real(8), pointer :: bnd(:)    ! out, (is-1:ie)
  real(8), pointer :: width(:)  ! out, (is:ie)
  integer(8), intent(in) :: n
  real(8), intent(in)  :: vmin, vmax

  integer(8) :: is, ie
  integer(8) :: i0, i_step, i
  integer(8) :: vrange
  integer(8) :: vrange_step
  integer(8) :: n_step
  integer(8) :: nBlocks, iBlock
  real(8), allocatable :: bnd_all(:)

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is = lbound(width,1)
  ie = ubound(width,1)

  if( vmin < vmax )then
    vrange = int(vmax - vmin)
  else
    vrange = int(vmax - vmin) + 360
  endif

  nBlocks = gcd(vrange, n)

  vrange_step = vrange / nBlocks
  n_step      = n / nBlocks

  width(:) = real(vrange_step,8) / real(n_step,8)

  allocate(bnd_all(0:n))
  do iBlock = 1, nBlocks
    i0 = n_step*(iBlock-1)
    bnd_all(i0) = vmin + real(vrange_step * (iBlock-1),8)
    do i_step = 1, n_step-1
      bnd_all(i0+i_step) = bnd_all(i0) + real(vrange_step*i_step,8)/real(n_step,8)
    enddo
  enddo

  bnd_all(n) = vmax

  do i = 0_8, n
    if( bnd_all(i) >= 3.6d2 ) bnd_all(i) = bnd_all(i) - 3.6d2
  enddo

  bnd(:) = bnd_all(is-1_8:ie)

  deallocate(bnd_all)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_latlon_bounds_int
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
integer(4) function check_bounds_lon(west, east) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_bounds_lon'
  real(8), intent(in) :: west, east

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( west < -180.d0 .or. west > 360.d0 .or. &
      east < -180.d0 .or. east > 360.d0 )then
    info = 0
    call errret(msg_unexpected_condition()//&
              '\nRange of longitudes is invalid.'//&
              '\n  west: '//str(west)//&
              '\n  east: '//str(east)//&
              '\nLongitudes must be in the range of'//&
                ' [-180, 180] or [0, 360].')
    return
  endif
  !-------------------------------------------------------------
  ! Relation
  !-------------------------------------------------------------
  ! Case: -180 ~ 180
  if( west < 0.d0 )then
    if( east < -180.d0 .or. east > 180.d0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nRange of longitudes is invalid:'//&
                '\n  west < 0 .and. (east < -180 .or. east > 180)'//&
                '\nLongitudes must be in the range of [-180, 180]'//&
                  ' when $west is negative.')
      return
    endif
  !-------------------------------------------------------------
  ! Case: 0 ~ 360
  elseif( west > 180.d0 )then
    if( east < 0.d0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nRange of longitudes is invalid:'//&
                '\n  west > 180 .and. east < 0'//&
                '\nLongitudes must be in the range of [0, 360]'//&
                  ' when $west is negative.')
      return
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_bounds_lon
!===============================================================
!
!===============================================================
integer(4) function check_bounds_lat(south, north) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_bounds_lat'
  real(8), intent(in) :: south, north

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( south < -90.d0 .or. south > 90.d0 .or. &
      north < -90.d0 .or. north > 90.d0 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nRange of latitudes is invalid.'//&
              '\n  south: '//str(south)//&
              '\n  north: '//str(north)//&
              '\nLatitudes must be in the range of'//&
                ' [-90, 90].')
    return
  endif
  !-------------------------------------------------------------
  ! Relation
  !-------------------------------------------------------------
  if( south >= north )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\n  south >= north'//&
              '\n  south: '//str(south)//&
              '\n  north: '//str(north))
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_bounds_lat
!===============================================================
!
!===============================================================
subroutine print_grids_latlon(is_cyclic, lon, lat, lon0)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_grids_latlon'
  logical, intent(in) :: is_cyclic
  real(8), pointer :: lon(:), lat(:)
  logical, pointer :: lon0(:)

  integer(8) :: mh, hi, hf, mv, vi, vf, ih

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call logmsg('is_cyclic: '//str(is_cyclic))

  mh = size(lon)
  mv = size(lat)
  hi = lbound(lon,1) + 1_8
  hf = ubound(lon,1)
  vi = lbound(lat,1) + 1_8
  vf = ubound(lat,1)

  if( mh > 8_8 )then
    call logmsg('lon: '//str(lon(hi-1_8:hi+1_8)*r2d,'f12.7',', ')//&
            ', ..., '//str(lon(hf-2_8:hf)*r2d,'f12.7',', ')//' (deg)')
  else
    call logmsg('lon: '//str(lon*r2d,'f12.7',', ')//' (deg)')
  endif

  if( mv > 8_8 )then
    call logmsg('lat: '//str(lat(vi-1_8:vi+1_8)*r2d,'f12.7',', ')//&
            ', ..., '//str(lat(vf-2_8:vf)*r2d,'f12.7',', ')//' (deg)')
  else
    call logmsg('lat: '//str(lat*r2d,'f12.7',', ')//' (deg)')
  endif

  if( any(lon0) )then
    do ih = hi, hf
      if( .not. lon0(ih) ) cycle
      call logmsg('Cell # '//str(ih)//' intersects with zero-longit. line.'//&
              '\n(west: '//str(lon(ih-1)*r2d,'f12.7')//&
               ', east: '//str(lon(ih  )*r2d,'f12.7')//')')
    enddo
  else
    call logmsg('Any cell or pixel does not intersect with zero-longit. line.')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine print_grids_latlon
!===============================================================
!
!===============================================================
end module c1_gs_define
