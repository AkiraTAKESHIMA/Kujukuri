module c1_gs_define_polygon
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
  use c1_timer
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_gs__polygon

  public :: free_gs_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_gs_define_polygon'

  character(CLEN_WFMT), parameter :: WFMT_COORD = 'es10.3'
  character(CLEN_WFMT), parameter :: WFMT_LONLAT = 'f12.7'
  integer, parameter :: MIJ_PRINT = 10
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function set_gs__polygon(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_gs__polygon'
  type(gs_polygon_), intent(inout), target :: ap

  type(file_polygon_in_), pointer :: fp
  type(grid_)           , pointer :: g
  type(polygon_)        , pointer :: p
  integer(8) :: ijs, ije, ij
  logical :: idx_is_ok, msk_is_ok

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
!  if( .not. ap%is_valid )then
!    call logret(PRCNAM, MODNAM)
!    return
!  endif

  fp => ap%f_polygon_in
  g  => ap%grid

  idx_is_ok = g%status_idx == GRID_STATUS__PREPARED
  msk_is_ok = g%status_msk == GRID_STATUS__PREPARED
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs = ap%ijs
  ije = ap%ije
  if( ap%debug )then
    if( idx_is_ok )then
      ijs = g%ij_debug
      ije = g%ij_debug
    else
      call logwrn('Grid index data has not been prepared.')
      if( 1 <= ap%idx_debug .and. ap%idx_debug <= ap%nij )then
        ijs = g%ij_debug
        ije = g%ij_debug
      else
        info = 1
        call errret('$ap%idx_debug = '//str(ap%idx_debug)//' is out of range.')
        return
      endif
    endif
  endif

  call start_ctimer('buffer')

  allocate(ap%polygon(ijs:ije))

  do ij = ijs, ije
    p => ap%polygon(ij)

    nullify(p%lon)
    nullify(p%lat)
    nullify(p%x)
    nullify(p%y)
    nullify(p%z)
    nullify(p%arctyp)
    nullify(p%arcpos)
    nullify(p%a)
    nullify(p%b)
    nullify(p%c)
    nullify(p%convex)
    nullify(p%lontop)
    nullify(p%lattop)

    if( idx_is_ok )then
      p%idx = g%idx(ij)
    else
      p%idx = ij
    endif
  enddo  ! ij/

  call stop_ctimer('buffer')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call start_ctimer('io')

  call logmsg('Reading grid system data')
  if( read_data_plainbinary(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call stop_ctimer('io')

  call start_ctimer('bbox')

  call logmsg('Modifying coords.')
  if( modify_coords(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Counting the number of vertices')
  if( count_vertices(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Modifying arc type')
  if( modify_arctyp(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Finding polar vertices')
  if( find_polar_vertices(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Judging statuses of the arcs')
  if( judge_status_of_arcs(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Judging types of the grids')
  if( judge_type_of_grids(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Calculating coefs. of the arcs')
  if( calc_coefs_of_arcs(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Calculating the range of longit.')
  if( calc_range_of_longit(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Calculating the range of latit.')
  if( calc_range_of_latit(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Modifying loop directions')
  if( modify_loop_directions(ap) /= 0 )then
    info = 1; call errret(); return
  endif

  call stop_ctimer('bbox')

  call print_info(ap)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ap%debug )then
    p => ap%polygon(g%ij_debug)
    call logmsg('polygon '//str(g%ij_debug))
    call logmsg('n: '//str(p%n))
    call logmsg('pos: '//str(str_polygon_pos_long(p%pos)))
    call logmsg('bbox: '//str((/p%west,p%east,p%south,p%north/)*r2d,WFMT_LONLAT,','))
    call logmsg('n_west: '//str(p%n_west)//', n_east: '//str(p%n_east)//&
              ', n_pole: '//str(p%n_pole))
    call logmsg('lon   : '//str_coords(p%lon, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call logmsg('lat   : '//str_coords(p%lat, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call logmsg('lontop: '//str_coords(p%lontop, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call logmsg('lattop: '//str_coords(p%lattop, r2d, ap%coord_miss_s, WFMT_LONLAT, ',', p%n))
    call logmsg('convex: '//str(str_convex_long(p%convex),-cl(WFMT_LONLAT),','))
    call logmsg('arctyp: '//str(str_arctyp_long(p%arctyp),-cl(WFMT_LONLAT),','))
    call logmsg('arcpos: '//str(str_arcpos_long(p%arcpos),-cl(WFMT_LONLAT),','))
    nullify(p)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_gs__polygon
!===============================================================
!
!===============================================================
integer(4) function read_data_plainbinary(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_data_plainbinary'
  type(gs_polygon_), intent(inout), target :: ap

  type(file_polygon_in_), pointer :: fp
  type(polygon_)        , pointer :: p
  type(file_), pointer :: f
  real(8)   , allocatable :: coord(:,:)
  integer(1), allocatable :: arctyp(:,:)
  integer(8) :: fijs
  integer(8) :: ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fp => ap%f_polygon_in

  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)

    p%n = int(ap%np,4)
    allocate(p%lon(ap%np))
    allocate(p%lat(ap%np))
    allocate(p%x(ap%np))
    allocate(p%y(ap%np))
    allocate(p%z(ap%np))
    allocate(p%arctyp(ap%np))
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fijs = fp%lb(2) + ap%ijs - 1_8

  allocate(coord(ap%np,ap%ijs:ap%ije))

  selectcase( ap%coord_sys )
  !-------------------------------------------------------------
  ! Case: Spherical
  case( coord_sys_spherical )
    !-----------------------------------------------------------
    ! Read coordinate data
    !-----------------------------------------------------------
    call logent('Reading coordinate data', PRCNAM, MODNAM)

    f => fp%lon
    call logmsg('Reading lon '//str(fileinfo(f)))
    if( rbin(coord, f%path, f%dtype, f%endian, f%rec, &
             sz=f%sz(:2), lb=(/f%lb(1),fijs/)) /= 0 )then
      info = 1; call errret(); return
    endif

    do ij = ap%ijs, ap%ije
      ap%polygon(ij)%lon(:) = coord(:,ij)
    enddo

    f => fp%lat
    call logmsg('Reading lat '//str(fileinfo(f)))
    if( rbin(coord, f%path, f%dtype, f%endian, f%rec, &
             sz=f%sz(:2), lb=(/f%lb(1),fijs/)) /= 0 )then
      info = 1; call errret(); return
    endif

    do ij = ap%ijs, ap%ije
      ap%polygon(ij)%lat(:) = coord(:,ij)
    enddo

    if( ap%ije-ap%ijs+1_8 > MIJ_PRINT )then
      do ij = ap%ijs, ap%ijs+2_8
        p => ap%polygon(ij)
        call logmsg('ij '//str(ij,dgt(ap%ije))//&
                '\n  lon: '//str_coords(p%lon,1.d0,ap%coord_miss_s,WFMT_COORD)//&
                '\n  lat: '//str_coords(p%lat,1.d0,ap%coord_miss_s,WFMT_COORD))
      enddo
      call logmsg('...')
      do ij = ap%ije-2_8, ap%ije
        p => ap%polygon(ij)
        call logmsg('ij '//str(ij,dgt(ap%ije))//&
                  '\n  lon: '//str_coords(p%lon,1.d0,ap%coord_miss_s,WFMT_COORD)//&
                  '\n  lat: '//str_coords(p%lat,1.d0,ap%coord_miss_s,WFMT_COORD))
      enddo
    else
      do ij = ap%ijs, ap%ije
        p => ap%polygon(ij)
        call logmsg('ij '//str(ij,dgt(ap%ije))//&
                  '\n  lon: '//str_coords(p%lon,1.d0,ap%coord_miss_s,WFMT_COORD)//&
                  '\n  lat: '//str_coords(p%lat,1.d0,ap%coord_miss_s,WFMT_COORD))
      enddo
    endif

    call logext()
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
!    call logent('Checking values', PRCNAM, MODNAM)

!    call logext(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Case: Cartesian
  case( coord_sys_cartesian )
    !-----------------------------------------------------------
    ! Read coordinate data
    !-----------------------------------------------------------
    call logent('Reading coordinate data', PRCNAM, MODNAM)

    f => fp%x
    call logmsg('Reading x '//str(fileinfo(f)))
    if( rbin(coord, f%path, f%dtype, f%endian, f%rec, &
             sz=f%sz(:2), lb=(/f%lb(1),fijs/)) /= 0 )then
      info = 1; call errret(); return
    endif

    do ij = ap%ijs, ap%ije
      ap%polygon(ij)%x(:) = coord(:,ij)
    enddo

    f => fp%y
    call logmsg('Reading y '//str(fileinfo(f)))
    if( rbin(coord, f%path, f%dtype, f%endian, f%rec, &
             sz=f%sz(:2), lb=(/f%lb(1),fijs/)) /= 0 )then
      info = 1; call errret(); return
    endif

    do ij = ap%ijs, ap%ije
      ap%polygon(ij)%y(:) = coord(:,ij)
    enddo

    f => fp%z
    call logmsg('Reading z '//str(fileinfo(f)))
    if( rbin(coord, f%path, f%dtype, f%endian, f%rec, &
             sz=f%sz(:2), lb=(/f%lb(1),fijs/)) /= 0 )then
      info = 1; call errret(); return
    endif

    do ij = ap%ijs, ap%ije
      ap%polygon(ij)%z(:) = coord(:,ij)
    enddo

    if( ap%ije-ap%ijs+1_8 > MIJ_PRINT )then
      do ij = ap%ijs, ap%ijs+2_8
        p => ap%polygon(ij)
        call logmsg('ij '//str(ij,dgt(ap%ije))//&
                '\n  x: '//str_coords(p%x,1.d0,ap%coord_miss_c,WFMT_COORD)//&
                '\n  y: '//str_coords(p%y,1.d0,ap%coord_miss_c,WFMT_COORD)//&
                '\n  z: '//str_coords(p%z,1.d0,ap%coord_miss_c,WFMT_COORD))
      enddo
      call logmsg('...')
      do ij = ap%ije-2_8, ap%ije
        p => ap%polygon(ij)
        call logmsg('ij '//str(ij,dgt(ap%ije))//&
                '\n  x: '//str_coords(p%x,1.d0,ap%coord_miss_c,WFMT_COORD)//&
                '\n  y: '//str_coords(p%y,1.d0,ap%coord_miss_c,WFMT_COORD)//&
                '\n  z: '//str_coords(p%z,1.d0,ap%coord_miss_c,WFMT_COORD))
      enddo
    else
      do ij = ap%ijs, ap%ije
        p => ap%polygon(ij)
        call logmsg('ij '//str(ij,dgt(ap%ije))//&
                '\n  x: '//str_coords(p%x,1.d0,ap%coord_miss_c,WFMT_COORD)//&
                '\n  y: '//str_coords(p%y,1.d0,ap%coord_miss_c,WFMT_COORD)//&
                '\n  z: '//str_coords(p%z,1.d0,ap%coord_miss_c,WFMT_COORD))
      enddo
    endif

    call logext()
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
!    call logent('Checking values', PRCNAM, MODNAM)

!    call logext()
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('ap%coord_sys', ap%coord_sys))
    return
  endselect

  deallocate(coord)
  !-------------------------------------------------------------
  ! Read arc type data
  !-------------------------------------------------------------
  call logent('Reading arc type data', PRCNAM, MODNAM)

  f => fp%arctyp

  if( f%path == '' )then
    call logmsg('File was not given.')
    do ij = ap%ijs, ap%ije
      ap%polygon(ij)%arctyp(:) = ARC_TYPE_NORMAL
    enddo
  else
    allocate(arctyp(ap%np,ap%ijs:ap%ije))

    call logmsg('Reading arctyp '//str(fileinfo(f)))
    if( rbin(arctyp, f%path, f%dtype, f%endian, f%rec, &
             sz=fp%sz(:2), lb=(/fp%lb(1),fijs/)) /= 0 )then
      info = 1; call errret(); return
    endif

    do ij = ap%ijs, ap%ije
      ap%polygon(ij)%arctyp(:) = arctyp(:,ij)
    enddo

    deallocate(arctyp)
  endif

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function read_data_plainbinary
!===============================================================
!
!===============================================================
integer(4) function modify_coords(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_coords'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n
  logical :: found_0deg, found_lt180deg

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( ap%coord_sys )
  !-------------------------------------------------------------
  ! Case: Spherical
  case( COORD_SYS_SPHERICAL )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call logent('Converting unit', PRCNAM, MODNAM)

    selectcase( ap%coord_unit )
    case( UNIT_DEGREE )
      do ij = ap%ijs, ap%ije
        p => ap%polygon(ij)
        do n = 1, p%n
          if( p%lat(n) /= ap%coord_miss_s )then
            p%lon(n) = p%lon(n) * d2r
            p%lat(n) = p%lat(n) * d2r
            if( abs(p%lat(n)) == rad_90deg )then
              p%lon(n) = ap%coord_miss_s
            endif
          endif
        enddo  ! n/
      enddo  ! ij/
    case( UNIT_RADIAN )
      continue
    case default
      info = 1
      call errret(msg_invalid_value('ap%coord_unit', ap%coord_unit))
      return
    endselect

    call logext()
    !-----------------------------------------------------------
    ! Calc. cartesian coords.
    !-----------------------------------------------------------
    call logent('Calculating cartesian coords.', PRCNAM, MODNAM)

    do ij = ap%ijs, ap%ije
      p => ap%polygon(ij)
      if( spherical_to_cartesian_rad(&
            p%lon, p%lat, p%x, p%y, p%z, &
            ap%coord_miss_s, ap%coord_miss_c) /= 0 )then
        info = 1; call errret(); return
      endif
    enddo

    call logext()
  !-------------------------------------------------------------
  ! Case: Cartesian
  case( COORD_SYS_CARTESIAN )
    !-----------------------------------------------------------
    ! Conv. unit
    !-----------------------------------------------------------
    call logent('Converting unit', PRCNAM, MODNAM)

    selectcase( ap%coord_unit )
    case( UNIT_METER )
      continue
    case( UNIT_KILOMETER )
      do ij = ap%ijs, ap%ije
        p => ap%polygon(ij)
        do n = 1, p%n
          if( p%x(n) /= ap%coord_miss_c )then
            p%x(n) = p%x(n) * 1d3
            p%y(n) = p%y(n) * 1d3
            p%z(n) = p%z(n) * 1d3
          endif
        enddo  ! n/
      enddo  ! ij/
    case default
      info = 1
      call errret(msg_invalid_value('ap%coord_unit', ap%coord_unit))
      return
    endselect

    call logext()
    !-----------------------------------------------------------
    ! Calc. spherical coords.
    !-----------------------------------------------------------
    call logent('Calculating spherical coords.', PRCNAM, MODNAM)

    do ij = ap%ijs, ap%ije
      p => ap%polygon(ij)
      if( cartesian_to_spherical_rad(&
            p%x, p%y, p%z, p%lon, p%lat, &
            ap%coord_miss_c, ap%coord_miss_s) /= 0 )then
        info = 1; call errret(); return
      endif
    enddo

    call logext()
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('ap%coord_sys', ap%coord_sys))
    return
  endselect
  !-------------------------------------------------------------
  ! Modify spherical coords. of special points
  !-------------------------------------------------------------
  call logent('Modifying spherical coords. of special points', PRCNAM, MODNAM)

  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)

    do n = 1, p%n
      if( p%lat(n) == ap%coord_miss_s ) cycle

      if( abs(p%lat(n)) == rad_90deg )then
        p%lon(n) = ap%coord_miss_s
      elseif( p%lon(n) == rad_360deg )then
        p%lon(n) = rad_0deg
      endif
    enddo  ! n/
  enddo  ! ij/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Modifying longit.', PRCNAM, MODNAM)

  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)

    found_0deg     = .false.
    found_lt180deg = .false.
    do n = 1, p%n
      if( p%lon(n) == ap%coord_miss_s ) cycle

      if( p%lon(n) < rad_0deg )then
        p%lon(n) = p%lon(n) + rad_360deg
      elseif( p%lon(n) == rad_360deg )then
        p%lon(n) = rad_0deg
      endif

      if( p%lon(n) == rad_0deg )then
        found_0deg = .true.
      elseif( p%lon(n) < rad_180deg )then
        found_lt180deg = .true.
      endif
    enddo  ! n/

    if( found_0deg .and. .not. found_lt180deg )then
      do n = 1, p%n
        if( p%lon(n) == rad_0deg ) p%lon(n) = rad_360deg
      enddo  ! n/
    endif
  enddo  ! ij/

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function modify_coords
!===============================================================
!
!===============================================================
integer(4) function count_vertices(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'count_vertices'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  type(polygon_) :: p_
  integer(8) :: ij
  integer(4) :: n
  real(8) :: lon1, lat1, lon2, lat2
  logical :: is_same

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(p_%x(ap%np))
  allocate(p_%y(ap%np))
  allocate(p_%z(ap%np))
  allocate(p_%lon(ap%np))
  allocate(p_%lat(ap%np))
  allocate(p_%arctyp(ap%np))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)

    p%n = 0
    do n = 1, int(ap%np,4)
      lon1 = p%lon(n)
      lat1 = p%lat(n)
      lon2 = p%lon(next_cyclic(n,int(ap%np,4)))
      lat2 = p%lat(next_cyclic(n,int(ap%np,4)))

      is_same = .false.
      if( lat1 == ap%coord_miss_s )then
        if( lon1 /= ap%coord_miss_s )then
          info = 1
          call errret(msg_unexpected_condition()//&
                    '\nlat. is missing value but lon. is not missing value')
          return
        endif
        cycle
      elseif( abs(lat1) == rad_90deg )then
        is_same = lat1 == lat2
      elseif( lat1 == lat2 )then
        is_same = lon1 == lon2 .or. abs(lon2 - lon1) == rad_360deg
      endif

      if( is_same )then
        if( ap%allow_duplicated_vertex )then
          !call logmsg('polygon('//str(ij,dgt(ap%ije))//') '//&
          !          '('//str(n,dgt(ap%np))//') == ('//str(ap%n_next(n,ap%np))//') '//&
          !          '('//str(str_coords_lonlat((/lon1,lat1/),ap%coord_miss_s))//')')
        else
          info = 1
          call errret(msg_unexpected_condition()//&
                    '\nDuplicated vertices were found.'//&
                    '\npolygon('//str(ij,dgt(ap%ije))//') '//&
                      '('//str(n)//') == ('//str(next_cyclic(n,int(ap%np,4)))//') '//&
                      '('//str(str_coords_lonlat((/lon1,lat1/),ap%coord_miss_s))//')')
          return
        endif
        cycle
      endif

      call add(p%n)
    enddo  ! n/

    selectcase( p%n )
    case( 0 )
      deallocate(p%x)
      deallocate(p%y)
      deallocate(p%z)
      deallocate(p%lon)
      deallocate(p%lat)
      deallocate(p%arctyp)
      cycle
    case( 3: )
      continue
    case default
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  ij: '//str(ij)//&
                '\n  p%n: '//str(p%n))
      return
    endselect

    if( p%n == ap%np ) cycle

    p_%x(:) = p%x(:)
    p_%y(:) = p%y(:)
    p_%z(:) = p%z(:)
    p_%lon(:) = p%lon(:)
    p_%lat(:) = p%lat(:)
    p_%arctyp(:) = p%arctyp(:)

    p%n = 0
    do n = 1, int(ap%np,4)
      lon1 = p_%lon(n)
      lat1 = p_%lat(n)
      lon2 = p_%lon(next_cyclic(n,int(ap%np,4)))
      lat2 = p_%lat(next_cyclic(n,int(ap%np,4)))

      if( lat1 == ap%coord_miss_s )then
        cycle
      elseif( abs(lat1) == rad_90deg )then
        if( lat1 == lat2 ) cycle
      elseif( lat1 == lat2 )then
        if( lon1 == lon2 .or. abs(lon2-lon1) == rad_360deg ) cycle
      endif

      call add(p%n)
      p%x(p%n) = p_%x(n)
      p%y(p%n) = p_%y(n)
      p%z(p%n) = p_%z(n)
      p%lon(p%n) = p_%lon(n)
      p%lat(p%n) = p_%lat(n)
      p%arctyp(p%n) = p_%arctyp(n)
    enddo  ! n/

    if( p%n > 0 .and. p%n < 3 )then
      call logwrn('The number of vertices with valid coordinates is less than 3.'//&
          '\n  ij: '//str(ij)//&
          '\n  n: '//str(n)//&
          '\n  lon: '//str(p%lon*r2d,'es20.13',', ')//' (deg)'//&
          '\n  lat: '//str(p%lat*r2d,'es20.13',', ')//' (deg)'//&
          '\n  x  : '//str(p%x,'es20.13',', ')//' (m)'//&
          '\n  y  : '//str(p%y,'es20.13',', ')//' (m)'//&
          '\n  z  : '//str(p%z,'es20.13',', ')//' (m)')
    endif

    call realloc(p%x, p%n, clear=.false.)
    call realloc(p%y, p%n, clear=.false.)
    call realloc(p%z, p%n, clear=.false.)
    call realloc(p%lon, p%n, clear=.false.)
    call realloc(p%lat, p%n, clear=.false.)
    call realloc(p%arctyp, p%n, clear=.false.)
  enddo  ! ij/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(p_%x)
  deallocate(p_%y)
  deallocate(p_%z)
  deallocate(p_%lon)
  deallocate(p_%lat)
  deallocate(p_%arctyp)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function count_vertices
!===============================================================
!
!===============================================================
integer(4) function modify_arctyp(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_arctyp'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n, nn

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)

    do n = 1, p%n
      !nn = ap%n_next(n,p%n)
      nn = next_cyclic(n,p%n)

      if( p%lon(n) == p%lon(nn) )then
        p%arctyp(n) = ARC_TYPE_MERIDIAN
      elseif( p%lat(n) == p%lat(nn) .and. ap%arc_parallel )then
        p%arctyp(n) = ARC_TYPE_PARALLEL
      endif
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function modify_arctyp
!===============================================================
!
!===============================================================
integer(4) function find_polar_vertices(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'find_polar_vertices'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n
  logical :: is_ok

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  is_ok = .true.
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)

    p%n_pole = 0
    do n = 1, p%n
      if( abs(p%lat(n)) == rad_90deg )then
        if( p%n_pole /= 0 )then
          is_ok = .false.
          call erradd(msg_unexpected_condition()//&
                    '\n  p%n_pole /= 0'//&
                    '\n  ij: '//str(ij)//&
                    '\n  lon: '//str_coords(p%lon, r2d, ap%coord_miss_s, WFMT_LONLAT, n1max=p%n)//&
                    '\n  lat: '//str_coords(p%lat, r2d, ap%coord_miss_s, WFMT_LONLAT, n1max=p%n))
          exit
        endif
        p%n_pole = int(n,4)
      endif
    enddo  ! n/
  enddo  ! ij/

  if( .not. is_ok )then
    info = 1
    call errret()
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function find_polar_vertices
!===============================================================
!
!===============================================================
integer(4) function judge_status_of_arcs(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'judge_status_of_arcs'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n, n_next

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%arcpos(p%n))
    p%arcpos(:) = arc_position_normal

    do n = 1, p%n
      n_next = next_cyclic(n,p%n)

      if( n == p%n_pole .or. n_next == p%n_pole )then
        p%arctyp(n) = arc_type_meridian
        p%arcpos(n) = arc_position_polar
      elseif( p%lon(n) == p%lon(n_next) )then
        p%arctyp(n) = arc_type_meridian
      elseif( abs(p%lon(n) - p%lon(n_next)) > rad_180deg )then
        p%arcpos(n) = arc_position_lon0
      endif

      if( ap%arc_parallel )then
        if( p%lat(n) == p%lat(n_next) )then
          p%arctyp(n) = ARC_TYPE_PARALLEL
        endif
      endif
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function judge_status_of_arcs
!===============================================================
!
!===============================================================
integer(4) function judge_type_of_grids(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'judge_type_of_grids'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n
  integer :: counter_lon0

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle
    !-----------------------------------------------------------
    ! Count intersections with lon0-line
    !-----------------------------------------------------------
    counter_lon0 = 0

    do n = 1, p%n
      selectcase( p%arcpos(n) )
      case( ARC_POSITION_NORMAL, &
            ARC_POSITION_POLAR )
        continue
      case( ARC_POSITION_LON0 )
        call add(counter_lon0)
      case default
        info = 1
        call errret(msg_invalid_value('p%arcpos(n)', p%arcpos(n))//&
                  '\n  ij: '//str(ij)//&
                  '\n  n : '//str(n))
        return
      endselect
    enddo  ! n/
    !-----------------------------------------------------------
    ! Judge the type of the grid
    !-----------------------------------------------------------
    if( counter_lon0 == 0 )then
      p%pos = POLYGON_POSITION_NORMAL
    elseif( p%n_pole == 0 .and. mod(counter_lon0,2) == 1 )then
      p%pos = POLYGON_POSITION_POLAR
    else
      p%pos = POLYGON_POSITION_LON0
    endif
    !-----------------------------------------------------------
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function judge_type_of_grids
!===============================================================
!
!===============================================================
integer(4) function calc_coefs_of_arcs(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_coefs_of_arcs'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n, n_next

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%a(p%n))
    allocate(p%b(p%n))
    allocate(p%c(p%n))

    do n = 1, p%n
      n_next = next_cyclic(n,p%n)

      selectcase( p%arctyp(n) )
      case( ARC_TYPE_MERIDIAN )
        if( calc_coefs_large_arc(&
              p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
              p%a(n), p%b(n), p%c(n)) /= 0 )then
          info = 1; call errret(); return
        endif
        p%c(n) = 0.d0
      case( ARC_TYPE_PARALLEL )
        p%a(n) = 0.d0
        p%b(n) = 0.d0
        p%c(n) = 0.d0
      case( ARC_TYPE_NORMAL )
        if( calc_coefs_large_arc(&
              p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
              p%a(n), p%b(n), p%c(n)) /= 0 )then
          info = 1; call errret(); return
        endif
      case default
        info = 1
        call errret(msg_invalid_value('p%arctyp(n)', p%arctyp(n))//&
                  '\n  ij: '//str(ij)//&
                  '\n  n : '//str(n))
        return
      endselect
    enddo  ! n/
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_coefs_of_arcs
!===============================================================
!
!===============================================================
integer(4) function calc_range_of_longit(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_range_of_longit'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Normal
    case( polygon_position_normal )
      if( p%n_pole == 0 )then
        p%n_west = minloc(p%lon,1)
        p%n_east = maxloc(p%lon,1)
        p%west = p%lon(p%n_west)
        p%east = p%lon(p%n_east)
      else
        if( p%n_pole == 1 )then
          p%n_west = 2
          p%n_east = 2
        else
          p%n_west = 1
          p%n_east = 1
        endif
        p%west = p%lon(p%n_west)
        p%east = p%lon(p%n_east)

        do n = 1, p%n
          if( n /= p%n_pole )then
            if( p%lon(n) < p%west )then
              p%n_west = int(n,4)
              p%west = p%lon(n)
            elseif( p%lon(n) > p%east )then
              p%n_east = int(n,4)
              p%east = p%lon(n)
            endif
          endif
        enddo  ! n/
      endif
    !-----------------------------------------------------------
    ! Case: Lon0
    case( polygon_position_lon0 )
      p%n_west = 0
      p%n_east = 0
      p%west = rad_360deg
      p%east = rad_0deg

      do n = 1, p%n
        if( n == p%n_pole ) cycle

        if( p%lon(n) > rad_180deg )then
          if( p%lon(n) < p%west )then
            p%n_west = int(n,4)
            p%west = p%lon(n)
          endif
        else
          if( p%lon(n) > p%east )then
            p%n_east = int(n,4)
            p%east = p%lon(n)
          endif
        endif
      enddo

      call logmsg('ij '//str(ij)//' intersect with lon0.'//&
                ' West: '//str(p%west*r2d,'f12.8')//&
                ' East: '//str(p%east*r2d,'f12.8'))
    !-----------------------------------------------------------
    ! Case: Polar
    case( polygon_position_polar )
      p%n_west = 0
      p%n_east = 0
      p%west = rad_0deg
      p%east = rad_360deg

      call logmsg('ij '//str(ij)//' include a pole.'//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s)))
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('p%pos', p%pos))
      return
    endselect
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_range_of_longit
!===============================================================
!
!===============================================================
integer(4) function calc_range_of_latit(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_range_of_latit'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n, n_next
  real(8) :: south, north

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    allocate(p%convex(p%n))
    allocate(p%lontop(p%n))
    allocate(p%lattop(p%n))
    p%convex(:) = CONVEX_MONOTONE
    p%lontop(:) = ap%coord_miss_s
    p%lattop(:) = ap%coord_miss_s

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Polar
    case( POLYGON_POSITION_POLAR )

      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      if( p%north > rad_0deg )then
        p%north = rad_90deg
      else
        p%south = -rad_90deg
      endif

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( ARC_TYPE_NORMAL )
          n_next = next_cyclic(n,p%n)

          if( calc_lat_range_large_arc(&
                p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                p%a(n), p%b(n), p%c(n), &
                south, north, p%convex(n), p%lontop(n), p%lattop(n)) /= 0 )then
            info = 1; call errret(); return
          endif

          if( p%convex(n) == CONVEX_MONOTONE )then
            p%lontop(n) = ap%coord_miss_s
            p%lattop(n) = ap%coord_miss_s
          endif
        case( ARC_TYPE_MERIDIAN )

        case( ARC_TYPE_PARALLEL )

        case default
          info = 1
          call errret(msg_invalid_value('p%arctyp(n)', p%arctyp(n))//&
                    '\n  ij: '//str(ij)//&
                    '\n  n : '//str(n))
          return
        endselect
      enddo  ! n/
    !-----------------------------------------------------------
    ! Case: Lon0 or Normal
    case( POLYGON_POSITION_LON0, &
          POLYGON_POSITION_NORMAL )

      p%south = minval(p%lat)
      p%north = maxval(p%lat)

      do n = 1, p%n
        selectcase( p%arctyp(n) )
        case( ARC_TYPE_NORMAL )
          n_next = next_cyclic(n,p%n)

          if( calc_lat_range_large_arc(&
                p%lon(n), p%lat(n), p%lon(n_next), p%lat(n_next), &
                p%a(n), p%b(n), p%c(n), &
                south, north, p%convex(n), p%lontop(n), p%lattop(n)) /= 0 )then
            info = 1; call errret(); return
          endif

          if( p%convex(n) == CONVEX_MONOTONE )then
            p%lontop(n) = ap%coord_miss_s
            p%lattop(n) = ap%coord_miss_s
          endif

          p%south = min(p%south, south)
          p%north = max(p%north, north)
        case( ARC_TYPE_MERIDIAN )

        case( ARC_TYPE_PARALLEL )

        case default
          info = 1
          call errret(msg_invalid_value('p%arctyp(n)', p%arctyp(n))//&
                    '\n  ij: '//str(ij)//&
                    '\n  n : '//str(n))
          return
        endselect
      enddo  ! n/
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('p%pos', p%pos)//&
                '\n  ij: '//str(ij))
      return
    endselect
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_range_of_latit
!===============================================================
!
!===============================================================
integer(4) function modify_loop_directions(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_loop_directions'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij
  integer(4) :: n, n_prev, n_next
  logical :: is_anticlockwise
  logical :: is_north

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)
    if( p%n == 0 ) cycle

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: Normal
    case( POLYGON_POSITION_NORMAL )
      n = p%n_west
      n_prev = prev_cyclic(n,p%n)
      n_next = next_cyclic(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) < p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Lon0
    case( POLYGON_POSITION_LON0 )
      n = p%n_west
      n_prev = prev_cyclic(n,p%n)
      n_next = next_cyclic(n,p%n)

      if( p%lat(n_prev) == p%lat(n_next) )then
        is_anticlockwise = p%lon(n_prev) > p%lon(n_next)
      else
        is_anticlockwise = p%lat(n_prev) > p%lat(n_next)
      endif
    !-----------------------------------------------------------
    ! Case: Polar
    case( POLYGON_POSITION_POLAR )
      if( p%north == rad_90deg )then
        n = minloc(p%lat,1)
        is_north = .true.
      else
        n = maxloc(p%lat,1)
        is_north = .false.
      endif

      if( get_n_next_lon_is_unequal(&
            ij, n, p%n, p%n_pole, p%lon, n_next) /= 0 )then
        info = 1; call errret(); return
      endif
      n = prev_cyclic(n_next,p%n)

      if( p%arcpos(n) == ARC_POSITION_LON0 )then
        is_anticlockwise = p%lon(n) > p%lon(n_next) .eqv. is_north
      else
        is_anticlockwise = p%lon(n) < p%lon(n_next) .eqv. is_north
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('p%pos', p%pos))
      return
    endselect
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( .not. is_anticlockwise )then
      call reverse(p%lon)
      call reverse(p%lat)

      call reverse(p%x)
      call reverse(p%y)
      call reverse(p%z)

      p%n_west = p%n - p%n_west + 1
      p%n_east = p%n - p%n_east + 1

      if( p%n_pole /= 0 )then
        p%n_pole = p%n - p%n_pole + 1
      endif

      call reverse(p%arctyp(:p%n-1))
      call reverse(p%arcpos(:p%n-1))

      call reverse(p%a(:p%n-1))
      call reverse(p%b(:p%n-1))
      call reverse(p%c(:p%n-1))

      p%a(:) = -p%a(:)
      p%b(:) = -p%b(:)
      p%c(:) = -p%c(:)

      call reverse(p%convex(:p%n-1))
      call reverse(p%lontop(:p%n-1))
      call reverse(p%lattop(:p%n-1))
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function modify_loop_directions
!===============================================================
!
!===============================================================
integer(4) function get_n_next_lon_is_unequal(&
    ij, n, nmax, n_pole, lon, n_next) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_n_next_lon_is_unequal'
  integer(8), intent(in) :: ij
  integer(4), intent(in) :: n
  integer(4), intent(in) :: nmax, n_pole
  real(8)   , intent(in) :: lon(:)  !(nmax)
  integer(4), intent(out) :: n_next

  logical :: is_found
  integer :: i

  info = 0
  !call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  n_next = next_cyclic(n,nmax)
  do while( lon(n_next) == lon(n) .or. n_next == n_pole )
    n_next = next_cyclic(n_next,nmax)
!    if( n_next == n )then
!      call logbgn(PRCNAM, MODNAM, '-p -x2')
!      info = 1
!      call errret(msg_unexpected_condition()//&
!                '\n  n_next == n'//&
!                '\n  ij: '//str(ij))
!      return
!    endif
    ! TMP
    if( n_next == n )then
      is_found = .false.
      do i = 1, nmax
        if( i == n .or. i == n_pole ) cycle
        if( lon(i) /= lon(n) )then
          is_found = .true.
          exit
        endif
      enddo
      if( .not. is_found )then
        info = 1
        call logbgn(PRCNAM, MODNAM, '-p')
        call errret(msg_unexpected_condition()//&
                  '\nPoints with unequal longitudes were not found. ij: '//str(ij))
        return
      else
        info = 1
        call logbgn(PRCNAM, MODNAM, '-p')
        call errret(msg_internal_error()//&
                  '\nFailed to find points with unequal longitudes.')
        return
      endif
    endif
  enddo
  !-------------------------------------------------------------
  !call logret(PRCNAM, MODNAM)
end function get_n_next_lon_is_unequal
!===============================================================
!
!===============================================================
subroutine print_info(ap)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_info'
  type(gs_polygon_), intent(in), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij, mij_valid

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  mij_valid = 0_8
  do ij = ap%ijs, ap%ije
    p => ap%polygon(ij)
    selectcase( p%n )
    case( 0 )
      cycle
    case( 3: )
      call add(mij_valid)
    case default
      call errend(msg_internal_error()//&
                '\np%n /= 0 .and. p%n < 3 @ ij = '//str(ij))
    endselect
  enddo

  call logmsg('Num. of valid grids: '//str(mij_valid)//' / '//str(ap%ije-ap%ijs+1_8))

  if( mij_valid <= MIJ_PRINT )then
    do ij = ap%ijs, ap%ije
      p => ap%polygon(ij)
      if( p%n == 0 ) cycle

      call logmsg('ij '//str(ij,dgt(ap%ije))//' n '//str(p%n,dgt(ap%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s))//' (deg)')
    enddo  ! ij/
  else
    mij_valid = 0_8
    ij = ap%ijs - 1_8
    do while( mij_valid < MIJ_PRINT/2 )
      ij = ij + 1_8
      p => ap%polygon(ij)
      if( p%n == 0 ) cycle
      call add(mij_valid)

      call logmsg('ij '//str(ij,dgt(ap%ije))//' n '//str(p%n,dgt(ap%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s))//' (deg)')
    enddo

    call logmsg('...')

    mij_valid = 0_8
    ij = ap%ije + 1_8
    do while( mij_valid < MIJ_PRINT/2 )
      ij = ij - 1_8
      p => ap%polygon(ij)
      if( p%n == 0 )cycle
      call add(mij_valid)
    enddo

    ij = ij - 1_8
    do while( ij < ap%ije )
      ij = ij + 1
      p => ap%polygon(ij)
      if( p%n == 0 )cycle

      call logmsg('ij '//str(ij,dgt(ap%ije))//' n '//str(p%n,dgt(ap%np))//&
              '\n  lon: '//str(str_coords_lonlat(p%lon,ap%coord_miss_s))//&
              '\n  lat: '//str(str_coords_lonlat(p%lat,ap%coord_miss_s))//' (deg)')
    enddo
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine print_info
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
function str_coords_lonlat(coords, coord_miss) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_coords_lonlat'
  real(8), intent(in) :: coords(:)
  real(8), intent(in) :: coord_miss
  character(cl(WFMT_LONLAT)*size(coords)+2*(size(coords)-1)) :: res

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  res = str_coords(coords, r2d, coord_miss, WFMT_LONLAT)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function str_coords_lonlat
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
integer(4) function free_gs_polygon(ap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_gs_polygon'
  type(gs_polygon_), intent(inout), target :: ap

  type(polygon_), pointer :: p
  integer(8) :: ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  do ij = 1_8, ap%nij
    p => ap%polygon(ij)

    deallocate(p%lon)
    deallocate(p%lat)
    deallocate(p%x)
    deallocate(p%y)
    deallocate(p%z)
    deallocate(p%arctyp)
    deallocate(p%arcpos)
    deallocate(p%a)
    deallocate(p%b)
    deallocate(p%c)
    deallocate(p%convex)
    deallocate(p%lontop)
    deallocate(p%lattop)
  enddo

  deallocate(ap%polygon)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_gs_polygon
!===============================================================
!
!===============================================================
end module c1_gs_define_polygon
