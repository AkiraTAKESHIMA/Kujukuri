module mod_config
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: prep_static_data
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface read_map
    module procedure read_map__int4
    module procedure read_map__dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_static_data()
  implicit none

  call read_config()

  call setup_domain()

  call prep_landcover()

  call prep_topography()

  call prep_slo_idx()

  call prep_riv_idx()
end subroutine prep_static_data
!===============================================================
!
!===============================================================
subroutine read_config()
  implicit none
  !character(32) :: sdate_start, sdate_end
  integer :: un

  open(newunit=un, file='config.txt', status='old')

  read(un,*)

  read(un,*)
  read(un,"(a)") prcpfile
  read(un,"(a)") demfile
  read(un,"(a)") accfile
  read(un,"(a)") dirfile
  print"(a)", 'prcp: '//trim(prcpfile)
  print"(a)", 'dem: '//trim(demfile)
  print"(a)", 'acc: '//trim(accfile)
  print"(a)", 'dir: '//trim(dirfile)

  ! TMP
  read(un,*)
  read(un,*) time_hours

  time_start = 0.d0
  time_end = time_hours * 3600.d0
  !read(un,*) sdate_start
  !read(un,*) sdate_end

  !allocate(datetime_start(6))
  !allocate(datetime_end(6))
  !call s2date(sdate_start, datetime_start)
  !call s2date(sdate_end, datetime_end)

  read(un,*)
  read(un,*) dt_model
  read(un,*) dt_slo
  read(un,*) dt_riv
  print"(a,f10.3)", 'dt model: ',dt_model
  print"(a,f10.3)", 'dt slope: ',dt_slo
  print"(a,f10.3)", 'dt river: ',dt_riv

  ! TMP
  nt_model = time_end / dt_model

  read(un,*)
  read(un,*) dir_out
  read(un,*) dt_out

  read(un,*)
  read(un,*) utm
  read(un,*) eight_dir

  read(un,*)
  read(un,*) ns_river

  read(un,*)
  read(un,*) num_of_landuse

  allocate(dif(num_of_landuse))
  allocate(ns_slope(num_of_landuse))
  allocate(soildepth(num_of_landuse))
  allocate(gammaa(num_of_landuse))
  allocate(ksv(num_of_landuse))
  allocate(faif(num_of_landuse))
  allocate(ka(num_of_landuse))
  allocate(gammam(num_of_landuse))
  allocate(beta(num_of_landuse))
  allocate(ksg(num_of_landuse))
  allocate(gammag(num_of_landuse))
  allocate(kg0(num_of_landuse))
  allocate(fpg(num_of_landuse))
  allocate(rgl(num_of_landuse))

  read(un,*) dif(:)
  read(un,*) ns_slope(:)
  read(un,*) soildepth(:)
  read(un,*) gammaa(:)

  read(un,*) 
  read(un,*) ksv(:)
  read(un,*) faif(:)

  read(un,*) 
  read(un,*) ka(:)
  read(un,*) gammam(:)
  read(un,*) beta(:)

  read(un,*) 
  read(un,*) ksg(:)
  read(un,*) gammag(:)
  read(un,*) kg0(:)
  read(un,*) fpg(:)
  read(un,*) rgl(:)

  read(un,*) 
  read(un,*) riv_thresh
  read(un,*) width_param_c
  read(un,*) width_param_s
  read(un,*) depth_param_c
  read(un,*) depth_param_s
  read(un,*) height_param
  read(un,*) height_limit_param

  read(un,*)
  read(un,*) rivfile_switch
  read(un,"(a)") widthfile
  read(un,"(a)") depthfile
  read(un,"(a)") heightfile

  read(un,*)
  read(un,*) init_slo_switch, init_riv_switch, init_gw_switch, init_gampt_ff_switch
  read(un,"(a)") initfile_slo
  read(un,"(a)") initfile_riv
  read(un,"(a)") initfile_gw
  read(un,"(a)") initfile_gampt_ff

  read(un,*)
  read(un,*) bound_slo_wlev_switch, bound_riv_wlev_switch
  read(un,"(a)") boundfile_slo_wlev
  read(un,"(a)") boundfile_riv_wlev

  read(un,*) 
  read(un,*) bound_slo_disc_switch, bound_riv_disc_switch
  read(un,"(a)") boundfile_slo_disc
  read(un,"(a)") boundfile_riv_disc

  read(un,*)
  read(un,*) land_switch
  read(un,"(a)") landfile
  if( land_switch == 1 ) print"(a)", 'landfile: '//trim(landfile)

  read(un,*)
  read(un,*) dam_switch
  read(un,"(a)") damfile

  read(un,*)
  read(un,*) div_switch
  read(un,"(a)") divfile

  read(un,*)
  read(un,*) evp_switch
  read(un,"(a)") evpfile

  read(un,*)
  read(un,*) sec_length_switch
  read(un,"(a)") sec_length_file

  read(un,*)
  read(un,*) sec_switch
  read(un,"(a)") sec_map_file
  read(un,"(a)") sec_file

  close(un)
end subroutine read_config
!===============================================================
!
!===============================================================
subroutine setup_domain()
  implicit none
  real(8) :: d1, d2, d3, d4
  integer :: un
  character :: ctmp

  open(newunit=un, file=demfile, status='old')
  read(un,*) ctmp, nx
  read(un,*) ctmp, ny
  read(un,*) ctmp, xllcorner
  read(un,*) ctmp, yllcorner
  read(un,*) ctmp, cellsize
  read(un,*) ! nodata
  close(un)
  print*, 'nx: ',nx,' ny: ',ny

  xurcorner = xllcorner + nx * cellsize
  yurcorner = yllcorner + ny * cellsize

  ! Length of sides
  selectcase( utm )
  case( 0 )
    call hubeny( xllcorner, yllcorner, xurcorner, yllcorner, d1 )  ! south
    call hubeny( xllcorner, yurcorner, xurcorner, yurcorner, d2 )  ! north
    call hubeny( xllcorner, yllcorner, xllcorner, yurcorner, d3 )  ! west
    call hubeny( xurcorner, yllcorner, xurcorner, yurcorner, d4 )  ! east
    dx = (d1 + d2) / 2.d0 / real(nx)
    dy = (d3 + d4) / 2.d0 / real(ny)
  case( 1 )
    dx = cellsize
    dy = cellsize
  endselect
  print*, 'dx [m]: ',dx,' dy [m]: ',dy

  length = sqrt(dx * dy)
  area = dx * dy
end subroutine setup_domain
!===============================================================
!
!===============================================================
subroutine prep_landcover()
  implicit none
  integer :: i

  allocate(land(nx, ny))
  land(:,:) = 1
  if( land_switch == 1 )then
    call read_map(landfile, land)
  endif

  where( land <= 0 .or. land > num_of_landuse ) land = num_of_landuse

  ! Parameter Check
  do i = 1, num_of_landuse
    if( ksv(i) .gt. 0.d0 .and. ka(i) .gt. 0.d0 ) &
      stop "Error: both ksv and ka are non-zero."
    if( gammam(i) .gt. gammaa(i) ) &
      stop "Error: gammam must be smaller than gammaa."
  enddo

  ! Set da, dm and infilt_limit
  allocate( da(num_of_landuse), dm(num_of_landuse), infilt_limit(num_of_landuse))
  da(:) = 0.d0
  dm(:) = 0.d0
  infilt_limit(:) = 0.d0
  do i = 1, num_of_landuse
    if( soildepth(i) .gt. 0.d0 .and. ksv(i) .gt. 0.d0 ) infilt_limit(i) = soildepth(i) * gammaa(i)
    if( soildepth(i) .gt. 0.d0 .and. ka(i) .gt. 0.d0 ) da(i)= soildepth(i) * gammaa(i)
    if( soildepth(i) .gt. 0.d0 .and. ka(i) .gt. 0.d0 .and. gammam(i) .gt. 0.d0 ) &
    dm(i) = soildepth(i) * gammam(i)
  enddo

  ! if ksg(i) = 0.d0 -> no gw calculation
  gw_switch = 0
  do i = 1, num_of_landuse
    if( ksg(i) .gt. 0.d0 ) then
      gw_switch = 1
    else
      gammag(i) = 0.d0
      kg0(i) = 0.d0
      fpg(i) = 0.d0
      rgl(i) = 0.d0
    endif
  enddo
end subroutine prep_landcover
!===============================================================
!
!===============================================================
subroutine prep_topography()
  use mod_section, only: &
    set_section
  implicit none
  integer :: i, j

  allocate(zs(nx, ny))
  allocate(zb(nx, ny))
  allocate(zb_riv(nx, ny))
  allocate(domain(nx, ny))

  allocate(riv(nx, ny))
  allocate(acc(nx, ny))
  allocate(dir(nx, ny))

  allocate(width(nx, ny))
  allocate(depth(nx, ny))
  allocate(height(nx, ny))
  allocate(len_riv(nx, ny))
  allocate(area_ratio(nx, ny))

  call read_map(demfile, zs)
  call read_map(accfile, acc)
  call read_map(dirfile, dir)

  ! River cell (0: slope, 1: river)
  riv(:,:) = 0
  width(:,:) = 0.d0
  depth(:,:) = 0.d0
  height(:,:) = 0.d0
  len_riv(:,:) = 0.d0
  if( riv_thresh > 0 )then
    where( acc > riv_thresh ) riv = 1
    where( riv == 1 ) width = width_param_c * ( acc * dx * dy * 1d-6 ) ** width_param_s
    where( riv == 1 ) depth = depth_param_c * ( acc * dx * dy * 1d-6 ) ** depth_param_s
    where( riv == 1 .and. acc > height_limit_param ) height = height_param
    where( riv == 1 ) len_riv = length
  endif

  ! River cross section (rectangular) is replaced by the information in files
  if( rivfile_switch > 1 ) then
    riv(:,:) = 0
    riv_thresh = 1
    call read_map(widthfile, width)
    where( width > 0) riv = 1 ! river cells (if width >= 0.)
    call read_map(depthfile, depth)
    call read_map(heightfile, height)
    where( height < 0.d0 ) height = 0.d0
  endif

  ! River cross section (arbitrary) is set by section file
  allocate( sec_map(nx,ny) )
  sec_map = 0
  if( sec_switch.eq.1 ) then
    call read_map(sec_map_file, sec_map)
    sec_id_max = maxval( sec_map(:,:) )
    call set_section()
  endif
  !where( riv == 1 ) len_riv = length ! added on Dec. 27, 2021

  ! River length is set by input file
  allocate( sec_length(nx,ny) )
  sec_length(:,:) = 0
  if( sec_length_switch == 1 ) then
    call read_map(sec_length_file, sec_length)
    where( sec_length > 0.d0 ) len_riv = sec_length
  endif

  if( rivfile_switch == 2 ) then
    ! levee on both river and slope grid cells : zs is increased with height
    where( height > 0.d0 ) zs = zs + height
  else
    ! levee on only slope grid cells : zs is increased with height
    where( height > 0.d0 .and. riv == 0 ) zs = zs + height
  endif

  area_ratio(:,:) = 0.d0
  where( riv == 1 ) area_ratio = width * len_riv / area

  ! Elevations of slope bed rock (zb) and river bed (zb_riv)
  zb_riv(:,:) = zs(:,:)
  do j = 1, ny
  do i = 1, nx
    zb(i,j) = zs(i,j) - soildepth(land(i,j))
    if( riv(i,j) == 1 ) zb_riv(i,j) = zs(i,j) - depth(i,j)
  enddo
  enddo

  ! Domain mask
  domain(:,:) = DOMAIN__OUTSIDE
  num_of_cell = 0
  do j = 1, ny
  do i = 1, nx
    if( zs(i,j) <= -100.d0 ) cycle
    domain(i,j) = DOMAIN__INSIDE
    if( dir(i,j) == DIR__MOUTH .or. dir(i,j) == DIR__INLAND ) domain(i,j) = DOMAIN__OUTLET
    num_of_cell = num_of_cell + 1
  enddo
  enddo
end subroutine prep_topography
!===============================================================
!
!===============================================================
subroutine prep_riv_idx()
  implicit none

  integer :: i, j, ii, jj, k, kk
  real(8) :: distance

  riv_count = count(domain /= DOMAIN__OUTSIDE .and. riv == 1)

  allocate( riv_idx2i(riv_count), riv_idx2j(riv_count), riv_ij2idx(nx,ny) )
  allocate( down_riv_idx(riv_count), domain_riv_idx(riv_count) )
  allocate( width_idx(riv_count), depth_idx(riv_count) )
  allocate( height_idx(riv_count), area_ratio_idx(riv_count) )
  allocate( zb_riv_idx(riv_count), dis_riv_idx(riv_count) )
  allocate( dif_riv_idx(riv_count) )
  allocate( sec_map_idx(riv_count), len_riv_idx(riv_count) ) ! add v1.4

  riv_count = 0
  riv_ij2idx(:, :) = 0
  do j = 1, ny
  do i = 1, nx
    if( domain(i,j) == DOMAIN__OUTSIDE .or. riv(i,j) /= 1 ) cycle

    ! domain(i, j) = 1 or 2 and riv(i, j) = 1
    riv_count = riv_count + 1

    riv_idx2i(riv_count) = i
    riv_idx2j(riv_count) = j
    domain_riv_idx(riv_count) = domain(i, j)
    width_idx(riv_count) = width(i, j)
    depth_idx(riv_count) = depth(i, j)
    height_idx(riv_count) = height(i, j)
    area_ratio_idx(riv_count) = area_ratio(i, j)
    zb_riv_idx(riv_count) = zb_riv(i, j)
    riv_ij2idx(i, j) = riv_count
    dif_riv_idx(riv_count) = dif(land(i, j))
    sec_map_idx(riv_count) = sec_map(i, j)
    len_riv_idx(riv_count) = len_riv(i, j) ! add v1.4
  enddo  ! i/
  enddo  ! j/

  ! search for downstream gridcell (down_idx)
  riv_count = 0
  do j = 1, ny
  do i = 1, nx
    if( domain(i,j) == DOMAIN__OUTSIDE .or. riv(i,j) /= 1 ) cycle

    ! domain(i, j) = 1 or 2 and riv(i, j) = 1
    riv_count = riv_count + 1

    selectcase( dir(i,j) )
    ! right
    case( DIR__EAST )
      ii = i + 1
      jj = j
      distance=dx
    ! right down
    case( DIR__SOUTHEAST )
      ii = i + 1
      jj = j + 1
      distance=sqrt(dx*dx+dy*dy)
    ! down
    case( DIR__SOUTH )
      ii = i
      jj = j + 1
      distance=dy
    ! left down
    case( DIR__SOUTHWEST )
      ii = i - 1
      jj = j + 1
      distance=sqrt(dx*dx+dy*dy)
    ! left
    case( DIR__WEST )
      ii = i - 1
      jj = j
      distance=dx
    ! left up
    case( DIR__NORTHWEST )
      ii = i - 1
      jj = j - 1
      distance=sqrt(dx*dx+dy*dy)
    ! up
    case( DIR__NORTH )
      ii = i
      jj = j - 1
      distance=dy
    ! right up
    case( DIR__NORTHEAST )
      ii = i + 1
      jj = j - 1
      distance=sqrt(dx*dx+dy*dy)
    ! outlet
    case( DIR__MOUTH, DIR__INLAND )
      ii = i
      jj = j
    case default
      write(*,*) "dir(i, j) is error (", i, j, ")", dir(i, j)
      stop
    endselect

    ! If the downstream cell is outside the domain, set domain(i, j) = 2
    if( ii < 1 .or. ii > nx .or. jj < 1 .or. jj > ny )then
      domain(i, j) = DOMAIN__OUTLET
      dir(i, j) = DIR__MOUTH
      ii = i
      jj = j
    endif
    if( domain(ii, jj) == DOMAIN__OUTSIDE ) then
      domain(i, j) = DOMAIN__OUTLET
      dir(i, j) = DIR__MOUTH
      ii = i
      jj = j
    endif

    if( riv(ii,jj) == 0 ) then
      write(*,*) "riv(ii, jj) should be 1 (", i, j, ")", "(", ii, jj, ")"
      stop
    endif

    dis_riv_idx(riv_count) = distance
    down_riv_idx(riv_count) = riv_ij2idx(ii, jj)
  enddo  ! i/
  enddo  ! j/

  ! add v1.4
  if( sec_length_switch .eq. 1 ) then
    do k = 1, riv_count
      kk = down_riv_idx(k)
      dis_riv_idx(k) = ( len_riv_idx(k) + len_riv_idx(kk) ) / 2.d0
    enddo
  endif
end subroutine prep_riv_idx
!===============================================================
!
!===============================================================
subroutine prep_slo_idx()
  implicit none

  integer :: i, j, ii, jj, l
  real(8) :: distance, len, l1, l2, l3
  real(8) :: l1_kin, l2_kin, l3_kin

  slo_count = count(domain /= DOMAIN__OUTSIDE)

  allocate( slo_idx2i(slo_count), slo_idx2j(slo_count), slo_ij2idx(nx,ny) )
  allocate( down_slo_idx(I4, slo_count), domain_slo_idx(slo_count) )
  allocate( zb_slo_idx(slo_count), dis_slo_idx(I4, slo_count), &
            len_slo_idx(I4, slo_count), acc_slo_idx(slo_count) )
  allocate( down_slo_1d_idx(slo_count), dis_slo_1d_idx(slo_count), len_slo_1d_idx(slo_count) )
  allocate( land_idx(slo_count) )

  allocate( dif_slo_idx(slo_count) )
  allocate( ns_slo_idx(slo_count) )
  allocate( soildepth_idx(slo_count) )
  allocate( gammaa_idx(slo_count) )

  allocate( ksv_idx(slo_count), faif_idx(slo_count), infilt_limit_idx(slo_count) )
  allocate( ka_idx(slo_count), gammam_idx(slo_count), beta_idx(slo_count), &
            da_idx(slo_count), dm_idx(slo_count) )
  allocate( ksg_idx(slo_count), gammag_idx(slo_count), kg0_idx(slo_count), &
            fpg_idx(slo_count), rgl_idx(slo_count) )

  slo_count = 0
  slo_ij2idx(:,:) = 0
  do j = 1, ny
  do i = 1, nx
    if( domain(i,j) == 0 ) cycle

    slo_count = slo_count + 1
    slo_idx2i(slo_count) = i
    slo_idx2j(slo_count) = j
    domain_slo_idx(slo_count) = domain(i,j)
    zb_slo_idx(slo_count) = zb(i,j)
    acc_slo_idx(slo_count) = acc(i,j)
    slo_ij2idx(i,j) = slo_count
    land_idx(slo_count) = land(i,j)

    dif_slo_idx(slo_count) = dif(land(i,j))
    ns_slo_idx(slo_count) = ns_slope(land(i,j))
    soildepth_idx(slo_count) = soildepth(land(i,j))
    gammaa_idx(slo_count) = gammaa(land(i,j))

    ksv_idx(slo_count) = ksv(land(i,j))
    faif_idx(slo_count) = faif(land(i,j))
    infilt_limit_idx(slo_count) = infilt_limit(land(i,j))

    ka_idx(slo_count) = ka(land(i,j))
    gammam_idx(slo_count) = gammam(land(i,j))
    beta_idx(slo_count) = beta(land(i,j))
    da_idx(slo_count) = da(land(i,j))
    dm_idx(slo_count) = dm(land(i,j))

    ksg_idx(slo_count) = ksg(land(i,j))
    gammag_idx(slo_count) = gammag(land(i,j))
    kg0_idx(slo_count) = kg0(land(i,j))
    fpg_idx(slo_count) = fpg(land(i,j))
    rgl_idx(slo_count) = rgl(land(i,j))
  enddo
  enddo

  selectcase( eight_dir )
  case( 1 )
    ! Hromadka etal, JAIH2006 (USE THIS AS A DEFAULT)
    ! 8-direction
    lmax = 4
    l1 = dy / 2.d0
    l2 = dx / 2.d0
    l3 = sqrt(dx ** 2.d0 + dy ** 2.d0) / 4.d0
  case( 0 )
    ! 4-direction
    lmax = 2
    l1 = dy
    l2 = dx
    l3 = 0.d0
  case default
    stop "error: eight_dir should be 0 or 1."
  endselect

  ! search for downstream gridcell (down_slo_idx)
  slo_count = 0
  down_slo_idx(:,:) = -1

  do j = 1, ny
  do i = 1, nx
    if( domain(i,j) == 0 ) cycle
    slo_count = slo_count + 1

    ! 8-direction: lmax = 4, 4-direction: lmax = 2
    do l = 1, lmax ! (1: right，2: down, 3: right down, 4: left down)

      selectcase( l )
      case( 1 )
        ii = i + 1
        jj = j
        distance = dx
        len = l1
      case( 2 )
        ii = i
        jj = j + 1
        distance = dy
        len = l2
      case( 3 )
        ii = i + 1
        jj = j + 1
        distance = sqrt( dx * dx + dy * dy )
        len = l3
      case( 4 )
        ii = i - 1
        jj = j + 1
        distance = sqrt( dx * dx + dy * dy )
        len = l3
      endselect

      if( ii < 1 .or. ii > nx .or. jj < 1 .or. jj > ny ) cycle
      if( domain(ii,jj) == 0 ) cycle

      down_slo_idx(l, slo_count) = slo_ij2idx(ii, jj)
      dis_slo_idx(l, slo_count) = distance
      len_slo_idx(l, slo_count) = len
    enddo
  enddo  ! i/
  enddo  ! j/

  ! search for downstream gridcell (down_slo_1d_idx) (used only for kinematic with 1-direction)
  slo_count = 0
  down_slo_1d_idx(:) = -1
  dis_slo_1d_idx(:) = l1
  len_slo_1d_idx(:) = dx

  l1_kin = dy
  l2_kin = dx
  l3_kin = dx * dy / sqrt( dx ** 2.d0 + dy ** 2.d0 )

  do j = 1, ny
  do i = 1, nx
    if( domain(i,j) == 0 ) cycle
    slo_count = slo_count + 1

    selectcase( dir(i,j) )
    ! right
    case( DIR__EAST )
      ii = i + 1
      jj = j
      distance = dx
      len = l1_kin
    ! right down
    case( DIR__SOUTHEAST )
      ii = i + 1
      jj = j + 1
      distance=sqrt(dx*dx+dy*dy)
      len = l3_kin
    ! down
    case( DIR__SOUTH )
      ii = i
      jj = j + 1
      distance = dy
      len = l2_kin
    ! left down
    case( DIR__SOUTHWEST )
      ii = i - 1
      jj = j + 1
      distance = sqrt(dx*dx+dy*dy)
      len = l3_kin
    ! left
    case( DIR__WEST )
      ii = i - 1
      jj = j
      distance = dx
      len = l1_kin
    ! left up
    case( DIR__NORTHWEST )
      ii = i - 1
      jj = j - 1
      distance = sqrt(dx*dx+dy*dy)
      len = l3_kin
    ! up
    case( DIR__NORTH )
      ii = i
      jj = j - 1
      distance = dy
      len = l2_kin
    ! right up
    case( DIR__NORTHEAST )
      ii = i + 1
      jj = j - 1
      distance = sqrt(dx*dx+dy*dy)
      len = l3_kin
    ! outlet
    case( DIR__MOUTH, DIR__INLAND )
      ii = i
      jj = j
      distance = dx
      len = l1_kin
    case default
      write(*,*) "dir(i, j) is error (", i, j, ")", dir(i, j)
      stop
    endselect

    if( ii < 1 .or. ii > nx .or. jj < 1 .or. jj > ny ) cycle
    if( domain(ii,jj) == 0 ) cycle

    down_slo_1d_idx(slo_count) = slo_ij2idx(ii, jj)
    dis_slo_1d_idx(slo_count) = distance
    len_slo_1d_idx(slo_count) = len
  enddo  ! i/
  enddo  ! j/
end subroutine prep_slo_idx
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
subroutine read_map__int4(f, dat)
  implicit none
  character(*), intent(in) :: f
  integer(4), intent(out) :: dat(:,:)

  integer :: j
  integer :: un

  open(newunit=un, file=f, status='old')
  do j = 1, 6
    read(un,*)
  enddo
  do j = 1, ny
    read(un,*) dat(:,j)
  enddo
  close(un)
end subroutine read_map__int4
!===============================================================
!
!===============================================================
subroutine read_map__dble(f, dat)
  implicit none
  character(*), intent(in) :: f
  real(8), intent(out) :: dat(:,:)

  integer :: j
  integer :: un

  open(newunit=un, file=f, status='old')
  do j = 1, 6
    read(un,*)
  enddo
  do j = 1, ny
    read(un,*) dat(:,j)
  enddo
  close(un)
end subroutine read_map__dble
!===============================================================
!
!===============================================================
subroutine hubeny( x1_deg, y1_deg, x2_deg, y2_deg, d )
  implicit none
  real(8), intent(in) :: x1_deg, y1_deg, x2_deg, y2_deg
  real(8), intent(out) :: d

  real(8) :: x1, y1, x2, y2
  real(8) :: dx, dy, mu, a, b, e, W, N, M

  x1 = deg2rad(x1_deg)
  y1 = deg2rad(y1_deg)
  x2 = deg2rad(x2_deg)
  y2 = deg2rad(y2_deg)

  dy = y1 - y2
  dx = x1 - x2
  mu = (y1 + y2) / 2.

  a = 6378137.000d0 ! Semi-Major Axis
  b = 6356752.314d0 ! Semi-Minor Axis

  e = sqrt((a**2.d0 - b**2.d0) / (a**2.d0))

  W = sqrt(1. - e**2.d0 * (sin(mu))**2.d0)

  N = a / W

  M = a * (1. - e ** 2.d0) / W**(3.d0)

  d = sqrt((dy * M) ** 2.d0 + (dx * N * cos(mu)) ** 2.d0)
end subroutine hubeny
!===============================================================
!
!===============================================================
real(8) function deg2rad(deg) result(rad)
  implicit none
  real(8), intent(in) ::deg

  rad = deg * PI / 180.d0
end function deg2rad
!===============================================================
!
!===============================================================
subroutine s2date(s, d)
  implicit none
  character(*), intent(in) :: s
  integer, intent(out) :: d(:)

  ! yyyy-mm-dd?HH:MM
  read(s(1:4),*) d(1)
  read(s(6:7),*) d(2)
  read(s(9:10),*) d(3)

  ! TMP
  read(s(11:11),*)

  read(s(12:13),*) d(4)
  read(s(15:16),*) d(5)
  d(6) = 0
end subroutine s2date
!===============================================================
!
!===============================================================
end module mod_config
