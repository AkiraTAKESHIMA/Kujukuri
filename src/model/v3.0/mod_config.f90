module mod_config
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: prep_static_data
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_map
    module procedure read_map__int4
    module procedure read_map__dble
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_config'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_static_data()
  implicit none

  character(CLEN_PROC), parameter :: PRCNAM = 'prep_static_data'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_config()

  call setup_domain()

  call prep_landcover()

  call prep_topography()

  call prep_slo_idx()

  call prep_river()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_static_data
!===============================================================
!
!===============================================================
subroutine read_config()
  implicit none

  character(CLEN_PROC), parameter :: PRCNAM = 'read_config'
  !character(32) :: sdate_start, sdate_end
  character(CLEN_PATH) :: f_conf
  integer :: un
  integer :: access
  integer :: ios
  character(6) :: c

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_command_argument(1, f_conf)
  if( access(f_conf,' ') /= 0 )then
    write(0,"(a)") 'Configuration file not found: '//trim(f_conf)
    stop 1
  endif
  open(newunit=un, file=f_conf, status='old')

  read(un,*)

  read(un,*)
  read(un,*) rainfile
  read(un,*) demfile
  read(un,*) dirfile
  read(un,*) upafile
  print"(1x,a)", 'rain: '//trim(rainfile)
  print"(1x,a)", 'dem: '//trim(demfile)
  print"(1x,a)", 'dir: '//trim(dirfile)
  print"(1x,a)", 'upa: '//trim(upafile)

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
  print"(1x,a,f10.3)", 'dt_model: ',dt_model
  print"(1x,a,f10.3)", 'dt_slope: ',dt_slo
  print"(1x,a,f10.3)", 'dt_river: ',dt_riv

  ! TMP
  nt_model = time_end / dt_model

  read(un,*)
  read(un,*) dt_out
  read(un,*) dir_out

  if( dir_out(len_trim(dir_out):) == '/' )then
    dir_out = dir_out(:len_trim(dir_out)-1)
  endif
  print*
  print"(1x,a,f10.2)", 'dt_out: ', dt_out
  print"(1x,a)", 'dir_out: '//trim(dir_out)

  ! Check output directory
  open(11, file=trim(dir_out)//'/tmp', status='replace', iostat=ios)
  if( ios /= 0 )then
    print"(a)", 'Failed to make a new file in the output directory.'
    stop 1
  endif
  close(11, status='delete')

  read(un,*)
  read(un,*) eight_dir

  read(un,*)
  read(un,*) ns_river
  print*
  print"(1x,a,f12.3)", 'ns_river: ', ns_river

  read(un,*)
  read(un,*) num_of_landuse
  print*
  print"(1x,a,i5)", 'num_of_landuse: ', num_of_landuse

  allocate(flow(num_of_landuse))
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

  read(un,*) flow(:)
  read(un,*) ns_slope(:)
  read(un,*) soildepth(:)
  read(un,*) gammaa(:)

  c = str(num_of_landuse)
  print"(1x,a,"//trim(c)//"(1x,i12))", 'flow:', flow(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'ns_slope :', ns_slope(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'soildepth:', soildepth(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'gammaa   :', gammaa(:)

  read(un,*) 
  read(un,*) ksv(:)
  read(un,*) faif(:)

  print*
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'ksv      :', ksv(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'faif     :', faif(:)

  read(un,*) 
  read(un,*) ka(:)
  read(un,*) gammam(:)
  read(un,*) beta(:)
  print*
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'ka       :', ka(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'gammam   :', gammam(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'beta     :', beta(:)

  read(un,*) 
  read(un,*) ksg(:)
  read(un,*) gammag(:)
  read(un,*) kg0(:)
  read(un,*) fpg(:)
  read(un,*) rgl(:)
  print*
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'ksg      :', ksg(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'gammag   :', gammag(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'kg0      :', kg0(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'fpg      :', fpg(:)
  print"(1x,a,"//trim(c)//"(1x,f12.3))", 'rgl      :', rgl(:)

  read(un,*)
  read(un,*) file_riv_network
  print"(1x,a)", 'network: '//trim(file_riv_network)

  read(un,*) 
  read(un,*) switch_riv_crssct
  read(un,*) width_param_c
  read(un,*) width_param_s
  read(un,*) width_llim
  read(un,*) depth_param_c
  read(un,*) depth_param_s
  read(un,*) depth_llim
  read(un,*) height_param
  read(un,*) height_limit_param
  read(un,*) widthfile
  read(un,*) depthfile
  read(un,*) heightfile
  read(un,*) sec_map_file
  read(un,*) sec_file

  selectcase( switch_riv_crssct )
  case( SWITCH_RIV_CRSSCT__REGIME )
    call logmsg('cross section: regime')
    call logmsg('width s: '//str(width_param_s)//' c: '//str(width_param_c))
    call logmsg('depth s: '//str(depth_param_s)//' c: '//str(depth_param_c))
    call logmsg('levee height: '//str(height_param)//' upa_thresh: '//str(height_limit_param))
  case( SWITCH_RIV_CRSSCT__RECTANGLE )

  case( SWITCH_RIV_CRSSCT__ARBITRARY )

  case default
    call errend(msg_invalid_value('switch_riv_crssct', switch_riv_crssct))
  endselect

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
  if( land_switch == 1 ) print"(1x,a)", 'landfile: '//trim(landfile)

  read(un,*)
  read(un,*) dam_switch
  read(un,"(a)") damfile

  read(un,*)
  read(un,*) evp_switch
  read(un,"(a)") evpfile

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
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
  read(un,*) ctmp, west
  read(un,*) ctmp, south
  read(un,*) ctmp, cellsize
  read(un,*) ! nodata
  close(un)
  print*, 'nx: ',nx,' ny: ',ny

  east = west + nx * cellsize
  north = south + ny * cellsize

  ! Length of sides
  call hubeny( west, south, east, south, d1 )  ! south
  call hubeny( west, north, east, north, d2 )  ! north
  call hubeny( west, south, west, north, d3 )  ! west
  call hubeny( east, south, east, north, d4 )  ! east
  dx = (d1 + d2) / 2.d0 / real(nx)
  dy = (d3 + d4) / 2.d0 / real(ny)
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
  allocate(domain(nx, ny))

  allocate(dir(nx, ny))
  allocate(upa(nx, ny))

  call read_map(demfile, zs)
  call read_map(dirfile, dir)
  call read_map(upafile, upa)

  ! elevations of slope bed rock (zb)
  do j = 1, ny
  do i = 1, nx
    zb(i,j) = zs(i,j) - soildepth(land(i,j))
  enddo
  enddo

  ! domain mask
  domain(:,:) = DOMAIN__OUTSIDE
  num_of_cell = 0
  do j = 1, ny
  do i = 1, nx
    if( zs(i,j) <= ZS_THRESH ) cycle
    domain(i,j) = DOMAIN__INSIDE
    num_of_cell = num_of_cell + 1
  enddo
  enddo

  call logmsg('zs  min: '//str(minval(zs,mask=domain==DOMAIN__INSIDE))//&
                 ' max: '//str(maxval(zs,mask=domain==DOMAIN__INSIDE)))
  call logmsg('zb  min: '//str(minval(zb,mask=domain==DOMAIN__INSIDE))//&
                 ' max: '//str(maxval(zb,mask=domain==DOMAIN__INSIDE)))
  call logmsg('upa min: '//str(minval(upa,mask=domain==DOMAIN__INSIDE))//&
                 ' max: '//str(maxval(upa,mask=domain==DOMAIN__INSIDE)))
end subroutine prep_topography
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
            len_slo_idx(I4, slo_count) )
  allocate( down_slo_1d_idx(slo_count), dis_slo_1d_idx(slo_count), len_slo_1d_idx(slo_count) )
  allocate( land_idx(slo_count) )

  allocate( flow_slo_idx(slo_count) )
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
    if( domain(i,j) == DOMAIN__OUTSIDE ) cycle

    slo_count = slo_count + 1
    slo_idx2i(slo_count) = i
    slo_idx2j(slo_count) = j
    domain_slo_idx(slo_count) = domain(i,j)
    zb_slo_idx(slo_count) = zb(i,j)
    slo_ij2idx(i,j) = slo_count
    land_idx(slo_count) = land(i,j)

    flow_slo_idx(slo_count) = flow(land(i,j))
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
subroutine prep_river()
  implicit none

  call read_river_network()

  call prep_river_grid_isct()

  call prep_river_topography()

  call prep_river_connection()
end subroutine prep_river
!===============================================================
!
!===============================================================
subroutine read_river_network()
  implicit none

  type(channel_), pointer :: ch
  integer :: k
  integer :: jPoint

  integer :: un
  character :: c_

  open(newunit=un, file=file_riv_network, status='old')
  read(un,*) c_, riv_count

  allocate(channel(riv_count))

  do k = 1, riv_count
    ch => channel(k)
    allocate(ch%node(2))

    read(un,*) ! index
    read(un,*) ! original index
    read(un,*) c_, ch%node(:)%is_outlet
    read(un,*) c_, ch%node(:)%dist_to_mouth
    read(un,*) c_, ch%nPoint

    allocate(ch%point(ch%nPoint))
    read(un,*) c_, ch%point(:)%lon
    read(un,*) c_, ch%point(:)%lat

    ch%is_outlet = any(ch%node(:)%is_outlet)
    ch%node(1)%lon = ch%point(1)%lon
    ch%node(1)%lat = ch%point(1)%lat
    ch%node(2)%lon = ch%point(ch%nPoint)%lon
    ch%node(2)%lat = ch%point(ch%nPoint)%lat

    ch%leng = 0.d0
    do jPoint = 1, ch%nPoint-1
      call add(ch%leng, &
        dist_sphere( &
          ch%point(jPoint)%lon*D2R, ch%point(jPoint)%lat*D2R, &
          ch%point(jPoint+1)%lon*D2R, ch%point(jPoint+1)%lat*D2R &
        ) &
      )
    enddo  ! jPoint/
    call mul(ch%leng, EARTH_R)

    if( ch%node(1)%dist_to_mouth > ch%node(2)%dist_to_mouth )then
      ch%node(1)%stat_updown = NODE_STAT_UPDOWN__UP
      ch%node(2)%stat_updown = NODE_STAT_UPDOWN__DOWN
    elseif( ch%node(1)%dist_to_mouth < ch%node(2)%dist_to_mouth )then
      ch%node(1)%stat_updown = NODE_STAT_UPDOWN__DOWN
      ch%node(2)%stat_updown = NODE_STAT_UPDOWN__UP
    else
      ch%node(1)%stat_updown = NODE_STAT_UPDOWN__UNKNOWN
      ch%node(2)%stat_updown = NODE_STAT_UPDOWN__UNKNOWN
    endif
  enddo  ! k/

  close(un)
end subroutine read_river_network
!===============================================================
!
!===============================================================
subroutine prep_river_grid_isct()
  use lib_array
  use lib_math
  use mod_base, only: &
    slonlat
  implicit none

  type(channel_), pointer :: ch
  integer :: xs, xe, ys, ye
  real(8) :: wlon, wlat, elon, elat
  real(8) :: clon_west, clat_west, clon_east, clat_east
  real(8) :: dlon_west, dlat_west, dlon_east, dlat_east
  real(8) :: leng
  integer :: nGrid_tmp

  integer :: k
  integer :: ix, iy
  integer :: jPoint
  integer :: iGrid

!  integer :: k_debug = 490
!  logical :: debug_this
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do k = 1, riv_count
    ch => channel(k)

!    debug_this = k == k_debug

    ch%isct%nGrid = 0

    xs = xs_of_lon(minval(ch%node(:)%lon))
    xe = xe_of_lon(maxval(ch%node(:)%lon))
    ys = ys_of_lat(maxval(ch%node(:)%lat))
    ye = ye_of_lat(minval(ch%node(:)%lat))
    nGrid_tmp = (xe - xs + 1) * (ye - ys + 1) * 4
    allocate(ch%isct%x(nGrid_tmp))
    allocate(ch%isct%y(nGrid_tmp))
    allocate(ch%isct%leng(nGrid_tmp))
    allocate(ch%isct%domain(nGrid_tmp))

    do jPoint = 1, ch%nPoint-1
      if( ch%point(jPoint)%lon < ch%point(jPoint+1)%lon )then
        wlon = ch%point(jPoint)%lon
        wlat = ch%point(jPoint)%lat
        elon = ch%point(jPoint+1)%lon
        elat = ch%point(jPoint+1)%lat
      else
        wlon = ch%point(jPoint+1)%lon
        wlat = ch%point(jPoint+1)%lat
        elon = ch%point(jPoint)%lon
        elat = ch%point(jPoint)%lat
      endif

      ys = ys_of_lat(max(wlat, elat))
      ye = ye_of_lat(min(wlat, elat))
      !call logmsg('y: '//str((/ys,ye/),' - '))
      !---------------------------------------------------------
      ! Case: north to south
      if( wlat > elat )then
        do iy = ys, ye
          if( iy == ys )then
            clat_west = wlat
            clon_west = wlon
          else
            clat_west = clat_east
            clon_west = clon_east
          endif
          if( iy == ye )then
            clat_east = elat
            clon_east = elon
          else
            clat_east = south_of_y(iy)
            clon_east = apprx_isct_with_parallel(&
              wlon, wlat, elon, elat, clat_east)
          endif

          call loop_for_x()
        enddo  ! iy/
      !---------------------------------------------------------
      ! Case: south to north
      else
        do iy = ye, ys, -1
          if( iy == ye )then
            clat_west = wlat
            clon_west = wlon
          else
            clat_west = clat_east
            clon_west = clon_east
          endif
          if( iy == ys )then
            clat_east = elat
            clon_east = elon
          else
            clat_east = north_of_y(iy)
            clon_east = apprx_isct_with_parallel(&
              wlon, wlat, elon, elat, clat_east)
          endif

          call loop_for_x()
        enddo  ! iy/
      endif
    enddo  ! jPoint/

    call realloc(ch%isct%x, ch%isct%nGrid, clear=.false.)
    call realloc(ch%isct%y, ch%isct%nGrid, clear=.false.)
    call realloc(ch%isct%leng, ch%isct%nGrid, clear=.false.)
    call realloc(ch%isct%domain, ch%isct%nGrid, clear=.false.)

!    if( debug_this )then
!      call logmsg('nGrid: '//str(ch%isct%nGrid)//&
!        ' x: '//str((/minval(ch%isct%x),maxval(ch%isct%x)/),' - ')//&
!        ' y: '//str((/minval(ch%isct%y),maxval(ch%isct%y)/),' - '))
!    endif

    ch%isct%leng_domain = 0.d0
    do iGrid = 1, ch%isct%nGrid
      if( ch%isct%domain(iGrid) == DOMAIN__OUTSIDE ) cycle
      call add(ch%isct%leng_domain, ch%isct%leng(iGrid))
!      if( debug_this )then
!        call logmsg('grid#'//str(iGrid)//' leng '//str(ch%isct%leng(iGrid)))
!      endif
    enddo

    if( any(ch%isct%domain /= DOMAIN__OUTSIDE) .and. &
        ch%isct%leng_domain < LENG_DOMAIN_THRESH )then
      call errend('ch#'//str(k)//' leng_domain < '//str(LENG_DOMAIN_THRESH)//&
        '\nleng_domain: '//str(ch%isct%leng_domain)//&
        '\nleng: '//str(ch%leng))
    endif
  enddo  ! k/
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine loop_for_x()
  implicit none

  xs = xs_of_lon(clon_west)
  xe = xe_of_lon(clon_east)

  !call logmsg('x: '//str((/xs,xe/),' - '))

  nGrid_tmp = ch%isct%nGrid + (xe - xs + 1)
  if( size(ch%isct%x) < nGrid_tmp )then
    call realloc(ch%isct%x, nGrid_tmp*2, clear=.false.)
    call realloc(ch%isct%y, nGrid_tmp*2, clear=.false.)
    call realloc(ch%isct%leng, nGrid_tmp*2, clear=.false.)
    call realloc(ch%isct%domain, nGrid_tmp*2, clear=.false.)
  endif

  do ix = xs, xe
    if( ix == xs )then
      dlon_west = clon_west
      dlat_west = clat_west
    else
      dlon_west = dlon_east
      dlat_west = dlat_east
    endif
    if( ix == xe )then
      dlon_east = clon_east
      dlat_east = clat_east
    else
      dlon_east = east_of_x(ix)
      dlat_east = apprx_isct_with_meridian(&
        wlon, wlat, elon, elat, dlon_east)
    endif

    leng = dist_sphere(&
      dlon_west*d2r, dlat_west*d2r, &
      dlon_east*d2r, dlat_east*d2r) * EARTH_R

    if( domain(ix,iy) /= DOMAIN__OUTSIDE .and. leng < LENG_DOMAIN_THRESH )then
      call logmsg('ch#'//str(k)//' p#'//str(jPoint)//' ('//&
        slonlat(wlon,wlat)//')'//' - p#'//str(jPoint+1)//' ('//&
        slonlat(elon,elat)//')'//&
        '\n  isct ('//slonlat(dlon_west,dlat_west)//') - ('//&
        slonlat(dlon_east,dlat_east)//') leng: '//str(leng))
    endif

    call add(ch%isct%nGrid)
    ch%isct%x(ch%isct%nGrid) = ix
    ch%isct%y(ch%isct%nGrid) = iy
    ch%isct%leng(ch%isct%nGrid) = leng
    ch%isct%domain(ch%isct%nGrid) = domain(ix,iy)

!    if( debug_this )then
!      call logmsg('ch#'//str(k)//' p#'//str(jPoint)//' ('//&
!        slonlat(wlon,wlat)//')'//' - p#'//str(jPoint+1)//' ('//&
!        slonlat(elon,elat)//')'//&
!        '\n  isct#'//str(ch%isct%nGrid)//&
!        ' ('//slonlat(dlon_west,dlat_west)//') - ('//&
!        slonlat(dlon_east,dlat_east)//')'//&
!        ' domain: '//str(domain(ix,iy))//' leng: '//str(leng))
!    endif

  enddo  ! ix/
end subroutine
!---------------------------------------------------------------
!
!---------------------------------------------------------------
end subroutine prep_river_grid_isct
!===============================================================
!
!===============================================================
subroutine prep_river_topography()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_river_topography'

  type(channel_), pointer :: ch
  real(8), allocatable :: lst_leng(:)
  real(8), allocatable :: lst_upa(:)
  integer, allocatable :: arg(:)
  real(8) :: leng_tmp
  integer :: k
  integer :: iGrid

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Get width, depth
  !-------------------------------------------------------------

  ! get median of upper area
  !-------------------------------------------------------------
  do k = 1, riv_count
    ch => channel(k)

    allocate(lst_leng(ch%isct%nGrid))
    allocate(lst_upa(ch%isct%nGrid))
    lst_leng(:) = 0.d0
    lst_upa(:) = 0.d0
    do iGrid = 1, ch%isct%nGrid
      if( ch%isct%domain(iGrid) == DOMAIN__OUTSIDE ) cycle
      lst_leng(iGrid) = ch%isct%leng(iGrid)
      lst_upa(iGrid) = upa(ch%isct%x(iGrid), ch%isct%y(iGrid))
    enddo

    allocate(arg(ch%isct%nGrid))
    call argsort(lst_upa, arg)
    call sort(lst_upa, arg)
    call sort(lst_leng, arg)
    deallocate(arg)

    leng_tmp = 0.d0
    ch%upa = -1d20
    do iGrid = 1, ch%isct%nGrid
      leng_tmp = leng_tmp + ch%isct%leng(iGrid)
      if( leng_tmp >= ch%isct%leng_domain*0.5d0 )then
        ch%upa = lst_upa(iGrid)
        exit
      endif
    enddo
    !call logmsg('ch#'//str(k)//' upa: '//str(ch%upa)//&
    !  ' (min: '//str(minval(lst_upa))//' max: '//str(maxval(lst_upa))//')')

    if( ch%upa == -1d20 )then
      call errend('ch#'//str(k)//' upper area was not obtained.')
    endif

    deallocate(lst_leng)
    deallocate(lst_upa)
  enddo  ! k = 1, riv_count

  ! TMP
  ! modify upper area
  do k = 1, riv_count
    ch => channel(k)

  enddo  ! k/

  ! get width and depth
  !-------------------------------------------------------------
  selectcase( switch_riv_crssct )
  !-------------------------------------------------------------
  ! Case: regime theory
  case( SWITCH_RIV_CRSSCT__REGIME )
    do k = 1, riv_count
      ch => channel(k)

      ch%width = max(ch%upa ** width_param_s * width_param_c, width_llim)
      ch%depth = max(ch%upa ** depth_param_s * depth_param_c, depth_llim)

      ! TMP
      ch%height = 0.d0
    enddo  ! k/
  !-------------------------------------------------------------
  ! Case: rectangle; read from file
  case( SWITCH_RIV_CRSSCT__RECTANGLE )
    do k = 1, riv_count
      ch => channel(k)

      if( ch%width < width_llim )then
        print"(1x,a)", '[ERROR] width is smaller than the lower limit.'
        stop
      endif
      if( ch%depth < depth_llim )then
        print"(1x,a)", '[ERROR] depth is smaller than the lower limit.'
        stop
      endif
    enddo  ! k/
  !-------------------------------------------------------------
  ! Case: arbitrary; read from file
  case( SWITCH_RIV_CRSSCT__ARBITRARY )
    do k = 1, riv_count
      ch => channel(k)

    enddo  ! k/
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('switch_riv_crssct', switch_riv_crssct))
  endselect

  call logmsg('width min: '//str(minval(channel(:)%width))//&
                   ' max: '//str(maxval(channel(:)%width)))
  call logmsg('depth min: '//str(minval(channel(:)%depth))//&
                   ' max: '//str(maxval(channel(:)%depth)))
  call logmsg('levee min: '//str(minval(channel(:)%height))//&
                   ' max: '//str(maxval(channel(:)%height)))
  !-------------------------------------------------------------
  ! Calc. elevation
  !-------------------------------------------------------------
  do k = 1, riv_count
    ch => channel(k)

    ! mean surface elevation
    ch%zs = 0.d0
    do iGrid = 1, ch%isct%nGrid
      if( ch%isct%domain(iGrid) == DOMAIN__OUTSIDE ) cycle
      ch%zs = ch%zs + zs(ch%isct%x(iGrid),ch%isct%y(iGrid)) &
                * ch%isct%leng(iGrid) / ch%isct%leng_domain
    enddo

    ! mean bed rock elevation
    ch%zb = ch%zs - ch%depth
  enddo  ! k/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_river_topography
!===============================================================
!
!===============================================================
subroutine prep_river_connection()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_river_connection'

  type(channel_), pointer :: ch, ch2
  type(ch_node_), pointer :: chnode
  type(nwk_node_), pointer :: node
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_iCh(:), lst_jNode(:)
  integer, allocatable :: arg(:)
  integer :: nNode, kNode, is, ie, iis, iie
  integer :: iNode
  integer :: jNode
  integer :: iiCh, iiCh_down, iiCh2
  integer :: iCh, iCh2
  logical :: is_found

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Construct network
  !-------------------------------------------------------------
  call logent('constructing network')

  nNode = riv_count * 2
  allocate(lst_lon(nNode))
  allocate(lst_lat(nNode))
  allocate(lst_iCh(nNode))
  allocate(lst_jNode(nNode))

  ! make a list of endpoint nodes
  kNode = 0
  do iCh = 1, riv_count
    ch => channel(iCh)

    do jNode = 1, 2
      kNode = kNode + 1
      lst_lon(kNode) = ch%node(jNode)%lon
      lst_lat(kNode) = ch%node(jNode)%lat
      lst_iCh(kNode) = iCh
      lst_jNode(kNode) = jNode
    enddo
  enddo

  allocate(arg(nNode))
  call argsort(lst_lon, arg)
  call sort(lst_lon  , arg)
  call sort(lst_lat  , arg)
  call sort(lst_iCh  , arg)
  call sort(lst_jNode, arg)

  nwk%nNode = 0
  ie = 0
  do while( ie < nNode )
    is = ie + 1
    ie = is
    do while( ie < nNode )
      if( lst_lon(ie+1) /= lst_lon(is) ) exit
      ie = ie + 1
    enddo
    call argsort(lst_lat(is:ie), arg(is:ie))
    call sort(lst_lat(is:ie), arg(is:ie))
    call sort(lst_iCh(is:ie), arg(is:ie))
    call sort(lst_jNode(is:ie), arg(is:ie))

    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( lst_lat(iie+1) /= lst_lat(iis) ) exit
        iie = iie + 1
      enddo
      nwk%nNode = nwk%nNode + 1
    enddo  ! iie/
  enddo  ! ie/

  allocate(nwk%node(nwk%nNode))

  call logext()
  !-------------------------------------------------------------
  ! Make lists of adjacent channels
  !-------------------------------------------------------------
  call logent('making lists of adjacent channels')

  iNode = 0
  ie = 0
  do while( ie < size(lst_lon) )
    is = ie + 1
    ie = is
    do while( ie < size(lst_lon) )
      if( lst_lon(ie+1) /= lst_lon(is) ) exit
      ie = ie + 1
    enddo
  
    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( lst_lat(iie+1) /= lst_lat(iis) ) exit
        iie = iie + 1
      enddo

      iNode = iNode + 1
      node => nwk%node(iNode)
      node%nCh = iie - iis + 1

      allocate(node%iCh(node%nCh))

      do kNode = iis, iie
        iiCh = kNode - iis + 1
        node%iCh(iiCh) = lst_iCh(kNode)
        ch => channel(lst_iCh(kNode))
        ch%node(lst_jNode(kNode))%iNode = iNode
      enddo  ! iiCh/
    enddo  ! iie/
  enddo  ! ie/

  do iCh = 1, riv_count
    ch => channel(iCh)

    ch%nCh_up = 0
    do jNode = 1, 2
      chnode => ch%node(jNode)
      node => nwk%node(chnode%iNode)
      call add(ch%nCh_up, node%nCh-1)
    enddo  ! jNode/

    allocate(ch%iCh_up(ch%nCh_up))
    allocate(ch%iCh_down(ch%nCh_up))
    ch%nCh_up = 0
    ch%nCh_down = 0
    do jNode = 1, 2
      chnode => ch%node(jNode)
      node => nwk%node(chnode%iNode)
      do iiCh = 1, node%nCh
        iCh2 = node%iCh(iiCh)
        if( iCh2 == iCh ) cycle
        ch2 => channel(iCh2)

        if( ch%dist_to_mouth < ch2%dist_to_mouth )then
          if( ch%nCh_down > 0 )then
            if( any(ch%iCh_down(:ch%nCh_down) == iCh2) ) cycle
          endif
          call add(ch%nCh_down)
          ch%iCh_down(ch%nCh_down) = iCh2
        else
          if( ch%nCh_up > 0 )then
            if( any(ch%iCh_up(:ch%nCh_up) == iCh2) ) cycle
          endif
          call add(ch%nCh_up)
          ch%iCh_up(ch%nCh_up) = iCh2
        endif
      enddo  ! iiCh/
    enddo  ! jNode/

    call realloc(ch%iCh_up, ch%nCh_up, clear=.false.)
    call realloc(ch%iCh_down, ch%nCh_down, clear=.false.)
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  ! check consistency
  !-------------------------------------------------------------
  call logent('checking consistency')

  do iCh = 1, riv_count
    ch => channel(iCh)

    do iiCh = 1, ch%nCh_down
      ch2 => channel(ch%iCh_down(iiCh))

      is_found = .false.
      do iiCh2 = 1, ch2%nCh_up
        if( ch2%iCh_up(iiCh2) == iCh )then
          is_found = .true.
          exit
        endif
      enddo  ! iiCh2/
      if( .not. is_found )then
        call errend('ch#'//str(iCh)//' inconsistency was detected.')
      endif

      is_found = .false.
      do iiCh2 = 1, ch2%nCh_down
        if( ch2%iCh_down(iiCh2) == iCh )then
          is_found = .true.
          exit
        endif
      enddo  ! iiCh2/
      if( is_found )then
        call errend('ch#'//str(iCh)//' inconsistency was detected.')
      endif
    enddo  ! iiCh/
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  ! calc. dist. to downstream channel
  !-------------------------------------------------------------
  call logent('calculating distance to downstream channels')

  do iCh = 1, riv_count
    ch => channel(iCh)

    if( ch%nCh_down == 0 ) cycle

    allocate(ch%dist_down(ch%nCh_down))
    do iiCh_down = 1, ch%nCh_down
      ch2 => channel(ch%iCh_down(iiCh_down))
      ch%dist_down(iiCh_down) = (ch%leng + ch2%leng) * 0.5d0
    enddo  ! iiCh_down/
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_river_connection
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
real(8) function west_of_x(x) result(res)
  implicit none
  integer, intent(in) :: x

  res = west + cellsize*(x-1)
end function west_of_x
!===============================================================
!
!===============================================================
real(8) function east_of_x(x) result(res)
  implicit none
  integer, intent(in) :: x

  res = west + cellsize*x
end function
!===============================================================
!
!===============================================================
real(8) function south_of_y(y) result(res)
  implicit none
  integer, intent(in) :: y

  res = north - cellsize*y
end function south_of_y
!===============================================================
!
!===============================================================
real(8) function north_of_y(y) result(res)
  implicit none
  integer, intent(in) :: y

  res = north - cellsize*(y-1)
end function north_of_y
!===============================================================
!
!===============================================================
integer function xs_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = floor((lon - west) / cellsize) + 1
end function xs_of_lon
!===============================================================
!
!===============================================================
integer function xe_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = ceiling((lon - west) / cellsize)
end function xe_of_lon
!===============================================================
!
!===============================================================
integer function ys_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = floor((north - lat) / cellsize) + 1
end function ys_of_lat
!===============================================================
!
!===============================================================
integer function ye_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = ceiling((north - lat) / cellsize)
end function ye_of_lat
!===============================================================
!
!===============================================================
real(8) function apprx_isct_with_meridian(&
  lon0, lat0, lon1, lat1, lon &
) result(lat)
  implicit none
  character(128), parameter :: PRCNAM = 'apprx_isct_with_meridian'
  real(8), intent(in) :: lon0, lat0, lon1, lat1
  real(8), intent(in) :: lon

  if( lon0 == lon1 )then
    print"(1x,a)", '****** '//trim(PRCNAM)//' ******'
    print"(1x,a)", 'lon0 == lon1'
  elseif( lon < min(lon0,lon1) .or. lon > max(lon0,lon1) )then
    print"(1x,a)", '****** '//trim(PRCNAM)//' ******'
    print"(1x,a)", '`lon` is out of range.'
  endif

  lat = lat0 + (lat1-lat0) * ((lon-lon0) / (lon1-lon0))
end function apprx_isct_with_meridian
!===============================================================
!
!===============================================================
real(8) function apprx_isct_with_parallel(&
    lon0, lat0, lon1, lat1, lat &
) result(lon)
  implicit none
  character(128), parameter :: PRCNAM = 'apprx_isct_with_parallel'
  real(8), intent(in) :: lon0, lat0, lon1, lat1
  real(8), intent(in) :: lat

  if( lat0 == lat1 )then
    print"(1x,a)", '****** '//trim(PRCNAM)//' ******'
    print"(1x,a)", 'lat0 == lat1'
  elseif( lat < min(lat0,lat1) .or. lat > max(lat0,lat1) )then
    print"(1x,a)", '****** '//trim(PRCNAM)//' ******'
    print"(1x,a)", '`lat` is out of range'
  endif

  lon = lon0 + (lon1-lon0) * ((lat-lat0) / (lat1-lat0))
end function apprx_isct_with_parallel
!===============================================================
!
!===============================================================
subroutine hubeny( x1_deg, y1_deg, x2_deg, y2_deg, d )
  implicit none
  real(8), intent(in) :: x1_deg, y1_deg, x2_deg, y2_deg
  real(8), intent(out) :: d

  real(8) :: x1, y1, x2, y2
  real(8) :: dx, dy, mu, a, b, e, W, N, M

  x1 = x1_deg * D2R
  y1 = y1_deg * D2R
  x2 = x2_deg * D2R
  y2 = y2_deg * D2R

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
