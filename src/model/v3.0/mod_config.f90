module mod_config
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
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
  integer :: cl

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('read config.')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_command_argument(1, f_conf)
  if( access(f_conf,' ') /= 0 )then
    call errend('Configuration file not found: '//trim(f_conf))
  endif
  open(newunit=un, file=f_conf, status='old')

  read(un,*)

  read(un,*)
  read(un,*) rainfile
  read(un,*) demfile
  read(un,*) dirfile
  read(un,*) upafile
  call logmsg('rain: '//trim(rainfile))
  call logmsg('dem: '//trim(demfile))
  call logmsg('dir: '//trim(dirfile))
  call logmsg('upa: '//trim(upafile))

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
  call logmsg('')
  call logmsg('dt_model: '//str(dt_model,'f10.3'))
  call logmsg('dt_slope: '//str(dt_slo,'f10.3'))
  call logmsg('dt_river: '//str(dt_riv,'f10.3'))

  ! TMP
  nt_model = time_end / dt_model

  read(un,*)
  read(un,*) dt_out
  read(un,*) dir_out

  if( dir_out(len_trim(dir_out):) == '/' )then
    dir_out = dir_out(:len_trim(dir_out)-1)
  endif
  call logmsg('')
  call logmsg('dt_out: '//str(dt_out,'f10.2'))
  call logmsg('dir_out: '//str(dir_out))

  if( dt_out < dt_model )then
    call errend('`dt_out` must be equal to or larger than `dt_model`.')
  endif

  ! Check output directory
  open(11, file=trim(dir_out)//'/tmp', status='replace', iostat=ios)
  if( ios /= 0 )then
    call errend('Failed to make a new file in the output directory.')
  endif
  close(11, status='delete')

  read(un,*)
  read(un,*) eight_dir

  read(un,*)
  read(un,*) ns_river
  call logmsg('')
  call logmsg('ns_river: '//str(ns_river))

  read(un,*)
  read(un,*) num_of_landuse
  call logmsg('')
  call logmsg('num_of_landuse: '//str(num_of_landuse))

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

  cl = 9
  call logmsg(str('flow',cl)//': '//str(flow,10))
  call logmsg(str('ns_slope',cl)//': '//str(ns_slope,'es10.3'))
  call logmsg(str('soildepth',cl)//': '//str(soildepth,'es10.3'))
  call logmsg(str('gammaa',cl)//': '//str(gammaa,'es10.3'))

  read(un,*) 
  read(un,*) ksv(:)
  read(un,*) faif(:)

  call logmsg('')
  call logmsg(str('ksv',cl)//': '//str(ksv,'es10.3'))
  call logmsg(str('faif',cl)//': '//str(faif,'es10.3'))

  read(un,*) 
  read(un,*) ka(:)
  read(un,*) gammam(:)
  read(un,*) beta(:)
  print*
  call logmsg(str('ka',cl)//': '//str(ka,'es10.3'))
  call logmsg(str('gammam',cl)//': '//str(gammam,'es10.3'))
  call logmsg(str('beta',cl)//': '//str(beta,'es10.3'))

  read(un,*) 
  read(un,*) ksg(:)
  read(un,*) gammag(:)
  read(un,*) kg0(:)
  read(un,*) fpg(:)
  read(un,*) rgl(:)
  call logmsg('')
  call logmsg(str('ksg',cl)//': '//str(ksg,'es10.3'))
  call logmsg(str('gammag',cl)//': '//str(gammag,'es10.3'))
  call logmsg(str('kg0',cl)//': '//str(kg0,'es10.3'))
  call logmsg(str('fpg',cl)//': '//str(fpg,'es10.3'))
  call logmsg(str('rgl',cl)//': '//str(rgl,'es10.3'))

  read(un,*)
  read(un,*) file_riv_network
  call logmsg('file_network: '//trim(file_riv_network))

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
  if( land_switch == 1 ) call logmsg('landfile: '//trim(landfile))

  read(un,*)
  read(un,*) dam_switch
  read(un,"(a)") damfile

  read(un,*)
  read(un,*) evp_switch
  read(un,"(a)") evpfile

  close(un)
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine read_config
!===============================================================
!
!===============================================================
subroutine setup_domain()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_domain'

  real(8) :: d1, d2, d3, d4
  integer :: un
  character :: ctmp

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('setup domain')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=demfile, status='old')
  read(un,*) ctmp, nx
  read(un,*) ctmp, ny
  read(un,*) ctmp, west
  read(un,*) ctmp, south
  read(un,*) ctmp, cellsize
  read(un,*) ! nodata
  close(un)
  call logmsg('nx: '//str(nx)//' ny: '//str(ny))

  east = west + nx * cellsize
  north = south + ny * cellsize

  ! Length of sides
  call hubeny( west, south, east, south, d1 )  ! south
  call hubeny( west, north, east, north, d2 )  ! north
  call hubeny( west, south, west, north, d3 )  ! west
  call hubeny( east, south, east, north, d4 )  ! east
  dx = (d1 + d2) / 2.d0 / real(nx)
  dy = (d3 + d4) / 2.d0 / real(ny)
  call logmsg('dx [m]: '//str(dx)//' dy [m]: '//str(dy))

  length = sqrt(dx * dy)
  area = dx * dy
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine setup_domain
!===============================================================
!
!===============================================================
subroutine prep_landcover()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_landcover'

  integer :: i

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('prep. land cover')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
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
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine prep_landcover
!===============================================================
!
!===============================================================
subroutine prep_topography()
  use mod_section, only: &
    set_section
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_toporaphy'

  integer :: i, j

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('prep. topography data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(zs(nx, ny))
  allocate(zb(nx, ny))
  allocate(domain(nx, ny))

  allocate(dir(nx, ny))
  allocate(upa(nx, ny))

  call read_map(demfile, zs)
  call read_map(dirfile, dir)
  call read_map(upafile, upa)

  ! TMP
  upa = upa * area * 1d-6  ! m2 -> km2

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
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine prep_topography
!===============================================================
!
!===============================================================
subroutine prep_slo_idx()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_slo_idx'

  integer :: i, j, ii, jj, l
  real(8) :: distance, len, l1, l2, l3
  real(8) :: l1_kin, l2_kin, l3_kin

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('prep. 1D data of slope')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
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
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( wbin(slo_idx2i, joined(dir_out,'slo_idx2ij.bin'), rec=1, replace=.true.) )
  call traperr( wbin(slo_idx2j, joined(dir_out,'slo_idx2ij.bin'), rec=2, replace=.false.) )
  call traperr( wbin(slo_ij2idx, joined(dir_out,'slo_ij2idx.bin'), replace=.true.) )
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine prep_slo_idx
!===============================================================
!
!===============================================================
subroutine prep_river()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_river'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('prep. river')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_river_network()

  call prep_river_connection()

  call prep_river_grid_isct()

  call prep_river_topography()
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine prep_river
!===============================================================
!
!===============================================================
subroutine read_river_network()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_river_network'

  type(channel_), pointer :: ch
  type(ch_node_), pointer :: chnode
  integer :: iCh
  integer :: jNode
  integer :: jPoint

  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('read river network')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=file_riv_network, status='old')
  read(un,*) c_, riv_count

  allocate(channel(riv_count))

  do iCh = 1, riv_count
    ch => channel(iCh)
    allocate(ch%node(2))

    read(un,*) ! index
    read(un,*) ! original index
    read(un,*) c_, ch%node(:)%is_outlet
    read(un,*) c_, ch%node(:)%dist_to_mouth
    read(un,*) c_, ch%nPoint

    allocate(ch%point(ch%nPoint))
    read(un,*) c_, ch%point(:)%lon
    read(un,*) c_, ch%point(:)%lat

    ch%dist_to_mouth = sum(ch%node(:)%dist_to_mouth) / 2.d0
    ch%is_outlet = any(ch%node(:)%is_outlet)
    ch%node(1)%lon = ch%point(1)%lon
    ch%node(1)%lat = ch%point(1)%lat
    ch%node(2)%lon = ch%point(ch%nPoint)%lon
    ch%node(2)%lat = ch%point(ch%nPoint)%lat

    do jNode = 1, 2
      chnode => ch%node(jNode)
      if( .not. chnode%is_outlet .and. chnode%dist_to_mouth == 0.d0 )then
        call logmsg('modify outlet node: is_outlet '//str(chnode%is_outlet)//' -> '//str(.true.))
        chnode%is_outlet = .true.
      endif
    enddo

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
      call logmsg('updown unknown: ch#'//str(iCh))
      ch%node(1)%stat_updown = NODE_STAT_UPDOWN__UNKNOWN
      ch%node(2)%stat_updown = NODE_STAT_UPDOWN__UNKNOWN
    endif
  enddo  ! iCh/

  close(un)

  call logmsg('channel length min: '//str(minval(channel(:)%leng))//&
              ' max: '//str(maxval(channel(:)%leng)))
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine read_river_network
!===============================================================
!
!===============================================================
subroutine prep_river_connection()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_river_connection'

  type(channel_), pointer :: ch, ch2
  type(ch_node_), pointer :: chnode, chnode2
  type(nwk_node_), pointer :: node
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_iCh(:), lst_jNode(:)
  integer, allocatable :: arg(:)
  integer :: is, ie, iis, iie, ii
  integer :: iCh, iCh2, iCh_up, iCh_down
  integer :: jCh, jCh2
  integer :: nNode, iNode
  integer :: jNode, jNode2
  integer :: n_outlet

  integer :: iCh_debug = 1675
  logical :: debug_this

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('prep. river connection')
  !-------------------------------------------------------------
  ! make a list of endpoint nodes
  !-------------------------------------------------------------
  call logent('make a list of enpoint nodes')

  nNode = riv_count * 2
  allocate(lst_lon(nNode))
  allocate(lst_lat(nNode))
  allocate(lst_iCh(nNode))
  allocate(lst_jNode(nNode))

  iNode = 0
  do iCh = 1, riv_count
    ch => channel(iCh)

    do jNode = 1, 2
      call add(iNode)
      lst_lon(iNode) = ch%node(jNode)%lon
      lst_lat(iNode) = ch%node(jNode)%lat
      lst_iCh(iNode) = iCh
      lst_jNode(iNode) = jNode
    enddo
  enddo

  do iCh = 1, riv_count
    ch => channel(iCh)
    ch%node(:)%nCh_adj = 0
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

      do ii = iis, iie
        chnode => channel(lst_iCh(ii))%node(lst_jNode(ii))
        call add(chnode%nCh_adj, iie-iis)
      enddo
    enddo  ! iie/
  enddo  ! ie/

  call logmsg('nodes: '//str(nwk%nNode))

  allocate(nwk%node(nwk%nNode))

  do iCh = 1, riv_count
    ch => channel(iCh)
    ch%nCh_adj = sum(ch%node(:)%nCh_adj)
    allocate(ch%iCh_up(ch%nCh_adj))
    allocate(ch%jNode_up(ch%nCh_adj))
    allocate(ch%iCh_down(ch%nCh_adj))
    allocate(ch%jNode_down(ch%nCh_adj))
    ch%nCh_up = 0
    ch%nCh_down = 0

    do jNode = 1, 2
      chnode => ch%node(jNode)
      allocate(chnode%iCh_up(chnode%nCh_adj))
      allocate(chnode%jNode_up(chnode%nCh_adj))
      allocate(chnode%iCh_down(chnode%nCh_adj))
      allocate(chnode%jNode_down(chnode%nCh_adj))
      chnode%nCh_up = 0
      chnode%nCh_down = 0
    enddo  ! jNode/
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  ! determine flow direction at each node
  !-------------------------------------------------------------
  call logent('determine flow direction at each node')

  ie = 0
  iNode = 0
  do while( ie < nNode )
    is = ie + 1
    ie = is
    do while( ie < nNode )
      if( lst_lon(ie+1) /= lst_lon(is) .or. lst_lat(ie+1) /= lst_lat(is) ) exit
      ie = ie + 1
    enddo
    !-----------------------------------------------------------
    ! make lists of channels connected to the nwknode
    !-----------------------------------------------------------
    call add(iNode)
    node => nwk%node(iNode)
    node%nCh = ie - is + 1
    allocate(node%iCh(node%nCh))
    allocate(node%jNode(node%nCh))
    node%iCh(:) = lst_iCh(is:ie)
    node%jNode(:) = lst_jNode(is:ie)

    allocate(node%iCh_up(node%nCh))
    allocate(node%jNode_up(node%nCh))

    allocate(node%iCh_down(node%nCh))
    allocate(node%jNode_down(node%nCh))

    allocate(node%iCh_unknown(node%nCh))
    allocate(node%jNode_unknown(node%nCh))

    node%nCh_up = 0
    node%nCh_down = 0
    node%nCh_unknown = 0
    do jCh = 1, node%nCh
      iCh = node%iCh(jCh)
      jNode = node%jNode(jCh)
      ch => channel(iCh)
      chnode => ch%node(jNode)

      chnode%iNode = iNode

      selectcase( chnode%stat_updown )
      case( NODE_STAT_UPDOWN__UP )
        call add(node%nCh_up)
        node%iCh_up(node%nCh_up) = iCh
        node%jNode_up(node%nCh_up) = jNode
      case( NODE_STAT_UPDOWN__DOWN )
        call add(node%nCh_down)
        node%iCh_down(node%nCh_down) = iCh
        node%jNode_down(node%nCh_down) = jNode
      case( NODE_STAT_UPDOWN__UNKNOWN )
        call add(node%nCh_unknown)
        node%iCh_unknown(node%nCh_unknown) = iCh
        node%jNode_unknown(node%nCh_unknown) = jNode
      case default
        call errend(msg_invalid_value('chnode%stat_updown', chnode%stat_updown))
      endselect
    enddo  ! jCh/

    call realloc(node%iCh_up, node%nCh_up, clear=.false.)
    call realloc(node%jNode_up, node%nCh_up, clear=.false.)

    call realloc(node%iCh_down, node%nCh_down, clear=.false.)
    call realloc(node%jNode_down, node%nCh_down, clear=.false.)

    call realloc(node%iCh_unknown, node%nCh_unknown, clear=.false.)
    call realloc(node%jNode_unknown, node%nCh_unknown, clear=.false.)
    !-----------------------------------------------------------
    ! make lists of channels connected to chnode
    !-----------------------------------------------------------
    ! CASE: up > 0 and down > 0
    ! water flows up to down
    if( node%nCh_up > 0 .and. node%nCh_down > 0 )then

      ! upstream node of ch == downstream node of ch2
      ! -> ch2 is upstream of ch
      do jCh = 1, node%nCh_up
        iCh = node%iCh_up(jCh)
        jNode = node%jNode_up(jCh)
        ch => channel(iCh)
        chnode => ch%node(jNode)

        do jCh2 = 1, node%nCh_down
          iCh2 = node%iCh_down(jCh2)
          jNode2 = node%jNode_down(jCh2)
          ch2 => channel(iCh2)
          chnode2 => ch2%node(jNode2)

          if( ch%nCh_up > 0 )then
            if( any(iCh2 == ch%iCh_up(:ch%nCh_up)) )then
              call errend('iCh2 already exists in ch%iCh_up(:)'//&
                '\nch#'//str(iCh)//' iCh_up: '//str(ch%iCh_up(:ch%nCh_up))//&
                '\nch2#'//str(iCh2))
            endif
          endif

          call add(ch%nCh_up)
          ch%iCh_up(ch%nCh_up) = iCh2
          ch%jNode_up(ch%nCh_up) = jNode2

          call add(chnode%nCh_up)
          chnode%iCh_up(chnode%nCh_up) = iCh2
          chnode%jNode_up(chnode%nCh_up) = jNode2

          call add(ch2%nCh_down)
          ch2%iCh_down(ch2%nCh_down) = iCh
          ch2%jNode_down(ch2%nCh_down) = jNode

          call add(chnode2%nCh_down)
          chnode2%iCh_down(chnode2%nCh_down) = iCh
          chnode2%jNode_down(chnode2%nCh_down) = jNode
        enddo  ! jCh2/
      enddo  ! jCh/
    !-----------------------------------------------------------
    ! CASE: up > 0 and down == 0
    ! node is source
    elseif( node%nCh_up > 0 .and. node%nCh_down == 0 )then
      continue
    !-----------------------------------------------------------
    ! CASE: up == 0 and down > 0
    ! IF unknown == 0: node is mouth (can be inland)
    ! IF unknown > 0: water flows to unknown
    elseif( node%nCh_up == 0 .and. node%nCh_down > 0 )then
      n_outlet = 0
      do jCh = 1, node%nCh_down
        iCh = node%iCh_down(jCh)
        jNode = node%jNode_down(jCh)
        chnode => channel(iCh)%node(jNode)
        if( chnode%is_outlet ) call add(n_outlet)
      enddo
      !---------------------------------------------------------
      ! CASE: outlet
      if( n_outlet == node%nCh_down )then
        continue
      !---------------------------------------------------------
      ! CASE: ERROR: inconsistency
      elseif( n_outlet > 0 )then
        do jCh = 1, node%nCh_down
          iCh = node%iCh_down(jCh)
          jNode = node%jNode_down(jCh)
          chnode => channel(iCh)%node(jNode)
          if( chnode%is_outlet ) cycle
          call errend('node is outlet but status `is_outlet` is False.'//&
            '\nch#'//str(iCh)//' node#'//str(jNode)//' dist_to_mouth: '//str(chnode%dist_to_mouth))
        enddo  ! jCh/
      !---------------------------------------------------------
      ! CASE: not outlet
      else
        call logerr(msg_not_implemented())
        if( node%nCh_unknown == 0 )then
          !call logwrn('inland outlet')
        else
          continue
        endif
      endif
    !-----------------------------------------------------------
    ! CASE: up == 0 and down == 0
    elseif( node%nCh_up == 0 .and. node%nCh_down == 0 )then
      call logerr(msg_not_implemented()//'\nup == 0 and down == 0')
      continue
    endif
  enddo  ! ie/

  if( iNode /= nwk%nNode )then
    call errend('iNode: '//str(iNode))
  endif

  nCh_down_max = maxval(channel(:)%nCh_down)
  call logmsg('nCh_down max: '//str(nCh_down_max))

  ! realloc.
  do iCh = 1, riv_count
    ch => channel(iCh)

    ch%nCh_adj = ch%nCh_up + ch%nCh_down
    call realloc(ch%iCh_up, ch%nCh_up, clear=.false.)
    call realloc(ch%iCh_down, ch%nCh_down, clear=.false.)

    do jNode = 1, 2
      chnode => ch%node(jNode)
      chnode%nCh_adj = chnode%nCh_up + chnode%nCh_down
      call realloc(chnode%iCh_up, chnode%nCh_up, clear=.false.)
      call realloc(chnode%iCh_down, chnode%nCh_down, clear=.false.)
    enddo
  enddo

  ! check consistency
  do iCh = 1, riv_count
    ch => channel(iCh)

    ! consistency of %iCh_up and %iCh_down
    do jCh = 1, ch%nCh_up
      iCh_up = ch%iCh_up(jCh)
      do jCh2 = 1, ch%nCh_down
        iCh_down = ch%iCh_down(jCh2)
        if( iCh_up == iCh_down )then
          call errend('iCh_up == iCh_down')
        endif
      enddo  ! jCh2/
    enddo  ! jCh/

    ! consistency with connected channels
    do jCh = 1, ch%nCh_up
      iCh2 = ch%iCh_up(jCh)
      ch2 => channel(iCh2)
      if( ch2%nCh_down == 0 )then
        call errend('ch2%nCh_down == 0')
      else
        if( all(ch2%iCh_down /= iCh) )then
          call errend('all(ch2%iCh_down /= iCh)')
        endif
      endif
    enddo  ! jCh/

    do jCh = 1, ch%nCh_down
      iCh2 = ch%iCh_down(jCh)
      ch2 => channel(iCh2)
      if( ch2%nCh_up == 0 )then
        call errend('ch2%nCh_up == 0')
      else
        if( all(ch2%iCh_up /= iCh) )then
          call errend('all(ch2%iCh_up /= iCh)')
        endif
      endif
    enddo  ! jCh/

    if( iCh == iCh_debug )then
      call logmsg('ch#'//str(iCh))
      if( ch%nCh_up == 0 )then
        call logmsg('  ch_up  : (none)')
      else
        call logmsg('  ch_up  : '//str(ch%iCh_up))
      endif
      if( ch%nCh_down == 0 )then
        call logmsg('  ch_down: (none)')
      else
        call logmsg('  ch_down: '//str(ch%iCh_down))
      endif
    endif
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  ! calc. dist. to downstream channel
  !-------------------------------------------------------------
  call logent('calc. distance to downstream channels')

  do iCh = 1, riv_count
    ch => channel(iCh)

    if( ch%nCh_down == 0 ) cycle

    allocate(ch%dist_down(ch%nCh_down))
    do jCh = 1, ch%nCh_down
      ch2 => channel(ch%iCh_down(jCh))
      ch%dist_down(jCh) = (ch%leng + ch2%leng) * 0.5d0
    enddo  ! jCh/
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine prep_river_connection
!===============================================================
!
!===============================================================
subroutine prep_river_grid_isct()
  use lib_array
  use lib_math
  use mod_base, only: &
    slonlat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_river_grid_isct'

  type(channel_), pointer :: ch
  type(slo_riv_isct_), pointer :: sloriv
  integer :: xs, xe, ys, ye
  real(8) :: wlon, wlat, elon, elat
  real(8) :: clon_west, clat_west, clon_east, clat_east
  real(8) :: dlon_west, dlat_west, dlon_east, dlat_east
  real(8) :: leng
  integer :: nSlo_tmp

  integer :: k
  integer :: ix, iy
  integer :: jPoint
  integer :: iSlo
  integer :: jSlo
  integer :: nSlo_isct_max
  integer, allocatable :: ch_isct_x(:,:), ch_isct_y(:,:)
  real(8), allocatable :: ch_isct_leng(:,:)

  integer :: k_debug = 1675
  logical :: debug_this

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('calc. intersection of river and grid')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do k = 1, riv_count
    ch => channel(k)

    debug_this = k == k_debug

    ch%isct%nSlo = 0

    xs = xs_of_lon(minval(ch%node(:)%lon))
    xe = xe_of_lon(maxval(ch%node(:)%lon))
    ys = ys_of_lat(maxval(ch%node(:)%lat))
    ye = ye_of_lat(minval(ch%node(:)%lat))
    nSlo_tmp = (xe - xs + 1) * (ye - ys + 1) * 4
    allocate(ch%isct%x(nSlo_tmp))
    allocate(ch%isct%y(nSlo_tmp))
    allocate(ch%isct%iSlo(nSlo_tmp))
    allocate(ch%isct%leng(nSlo_tmp))
    allocate(ch%isct%domain(nSlo_tmp))

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
      if( debug_this )then
        call logmsg('y: '//str((/ys,ye/),' - '))
      endif
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

    call realloc(ch%isct%x, ch%isct%nSlo, clear=.false.)
    call realloc(ch%isct%y, ch%isct%nSlo, clear=.false.)
    call realloc(ch%isct%iSlo, ch%isct%nSlo, clear=.false.)
    call realloc(ch%isct%leng, ch%isct%nSlo, clear=.false.)
    call realloc(ch%isct%domain, ch%isct%nSlo, clear=.false.)

    ! calculated later
    allocate(ch%isct%jCh(ch%isct%nSlo))

!    if( debug_this )then
!      call logmsg('nSlo: '//str(ch%isct%nSlo)//&
!        ' x: '//str((/minval(ch%isct%x),maxval(ch%isct%x)/),' - ')//&
!        ' y: '//str((/minval(ch%isct%y),maxval(ch%isct%y)/),' - '))
!    endif

    ch%isct%leng_domain = 0.d0
    do iSlo = 1, ch%isct%nSlo
      if( ch%isct%domain(iSlo) == DOMAIN__OUTSIDE ) cycle
      call add(ch%isct%leng_domain, ch%isct%leng(iSlo))
!      if( debug_this )then
!        call logmsg('grid#'//str(iSlo)//' leng '//str(ch%isct%leng(iSlo)))
!      endif
    enddo

    if( any(ch%isct%domain /= DOMAIN__OUTSIDE) .and. &
        ch%isct%leng_domain < LENG_DOMAIN_THRESH )then
      call errend('ch#'//str(k)//' leng_domain < '//str(LENG_DOMAIN_THRESH)//&
        '\nleng_domain: '//str(ch%isct%leng_domain)//&
        '\nleng: '//str(ch%leng))
    endif
  enddo  ! k/
  !-------------------------------------------------------------
  ! prep. $slo_riv_isct
  !-------------------------------------------------------------
  allocate(slo_riv_isct(slo_count))
  slo_riv_isct(:)%nCh = 0

  do k = 1, riv_count
    ch => channel(k)
    do jSlo = 1, ch%isct%nSlo
      if( ch%isct%domain(jSlo) == DOMAIN__OUTSIDE ) cycle
      iSlo = ch%isct%iSlo(jSlo)
      sloriv => slo_riv_isct(iSlo)
      sloriv%nCh = sloriv%nCh + 1
    enddo
  enddo  ! k/

  do iSlo = 1, slo_count
    sloriv => slo_riv_isct(iSlo)
    if( sloriv%nCh == 0 ) cycle
    allocate(sloriv%iCh(sloriv%nCh))
    allocate(sloriv%jSlo(sloriv%nCh))
  enddo

  slo_riv_isct(:)%nCh = 0

  do k = 1, riv_count
    ch => channel(k)
    do jSlo = 1, ch%isct%nSlo
      if( ch%isct%domain(jSlo) == DOMAIN__OUTSIDE ) cycle
      iSlo = ch%isct%iSlo(jSlo)
      sloriv => slo_riv_isct(iSlo)

      sloriv%nCh = sloriv%nCh + 1
      sloriv%iCh(sloriv%nCh) = k
      sloriv%jSlo(sloriv%nCh) = jSlo

      ch%isct%jCh(jSlo) = sloriv%nCh
    enddo
  enddo  ! k/
  !-------------------------------------------------------------
  ! Save data
  !-------------------------------------------------------------
  call logent('save')

  nSlo_isct_max = maxval(channel(:)%isct%nSlo)
  allocate(ch_isct_x(nSlo_isct_max,riv_count))
  allocate(ch_isct_y(nSlo_isct_max,riv_count))
  allocate(ch_isct_leng(nSlo_isct_max,riv_count))

  ch_isct_x(:,:) = 0
  ch_isct_y(:,:) = 0
  ch_isct_leng(:,:) = 0.d0
  do k = 1, riv_count
    ch => channel(k)
    ch_isct_x(:ch%isct%nSlo,k) = ch%isct%x(:)
    ch_isct_y(:ch%isct%nSlo,k) = ch%isct%y(:)
    ch_isct_leng(:ch%isct%nSlo,k) = ch%isct%leng(:)
  enddo  ! k/

  call traperr( wbin(ch_isct_x, joined(dir_out,'ch_isct_xy.bin'), rec=1, replace=.true.) )
  call traperr( wbin(ch_isct_y, joined(dir_out,'ch_isct_xy.bin'), rec=2, replace=.false.) )
  call traperr( wbin(ch_isct_leng, joined(dir_out,'ch_isct_leng.bin'), replace=.true.) )

  deallocate(ch_isct_x)
  deallocate(ch_isct_y)
  deallocate(ch_isct_leng)

  call logext()
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine loop_for_x()
  implicit none

  xs = xs_of_lon(clon_west)
  xe = xe_of_lon(clon_east)

  if( debug_this )then
    call logmsg('x: '//str((/xs,xe/),' - '))
  endif

  nSlo_tmp = ch%isct%nSlo + (xe - xs + 1)
  if( size(ch%isct%x) < nSlo_tmp )then
    call realloc(ch%isct%x, nSlo_tmp*2, clear=.false.)
    call realloc(ch%isct%y, nSlo_tmp*2, clear=.false.)
    call realloc(ch%isct%iSlo, nSlo_tmp*2, clear=.false.)
    call realloc(ch%isct%leng, nSlo_tmp*2, clear=.false.)
    call realloc(ch%isct%domain, nSlo_tmp*2, clear=.false.)
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

    do jSlo = 1, ch%isct%nSlo
      if( ch%isct%x(jSlo) == ix .and. ch%isct%y(jSlo) == iy )then
        call add(ch%isct%leng(jSlo), leng)
        exit
      endif
    enddo
    if( jSlo == ch%isct%nSlo+1 )then
      call add(ch%isct%nSlo)
      ch%isct%x(jSlo) = ix
      ch%isct%y(jSlo) = iy
      ch%isct%iSlo(jSlo) = slo_ij2idx(ix,iy)
      ch%isct%leng(jSlo) = leng
      ch%isct%domain(jSlo) = domain(ix,iy)
    endif

    if( debug_this )then
      call logmsg('jSlo '//str(ch%isct%nSlo)//' x '//str(ix)//' y '//str(iy)//&
          ' iSlo '//str(slo_ij2idx(ix,iy)))
    endif
!    if( debug_this )then
!      call logmsg('ch#'//str(k)//' p#'//str(jPoint)//' ('//&
!        slonlat(wlon,wlat)//')'//' - p#'//str(jPoint+1)//' ('//&
!        slonlat(elon,elat)//')'//&
!        '\n  isct#'//str(ch%isct%nSlo)//&
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

  type(channel_), pointer :: ch, ch2
  real(8), allocatable :: lst_leng(:)
  real(8), allocatable :: lst_upa(:)
  integer, allocatable :: arg(:)
  real(8) :: leng_tmp
  integer :: k
  integer :: iCh, jCh
  integer :: jSlo
  logical :: is_ok

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! get median of upper area
  !-------------------------------------------------------------
  call logent('get upper area')

  do k = 1, riv_count
    ch => channel(k)

    allocate(lst_leng(ch%isct%nSlo))
    allocate(lst_upa(ch%isct%nSlo))
    lst_leng(:) = 0.d0
    lst_upa(:) = 0.d0
    do jSlo = 1, ch%isct%nSlo
      if( ch%isct%domain(jSlo) == DOMAIN__OUTSIDE ) cycle
      lst_leng(jSlo) = ch%isct%leng(jSlo)
      lst_upa(jSlo) = upa(ch%isct%x(jSlo), ch%isct%y(jSlo))
    enddo

    allocate(arg(ch%isct%nSlo))
    call argsort(lst_upa, arg)
    call sort(lst_upa, arg)
    call sort(lst_leng, arg)
    deallocate(arg)

    leng_tmp = 0.d0
    ch%upa = -1d20
    do jSlo = 1, ch%isct%nSlo
      leng_tmp = leng_tmp + ch%isct%leng(jSlo)
      if( leng_tmp >= ch%isct%leng_domain*0.5d0 )then
        ch%upa = lst_upa(jSlo)
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

  call logext()
  !-------------------------------------------------------------
  ! TMP
  ! modify upa
  !-------------------------------------------------------------
  call logent('modify upa')

  is_ok = .false.
  do while( .not. is_ok )
    is_ok = .true.

    do iCh = 1, riv_count
      ch => channel(iCh)
      do jCh = 1, ch%nCh_down
        ch2 => channel(ch%iCh_down(jCh))
        if( ch2%upa >= ch%upa ) cycle
!        call logmsg('ch#'//str(ch%iCh_down(jCh))//' '//str(ch2%upa)//' -> '//str(ch%upa))
        ch2%upa = ch%upa
        is_ok = .false.
      enddo
    enddo  ! iCh/
  enddo  ! is_ok/

  call traperr( wbin(channel(:)%upa, joined(dir_out, 'riv_upa.bin'), replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  ! get width and depth
  !-------------------------------------------------------------
  call logent('get width and depth')

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

  call logmsg('channel width min: '//str(minval(channel(:)%width))//&
                           ' max: '//str(maxval(channel(:)%width)))
  call logmsg('channel depth min: '//str(minval(channel(:)%depth))//&
                           ' max: '//str(maxval(channel(:)%depth)))
  call logmsg('channel levee min: '//str(minval(channel(:)%height))//&
                           ' max: '//str(maxval(channel(:)%height)))

  call traperr( wbin(channel(:)%width, joined(dir_out, 'riv_width.bin'), replace=.true.) )
  call traperr( wbin(channel(:)%depth, joined(dir_out, 'riv_depth.bin'), replace=.true.) )
  call traperr( wbin(channel(:)%height, joined(dir_out, 'riv_levee.bin'), replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  ! calc. area
  !-------------------------------------------------------------
  call logent('calc area')

  do k = 1, riv_count
    ch => channel(k)

    ch%area = ch%leng * ch%width

    allocate(ch%isct%area(ch%isct%nSlo))
    ch%isct%area(:) = ch%isct%leng(:) * ch%width
  enddo

  call logmsg('channel area  min: '//str(minval(channel(:)%area))//&
                           ' max: '//str(maxval(channel(:)%area)))

  call traperr( wbin(channel(:)%area, joined(dir_out, 'riv_area.bin'), replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  ! calc. elevation
  !-------------------------------------------------------------
  call logent('calc elevation')

  do k = 1, riv_count
    ch => channel(k)

    ! mean surface elevation
    ch%zs = 0.d0
    do jSlo = 1, ch%isct%nSlo
      if( ch%isct%domain(jSlo) == DOMAIN__OUTSIDE ) cycle
      ch%zs = ch%zs + zs(ch%isct%x(jSlo),ch%isct%y(jSlo)) &
                * ch%isct%leng(jSlo) / ch%isct%leng_domain
    enddo

    ! mean bed rock elevation
    ch%zb = ch%zs - ch%depth
  enddo  ! k/

  call logmsg('channel elevation min: '//str(minval(channel(:)%zs))//&
                               ' max: '//str(maxval(channel(:)%zs)))
  call logmsg('channel bed elvtn min: '//str(minval(channel(:)%zb))//&
                               ' max: '//str(maxval(channel(:)%zb)))

  call traperr( wbin(channel(:)%zs, joined(dir_out, 'riv_zs.bin'), replace=.true.) )
  call traperr( wbin(channel(:)%zb, joined(dir_out, 'riv_zb.bin'), replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_river_topography
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
