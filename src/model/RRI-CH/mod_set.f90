module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use def_const
  use def_type
  use mod_param
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_set'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_conf()

  call set_mesh()

  call build_river_network()

  call make_river_shape()

  call get_topo_map()

  call calc_river_cell_relations()

  call make_slope_1d()

  call make_river_topo()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine init
!===============================================================
!
!===============================================================
subroutine read_conf()
  use mod_util, only: &
        stime2time
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_conf'

  !integer :: hours
  character(32) :: stime_bgn, stime_end

  character(CLEN_PATH) :: f_conf = 'conf'
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_conf = argument(1)
  open(newunit=un, file=f_conf, status='old')

  read(un,*) c_, dir_out
  call logmsg('dir_out: '//str(dir_out))
  call traperr( mkdir(dir_out) )

  read(un,*)
  read(un,*) c_, stime_bgn
  read(un,*) c_, stime_end
  read(un,*) c_, dt_out
  read(un,*) c_, dt
  read(un,*) c_, dt_riv
  read(un,*) c_, dt_slo
  read(un,*) c_, eps
  read(un,*) c_, ddt_min_riv
  read(un,*) c_, ddt_min_slo

  allocate(time_bgn(6))
  allocate(time_end(6))
  call stime2time(stime_bgn, time_bgn)
  call stime2time(stime_end, time_end)

  call logmsg('')
  call logmsg('dt_out: '//str(dt_out))
  call logmsg('dt    : '//str(dt))
  call logmsg('dt_riv: '//str(dt_riv))
  call logmsg('dt_slo: '//str(dt_slo))
  call logmsg('nt    : '//str(nt))
  call logmsg('eps: '//str(eps))
  call logmsg('ddt_min_riv: '//str(ddt_min_riv))
  call logmsg('ddt_min_slo: '//str(ddt_min_slo))

  read(un,*)
  read(un,*) c_, west
  read(un,*) c_, east
  read(un,*) c_, south
  read(un,*) c_, north
  read(un,*) c_, nx
  read(un,*) c_, ny
  call logmsg('')
  call logmsg('west : '//str(west,'f12.7'))
  call logmsg('east : '//str(east,'f12.7'))
  call logmsg('south: '//str(south,'f11.7'))
  call logmsg('north: '//str(north,'f11.7'))
  call logmsg('nx: '//str(nx))
  call logmsg('ny: '//str(ny))

  read(un,*)
  read(un,*) c_, file_elvtn
  read(un,*) c_, file_flwdir
  read(un,*) c_, file_landuse
  read(un,*) c_, file_rivshp
  read(un,*) c_, allow_channel_outside_domain
  call logmsg('')
  call logmsg('file_elvtn  : '//str(file_elvtn))
  call logmsg('file_flwdir : '//str(file_flwdir))
  call logmsg('file_landuse: '//str(file_landuse))
  call logmsg('file_rivshp : '//str(file_rivshp))
  call logmsg('allow_channel_outside_domain: '//str(allow_channel_outside_domain))

  read(un,*)
  read(un,*) c_, ns_river
  call logmsg('')
  call logmsg('ns_river: '//str(ns_river))

  read(un,*)
  read(un,*) c_, nLu
  call logmsg('')
  call logmsg('num_landuse: '//str(nLu))

  lmax = 4

  allocate(dif(nLu))
  allocate(ns_slope(nLu))
  allocate(soildepth(nLu))
  allocate(gammaa(nLu))
  allocate(ksv(nLu))
  allocate(faif(nLu))
  allocate(infilt_limit(nLu))
  allocate(ka(nLu))
  allocate(gammam(nLu))
  allocate(beta(nLu))
  allocate(da(nLu))
  allocate(dm(nLu))
  allocate(km(nLu))
  allocate(ksg(nLu))
  allocate(gammag(nLu))
  allocate(kg0(nLu))
  allocate(fpg(nLu))
  allocate(rgl(nLu))

  read(un,*) c_, dif(:)
  read(un,*) c_, ns_slope(:)
  read(un,*) c_, soildepth(:)
  read(un,*) c_, gammaa(:)

  call logmsg('dif      : '//str(dif,12))
  call logmsg('ns_slope : '//str(ns_slope,'f12.3'))
  call logmsg('soildepth: '//str(soildepth,'f12.3'))
  call logmsg('gammaa   : '//str(gammaa,'f12.3'))

  read(un,*)
  read(un,*) c_, ksv(:)
  read(un,*) c_, faif(:)
  call logmsg('')
  call logmsg('ksv      : '//str(ksv,'f12.3'))
  call logmsg('faif     : '//str(faif,'f12.3'))

  read(un,*)
  read(un,*) c_, ka(:)
  read(un,*) c_, gammam(:)
  read(un,*) c_, beta(:)

  call logmsg('')
  call logmsg('ka       : '//str(ka,'f12.3'))
  call logmsg('gammam   : '//str(gammam,'f12.3'))
  call logmsg('beta     : '//str(beta,'f12.3'))

  read(un,*)
  read(un,*) c_, ksg(:)
  read(un,*) c_, gammag(:)
  read(un,*) c_, kg0(:)
  read(un,*) c_, fpg(:)
  read(un,*) c_, rgl(:)
  call logmsg('')
  call logmsg('ksg      : '//str(ksg,'f12.3'))
  call logmsg('gammag   : '//str(gammag,'f12.3'))
  call logmsg('kg0      : '//str(kg0,'f12.3'))
  call logmsg('fpg      : '//str(fpg,'f12.3'))
  call logmsg('rgl      : '//str(rgl,'f12.3'))

  read(un,*)
  call logmsg('')

  da(:) = soildepth(:) * gammaa(:)

  dm(:) = soildepth(:) * gammam(:)

  where( beta == 0.d0 )
    km = 0.d0
  elsewhere
    km = ka / beta
  endwhere

  ! River
  read(un,*) c_, width_mode, depth_mode, levee_mode
  read(un,*) c_, width_param_c
  read(un,*) c_, width_param_s
  read(un,*) c_, depth_param_c
  read(un,*) c_, depth_param_s
  read(un,*) c_, levee_param
  read(un,*) c_, levee_upa_thresh

  call logmsg('width_mode: '//str(width_mode))
  selectcase( width_mode )
  case( WIDTH_MODE__UPA )
    call logmsg('width_param c: '//str(width_param_c)//', s: '//str(width_param_s))
  case( WIDTH_MODE__FILE )
    continue
  case default
    call errend(msg_invalid_value('width_mode', width_mode))
  endselect

  call logmsg('depth_mode: '//str(depth_mode))
  selectcase( depth_mode )
  case( DEPTH_MODE__UPA )
    call logmsg('depth_param c: '//str(depth_param_c)//', s: '//str(depth_param_s))
  case( DEPTH_MODE__FILE )
    continue
  case default
    call errend(msg_invalid_value('depth_mode', depth_mode))
  endselect

  call logmsg('levee_mode: '//str(levee_mode))
  selectcase( levee_mode )
  case( LEVEE_MODE__UPA )
    call logmsg('levee_param: '//str(levee_param)//' upa_thresh: '//str(levee_upa_thresh))
  case( LEVEE_MODE__FILE )
    continue
  case default
    call errend(msg_invalid_value('levee_mode', levee_mode))
  endselect

  read(un,*)
  read(un,*) c_, file_prcp
  call logmsg('')
  call logmsg('file_prcp: '//str(file_prcp))

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_conf
!===============================================================
!
!===============================================================
subroutine set_mesh()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_mesh'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  cellsize_lon = (east - west) / nx
  cellsize_lat = (north - south) / ny

  call logmsg('cellsize lon: '//str(cellsize_lon)//&
                      ' lat: '//str(cellsize_lat))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_mesh
!===============================================================
!
!===============================================================
subroutine build_river_network()
  use mod_mesh, only: &
       xs_of_lon, &
       ys_of_lat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_network'

  type(ch_)     , pointer :: ch
  type(pt_)     , pointer :: pt
  type(nd_)     , pointer :: nd, nd1, nd2
  type(ch_conn_), pointer :: ch_conn
  type(nd_conn_), pointer :: nd_conn
  type(outlet_) , pointer :: outlet
  real(8), allocatable :: nd_lon(:), nd_lat(:)
  integer, allocatable :: nd_iCh(:), nd_jNode(:)
  integer :: k
  integer :: iPt
  integer :: jNode
  integer :: nnNode
  integer :: kNode, ksNode, keNode, k0Node, kksNode, kkeNode
  integer :: iNode_conn
  integer, allocatable :: arg(:)

  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Read data
  !-------------------------------------------------------------
  call logent('Reading network data')

  call logmsg('Reading '//str(file_rivshp))
  open(newunit=un, file=file_rivshp, status='old')

  read(un,*) c_, nCh

  allocate(lst_ch(nCh))

  do k = 1, nCh
    ch => lst_ch(k)

    read(un,*)

    allocate(ch%node(2))
    nd1 => ch%node(1)
    nd2 => ch%node(2)
    read(un,*) c_, nd1%is_outlet, nd2%is_outlet
    read(un,*)  ! distance to mouth

    read(un,*) c_, ch%nPt
    allocate(ch%pt(ch%nPt))

    read(un,*) c_, ch%pt(:)%lon
    read(un,*) c_, ch%pt(:)%lat

    do iPt = 1, ch%nPt
      pt => ch%pt(iPt)
      pt%x = xs_of_lon(pt%lon)
      pt%y = ys_of_lat(pt%lat)
      if( pt%lon == east ) pt%x = nx
      if( pt%lat == south ) pt%y = ny
      if( pt%x < 1 .or. pt%x > nx .or. pt%y < 1 .or. pt%y > ny )then
        if( allow_channel_outside_domain )then
          call logmsg('Point is outside the domain.'//&
                     '\n  channel '//str(k)//' point '//str(iPt)//&
                     ' (x,y): ('//str((/pt%x,pt%y/),',')//')')
        else
          call errend('Point is outside the domain.'//&
                     '\n  channel '//str(k)//' point '//str(iPt)//&
                     ' (x,y): ('//str((/pt%x,pt%y/),',')//')')
        endif
      endif
    enddo  ! iPt/

    nd1%lon = ch%pt(1)%lon
    nd1%lat = ch%pt(1)%lat
    nd2%lon = ch%pt(ch%nPt)%lon
    nd2%lat = ch%pt(ch%nPt)%lat
  enddo  ! k/
  close(un)

  call logext()
  !-------------------------------------------------------------
  ! Count nodes and store index in $lst_ch%node
  !-------------------------------------------------------------
  call logent('Counting nodes and storing index')

  nnNode = nCh * 2
  allocate(nd_lon(nnNode))
  allocate(nd_lat(nnNode))
  allocate(nd_iCh(nnNode))
  allocate(nd_jNode(nnNode))

  kNode = 0
  do k = 1, nCh
    ch => lst_ch(k)

    call add(kNode)
    nd_lon(kNode) = ch%node(1)%lon
    nd_lat(kNode) = ch%node(1)%lat
    nd_iCh(kNode) = k
    nd_jNode(kNode) = 1

    call add(kNode)
    nd_lon(kNode) = ch%node(2)%lon
    nd_lat(kNode) = ch%node(2)%lat
    nd_iCh(kNode) = k
    nd_jNode(kNode) = 2
  enddo  ! k/

  allocate(arg(nnNode))
  call argsort(nd_lon, arg)
  call sort(nd_lon, arg)
  call sort(nd_lat, arg)
  call sort(nd_iCh, arg)
  call sort(nd_jNode, arg)

  nNode = 0
  keNode = 0
  do while( keNode < nnNode )
    ksNode = keNode + 1
    keNode = ksNode
    do while( keNode < nnNode )
      if( nd_lon(keNode+1) /= nd_lon(ksNode) ) exit
      keNode = keNode + 1
    enddo

    if( ksNode == keNode )then
      call add(nNode)
      cycle
    endif

    call argsort(nd_lat(ksNode:keNode), arg(ksNode:keNode))
    call sort(nd_lat(ksNode:keNode), arg(ksNode:keNode))
    call sort(nd_iCh(ksNode:keNode), arg(ksNode:keNode))
    call sort(nd_jNode(ksNode:keNode), arg(ksNode:keNode))

    kkeNode = ksNode - 1
    do while( kkeNode < keNode )
      kksNode = kkeNode + 1
      kkeNode = kksNode
      do while( kkeNode < keNode )
        if( nd_lat(kkeNode+1) /= nd_lat(kksNode) ) exit
        kkeNode = kkeNode + 1
      enddo
      call add(nNode)

      do kNode = kksNode, kkeNode
        lst_ch(nd_iCh(kNode))%node(nd_jNode(kNode))%iNode = nNode
      enddo

    enddo  ! kkeNode/
  enddo  ! keNode/

  call logmsg('Nodes: '//str(nNode))

  call logext()
  !-------------------------------------------------------------
  ! Store indices of outlet nodes
  !-------------------------------------------------------------
  call logent('Storing indices of outlet nodes')

  nOutlet = 0
  do k = 1, nCh
    ch => lst_ch(k)
    do jNode = 1, 2
      nd => ch%node(jNode)
      if( nd%is_outlet ) call add(nOutlet)
    enddo  ! jNode/
  enddo  ! k/

  call logmsg('Outlets: '//str(nOutlet))
  allocate(lst_outlet(nOutlet))

  nOutlet = 0
  do k = 1, nCh
    ch => lst_ch(k)
    do jNode = 1, 2
      nd => ch%node(jNode)
      if( .not. nd%is_outlet ) cycle
      call add(nOutlet)
      outlet => lst_outlet(nOutlet)
      outlet%iCh = k
      outlet%jNode = jNode
      outlet%x = xs_of_lon(nd%lon)
      outlet%y = ys_of_lat(nd%lat)
    enddo  ! jNode/
  enddo  ! k/

  call logext()
  !-------------------------------------------------------------
  ! Calc. connections
  !-------------------------------------------------------------
  call logent('Calculating connections')

  do k = 1, nCh
    ch => lst_ch(k)

    do jNode = 1, 2
      nd => ch%node(jNode)

      call search_nearest(nd%lon, nd_lon, ksNode, keNode)
      if( ksNode == keNode )then
        if( nd_iCh(ksNode) /= k )then
          call errend(msg_unexpected_condition()//&
                    '\n  ksNode == keNode .and. nd_iCh(ksNode) /= k'//&
                    '\nLongitude was not found.')
        endif
        nd%nNode_conn = 0
        cycle
      elseif( ksNode < 1 .or. keNode > nnNode )then
        call errend(msg_unexpected_condition()//&
                  '\n  ksNode < 1 .or. keNode > nnNode'//&
                  '\nLongitude is out of range.')
      endif
      k0Node = ksNode - 1
      call search_nearest(nd%lat, nd_lat(ksNode:keNode), ksNode, keNode)
      ksNode = ksNode + k0Node
      keNode = keNode + k0Node
      if( ksNode == keNode )then
        if( nd_iCh(ksNode) /= k )then
          call errend(msg_unexpected_condition()//&
                    '\n  nd_iCh(ksNode) /= k'//&
                    '\nLatitude was not found.')
        endif
        nd%nNode_conn = 0
        cycle
      elseif( ksNode < 1 .or. keNode > nnNode )then
        call errend(msg_unexpected_condition()//&
                  '\n  ksNode < 1 .or. keNode > nnNode'//&
                  '\nLatitude is out of range.')
      endif

      nd%nNode_conn = keNode - ksNode
      allocate(nd%node_conn(nd%nNode_conn))

      nd%nNode_conn = 0
      do kNode = ksNode, keNode
        if( nd_iCh(kNode) == k ) cycle
        call add(nd%nNode_conn)
        nd%node_conn(nd%nNode_conn)%iCh = nd_iCh(kNode)
        nd%node_conn(nd%nNode_conn)%jNode = nd_jNode(kNode)
      enddo
      if( nd%nNode_conn /= size(nd%node_conn) )then
        call errend(msg_unexpected_condition()//&
          '\n  nd%nNode_conn /= size(nd%node_conn)'//&
          '\nchannel #'//str(k)//' node #'//str(jNode)//&
          '\nnd%nNode_conn: '//str(nd%nNode_conn)//&
          '\nsize(nd%node_conn): '//str(size(nd%node_conn)))
      endif
    enddo  ! jNode/

    ch%nCh_conn = sum(ch%node(:)%nNode_conn)
    allocate(ch%ch_conn(ch%nCh_conn))
    ch%nCh_conn = 0
    do jNode = 1, 2
      nd => ch%node(jNode)
      do iNode_conn = 1, nd%nNode_conn
        nd_conn => nd%node_conn(iNode_conn)
        call add(ch%nCh_conn)
        ch_conn => ch%ch_conn(ch%nCh_conn)
        ch_conn%jNode_self = jNode
        ch_conn%iCh   = nd_conn%iCh
        ch_conn%jNode = nd_conn%jNode
      enddo
    enddo
  enddo  ! k/

if( debug )then
  k = k_debug
  ch => lst_ch(k)
  call logmsg('ch('//str(k)//')')
  do jNode = 1, 2
    nd => ch%node(jNode)
    call logmsg('node('//str(jNode)//') nNode_conn: '//str(nd%nNode_conn))
    do iNode_conn = 1, nd%nNode_conn
      nd_conn => nd%node_conn(iNode_conn)
      call logmsg('  ch('//str(nd_conn%iCh,dgt(nCh))//&
          ') nd('//str(nd_conn%jNode)//')')
    enddo  ! iNode_conn/
  enddo  ! jNode/
endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(nd_lon)
  deallocate(nd_lat)
  deallocate(nd_iCh)
  deallocate(nd_jNode)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_river_network
!===============================================================
!
!===============================================================
subroutine make_river_shape()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_river_shape'

  type(ch_), pointer :: ch
  type(pt_), pointer :: pt1, pt2
  integer :: k
  integer :: iPt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! TMP
  do k = 1, nCh
    ch => lst_ch(k)

    ch%pt(:)%width = 5.d0
    ch%pt(:)%depth = 5.d0
    ch%pt(:)%levee = 0.d0
  enddo
  !-------------------------------------------------------------
  ! Calc. cross section parameters
  !-------------------------------------------------------------
  call logent('Calculating cross section parameters')

  allocate(area_riv_idx(nCh))

  do k = 1, nCh
    ch => lst_ch(k)

    ! Length between points and length of channel
    ch%leng = 0.d0
    ch%width = 0.d0
    do iPt = 1, ch%nPt-1
      pt1 => ch%pt(iPt)
      pt2 => ch%pt(iPt+1)

      pt1%leng = dist_sphere(&
          pt1%lon*d2r, pt1%lat*d2r, pt2%lon*d2r, pt2%lat*d2r &
      ) * earth_r

      call add(ch%leng, pt1%leng)

      call add(ch%width, (pt1%width + pt2%width)*0.5d0*pt1%leng)
      call add(ch%depth, (pt1%depth + pt2%depth)*0.5d0*pt1%leng)
      call add(ch%levee, (pt1%levee + pt2%levee)*0.5d0*pt1%leng)
    enddo  ! ip/
    ch%pt(ch%nPt)%leng = 0.d0

    call mul(ch%width, 1.d0/ch%leng)
    call mul(ch%depth, 1.d0/ch%leng)
    call mul(ch%levee, 1.d0/ch%leng)

    ch%area = ch%width * ch%leng

    area_riv_idx(k) = ch%area

    !call logmsg(str(k,dgt(nCh))//' width '//str(ch%width)//&
    !            ' depth '//str(ch%depth)//' levee '//str(ch%levee)//&
    !            ' leng '//str(ch%leng)//' area '//str(ch%area))
    if( ch%leng < 1d-12 )then
      call logwrn(str(k,dgt(nCh))//' leng '//str(ch%leng))
    endif
  enddo  ! k/

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_river_shape
!===============================================================
!
!===============================================================
subroutine calc_river_cell_relations()
  use mod_mesh, only: &
        west_of_x, &
        east_of_x, &
        south_of_y, &
        north_of_y, &
        xs_of_lon, &
        xe_of_lon, &
        ys_of_lat, &
        ye_of_lat, &
        apprx_isct_with_meridian, &
        apprx_isct_with_parallel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_river_cell_relations'

  type(ch_), pointer :: ch
  type(pt_), pointer :: pt1, pt2
  type(ch_mesh_), pointer :: mesh
  real(8) :: wlon, wlat, elon, elat
  real(8) :: clon_west, clat_west, clon_east, clat_east
  real(8) :: dlon_west, dlat_west, dlon_east, dlat_east
  integer :: xs, xe, ix, ys, ye, iy
  real(8) :: leng, leng_sum
  integer :: k
  integer :: iPt
  integer :: iMesh
  integer :: szIsct_ch, nIsct_ch
  integer :: szIsct_pt, nIsct_pt
  integer :: is, ie, iis, iie
  integer, pointer :: isct_pt_x(:), isct_pt_y(:)
  real(8), pointer :: isct_pt_leng(:)
  integer, pointer :: isct_ch_x(:), isct_ch_y(:)
  real(8), pointer :: isct_ch_leng(:)
  integer, pointer :: arg(:)

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  szIsct_pt = nCh * 100
  allocate(isct_pt_x(szIsct_pt))
  allocate(isct_pt_y(szIsct_pt))
  allocate(isct_pt_leng(szIsct_pt))
  nullify(arg)

  szIsct_ch = nCh * 10
  allocate(isct_ch_x(szIsct_ch))
  allocate(isct_ch_y(szIsct_ch))
  allocate(isct_ch_leng(szIsct_ch))

  do k = 1, nCh
    ch => lst_ch(k)
    !-----------------------------------------------------------
    ! Calc. intersections for each section
    !-----------------------------------------------------------
    nIsct_pt = 0
    do iPt = 1, ch%nPt-1
      pt1 => ch%pt(iPt)
      pt2 => ch%pt(iPt+1)

      if( pt1%lon < pt2%lon )then
        wlon = pt1%lon
        wlat = pt1%lat
        elon = pt2%lon
        elat = pt2%lat
      else
        wlon = pt2%lon
        wlat = pt2%lat
        elon = pt1%lon
        elat = pt1%lat
      endif
      !---------------------------------------------------------
      ! Case: north to south
      if( wlat > elat )then
        ys = ys_of_lat(wlat)
        ye = ye_of_lat(elat)
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

          call func_x()
        enddo  ! iy/
      !---------------------------------------------------------
      ! Case: south to north
      else
        ys = ys_of_lat(elat)
        ye = ye_of_lat(wlat)
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

          call func_x()
        enddo  ! iy/
      endif
    enddo  ! iPt/
    !-----------------------------------------------------------
    ! Sum. leng. for each pair of (x,y)
    !-----------------------------------------------------------
    call realloc(arg, szIsct_pt)
    call argsort(isct_pt_x(:nIsct_pt), arg(:nIsct_pt))
    call sort(isct_pt_x(:nIsct_pt), arg(:nIsct_pt))
    call sort(isct_pt_y(:nIsct_pt), arg(:nIsct_pt))
    call sort(isct_pt_leng(:nIsct_pt), arg(:nIsct_pt))

    nIsct_ch = 0
    ie = 0
    do while( ie < nIsct_pt )
      is = ie + 1
      ie = is
      do while( ie < nIsct_pt )
        if( isct_pt_x(ie+1) /= isct_pt_x(is) ) exit
        ie = ie + 1
      enddo
      call argsort(isct_pt_y(is:ie), arg(is:ie))
      call sort(isct_pt_y(is:ie), arg(is:ie))
      call sort(isct_pt_leng(is:ie), arg(is:ie))
      iie = is - 1
      do while( iie < ie )
        iis = iie + 1
        iie = iis
        do while( iie < ie )
          if( isct_pt_y(iie+1) /= isct_pt_y(iis) ) exit
          iie = iie + 1
        enddo
        if( nIsct_ch == szIsct_ch )then
          call mul(szIsct_ch, 2)
          call realloc(isct_ch_x, szIsct_ch, clear=.false.)
          call realloc(isct_ch_y, szIsct_ch, clear=.false.)
          call realloc(isct_ch_leng, szIsct_ch, clear=.false.)
        endif
        call add(nIsct_ch)
        isct_ch_x(nIsct_ch) = isct_pt_x(iis)
        isct_ch_y(nIsct_ch) = isct_pt_y(iis)
        isct_ch_leng(nIsct_ch) = sum(isct_pt_leng(iis:iie))
      enddo  ! iie/
    enddo  ! ie/

    ch%nMesh = nIsct_ch
    allocate(ch%mesh(ch%nMesh))
    leng_sum = sum(isct_ch_leng(:ch%nMesh))
    do iMesh = 1, ch%nMesh
      mesh => ch%mesh(iMesh)
      mesh%x = isct_ch_x(iMesh)
      mesh%y = isct_ch_y(iMesh)
      mesh%leng = ch%leng * isct_ch_leng(iMesh) / leng_sum
      mesh%area = mesh%leng * ch%width

      mesh%is_outside_domain = .false.
      if( mesh%x < 1 .or. mesh%x > nx .or. mesh%y < 1 .or. mesh%y > ny )then
        mesh%is_outside_domain = .true.
      elseif( domain(mesh%x,mesh%y) == DOMAIN_OUTSIDE )then
        mesh%is_outside_domain = .true.
      endif
    enddo
  enddo  ! k/

  deallocate(arg)
  deallocate(isct_pt_x)
  deallocate(isct_pt_y)
  deallocate(isct_pt_leng)
  deallocate(isct_ch_x)
  deallocate(isct_ch_y)
  deallocate(isct_ch_leng)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine func_x()
  implicit none

  xs = xs_of_lon(clon_west)
  xe = xe_of_lon(clon_east)
  do ix = xs, xe
    if( ix == xs )then
      dlon_west = clon_west
      dlat_west = clat_west
    else
      dlon_west = dlon_east
      dlat_west = clat_east
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
        dlon_east*d2r, dlat_east*d2r &
    ) * earth_r
    if( leng < 1d-12 ) cycle

    if( nIsct_pt == szIsct_pt )then
      call mul(szIsct_pt, 2)
      call realloc(isct_pt_x, szIsct_pt, clear=.false.)
      call realloc(isct_pt_y, szIsct_pt, clear=.false.)
      call realloc(isct_pt_leng, szIsct_pt, clear=.false.)
    endif
    call add(nIsct_pt)
    isct_pt_x(nIsct_pt) = ix
    isct_pt_y(nIsct_pt) = iy
    isct_pt_leng(nIsct_pt) = leng
  enddo  ! ix/

  if( .not. allow_channel_outside_domain )then
    if( xs < 1 .or. xe > nx .or. ys < 1 .or. ye > ny )then
      call errend('Channel intersects the mesh outside the domain.'//&
                '\nchannel '//str(k)//&
                '\npoint '//str(iPt)//' ('//str(pt1%lon,'f12.7')//','//str(pt1%lat,'f11.7')//')'//&
                '\n      '//str(iPt+1)//' ('//str(pt2%lon,'f12.7')//','//str(pt2%lat,'f11.7')//')'//&
                '\nx: '//str((/xs,xe/),' - ')//&
                '\ny: '//str((/ys,ye/),' - '))
    endif
  endif
end subroutine func_x
!---------------------------------------------------------------
end subroutine calc_river_cell_relations
!===============================================================
!
!===============================================================
subroutine get_topo_map()
  use mod_mesh, only: &
        west_of_x, &
        east_of_x, &
        south_of_y, &
        north_of_y
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_topo_map'

  type(ch_), pointer :: ch
  type(pt_), pointer :: pt
  type(nd_), pointer :: nd
  integer :: k
  integer :: iPt
  integer :: jNode
  integer :: ix, iy

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(zs(nx,ny))
  call traperr( rbin(zs, file_elvtn, dtype=DTYPE_REAL) )

  allocate(domain(nx,ny))
  where( zs <= ZS_MISS_THRESH )
    domain = DOMAIN_OUTSIDE
  elsewhere
    domain = DOMAIN_INSIDE
  endwhere

  ! Check relations of channels and domain
  do k = 1, nCh
    ch => lst_ch(k)
    do iPt = 1, ch%nPt
      pt => ch%pt(iPt)

      if( allow_channel_outside_domain )then
        if( pt%x < 1 .or. pt%x > nx .or. pt%y < 1 .or. pt%y > ny )then
          cycle
        elseif( domain(pt%x,pt%y) == DOMAIN_OUTSIDE )then
          cycle
        endif
      else
        if( pt%x < 1 .or. pt%x > nx .or. pt%y < 1 .or. pt%y > ny )then
          call errend('Point is outside the domain.'//&
                     '\n  channel: '//str(k)//&
                     '\n  point: '//str(iPt)//&
                     '\n  (lon,lat): ('//str(pt%lon,'f12.7')//','//str(pt%lat,'f11.7')//')'//&
                     '\n  (x,y): ('//str((/pt%x,pt%y/),',')//')')
        elseif( domain(pt%x,pt%y) == DOMAIN_OUTSIDE )then
          call errend('Point is outside the domain.'//&
                     '\n  channel: '//str(k)//&
                     '\n  point: '//str(iPt)//&
                     '\n  (lon,lat): ('//str(pt%lon,'f12.7')//','//str(pt%lat,'f11.7')//')'//&
                     '\n  (x,y): ('//str((/pt%x,pt%y/),',')//')'//&
                     '\n  zs: '//str(zs(pt%x,pt%y)))
        endif
      endif

      if( iPt == 1 )then
        nd => ch%node(1)
      elseif( iPt == ch%nPt )then
        nd => ch%node(2)
      else
        cycle
      endif
      if( nd%is_outlet )then
        domain(pt%x,pt%y) = DOMAIN_OUTLET
      endif
    enddo  ! iPt/
  enddo  ! k/

  ! Land use
  allocate(land(nx,ny))
  if( file_landuse == '' )then
    land(:,:) = 1
  else
    call traperr( rbin(land, file_landuse, dtype=DTYPE_INT1) )
  endif

  ! Calc. zb, area_slo
  allocate(zb(nx,ny))
  allocate(area_slo(nx,ny))
  zb(:,:) = ZB_MISS
  area_slo(:,:) = 0.d0
  do iy = 1, ny
  do ix = 1, nx
    if( domain(ix,iy) == DOMAIN_OUTSIDE ) cycle

    zb(ix,iy) = zs(ix,iy) - soildepth(land(ix,iy))

    area_slo(ix,iy) = area_sphere_rect(&
        south_of_y(iy)*d2r, north_of_y(iy)*d2r &
    ) * (east_of_x(ix)-west_of_x(ix))*d2r * earth_r**2
    !area_slo(ix,iy) = area_sphere_rect(&
    !    south_of_y(ny/2)*d2r, north_of_y(ny/2)*d2r) &
    !    * (east_of_x(nx/2)-west_of_x(nx/2))*d2r * earth_r**2
  enddo  ! ix/
  enddo  ! iy/

  ! Mark outlet cell on $domain
  do k = 1, nCh
    ch => lst_ch(k)
    do jNode = 1, 2
      if( jNode == 1 )then
        pt => ch%pt(1)
      else
        pt => ch%pt(ch%nPt)
      endif
      nd => ch%node(jNode)
      if( nd%is_outlet )then
        if( domain(pt%x,pt%y) == DOMAIN_INSIDE )then
          domain(pt%x,pt%y) = DOMAIN_OUTLET
        endif
      endif
    enddo  ! jNode/
  enddo  ! k/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_topo_map
!===============================================================
!
!===============================================================
subroutine make_slope_1d()
  use mod_mesh, only: &
        west_of_x, &
        east_of_x, &
        south_of_y, &
        north_of_y, &
        lon_center_of_x, &
        lat_center_of_y
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_slope_1d'

  integer :: ix, iy, xx, yy
  integer :: k
  integer :: l
  integer :: lnd
  real(8) :: len

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nSlo = 0
  do iy = 1, ny
  do ix = 1, nx
    if( domain(ix,iy) /= DOMAIN_OUTSIDE ) call add(nSlo)
  enddo
  enddo

  allocate(slo_idx2i(nSlo))
  allocate(slo_idx2j(nSlo))
  allocate(slo_ij2idx(nx,ny))
  allocate(domain_slo_idx(nSlo))
  allocate(down_slo_idx(lmax,nSlo))
  allocate(area_slo_idx(nSlo))
  allocate(dis_slo_idx(lmax,nSlo))
  allocate(len_slo_idx(lmax,nSlo))
!  allocate(acc_slo_idx(nSlo))
!  allocate(down_slo_1d_idx(nSlo))
!  allocate(dis_slo_1d_idx(nSlo))
!  allocate(len_slo_1d_idx(nSlo))
  allocate(land_idx(nSlo))
  allocate(zb_slo_idx(nSlo))

  allocate(dif_slo_idx(nSlo))
  allocate(ns_slo_idx(nSlo))
  allocate(soildepth_idx(nSlo))
  allocate(gammaa_idx(nSlo))

  allocate(ksv_idx(nSlo))
  allocate(faif_idx(nSlo))
  allocate(infilt_limit_idx(nSlo))
  allocate(ka_idx(nSlo))
  allocate(km_idx(nSlo))
  allocate(gammam_idx(nSlo))
  allocate(beta_idx(nSlo))
  allocate(da_idx(nSlo))
  allocate(dm_idx(nSlo))
  allocate(ksg_idx(nSlo))
  allocate(gammag_idx(nSlo))
  allocate(kg0_idx(nSlo))
  allocate(fpg_idx(nSlo))
  allocate(rgl_idx(nSlo))

  k = 0
  slo_ij2idx(:,:) = 0
  do iy = 1, ny
  do ix = 1, nx
    if( domain(ix,iy) == DOMAIN_OUTSIDE ) cycle

    call add(k)
    slo_idx2i(k) = ix
    slo_idx2j(k) = iy
    slo_ij2idx(ix,iy) = k

    domain_slo_idx(k) = domain(ix,iy)
    zb_slo_idx(k) = zb(ix,iy)
!    acc_slo_idx(k) = acc(ix,iy)
    land_idx(k) = land(ix,iy)
    area_slo_idx(k) = area_slo(ix,iy)

    lnd = land(ix,iy)

    dif_slo_idx(k) = dif(lnd)
    ns_slo_idx(k) = ns_slope(lnd)
    soildepth_idx(k) = soildepth(lnd)
    gammaa_idx(k) = gammaa(lnd)

    ksv_idx(k) = ksv(lnd)
    faif_idx(k) = faif(lnd)
    infilt_limit_idx(k) = infilt_limit(lnd)

    ka_idx(k) = ka(lnd)
    km_idx(k) = km(lnd)
    gammam_idx(k) = gammam(lnd)
    beta_idx(k) = beta(lnd)
    da_idx(k) = da(lnd)
    dm_idx(k) = dm(lnd)

    ksg_idx(k) = ksg(lnd)
    gammag_idx(k) = gammag(lnd)
    kg0_idx(k) = kg0(lnd)
    fpg_idx(k) = fpg(lnd)
    rgl_idx(k) = rgl(lnd)
  enddo  ! ix/
  enddo  ! iy/

  ! Search for downstream cell
  k = 0
  down_slo_idx(:,:) = -1
  do iy = 1, ny
  do ix = 1, nx
    if( domain(ix,iy) == DOMAIN_OUTSIDE ) cycle

    call add(k)
    ! 8-direction: lmax = 4, 4-direction: lmax = 2
    do l = 1, lmax ! (1: right，2: down, 3: right down, 4: left down)
      selectcase( l )
      case( 1 )
        xx = ix + 1
        yy = iy
        ! len = dy / 2
        len = dist_sphere(&
            east_of_x(ix)*d2r, south_of_y(iy)*d2r, &
            east_of_x(ix)*d2r, north_of_y(iy)*d2r) &
            / 2.d0 * earth_r
      case( 2 )
        xx = ix
        yy = iy + 1
        ! len = dx / 2
        len = dist_sphere(&
            west_of_x(ix)*d2r, south_of_y(iy)*d2r, &
            east_of_x(ix)*d2r, south_of_y(iy)*d2r) &
            / 2.d0 * earth_r
      case( 3 )
        xx = ix + 1
        yy = iy + 1
        ! len = sqrt(dx**2 + dy**2) / 4
        len = dist_sphere(&
            west_of_x(ix)*d2r, south_of_y(iy)*d2r, &
            east_of_x(ix)*d2r, north_of_y(iy)*d2r) &
            / 4.d0 * earth_r
      case( 4 )
        xx = ix - 1
        yy = iy + 1
        len = dist_sphere(&
            west_of_x(ix)*d2r, north_of_y(iy)*d2r, &
            east_of_x(ix)*d2r, south_of_y(iy)*d2r) &
            / 4.d0 * earth_r
      endselect

      if( xx < 1 .or. xx > nx .or. yy < 1 .or. yy > ny ) cycle
      if( domain(xx,yy) == DOMAIN_OUTSIDE ) cycle

      down_slo_idx(l,k) = slo_ij2idx(xx,yy)
      dis_slo_idx(l,k) = dist_sphere(&
          lon_center_of_x(ix)*d2r, lat_center_of_y(iy)*d2r, &
          lon_center_of_x(xx)*d2r, lat_center_of_y(yy)*d2r) &
          * earth_r
      len_slo_idx(l,k) = len
    enddo  ! l/
  enddo  ! ix/
  enddo  ! iy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_slope_1d
!===============================================================
!
!===============================================================
subroutine make_river_topo()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_river_topo'

  type(ch_), pointer :: ch
  type(ch_mesh_), pointer :: mesh
  real(8) :: leng_sum
  integer :: k
  integer :: iMesh

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(zb_riv_idx(nCh))

  do k = 1, nCh
    ch => lst_ch(k)

    ch%zb = 0.d0
    leng_sum = 0.d0
    do iMesh = 1, ch%nMesh
      mesh => ch%mesh(iMesh)
      if( mesh%is_outside_domain ) cycle
      call add(ch%zb, (zs(mesh%x,mesh%y) - ch%depth) * mesh%leng)
      call add(leng_sum, mesh%leng)
    enddo
    ! TMP
    if( leng_sum < 1d-12 )then
      call logwrn('ch('//str(k)//') nMesh: '//str(ch%nMesh)//' leng: '//str(leng_sum))
      ch%zb = 1d-3
    else
      call mul(ch%zb, 1.d0/leng_sum)
    endif
    zb_riv_idx(k) = ch%zb
  enddo  ! k/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_river_topo
!===============================================================
!
!===============================================================
end module mod_set
