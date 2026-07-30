module mod_modify_channeldir
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: modifyChannelDir
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_modify_channeldir'

  type way_
    integer :: n
    integer, allocatable :: iNode(:)
    integer, allocatable :: jCh(:)
    integer, allocatable :: jNode(:)
    real(8) :: leng
  end type

  type nwknode_src2out_
    integer :: n
    integer, pointer :: iNode(:)
    logical :: is_new
  end type

  type nwknode_
    real(8) :: lon, lat
    integer :: gx, gy
    real(8) :: elv
    integer :: nCh
    integer, pointer :: jCh(:)
    integer, pointer :: jNode(:)
    integer, pointer :: iNode(:)  ! neighbour nwknode
    real(8) :: score_source, score_outlet
    integer(1) :: typ
    type(nwknode_src2out_) :: src2out
    real(8) :: dist2sea
    real(8) :: downleng
    integer :: iNode_down
    integer :: iCh_down  ! index of %iNode_down in %iNode
    real(8) :: upleng
  end type

  type node_
    real(8) :: lon, lat
    real(8) :: theta
    integer :: iNode
    real(8) :: elv
  end type

  type channel_
    integer :: n
    real(8), pointer :: lon(:), lat(:)
    type(node_), pointer :: node(:)
    real(8) :: leng
    real(8) :: slope
    integer(1) :: dir
    real(8) :: upleng_fwrd, upleng_back
  end type

  type network_
    character(:), allocatable :: uid
    integer :: nCh
    type(channel_), pointer :: channel(:)
    integer :: nNode
    type(nwknode_), pointer :: node(:)
    real(8) :: west, east, south, north
    integer :: gxs, gxe, gys, gye
  end type

  type point_
    integer :: iNode_now
    real(8) :: dist_now
    real(8) :: dist_tot
    real(8) :: lon, lat
  end type

  real(8), parameter :: THRESH_ELV__SRC = 100.d0
  real(8), parameter :: THRESH_ELV__OUT = 50.d0

  real(8), parameter :: THRESH_DIST2SEA_SRC = 1.d2  ![m]

  real(8), parameter :: ELV__FORMISS = 0.d0
  real(8), parameter :: SLOPE__MISS = -9999.d0

  real(8), parameter :: DIST_INTVL = 1.d2  ![m]

  integer(1), parameter :: NODETYPE__SRC     = 1
  integer(1), parameter :: NODETYPE__OUT     = 2
  integer(1), parameter :: NODETYPE__DEADEND = 3
  integer(1), parameter :: NODETYPE__UNDEF   = -9

  integer(1), parameter :: DIR__FWRD    = 1
  integer(1), parameter :: DIR__BACK    = 2
  integer(1), parameter :: DIR__UNKNOWN = -9
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine modifyChannelDir(uid)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modifyChannelDir'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call modify_channeldir(uid)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine modifyChannelDir
!===============================================================
!
!===============================================================
subroutine modify_channeldir(uid)
  use c2_jflw_const, &
        set_resolution => set_resolution
  use c2_jflw_grid, only: &
        gxs_of_lon  , &
        gxe_of_lon  , &
        gys_of_lat  , &
        gye_of_lat  , &
        center_of_gx, &
        center_of_gy, &
        west_of_gx  , &
        east_of_gx  , &
        south_of_gy , &
        north_of_gy
  use c2_jflw_io, only: &
        read_map_from_tile
  use c2_strnk_io, only: &
        strnk_get_f_network_channel => get_f_network_channel
  use c3_joint_io, only: &
        joint_get_f_nwk_chn_dirstat => get_f_nwk_chn_dirstat
  use mod_util, only: &
        jNode2jPt, &
        slonlat, &
        sBBox, &
        sMeshRange
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_channeldir'
  character(*), intent(in) :: uid

  type(network_) :: nwk
  type(channel_), pointer :: ch, ch2
  type(node_), pointer :: node !, node2
  type(nwknode_), pointer :: nwknode, nwknode2
  real(4), allocatable :: elvmap(:,:)
  logical, allocatable :: mask(:,:)
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_jCh(:), lst_jNode(:), lst_iNode(:)
  type(point_), pointer :: point(:), p
  integer, allocatable :: arg(:)
  integer :: jCh, jCh2
  integer :: iiCh
  integer :: jNode
  integer :: iNode, iNode2
  integer :: iiNode
  integer :: jPt, jPt1, jPt2
  integer :: is, ie, iis, iie
  integer :: gx, gy
  real(4) :: elv_ll, elv_lr, elv_ul, elv_ur
  real(8) :: dlon, dlat
  integer :: n_updated, n_updated_next
  real(8) :: lonrange, latrange
  integer :: gxrange, gyrange
  real(8) :: west, east, south, north
  real(8) :: dist_west, dist_east, dist_south, dist_north
  integer :: igx, igy
  integer, allocatable :: iNode_updated(:), iNode_updated_next(:)
  logical, allocatable :: is_updated_next(:)
  real(8), allocatable :: upleng_add(:), upleng_add_next(:)
  logical, allocatable :: has_been_counted(:)

  logical :: is_updated
  integer :: nNode_src
  real(8) :: dist, dist_add
  real(8) :: w
  integer :: jPt_bgn, jPt_end, jPt_int, jPt_next

  integer :: n
  integer :: i

  character(CLEN_PATH) :: f
  integer :: un
  integer :: dgt_nCh
  character(:), allocatable :: c1, c2

  logical :: debug = .true.
  logical :: debug_this
  integer :: jCh_debug, jNode_debug, iNode_debug
  type(channel_), pointer :: ch_debug
  type(node_), pointer :: node_debug
  type(nwknode_), pointer :: nwknode_debug

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  ! Read network data
  !-------------------------------------------------------------
  allocate(character(1) :: nwk%uid)
  nwk%uid = uid

  f = strnk_get_f_network_channel(nwk%uid, 'sbin')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, form='unformatted', access='sequential', &
       action='read', status='old')

  read(un) nwk%nCh, nwk%west, nwk%east, nwk%south, nwk%north

  allocate(nwk%channel(nwk%nCh))

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    read(un) ch%n  !, ch%west, ch%east, ch%south, ch%north

    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))

    read(un) ch%lon
    read(un) ch%lat

    allocate(ch%node(2))
    ch%node(1)%lon = ch%lon(1)
    ch%node(1)%lat = ch%lat(1)
    ch%node(2)%lon = ch%lon(ch%n)
    ch%node(2)%lat = ch%lat(ch%n)

    read(un) !ch%node(1)%typ, ch%node(2)%typ
  enddo  ! jCh/

  close(un)

  dgt_nCh = dgt(nwk%nCh)

  nwk%gxs = gxs_of_lon(nwk%west)
  nwk%gxe = gxe_of_lon(nwk%east)
  nwk%gys = gys_of_lat(nwk%north)
  nwk%gye = gye_of_lat(nwk%south)

  nwk%nNode = nwk%nCh * 2
  allocate(nwk%node(nwk%nNode))

  allocate(lst_lon(nwk%nNode))
  allocate(lst_lat(nwk%nNode))
  allocate(lst_jCh(nwk%nNode))
  allocate(lst_jNode(nwk%nNode))

  iNode = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    do jNode = 1, 2
      node => ch%node(jNode)
      call add(iNode)

      jPt = jNode2jPt(jNode, ch%n)
      lst_lon(iNode) = ch%lon(jPt)
      lst_lat(iNode) = ch%lat(jPt)
      lst_jCh(iNode) = jCh
      lst_jNode(iNode) = jNode
    enddo  ! jNode/
  enddo  ! jCh/

  iNode = 0
  allocate(arg(nwk%nNode))
  call argsort(lst_lon, arg)
  call sort(lst_lon, arg)
  call sort(lst_lat, arg)
  call sort(lst_jCh, arg)
  call sort(lst_jNode, arg)
  ie = 0
  do while( ie < nwk%nNode )
    is = ie + 1
    ie = is
    do while( ie < nwk%nNode )
      if( lst_lon(ie+1) /= lst_lon(is) ) exit
      ie = ie + 1
    enddo
    call argsort(lst_lat(is:ie), arg(is:ie))
    call sort(lst_lat(is:ie), arg(is:ie))
    call sort(lst_jCh(is:ie), arg(is:ie))
    call sort(lst_jNode(is:ie), arg(is:ie))
    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( lst_lat(iie+1) /= lst_lat(iis) ) exit
        iie = iie + 1
      enddo

      call add(iNode)
      nwknode => nwk%node(iNode)
      nwknode%lon = lst_lon(iis)
      nwknode%lat = lst_lat(iis)
      nwknode%gx = gxs_of_lon(nwknode%lon)
      nwknode%gy = gys_of_lat(nwknode%lat)
      nwknode%nCh = iie - iis + 1
      allocate(nwknode%jCh(nwknode%nCh))
      allocate(nwknode%jNode(nwknode%nCh))
      nwknode%jCh(:)   = lst_jCh(iis:iie)
      nwknode%jNode(:) = lst_jNode(iis:iie)
      do iiCh = 1, nwknode%nCh
        jCh = nwknode%jCh(iiCh)
        jNode = nwknode%jNode(iiCh)
        nwk%channel(jCh)%node(jNode)%iNode = iNode
      enddo  ! iiCh/
    enddo  ! iis, iie/
  enddo  ! is, ie/

  nwk%nNode = iNode

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    allocate(nwknode%iNode(nwknode%nCh))
    do iiCh = 1, nwknode%nCh
      jCh = nwknode%jCh(iiCh)
      jNode = nwknode%jNode(iiCh)
      nwknode%iNode(iiCh) = nwk%channel(jCh)%node(3-jNode)%iNode
    enddo  ! iCh/
  enddo  ! iNode/

  deallocate(arg)
  deallocate(lst_lon)
  deallocate(lst_lat)
  deallocate(lst_jCh)
  deallocate(lst_jNode)

! DEBUG
open(newunit=un, file='tmp/node.txt', status='replace')
do iNode = 1, nwk%nNode
  nwknode => nwk%node(iNode)
  write(un,"(a)") str((/nwknode%lon,nwknode%lat/),'es20.13')
enddo
close(un)
  !-------------------------------------------------------------
  ! (debug)
  !-------------------------------------------------------------
  if( debug )then
    jCh_debug = 0
    jNode_debug = 0
    do jCh = 1, nwk%nCh
      do jNode = 1, 2
        node => nwk%channel(jCh)%node(jNode)
        if( 130.51300 < node%lon .and. node%lon < 130.51310 .and. &
            33.20280 < node%lat .and. node%lat < 33.20288 )then
          jCh_debug = jCh
          jNode_debug = jNode
          iNode_debug = nwk%channel(jCh)%node(jNode)%iNode
        endif
      enddo
    enddo

    !jCh_debug = 745
    !jNode_debug = 1

    call logmsg('Debugging jCh: '//str(jCh_debug)//', jNode: '//str(jNode_debug)//&
         ', iNode: '//str(iNode_debug))

    if( jCh_debug > 0 )then
      ch_debug => nwk%channel(jCh_debug)
      node_debug => ch_debug%node(jNode_debug)
      nwknode_debug => nwk%node(node_debug%iNode)
      call logmsg('node nCh: '//str(nwknode_debug%nCh))
    endif
  endif
  !-------------------------------------------------------------
  ! Calculate elevation of nodes
  !-------------------------------------------------------------
  call logent('Calculating elevation of nodes')

  allocate(elvmap(nwk%gxs-2:nwk%gxe+2,nwk%gys-2:nwk%gye+2))
  call read_map_from_tile(&
      RESOLUTION_1SEC, 'elv', DTYPE_REAL, ELV_MISS, nwk%gxs-2, nwk%gys-2, elvmap)

  allocate(mask(3,3))

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    gx = gxs_of_lon(nwknode%lon)
    gy = gys_of_lat(nwknode%lat)
    if( nwknode%lon < center_of_gx(gx) ) gx = gx - 1
    if( nwknode%lat < center_of_gy(gy) ) gy = gy + 1
    dlon = nwknode%lon - center_of_gx(gx)
    dlat = nwknode%lat - center_of_gy(gy)
    if( dlon < 0.d0 .or. dlon > GRIDSIZE_LON .or. &
        dlat < 0.d0 .or. dlat > GRIDSIZE_LAT )then
      print*, dlon, dlat
      stop
    endif

    elv_ll = elvmap(gx  , gy+1)
    elv_lr = elvmap(gx+1, gy+1)
    elv_ul = elvmap(gx  , gy  )
    elv_ur = elvmap(gx+1, gy  )

    if( elv_ll == ELV_MISS )then
      mask = elvmap(gx-1:gx+1,gy:gy+2) /= ELV_MISS
      if( any(mask) )then
        elv_ll = sum(elvmap(gx-1:gx+1,gy:gy+2), mask=mask) / count(mask)
      endif
    endif
    if( elv_lr == ELV_MISS )then
      mask = elvmap(gx:gx+2,gy:gy+2) /= ELV_MISS
      if( any(mask) )then
        elv_lr = sum(elvmap(gx:gx+2,gy:gy+2), mask=mask) / count(mask)
      endif
    endif
    if( elv_ul == ELV_MISS )then
      mask = elvmap(gx-1:gx+1,gy-1:gy+1) /= ELV_MISS
      if( any(mask) )then
        elv_ul = sum(elvmap(gx-1:gx+1,gy-1:gy+1), mask=mask) / count(mask)
      endif
    endif
    if( elv_ur == ELV_MISS )then
      mask = elvmap(gx:gx+2,gy-1:gy+1) /= ELV_MISS
      if( any(mask) )then
        elv_ur = sum(elvmap(gx:gx+2,gy-1:gy+1), mask=mask) / count(mask)
      endif
    endif

    n = count((/elv_ll, elv_lr, elv_ul, elv_ur/) /= ELV_MISS)
    if( n == 0 )then
      nwknode%elv = ELV__FORMISS
    elseif( n <= 3 )then
      nwknode%elv = 0.d0
      if( elv_ll /= ELV_MISS ) call add(nwknode%elv, real(elv_ll,8))
      if( elv_lr /= ELV_MISS ) call add(nwknode%elv, real(elv_lr,8))
      if( elv_ul /= ELV_MISS ) call add(nwknode%elv, real(elv_ul,8))
      if( elv_ur /= ELV_MISS ) call add(nwknode%elv, real(elv_ur,8))
      call mul(nwknode%elv, 1.d0/n)
    else
      nwknode%elv = interp_bilinear(&
          real(elv_ll,8), real(elv_lr,8), real(elv_ul,8), real(elv_ur,8), &
          dlon/GRIDSIZE_LON, dlat/GRIDSIZE_LAT)
      if( nwknode%elv < 0.d0 )then
        call logmsg('elv('//str((/elv_ll,elv_lr,elv_ul,elv_ur/),'es10.3',',')//&
                    ') -> '//str(nwknode%elv,'es10.3')//&
            ' (x,y): ('//str((/dlon,dlat/),'es10.3',',')//')')
      endif
    endif

    do iiCh = 1, nwknode%nCh
      nwk%channel(nwknode%jCh(iiCh))%node(nwknode%jNode(iiCh))%elv = nwknode%elv
    enddo  ! iiCh/
  enddo  ! iNode/

  deallocate(mask)

  call logmsg('elvmap('//str(shape(elvmap),', ')//')')
  call logmsg(sBBox(nwk%west-GRIDSIZE_LON*2, nwk%east+GRIDSIZE_LON*2, &
              nwk%south-GRIDSIZE_LAT*2, nwk%north+GRIDSIZE_LAT*2))
  call traperr( wbin(elvmap, 'tmp/elvmap.bin', replace=.true.) )

  if( debug .and. jCh_debug > 0 )then
    call logmsg('elv: '//str(node_debug%elv))
  endif

  call logext()
  !-------------------------------------------------------------
  ! Calc. distance from coast lines
  !-------------------------------------------------------------
  call logent('Calculating distance from coast lines')

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    latrange = THRESH_DIST2SEA_SRC / EARTH_R * r2d
    lonrange = THRESH_DIST2SEA_SRC / (EARTH_R * cos(nwknode%lat*d2r)) * r2d

    gxrange = ceiling(lonrange / GRIDSIZE_LON)
    gyrange = ceiling(latrange / GRIDSIZE_LAT)
    west = west_of_gx(nwknode%gx - gxrange)
    east = east_of_gx(nwknode%gx + gxrange)
    south = south_of_gy(nwknode%gy + gyrange)
    north = north_of_gy(nwknode%gy - gyrange)
    dist_west = dist_sphere(west*d2r, nwknode%lat*d2r, nwknode%lon*d2r, nwknode%lat*d2r) * EARTH_R
    dist_east = dist_sphere(east*d2r, nwknode%lat*d2r, nwknode%lon*d2r, nwknode%lat*d2r) * EARTH_R
    dist_south = dist_sphere(nwknode%lon*d2r, south*d2r, nwknode%lon*d2r, nwknode%lat*d2r) * EARTH_R
    dist_north = dist_sphere(nwknode%lon*d2r, north*d2r, nwknode%lon*d2r, nwknode%lat*d2r) * EARTH_R
    if( dist_west < THRESH_DIST2SEA_SRC .or. dist_east < THRESH_DIST2SEA_SRC )then
      call errend('dist < thresh'//&
                '\n  lonrange: '//str(lonrange)//&
                '\n  gxrange: '//str(gxrange)//&
                '\n  west: '//str(west)//', east: '//str(east)//&
                '\n  dist_west: '//str(dist_west)//&
                '\n  dist_east: '//str(dist_east))
    endif
    if( dist_south < THRESH_DIST2SEA_SRC .or. dist_north < THRESH_DIST2SEA_SRC )then
      call errend('dist < thresh'//&
                '\n  latrange: '//str(latrange)//&
                '\n  gyrange: '//str(gyrange)//&
                '\n  south: '//str(south)//', north: '//str(north)//&
                '\n  dist_south: '//str(dist_south)//&
                '\n  dist_north: '//str(dist_north))
    endif

    nwknode%dist2sea = EARTH_R
    do igy = max(nwknode%gy-gyrange,nwk%gys), min(nwknode%gy+gyrange,nwk%gye)
    do igx = max(nwknode%gx-gxrange,nwk%gxs), min(nwknode%gx+gxrange,nwk%gxe)
      if( elvmap(igx,igy) /= ELV_MISS ) cycle
      nwknode%dist2sea = min(nwknode%dist2sea, &
          dist_sphere(nwknode%lon*d2r, nwknode%lat*d2r, &
                      center_of_gx(igx)*d2r, center_of_gy(igy)*d2r) * EARTH_R)
    enddo  ! igx/
    enddo  ! igy/
  enddo  ! iNode/

  deallocate(elvmap)

  call logext()
  !-------------------------------------------------------------
  ! Set node type
  !-------------------------------------------------------------
  call logent('Setting types of nodes')

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    if( nwknode%nCh == 1 )then
      if( nwknode%elv > THRESH_ELV__SRC .or. &
          nwknode%dist2sea > THRESH_DIST2SEA_SRC )then
        nwknode%typ = NODETYPE__SRC

      elseif( nwknode%elv < THRESH_ELV__OUT .and. &
              nwknode%dist2sea < THRESH_DIST2SEA_SRC )then
        nwknode%typ = NODETYPE__OUT

      else
        nwknode%typ = NODETYPE__DEADEND
      endif
    else
      nwknode%typ = NODETYPE__UNDEF
    endif
  enddo  ! iNode/

!DEBUG
open(newunit=un, file='tmp/nodetype1.txt', status='replace')
do iNode = 1, nwk%nNode
  write(un,*) nwk%node(iNode)%typ
enddo
close(un)

  call logext()
  !-------------------------------------------------------------
  ! Calc. slope of channels and angles of head and tail
  !-------------------------------------------------------------
  call logent('Calculating slopes and angles')

  allocate(character(1) :: c1, c2)

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    do jNode = 1, 2
      node => ch%node(jNode)
      jPt = jNode2jPt(jNode, ch%n)

      if( jNode == 1 )then
        jPt2 = 2
      else
        jPt2 = ch%n
      endif
      jPt1 = jpt2 - 1

      node%theta = atan2(ch%lat(jPt2) - ch%lat(jPt1), ch%lon(jPt2) - ch%lon(jPt1)) * r2d

      !debug_this = debug .and. jCh == jCh_debug .and. jNode == jNode_debug
      debug_this = debug .and. jCh == jCh_debug
      if( debug_this )then
        call logmsg('lon: '//str(ch%lon(jPt1),'f12.7')//' -> '//str(ch%lon(jPt2),'f12.7')//&
                    ' ('//str(ch%lon(jPt2)-ch%lon(jPt1),'f12.7')//')'//&
                  '\nlat: '//str(ch%lat(jPt1),'f12.7')//' -> '//str(ch%lat(jPt2),'f12.7')//&
                    ' ('//str(ch%lat(jPt2)-ch%lat(jPt1),'f12.7')//')'//&
                  '\ntheta: '//str(node%theta,'f8.3'))
      endif
    enddo  ! jNode/

    ch%leng = 0.d0
    do jPt = 1, ch%n-1
      call add(ch%leng, dist_sphere(ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, &
          ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r))
    enddo
    call mul(ch%leng, EARTH_R)

    if( ch%node(1)%elv == ELV_MISS .or. ch%node(2)%elv == ELV_MISS )then
      ch%slope = SLOPE__MISS
    else
      ch%slope = ( ch%node(1)%elv - ch%node(2)%elv ) / ch%leng
    endif
  enddo  ! jCh/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Source to outlet')

  !allocate(is_updated(nwk%nNode))
  allocate(is_updated_next(nwk%nNode))
  allocate(iNode_updated(sum(nwk%node(:)%nCh)))
  allocate(iNode_updated_next(sum(nwk%node(:)%nCh)))

  ! Initialize
  nwk%node(1)%downleng = sum(nwk%channel(:)%leng)
  do iNode = 2, nwk%nNode
    nwknode => nwk%node(iNode)
    nwknode%downleng = nwk%node(1)%downleng
    nwknode%iNode_down = 0
  enddo

  ! Set leng. for outlets (leng = 0)
  n_updated = 0
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    if( nwknode%typ /= NODETYPE__OUT ) cycle
    call add(n_updated)
    iNode_updated(n_updated) = iNode
    nwknode%downleng = 0.d0
  enddo

  !call logmsg('Updated: '//str(n_updated))

  ! Find shortest way to outlet
  is_updated_next(:) = .false.
  do while( n_updated > 0 )
    n_updated_next = 0
    do i = 1, n_updated
      iNode = iNode_updated(i)
      nwknode => nwk%node(iNode)
!if( iNode == iNode_debug .or. any(nwknode%iNode == iNode_debug) )then
!  call logmsg('nd('//str(iNode)//') '//slonlat(nwknode%lon,nwknode%lat)//&
!      ' leng: '//str(nwknode%downleng))
!endif

      do iiCh = 1, nwknode%nCh
        jCh2 = nwknode%jCh(iiCh)
        ch2 => nwk%channel(jCh2)
        iNode2 = nwknode%iNode(iiCh)
        nwknode2 => nwk%node(iNode2)
!if( iNode2 == iNode_debug )then
!  call logmsg('  nd('//str(iNode2)//') '//slonlat(nwknode2%lon,nwknode2%lat)//&
!      ' leng: '//str(nwknode2%downleng)//' ch2 leng:'//str(ch2%leng))
!endif
        if( nwknode%downleng + ch2%leng < nwknode2%downleng )then
          is_updated_next(iNode2) = .true.
          call add(n_updated_next)
          iNode_updated_next(n_updated_next) = iNode2
          nwknode2%downleng = nwknode%downleng + ch2%leng
          nwknode2%iNode_down = iNode
        endif
      enddo
    enddo  ! i/

    ! Copy list of updated nodes avoiding duplication
    n_updated = 0
    do i = 1, n_updated_next
      if( is_updated_next(iNode_updated_next(i)) )then
        is_updated_next(iNode_updated_next(i)) = .false.
        call add(n_updated)
        iNode_updated(n_updated) = iNode_updated_next(i)
      endif
    enddo

    !call logmsg('Updated: '//str(n_updated))
  enddo  ! while n_updated > 0

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    if( nwknode%typ == NODETYPE__OUT ) cycle

    if( nwknode%iNode_down == 0 )then
      call errend('Missing value in node('//str(iNode)//')')
    endif

    nwknode%iCh_down = 0
    do iiCh = 1, nwknode%nCh
      if( nwknode%iNode(iiCh) == nwknode%iNode_down )then
        nwknode%iCh_down = iiCh
      endif
    enddo
    if( nwknode%iCh_down == 0 )then
      call errend('%iNode_down was not found in %iNode(:).')
    endif
  enddo  ! iNode/

  call logmsg('Max: '//str(maxval(nwk%node(:)%downleng)))

!DEBUG
open(newunit=un, file='tmp/node_downleng.txt', status='replace')
do iNode = 1, nwk%nNode
  write(un,*) nwk%node(iNode)%downleng
enddo
close(un)

nwknode => nwk%node(iNode_debug)
open(newunit=un, file='tmp/src2out.txt', status='replace')
do
  write(un,*) nwknode%lon, nwknode%lat
  if( nwknode%iCh_down == 0 ) exit
  nwknode => nwk%node(nwknode%iNode(nwknode%iCh_down))
enddo
close(un)


  deallocate(is_updated_next)
  deallocate(iNode_updated)
  deallocate(iNode_updated_next)

  call logext()
stop
  !-------------------------------------------------------------
  ! Calc. upleng of nodes
  !-------------------------------------------------------------
  call logent('Calculating upstream length of each node')

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    nwknode%upleng = 0.d0
  enddo  ! iNode/

  allocate(upleng_add(nwk%nNode))
  allocate(upleng_add_next(nwk%nNode))
  allocate(is_updated_next(nwk%nNode))
  allocate(iNode_updated(nwk%nNode))
  allocate(iNode_updated_next(nwk%nNode))
  allocate(has_been_counted(nwk%nCh))

  n_updated = 0
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    if( nwknode%typ /= NODETYPE__SRC ) cycle
    call add(n_updated)
    iNode_updated(n_updated) = iNode
    upleng_add(iNode) = 0.d0
  enddo  ! iNode/
  has_been_counted(:) = .false.
  is_updated_next(:) = .false.

  do while( n_updated > 0 )

    n_updated_next = 0
    do i = 1, n_updated
      iNode = iNode_updated(i)
      nwknode => nwk%node(iNode)
      if( nwknode%typ == NODETYPE__OUT ) cycle

      jCh = nwknode%jCh(nwknode%iCh_down)
      ch => nwk%channel(jCh)

      iNode2 = nwknode%iNode_down
      nwknode2 => nwk%node(iNode2)
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      if( .not. has_been_counted(jCh) )then
        has_been_counted(jCh) = .true.
        call add(upleng_add(iNode), ch%leng)
      endif

      call add(upleng_add_next(iNode2), upleng_add(iNode))

      upleng_add(iNode) = 0.d0

      if( .not. is_updated_next(iNode2) )then
        is_updated_next(iNode2) = .true.
        call add(n_updated_next)
        iNode_updated_next(n_updated_next) = iNode2
      endif
    enddo  ! i/

    n_updated = n_updated_next
    do i = 1, n_updated
      iNode2 = iNode_updated_next(i)

      call add(nwk%node(iNode2)%upleng, upleng_add_next(iNode2))

      is_updated_next(iNode2) = .false.
      iNode_updated(i) = iNode2
      upleng_add(iNode2) = upleng_add_next(iNode2)
      upleng_add_next(iNode2) = 0.d0
    enddo
  enddo  ! while n_updated > 0/

  call logmsg('Sum at outlets: '//str(sum(nwk%node(:)%upleng, mask=nwk%node(:)%typ==NODETYPE__OUT)))
  call logmsg('Total length  : '//str(sum(nwk%channel(:)%leng, mask=has_been_counted)))

  deallocate(upleng_add)
  deallocate(upleng_add_next)
  deallocate(is_updated_next)
  deallocate(iNode_updated)
  deallocate(iNode_updated_next)
  deallocate(has_been_counted)

  call logext()
  !-------------------------------------------------------------
  ! Set channel direction
  !-------------------------------------------------------------
  call logent('Setting directions of channels')

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    nwknode => nwk%node(ch%node(1)%iNode)
    nwknode2 => nwk%node(ch%node(2)%iNode)
    if( nwknode%downleng > nwknode2%downleng )then
      ch%dir = DIR__FWRD
    elseif( nwknode%downleng < nwknode2%downleng )then
      ch%dir = DIR__BACK
    else
      ch%dir = DIR__UNKNOWN
    endif
  enddo  ! jCh/

  nwk%channel(:)%upleng_fwrd = 0.d0
  nwk%channel(:)%upleng_back = 0.d0
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    if( nwknode%typ == NODETYPE__OUT ) cycle
    jCh = nwknode%jCh(nwknode%iCh_down)
    jNode = nwknode%jNode(nwknode%iCh_down)
    ch => nwk%channel(jCh)
    if( jNode == 1 )then
      call add(ch%upleng_fwrd, nwknode%upleng+ch%leng)
    else
      call add(ch%upleng_back, nwknode%upleng+ch%leng)
    endif
  enddo  ! iNode/

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( (ch%dir == DIR__FWRD .and. ch%upleng_back > 0.d0) .or. &
        (ch%dir == DIR__BACK .and. ch%upleng_fwrd > 0.d0) )then
      call errend('dir: '//str(ch%dir)//' upleng_fwrd: '//str(ch%upleng_fwrd)//&
          ' upleng_back: '//str(ch%upleng_back))
    endif
  enddo  ! jCh/

  if( any(nwk%channel(:)%upleng_fwrd == 0.d0 .and. nwk%channel(:)%upleng_back > 0.d0) )then
    call logmsg('Reversed: ')
    do jCh = 1, nwk%nCh
      ch => nwk%channel(jCh)
      if( ch%upleng_fwrd == 0.d0 .and. ch%upleng_back > 0.d0 )then
        call logmsg('  ch('//str(jCh,dgt_nCh)//') fwrd: '//str(ch%upleng_fwrd)//&
            ' back: '//str(ch%upleng_back))
      endif
    enddo  ! jCh/
  endif
  if( any(nwk%channel(:)%upleng_fwrd > 0.d0 .and. nwk%channel(:)%upleng_back > 0.d0) )then
    call logmsg('Non-uniform: ')
    do jCh = 1, nwk%nCh
      ch => nwk%channel(jCh)
      if( ch%upleng_fwrd > 0.d0 .and. ch%upleng_back > 0.d0 )then
        call logmsg('  ch('//str(jCh,dgt_nCh)//') fwrd: '//str(ch%upleng_fwrd)//&
            ' back: '//str(ch%upleng_back))
      endif
    enddo  ! jCh/
  endif
  if( any(nwk%channel(:)%upleng_fwrd == 0.d0 .and. nwk%channel(:)%upleng_back == 0.d0) )then
    call logmsg('Not passed: ')
    do jCh = 1, nwk%nCh
      ch => nwk%channel(jCh)
      if( ch%upleng_fwrd == 0.d0 .and. ch%upleng_back == 0.d0 )then
        call logmsg('  ch('//str(jCh,dgt_nCh)//') fwrd: '//str(ch%upleng_fwrd)//&
            ' back: '//str(ch%upleng_back))
      endif
    enddo  ! jCh/
  endif

!DEBUG
open(newunit=un, file='tmp/node_upleng.txt', status='replace')
do iNode = 1, nwk%nNode
  write(un,*) nwk%node(iNode)%upleng
enddo
close(un)

!DEBUG
open(newunit=un, file='tmp/chdir1.txt', status='replace')
do jCh = 1, nwk%nCh
  ch => nwk%channel(jCh)
  write(un,*) ch%dir
enddo
close(un)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
if( .false. )then
  nNode_src = count(nwk%node(:)%typ == NODETYPE__SRC)
  allocate(point(nNode_src))
  allocate(lst_iNode(nNode_src))

  iiNode = 0
  do iNode = 1, nwk%nNode
    if( nwk%node(iNode)%typ /= NODETYPE__SRC ) cycle
    call add(iiNode)
    lst_iNode(iiNode) = iNode
    p => point(iiNode)
    p%iNode_now = iNode
    p%dist_now = 0.d0
    p%dist_tot = 0.d0
  enddo  ! iNode/

  open(newunit=un, file='tmp/point.txt', status='replace')

  is_updated = .true.
  do while( is_updated )

    is_updated = .false.
    do iiNode = 1, nNode_src
      p => point(iiNode)
      if( p%iNode_now == 0 ) cycle
      is_updated = .true.

      nwknode => nwk%node(p%iNode_now)
      ch => nwk%channel(nwknode%jCh(nwknode%iCh_down))
      call add(p%dist_now, DIST_INTVL)
      call add(p%dist_tot, DIST_INTVL)
      do while( p%dist_now > ch%leng )
        call add(p%dist_now, -ch%leng)
        p%iNode_now = nwknode%iNode(nwknode%iCh_down)
        nwknode => nwk%node(p%iNode_now)
        if( nwknode%typ == NODETYPE__OUT )then
          call add(p%dist_tot, -p%dist_now)
          p%iNode_now = 0
          exit
        endif
        ch => nwk%channel(nwknode%jCh(nwknode%iCh_down))
      enddo  ! while p%dist_now > ch%leng/

      if( p%iNode_now == 0 ) cycle

      if( nwknode%jNode(nwknode%iCh_down) == 1 )then
        jPt_bgn = 1
        jPt_end = ch%n - 1
        jPt_int = 1
      else
        jPt_bgn = ch%n
        jPt_end = 2
        jPt_int = -1
      endif

      dist = 0.d0
      do jPt = jPt_bgn, jPt_end, jPt_int
        jPt_next = jPt + jPt_int
        dist_add = dist_sphere(&
            ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, &
            ch%lon(jPt_next)*d2r, ch%lat(jPt_next)*d2r) * EARTH_R
        if( dist + dist_add > p%dist_now )then
          w = (p%dist_now - dist) / dist_add
          p%lon = ch%lon(jPt) + (ch%lon(jPt_next) - ch%lon(jPt))*w
          p%lat = ch%lat(jPt) + (ch%lat(jPt_next) - ch%lat(jPt))*w
          exit
        endif
        call add(dist, dist_add)
      enddo
      if( jPt == ch%n )then
        p%lon = ch%lon(jPt)
        p%lat = ch%lat(jPt)
      endif
    enddo  ! iiNode/

    write(un,"(1x,a,"//str(nNode_src)//"(1x,es20.13))") 'lon', point(:)%lon
    write(un,"(1x,a,"//str(nNode_src)//"(1x,es20.13))") 'lat', point(:)%lat
  enddo  ! while is_updated/

  close(un)
endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
end subroutine modify_channeldir
!===============================================================
!
!===============================================================
real(8) function get_theta(theta_in, forward) result(theta)
  implicit none
  real(8), intent(in) :: theta_in
  logical, intent(in) :: forward

  if( forward )then
    theta = theta_in
  else
    theta = theta_in + 180.d0
    if( theta > 180.d0 ) theta = theta - 360.d0
  endif
end function get_theta
!===============================================================
!
!===============================================================
end module mod_modify_channeldir
