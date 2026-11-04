module mod_topo
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_util, only: &
    sBBox  , &
    slonlat
  use c2_strnk_const, only: &
    DGT_NWKID
  use c2_strnk_type, only: &
    cmn_node_, &
    cmn_channel_, &
    cmn_watsys_, &
    cmn_nwknode_, &
    cmn_network_
  use c3_joint_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: modifyElv
  public :: modifyFlwDir
  public :: calcUpperArea
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  type :: node_
    real(8) :: lon, lat
    integer :: typ
    integer :: iNode  ! index of nwknode
    real(8) :: downleng
    real(8) :: elv
    logical :: is_outlet
  end type

  type isct_
    integer :: n
    integer, pointer :: gx(:), gy(:)
    real(8), pointer :: leng(:)
    integer :: gxs, gxe, gys, gye
  end type

  type :: channel_
    integer :: n
    real(8), pointer :: lon(:), lat(:)
    type(node_), pointer :: node(:)
    real(8) :: west, east, south, north
    real(8) :: leng
    type(isct_) :: isct
    logical :: is_isolated
  end type

  type :: nwknode_
    real(8) :: lon, lat
    integer :: nCh
    integer, pointer :: jCh(:)
    integer, pointer :: jNode(:)
    integer, pointer :: iNode(:)  ! opposite side nwknode
    integer :: typ
    real(8) :: downleng
    integer :: iNode_to_outlet
    integer :: jjCh_to_outlet
  end type

  type :: network_
    character(DGT_NWKID) :: id
    integer :: nCh
    type(channel_), pointer :: channel(:)
    integer :: nNode
    type(nwknode_), pointer :: node(:)
    integer :: nx, ny
    integer :: gxs, gxe, gys, gye
    real(8) :: west, east, south, north
    integer, pointer :: jCh(:)
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_topo'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine modifyElv(networkType, resl, id)
  use c3_strnk_io, only: &
    strnk_get_f_lst_networks_channel
  use c3_jflw_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modifyElv'
  character(*), intent(in) :: networkType
  character(*), intent(in) :: resl
  character(*), intent(in) :: id

  character(DGT_NWKID) :: id_
  integer :: nBasin, iBasin

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call jflw_set_resolution(resl)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( networkType )
  !-------------------------------------------------------------
  ! Case: network
  case( NETWORKTYPE__NETWORK )

    if( id == '' )then
      f = strnk_get_f_lst_networks_channel()
      call logmsg('Reading '//str(f))
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nBasin
      read(un,*)
      do iBasin = 1, nBasin
        read(un,*) c_, id_
        call modify_elv_network(resl, id_)
      enddo  ! iBasin/
      close(un)
    else
      call modify_elv_network(resl, id)
    endif
  !-------------------------------------------------------------
  ! Case: networkset
  case( NETWORKTYPE__NETWORKSET )
    call errend(msg_not_implemented()//&
      '\nnetworkType: '//str(networkType))

  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('networkType', networkType))

  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine modifyElv
!===============================================================
!
!===============================================================
subroutine modify_elv_network(resl, id)
  use c2_strnk_io, only: &
    read_network_mesh_domain, &
    get_f_network_mesh
  use c2_jflw_const, only: &
    ELV_MISS
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_elv_network'
  character(*), intent(in) :: resl
  character(*), intent(in) :: id

  real(4), allocatable :: elv(:,:)
  real(4), allocatable :: upa(:,:)
  real(4), allocatable :: elv_mod(:,:)
  real(4) :: elv_neighbor
  integer :: n_neighbor
  integer :: nx, ny, ix, iy, ibx, iby
  integer :: gxs, gxe, gys, gye
  real(8) :: west, east, south, north
  character(CLEN_PATH) :: f

  real(4), parameter :: UPA_RIV_THRESH = 1e6  ! [m2]

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Load topography
  !-------------------------------------------------------------
  call logent('Loading topography data')

  call read_network_mesh_domain(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  allocate(elv(nx,ny))
  allocate(upa(nx,ny))

  f = get_f_network_mesh(resl, 'elv', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(elv, f) )

  f = get_f_network_mesh(resl, 'upa', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(upa, f) )

  call logext()
  !-------------------------------------------------------------
  ! Modify elevation
  !-------------------------------------------------------------
  call logent('Modifying elevation')

  allocate(elv_mod(nx,ny))
  elv_mod(:,:) = elv(:,:)

  do iy = 1, ny
  do ix = 1, nx
    if( upa(ix,iy) < UPA_RIV_THRESH ) cycle

    n_neighbor = 0
    elv_neighbor = 0.0
    do iby = max(iy-1,1), min(iy+1,ny)
    do ibx = max(ix-1,1), min(ix+1,nx)
      if( upa(ibx,iby) >= UPA_RIV_THRESH ) cycle
      if( elv(ibx,iby) == ELV_MISS ) cycle

      call add(elv_neighbor, elv(ibx,iby))
      call add(n_neighbor)
    enddo  ! ibx/
    enddo  ! iby/

    if( n_neighbor == 0 )then
      call logwrn('Elevation was not updated because non-river grid was not found around.')
      cycle
    endif

    elv_mod(ix,iy) = elv_neighbor / n_neighbor
  enddo  ! ix/
  enddo  ! iy/

  f = get_f_network_mesh(resl, 'elv_mod', id)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(elv_mod, f) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine modify_elv_network
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
subroutine modifyFlwDir(networkType, resl, id)
  use c3_strnk_io, only: &
    strnk_get_f_lst_networks_channel
  use c3_jflw_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modifyFlwDir'
  character(*), intent(in) :: networkType
  character(*), intent(in) :: resl
  character(*), intent(in) :: id

  character(DGT_NWKID) :: id_
  integer :: nBasin, iBasin

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call jflw_set_resolution(resl)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( networkType )
  !-------------------------------------------------------------
  ! Case: network
  case( NETWORKTYPE__NETWORK )

    if( id == '' )then
      f = strnk_get_f_lst_networks_channel()
      call logmsg('Reading '//str(f))
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nBasin
      read(un,*)
      do iBasin = 1, nBasin
        read(un,*) c_, id_
        call modify_flwdir_network(resl, id_)
      enddo  ! iBasin/
      close(un)
    else
      call modify_flwdir_network(resl, id)
    endif
  !-------------------------------------------------------------
  ! Case: networkset
  case( NETWORKTYPE__NETWORKSET )
    call errend(msg_not_implemented()//&
      '\nnetworkType: '//str(networkType))

  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('networkType', networkType))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine modifyFlwDir
!===============================================================
!
!===============================================================
subroutine modify_flwdir_network(resl, id)
  use c2_jflw_const
  use c2_jflw_grid, only: &
    center_of_gx , &
    center_of_gy , &
    north_of_gy  , &
    south_of_gy  , &
    get_nextxy   , &
    get_fdr      , &
    get_angle_fdr
  use c2_strnk_const
  use c2_strnk_io, only: &
    read_network            , &
    read_network_mesh_domain, &
    get_f_network_channel   , &
    get_f_network_meshisct  , &
    get_f_network_mesh    
  use c2_strnk_util, only: &
    clear_cmn_network
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_flwdir_network'
  character(*), intent(in) :: resl
  character(*), intent(in) :: id

  type(network_) :: nwk
  type(channel_), pointer :: ch
  type(node_), pointer :: node, node2
  type(isct_), pointer :: ci
  type(nwknode_), pointer :: nwknode
  type(cmn_network_) :: cmnnwk
  integer(1), allocatable :: isc(:,:)  !(xs:xe,ys:ye)
  integer(1), allocatable :: msk(:,:)  !(xs:xe,ys:ye)
  integer(1), allocatable :: fdr(:,:)
  integer(1), allocatable :: reach(:,:)
  logical(1), allocatable :: passed(:,:)
  integer, allocatable :: lst_iNode_src(:)  !(nNode_src)
  real(8), allocatable :: lst_dist_src(:)
  real(8), allocatable :: mesh_size(:)  !(gys:gye)
  integer, pointer :: lst_gx(:), lst_gy(:)  !(nMesh)
  real(8), pointer :: lst_leng(:)
  logical, pointer :: lst_counted(:)
  integer :: nNode_src
  integer :: nCh_isolated
  integer :: jCh, jjCh
  integer :: is, ie, iis, iie
  integer :: jNode
  integer :: iNode, iiNode
  integer :: mNode_up, mNode_down
  real(8) :: downleng_min
  integer :: chdir
  integer :: nMesh, iMesh, jMesh, kMesh
  logical :: is_updated
  logical :: is_ok
  integer :: igx, igy, jgx, jgy, kgx, kgy, gx, gy, gxx, gyy
  integer :: gx_next, gy_next
  integer :: n, i
  real(8) :: angle_in, angle_target
  real(8) :: angle_join, angle_join_min
  real(8), pointer :: lst_lon(:), lst_lat(:)
  integer, pointer :: lst_jCh(:), lst_jNode(:)
  integer, pointer :: arg(:)

  integer :: iter
  real(8) :: leng_mesh

  character(CLEN_PATH) :: f
  integer :: un

  real(8), parameter :: THRESH_MESHSIZE_FRAC = 1.d0
  integer, parameter :: THRESH_ITER = 10000

  integer(1), parameter :: ISCT__NO       = 0_1
  integer(1), parameter :: ISCT__NETWORK  = 1_1
  integer(1), parameter :: ISCT__ISOLATED = 2_1

  integer(1), parameter :: REACH__RIVERMOUTH = 1
  integer(1), parameter :: REACH__INLAND     = 2
  integer(1), parameter :: REACH__OCEAN      = 3
  integer(1), parameter :: REACH__SOURCE     = -1
  integer(1), parameter :: REACH__MISS       = -2
  integer(1), parameter :: REACH__UNDEF      = -99

  logical :: debug_this = .false.

  call logbgn(PRCNAM, MODNAM)
  call logmsg('Network '//str(id))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(lst_gx, lst_gy)
  nullify(lst_leng)
  nullify(lst_counted)
  nullify(lst_lon, lst_lat)
  nullify(lst_jCh, lst_jNode)
  nullify(arg)
  !-------------------------------------------------------------
  ! Read input data
  !-------------------------------------------------------------
  call logent('Loading input data')

  ! Channels
  f = get_f_network_channel(id, 'sbin')
  call logmsg('Reading '//str(f))

  allocate(character(1) :: cmnnwk%id)
  cmnnwk%id = trim(id)
  call read_network(f, cmnnwk)

  call copy_cmnnwk(cmnnwk, nwk)

  call clear_cmn_network(cmnnwk)

  ! Mesh domain
  call read_network_mesh_domain(&
    resl, id, &
    nwk%nx, nwk%ny, nwk%gxs, nwk%gxe, nwk%gys, nwk%gye, &
    nwk%west, nwk%east, nwk%south, nwk%north, &
    echo=.true.)

  call logmsg('Mesh: ('//str((/nwk%nx,nwk%ny/),',')//') '//&
                    '['//str((/nwk%gxs,nwk%gxe/),DGT_GXY,':')//&
                    ','//str((/nwk%gys,nwk%gye/),DGT_GXY,':')//']')

  ! Intersections
  f = get_f_network_meshisct(MESHTYPE__JFLW, resl, id)
  call logmsg('Reading '//str(f))

  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')
  read(un)  ! nCh
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    ci => ch%isct

    read(un) ci%n
    allocate(ci%gx(ci%n))
    allocate(ci%gy(ci%n))
    allocate(ci%leng(ci%n))
    read(un) ci%gx(:)
    read(un) ci%gy(:)
    read(un) ci%leng(:)

    ci%gxs = minval(ci%gx)
    ci%gxe = maxval(ci%gx)
    ci%gys = minval(ci%gy)
    ci%gye = maxval(ci%gy)
  enddo  ! jCh
  close(un)

  if( nwk%gxs > minval(nwk%channel(:)%isct%gxs) .or. &
      nwk%gxe < maxval(nwk%channel(:)%isct%gxe) .or. &
      nwk%gys > minval(nwk%channel(:)%isct%gys) .or. &
      nwk%gye < maxval(nwk%channel(:)%isct%gye) )then
    call errend('Inconsistency between domain and intersection.'//&
      '\nintersection: ['//&
      str((/minval(nwk%channel(:)%isct%gxs),maxval(nwk%channel(:)%isct%gxe)/),DGT_GXY,':')//','//&
      str((/minval(nwk%channel(:)%isct%gys),maxval(nwk%channel(:)%isct%gye)/),DGT_GXY,':')//']')
  endif

  ! Mesh data
  allocate(fdr(nwk%gxs:nwk%gxe,nwk%gys:nwk%gye))

  f = get_f_network_mesh(resl, 'dir', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(fdr, f, dtype=DTYPE_INT4) )

  call logext()
  !-------------------------------------------------------------
  ! Make node list
  !-------------------------------------------------------------
  call logent('Making node list')

  nwk%nNode = nwk%nCh * 2
  allocate(nwk%node(nwk%nNode))

  call realloc(lst_lon, nwk%nNode)
  call realloc(lst_lat, nwk%nNode)
  call realloc(lst_jCh, nwk%nNode)
  call realloc(lst_jNode, nwk%nNode)

  iNode = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    do jNode = 1, 2
      node => ch%node(jNode)
      call add(iNode)

      lst_lon(iNode) = node%lon
      lst_lat(iNode) = node%lat
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

      nwknode%nCh = iie - iis + 1

      allocate(nwknode%jCh(nwknode%nCh))
      allocate(nwknode%jNode(nwknode%nCh))
      nwknode%jCh(:)   = lst_jCh(iis:iie)
      nwknode%jNode(:) = lst_jNode(iis:iie)

      node => nwk%channel(lst_jCh(iis))%node(lst_jNode(iis))
      
      nwknode%typ = node%typ
      nwknode%downleng = node%downleng

      do jjCh = iis, iie
        node => nwk%channel(lst_jCh(jjCh))%node(lst_jNode(jjCh))
        node%iNode = iNode
      enddo
    enddo  ! iis, iie/
  enddo  ! is, ie/

  nwk%nNode = iNode
  call logmsg('Nodes: '//str(nwk%nNode))

  deallocate(arg)
  call realloc(lst_lon, 0)
  call realloc(lst_lat, 0)
  call realloc(lst_jCh, 0)
  call realloc(lst_jNode, 0)

  ! Info. of opposite side node
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    allocate(nwknode%iNode(nwknode%nCh))
    nwknode%iNode_to_outlet = 0
    downleng_min = EARTH_R
    is_ok = .true.
    do jjCh = 1, nwknode%nCh
      ch => nwk%channel(nwknode%jCh(jjCh))
      node => ch%node(3-nwknode%jNode(jjCh))
      nwknode%iNode(jjCh) = node%iNode
      if( node%downleng < downleng_min )then
        downleng_min = node%downleng
        nwknode%iNode_to_outlet = node%iNode
        nwknode%jjCh_to_outlet = jjCh
      elseif( node%downleng == downleng_min )then
        if( node%iNode /= nwknode%iNode_to_outlet )then
          if( ch%leng < nwk%channel(nwknode%jjCh_to_outlet)%leng )then
            nwknode%iNode_to_outlet = node%iNode
            nwknode%jjCh_to_outlet = jjCh
          elseif( ch%leng == nwk%channel(nwknode%jjCh_to_outlet)%leng )then
            is_ok = .false.
          endif
        endif
      endif
    enddo  ! jjCh/

    if( .not. is_ok )then
      call logmsg('node#'//str(iNode)//' '//slonlat(nwknode%lon, nwknode%lat)//&
        ' iNode_to_outlet: '//str(nwknode%iNode_to_outlet)//&
        ' downleng: '//str(nwknode%downleng)//&
        ' downleng_min: '//str(downleng_min))
      do jjCh = 1, nwknode%nCh
        node => nwk%channel(nwknode%jCh(jjCh))%node(3-nwknode%jNode(jjCh))
        call logmsg('node#'//str(node%iNode,dgt(nwk%nNode))//&
         ' '//slonlat(node%lon, node%lat)//' downleng: '//str(node%downleng))
      enddo
      call logmsg('Same distance to outlet and same channel length: not implemented yet')
      stop
    endif
  enddo  ! iNode/

  call logext()
  !-------------------------------------------------------------
  ! Find source / outlet nodes
  !-------------------------------------------------------------
  call logent('Searching for source / outlet nodes')

  nNode_src = 0
  allocate(lst_iNode_src(nwk%nNode))
  allocate(lst_dist_src(nwk%nNode))

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    mNode_up = 0
    mNode_down = 0
    do jjCh = 1, nwknode%nCh
      jCh = nwknode%jCh(jjCh)
      jNode = nwknode%jNode(jjCh)
      ch => nwk%channel(jCh)
      node => ch%node(jNode)
      node2 => ch%node(3-jNode)
      if( node%downleng > node2%downleng )then
        call add(mNode_up)
      else
        call add(mNode_down)
      endif
    enddo  ! jjCh/

    ! source
    if( mNode_up > 0 .and. mNode_down == 0 )then
      call add(nNode_src)
      lst_iNode_src(nNode_src) = iNode
      lst_dist_src(nNode_src) = nwknode%downleng

    ! outlet
    elseif( mNode_up == 0 .and. mNode_down > 0 )then
      continue

    ! other
    else
      continue

    endif
  enddo  ! iNode/

  call logmsg('Sources: '//str(nNode_src))

  allocate(arg(nNode_src))
  call argsort(lst_dist_src(:nNode_src), arg)
  call sort(lst_iNode_src(:nNode_src), arg)
  call sort(lst_dist_src(:nNode_src), arg)
  if( .not. all(lst_dist_src(2:nNode_src) >= lst_dist_src(:nNode_src-1)) )then
    call errend('Sorting failed')
  endif
  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  ! Find channels not on the route from source to outlet
  !-------------------------------------------------------------
  call logent('Searching for channels not on the route from source to outlet')

  nwk%channel(:)%is_isolated = .true.

  do iiNode = 1, nNode_src
    iNode = lst_iNode_src(iiNode)
    nwknode => nwk%node(iNode)
    do while( nwknode%downleng > 0.d0 )
      jjCh = nwknode%jjCh_to_outlet
      jCh = nwknode%jCh(jjCh)
      jNode = nwknode%jNode(jjCh)

      ch => nwk%channel(jCh)
      ch%is_isolated = .false.

      iNode = nwknode%iNode_to_outlet
      nwknode => nwk%node(nwknode%iNode_to_outlet)
    enddo  ! while nwknode%downleng > 0
  enddo  ! iiNode

  nCh_isolated = count(nwk%channel(:)%is_isolated)
  call logmsg('Isolated channels: '//str(nCh_isolated))

  call logext()
  !-------------------------------------------------------------
  ! Calc. mesh size
  !-------------------------------------------------------------
  call logent('Calculating mesh size')

  allocate(mesh_size(nwk%gys:nwk%gye))

  do igy = nwk%gys, nwk%gye
    ! length in longit: dlon * R*cos(lat)
    !           latit : dlat * R
    mesh_size(igy) = sqrt(&
      (GRIDSIZE_LON*d2r * cos(center_of_gy(igy)*d2r)) &
      * (GRIDSIZE_LAT*d2r) &
    ) * EARTH_R
    !mesh_size(igy) = area_sphere_rect(north_of_gy(igy)*d2r, south_of_gy(igy)*d2r)&
    !  * GRIDSIZE_LON*d2r * EARTH_R**2
  enddo  ! iy/
  ! Diameter of the circle with the same area to the mesh
  !mesh_size(:) = sqrt(mesh_size(:) / pi) * 2.d0
  call logmsg('Mesh size: '//str(sum(mesh_size)/(nwk%gye-nwk%gys+1))//' [m]')

  call logext()
  !-------------------------------------------------------------
  ! Make mesh line from source to outlet
  !-------------------------------------------------------------
  call logent('Making mesh line from source to outlet')

  nMesh = (nwk%gxe-nwk%gxs+1) + (nwk%gye-nwk%gys+1)
  allocate(lst_gx(nMesh))
  allocate(lst_gy(nMesh))
  allocate(lst_leng(nMesh))
  allocate(lst_counted(nMesh))

  allocate(isc(nwk%gxs:nwk%gxe,nwk%gys:nwk%gye))
  isc(:,:) = ISCT__NO

  do iiNode = 1, nNode_src
    iNode = lst_iNode_src(iiNode)
    nwknode => nwk%node(iNode)
    !call logmsg('src node#'//str(iNode,dgt(nwk%nNode))//' dist '//str(nwknode%downleng))

    !debug_this = iNode == 10658
    !-----------------------------------------------------------
    ! Rasterize the channels
    !-----------------------------------------------------------
    nMesh = 0
    iter = 0
    do while( nwknode%downleng > 0.d0 )
      call add(iter)
      if( iter > THRESH_ITER )then
        call errend('iter > thresh')
      endif

      jjCh = nwknode%jjCh_to_outlet
      jCh = nwknode%jCh(jjCh)
      jNode = nwknode%jNode(jjCh)

      ch => nwk%channel(jCh)
      ci => ch%isct

      if( jNode == 1 )then
        chdir = 1
        is = 1
        ie = ci%n
      else
        chdir = -1
        is = ci%n
        ie = 1
      endif

      if( debug_this )then
        call logmsg('ch#'//str(jCh,dgt(nwk%nCh))//' dir '//str(chdir,2)//&
          ' '//slonlat(ch%node(jNode)%lon,ch%node(jNode)%lat)//&
          ' - '//str(slonlat(ch%node(3-jNode)%lon,ch%node(3-jNode)%lat))//&
          ' dist: '//str(nwk%node(ch%node(3-jNode)%iNode)%downleng)//&
          '\n['//str((/ci%gx(1),ci%gy(1)/),DGT_GXY,',')//'] - ['//&
          str((/ci%gx(ci%n),ci%gy(ci%n)/),DGT_GXY,',')//']'//&
          ' n: '//str(ci%n))
      endif

      call update_list_isct()

      iNode = nwknode%iNode_to_outlet
      nwknode => nwk%node(nwknode%iNode_to_outlet)
      !call logmsg('  - node#'//str(iNode,dgt(nwk%nNode))//' dist '//str(nwknode%downleng))
    enddo  ! while nwknode%downleng > 0

    call make_continuous_mesh_line()
    !-----------------------------------------------------------
    ! Modify flwdir
    !-----------------------------------------------------------
    do iMesh = 1, nMesh-1
!if( (lst_gx(iMesh) == 63058 .and. lst_gy(iMesh) == 35215) .or. &
!    (lst_gx(iMesh) == 63059 .and. lst_gy(iMesh) == 35215) )then
!  call logmsg('fdr('//str((/lst_gx(iMesh),lst_gy(iMesh)/),DGT_GXY,',')//&
!    ') '//str(fdr(lst_gx(iMesh),lst_gy(iMesh)))//' -> '//&
!    str(get_fdr(lst_gx(iMesh), lst_gy(iMesh), lst_gx(iMesh+1), lst_gy(iMesh+1))))
!endif
      fdr(lst_gx(iMesh),lst_gy(iMesh)) &
      = get_fdr(lst_gx(iMesh), lst_gy(iMesh), lst_gx(iMesh+1), lst_gy(iMesh+1))
    enddo

    gx = lst_gx(nMesh)
    gy = lst_gy(nMesh)
    if( fdr(gx,gy) /= FDR_RIVERMOUTH )then
      call logmsg('fdr('//str((/gx,gy/),',')//') -> river mouth')
      fdr(gx,gy) = FDR_RIVERMOUTH
    endif
    !-----------------------------------------------------------
    ! Calc. length
    !-----------------------------------------------------------
    leng_mesh = 0.d0
    do jMesh = 1, nMesh
      call add(leng_mesh, mesh_size(lst_gy(iMesh)))
    enddo  ! jMesh/

    !call logmsg('leng channel: '//str(nwk%node(lst_iNode_src(iiNode))%downleng)//&
    !            ' isct: '//str(sum(lst_leng(:nMesh)))//&
    !            ' mesh: '//str(leng_mesh))

    do iMesh = 1, nMesh
      isc(lst_gx(iMesh),lst_gy(iMesh)) = ISCT__NETWORK
    enddo
    !-----------------------------------------------------------
  enddo  ! iiNode

  deallocate(lst_iNode_src)
  deallocate(lst_dist_src)

  call logext()
  !-------------------------------------------------------------
  ! Isolated channels
  !-------------------------------------------------------------
  call logent('Merging intersections of isolated channels')

  ! Sort channels in the descending order of the length
  n = 0
  call realloc(lst_jCh, nwk%nCh)
  call realloc(lst_leng, nwk%nCh)
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( .not. ch%is_isolated ) cycle
    call add(n)
    lst_jCh(n) = jCh
    lst_leng(n) = ch%leng
  enddo  ! jCh
  if( n > 0 )then
    allocate(arg(n))
    call argsort(lst_leng(:n), arg)
    call sort(lst_jCh(:n), arg)
    call reverse(lst_jCh(:n))
    deallocate(arg)
  endif

  call realloc(lst_leng, nMesh)

  do jjCh = 1, n
    jCh = lst_jCh(jjCh)
    ch => nwk%channel(jCh)
    !call logmsg('ch#'//str(jCh)//' leng '//str(ch%leng))
    ci => ch%isct
    !-----------------------------------------------------------
    ! Rasterize the channel
    !-----------------------------------------------------------
    if( ch%node(1)%downleng > ch%node(2)%downleng )then
      is = 1
      ie = ci%n
      chdir = 1
    else
      is = ci%n
      ie = 1
      chdir = -1
    endif

    nMesh = 0
    call update_list_isct()

    call make_continuous_mesh_line()
    !-----------------------------------------------------------
    ! Modify flwdir
    !-----------------------------------------------------------
    is_ok = .false.
    do iMesh = 2, nMesh
      if( isc(lst_gx(iMesh), lst_gy(iMesh)) == ISCT__NETWORK )then
        is_ok = .true.
        exit
      endif
    enddo
    if( .not. is_ok ) cycle

    do iMesh = 1, nMesh-1
      !if( isc(lst_gx(iMesh), lst_gy(iMesh)) == ISCT__NETWORK )then
      if( isc(lst_gx(iMesh), lst_gy(iMesh)) > 0_1 )then
        lst_gx(iMesh) = 0
      else
!if( (lst_gx(iMesh) == 63058 .and. lst_gy(iMesh) == 35215) .or. &
!    (lst_gx(iMesh) == 63059 .and. lst_gy(iMesh) == 35215) )then
!  call logmsg('fdr('//str((/lst_gx(iMesh),lst_gy(iMesh)/),DGT_GXY,',')//&
!    ') '//str(fdr(lst_gx(iMesh),lst_gy(iMesh)))//' -> '//&
!    str(get_fdr(lst_gx(iMesh), lst_gy(iMesh), lst_gx(iMesh+1), lst_gy(iMesh+1))))
!endif
        fdr(lst_gx(iMesh),lst_gy(iMesh)) &
        = get_fdr(lst_gx(iMesh), lst_gy(iMesh), lst_gx(iMesh+1), lst_gy(iMesh+1))
      endif
    enddo

    iMesh = 0
    do jMesh = 1, nMesh
      if( lst_gx(jMesh) == 0 ) cycle
      call add(iMesh)
      lst_gx(iMesh) = lst_gx(jMesh)
      lst_gy(iMesh) = lst_gy(jMesh)
      lst_leng(iMesh) = lst_leng(jMesh)
    enddo  ! jMesh/
    nMesh = iMesh

    do iMesh = 1, nMesh
      isc(lst_gx(iMesh),lst_gy(iMesh)) = ISCT__ISOLATED
    enddo
  enddo  ! jCh/

  call realloc(lst_jCh, 0)
  call realloc(lst_leng, 0)

  deallocate(mesh_size)

  call logext()
  !-------------------------------------------------------------
  ! Make a mask of grids that reach ocean
  !-------------------------------------------------------------
  call logent('Making a mask of grids that reach ocean')

  allocate(msk(nwk%gxs:nwk%gxe,nwk%gys:nwk%gye))
  allocate(reach(nwk%gxs:nwk%gxe,nwk%gys:nwk%gye))

  f = get_f_network_mesh(resl, 'mask', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(msk, f) )

  ! Find source grids
  !-------------------------------------------------------------
  call logent('Finding source grids')

  reach(:,:) = REACH__UNDEF
  do igy = nwk%gys, nwk%gye
  do igx = nwk%gxs, nwk%gxe
    if( msk(igx,igy) <= 0_1 .and. isc(igx,igy) > 0_1 )then
      call errend('msk <= 0 but isc > 0')
    endif

    if( msk(igx,igy) <= 0_1 ) cycle

    if( fdr(igx,igy) <= 0_1 ) cycle
    call get_nextxy(igx, igy, fdr(igx,igy), gxx, gyy)

    reach(gxx,gyy) = REACH__SOURCE
  enddo  ! igx
  enddo  ! igy

  call logext()

  ! Mask grids that does not reach oceans
  !-------------------------------------------------------------
  call logent('Making a mask')

  do igy = nwk%gys, nwk%gye
  do igx = nwk%gxs, nwk%gxe
    if( fdr(igx,igy) == FDR_RIVERMOUTH )then
      reach(igx,igy) = REACH__RIVERMOUTH
    elseif( fdr(igx,igy) == FDR_INLAND )then
      reach(igx,igy) = REACH__INLAND
    elseif( reach(igx,igy) == REACH__SOURCE )then
      continue
    elseif( msk(igx,igy) <= 0_1 )then
      reach(igx,igy) = REACH__MISS
    else
      reach(igx,igy) = REACH__UNDEF
    endif
  enddo  ! igx
  enddo  ! igy

  allocate(passed(nwk%gxs:nwk%gxe,nwk%gys:nwk%gye))
  passed = .false.

  n = nwk%nx + nwk%ny
  call realloc(lst_gx, n, clear=.true.)
  call realloc(lst_gy, n, clear=.true.)

  do igy = nwk%gys, nwk%gye
  do igx = nwk%gxs, nwk%gxe
    if( reach(igx,igy) == REACH__SOURCE .and. fdr(igx,igy) == FDR_MISS )then
      !call logmsg('ocean '//slonlat(center_of_gx(igx),center_of_gy(igy)))
      reach(igx,igy) = REACH__RIVERMOUTH
    endif
  enddo
  enddo

  do igy = nwk%gys, nwk%gye
  do igx = nwk%gxs, nwk%gxe
    if( reach(igx,igy) /= REACH__SOURCE ) cycle

    !call logmsg('source('//str((/igx,igy/),DGT_GXY,',')//')')
    !-----------------------------------------------------------
    ! Judge the type of the destination
    !-----------------------------------------------------------
    call logent('Judging the type of the destination', opt='-p')

    gx = igx
    gy = igy
    is_ok = .true.
    n = 1
    lst_gx(n) = gx
    lst_gy(n) = gy
    do
      passed(gx,gy) = .true.

      call get_nextxy(gx, gy, fdr(gx,gy), gxx, gyy)
      gx = gxx
      gy = gyy

      if( n == size(lst_gx) )then
        call realloc(lst_gx, n*2, clear=.false.)
        call realloc(lst_gy, n*2, clear=.false.)
      endif
      call add(n)
      lst_gx(n) = gx
      lst_gy(n) = gy

      if( passed(gx,gy) )then
        is_ok = .false.
        exit
      endif
      !---------------------------------------------------------
      ! Case: River mouth
      if( reach(gx,gy) == REACH__RIVERMOUTH )then
        reach(igx,igy) = REACH__RIVERMOUTH
        exit
      !---------------------------------------------------------
      ! Case: Inland outlet
      elseif( reach(gx,gy) == REACH__INLAND )then
        reach(igx,igy) = REACH__INLAND
        exit
      !---------------------------------------------------------
      ! Case: Directly runoff to the ocean
      elseif( msk(gx,gy) <= 0_1 )then
        reach(igx,igy) = REACH__OCEAN
        reach(gx,gy) = REACH__OCEAN
        exit
      !---------------------------------------------------------
      ! Case: msk > 0 and fdr == miss
      !   Modified to river mouth later
      elseif( fdr(gx,gy) == FDR_MISS )then
        reach(igx,igy) = REACH__RIVERMOUTH
        reach(gx,gy) = REACH__RIVERMOUTH
        exit
      endif

      selectcase( fdr(gx,gy) )
      case( 1_1: )
        continue
      case( FDR_RIVERMOUTH, &
            FDR_INLAND, &
            FDR_MISS, &
            FDR_UNDEF )
        call errend(msg_unexpected_condition()//&
          '\nfdr == '//str(fdr(gx,gy)))
      case default
        call errend(msg_invalid_value('fdr', fdr(gx,gy)))
      endselect
    enddo

    do i = 1, n
      passed(lst_gx(i), lst_gy(i)) = .false.
    enddo

    if( .not. is_ok )then
      call erradd('Flow direction is cycling. source: ('//str((/igx,igy/),',')//')')
      open(newunit=un, file='tmp/passed.txt', status='replace')
      do i = 1, n
        gx = lst_gx(i)
        gy = lst_gy(i)
        write(un,"(a)") str((/gx,gy/),DGT_GXY)//&
          ' '//str((/center_of_gx(gx),center_of_gy(gy)/),'es20.13')//&
          ' '//str(fdr(gx,gy),2)
        if( passed(gx,gy) ) exit
        passed(gx,gy) = .true.
      enddo
      close(un)
      call errend()
    endif

    call logext()
    !-----------------------------------------------------------
    ! Update the mask
    !-----------------------------------------------------------
    call logent('Updating the mask', opt='-p')

    selectcase( reach(igx,igy) )
    case( REACH__RIVERMOUTH, &
          REACH__INLAND, &
          REACH__OCEAN )
      gx = igx
      gy = igy
      do
        call get_nextxy(gx, gy, fdr(gx,gy), gxx, gyy)
        gx = gxx
        gy = gyy
        if( reach(gx,gy) > 0_1 ) exit
        reach(gx,gy) = reach(igx,igy)
      enddo
    case( REACH__SOURCE, &
          REACH__UNDEF )
      call errend(msg_unexpected_condition()//&
        '\n  reach == '//str(reach(gx,gy)))
    case default
      call errend(msg_invalid_value('reach', reach(gx,gy)))
    endselect

    call logext()
    !-----------------------------------------------------------
  enddo  ! igx
  enddo  ! igy

  deallocate(passed)
  call realloc(lst_gx, 0)
  call realloc(lst_gy, 0)

  call logext()
  !-------------------------------------------------------------

  call logext()
  !-------------------------------------------------------------
  ! Connect flwdir to the network that reaches outlet
  !-------------------------------------------------------------
  call logent('Connecting flow to the network that reaches outlet')

  do igy = nwk%gys, nwk%gye
  do igx = nwk%gxs, nwk%gxe
    if( msk(igx,igy) <= 0_1 ) cycle

    selectcase( fdr(igx,igy) )
    !-----------------------------------------------------------
    ! Case: Ocean
    case( FDR_MISS )
      call logmsg('Modifying: '//&
        '('//str((/igx,igy/),DGT_GXY,',')//')'//&
        ' msk '//str(msk(igx,igy),2)//' isc '//str(isc(igx,igy),1)//&
        ' -> ocean')
      call logmsg('  Set to river mouth')

      fdr(igx,igy) = FDR_RIVERMOUTH
    !-----------------------------------------------------------
    ! Case: Outlet
    case( FDR_RIVERMOUTH, &
          FDR_INLAND )
      cycle
    !-----------------------------------------------------------
    ! Case: Downstream exists
    case( 1_1: )
      call get_nextxy(igx, igy, fdr(igx,igy), gxx, gyy)

      if( msk(gxx,gyy) > 0_1 ) cycle

      call logmsg('Modifying: '//&
        '('//str((/igx,igy/),DGT_GXY,',')//')'//&
        ' msk '//str(msk(igx,igy),2)//' isc '//str(isc(igx,igy),1)//&
        ' -> ('//str((/gxx,gyy/),DGT_GXY,',')//')'//&
        ' msk '//str(msk(gxx,gyy),2)//' isc '//str(isc(gxx,gyy),1))
      call setlog('+x2')

      if( msk(igx,igy) == STAT_NWKMESH__OUT .neqv. msk(gxx,gyy) == -9_1 )then
        call errend('Inconsistency in the mask. `this` == out .neqv. `next` == ocean')
      endif

      angle_join_min = 360.d0
      do jgy = igy-1, igy+1
      do jgx = igx-1, igx+1
        if( jgx == igx .and. jgy == igy ) cycle

        if( reach(jgx,jgy) /= REACH__RIVERMOUTH ) cycle

        selectcase( fdr(jgx,jgy) )
        case( 1_1:, &
              FDR_RIVERMOUTH, &
              FDR_INLAND )

          if( fdr(jgx,jgy) > 0_1 )then
            call get_nextxy(jgx, jgy, fdr(jgx,jgy), gxx, gyy)

            if( gxx == igx .and. gyy == igy )then
              cycle
            elseif( gxx <= 0 )then
              call errend('Downstream is ocean although fdr > 0')
            endif
          endif

          ! Angle of flwdir from (igx,igy) to (jgx,jgy)
          angle_in = get_angle_fdr(get_fdr(igx, igy, jgx, jgy))

          ! Calc. angle of flwdir of the target grid (jgx,jgy)
          !   angle_target := (theta1 + theta2) / 2
          !   theta1 := mean angle of the inflow to (jgx,jgy)
          !   theta2 := angle of outflow from (jgx,jgy)
          n = 0
          angle_target = 0.d0
          do kgy = jgy-1, jgy+1
          do kgx = jgx-1, jgx+1
            if( fdr(kgx,kgy) <= 0_1 ) cycle
            call get_nextxy(kgx, kgy, fdr(kgx,kgy), gxx, gyy)
            if( gxx /= jgx .or. gyy /= jgy ) cycle
            call add(n)
            call add(angle_target, get_angle_fdr(fdr(kgx,kgy)))
          enddo
          enddo
          if( n == 0 ) cycle

          call div(angle_target, n)

          if( fdr(jgx,jgy) > 0_1 )then
            angle_target = (angle_target + get_angle_fdr(fdr(jgx,jgy))) / 2
          endif
        case( FDR_MISS )
          cycle
        case default
          call errend(msg_invalid_value('fdr', fdr(jgx,jgy)))
        endselect

        angle_join = londiff_deg(angle_in, angle_target)
        if( angle_join < angle_join_min )then
          angle_join_min = angle_join
          gx_next = jgx
          gy_next = jgy
        endif

        call logmsg('('//str((/jgx,jgy/),DGT_GXY,',')//') isc '//str(isc(jgx,jgy),1)//&
          ' angle in '//str(angle_in,'f6.2')//' target '//str(angle_target,'f6.2')//&
          ' join '//str(angle_join,'f6.2'))
      enddo  ! jgx
      enddo  ! jgy

      if( angle_join_min == 360.d0 )then
        call logwrn('Downstream grid was not obtained. Set to inland outlet.')
        fdr(igx,igy) = FDR_INLAND
      else
        fdr(igx,igy) = get_fdr(igx, igy, gx_next, gy_next)
      endif

      call setlog('-x2')
    !-----------------------------------------------------------
    ! Case: ERROR (Undef)
    case( FDR_UNDEF )
      call errend(msg_unexpected_condition()//&
        '\n fdr('//str((/igx,igy/),',')//') == undef')
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('fdr('//str((/igx,igy/),',')//')', fdr(igx,igy)))
    endselect
  enddo  ! igx
  enddo  ! igy

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  f = get_f_network_mesh(resl, 'dir_mod', id)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(fdr, f, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(isc)
  deallocate(msk)
  deallocate(fdr)
  deallocate(reach)

  call realloc(lst_gx, 0)
  call realloc(lst_gy, 0)
  call realloc(lst_leng, 0)
  call realloc(lst_counted, 0)
  call realloc(lst_lon, 0)
  call realloc(lst_lat, 0)
  call realloc(lst_jCh, 0)
  call realloc(lst_jNode, 0)
  call realloc(arg, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine update_list_isct()
  implicit none

  do iMesh = is, ie, chdir
    if( size(lst_gx) == nMesh )then
      call realloc(lst_gx, nMesh*2, clear=.false.)
      call realloc(lst_gy, nMesh*2, clear=.false.)
      call realloc(lst_leng, nMesh*2, clear=.false.)
    endif
    call add(nMesh)
    lst_gx(nMesh) = ci%gx(iMesh)
    lst_gy(nMesh) = ci%gy(iMesh)
    lst_leng(nMesh) = ci%leng(iMesh)
    if( nMesh > 1 )then
      if( abs(lst_gx(nMesh) - lst_gx(nMesh-1)) > 1 .or. &
          abs(lst_gy(nMesh) - lst_gy(nMesh-1)) > 1 )then
        call errend('Discontinuition was found in the intersection.'//&
          '\ncurrent: #'//str(nMesh-1)//&
            '('//str((/lst_gx(nMesh-1),lst_gy(nMesh-1)/),DGT_GXY,',')//')'//&
          '\nnext : #'//str(iMesh)//&
            '('//str((/ci%gx(iMesh),ci%gy(iMesh)/),DGT_GXY,',')//')'//&
          '\nchdir: '//str(chdir)//&
          '\nisct x: '//str(ci%gx,DGT_GXY)//&
          '\n     y: '//str(ci%gy,DGT_GXY))
      endif
    endif
  enddo  ! iMesh
end subroutine update_list_isct
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_continuous_mesh_line()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__make_continuous_mesh_line'
  !-------------------------------------------------------------
  ! Integrate intersections with same mesh
  !-------------------------------------------------------------
  !call logmsg('Meshes: '//str(nMesh)//' leng: '//str(sum(lst_leng(:nMesh))))

  is_updated = .true.
  iMesh = 1
  do jMesh = 2, nMesh
    if( lst_gx(jMesh) == lst_gx(iMesh) .and. &
        lst_gy(jMesh) == lst_gy(iMesh) )then
      !call logmsg('integ mesh#'//str(jMesh,dgt(nMesh))//' to mesh#'//str(iMesh,dgt(nMesh)))
      call add(lst_leng(iMesh), lst_leng(jMesh))
      lst_gx(jMesh) = 0
      is_updated = .true.
    else
      iMesh = jMesh
    endif
  enddo  ! jMesh

  iMesh = 0
  do jMesh = 1, nMesh
    if( lst_gx(jMesh) == 0 ) cycle
    call add(iMesh)
    lst_gx(iMesh) = lst_gx(jMesh)
    lst_gy(iMesh) = lst_gy(jMesh)
    lst_leng(iMesh) = lst_leng(jMesh)
  enddo
  nMesh = iMesh

  do iMesh = 2, nMesh
    if( lst_gx(iMesh) == lst_gx(iMesh-1) .and. &
        lst_gy(iMesh) == lst_gy(iMesh-1) )then
      call errend('mesh#'//str(iMesh)//&
        'Same pair of (x,y): ('//str((/lst_gx(iMesh),lst_gy(iMesh)/),',')//')')
    endif
  enddo
  !call logmsg('Meshes: '//str(nMesh)//' leng: '//str(sum(lst_leng(:nMesh))))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(lst_counted, max(size(lst_counted), nMesh), clear=.true.)

  do iMesh = 1, nMesh
    lst_counted(iMesh) = is_counted(lst_leng(iMesh), lst_gy(iMesh))
  enddo  ! iMesh

  !call logmsg('Meshes where isct > thresh: '//str(count(lst_counted(:nMesh))))
  !-------------------------------------------------------------
  ! Temporarily set the end points as counted
  !-------------------------------------------------------------
  lst_counted(1) = .true.
  lst_counted(nMesh) = .true.
  !-------------------------------------------------------------
  ! Make the line of intersecting meshes continuous
  !-------------------------------------------------------------
  if( debug_this ) call logmsg('Making the line of intersecting meshes continuous')

  is_updated = .true.
  do while( is_updated )
    is_updated = .false.
    iMesh = 1
    jMesh = min(1, nMesh-1)
    do while( jMesh < nMesh )
      call add(jMesh)
      do while( .not. lst_counted(jMesh) )
        call add(jMesh)
      enddo

      if( abs(lst_gx(jMesh) - lst_gx(iMesh)) > 1 .or. &
          abs(lst_gy(jMesh) - lst_gy(iMesh)) > 1 )then
        kMesh = iMesh + maxloc(lst_leng(iMesh+1:jMesh-1),1)
        !call logmsg(&
        !  str(iMesh,dgt(nMesh))//' ('//str((/lst_gx(iMesh),lst_gy(iMesh)/),DGT_GXY,',')//') - '//&
        !  str(kMesh,dgt(nMesh))//' ('//str((/lst_gx(kMesh),lst_gy(kMesh)/),DGT_GXY,',')//') - '//&
        !  str(jMesh,dgt(nMesh))//' ('//str((/lst_gx(jMesh),lst_gy(jMesh)/),DGT_GXY,',')//')')
        if( lst_counted(kMesh) )then
          call errend('Mesh#'//str(kMesh)//' is already counted.')
        endif
        lst_counted(kMesh) = .true.
        is_updated = .true.
      endif

      iMesh = jMesh
    enddo  ! jMesh
  enddo  ! while is_updated

  if( debug_this ) call logmsg('done')
  !-------------------------------------------------------------
  ! Restore the end points
  !-------------------------------------------------------------
  lst_counted(1) = is_counted(lst_leng(1), lst_gy(1))
  !lst_counted(nMesh) = is_counted(lst_leng(nMesh), lst_gy(nMesh))

  leng_mesh = sum(lst_leng(:nMesh), mask=lst_counted(:nMesh))
  !-------------------------------------------------------------
  ! Remove uncounted elements
  !-------------------------------------------------------------
  iMesh = 0
  do jMesh = 1, nMesh
    if( .not. lst_counted(jMesh) ) cycle
    call add(iMesh)
    lst_gx(iMesh) = lst_gx(jMesh)
    lst_gy(iMesh) = lst_gy(jMesh)
    lst_leng(iMesh) = lst_leng(jMesh)
    lst_counted(jMesh) = .false.
    lst_counted(iMesh) = .true.
  enddo  ! jMesh
  nMesh = iMesh

  call assert_mesh_leng_unchanged('remove uncounted elements')
  !-------------------------------------------------------------
  ! Merge same mesh
  !-------------------------------------------------------------
  iMesh = 1
  do jMesh = 2, nMesh
    if( lst_gx(jMesh) == lst_gx(iMesh) .and. lst_gy(jMesh) == lst_gy(iMesh) )then
      call add(lst_leng(iMesh), lst_leng(jMesh))
      lst_counted(jMesh) = .false.
    else
      iMesh = jMesh
    endif
  enddo
  nMesh = iMesh

  call assert_mesh_leng_unchanged('merge same mesh')
  !-------------------------------------------------------------
  ! Remove empty slots
  !-------------------------------------------------------------
  iMesh = 0
  do jMesh = 1, nMesh
    if( .not. lst_counted(jMesh) ) cycle
    call add(iMesh)
    lst_gx(iMesh) = lst_gx(jMesh)
    lst_gy(iMesh) = lst_gy(jMesh)
    lst_leng(iMesh) = lst_leng(jMesh)
    lst_counted(iMesh) = .true.
  enddo  ! jMesh
  nMesh = iMesh
  !-------------------------------------------------------------
  ! Validate results
  !-------------------------------------------------------------
  call assert_mesh_leng_unchanged('remove empty slots')

  do iMesh = 1, nMesh-1
    if( abs(lst_gx(iMesh+1) - lst_gx(iMesh)) > 1 .or. &
        abs(lst_gy(iMesh+1) - lst_gy(iMesh)) > 1 )then
      call errend('mesh#'//str(iMesh)//' ('//str((/lst_gx(iMesh),lst_gy(iMesh)/),',')//')'//&
                '\nmesh#'//str(iMesh+1)//' ('//str((/lst_gx(iMesh+1),lst_gy(iMesh+1)/),',')//')')
    endif
  enddo
end subroutine make_continuous_mesh_line
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine assert_mesh_leng_unchanged(step)
  implicit none
  character(*), intent(in) :: step

  real(8) :: leng_mesh_new

  leng_mesh_new = sum(lst_leng(:nMesh), mask=lst_counted(:nMesh))

  if( leng_mesh == 0.d0 )then
    if( leng_mesh_new > 0.d0 )then
      call errend('leng_mesh '//str(leng_mesh)//' -> '//str(leng_mesh_new)//&
        trim(step), PRCNAM, MODNAM)
    endif
  elseif( abs(leng_mesh_new - leng_mesh) / leng_mesh > 1d-6 )then
    call errend('leng_mesh '//str(leng_mesh)//' -> '//str(leng_mesh_new)//&
      trim(step), PRCNAM, MODNAM)
  endif
end subroutine assert_mesh_leng_unchanged
!---------------------------------------------------------------
!
!---------------------------------------------------------------
logical function is_counted(leng, iy) result(res)
  implicit none
  real(8), intent(in) :: leng
  integer, intent(in) :: iy

  res = leng > mesh_size(iy) * THRESH_MESHSIZE_FRAC
end function is_counted
!---------------------------------------------------------------
end subroutine modify_flwdir_network
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
subroutine calcUpperArea(catmType, resl, modified, id)
  use c2_strnk_io, only: &
    get_f_lst_networks_channel
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c2_jflw_grid, only: &
    south_of_gy
  use c3_joint_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calcUpperArea'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl
  logical     , intent(in) :: modified
  character(*), intent(in) :: id

  character(32) :: id_
  integer :: nCatm, jCatm
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixara(:)
  integer :: igy

  character(4) :: c_mod
  character(CLEN_PATH) :: f
  integer :: un
  character :: c_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(resl)

  ! Calc. pixel area
  allocate(pixara(1:NGY))
  allocate(pixlat(0:NGY))

  pixlat(0) = REGION_NORTH * d2r
  do igy = 1, NGY
    pixlat(igy) = south_of_gy(igy) * d2r
  enddo
  !call logmsg('pixlat: '//str(pixlat(:2)*r2d,'f11.7',', ')//&
  !            ', ..., '//str(pixlat(NGY-2:)*r2d,'f11.7',', '))

  pixara(:) = area_sphere_rect(pixlat(0:NGY-1), pixlat(1:NGY)) * (GRIDSIZE_LON*d2r) * EARTH_R**2

  deallocate(pixlat)

  if( modified )then
    c_mod = '_mod'
  else
    c_mod = ''
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( catmType )
  !-------------------------------------------------------------
  ! Case:
  case( CATMTYPE__BASIN )
  !-------------------------------------------------------------
  ! Case: Network
  case( CATMTYPE__NETWORK )
    if( id == '' )then
      f = get_f_lst_networks_channel()
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nCatm
      do jCatm = 1, nCatm
        read(un,*) c_, id_
        call calc_upper_area__network(resl, c_mod, id_, pixara)
      enddo
      close(un)
    else
      call calc_upper_area__network(resl, c_mod, id, pixara)
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('catmType', catmType))
  endselect
end subroutine calcUpperArea
!===============================================================
!
!===============================================================
subroutine calc_upper_area__network(resl, c_mod, id, pixara)
  use c2_strnk_io, only: &
    read_network_mesh_domain, &
    get_f_network_mesh      
  use c2_jflw_const
!  use c2_jflw_grid, only: &
!    get_nextxy
  use c2_jflw_grid
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_upper_area__network'
  character(*), intent(in) :: resl
  character(*), intent(in) :: c_mod
  character(*), intent(in) :: id
  real(8)     , intent(in) :: pixara(:)

  integer(1), allocatable :: msk(:,:)
  integer(1), allocatable :: fdr(:,:)
  real(8)   , allocatable :: upa(:,:), upaadd(:,:)
  integer   , allocatable :: upg(:,:), upgadd(:,:)
  integer, allocatable :: lst_gx(:), lst_gy(:)
  integer, allocatable :: passed(:,:)
  integer :: nx, ny, gxs, gxe, gys, gye
  integer :: igx, igy, gx, gy, gxx, gyy
  integer :: gxs1, gxe1, gys1, gye1, gxs2, gxe2, gys2, gye2
  real(8) :: west, east, south, north
  integer :: n, n_next, i
  logical :: is_updated
  integer :: iter
  integer :: grdnum_true, grdnum
  real(8) :: bsnara_true, bsnara
  real(8) :: err_norm

  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Read domain info.
  !-------------------------------------------------------------
  call logmsg('Network '//str(id))

  call read_network_mesh_domain(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north &
  )

  allocate(msk(gxs:gxe,gys:gye))
  allocate(fdr(gxs:gxe,gys:gye))
  allocate(upa(gxs:gxe,gys:gye))
  allocate(upg(gxs:gxe,gys:gye))
  allocate(upaadd(gxs:gxe,gys:gye))
  allocate(upgadd(gxs:gxe,gys:gye))
  allocate(passed(gxs:gxe,gys:gye))
  !-------------------------------------------------------------
  ! Read maps
  !-------------------------------------------------------------
  call logent('Reading topography data')

  f = get_f_network_mesh(resl, 'mask', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(msk, f) )

  f = get_f_network_mesh(resl, 'dir'//str(c_mod), id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(fdr, f) )

  call logext()
  !-------------------------------------------------------------
  ! Calc. upper area
  !-------------------------------------------------------------
  call logent('Calculating upper area')

  n = count(msk > 0_1)
  allocate(lst_gx(n))
  allocate(lst_gy(n))

  n = 0
  upgadd(:,:) = 0
  upaadd(:,:) = 0.d0
  do igy = gys, gye
  do igx = gxs, gxe
    if( msk(igx,igy) <= 0_1 ) cycle
    upgadd(igx,igy) = 1
    upaadd(igx,igy) = pixara(igy)
    call add(n)
    lst_gx(n) = igx
    lst_gy(n) = igy
  enddo
  enddo
  grdnum_true = sum(upgadd)
  bsnara_true = sum(upaadd)

  upa(:,:) = 0.d0
  upg(:,:) = 0
  is_updated = .true.
  iter = 0
  passed(:,:) = 0
  do while( is_updated )
    is_updated = .false.
    call add(iter)

    n_next = 0
    do i = 1, n
      gx = lst_gx(i)
      gy = lst_gy(i)

      if( upgadd(gx,gy) == 0 ) cycle

      is_updated = .true.

      call get_nextxy(gx, gy, fdr(gx,gy), gxx, gyy)

      call add(upg(gx,gy), upgadd(gx,gy))
      call add(upa(gx,gy), upaadd(gx,gy))

      if( gxx > 0 )then
        if( passed(gxx,gyy) /= iter )then
          passed(gxx,gyy) = iter
          call add(n_next)
          lst_gx(n_next) = gxx
          lst_gy(n_next) = gyy
        endif
        call add(upgadd(gxx,gyy), upgadd(gx,gy))
        call add(upaadd(gxx,gyy), upaadd(gx,gy))
      endif

      upgadd(gx,gy) = 0
      upaadd(gx,gy) = 0.d0
    enddo  ! i

    n = n_next
  enddo  ! while is_updated

  ! Validate results
  !-------------------------------------------------------------
  call logent('Validating the results')

  grdnum = 0
  bsnara = 0.d0
  do igy = gys, gye
  do igx = gxs, gxe
    selectcase( fdr(igx,igy) )
    case( FDR_RIVERMOUTH, &
          FDR_INLAND )
      call add(grdnum, upg(igx,igy))
      call add(bsnara, upa(igx,igy))
    endselect
  enddo
  enddo

!  grdnum = 0
!  bsnara = 0.d0
!  do igy = gys, gye
!  do igx = gxs, gxe
!    if( msk(igx,igy) < 0_1 ) cycle
!    if( fdr(igx,igy) <= 0_1 )then
!      call add(bsnara, upa(igx,igy))
!      call add(grdnum, upg(igx,igy))
!    else
!      call get_nextxy(igx, igy, fdr(igx,igy), gxx, gyy)
!      if( msk(gxx,gyy) < 0_1 )then
!      call logwrn('('//str((/igx,igy/),',')//') msk '//str(msk(igx,igy))//&
!        ' fdr '//str(fdr(igx,igy))//&
!        ' ('//str((/center_of_gx(igx),center_of_gy(igy)/),'es20.13',',')//')')
!        call add(bsnara, upa(igx,igy))
!        call add(grdnum, upg(igx,igy))
!      endif
!    endif
!  enddo  ! igx
!  enddo  ! igy

  call logmsg('upg max: '//str(maxval(upg))//' grdnum: '//str(grdnum_true))
  call logmsg('upa max: '//str(maxval(upa))//' bsnara: '//str(bsnara_true))

  if( grdnum /= grdnum_true )then
    call errend('The number of grids do not match.'//&
      '\n  truth     : '//str(grdnum_true)//&
      '\n  calculated: '//str(grdnum))
  endif

  err_norm = abs(bsnara - bsnara_true) / bsnara_true
  call logmsg('basin area truth: '//str(bsnara_true)//&
            ', upa: '//str(bsnara)//&
            ', normalized error: '//str(err_norm))
  if( err_norm > 1d-8 )then
    call errend('Error exceeded the threshold.')
  endif

  call logext()
  !-------------------------------------------------------------
  deallocate(msk)
  deallocate(fdr)
  deallocate(upgadd)
  deallocate(upaadd)
  deallocate(passed)

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f = get_f_network_mesh(resl, 'upa'//str(c_mod), id)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(upa(gxs:gxe,gys:gye), f, dtype=DTYPE_REAL, replace=.true.) )

  f = get_f_network_mesh(resl, 'upg'//str(c_mod), id)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(upg(gxs:gxe,gys:gye), f, replace=.true.) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(upg)
  deallocate(upa)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calc_upper_area__network
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
subroutine copy_cmnnwk(cmn, nwk)
  implicit none
  type(cmn_network_), intent(in) :: cmn
  type(network_), intent(out) :: nwk

  type(cmn_channel_), pointer :: cch
  type(cmn_node_), pointer :: cnode
  type(channel_), pointer :: ch
  type(node_), pointer :: node
  integer :: jCh
  integer :: jNode

  nwk%id = cmn%id

  nwk%nCh = cmn%nCh
  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    cch => cmn%channel_(jCh)
    ch => nwk%channel(jCh)

    ch%n = cch%n
    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))
    ch%lon(:) = cch%lon(:)
    ch%lat(:) = cch%lat(:)

    ch%leng = cch%leng

    allocate(ch%node(2))
    do jNode = 1, 2
      cnode => cch%node_(jNode)
      node => ch%node(jNode)
      node%lon = cnode%lon
      node%lat = cnode%lat
      node%typ = cnode%typ
      node%elv = cnode%elv
      node%downleng = cnode%downleng
      node%iNode = cnode%iNode
    enddo  ! jNode/
  enddo  ! jCh/
end subroutine copy_cmnnwk
!===============================================================
!
!===============================================================
end module mod_topo
