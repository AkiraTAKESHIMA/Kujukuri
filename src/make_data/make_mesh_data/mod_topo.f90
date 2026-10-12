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
  public :: modifyFlwDir
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
    integer, pointer :: x(:), y(:)
    real(8), pointer :: leng(:)
    integer :: xs, xe, ys, ye
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
    real(8) :: west, east, south, north
    integer :: xs, xe, ys, ye
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
  selectcase( networkType )
  !-------------------------------------------------------------
  ! Case: network
  case( NETWORKTYPE__NETWORK )
    call jflw_set_resolution(resl)

    if( id == '' )then
      f = strnk_get_f_lst_networks_channel()
      call logmsg('Reading '//str(f))
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nBasin
      read(un,*)
      do iBasin = 1, nBasin
        read(un,*) c_, id_
        call modify_flwdir(resl, id_)
      enddo  ! iBasin/
      close(un)
    else
      call modify_flwdir(resl, id)
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
subroutine modify_flwdir(resl, id)
  use c2_jflw_const
  use c2_jflw_grid, only: &
    center_of_gy, &
    north_of_gy, &
    south_of_gy
  use c2_strnk_io, only: &
    read_network            , &
    read_network_mesh_domain, &
    get_f_network_channel   , &
    get_f_network_meshisct  , &
    get_f_network_mesh    
  use c2_strnk_util, only: &
    clear_cmn_network
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_flwdir'
  character(*), intent(in) :: resl
  character(*), intent(in) :: id

  type(network_) :: nwk
  type(channel_), pointer :: ch
  type(node_), pointer :: node, node2
  type(isct_), pointer :: ci
  type(nwknode_), pointer :: nwknode
  type(cmn_network_) :: cmnnwk
  integer(1), allocatable :: nwkmap(:,:)  !(xs:xe,ys:ye)
  integer(1), allocatable :: fdrmap(:,:)
  integer, allocatable :: lst_iNode_src(:)  !(nNode_src)
  real(8), allocatable :: lst_dist_src(:)
  real(8), allocatable :: mesh_size(:)  !(gys:gye)
  integer, pointer :: lst_x(:), lst_y(:)  !(nMesh)
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
  integer :: ix, iy
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_jCh(:), lst_jNode(:)
  integer, allocatable :: arg(:)

  integer :: iter
  real(8) :: leng_mesh

  real(8), parameter :: THRESH_MESHSIZE_FRAC = 1.d0
  integer, parameter :: THRESH_ITER = 10000

  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  call logmsg('Network '//str(id))
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
    nwk%xs, nwk%xe, nwk%ys, nwk%ye, &
    nwk%west, nwk%east, nwk%south, nwk%north, &
    echo=.true.)

  call logmsg('Mesh: ['//str((/nwk%xs,nwk%xe/),DGT_GXY,':')//&
                    ','//str((/nwk%ys,nwk%ye/),DGT_GXY,':')//']')

  ! Intersections
  f = get_f_network_meshisct(MESHTYPE__JFLW, resl, id)
  call logmsg('Reading '//str(f))

  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')
  read(un)  ! nCh
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    ci => ch%isct

    read(un) ci%n
    allocate(ci%x(ci%n))
    allocate(ci%y(ci%n))
    allocate(ci%leng(ci%n))
    read(un) ci%x(:)
    read(un) ci%y(:)
    read(un) ci%leng(:)

    ci%xs = minval(ci%x)
    ci%xe = maxval(ci%x)
    ci%ys = minval(ci%y)
    ci%ye = maxval(ci%y)
  enddo  ! jCh
  close(un)

  if( nwk%xs > minval(nwk%channel(:)%isct%xs) .or. &
      nwk%xe < maxval(nwk%channel(:)%isct%xe) .or. &
      nwk%ys > minval(nwk%channel(:)%isct%ys) .or. &
      nwk%ye < maxval(nwk%channel(:)%isct%ye) )then
    call errend('Inconsistency between domain and intersection.'//&
      '\nintersection: ['//&
      str((/minval(nwk%channel(:)%isct%xs),maxval(nwk%channel(:)%isct%xe)/),DGT_GXY,':')//','//&
      str((/minval(nwk%channel(:)%isct%ys),maxval(nwk%channel(:)%isct%ye)/),DGT_GXY,':')//']')
  endif

  ! Mesh data
  allocate(fdrmap(nwk%xs:nwk%xe,nwk%ys:nwk%ye))

  f = get_f_network_mesh(resl, 'dir', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(fdrmap, f, dtype=DTYPE_INT4) )

  call logext()
  !-------------------------------------------------------------
  ! Make node list
  !-------------------------------------------------------------
  call logent('Making node list')

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
  deallocate(lst_lon)
  deallocate(lst_lat)
  deallocate(lst_jCh)
  deallocate(lst_jNode)

  ! Info. of opposite side node
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    allocate(nwknode%iNode(nwknode%nCh))
    nwknode%iNode_to_outlet = 0
    downleng_min = nwknode%downleng
    do jjCh = 1, nwknode%nCh
      node => nwk%channel(nwknode%jCh(jjCh))%node(3-nwknode%jNode(jjCh))
      nwknode%iNode(jjCh) = node%iNode
      if( node%downleng < downleng_min )then
        downleng_min = node%downleng
        nwknode%iNode_to_outlet = node%iNode
        nwknode%jjCh_to_outlet = jjCh
      elseif( node%downleng == downleng_min )then
        if( iNode /= nwknode%iNode_to_outlet )then
          call errend('node%downleng == downleng_min')
        endif
      endif
    enddo  ! jjCh/
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

  allocate(mesh_size(nwk%ys:nwk%ye))

  do iy = nwk%ys, nwk%ye
    ! length in longit: dlon * R*cos(lat)
    !           latit : dlat * R
    mesh_size(iy) = sqrt(&
      (GRIDSIZE_LON*d2r * cos(center_of_gy(iy)*d2r)) &
      * (GRIDSIZE_LAT*d2r) &
    ) * EARTH_R
    !mesh_size(iy) = area_sphere_rect(north_of_gy(iy)*d2r, south_of_gy(iy)*d2r)&
    !  * GRIDSIZE_LON*d2r * EARTH_R**2
  enddo  ! iy/
  ! Diameter of the circle with the same area to the mesh
  !mesh_size(:) = sqrt(mesh_size(:) / pi) * 2.d0
  call logmsg('Mesh size: '//str(sum(mesh_size)/(nwk%ye-nwk%ys+1))//' [m]')

  call logext()
  !-------------------------------------------------------------
  ! Make mesh line from source to outlet
  !-------------------------------------------------------------
  call logent('Making mesh line from source to outlet')

  nMesh = (nwk%xe-nwk%xs+1) + (nwk%ye-nwk%ys+1)
  allocate(lst_x(nMesh))
  allocate(lst_y(nMesh))
  allocate(lst_leng(nMesh))
  allocate(lst_counted(nMesh))

  allocate(nwkmap(nwk%xs:nwk%xe,nwk%ys:nwk%ye))
  nwkmap(:,:) = 0_1

  do iiNode = 1, nNode_src
    !-----------------------------------------------------------
    ! Make a list of intersections
    !-----------------------------------------------------------
    iNode = lst_iNode_src(iiNode)
    nwknode => nwk%node(iNode)
    call logmsg('src node#'//str(iNode,dgt(nwk%nNode))//' dist '//str(nwknode%downleng))

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

      !call logmsg('ch#'//str(jCh,dgt(nwk%nCh))//' dir '//str(chdir,2)//&
      !' '//slonlat(ch%node(1)%lon,ch%node(1)%lat)//&
      !' - '//str(slonlat(ch%node(2)%lon,ch%node(2)%lat))//&
      !'\n['//str((/ci%x(1),ci%y(1)/),DGT_GXY,',')//'] - ['//&
      ! str((/ci%x(ci%n),ci%y(ci%n)/),DGT_GXY,',')//']'//&
      ! ' n: '//str(ci%n))

      call update_list_isct()

      iNode = nwknode%iNode_to_outlet
      nwknode => nwk%node(nwknode%iNode_to_outlet)
      !call logmsg('  - node#'//str(iNode,dgt(nwk%nNode))//' dist '//str(nwknode%downleng))
    enddo  ! while nwknode%downleng > 0


    call make_continuous_mesh_line()


    call modify_flow_direction(.true.)


    leng_mesh = 0.d0
    do jMesh = 1, nMesh
      call add(leng_mesh, mesh_size(lst_y(iMesh)))
    enddo  ! jMesh/

    call logmsg('leng channel: '//str(nwk%node(lst_iNode_src(iiNode))%downleng)//&
                ' isct: '//str(sum(lst_leng(:nMesh)))//&
                ' mesh: '//str(leng_mesh))

    call update_network_map(1_1)
    !-----------------------------------------------------------
  enddo  ! iiNode


  call logext()
  !-------------------------------------------------------------
  ! Isolated channels
  !-------------------------------------------------------------
  call logent('Merging intersections of isolated channels')

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( .not. ch%is_isolated ) cycle
    call logmsg('ch#'//str(jCh)//' leng '//str(ch%leng))
    ci => ch%isct

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

    call modify_flow_direction(.false.)

    do iMesh = 1, nMesh
      if( nwkmap(lst_x(iMesh),lst_y(iMesh)) /= 0_1 )then
        lst_x(iMesh) = 0
      endif
    enddo  ! iMesh/

    iMesh = 0
    do jMesh = 1, nMesh
      if( lst_x(jMesh) == 0 ) cycle
      call add(iMesh)
      lst_x(iMesh) = lst_x(jMesh)
      lst_y(iMesh) = lst_y(jMesh)
      lst_leng(iMesh) = lst_leng(jMesh)
    enddo  ! jMesh/
    nMesh = iMesh

    call update_network_map(2_1)
  enddo  ! jCh/

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  call traperr( wbin(fdrmap, 'tmp/dir.bin', replace=.true.) )

  call logext()
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
    if( size(lst_x) == nMesh )then
      call realloc(lst_x, nMesh*2, clear=.false.)
      call realloc(lst_y, nMesh*2, clear=.false.)
      call realloc(lst_leng, nMesh*2, clear=.false.)
    endif
    call add(nMesh)
    lst_x(nMesh) = ci%x(iMesh)
    lst_y(nMesh) = ci%y(iMesh)
    lst_leng(nMesh) = ci%leng(iMesh)
    if( nMesh > 1 )then
      if( abs(lst_x(nMesh) - lst_x(nMesh-1)) > 1 .or. &
          abs(lst_y(nMesh) - lst_y(nMesh-1)) > 1 )then
        call errend('Discontinuition was found in the intersection.'//&
          '\ncurrent: #'//str(nMesh-1)//&
            '('//str((/lst_x(nMesh-1),lst_y(nMesh-1)/),DGT_GXY,',')//')'//&
          '\nnext : #'//str(iMesh)//&
            '('//str((/ci%x(iMesh),ci%y(iMesh)/),DGT_GXY,',')//')'//&
          '\nchdir: '//str(chdir)//&
          '\nisct x: '//str(ci%x,DGT_GXY)//&
          '\n     y: '//str(ci%y,DGT_GXY))
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
  do while( is_updated )
    is_updated = .false.
    iMesh = 1
    do jMesh = 2, nMesh
      if( lst_x(jMesh) == lst_x(iMesh) .and. &
          lst_y(jMesh) == lst_y(iMesh) )then
        !call logmsg('integ mesh#'//str(jMesh,dgt(nMesh))//' to mesh#'//str(iMesh,dgt(nMesh)))
        call add(lst_leng(iMesh), lst_leng(jMesh))
        lst_x(jMesh) = 0
        is_updated = .true.
      else
        iMesh = jMesh
      endif
    enddo  ! jMesh
  enddo  ! while is_updated

  iMesh = 0
  do jMesh = 1, nMesh
    if( lst_x(jMesh) == 0 ) cycle
    call add(iMesh)
    lst_x(iMesh) = lst_x(jMesh)
    lst_y(iMesh) = lst_y(jMesh)
    lst_leng(iMesh) = lst_leng(jMesh)
  enddo
  nMesh = iMesh

  do iMesh = 2, nMesh
    if( lst_x(iMesh) == lst_x(iMesh-1) .and. &
        lst_y(iMesh) == lst_y(iMesh-1) )then
      call errend('mesh#'//str(iMesh)//&
        'Same pair of (x,y): ('//str((/lst_x(iMesh),lst_y(iMesh)/),',')//')')
    endif
  enddo
  !call logmsg('Meshes: '//str(nMesh)//' leng: '//str(sum(lst_leng(:nMesh))))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(lst_counted, max(size(lst_counted), nMesh), clear=.true.)

  do iMesh = 1, nMesh
    lst_counted(iMesh) = is_counted(lst_leng(iMesh), lst_y(iMesh))
  enddo  ! iMesh

  !call logmsg('Meshes where isct > thresh: '//str(count(lst_counted(:nMesh))))

  ! Temporary set the end points as counted
  lst_counted(1) = .true.
  lst_counted(nMesh) = .true.

  ! Make the line of intersecting meshes continuous
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

      if( abs(lst_x(jMesh) - lst_x(iMesh)) > 1 .or. &
          abs(lst_y(jMesh) - lst_y(iMesh)) > 1 )then
        kMesh = iMesh + maxloc(lst_leng(iMesh+1:jMesh-1),1)
        !call logmsg(&
        !  str(iMesh,dgt(nMesh))//' ('//str((/lst_x(iMesh),lst_y(iMesh)/),DGT_GXY,',')//') - '//&
        !  str(kMesh,dgt(nMesh))//' ('//str((/lst_x(kMesh),lst_y(kMesh)/),DGT_GXY,',')//') - '//&
        !  str(jMesh,dgt(nMesh))//' ('//str((/lst_x(jMesh),lst_y(jMesh)/),DGT_GXY,',')//')')
        if( lst_counted(kMesh) )then
          call errend('Mesh#'//str(kMesh)//' is already counted.')
        endif
        lst_counted(kMesh) = .true.
        is_updated = .true.
      endif

      iMesh = jMesh
    enddo  ! jMesh
  enddo  ! while is_updated

  ! Restore the end points
  lst_counted(1) = is_counted(lst_leng(1), lst_y(1))
  lst_counted(nMesh) = is_counted(lst_leng(nMesh), lst_y(nMesh))

  leng_mesh = sum(lst_leng(:nMesh), mask=lst_counted(:nMesh))

  ! Remove uncounted elements
  iMesh = 0
  do jMesh = 1, nMesh
    if( .not. lst_counted(jMesh) ) cycle
    call add(iMesh)
    lst_x(iMesh) = lst_x(jMesh)
    lst_y(iMesh) = lst_y(jMesh)
    lst_leng(iMesh) = lst_leng(jMesh)
    lst_counted(jMesh) = .false.
    lst_counted(iMesh) = .true.
  enddo  ! jMesh
  nMesh = iMesh

  call assert_mesh_leng_unchanged('remove uncounted elements')

  ! Merge same mesh
  iMesh = 1
  do jMesh = 2, nMesh
    if( lst_x(jMesh) == lst_x(iMesh) .and. lst_y(jMesh) == lst_y(iMesh) )then
      call add(lst_leng(iMesh), lst_leng(jMesh))
      lst_counted(jMesh) = .false.
    else
      iMesh = jMesh
    endif
  enddo
  nMesh = iMesh

  call assert_mesh_leng_unchanged('merge same mesh')

  ! Remove empty slots
  iMesh = 0
  do jMesh = 1, nMesh
    if( .not. lst_counted(jMesh) ) cycle
    call add(iMesh)
    lst_x(iMesh) = lst_x(jMesh)
    lst_y(iMesh) = lst_y(jMesh)
    lst_leng(iMesh) = lst_leng(jMesh)
    lst_counted(iMesh) = .true.
  enddo  ! jMesh
  nMesh = iMesh

  call assert_mesh_leng_unchanged('remove empty slots')

  do iMesh = 1, nMesh-1
    if( abs(lst_x(iMesh+1) - lst_x(iMesh)) > 1 .or. &
        abs(lst_y(iMesh+1) - lst_y(iMesh)) > 1 )then
      call errend('mesh#'//str(iMesh)//' ('//str((/lst_x(iMesh),lst_y(iMesh)/),',')//')'//&
                '\nmesh#'//str(iMesh+1)//' ('//str((/lst_x(iMesh+1),lst_y(iMesh+1)/),',')//')')
    endif
  enddo
end subroutine make_continuous_mesh_line
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine assert_mesh_leng_unchanged(step)
  implicit none
  character(*), intent(in) :: step

  if( abs(sum(lst_leng(:nMesh), mask=lst_counted(:nMesh)) - leng_mesh) / leng_mesh > 1d-6 )then
    call errend('leng_mesh '//str(leng_mesh)//' -> '//&
      str(sum(lst_leng(:nMesh), mask=lst_counted(:nMesh))), &
      trim(step), PRCNAM, MODNAM)
  endif
end subroutine assert_mesh_leng_unchanged
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine modify_flow_direction(&
  is_end_outlet &
)
  implicit none
  logical, intent(in) :: is_end_outlet

  do iMesh = 1, nMesh-1
    fdrmap(lst_x(iMesh),lst_y(iMesh)) &
    = get_flwdir(lst_x(iMesh+1)-lst_x(iMesh), lst_y(iMesh+1)-lst_y(iMesh))
  enddo  ! iMesh/

  if( is_end_outlet )then
    ix = lst_x(nMesh)
    iy = lst_y(nMesh)
    if( fdrmap(ix,iy) /= FDR_RIVERMOUTH )then
      call logmsg('fdr('//str((/ix,iy/),',')//') -> river mouth')
      fdrmap(ix,iy) = FDR_RIVERMOUTH
    endif
  endif

  !if( fdrmap(ix,iy) > 0_1 )then
  !  call logmsg('flwdir at the outlet ('//str((/ix,iy/),DGT_GXY,',')//'): '//str(fdrmap(ix,iy)))
  !endif
end subroutine modify_flow_direction
!---------------------------------------------------------------
!
!---------------------------------------------------------------
integer(1) function get_flwdir(xdir, ydir) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_flwdir'
  integer, intent(in) :: xdir, ydir

  selectcase( xdir )
  case( 0 )  ! none
    selectcase( ydir )
    case( 1 )
      res = FDR_SOUTH
    case( -1 )
      res = FDR_NORTH
    case default
      call errend(msg_invalid_value('(xdir,ydir)','('//str((/xdir,ydir/),',')//')'), &
                  '', PRCNAM, MODNAM)
    endselect
  case( 1 )  ! east
    selectcase( ydir )
    case( 0 )
      res = FDR_EAST
    case( 1 )
      res = FDR_SOUTHEAST
    case( -1 )
      res = FDR_NORTHEAST
    case default
      call errend(msg_invalid_value('(xdir,ydir)','('//str((/xdir,ydir/),',')//')'), &
                  '', PRCNAM, MODNAM)
    endselect
  case( -1 )  ! west
    selectcase( ydir )
    case( 0 )
      res = FDR_WEST
    case( 1 )
      res = FDR_SOUTHWEST
    case( -1 )
      res = FDR_NORTHWEST
    case default
      call errend(msg_invalid_value('(xdir,ydir)','('//str((/xdir,ydir/),',')//')'), &
                  '', PRCNAM, MODNAM)
    endselect
  case default
    call errend(msg_invalid_value('xdir', xdir), &
                '', PRCNAM, MODNAM)
  endselect
end function get_flwdir
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
!
!---------------------------------------------------------------
subroutine update_network_map(i1)
  implicit none
  integer(1), intent(in) :: i1

  do iMesh = 1, nMesh
    nwkmap(lst_x(iMesh),lst_y(iMesh)) = i1
  enddo
end subroutine update_network_map
!---------------------------------------------------------------
!
!---------------------------------------------------------------
end subroutine modify_flwdir
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
