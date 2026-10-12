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
    read_network, &
    get_f_network_channel , &
    get_f_network_meshisct, &
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
  integer :: nNode_src
  integer, allocatable :: lst_iNode_src(:)
  real(8), allocatable :: lst_dist_src(:)
  integer :: nCh_isolated
  integer :: jCh, jjCh
  integer :: is, ie, iis, iie
  integer :: jNode
  integer :: iNode, iiNode
  integer :: mNode_up, mNode_down
  real(8) :: downleng_min
  integer, pointer :: lst_x(:), lst_y(:)
  real(8), pointer :: lst_leng(:)
  logical, pointer :: lst_counted(:)
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_jCh(:), lst_jNode(:)
  integer, allocatable :: arg(:)
  integer :: iter
!  integer(1), allocatable :: nwkmap(:,:)

  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  call logmsg('Network '//str(id))
  !-------------------------------------------------------------
  ! Read channel data
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

  nwk%xs = minval(nwk%channel(:)%isct%xs)
  nwk%xe = maxval(nwk%channel(:)%isct%xe)
  nwk%ys = minval(nwk%channel(:)%isct%ys)
  nwk%ye = maxval(nwk%channel(:)%isct%ye)

  call logmsg('Mesh: ['//str((/nwk%xs,nwk%xe/),DGT_GXY,':')//&
                    ','//str((/nwk%ys,nwk%ye/),DGT_GXY,':')//']')

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
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')


  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
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
