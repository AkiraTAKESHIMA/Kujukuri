module mod_separate_networks
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c2_nlni_const, only: &
        DGT_WSCODE
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: separateNetworks
  public :: makeModelNetworkData
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_separate_networks'

  type nwknode_
    real(8) :: lon, lat
    integer :: gx, gy
    real(8) :: elv
    integer :: typ
    integer :: nCh
    integer, pointer :: jCh(:)
    integer, pointer :: jNode(:)
    integer, pointer :: iNode(:)  ! neighbour nwknode
    integer :: iCh_down
    integer :: iNode_down
    real(8) :: downleng
  end type

  type node_
    real(8) :: lon, lat
    integer :: iNode
    real(8) :: elv
    integer :: typ
    real(8) :: downleng
  end type

  type channel_
    integer :: n
    character(:), allocatable :: wsCode
    character(:), allocatable :: rvCode
    character(:), allocatable :: rvName
    integer :: wsCode_i
    logical :: is_wsCode_temporal
    character(:), allocatable :: wsCode_org
    integer :: jWsys
    real(8), pointer :: lon(:), lat(:)
    type(node_), pointer :: node(:)
    real(8) :: leng
    integer :: nwkId
    real(8) :: west, east, south, north
  end type

  type wsys_
    character(DGT_WSCODE) :: wsCode
    integer :: wsCode_i
    integer :: nCh
    integer, pointer :: jCh(:)
    real(8) :: leng
    integer :: jNwk
  end type

  type network_
    character(:), allocatable :: uid
    character(:), allocatable :: uid_old
    real(8) :: west, east, south, north
    integer :: gxs, gxe, gys, gye
    integer :: nCh
    type(channel_), pointer :: channel(:)
    integer, pointer :: jCh(:)
    integer :: nNode
    type(nwknode_), pointer :: node(:)
    integer :: nWsys
    type(wsys_), pointer :: wsys(:)
  end type

  type conn_mem_
    integer :: jCh, jCh2
    integer :: iNode
  end type

  type conn_
    integer :: n
    type(conn_mem_), pointer :: mem(:)
    logical :: is_removed
  end type

  type network_new_
    integer :: nWsys
    integer, pointer :: jWsys(:)
    integer :: nCh
    integer, pointer :: jCh(:)
    integer :: nNode
    type(nwknode_), pointer :: node(:)
    real(8), pointer :: wsleng(:)
    integer :: id
  end type
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  real(8), parameter :: THRESH_ELV_DISCONNECT = 50.d0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine separateNetworks(uid_in)
  use c2_nlni_const, only: &
        DGT_WSCODE
  use c2_strnk_io, only: &
        get_f_tmp_networks_lst      , &
        get_f_tmp_network_channel   , &
        get_f_tmp_network_separation, &
        get_f_lst_networks_channel  , &
        get_f_network_channel
  use mod_util, only: &
        comma_json, &
        sBBox
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'separateNetworks'
  character(*), intent(in) :: uid_in

  type(network_), pointer :: lst_nwk(:), nwk
  type(wsys_), pointer :: wsys
  type(channel_), pointer :: ch
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode
  integer :: nNwk, jNwk, jjNwk, jNwk0
  integer :: mNwk, iNwk
  integer :: nNwk_old, jNwk_old
  integer :: jWsys
  integer :: nCh, jCh, iiCh
  integer :: jNode
  character(32) :: uid_old
  integer(4), allocatable :: lst_jNwk(:)
  integer :: cl_wsCode, cl_rvCode, cl_rvName

  character(CLEN_PATH) :: f_lst, f
  integer :: un_lst, un
  character :: c_
  integer :: dgt_nCh

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! *** For testing SUBROUTINE separate_network
  ! Separate the specified network and RETURN
  !-------------------------------------------------------------
  if( uid_in /= '' )then
    call separate_network(uid_in)
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! Separate networks
  !-------------------------------------------------------------
  call logent('Separating networks')

  f_lst = get_f_tmp_networks_lst()
  call logmsg('Reading '//str(f_lst))
  open(newunit=un_lst, file=f_lst, status='old')

  read(un_lst,*) c_, nNwk_old
  read(un_lst,*)
  nNwk = 0
  do jNwk_old = 1, nNwk_old
    read(un_lst,*) c_, uid_old
    call separate_network(uid_old)
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Read networks
  !-------------------------------------------------------------
  call logent('Reading network data')

  rewind(un_lst)
  read(un_lst,*) c_, nNwk_old
  read(un_lst,*)
  nNwk = 0
  do jNwk_old = 1, nNwk_old
    read(un_lst,*) c_, uid_old
    f = get_f_tmp_network_separation(uid_old)
    open(newunit=un, file=f, status='old')
    read(un,*) c_, mNwk
    close(un)

    call add(nNwk, mNwk)
  enddo

  call logmsg('Networks old: '//str(nNwk_old)//' new: '//str(nNwk))
  allocate(lst_nwk(nNwk))

  rewind(un_lst)
  read(un_lst,*)
  read(un_lst,*)
  jNwk0 = 0
  do jNwk_old = 1, nNwk_old
    read(un_lst,*) c_, uid_old

    ! Channel index in the network
    f = get_f_tmp_network_separation(uid_old)
    open(newunit=un, file=f, status='old')
    read(un,*) c_, mNwk

    do iNwk = 1, mNwk
      nwk => lst_nwk(jNwk0+iNwk)
      allocate(character(1) :: nwk%uid_old)
      nwk%uid_old = trim(uid_old)
    enddo

    read(un,*)
    do iNwk = 1, mNwk
      nwk => lst_nwk(jNwk0+iNwk)
      read(un,*) c_, c_, nwk%nWsys
      allocate(nwk%wsys(nwk%nWsys))
      do jWsys = 1, nwk%nWsys
        wsys => nwk%wsys(jWsys)
        read(un,*) wsys%wsCode, wsys%leng
        wsys%wsCode_i = int4_char(wsys%wsCode)
        wsys%wsCode_i = wsys%wsCode_i
      enddo  ! jWsys/
    enddo  ! jNwk/

    read(un,*)
    do iNwk = 1, mNwk
      nwk => lst_nwk(jNwk0+iNwk)
      read(un,*) c_, c_, nwk%nCh
      allocate(nwk%jCh(nwk%nCh))
      read(un,*) nwk%jCh(:)
      call sort(nwk%jCh)
    enddo  ! jNwk/
    close(un)

    ! Channel data
    nCh = sum(lst_nwk(jNwk0+1:jNwk0+mNwk)%nCh)

    allocate(lst_jNwk(nCh))
    lst_jNwk(:) = 0
    do iNwk = 1, mNwk
      jNwk = jNwk0 + iNwk
      nwk => lst_nwk(jNwk)
      do iiCh = 1, nwk%nCh
        jCh = nwk%jCh(iiCh)
        if( lst_jNwk(jCh) /= 0 )then
          call errend('lst_jNwk(jCh) /= 0')
        endif
        lst_jNwk(jCh) = jNwk
      enddo
    enddo
    if( any(lst_jNwk == 0) )then
      call errend('any(lst_jNwk == 0)')
    endif

    do iNwk = 1, mNwk
      nwk => lst_nwk(jNwk0+iNwk)
      allocate(nwk%channel(nwk%nCh))
      nwk%west = 1d20
      nwk%east = -1d20
      nwk%south = 1d20
      nwk%north = -1d20
    enddo

    f = get_f_tmp_network_channel(uid_old, 'sbin')
    open(newunit=un, file=f, form='unformatted', access='sequential', &
         action='read', status='old')

    read(un) ! nCh, west, east, south, north

    do jCh = 1, nCh
      jNwk = lst_jNwk(jCh)
      nwk => lst_nwk(jNwk)
      call search(jCh, nwk%jCh, iiCh)
      if( iiCh == 0 )then
        call errend('jCh was not found in nwk%jCh')
      endif
      ch => nwk%channel(iiCh)

      read(un) ch%west, ch%east, ch%south, ch%north

      read(un) cl_wsCode, cl_rvCode, cl_rvName
      allocate(character(cl_wsCode) :: ch%wsCode)
      allocate(character(cl_rvCode) :: ch%rvCode)
      allocate(character(cl_rvName) :: ch%rvName)
      read(un) ch%wsCode
      read(un) ch%rvCode
      read(un) ch%rvName

      read(un) ch%n

      allocate(ch%lon(ch%n))
      allocate(ch%lat(ch%n))

      read(un) ch%lon(:)
      read(un) ch%lat(:)

      read(un) ch%leng

      allocate(ch%node(2))
      ch%node(1)%lon = ch%lon(1)
      ch%node(1)%lat = ch%lat(1)
      ch%node(2)%lon = ch%lon(ch%n)
      ch%node(2)%lat = ch%lat(ch%n)

      do jNode = 1, 2
        node => ch%node(jNode)
        read(un) node%typ, node%elv
      enddo  ! jNode/

      nwk%west = min(nwk%west, ch%west)
      nwk%east = max(nwk%east, ch%east)
      nwk%south = min(nwk%south, ch%south)
      nwk%north = max(nwk%north, ch%north)
    enddo  ! jCh/

    close(un)

    deallocate(lst_jNwk)

    call add(jNwk0, mNwk)
  enddo  ! jNwk_old/
  close(un_lst)

  dgt_nCh = dgt(maxval(lst_nwk(:)%nCh))

  call logext()
  !-------------------------------------------------------------
  ! Make new ids
  !-------------------------------------------------------------
  call logent('Making new ids')

  allocate(lst_jNwk(nNwk))

  call set_separated_network_ids(lst_nwk, lst_jNwk)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating distance nodes to river mouth')

  do jNwk = 1, nNwk
    call calc_distance_to_river_mouth(lst_nwk(jNwk))
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  call logent('List')

  f = get_f_lst_networks_channel()
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(1x,a)") 'networks '//str(nNwk)
  write(un,"(1x,a)") 'i uid nCh leng west east south north'
  do jjNwk = 1, nNwk
    jNwk = lst_jNwk(jjNwk)
    nwk => lst_nwk(jNwk)

    write(un,"(1x,a)") &
        str(jjNwk,dgt(nNwk))//' '//&
        str(nwk%uid)//' '//str(nwk%nCh,dgt_nCh)//' '//str(sum(nwk%wsys(:)%leng))//&
        str((/nwk%west,nwk%east/),'f16.11')//' '//str((/nwk%south,nwk%north/),'f15.11')
  enddo  ! jNwk/
  close(un)

  call logext()

  call logent('Sequential binary')

  do jjNwk = 1, nNwk
    jNwk = lst_jNwk(jjNwk)
    nwk => lst_nwk(jNwk)

    f = get_f_network_channel(nwk%uid, 'sbin')
    if( jjNwk <= 3 .or. jjNwk >= nNwk-2 )then
      call logmsg('Writing '//str(f))
    elseif( jjNwk == 4 )then
      call logmsg('...')
    endif
    open(newunit=un, file=f, form='unformatted', access='sequential', status='replace')

    write(un) nwk%nWsys
    write(un) nwk%nCh
    write(un) nwk%nNode

    do jWsys = 1, nwk%nWsys
      wsys => nwk%wsys(jWsys)
      write(un) wsys%wsCode, wsys%leng
    enddo

    do iiCh = 1, nwk%nCh
      ch => nwk%channel(iiCh)

      write(un) len_trim(ch%wsCode), len_trim(ch%rvCode), len_trim(ch%rvName)
      write(un) ch%wsCode
      write(un) ch%rvCode
      write(un) ch%rvName
      write(un) ch%n
      write(un) ch%lon(:)
      write(un) ch%lat(:)
      write(un) ch%leng
      do jNode = 1, 2
        node => ch%node(jNode)
        nwknode => nwk%node(node%iNode)
        write(un) node%iNode, node%typ, node%elv, nwknode%downleng
      enddo  ! jNode/
      write(un) nwk%jCh(iiCh)  ! Index in old network
    enddo  ! iiCh/

    close(un)
  enddo  ! jNwk/

  call logext()

  call logent('JSON')

  do jjNwk = 1, nNwk
    jNwk = lst_jNwk(jjNwk)
    nwk => lst_nwk(jNwk)

    f = get_f_network_channel(nwk%uid, 'json')
    if( jjNwk <= 3 .or. jjNwk >= nNwk-2 )then
      call logmsg('Writing '//str(f))
    elseif( jjNwk == 4 )then
      call logmsg('...')
    endif
    open(newunit=un, file=f, status='replace')
    write(un,"(a)") '{'
    write(un,"(2x,a)") '"bbox": ['//str((/nwk%west,nwk%east/),'f16.11',', ')//&
                       ', '//str((/nwk%south,nwk%north/),'f15.11',', ')//'],'
    write(un,"(2x,a)") '"length": '//str(sum(nwk%wsys(:)%leng),'es15.8')//','
    write(un,"(2x,a)") '"num_channel": '//str(nwk%nCh)//','
    write(un,"(2x,a)") '"num_node": '//str(nwk%nNode)//','
    write(un,"(2x,a)") '"channel": ['
    do iiCh = 1, nwk%nCh
      ch => nwk%channel(iiCh)

      write(un,"(4x,a)")  '{'
      write(un,"(6x,a)")  '"index": '//str(iiCh)//','
      write(un,"(6x,a)")  '"wsCode": "'//ch%wsCode//'",'
      write(un,"(6x,a)")  '"rvCode": "'//ch%rvCode//'",'
      write(un,"(6x,a)")  '"rvName": "'//ch%rvName//'",'
      write(un,"(6x,a)")  '"bbox": ['//str((/ch%west,ch%east/),'f16.11',', ')//&
                          ', '//str((/ch%south,ch%north/),'f15.11',', ')//'],'
      write(un,"(6x,a)")  '"lon": ['//str(ch%lon,'f16.11',',')//'],'
      write(un,"(6x,a)")  '"lat": ['//str(ch%lat,'f15.11',',')//'],'
      write(un,"(6x,a)")  '"length": '//str(ch%leng,'es15.8')//','
      write(un,"(6x,a)")  '"node": ['
      do jNode = 1, 2
        node => ch%node(jNode)
        nwknode => nwk%node(node%iNode)
        write(un,"(8x,a)")  '{'
        write(un,"(10x,a)")   '"index": '//str(node%iNode)//','
        write(un,"(10x,a)")   '"type": '//str(node%typ)//','
        write(un,"(10x,a)")   '"elevation": '//str(node%elv,'es15.8')//','
        write(un,"(10x,a)")   '"distToMouth": '//str(nwknode%downleng,'es15.8')
        write(un,"(8x,a)")  '}'//comma_json(jNode,2)
      enddo
      write(un,"(6x,a)")  '],'
      write(un,"(6x,a)")  '"index_in_tmpnwk": '//str(nwk%jCh(iiCh))
      write(un,"(4x,a)")  '}'//comma_json(iiCh,nwk%nCh)
    enddo  ! jCh/
    write(un,"(2x,a)") '],'
    write(un,"(2x,a)") '"tmpuid": "'//str(nwk%uid_old)//'"'
    write(un,"(a)") '}'
    close(un)
  enddo  ! jNwk/

  deallocate(lst_jNwk)

  call logext()

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(lst_nwk)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine separateNetworks
!===============================================================
!
!===============================================================
subroutine separate_network(uid)
  use c2_strnk_io, only: &
        get_f_tmp_network_channel, &
        get_f_tmp_network_separation
  use c2_nlni_util, only: &
        is_wsCode_temporal
  use c3_jflw_const
  use c3_jflw_grid, only: &
        jflw_gxs_of_lon, &
        jflw_gxe_of_lon, &
        jflw_gys_of_lat, &
        jflw_gye_of_lat
  use mod_util, only: &
        jNode2jPt, &
        slonlat, &
        sBBox, &
        sMeshRange
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'separate_network'
  character(*), intent(in) :: uid

  type(network_) :: nwk
  type(channel_), pointer :: ch, ch2
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode
  type(wsys_), pointer :: wsys, wsys2
  type(conn_), pointer :: mtx_conn(:,:), conn
  type(conn_mem_), pointer :: cmem
  type(network_new_), pointer :: snetwork_tmp(:), snetwork_new(:), snwk, snwktmp
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_jCh(:), lst_jNode(:)
  integer, allocatable :: lst_wsCode_i(:)
  real(8), allocatable :: lst_chleng(:)
  integer, allocatable :: lst_nwkId(:)
  integer, allocatable :: lst_jWsys(:)
  real(8), allocatable :: lst_wsleng(:)
  integer, allocatable :: arg(:)
  integer :: jCh, jCh2
  integer :: iiCh, iiCh2
  integer :: jNode
  integer :: iNode
  integer :: jMem
  integer :: jWsys, jWsys2
  integer :: iiWsys
  integer :: nNwk, nNwk_tmp, jNwk, jNwk_tmp
  integer :: i, is, ie, iis, iie
  real(8) :: elv_mean, elv_min, elv_max
  integer :: n, nmax
  logical :: is_updated, is_finished
  real(8) :: leng_max

  character(CLEN_PATH) :: f
  integer :: un
  integer :: dgt_nCh, dgt_nNode, dgt_nWsys

  integer :: cl_wsCode, cl_rvCode, cl_rvName

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call jflw_set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  ! Read network data
  !-------------------------------------------------------------
  call logent('Reading network data')

  allocate(character(1) :: nwk%uid)
  nwk%uid = uid

  f = get_f_tmp_network_channel(nwk%uid, 'sbin')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, form='unformatted', access='sequential', &
       action='read', status='old')

  read(un) nwk%nCh, nwk%west, nwk%east, nwk%south, nwk%north

  allocate(nwk%channel(nwk%nCh))

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    read(un) !ch%west, ch%east, ch%south, ch%north

    read(un) cl_wsCode, cl_rvCode, cl_rvName
    allocate(character(cl_wsCode) :: ch%wsCode)
    allocate(character(cl_rvCode) :: ch%rvCode)
    allocate(character(cl_rvName) :: ch%rvName)
    read(un) ch%wsCode
    read(un) ch%rvCode
    read(un) ch%rvName

    ch%wsCode_i = int4_char(ch%wsCode)
    ch%is_wsCode_temporal = is_wsCode_temporal(ch%wsCode_i)
    if( ch%is_wsCode_temporal )then
      call logmsg('temp. wsCode: ch('//str(jCh)//')'//&
          ' wsCode: '//str(ch%wsCode)//&
          ' rvCode: '//str(ch%rvCode)//&
          ' rvName: '//str(ch%rvName))
    endif

    read(un) ch%n

    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))

    read(un) ch%lon
    read(un) ch%lat

    read(un) ch%leng

    allocate(ch%node(2))
    ch%node(1)%lon = ch%lon(1)
    ch%node(1)%lat = ch%lat(1)
    ch%node(2)%lon = ch%lon(ch%n)
    ch%node(2)%lat = ch%lat(ch%n)

    do jNode = 1, 2
      node => ch%node(jNode)
      read(un) node%typ, node%elv
    enddo
  enddo  ! jCh/

  close(un)

  dgt_nCh = dgt(nwk%nCh)

  nwk%gxs = jflw_gxs_of_lon(nwk%west)
  nwk%gxe = jflw_gxe_of_lon(nwk%east)
  nwk%gys = jflw_gys_of_lat(nwk%north)
  nwk%gye = jflw_gye_of_lat(nwk%south)

  call logext()
  !-------------------------------------------------------------
  ! Calc. connections of nodes
  !-------------------------------------------------------------
  call logent('Calculating connections among nodes')

  nwk%nNode = nwk%nCh * 2
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

  allocate(arg(nwk%nNode))
  call argsort(lst_lon, arg)
  call sort(lst_lon, arg)
  call sort(lst_lat, arg)
  call sort(lst_jCh, arg)
  call sort(lst_jNode, arg)
  nwk%nNode = 0
  ie = 0
  do while( ie < size(arg) )
    is = ie + 1
    ie = is
    do while( ie < size(arg) )
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

      call add(nwk%nNode)
      arg(nwk%nNode) = iie
    enddo  ! iis, iie/
  enddo  ! is, ie/

  allocate(nwk%node(nwk%nNode))
  dgt_nNode = dgt(nwk%nNode)

  iie = 0
  do iNode = 1, nwk%nNode
    iis = iie + 1
    iie = arg(iNode)

    nwknode => nwk%node(iNode)
    nwknode%lon = lst_lon(iis)
    nwknode%lat = lst_lat(iis)
    nwknode%gx = jflw_gxs_of_lon(nwknode%lon)
    nwknode%gy = jflw_gys_of_lat(nwknode%lat)

    nwknode%nCh = iie - iis + 1
    allocate(nwknode%jCh(nwknode%nCh))
    allocate(nwknode%jNode(nwknode%nCh))
    nwknode%jCh(:) = lst_jCh(iis:iie)
    nwknode%jNode(:) = lst_jNode(iis:iie)
    do iiCh = 1, nwknode%nCh
      jCh = nwknode%jCh(iiCh)
      jNode = nwknode%jNode(iiCh)
      node => nwk%channel(jCh)%node(jNode)
      node%iNode = iNode
    enddo  ! iiCh/

    nwknode%elv = node%elv
  enddo  ! iNode/

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

  call logext()
  !-------------------------------------------------------------
  ! Modify wsCode
  !-------------------------------------------------------------
  call logent('Modifying wsCode')

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    allocate(character(1) :: ch%wsCode_org)
    ch%wsCode_org = ch%wsCode
  enddo

  allocate(lst_wsCode_i(16))

  is_finished = .false.
  is_updated = .true.
  do while( .not. is_finished )
    if( .not. is_updated ) is_finished = .true.
    is_updated = .false.

    do jCh = 1, nwk%nCh
      ch => nwk%channel(jCh)

      n = 0
      nmax = 0
      do jNode = 1, 2
        node => ch%node(jNode)
        nwknode => nwk%node(node%iNode)
        call add(nmax, nwknode%nCh-1)
        do iiCh = 1, nwknode%nCh
          jCh2 = nwknode%jCh(iiCh)
          if( jCh2 == jCh ) cycle
          ch2 => nwk%channel(jCh2)
          if( ch2%wsCode_i /= ch%wsCode_i )then
            if( n == size(lst_wsCode_i) )then
              call errend('n == size(lst_wsCode_i)')
            endif
            call add(n)
            lst_wsCode_i(n) = ch2%wsCode_i
          endif
        enddo  ! iiCh/
      enddo  ! jNode/

      if( nmax == 0 ) cycle
      if( n == 0 ) cycle

      if( .not. ch%is_wsCode_temporal )then
        if( n /= nmax ) cycle
      endif

      if( all(lst_wsCode_i(:n) == lst_wsCode_i(1)) )then
        if( .not. is_wsCode_temporal(lst_wsCode_i(1)) )then
          call logmsg('Isolated wsCode '//str(ch%wsCode)//' of channel('//&
              str(jCh,dgt_nCh)//') was modified to '//str(lst_wsCode_i(1),-DGT_WSCODE))
          is_updated = .true.
          ch%wsCode_i = lst_wsCode_i(1)
          ch%wsCode = str(ch%wsCode_i,-DGT_WSCODE)
          ch%is_wsCode_temporal = is_wsCode_temporal(ch%wsCode_i)
        endif
      else
        if( is_finished )then
          call logmsg('Isolated channel ('//str(jCh,dgt_nCh)//&
              ') w/ wsCode '//str(ch%wsCode)//&
              ' in wsys '//str(lst_wsCode_i(:n),-DGT_WSCODE))
        endif
      endif
      !---------------------------------------------------------
    enddo  ! jCh/
  enddo  ! while .not. is_finished/

  deallocate(lst_wsCode_i)

  call logext()
  !-------------------------------------------------------------
  ! Make channel lists in water systems
  !-------------------------------------------------------------
  call logent('Making channel lists in water systems')

  allocate(lst_wsCode_i(nwk%nCh))
  allocate(lst_chleng(nwk%nCh))

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    lst_wsCode_i(jCh) = ch%wsCode_i
    lst_chleng(jCh) = ch%leng
  enddo
  allocate(arg(nwk%nCh))
  call argsort(lst_wsCode_i, arg)
  call sort(lst_wsCode_i, arg)
  call sort(lst_chleng, arg)
  deallocate(arg)

  nwk%nWsys = 0
  ie = 0
  do while( ie < nwk%nCh )
    is = ie + 1
    ie = is
    do while( ie < nwk%nCh )
      if( lst_wsCode_i(ie+1) /= lst_wsCode_i(is) ) exit
      ie = ie + 1
    enddo  ! ie/
    call add(nwk%nWsys)
    lst_wsCode_i(nwk%nWsys) = lst_wsCode_i(is)
    lst_chleng(nwk%nWsys) = sum(lst_chleng(is:ie))
  enddo  ! is, ie/
  call logmsg('Water systems: '//str(nwk%nWsys))

  dgt_nWsys = dgt(nwk%nWsys)

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    call search(ch%wsCode_i, lst_wsCode_i(:nwk%nWsys), ch%jWsys)
  enddo

  allocate(nwk%wsys(nwk%nWsys))
  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    wsys%wsCode_i = lst_wsCode_i(jWsys)
    wsys%wsCode = str(wsys%wsCode_i,-DGT_WSCODE)
    wsys%leng = lst_chleng(jWsys)
  enddo

  deallocate(lst_wsCode_i)
  deallocate(lst_chleng)

  nwk%wsys(:)%nCh = 0
  do jCh = 1, nwk%nCh
    call add(nwk%wsys(nwk%channel(jCh)%jWsys)%nCh)
  enddo

  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    if( wsys%nCh == 0 ) cycle
    allocate(wsys%jCh(wsys%nCh))
  enddo

  nwk%wsys(:)%nCh = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    wsys => nwk%wsys(ch%jWsys)
    call add(wsys%nCh)
    wsys%jCh(wsys%nCh) = jCh
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Find connections among water systems
  !-------------------------------------------------------------
  call logent('Finding connections among water systems')

  allocate(mtx_conn(nwk%nWsys, nwk%nWsys))
  do jWsys = 1, nwk%nWsys
    do jWsys2 = jWsys+1, nwk%nWsys
      conn => mtx_conn(jWsys, jWsys2)
      conn%n = 0
      allocate(conn%mem(2))
      conn%is_removed = .false.
    enddo
  enddo

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)

    do jNode = 1, 2
      node => ch%node(jNode)
      nwknode => nwk%node(node%iNode)
      do iiCh = 1, nwknode%nCh
        jCh2 = nwknode%jCh(iiCh)
        if( jCh2 == jCh ) cycle
        ch2 => nwk%channel(jCh2)
        if( ch%jWsys >= ch2%jWsys ) cycle

        conn => mtx_conn(ch%jWsys, ch2%jWsys)
        if( conn%n == size(conn%mem) )then
          call realloc_conn_mem(conn%mem, conn%n*2)
        endif
        call add(conn%n)
        cmem => conn%mem(conn%n)
        cmem%jCh = jCh
        cmem%jCh2 = jCh2
        cmem%iNode = node%iNode
      enddo  ! iiCh/
    enddo  ! jNode/
  enddo  ! jCh/

  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    do jWsys2 = jWsys+1, nwk%nWsys
      wsys2 => nwk%wsys(jWsys2)
      conn => mtx_conn(jWsys, jWsys2)
      if( conn%n == 0 ) cycle

      call logmsg('wsys '//str(wsys%wsCode)//' - '//str(wsys2%wsCode)//&
          ' conn: '//str(conn%n))
      call logmsg('  wsys '//str(wsys%wsCode)//' leng: '//str(wsys%leng))
      call logmsg('  wsys '//str(wsys2%wsCode)//' leng: '//str(wsys2%leng))

      elv_mean = 0.d0
      elv_min = 1d20
      elv_max = -1d20
      do jMem = 1, conn%n
        cmem => conn%mem(jMem)
        nwknode => nwk%node(cmem%iNode)
        call add(elv_mean, nwknode%elv)
        elv_min = min(elv_min, nwknode%elv)
        elv_max = max(elv_max, nwknode%elv)
      enddo  ! jMem/
      elv_mean = elv_mean / conn%n
      call logmsg('  elv mean: '//str(elv_mean)//&
          ', min: '//str(elv_min)//', max: '//str(elv_max))

      if( elv_min > THRESH_ELV_DISCONNECT )then
        call logmsg('  REMOVED')
        conn%is_removed = .true.
      endif

    enddo  ! jWsys2/
  enddo  ! jWsys/

  call logext()
  !-------------------------------------------------------------
  ! Separate original networks
  !-------------------------------------------------------------
  call logent('Separating original networks')

  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    wsys%jNwk = jWsys
  enddo

  is_updated = .true.
  do while( is_updated )
    is_updated = .false.
    do jWsys = 1, nwk%nWsys
      wsys => nwk%wsys(jWsys)
      do jWsys2 = jWsys+1, nwk%nWsys
        wsys2 => nwk%wsys(jWsys2)
        conn => mtx_conn(jWsys, jWsys2)
        if( conn%n == 0 ) cycle
        if( conn%is_removed ) cycle

        if( wsys2%jNwk == wsys%jNwk ) cycle
        is_updated = .true.
        wsys%jNwk  = min(wsys%jNwk, wsys2%jNwk)
        wsys2%jNwk = wsys%jNwk
      enddo  ! jWsys2/
    enddo  ! jWsys/
  enddo  ! while is_updated/

  nNwk_tmp = 0
  allocate(lst_nwkId(nwk%nWsys))
  lst_nwkId(:) = nwk%wsys(:)%jNwk
  call sort(lst_nwkId)
  ie = 0
  do while( ie < nwk%nWsys )
    is = ie + 1
    ie = is
    do while( ie < nwk%nWsys )
      if( lst_nwkId(ie+1) /= lst_nwkId(is) ) exit
      ie = ie + 1
    enddo
    call add(nNwk_tmp)
    lst_nwkId(nNwk_tmp) = lst_nwkId(is)
  enddo
  call logmsg('New networks (tmp): '//str(nNwk_tmp))

  allocate(snetwork_tmp(nNwk_tmp))
  do jNwk = 1, nNwk_tmp
    snwk => snetwork_tmp(jNwk)
    snwk%nWsys = 0
    allocate(snwk%jWsys(8))
  enddo  ! jNwk/

  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    call search(wsys%jNwk, lst_nwkId(:nNwk_tmp), i)
    wsys%jNwk = i

    snwk => snetwork_tmp(wsys%jNwk)
    if( snwk%nWsys == size(snwk%jWsys) )then
      call realloc(snwk%jWsys, snwk%nWsys*2, clear=.false.)
    endif
    call add(snwk%nWsys)
    snwk%jWsys(snwk%nWsys) = jWsys
  enddo  ! jNwk/

  do jNwk = 1, nNwk_tmp
    snwk => snetwork_tmp(jNwk)
    call logmsg('newnwk_tmp('//str(jNwk)//') nWsys: '//str(snwk%nWsys))
    do iiWsys = 1, snwk%nWsys
      call logmsg('  wsys '//nwk%wsys(snwk%jWsys(iiWsys))%wsCode)
    enddo  ! iiWsys/
  enddo  ! jNwk/

  deallocate(lst_nwkId)

  call logext()
  !-------------------------------------------------------------
  ! Separate new networks that are actually not connected
  !-------------------------------------------------------------
  call logent('Separating new networks')

  nNwk = 0
  do jNwk = 1, nNwk_tmp
    snwk => snetwork_tmp(jNwk)

    snwk%nCh = 0
    do iiWsys = 1, snwk%nWsys
      jWsys = snwk%jWsys(iiWsys)
      wsys => nwk%wsys(jWsys)
      call add(snwk%nCh, wsys%nCh)
    enddo  ! iiWsys/

    allocate(snwk%jCh(snwk%nCh))

    snwk%nCh = 0
    do iiWsys = 1, snwk%nWsys
      jWsys = snwk%jWsys(iiWsys)
      wsys => nwk%wsys(jWsys)
      do iiCh = 1, wsys%nCh
        call add(snwk%nCh)
        snwk%jCh(snwk%nCh) = wsys%jCh(iiCh)
      enddo  ! iiCh/
    enddo  ! iiWsys/

    snwk%nNode = snwk%nCh * 2
    allocate(lst_lon(snwk%nNode))
    allocate(lst_lat(snwk%nNode))
    allocate(lst_jCh(snwk%nNode))
    allocate(lst_jNode(snwk%nNode))

    iNode = 0
    do iiCh = 1, snwk%nCh
      jCh = snwk%jCh(iiCh)
      ch => nwk%channel(jCh)
      do jNode = 1, 2
        node => ch%node(jNode)
        call add(iNode)
        lst_lon(iNode) = node%lon
        lst_lat(iNode) = node%lat
        lst_jCh(iNode) = jCh
        lst_jNode(iNode) = jNode
      enddo  ! jNode/
    enddo  ! iiCh/

    allocate(arg(snwk%nNode))
    call argsort(lst_lon, arg)
    call sort(lst_lon, arg)
    call sort(lst_lat, arg)
    call sort(lst_jCh, arg)
    call sort(lst_jNode, arg)
    snwk%nNode = 0
    ie = 0
    do while( ie < size(arg) )
      is = ie + 1
      ie = is
      do while( ie < size(arg) )
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
        call add(snwk%nNode)
        arg(snwk%nNode) = iie
      enddo
    enddo

    allocate(snwk%node(snwk%nNode))

    iie = 0
    do iNode = 1, snwk%nNode
      iis = iie + 1
      iie = arg(iNode)
      nwknode => snwk%node(iNode) 
      nwknode%lon = lst_lon(iis)
      nwknode%lat = lst_lat(iis)
      nwknode%nCh = iie - iis + 1
      allocate(nwknode%jCh(nwknode%nCh))
      allocate(nwknode%jNode(nwknode%nCh))
      nwknode%jCh(:) = lst_jCh(iis:iie)
      nwknode%jNode(:) = lst_jNode(iis:iie)
      do i = iis, iie
        jCh = lst_jCh(i)
        jNode = lst_jNode(i)
        node => nwk%channel(jCh)%node(jNode)
        node%iNode = iNode
      enddo  ! i/
    enddo  ! iNode/

    deallocate(lst_lon)
    deallocate(lst_lat)
    deallocate(lst_jCh)
    deallocate(lst_jNode)
    deallocate(arg)

    do iNode = 1, snwk%nNode
      nwknode => snwk%node(iNode)
      allocate(nwknode%iNode(nwknode%nCh))
      do iiCh = 1, nwknode%nCh
        jCh = nwknode%jCh(iiCh)
        jNode = nwknode%jNode(iiCh)
        nwknode%iNode(iiCh) = nwk%channel(jCh)%node(3-jNode)%iNode
      enddo  ! iCh/
    enddo  ! iNode/

    ! Put same network IDs to connected channels
    do iiCh = 1, snwk%nCh
      nwk%channel(snwk%jCh(iiCh))%nwkId = iiCh
    enddo

    is_updated = .true.
    do while( is_updated )
      is_updated = .false.
      do iiCh = 1, snwk%nCh
        ch => nwk%channel(snwk%jCh(iiCh))
        do jNode = 1, 2
          node => ch%node(jNode)
          nwknode => snwk%node(node%iNode)
          do iiCh2 = 1, nwknode%nCh
            jCh2 = nwknode%jCh(iiCh2)
            if( jCh2 == jCh ) cycle
            ch2 => nwk%channel(jCh2)
            if( ch2%nwkId == ch%nwkId ) cycle

            if( ch%nwkId < ch2%nwkId )then
              ch2%nwkId = ch%nwkId
            else
              ch%nwkId = ch2%nwkId
            endif
            is_updated = .true.
          enddo  ! iiCh2/
        enddo  ! jNode/
      enddo  ! iiCh/
    enddo  ! while is_updated/

    allocate(arg(snwk%nCh))
    n = 1
    arg(1) = nwk%channel(snwk%jCh(1))%nwkId
    do iiCh = 2, snwk%nCh
      ch => nwk%channel(snwk%jCh(iiCh))
      if( any(arg(:n) == ch%nwkId) ) cycle
      call add(n)
      arg(n) = ch%nwkId
    enddo  ! iiCh/

    if( n == 1 )then
      call add(nNwk)
      do iiCh = 1, snwk%nCh
        nwk%channel(snwk%jCh(iiCh))%nwkId = nNwk
      enddo
      deallocate(arg)
      cycle
    endif

    call logmsg('newnwk('//str(jNwk)//') is divided into '//str(n))

    call sort(arg(:n))

    do iiCh = 1, snwk%nCh
      ch => nwk%channel(snwk%jCh(iiCh))
      call search(ch%nwkId, arg(:n), i)
      ch%nwkId = nNwk + i
    enddo  ! iNode/
    call add(nNwk, n)

    deallocate(arg)
  enddo  ! jNwk/

  call logmsg('New networks: '//str(nNwk))

  if( nNwk == nNwk_tmp )then
    snetwork_new => snetwork_tmp
  else
    allocate(snetwork_new(nNwk))

    snetwork_new(:)%nCh = 0
    do jNwk_tmp = 1, nNwk_tmp
      snwktmp => snetwork_tmp(jNwk_tmp)
      do iiCh = 1, snwktmp%nCh
        jNwk = nwk%channel(snwktmp%jCh(iiCh))%nwkId
        call add(snetwork_new(jNwk)%nCh)
      enddo  ! iiCh/
    enddo  ! jNwk_tmp/

    do jNwk = 1, nNwk
      snwk => snetwork_new(jNwk)
      allocate(snwk%jCh(snwk%nCh))
      nullify(snwk%jWsys)
      nullify(snwk%wsleng)
      nullify(snwk%node)
      snwk%nCh = 0
    enddo

    do jNwk_tmp = 1, nNwk_tmp
      snwktmp => snetwork_tmp(jNwk_tmp)
      do iiCh = 1, snwktmp%nCh
        jNwk = nwk%channel(snwktmp%jCh(iiCh))%nwkId
        snwk => snetwork_new(jNwk)
        call add(snwk%nCh)
        snwk%jCh(snwk%nCh) = snwktmp%jCh(iiCh)
      enddo  ! iiCh/
    enddo  ! jNwk_tmp/

    deallocate(snetwork_tmp)
  endif

  call logext()
  !-------------------------------------------------------------
  ! Make new network data
  !-------------------------------------------------------------
  call logent('Making new network data')

  ! Sort by length
  do jNwk = 1, nNwk
    snwk => snetwork_new(jNwk)

    allocate(lst_jWsys(snwk%nCh))
    allocate(arg(snwk%nCh))
    do iiCh = 1, snwk%nCh
      lst_jWsys(iiCh) = nwk%channel(snwk%jCh(iiCh))%jWsys
    enddo  ! iiCh/
    call sort(lst_jWsys)
    snwk%nWsys = 0
    ie = 0
    do while( ie < snwk%nCh )
      is = ie + 1
      ie = is
      do while( ie < snwk%nCh )
        if( lst_jWsys(ie+1) /= lst_jWsys(is) ) exit
        ie = ie + 1
      enddo
      call add(snwk%nWsys)
      arg(snwk%nWsys) = ie
    enddo

    call logmsg('newnwk('//str(jNwk)//') nWsys: '//str(snwk%nWsys))

    do iiWsys = 1, snwk%nWsys
      lst_jWsys(iiWsys) = lst_jWsys(arg(iiWsys))
    enddo  ! iiWsys/

    deallocate(arg)

    allocate(lst_wsleng(snwk%nWsys))
    lst_wsleng(:) = 0.d0
    do iiCh = 1, snwk%nCh
      ch => nwk%channel(snwk%jCh(iiCh))
      call search(ch%jWsys, lst_jWsys(:snwk%nWsys), iiWsys)
      call add(lst_wsleng(iiWsys), ch%leng)
    enddo  ! iiCh/

    allocate(arg(snwk%nWsys))
    call argsort(lst_wsleng, arg)
    call reverse(arg)
    call sort(lst_jWsys, arg)
    call sort(lst_wsleng, arg)
    deallocate(arg)

    call realloc(snwk%jWsys, snwk%nWsys, clear=.true.)
    allocate(snwk%wsleng(snwk%nWsys))
    snwk%jWsys(:) = lst_jWsys(:snwk%nWsys)
    snwk%wsleng(:) = lst_wsleng(:)

    deallocate(lst_jWsys)
    deallocate(lst_wsleng)

    do iiWsys = 1, snwk%nWsys
      jWsys = snwk%jWsys(iiWsys)
      wsys => nwk%wsys(jWsys)
      call logmsg('  wsys '//str(wsys%wsCode)//&
          ' leng: '//str(snwk%wsleng(iiWsys)))
    enddo  ! iiWsys/

    leng_max = maxval(snwk%wsleng(:))
    if( count(snwk%wsleng(:) == leng_max) == 1 )then
      iiWsys = maxloc(snwk%wsleng,1)
    else
      n = 0
      do iiWsys = 1, snwk%nWsys
        wsys => nwk%wsys(snwk%jWsys(iiWsys))
        if( is_wsCode_temporal(wsys%wsCode_i) ) cycle
        if( snwk%wsleng(iiWsys) /= leng_max ) cycle
        call add(n)
      enddo  ! iiWsys/

      !---------------------------------------------------------
      ! Case: All wsCode are temporal
      if( n == 0 )then
        iiWsys = minloc(snwk%jWsys, 1, mask=snwk%wsleng==leng_max)
      !---------------------------------------------------------
      ! Case: Non-temporal wsCode was found
      else
        do iiWsys = 1, snwk%nWsys
          wsys => nwk%wsys(snwk%jWsys(iiWsys))
          if( is_wsCode_temporal(wsys%wsCode_i) ) cycle
          if( snwk%wsleng(iiWsys) /= leng_max ) cycle
          exit
        enddo  ! iiWsys/
      endif
    endif
    snwk%id = nwk%wsys(snwk%jWsys(iiWsys))%wsCode_i
  enddo  ! jNwk/

  call logext()
  !-------------------------------------------------------------
  ! Output info. of separation
  !-------------------------------------------------------------
  call logent('Outputting info. of separation')

  f = get_f_tmp_network_separation(uid)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')

  write(un,"(1x,a)") 'New_networks '//str(nNwk)

  write(un,"(1x,a)") '--- Length of each wsCode'
  do jNwk = 1, nNwk
    snwk => snetwork_new(jNwk)
    write(un,"(1x,a)") 'Network('//str(jNwk)//') wsys '//str(snwk%nWsys)
    do iiWsys = 1, snwk%nWsys
      jWsys = snwk%jWsys(iiWsys)
      wsys => nwk%wsys(jWsys)
      write(un,"(3x,a)") wsys%wsCode//' '//str(snwk%wsleng(iiWsys))
    enddo  ! iiWsys/
  enddo  ! jNwk/

  write(un,"(1x,a)") '--- Channel indices'
  do jNwk = 1, nNwk
    snwk => snetwork_new(jNwk)
    write(un,"(1x,a)") 'Network('//str(jNwk)//') ch '//str(snwk%nCh)
    write(un,"(2x,"//str(snwk%nCh)//"(1x,i0))") snwk%jCh(:)
  enddo  ! jNwk/

  write(un,"(1x,a)") '--- Pairs of connected wsys '//str(count(mtx_conn(:,:)%n > 0))
  n = 0
  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    do jWsys2 = jWsys+1, nwk%nWsys
      wsys2 => nwk%wsys(jWsys2)
      conn => mtx_conn(jWsys, jWsys2)
      if( conn%n == 0 ) cycle
      call add(n)

      write(un,"(1x,a)") 'Pair('//str(n)//') wsCode '//wsys%wsCode//' '//wsys2%wsCode
      write(un,"(3x,a)") 'connections '//str(conn%n)//' is_separated '//str(conn%is_removed)
      write(un,"(3x,a)") 'lon lat elv'
      do jMem = 1, conn%n
        cmem => conn%mem(jMem)
        nwknode => nwk%node(cmem%iNode)
        write(un,"(5x,a)") &
            str((/nwknode%lon,nwknode%lat/),'es18.11')//' '//str(nwknode%elv,'es10.3')
      enddo  ! jMem/
    enddo  ! jWsys2/
  enddo  ! jWsys/

  close(un)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( nNwk == nNwk_tmp )then
    nullify(snetwork_new)
    deallocate(snetwork_tmp)
  else
    deallocate(snetwork_new)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine separate_network
!===============================================================
!
!===============================================================
subroutine set_separated_network_ids(lst_nwk, lst_jNwk)
  use c2_nlni_const, only: &
        DIV_WSCODE_RVUNKNOWN, &
        DGT_WSCODE
  use c2_strnk_const, only: &
        DGT_NWK_SAME_WSCODE, &
        DGT_NWKUID
  implicit none
  character(CLEN_PROC) :: PRCNAM = 'set_separated_network_ids'
  type(network_), intent(inout), target :: lst_nwk(:)
  integer(4), intent(out) :: lst_jNwk(:)

  type(network_), pointer :: nwk
  integer :: nNwk, jNwk, jjNwk
  character(1) :: uid_head
  integer(4), allocatable :: lst_wsCode_i(:)
  real(8)   , allocatable :: lst_wsLeng(:)
  integer(8), allocatable :: lst_uid_i(:)
  integer(4), allocatable :: arg(:)
  integer :: is, ie, i

  integer, parameter :: MUL_WSCODE = 10 ** DGT_NWK_SAME_WSCODE
  integer, parameter :: NLIM_SAME_WSCODE = 10 ** (DGT_NWK_SAME_WSCODE) - 1
  integer, parameter :: DGT_NWKGID_BODY = DGT_NWKUID - 1

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Make new IDs
  !-------------------------------------------------------------
  call logent('Making new ids')

  nNwk = size(lst_nwk)

  allocate(lst_wsCode_i(nNwk))
  allocate(lst_wsLeng(nNwk))
  do jNwk = 1, nNwk
    nwk => lst_nwk(jNwk)
    lst_jNwk(jNwk) = jNwk
    lst_wsCode_i(jNwk) = nwk%wsys(1)%wsCode_i
    lst_wsLeng(jNwk) = nwk%wsys(1)%leng
    allocate(character(1) :: nwk%uid)
  enddo  ! jNwk/

  allocate(arg(nNwk))
  call argsort(lst_wsCode_i, arg)
  call sort(lst_jNwk, arg)
  call sort(lst_wsCode_i, arg)
  call sort(lst_wsLeng, arg)

  ie = 0
  do while( ie < nNwk )
    is = ie + 1
    ie = is
    do while( ie < nNwk )
      if( lst_wsCode_i(ie+1) /= lst_wsCode_i(is) ) exit
      ie = ie + 1
    enddo

    if( ie - is + 1 > NLIM_SAME_WSCODE )then
      call errend('More than '//str(NLIM_SAME_WSCODE)//&
          ' networks with same wsCode '//str(lst_wsCode_i(is)))
    endif

    call argsort(lst_wsLeng(is:ie), arg(is:ie))
    call reverse(arg(is:ie))
    call sort(lst_jNwk(is:ie), arg(is:ie))
    call sort(lst_wsLeng(is:ie), arg(is:ie))

    call logmsg('wsCode: '//str(lst_wsCode_i(is),-DGT_WSCODE)//&
                ' n: '//str(ie-is+1))

    if( mod(lst_wsCode_i(is), DIV_WSCODE_RVUNKNOWN) == 0 )then
      uid_head = '1'
    else
      uid_head = '0'
    endif

    do i = is, ie
      jNwk = lst_jNwk(i)
      nwk => lst_nwk(jNwk)
      nwk%uid = uid_head//str(lst_wsCode_i(i) * MUL_WSCODE + (i - is + 1), -DGT_NWKGID_BODY)
      !if( i == is .or. i == ie )then
      !  call logmsg(nwk%wsys(1)%wsCode//' -> '//nwk%uid, opt='+x2')
      !elseif( i == is+1 )then
      !  call logmsg('...', opt='+x2')
      !endif
    enddo
  enddo  ! is, ie/

  deallocate(lst_wsCode_i)
  deallocate(lst_wsLeng)
  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  ! Sort and check duplication
  !-------------------------------------------------------------
  call logent('Sorting and checking duplication of uid')

  allocate(lst_uid_i(nNwk))
  do jNwk = 1, nNwk
    lst_uid_i(jNwk) = int8_char(lst_nwk(jNwk)%uid)
  enddo  ! jNwk/
  call argsort(lst_uid_i, lst_jNwk)
  do jjNwk = 1, nNwk-1
    if( lst_uid_i(lst_jNwk(jjNwk+1)) == lst_uid_i(lst_jNwk(jjNwk)) )then
      call errend('Duplicated uid: '//str(lst_uid_i(lst_jNwk(jjNwk))))
    endif
  enddo  ! jNwk/
  deallocate(lst_uid_i)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_separated_network_ids
!===============================================================
!
!===============================================================
subroutine calc_distance_to_river_mouth(nwk)
  use c2_strnk_const
  use c2_strnk_io, only: &
    get_f_network_channel
  use c2_jflw_const, &
    jflw_set_resolution => set_resolution
  use c2_jflw_grid, only: &
    gxs_of_lon, &
    gys_of_lat
  use mod_util, only: &
    jNode2jPt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_distance_to_river_mouth'
  type(network_), intent(inout) :: nwk

  type(channel_), pointer :: ch, ch2
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode, nwknode2
  integer :: n_updated, n_updated_next
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_jCh(:), lst_jNode(:)
  integer, allocatable :: arg(:)
  integer, allocatable :: iNode_updated(:)
  integer, allocatable :: iNode_updated_next(:)
  logical, allocatable :: is_updated_next(:)
  integer :: jCh, jCh2
  integer :: iiCh
  integer :: jNode
  integer :: iNode, iNode2
  integer :: jPt
  integer :: i
  integer :: is, ie, iis, iie
  logical :: is_outlet_exist

  call logbgn(PRCNAM, MODNAM)
  call logmsg('Network '//str(nwk%uid))
  !-------------------------------------------------------------
  ! Prepare static data
  !-------------------------------------------------------------
  call logent('Preparing static data')

  call logmsg('Water systems: '//str(nwk%nWsys))
  call logmsg('Channels: '//str(nwk%nCh))

  ! Make a list of nodes
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

      nwknode%typ = nwk%channel(lst_jCh(iis))%node(lst_jNode(iis))%typ

      do iiCh = 1, nwknode%nCh
        jCh = nwknode%jCh(iiCh)
        jNode = nwknode%jNode(iiCh)

        node => nwk%channel(jCh)%node(jNode)

        node%iNode = iNode

        if( node%typ /= nwknode%typ )then
          call errend(msg_unexpected_condition()//&
            '\n  node%typ /= nwknode%typ')
        endif
      enddo  ! iiCh/
    enddo  ! iis, iie/
  enddo  ! is, ie/

  nwk%nNode = iNode
  call logmsg('Nodes: '//str(nwk%nNode))

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

  call logext()
  !-------------------------------------------------------------
  ! Check if outlet exists
  !-------------------------------------------------------------
  call logent('Searching for outlet')

  is_outlet_exist = .false.
  loop_find_outlet: &
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    do jNode = 1, 2
      if( ch%node(jNode)%typ == NODETYPE_NEW__OUT )then
        is_outlet_exist = .true.
        exit loop_find_outlet
      endif
    enddo
  enddo &
  loop_find_outlet

  if( .not. is_outlet_exist )then
    call logmsg('Outlet was not found.')

    do iNode = 1, nwk%nNode
      nwknode => nwk%node(iNode)
      nwknode%downleng = -9999.d0
    enddo  ! iNode/

    call logret(PRCNAM, MODNAM)
    return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Calc. dist. to river mouth
  !-------------------------------------------------------------
  call logent('Calculating distance from nodes to river mouth')

  ! Initialize
  nwk%node(1)%downleng = sum(nwk%channel(:)%leng) * 2.d0
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    nwknode%downleng = nwk%node(1)%downleng
    nwknode%iNode_down = 0
  enddo

  allocate(is_updated_next(nwk%nNode))
  allocate(iNode_updated(nwk%nNode))
  allocate(iNode_updated_next(nwk%nNode))

  ! Set leng. for outlets (leng = 0)
  n_updated = 0
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    if( nwknode%typ /= NODETYPE_NEW__OUT ) cycle
    call add(n_updated)
    iNode_updated(n_updated) = iNode
    nwknode%downleng = 0.d0
  enddo

  !call logmsg('Outlets: '//str(n_updated))

  ! Find shortest way to outlet
  is_updated_next(:) = .false.
  do while( n_updated > 0 )
    n_updated_next = 0
    do i = 1, n_updated
      iNode = iNode_updated(i)
      nwknode => nwk%node(iNode)
      do iiCh = 1, nwknode%nCh
        jCh2 = nwknode%jCh(iiCh)
        ch2 => nwk%channel(jCh2)
        iNode2 = nwknode%iNode(iiCh)
        nwknode2 => nwk%node(iNode2)
        if( nwknode%downleng + ch2%leng < nwknode2%downleng )then
          is_updated_next(iNode2) = .true.
          call add(n_updated_next)
          iNode_updated_next(n_updated_next) = iNode2
          nwknode2%downleng = nwknode%downleng + ch2%leng
          nwknode2%iNode_down = iNode
        endif
      enddo
    enddo  ! i/

    !call logmsg('Updated: '//str(n_updated_next))
  
    ! Copy list of updated nodes avoiding duplication
    n_updated = 0
    do i = 1, n_updated_next
      if( is_updated_next(iNode_updated_next(i)) )then
        is_updated_next(iNode_updated_next(i)) = .false.
        call add(n_updated)
        iNode_updated(n_updated) = iNode_updated_next(i)
      endif
    enddo
  enddo  ! while n_updated > 0

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    if( nwknode%typ == NODETYPE_NEW__OUT ) cycle

    if( nwknode%iNode_down == 0 )then
      call errend('%iNode_down is missing: node('//str(iNode)//')'//&
        '\njCh: '//str(nwknode%jCh(:))//&
        '\niNode: '//str(iNode)//&
        '\ntyp: '//str(nwknode%typ))
    endif

    nwknode%iCh_down = 0
    do iiCh = 1, nwknode%nCh
      if( nwknode%iNode(iiCh) == nwknode%iNode_down )then
        nwknode%iCh_down = iiCh
      endif
    enddo
    if( nwknode%iCh_down == 0 )then
      call errend('%iNode_down was not found in %iNode(:).'//&
        '\niNode_down: '//str(nwknode%iNode_down)//&
        '\niNode: '//str(iNode)//&
        '\ntyp: '//str(nwknode%typ))
    endif
  enddo  ! iNode/

  call logmsg('Max: '//str(maxval(nwk%node(:)%downleng)))

  deallocate(is_updated_next)
  deallocate(iNode_updated)
  deallocate(iNode_updated_next)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calc_distance_to_river_mouth
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
subroutine makeModelNetworkData(name_leng, leng_standard, uid_in)
  use c2_strnk_const, only: &
     DGT_NWKUID
  use c2_strnk_io, only: &
    get_f_lst_networks_channel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeModelNetworkData'
  character(*), intent(in) :: name_leng
  real(8), intent(in) :: leng_standard
  character(*), intent(in) :: uid_in

  character(DGT_NWKUID) :: uid
  integer :: nNwk, jNwk

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uid_in == '' )then
    f = get_f_lst_networks_channel()
    open(newunit=un, file=f, status='old')
    read(un,*) c_, nNwk
    read(un,*)
    do jNwk = 1, nNwk
      read(un,*) c_, uid
      call make_model_network_data(name_leng, leng_standard, uid)
    enddo  ! jNwk/
    close(un)
  else
    call make_model_network_data(name_leng, leng_standard, uid_in)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeModelNetworkData
!===============================================================
!
!===============================================================
subroutine make_model_network_data(name_leng, leng_standard, uid)
  use c2_strnk_const
  use c2_strnk_io, only: &
    get_f_network_channel, &
    get_f_network_model
  use mod_util, only: &
    slonlat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_model_network_data'
  character(*), intent(in) :: name_leng
  real(8), intent(in) :: leng_standard  ! [m]
  character(*), intent(in) :: uid

  type edge_
    real(8) :: pos
    logical :: is_outlet
    real(8) :: lon, lat
  end type

  type div_
    integer :: n
    integer, pointer :: jPt(:)
    type(edge_) :: edge(2)
    real(8), pointer :: lon(:), lat(:)
  end type

  type chdiv_
    integer :: n
    type(div_), pointer :: div(:)
  end type

  type(network_) :: nwk
  type(wsys_), pointer :: wsys
  type(channel_), pointer :: ch
  type(node_), pointer :: node
  integer :: clen_wsCode, clen_rvCode, clen_rvName
  type(chdiv_), pointer :: lst_chdiv(:), chdiv
  type(div_), pointer :: div
  type(edge_), pointer :: edge
  integer :: iDiv
  real(8) :: leng_section, leng_sum, leng_next
  real(8) :: pos, pos_prev
  integer :: jWsys
  integer :: jCh
  integer :: iCh
  integer :: jNode
  integer :: jPt, jjPt

  character(CLEN_PATH) :: f
  integer :: un

  real(8), parameter :: LENG_NEXT_LLIM = 1d-8

logical :: debug_this

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nwk%uid = uid

  f = get_f_network_channel(nwk%uid, 'sbin')
  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')

  read(un) nwk%nWsys
  read(un) nwk%nCh
  read(un) nwk%nNode

  allocate(nwk%wsys(nwk%nWsys))
  allocate(nwk%channel(nwk%nCh))

  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    read(un) wsys%wsCode, wsys%leng
  enddo

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)

    read(un) clen_wsCode, clen_rvCode, clen_rvName
    allocate(character(clen_wsCode) :: ch%wsCode)
    allocate(character(clen_rvCode) :: ch%rvCode)
    allocate(character(clen_rvName) :: ch%rvName)
    read(un) ch%wsCode
    read(un) ch%rvCode
    read(un) ch%rvName

    read(un) ch%n
    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))
    read(un) ch%lon(:)
    read(un) ch%lat(:)

    read(un) ch%leng

    allocate(ch%node(2))
    do jNode = 1, 2
      node => ch%node(jNode)
      read(un) node%iNode, node%typ, node%elv, node%downleng
    enddo  ! jNode/
    read(un) ! Index in old network
  enddo  ! jCh/

  close(un)
  !-------------------------------------------------------------
  ! Divide channels
  !-------------------------------------------------------------
  call logent('Dividing channels into suitable length of sections')

  allocate(lst_chdiv(nwk%nCh))

  do jCh = 1, nwk%nCh
!print*, jCh
debug_this = jCh == 49

    ch => nwk%channel(jCh)

    chdiv => lst_chdiv(jCh)
    !-----------------------------------------------------------
    ! Calc. number of division
    !-----------------------------------------------------------
    chdiv%n = int(ch%leng / leng_standard)

    if( chdiv%n == 0 )then
      chdiv%n = 1
    else
      if( abs(ch%leng / chdiv%n - leng_standard) > abs(ch%leng / (chdiv%n+1) - leng_standard) )then
        call add(chdiv%n)
      endif
    endif

    leng_section = ch%leng / chdiv%n
if( debug_this )then
    call logmsg('('//str(jCh)//') nPt '//str(ch%n)//' leng '//str(ch%leng)//&
      ' nDiv '//str(chdiv%n)//' leng_section '//str(leng_section))
endif

    allocate(chdiv%div(chdiv%n))
    do iDiv = 1, chdiv%n
      div => chdiv%div(iDiv)
      div%n = 0
      allocate(div%jPt(ch%n))
    enddo  ! iDiv/
    !-----------------------------------------------------------
    ! Determine points where the channel is divided
    !-----------------------------------------------------------
    iDiv = 0
    jPt = 1
    pos = 0.d0
    do while( jPt < ch%n )
      call add(iDiv)
      div => chdiv%div(iDiv)
      div%n = 1
      div%jPt(1) = jPt
      div%edge(1)%pos = pos
      leng_sum = 0.d0

      do while( jPt < ch%n )
        leng_next = dist_sphere(&
          ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r &
        ) * EARTH_R
if( debug_this )then
  call logmsg(str(jPt)//' pos '//str(pos,'f5.3')//&
    ' sum '//str(leng_sum)//' next '//str(leng_next)//&
    ' sum+next*(1-pos) '//str(leng_sum+leng_next*(1-pos)))
endif
        if( leng_sum + leng_next*(1.d0-pos) > leng_section ) exit
        call add(leng_sum, leng_next*(1.d0-pos))
        call add(jPt)
        pos = 0.d0
        call add(div%n)
        div%jPt(div%n) = jPt
      enddo  ! jPt/

      pos_prev = pos
      if( leng_next < LENG_NEXT_LLIM )then
        pos = 1.d0
      else
        pos = min(pos + min(max(leng_section - leng_sum, 0.d0) / leng_next, 1.d0), 1.d0)
      endif

      if( pos < 1d-8 )then
        pos = 0.d0
      elseif( 1.d0 - pos < 1d-8 )then
        pos = 1.d0
      endif

      div%edge(2)%pos = pos

if( debug_this )then
      !if( abs((leng_sum + leng_next*(pos-pos_prev)) - leng_section) > 1d-2 )then
        call logmsg('('//str(jCh)//') '//str(jPt)//&
          ' pos '//str(pos,'f5.3')//' pos_prev '//str(pos_prev,'f5.3')//&
          ' sum '//str(leng_sum)//' next '//str(leng_next)//&
          ' err '//str((leng_sum + leng_next*(pos-pos_prev)) - leng_section))
      !endif
endif

      call add(leng_sum, leng_next*(pos-pos_prev))

      if( pos == 1.d0 )then
        pos_prev = 0.d0
        pos = 0.d0
        call add(jPt)
      endif

      if( jPt < ch%n .and. iDiv == chdiv%n )then
        call errend('jPt < ch%n and iDiv == chdiv%n')
      endif
    enddo  ! jPt, iDiv/
    !-----------------------------------------------------------
    ! Calc. coords. of edges
    !-----------------------------------------------------------
if( debug_this )then
  call logmsg('ch '//slonlat(ch%lon(1),ch%lat(1))//' - '//slonlat(ch%lon(ch%n),ch%lat(ch%n)))
endif
    do iDiv = 1, chdiv%n
      div => chdiv%div(iDiv)
      call realloc(div%jPt, div%n, clear=.false.)

      jPt = div%jPt(1)
      edge => div%edge(1)
      edge%lon = ch%lon(jPt)*(1.d0-edge%pos) + ch%lon(jPt+1)*edge%pos
      edge%lat = ch%lat(jPt)*(1.d0-edge%pos) + ch%lat(jPt+1)*edge%pos

      jPt = div%jPt(div%n)
      edge => div%edge(2)
      if( jPt == ch%n )then
        edge%lon = ch%lon(jPt)
        edge%lat = ch%lat(jPt)
      else
        edge%lon = ch%lon(jPt)*(1.d0-edge%pos) + ch%lon(jPt+1)*edge%pos
        edge%lat = ch%lat(jPt)*(1.d0-edge%pos) + ch%lat(jPt+1)*edge%pos
      endif
if( debug_this )then
  call logmsg('div('//str(iDiv,dgt(chdiv%n))//')'//&
    str(div%edge(1)%pos)//' '//str(div%jPt(1))//' '//&
    slonlat(div%edge(1)%lon,div%edge(1)%lat)//' - '//&
    str(div%edge(2)%pos)//' '//str(div%jPt(div%n))//' '//&
    slonlat(div%edge(2)%lon,div%edge(2)%lat))
 
endif

      allocate(div%lon(div%n+2))
      allocate(div%lat(div%n+2))
      div%lon(1) = div%edge(1)%lon
      div%lat(1) = div%edge(1)%lat
      do jjPt = 1, div%n
        div%lon(jjPt+1) = ch%lon(div%jPt(jjPt))
        div%lat(jjPt+1) = ch%lat(div%jPt(jjPt))
      enddo
      div%lon(div%n+2) = div%edge(2)%lon
      div%lat(div%n+2) = div%edge(2)%lat
    enddo  ! iDiv/
    !-----------------------------------------------------------
    ! Determine whether edges are outlet or not
    !-----------------------------------------------------------
    do iDiv = 1, chdiv%n
      chdiv%div(iDiv)%edge(:)%is_outlet = .false.
    enddo
    chdiv%div(1)%edge(1)%is_outlet = ch%node(1)%typ == NODETYPE_NEW__OUT
    chdiv%div(chdiv%n)%edge(2)%is_outlet = ch%node(2)%typ == NODETYPE_NEW__OUT
  enddo  ! jCh/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_network_model(name_leng, uid)
  open(newunit=un, file=f, status='replace')

  write(un,"(a)") 'channel '//str(sum(lst_chdiv(:)%n))

  iCh = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    chdiv => lst_chdiv(jCh)

    do iDiv = 1, chdiv%n
      div => chdiv%div(iDiv)
      call add(iCh)

      write(un,"(a)") '  channel '//str(iCh)
      write(un,"(a)") '  node_outlet '//str(div%edge(:)%is_outlet)
      write(un,"(a)") '  points '//str(div%n+2)
      write(un,"(a)") '  lon '//str(div%lon,'f12.7')
      write(un,"(a)") '  lat '//str(div%lat,'f11.7')

      write(un,"(a)") '  width '//str(10.d0)
      write(un,"(a)") '  depth '//str(5.d0)
      write(un,"(a)") '  levee '//str(0.d0)
    enddo  ! iDiv/
  enddo  ! jCh/

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_model_network_data
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
subroutine realloc_conn_mem(mem, n)
  implicit none
  type(conn_mem_), pointer :: mem(:)
  integer, intent(in) :: n

  integer :: m
  type(conn_mem_), allocatable :: tmp(:)

  m = size(mem)
  allocate(tmp(m))
  tmp(:) = mem(:)
  deallocate(mem)
  allocate(mem(n))
  mem(:m) = tmp(:)
  deallocate(tmp)
end subroutine realloc_conn_mem
!===============================================================
!
!===============================================================
end module mod_separate_networks
