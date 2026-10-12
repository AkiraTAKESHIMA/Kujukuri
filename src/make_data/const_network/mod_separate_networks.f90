module mod_separate_networks
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_util, only: &
        slonlat, &
        sBBox
  use c2_strnk_type
  use c2_nlni_const, only: &
        DGT_WSCODE
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: separateNetworks
  public :: makeSeparatedNetworkData
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_separate_networks'

  type, extends(cmn_nwknode_) :: nwknode_
    integer :: iCh_down
    integer :: iNode_down
  end type

  type, extends(cmn_node_) :: node_
  end type

  type, extends(cmn_channel_) :: channel_
    integer :: wsCode_i
    logical :: is_wsCode_temporal
    character(:), allocatable :: wsCode_org
    integer :: jWsys
    type(node_), pointer :: node(:)
    integer :: nwkId
    real(8) :: west, east, south, north
  end type

  type, extends(cmn_watsys_) :: watsys_
    integer :: wsCode_i
    integer :: jNwk
    integer :: nCh
    integer, pointer :: jCh(:)  ! (nCh) Index in network
  end type

  type network_
    character(:), allocatable :: id
    character(:), allocatable :: tmpid
    integer :: jNwk  ! index in tmpnwk
    real(8) :: west, east, south, north
    integer :: nCh, nCh_valid, nCh_removed
    type(channel_), pointer :: channel(:)
    integer, pointer :: jCh(:)
    integer :: nNode
    type(nwknode_), pointer :: node(:)
    integer :: nWsys
    type(watsys_), pointer :: wsys(:)
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
  real(8), parameter :: THRESH_LENG_SECTION = 1d-3
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine separateNetworks(tmpid_in)
  use c2_strnk_io, only: &
    get_f_tmp_networks_lst      , &
    get_f_tmp_network_separation, &
    get_f_tmp_network_channel   , &
    get_f_lst_networks_channel  , &
    get_f_network_channel       , &
    write_network
  use c3_jflw_const, only: &
    jflw_set_resolution
  use mod_util, only: &
    comma_json
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'separateNetworks'
  character(*), intent(in) :: tmpid_in

  type(network_), pointer :: lst_nwk(:), nwk
  type(watsys_), pointer :: wsys
  integer :: nNwk, mNwk, jNwk, jjNwk, jNwk0
  integer :: nNwk_tmp, iNwk_tmp
  integer :: jWsys
  character(32) :: tmpid
  integer(4), allocatable :: lst_jNwk(:)

  character(CLEN_PATH) :: f_lst, f
  integer :: un_lst, un
  character :: c_
  integer :: dgt_nCh, dgt_jNwk

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Setup J-FlwDir mesh
  !-------------------------------------------------------------
  call jflw_set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  ! Separate tmp networks
  !-------------------------------------------------------------
  call logent('Separating temporal networks')

  f_lst = get_f_tmp_networks_lst()
  call logmsg('Reading '//str(f_lst))
  open(newunit=un_lst, file=f_lst, status='old')

  read(un_lst,*) c_, nNwk_tmp
  read(un_lst,*)
  nNwk = 0
  do iNwk_tmp = 1, nNwk_tmp
    read(un_lst,*) c_, tmpid

    if( tmpid_in /= '' .and. tmpid /= tmpid_in ) cycle

    call logmsg('tmpnwk '//str(tmpid))
    call separate_network(tmpid)
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Read separation info.
  !-------------------------------------------------------------
  call logent('Reading separation info.')

  f_lst = get_f_tmp_networks_lst()
  call logmsg('Reading '//str(f_lst))
  open(newunit=un_lst, file=f_lst, status='old')

  rewind(un_lst)
  read(un_lst,*) c_, nNwk_tmp
  read(un_lst,*)
  nNwk = 0
  do iNwk_tmp = 1, nNwk_tmp
    read(un_lst,*) c_, tmpid

    if( tmpid_in /= '' .and. tmpid /= tmpid_in ) cycle

    f = get_f_tmp_network_separation(tmpid)
    open(newunit=un, file=f, status='old')
    read(un,*) ! nCh
    read(un,*) c_, mNwk
    close(un)

    call add(nNwk, mNwk)
  enddo

  if( tmpid_in /= '' )then
    call logmsg('Networks tmp: '//str(nNwk_tmp)//' new: '//str(nNwk))
  endif
  allocate(lst_nwk(nNwk))

  rewind(un_lst)
  read(un_lst,*)
  read(un_lst,*)
  jNwk0 = 0
  do iNwk_tmp = 1, nNwk_tmp
    read(un_lst,*) c_, tmpid

    if( tmpid_in /= '' .and. tmpid /= tmpid_in ) cycle

    f = get_f_tmp_network_separation(tmpid)
    open(newunit=un, file=f, status='old')
    read(un,*) ! nCh
    read(un,*) c_, mNwk

    do jNwk = 1, mNwk
      nwk => lst_nwk(jNwk0+jNwk)
      allocate(character(1) :: nwk%tmpid)
      nwk%tmpid = trim(tmpid)
      nwk%jNwk = jNwk
    enddo

    read(un,*)  ! length of each wsCode
    do jNwk = 1, mNwk
      nwk => lst_nwk(jNwk0+jNwk)
      read(un,*) c_, c_, nwk%nWsys
      allocate(nwk%wsys(nwk%nWsys))
      do jWsys = 1, nwk%nWsys
        wsys => nwk%wsys(jWsys)
        allocate(character(DGT_WSCODE) :: wsys%wsCode)
        read(un,*) wsys%wsCode, wsys%leng
        wsys%wsCode_i = int4_char(wsys%wsCode)
      enddo  ! jWsys/
    enddo  ! jNwk/

    read(un,*)  ! channel indices
    do jNwk = 1, mNwk
      nwk => lst_nwk(jNwk0+jNwk)
      read(un,*) c_, c_, nwk%nCh
      read(un,*)
    enddo  ! jNwk/

    close(un)

    call add(jNwk0, mNwk)
  enddo  ! iNwk_tmp/

  close(un_lst)

  dgt_jNwk = dgt(maxval(lst_nwk(:)%jNwk))
  dgt_nCh = dgt(maxval(lst_nwk(:)%nCh))

  call logext()
  !-------------------------------------------------------------
  ! Setting new ids
  !-------------------------------------------------------------
  if( tmpid_in == '' )then
    call logent('Setting new ids')

    allocate(lst_jNwk(nNwk))

    call set_separated_network_ids(lst_nwk, lst_jNwk)

    call logext()
  else
    do jNwk = 1, nNwk
      nwk => lst_nwk(jNwk)
      if( .not. allocated(nwk%tmpid) ) cycle
      nwk%id = '(id_empty)'
    enddo
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  if( tmpid_in == '' )then
    call logent('Outputting')

    f = get_f_lst_networks_channel()
    call logmsg('Writing '//str(f))
    open(newunit=un, file=f, status='replace')
    write(un,"(1x,a)") 'networks '//str(nNwk)
    write(un,"(1x,a)") 'i id tmpid jNwk nCh leng'
    do jjNwk = 1, nNwk
      jNwk = lst_jNwk(jjNwk)
      nwk => lst_nwk(jNwk)

      write(un,"(1x,a)") &
          str(jjNwk,dgt(nNwk))//' '//&
          str(nwk%id)//' '//str(nwk%tmpid)//' '//str(nwk%jNwk,dgt_jNwk)//' '//&
          str(nwk%nCh,dgt_nCh)//' '//str(sum(nwk%wsys(:)%leng))
    enddo  ! jNwk/
    close(un)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine separateNetworks
!===============================================================
!
!===============================================================
subroutine separate_network(id)
  use c2_strnk_io, only: &
    get_f_tmp_network_channel   , &
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
    jNode2jPt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'separate_network'
  character(*), intent(in) :: id

  type(network_) :: nwk
  type(channel_), pointer :: ch, ch2
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode
  type(watsys_), pointer :: wsys, wsys2
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
  integer :: jCh, jCh2, jjCh
  integer :: iiCh, iiCh2
  integer :: jPt, jPt0
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
  logical :: flag
  real(8) :: leng, leng_max

  character(CLEN_PATH) :: f
  integer :: un
  integer :: dgt_nCh, dgt_nNode, dgt_nWsys

  integer :: cl_wsCode, cl_rvCode, cl_rvName

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Read tmp. network data
  !-------------------------------------------------------------
  call logent('Reading temporal network data')

  allocate(character(1) :: nwk%id)
  nwk%id = id

  f = get_f_tmp_network_channel(nwk%id, 'sbin')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, form='unformatted', access='sequential', &
       action='read', status='old')

  read(un) nwk%nCh, nwk%west, nwk%east, nwk%south, nwk%north

  allocate(nwk%channel(nwk%nCh))

  nwk%nCh_removed = 0

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
    if( ch%n <= 1 )then
      call logmsg('Ignored : ch#'//str(jCh)//' n: '//str(ch%n))
      call add(nwk%nCh_removed)
      ch%n = 0
      deallocate(ch%wsCode, ch%rvCode, ch%rvName)
      read(un)  ! lon
      read(un)  ! lat
      read(un)  ! leng
      do jNode = 1, 2
        read(un)  ! typ, elv
      enddo
      cycle
    endif

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

  if( nwk%nCh_removed > 0 )then
    call logmsg('Removed '//str(nwk%nCh_removed)//' channels')
  endif

  nwk%nCh_valid = nwk%nCh - nwk%nCh_removed

  dgt_nCh = dgt(nwk%nCh)

  call logext()
  !-------------------------------------------------------------
  ! Remove extremely short sections
  !-------------------------------------------------------------
  call logent('Removing extremely short sections')

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( ch%n == 0 ) cycle

    flag = .false.
    ch%leng = 0.d0
    do jPt = 1, ch%n-1
      leng = dist_sphere( &
        ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r &
      ) * EARTH_R
      call add(ch%leng, leng)
      if( leng < THRESH_LENG_SECTION ) flag = .true.
    enddo

    if( ch%leng < THRESH_LENG_SECTION )then
      call logmsg('ch #'//str(jCh,dgt(nwk%nCh))//&
        ' n: '//str(ch%n)//' leng: '//str(ch%leng))
    elseif( flag )then
      jPt = 1
      do while( jPt < ch%n )
        leng = dist_sphere( &
          ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r &
        ) * EARTH_R

        if( leng < THRESH_LENG_SECTION )then
          jPt0 = jPt
          if( jPt == 1 ) jPt0 = 2
          ch%lon(jPt0:ch%n-1) = ch%lon(jPt0+1:ch%n)
          ch%lat(jPt0:ch%n-1) = ch%lat(jPt0+1:ch%n)
          call add(ch%n, -1)
        else
          call add(jPt)
        endif
      enddo  ! jPt/

      if( ch%n < size(ch%lon) )then
        call realloc(ch%lon, ch%n, clear=.false.)
        call realloc(ch%lat, ch%n, clear=.false.)

        ch%leng = 0.d0
        do jPt = 1, ch%n-1
          call add(ch%leng, dist_sphere( &
            ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r &
          ) * EARTH_R)
        enddo  ! jPt/
      endif
    endif
  enddo  ! jCh/

  call logext()
  !-------------------------------------------------------------
  ! Calc. connections at nodes
  !-------------------------------------------------------------
  call logent('Calculating connections at nodes')

  call make_list_nwknode(nwk)

  dgt_nNode = dgt(nwk%nNode)

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    do iiCh = 1, nwknode%nCh
      node => nwk%channel(nwknode%jCh(iiCh))%node(nwknode%jNode(iiCh))
      node%iNode = iNode
    enddo

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

  call logext()
  !-------------------------------------------------------------
  ! Modify wsCode
  !-------------------------------------------------------------
  call logent('Modifying wsCode')

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( ch%n == 0 ) cycle
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
      if( ch%n == 0 ) cycle
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

  allocate(lst_wsCode_i(nwk%nCh_valid))
  allocate(lst_chleng(nwk%nCh_valid))

  jCh = 0
  do jjCh = 1, nwk%nCh
    ch => nwk%channel(jjCh)
    if( ch%n == 0 ) cycle
    call add(jCh)
    lst_wsCode_i(jCh) = ch%wsCode_i
    lst_chleng(jCh) = ch%leng
  enddo
  allocate(arg(nwk%nCh_valid))
  call argsort(lst_wsCode_i, arg)
  call sort(lst_wsCode_i, arg)
  call sort(lst_chleng, arg)
  deallocate(arg)

  nwk%nWsys = 0
  ie = 0
  do while( ie < nwk%nCh_valid )
    is = ie + 1
    ie = is
    do while( ie < nwk%nCh_valid )
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
    if( ch%n == 0 ) cycle
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
    ch => nwk%channel(jCh)
    if( ch%n == 0 ) cycle
    call add(nwk%wsys(ch%jWsys)%nCh)
  enddo

  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys(jWsys)
    if( wsys%nCh == 0 ) cycle
    allocate(wsys%jCh(wsys%nCh))
  enddo

  nwk%wsys(:)%nCh = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( ch%n == 0 ) cycle
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
    if( ch%n == 0 ) cycle
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
  ! Making watsys list
  !-------------------------------------------------------------
  call logent('Making a water system code list')

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
  ! Separate network
  !-------------------------------------------------------------
  call logent('Separating the network by ineffective connections')

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
    call logmsg('snwk nCh: '//str(snwk%nCh))

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

  if( sum(snetwork_tmp(:)%nCh) /= nwk%nCh_valid )then
    call errend('Total number of channel mismatch.'//&
    '\n  sntework_tmp(:)%nCh: '//str(snetwork_tmp(:)%nCh)//&
    '\n  nwk%nCh_valid: '//str(nwk%nCh_valid))
  endif

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

  f = get_f_tmp_network_separation(id)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')

  write(un,"(1x,a)") 'Channels '//str(nwk%nCh)//' removed '//str(nwk%nCh_removed)
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
        DIV_WSCODE_RVUNKNOWN
  use c2_strnk_const, only: &
        DGT_NWK_SAME_WSCODE, &
        DGT_NWKID
  implicit none
  character(CLEN_PROC) :: PRCNAM = 'set_separated_network_ids'
  type(network_), intent(inout), target :: lst_nwk(:)
  integer(4), intent(out) :: lst_jNwk(:)

  type(network_), pointer :: nwk
  integer :: nNwk, jNwk, jjNwk
  character(1) :: id_head
  integer(4), allocatable :: lst_wsCode_i(:)
  real(8)   , allocatable :: lst_wsLeng(:)
  integer(8), allocatable :: lst_id_i(:)
  integer(4), allocatable :: arg(:)
  integer :: is, ie, i

  integer, parameter :: MUL_WSCODE = 10 ** DGT_NWK_SAME_WSCODE
  integer, parameter :: NLIM_SAME_WSCODE = 10 ** (DGT_NWK_SAME_WSCODE) - 1
  integer, parameter :: DGT_NWKGID_BODY = DGT_NWKID - 1

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
    allocate(character(1) :: nwk%id)
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
      id_head = '1'
    else
      id_head = '0'
    endif

    do i = is, ie
      jNwk = lst_jNwk(i)
      nwk => lst_nwk(jNwk)
      nwk%id = id_head//str(lst_wsCode_i(i) * MUL_WSCODE + (i - is + 1), -DGT_NWKGID_BODY)
      !if( i == is .or. i == ie )then
      !  call logmsg(nwk%wsys(1)%wsCode//' -> '//nwk%id, opt='+x2')
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
  call logent('Sorting and checking duplication of id')

  allocate(lst_id_i(nNwk))
  do jNwk = 1, nNwk
    lst_id_i(jNwk) = int8_char(lst_nwk(jNwk)%id)
  enddo  ! jNwk/
  call argsort(lst_id_i, lst_jNwk)
  do jjNwk = 1, nNwk-1
    if( lst_id_i(lst_jNwk(jjNwk+1)) == lst_id_i(lst_jNwk(jjNwk)) )then
      call errend('Duplicated id: '//str(lst_id_i(lst_jNwk(jjNwk))))
    endif
  enddo  ! jNwk/
  deallocate(lst_id_i)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_separated_network_ids
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
subroutine makeSeparatedNetworkData(id)
  use c2_strnk_const
  use c2_strnk_io, only: &
    get_f_lst_networks_channel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeSeparatedNetworkData'
  character(*), intent(in) :: id

  integer :: nNwk, iNwk
  integer :: jNwk
  character(DGT_NWKID) :: id_
  character(32) :: tmpid

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_lst_networks_channel()
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) c_, nNwk
  read(un,*)
  do iNwk = 1, nNwk
    read(un,*) c_, id_, tmpid, jNwk
    if( id /= '' .and. id_ /= id ) cycle

    call logent('Network '//str(id_))

    call make_separated_network_data(id_, tmpid, jNwk)

    call logext()
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeSeparatedNetworkData
!===============================================================
!
!===============================================================
subroutine make_separated_network_data(id, tmpid, jNwk)
  use c2_strnk_io, only: &
    write_network               , &
    get_f_tmp_networks_lst      , &
    get_f_tmp_network_separation, &
    get_f_tmp_network_channel   , &
    get_f_network_channel       
  use c2_strnk_util, only: &
    clear_cmn_network
  use mod_util, only: &
    jNode2jPt , &
    comma_json
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_separated_network_data'
  character(*), intent(in) :: id
  character(*), intent(in) :: tmpid
  integer, intent(in) :: jNwk

  type(network_), pointer :: nwk
  type(network_), target :: nwk_old, nwk_new
  type(watsys_), pointer :: wsys
  type(channel_), pointer :: ch, ch2
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode
  type(cmn_network_) :: cmnnwk
  integer :: mNwk, jjNwk
  integer :: nWsys, jWsys
  integer :: nCh, nCh_removed, jCh, jCh2, iiCh
  integer :: jNode, jNode2, iNode
  integer :: jPt
  integer(4), allocatable :: jCh2jNwk(:)
  integer :: cl_wsCode, cl_rvCode, cl_rvName

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_
  integer :: dgt_nCh

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Read channel index in the tmp network
  !-------------------------------------------------------------
  call logent('Reading channel index in the temporal network')

  nwk => nwk_old
  allocate(character(1) :: nwk%id)
  allocate(character(1) :: nwk%tmpid)
  nwk%id = trim(id)
  nwk%tmpid = trim(tmpid)

  f = get_f_tmp_network_separation(nwk%tmpid)
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) c_, nCh, c_, nCh_removed
  read(un,*) c_, mNwk

  ! Water system code
  read(un,*)
  do jjNwk = 1, mNwk
    if( jjNwk == jNwk )then
      read(un,*) c_, c_, nwk%nWsys
      allocate(nwk%wsys(nwk%nWsys))
      do jWsys = 1, nwk%nWsys
        wsys => nwk%wsys(jWsys)
        allocate(character(DGT_WSCODE) :: wsys%wsCode)
        read(un,*) wsys%wsCode, wsys%leng
        wsys%wsCode_i = int4_char(wsys%wsCode)
      enddo  ! jWsys/
    else
      read(un,*) c_, c_, nWsys
      do jWsys = 1, nWsys
        read(un,*)
      enddo
    endif
  enddo  ! jjNwk/

  ! Channel index
  read(un,*)
  do jjNwk = 1, mNwk
    if( jjNwk == jNwk )then
      read(un,*) c_, c_, nwk%nCh
      allocate(nwk%jCh(nwk%nCh))
      read(un,*) nwk%jCh(:)
      call sort(nwk%jCh)
    else
      read(un,*)
      read(un,*)
    endif
  enddo  ! jNwk/

  close(un)

  dgt_nCh = dgt(nwk%nCh)

  call logext()
  !-------------------------------------------------------------
  ! Read channel data
  !-------------------------------------------------------------
  call logent('Reading channel data')

  allocate(jCh2jNwk(nCh))
  jCh2jNwk(:) = 0
  do iiCh = 1, nwk%nCh
    jCh = nwk%jCh(iiCh)
    if( jCh2jNwk(jCh) /= 0 )then
      call errend('jCh2jNwk(jCh) /= 0')
    endif
    jCh2jNwk(jCh) = jNwk
  enddo

  allocate(nwk%channel(nwk%nCh))
  nwk%west = 1d20
  nwk%east = -1d20
  nwk%south = 1d20
  nwk%north = -1d20

  f = get_f_tmp_network_channel(nwk%tmpid, 'sbin')
  open(newunit=un, file=f, form='unformatted', access='sequential', &
       action='read', status='old')

  read(un) nCh

  do jCh = 1, nCh
    jjNwk = jCh2jNwk(jCh)
    if( jjNwk /= jNwk )then
      read(un)  ! bbox
      read(un)  ! cl
      read(un)  ! wsCode
      read(un)  ! rvCode
      read(un)  ! rvName
      read(un)  ! ch%n
      read(un)  ! ch%lon
      read(un)  ! ch%lat
      read(un)  ! ch%leng
      do jNode = 1, 2
        read(un)
      enddo
      cycle
    endif

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

  deallocate(jCh2jNwk)

  call make_list_nwknode(nwk)

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    do iiCh = 1, nwknode%nCh
      node => nwk%channel(nwknode%jCh(iiCh))%node(nwknode%jNode(iiCh))
      node%iNode = iNode
    enddo  ! iiCh/
  enddo  ! iNode/

  call logext()
  !-----------------------------------------------------------
  ! Integrate channels connected in series
  !-----------------------------------------------------------
  call logent('Integrating channels connected in series')

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    if( nwknode%nCh /= 2 ) cycle

    jCh = nwknode%jCh(1)
    jNode = nwknode%jNode(1)
    jCh2 = nwknode%jCh(2)
    jNode2 = nwknode%jNode(2)
    ch => nwk%channel(jCh)
    ch2 => nwk%channel(jCh2)

    call logmsg('node#'//str(iNode,dgt(nwk%nNode))//&
      ' integ. ch#'//str(jCh2,dgt_nCh)//' to ch#'//str(jCh,dgt_nCh)//&
    '\n  ch# '//str(jCh,dgt_nCh)//' '//slonlat(ch%node(1)%lon,ch%node(1)%lat)//&
         ' - '//slonlat(ch%node(2)%lon,ch%node(2)%lat)//&
    '\n  ch# '//str(jCh2,dgt_nCh)//' '//slonlat(ch2%node(1)%lon,ch2%node(1)%lat)//&
         ' - '//slonlat(ch2%node(2)%lon,ch2%node(2)%lat))

    if( ch%n == 0 .or. ch2%n == 0 )then
      call errend('ch#'//str(jCh)//' n: '//str(ch%n)//&
          ', ch#'//str(jCh2)//' n: '//str(ch2%n))
    endif

    if( jNode == 1 )then
      call logmsg('  reverse ch#'//str(jCh))
      call reverse(ch%lon)
      call reverse(ch%lat)
      call swap_chnode(ch)
    endif
    if( jNode2 /= 1 )then
      call logmsg('  reverse ch#'//str(jCh2))
      call reverse(ch2%lon)
      call reverse(ch2%lat)
      call swap_chnode(ch2)
    endif
    if( ch%lon(ch%n) /= ch2%lon(1) .or. ch%lat(ch%n) /= ch2%lat(1) )then
      call errend(&
        'jNode: '//str(jNode)//' jNode2: '//str(jNode2)//&
      '\nch  node(1): '//slonlat(ch%node(1)%lon,ch%node(1)%lat)//&
          ', node(2): '//slonlat(ch%node(2)%lon,ch%node(2)%lat)//&
      '\nch2 node(1): '//slonlat(ch2%node(1)%lon,ch2%node(1)%lat)//&
          ', node(2): '//slonlat(ch2%node(2)%lon,ch2%node(2)%lat)//&
      '\nch  pt(1)  : '//slonlat(ch%lon(1),ch%lat(1))//&
          ', pt(n)  : '//slonlat(ch%lon(ch%n),ch%lat(ch%n))//&
      '\nch2 pt(1)  : '//slonlat(ch2%lon(1),ch2%lat(1))//&
          ', pt(n)  : '//slonlat(ch2%lon(ch2%n),ch2%lat(ch2%n)))
    endif

    call realloc(ch%lon, ch%n+ch2%n-1, clear=.false.)
    call realloc(ch%lat, ch%n+ch2%n-1, clear=.false.)

    ch%lon(ch%n+1:) = ch2%lon(2:)  !(ch2%n-1)
    ch%lat(ch%n+1:) = ch2%lat(2:)
    ch%n = ch%n + ch2%n - 1
    ch%node(2)%lon = ch%lon(ch%n)
    ch%node(2)%lat = ch%lat(ch%n)
    ch%node(2)%iNode = ch2%node(2)%iNode

    call logmsg(&
      '  ch# '//str(jCh,dgt_nCh)//' '//slonlat(ch%node(1)%lon,ch%node(1)%lat)//&
         ' - '//slonlat(ch%node(2)%lon,ch%node(2)%lat))

    nwknode%nCh = 0

    ! Update nwknode of ch%node(1)
    jNode = 1
    nwknode => nwk%node(ch%node(jNode)%iNode)
    do iiCh = 1, nwknode%nCh
      if( nwknode%jCh(iiCh) /= jCh ) cycle
      nwknode%jNode(iiCh) = jNode
    enddo  ! iiCh/

    ! Update nwknode of ch2%node(2)
    jNode = 2
    nwknode => nwk%node(ch2%node(jNode)%iNode)
    if( .not. any(nwknode%jCh(:) == jCh2) )then
      call errend('jCh2 not found in nwknode%jCh')
    endif
    do iiCh = 1, nwknode%nCh
      if( nwknode%jCh(iiCh) /= jCh2 ) cycle
      nwknode%jCh(iiCh) = jCh
      nwknode%jNode(iiCh) = jNode
    enddo

    ch2%n = 0
  enddo  ! iNode/

  nullify(ch, ch2, nwknode)

  call logmsg('Channels: '//str(nwk%nCh)//' -> '//str(count(nwk%channel(:)%n>0)))

  ! Make new network data
  allocate(character(1) :: nwk_new%id)
  allocate(character(1) :: nwk_new%tmpid)
  nwk_new%id = trim(id)
  nwk_new%tmpid = nwk%tmpid

  nwk_new%nWsys = nwk%nWsys
  allocate(nwk_new%wsys(nwk%nWsys))
  nwk_new%wsys(:) = nwk%wsys(:)

  nwk_new%nCh = count(nwk%channel(:)%n > 0)
  allocate(nwk_new%channel(nwk_new%nCh))
  allocate(nwk_new%jCh(nwk_new%nCh))

  nCh = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( ch%n == 0 ) cycle

    call add(nCh)
    nwk_new%channel(nCh) = ch
    nwk_new%jCh(nCh) = nwk%jCh(jCh)  ! index in tmpnwk

    ch => nwk_new%channel(nCh)
    do jNode = 1, 2
      jPt = jNode2jPt(jNode, ch%n)
      ch%node(jNode)%lon = ch%lon(jPt)
      ch%node(jNode)%lat = ch%lat(jPt)
    enddo  ! jNode/
  enddo  ! jCh/

  ! Discard old network
  nullify(ch)
  deallocate(nwk%wsys)
  deallocate(nwk%channel)
  deallocate(nwk%node)
  deallocate(nwk%jCh)
  deallocate(nwk%id)
  deallocate(nwk%tmpid)

  ! Switch the pointer
  nwk => nwk_new

  ! Calc. bbox
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    ch%west = minval(ch%lon)
    ch%east = maxval(ch%lon)
    ch%south = minval(ch%lat)
    ch%north = maxval(ch%lat)
  enddo  ! /jCh/

  nwk%west = minval(nwk%channel(:)%west)
  nwk%east = maxval(nwk%channel(:)%east)
  nwk%south = minval(nwk%channel(:)%south)
  nwk%north = maxval(nwk%channel(:)%north)

  call make_list_nwknode(nwk)

  call logext()
  !-----------------------------------------------------------
  ! Calc. distance from nodes to outlet
  !-----------------------------------------------------------
  call logent('Calculating distance from nodes to outlet')

  call calc_distance_to_outlet(nwk)

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  ! Sequential binary
  f = get_f_network_channel(nwk%id, 'sbin')
  call logmsg('Writing '//str(f))
  call copy_nwk2cmn(nwk, cmnnwk)
  call write_network(f, cmnnwk)
  call clear_cmn_network(cmnnwk)

  ! JSON
  f = get_f_network_channel(nwk%id, 'json')
  call logmsg('Writing '//str(f))
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
    write(un,"(6x,a)")  '"num_point": "'//str(ch%n)//'",'
    write(un,"(6x,a)")  '"lon": ['//str(ch%lon,'f16.11',',')//'],'
    write(un,"(6x,a)")  '"lat": ['//str(ch%lat,'f16.11',',')//'],'
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
  write(un,"(2x,a)") '"tmpid": "'//str(nwk%tmpid)//'"'
  write(un,"(a)") '}'
  close(un)

  call logext()
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  deallocate(nwk%wsys)
  deallocate(nwk%channel)
  deallocate(nwk%node)
  deallocate(nwk%jCh)
  deallocate(nwk%id)
  deallocate(nwk%tmpid)
  nullify(nwk)
  !-----------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine swap_chnode(ch)
  implicit none
  type(channel_), intent(inout) :: ch

  type(node_) :: chnode

  chnode = ch%node(1)
  ch%node(1) = ch%node(2)
  ch%node(2) = chnode
end subroutine swap_chnode
!---------------------------------------------------------------
end subroutine make_separated_network_data
!===============================================================
!
!===============================================================
subroutine calc_distance_to_outlet(nwk)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_distance_to_outlet'
  type(network_), intent(inout) :: nwk

  type(channel_), pointer :: ch, ch2
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode, nwknode2
  type(node_) :: node_tmp
  integer :: n_updated, n_updated_next
  integer, allocatable :: iNode_updated(:)
  integer, allocatable :: iNode_updated_next(:)
  logical, allocatable :: is_updated_next(:)
  integer :: jCh, jCh2
  integer :: iiCh
  integer :: jNode
  integer :: iNode, iNode2
  integer :: i
  logical :: is_outlet_exist
  logical :: is_updated

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Check if outlet exists
  !-------------------------------------------------------------
  call logent('Searching for outlet')

  call extend_list_nwknode()

  call search_for_outlet(is_outlet_exist)

  if( .not. is_outlet_exist )then
    call logwrn('Outlet was not found.')
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

  call logmsg('Outlets: '//str(n_updated))

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
          !call logmsg('node('//str(iNode2)//') downleng '//&
          !            str(nwknode2%downleng)//' -> '//str(nwknode%downleng+ch2%leng))
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

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)

    do jNode = 1, 2
      node => ch%node(jNode)
      node%downleng = nwk%node(node%iNode)%downleng
    enddo  ! jNode/
  enddo  ! jCh/

  call logext()
  !-------------------------------------------------------------
  ! Swap nodes if node(1) is downstream of node(2)
  !-------------------------------------------------------------
  call logent('Swapping nodes of channels with reverse direction')

  is_updated = .false.

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)

    if( ch%node(1)%downleng > ch%node(2)%downleng ) cycle

    if( ch%node(1)%downleng == ch%node(2)%downleng )then
      cycle
    endif

    is_updated = .true.

    node_tmp = ch%node(1)
    ch%node(1) = ch%node(2)
    ch%node(2) = node_tmp

    call reverse(ch%lon)
    call reverse(ch%lat)
  enddo  ! jCh/

  if( is_updated )then
    call logmsg('Updated. Remaking a list of nodes')

    deallocate(nwk%node)

    call make_list_nwknode(nwk)

    call extend_list_nwknode()

    call search_for_outlet(is_outlet_exist)
  endif

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine extend_list_nwknode()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__extend_list_nwknode'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    allocate(nwknode%iNode(nwknode%nCh))
    do iiCh = 1, nwknode%nCh
      jCh = nwknode%jCh(iiCh)
      jNode = nwknode%jNode(iiCh)
      node => nwk%channel(jCh)%node(jNode)
      node%iNode = iNode
    enddo  ! iiCh/
  enddo  ! iNode/

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)

    allocate(nwknode%iNode(nwknode%nCh))
    do iiCh = 1, nwknode%nCh
      jCh = nwknode%jCh(iiCh)
      jNode = nwknode%jNode(iiCh)
      ch => nwk%channel(jCh)

      nwknode%iNode(iiCh) = ch%node(3-jNode)%iNode  ! opposite side
      nwknode%downleng = ch%node(jNode)%downleng
    enddo  ! iCh/
  enddo  ! iNode/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine extend_list_nwknode
!---------------------------------------------------------------
subroutine search_for_outlet(is_outlet_exist)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__search_for_outlet'
  logical, intent(out) :: is_outlet_exist

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
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
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine search_for_outlet
!---------------------------------------------------------------
end subroutine calc_distance_to_outlet
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
subroutine make_list_nwknode(nwk)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_list_nwknode'
  type(network_), intent(inout) :: nwk

  type(channel_), pointer :: ch
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_jCh(:), lst_jNode(:)
  integer, allocatable :: arg(:)
  integer :: jCh
  integer :: jNode, iNode
  integer :: is, ie, iis, iie

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nwk%nNode = (nwk%nCh - nwk%nCh_removed) * 2
  allocate(nwk%node(nwk%nNode))

  allocate(lst_lon(nwk%nNode))
  allocate(lst_lat(nwk%nNode))
  allocate(lst_jCh(nwk%nNode))
  allocate(lst_jNode(nwk%nNode))
  lst_jCh(:) = 0
  lst_jNode(:) = 0

  iNode = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( ch%n == 0 ) cycle
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

      nwknode%typ = nwk%channel(lst_jCh(iis))%node(lst_jNode(iis))%typ
    enddo  ! iis, iie/
  enddo  ! is, ie/

  nwk%nNode = iNode
  call logmsg('Nodes: '//str(nwk%nNode))

  deallocate(arg)
  deallocate(lst_lon)
  deallocate(lst_lat)
  deallocate(lst_jCh)
  deallocate(lst_jNode)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_list_nwknode
!===============================================================
!
!===============================================================
subroutine copy_nwk2cmn(nwk, cmn)
  implicit none
  type(network_), intent(in) :: nwk
  type(cmn_network_), intent(out) :: cmn

  type(cmn_watsys_), pointer :: cwsys
  type(cmn_channel_), pointer :: cch
  type(cmn_node_), pointer :: cnode
  type(watsys_), pointer :: wsys
  type(channel_), pointer :: ch
  type(node_), pointer :: node
  integer :: jWsys
  integer :: jCh
  integer :: jNode

  allocate(character(1) :: cmn%id)
  cmn%id = nwk%id

  cmn%nWsys = nwk%nWsys
  allocate(cmn%wsys_(cmn%nWsys))
  do jWsys = 1, cmn%nWsys
    cwsys => cmn%wsys_(jWsys)
    wsys => nwk%wsys(jWsys)

    allocate(character(1) :: cwsys%wsCode)
    cwsys%wsCode = wsys%wsCode

    cwsys%leng = wsys%leng
  enddo  ! jWsys/

  cmn%nCh = nwk%nCh
  allocate(cmn%channel_(cmn%nCh))
  do jCh = 1, cmn%nCh
    cch => cmn%channel_(jCh)
    ch => nwk%channel(jCh)

    allocate(character(1) :: cch%wsCode)
    allocate(character(1) :: cch%rvCode)
    allocate(character(1) :: cch%rvName)
    cch%wsCode = ch%wsCode
    cch%rvCode = ch%rvCode
    cch%rvName = ch%rvName

    cch%n = ch%n
    allocate(cch%lon(cch%n))
    allocate(cch%lat(cch%n))
    cch%lon(:) = ch%lon(:)
    cch%lat(:) = ch%lat(:)

    cch%leng = ch%leng

    allocate(cch%node_(2))
    do jNode = 1, 2
      cnode => cch%node_(jNode)
      node => ch%node(jNode)
      cnode%lon = node%lon
      cnode%lat = node%lat
      cnode%typ = node%typ
      cnode%elv = node%elv
      cnode%downleng = node%downleng
      cnode%iNode = node%iNode
    enddo  ! jNode/
  enddo  ! jCh/
end subroutine copy_nwk2cmn
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
