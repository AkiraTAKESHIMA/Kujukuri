module mod_modify_flwdir
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
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: modifyFlwdir
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_modify_flwdir'

  type node_
    integer :: typ
    integer :: i  ! Index in network
    real(8) :: theta
    logical :: is_outlet
  end type

  type seq_
    integer :: sz
    integer :: n
    integer, pointer :: gx(:), gy(:)
    real(8), pointer :: lon(:), lat(:)
    real(8), pointer :: leng(:)
    real(8), pointer :: ratio(:)
    integer(1), pointer :: stat(:)
    real(8), pointer :: angle(:)  ! Angle b/w fdr of J-FlwDir
    real(8) :: angle_mean
    integer :: jmin_used, jmax_used
  end type
  type(seq_) :: seq

  type channel_
    integer :: n
    real(8) :: west, east, south, north
    real(8), pointer :: lon(:), lat(:)
    type(node_), pointer :: node(:)
    type(seq_) :: seq
    real(8) :: upa
    real(8) :: width, depth, levee
  end type

  type nwk_node_
    real(8) :: lon, lat
    integer :: gx, gy
    integer :: nCh
    integer, pointer :: jCh(:)  !(nCh)
    integer, pointer :: jNode(:)  !(nCh)
  end type

  type network_
    character(:), allocatable :: uid
    integer :: nCh
    type(channel_), pointer :: channel(:)
    integer :: nNode
    type(nwk_node_), pointer :: node(:)
  end type


  integer(1), parameter :: FDR_NEW__TABLE = -20

  integer(1), parameter :: SEQ_STAT__USED = 1
  integer(1), parameter :: SEQ_STAT__SKIPPED = 0

  real(8), parameter :: THRESH_LENG_RATIO = 0.5d0
  integer, parameter :: THRESH_SEQ_N = 20
  real(8), parameter :: SEQ_ANGLE_UNDEF = -999.d0
  real(8), parameter :: CH_UPA_MISS = -9999.d0
  real(8), parameter :: CH_WIDTH_MIN = 1.d0
  real(8), parameter :: CH_DEPTH_MIN = 0.5d0
  !-------------------------------------------------------------
  ! Interfaces for intrisic functions
  !-------------------------------------------------------------
  interface
    integer function access(f, mode)
      character(*), intent(in) :: f
      character(*), intent(in) :: mode
    end function access
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine modifyFlwdir(resolution, uid)
  implicit none
  character(*), intent(in) :: resolution
  character(*), intent(in) :: uid

  call modify_flwdir_network(resolution, uid)
end subroutine modifyFlwdir
!===============================================================
!
!===============================================================
subroutine modify_flwdir_network(resolution, uid)
  use c1_grid, only: &
        apprx_isct_with_meridian, &
        apprx_isct_with_parallel
  use c2_jflw_const, &
        set_resolution => set_resolution
  use c2_jflw_grid, only: &
        gxs_of_lon, &
        gxe_of_lon, &
        gys_of_lat, &
        gye_of_lat, &
        center_of_gx, &
        center_of_gy, &
        west_of_gx, &
        east_of_gx, &
        south_of_gy, &
        north_of_gy, &
        get_nextxy, &
        get_fdr
  use c2_jflw_io, only: &
        read_map_from_tile, &
        read_basin_range_from_each, &
        get_f_dat_basin
  use c2_strnk_io, only: &
        strnk_get_f_network_channel => get_f_network_channel
  use c3_joint_grid, only: &
        joint_conv_fdr_jflw2rri => conv_fdr_jflw2rri
  use c3_joint_io, only: &
        joint_get_f_nwk_info    => get_f_nwk_info   , &
        joint_get_f_nwk_map     => get_f_nwk_map    , &
        joint_get_f_nwk_map_rri => get_f_nwk_map_rri, &
        joint_get_f_chn_rri     => get_f_chn_rri
  use mod_util, only: &
        jNode2jPt, &
        search_2, &
        sMeshRange
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_flwdir_network'
  character(*), intent(in) :: resolution
  character(*), intent(in) :: uid

  type dct_updated_
    integer :: n
    integer, pointer :: gx(:), gy(:)
    real(8), pointer :: area(:)
  end type

  type dct_fdr_
    integer :: n
    integer, pointer :: gx_up(:), gy_up(:)
    integer, pointer :: gx_dn(:), gy_dn(:)
  end type

  type(network_) :: nwk
  type(channel_), pointer :: ch, ch2
  type(seq_), pointer :: seq, seq2
  type(nwk_node_), pointer :: nwknode
  type(node_), pointer :: node, node2
  integer(1), allocatable :: fdrmap(:,:)
  integer   , allocatable :: bsnmap(:,:)
  real(4)   , allocatable :: upamap(:,:)
  integer   , allocatable :: upgmap(:,:)
  real(4)   , allocatable :: elvmap(:,:)
  type(dct_updated_) :: upd_this, upd_next
  type(dct_fdr_) :: dct_fdr
  real(8), allocatable :: grdara(:)
  real(8), allocatable :: lat(:)
  integer(4), allocatable :: fdrmap_rri_row(:)

  real(8) :: c_param_width, a_param_width
  real(8) :: c_param_depth, a_param_depth

  real(8), allocatable :: node_lon(:), node_lat(:)
  integer, allocatable :: node_jCh(:), node_jNode(:)
  integer, pointer :: lst_bsn(:)
  integer, pointer :: arg(:)
  real(8) :: west, east, south, north
  real(8) :: awest, aeast, asouth, anorth
  real(8) :: bwest, beast, bsouth, bnorth
  integer :: gxs, gxe, gys, gye
  integer :: agxs, agxe, agys, agye, amx, amy
  integer :: bgxs, bgxe, bgys, bgye
  integer :: gx0, gx1, gx, gxx, igx
  integer :: gy0, gy1, gy, gyy, igy
  integer :: gx_this, gx_next
  integer :: sgn_x, sgn_y
  real(8) :: lon0, lon1, lon_this, lon_next, clon_this, clon_next
  real(8) :: lat0, lat1, lat_this, lat_next, clat_this, clat_next
  real(8) :: leng, leng_base
  integer :: n_inflw, n_outflw
  real(8) :: angle
  real(8) :: upa
  integer :: bsn, bsn_prev

  integer :: jCh, jCh2, iiCh
  integer :: jPt
  integer :: nNode, iNode, jNode, jNode2
  integer :: jSeq, jSeq0, jSeq1
  integer :: iUpd
  integer :: iFdr, iFdrs, iFdre
  integer :: nBsn, iBsn
  integer :: i, i_prev
  integer :: is, ie, iis, iie
  integer :: sz
  integer :: n, n_valid, n_updated

  integer :: un
  character(CLEN_PATH) :: f
  character(CLEN_WFMT) :: wfmt

  integer(4), allocatable :: fdrmap_rri(:,:)
  character(8) :: var
  integer :: iVar

  character(2) :: arrow
  logical :: debug

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(resolution)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Reading data')

  allocate(character(1) :: nwk%uid)
  nwk%uid = uid

  f = strnk_get_f_network_channel(nwk%uid, 'sbi')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')
  read(un) nwk%nCh, west, east, south, north
  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    read(un) ch%n, ch%west, ch%east, ch%south, ch%north
    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))
    allocate(ch%node(2))
    read(un) ch%lon
    read(un) ch%lat
    read(un) ch%node(1)%typ, ch%node(2)%typ
  enddo
  close(un)

  gxs = gxs_of_lon(west)
  gxe = gxe_of_lon(east)
  gys = gys_of_lat(north)
  gye = gye_of_lat(south)

  call logmsg('BBox     : '//sbbox(west,east,south,north))
  call logmsg('Mesh BBox: '//sbbox(west_of_gx(gxs),east_of_gx(gxe),&
              south_of_gy(gye),north_of_gy(gys)))
  call logmsg('Mesh range: '//sMeshRange(gxs,gxe,gys,gye))

  allocate(fdrmap(gxs:gxe,gys:gye))
  call read_map_from_tile(&
      resolution, 'dir', DTYPE_INT1, FDR_MISS, gxs, gys, fdrmap)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating channel length in each grid')

  wfmt = 'es20.13'

  open(newunit=un, file='tmp/mesh_channel.txt', status='replace')

  do jCh = 1, nwk%nCh
debug = jCh == 3555
!if( .not. debug ) cycle
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    ch => nwk%channel(jCh)

    seq => ch%seq
    seq%sz = 1024
    seq%n = 0
    allocate(seq%gx(0:seq%sz))
    allocate(seq%gy(0:seq%sz))
    allocate(seq%leng(0:seq%sz))
    allocate(seq%angle(0:seq%sz))
    seq%gx(0) = 0
    seq%gy(0) = 0
    seq%leng(0) = 0.d0
    seq%angle(0) = 0.d0

    lon1 = ch%lon(1)
    lat1 = ch%lat(1)
    gx1 = gxs_of_lon(lon1)
    gy1 = gys_of_lat(lat1)
    do jPt = 2, ch%n
      lon0 = lon1
      lat0 = lat1
      gx0 = gx1
      gy0 = gy1
      lon1 = ch%lon(jPt)
      lat1 = ch%lat(jPt)
      gx1 = gxs_of_lon(lon1)
      gy1 = gys_of_lat(lat1)
      if( debug )then
        call logmsg('jPt: '//str(jPt)//&
            '\n  (lon0,lat0): ('//str((/lon0,lat0/),wfmt,',')//')'//&
              ', (x0,y0): ('//str((/gx0,gy0/),DGT_GXY,',')//')'//&
            '\n  (lon1,lat1): ('//str((/lon1,lat1/),wfmt,',')//')'//&
              ', (x1,y1): ('//str((/gx1,gy1/),DGT_GXY,',')//')')
        call setlog('+x2')
      endif

      if( gy0 == gy1 )then
        sgn_y = 1
      else
        sgn_y = sign(1, gy1-gy0)
      endif
      if( gx0 == gx1 )then
        sgn_x = 1
      else
        sgn_x = sign(1, gx1-gx0)
      endif

      do igy = gy0, gy1, sgn_y
        if( igy == gy0 )then
          lon_this = lon0
          lat_this = lat0
        else
          lon_this = lon_next
          lat_this = lat_next
        endif
        if( igy == gy1 )then
          lon_next = lon1
          lat_next = lat1
        else
          lat_next = center_of_gy(igy) - GRIDSIZE_LAT*0.5d0*sgn_y
          lon_next = apprx_isct_with_parallel(lon0, lat0, lon1, lat1, lat_next)
        endif

        gx_this = gxs_of_lon(lon_this)
        gx_next = gxs_of_lon(lon_next)
        do igx = gx_this, gx_next, sgn_x
          if( igx == gx_this )then
            clon_this = lon_this
            clat_this = lat_this
          else
            clon_this = clon_next
            clat_this = clat_next
          endif
          if( igx == gx_next )then
            clon_next = lon_next
            clat_next = lat_next
          else
            clon_next = center_of_gx(igx) + GRIDSIZE_LON*0.5d0*sgn_x
            clat_next = apprx_isct_with_meridian(lon0, lat0, lon1, lat1, clon_next)
          endif

          leng = dist_sphere(clon_this*d2r, clat_this*d2r, clon_next*d2r, clat_next*d2r)

          call update_seq(igx, igy, leng)

          if( debug )then
            call logmsg('gy: '//str(igy,DGT_GXY)//' gx: '//str((/gx_this,gx_next/),DGT_GXY,' - ')//&
                ' leng: '//str(leng))
          endif
        enddo  ! igx/
      enddo  ! igy/

      if( debug )then
        call setlog('-x2')
      endif
    enddo  ! jPt/

    call realloc(seq%gx  , (/1/), (/seq%n/), clear=.false.)
    call realloc(seq%gy  , (/1/), (/seq%n/), clear=.false.)
    call realloc(seq%leng, (/1/), (/seq%n/), clear=.false.)
    !-----------------------------------------------------------
    ! 
    !-----------------------------------------------------------
    seq%angle_mean = 0.d0
    n = 0
    do jSeq = 1, seq%n
      if( seq%angle(jSeq) == SEQ_ANGLE_UNDEF ) cycle
      call add(seq%angle_mean, seq%angle(jSeq))
      call add(n)
    enddo
    if( n == 0 )then
      seq%angle_mean = SEQ_ANGLE_UNDEF
    else
      call mul(seq%angle_mean, 1.d0/n)
    endif
    if( debug )then
      call logmsg('Angle mean: '//str(seq%angle_mean))
    endif
    !-----------------------------------------------------------
    ! (debug) Print messages of channel's head and tail
    !-----------------------------------------------------------
    !if( .not. debug )then
    !  if( jCh <= 3 .or. jCh >= nwk%nCh-2 )then
    !    call logmsg('ch('//str(jCh,dgt(nwk%nCh))//') '//&
    !        ' n: '//str(ch%n)//' '//&
    !        slonlat(ch%lon(1),ch%lat(1))//' - '//slonlat(ch%lon(ch%n),ch%lat(ch%n)))
    !    do i = 1, seq%n
    !      if( i <= 3 .or. i >= seq%n-2 )then
    !        call logmsg('  ('//str((/seq%gx(i),seq%gy(i)/),DGT_GXY,',')//') '//&
    !            slonlat(center_of_gx(seq%gx(i)),center_of_gy(seq%gy(i))))
    !      elseif( i == 4 )then
    !        call logmsg('  ...')
    !      endif
    !    enddo
    !  elseif( jCh == 4 )then
    !    call logmsg('...')
    !  endif
    !endif
    !-----------------------------------------------------------
    ! (debug) Output lon, lat
    !-----------------------------------------------------------
    allocate(seq%lon(seq%n))
    allocate(seq%lat(seq%n))
    do i = 1, seq%n
      seq%lon(i) = center_of_gx(seq%gx(i))
      seq%lat(i) = center_of_gy(seq%gy(i))
    enddo

    write(un,"("//str(seq%n)//"(1x,f12.7))") seq%lon
    write(un,"("//str(seq%n)//"(1x,f12.7))") seq%lat

    deallocate(seq%lon)
    deallocate(seq%lat)
    !-----------------------------------------------------------
    ! Set status of meshes
    !-----------------------------------------------------------
    allocate(seq%stat(seq%n))
    allocate(seq%ratio(seq%n))

    do i = 1, seq%n
      gx = seq%gx(i)
      gy = seq%gy(i)
      leng_base = dist_sphere(&
          west_of_gx(gx)*d2r, south_of_gy(gy)*d2r, &
          east_of_gx(gx)*d2r, north_of_gy(gy)*d2r)
      seq%ratio(i) = seq%leng(i) / leng_base
      if( seq%ratio(i) > THRESH_LENG_RATIO )then
        seq%stat(i) = SEQ_STAT__USED
      else
        seq%stat(i) = SEQ_STAT__SKIPPED
      endif
    enddo

    if( all(seq%stat == SEQ_STAT__SKIPPED) )then
      jSeq = maxloc(seq%ratio(:), 1)
      seq%stat(jSeq) = SEQ_STAT__USED
    endif
    !-----------------------------------------------------------
    ! (debug) Print messages of connection
    !-----------------------------------------------------------
    if( debug )then
      do i_prev = 1, seq%n
        if( seq%stat(i_prev) == SEQ_STAT__USED ) exit
      enddo
      do i = i_prev+1, seq%n
        if( seq%stat(i) == SEQ_STAT__USED )then
          if( abs(seq%gx(i) - seq%gx(i_prev)) <= 1 .and. &
              abs(seq%gy(i) - seq%gy(i_prev)) <= 1 )then
            arrow = '->'
          else
            arrow = '--'
          endif
          call logmsg('('//str((/seq%gx(i_prev),seq%gy(i_prev)/),DGT_GXY,',')//&
              ') '//arrow//' ('//str((/seq%gx(i),seq%gy(i)/),DGT_GXY,',')//')')
          i_prev = i
        endif
      enddo
    endif
    !-----------------------------------------------------------
    ! (debug) Output lon, lat of meshes with enough length
    !-----------------------------------------------------------
    allocate(seq%lon(seq%n))
    allocate(seq%lat(seq%n))

    n_valid = 0
    do i = 1, seq%n
      if( seq%stat(i) == SEQ_STAT__SKIPPED ) cycle
      call add(n_valid)
      seq%lon(n_valid) = center_of_gx(seq%gx(i))
      seq%lat(n_valid) = center_of_gy(seq%gy(i))
    enddo

    if( debug )then
      call logmsg('Grids with length above thresh: '//&
          str(n_valid)//' / '//str(seq%n))
    endif

    write(un,"("//str(n_valid)//"(1x,f12.7))") seq%lon(:n_valid)
    write(un,"("//str(n_valid)//"(1x,f12.7))") seq%lat(:n_valid)
    write(un,"(es20.13)") seq%angle_mean

    deallocate(seq%lon)
    deallocate(seq%lat)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    nullify(seq)
  enddo  ! jCh/

  close(un)

  deallocate(fdrmap)

  call logext()
  !---------------------------------------------------------------
  ! Make a list of nodes
  !---------------------------------------------------------------
  call logent('Making a list of nodes')

  nNode = nwk%nCh * 2
  allocate(node_lon(nNode))
  allocate(node_lat(nNode))
  allocate(node_jCh(nNode))
  allocate(node_jNode(nNode))
  allocate(arg(nNode))
  iNode = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    do jNode = 1, 2
      jPt = jNode2jPt(jNode, ch%n)
      call add(iNode)
      node_lon(iNode) = ch%lon(jPt)
      node_lat(iNode) = ch%lat(jPt)
      node_jCh(iNode) = jCh
      node_jNode(iNode) = jNode
    enddo
  enddo
  call argsort(node_lon, arg)
  call sort(node_lon, arg)
  call sort(node_lat, arg)
  call sort(node_jCh, arg)
  call sort(node_jNode, arg)

  nwk%nNode = 0
  ie = 0
  do while( ie < nNode )
    is = ie + 1
    ie = is
    do while( ie < nNode )
      if( node_lon(ie+1) /= node_lon(is) ) exit
      ie = ie + 1
    enddo
    if( is == ie )then
      call add(nwk%nNode)
      cycle
    endif
    call argsort(node_lat(is:ie), arg(is:ie))
    call sort(node_lat(is:ie), arg(is:ie))
    call sort(node_jNode(is:ie), arg(is:ie))
    call sort(node_jCh(is:ie), arg(is:ie))
    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( node_lat(iie+1) /= node_lat(iis) ) exit
        iie = iie + 1
      enddo
      call add(nwk%nNode)
    enddo  ! iis, iie/
  enddo  ! is, ie/

  allocate(nwk%node(nwk%nNode))

  iNode = 0
  ie = 0
  do while( ie < nNode )
    is = ie + 1
    ie = is
    do while( ie < nNode )
      if( node_lon(ie+1) /= node_lon(is) ) exit
      ie = ie + 1
    enddo
    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( node_lat(iie+1) /= node_lat(iis) ) exit
        iie = iie + 1
      enddo
      call add(iNode)
      nwknode => nwk%node(iNode)
      nwknode%lon = node_lon(iis)
      nwknode%lat = node_lat(iis)
      nwknode%gx = gxs_of_lon(nwknode%lon)
      nwknode%gy = gys_of_lat(nwknode%lat)
      nwknode%nCh = iie - iis + 1
      allocate(nwknode%jCh(nwknode%nCh))
      allocate(nwknode%jNode(nwknode%nCh))
      do i = iis, iie
        nwknode%jCh(i-iis+1) = node_jCh(i)
        nwknode%jNode(i-iis+1) = node_jNode(i)
        node => nwk%channel(node_jCh(i))%node(node_jNode(i))
        node%i = iNode
        node%is_outlet = nwknode%nCh == 1 .and. node_jNode(i) == 2
      enddo  ! i/
    enddo  ! iis, iie/
  enddo  ! is, ie/

  deallocate(node_lon)
  deallocate(node_lat)
  deallocate(node_jCh)
  deallocate(node_jNode)
  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Updating status of channel meshes')

  open(newunit=un, file='tmp/node_strange.txt', status='replace')

  do iNode = 1, nwk%nNode
    nwknode => nwk%node(iNode)
    selectcase( nwknode%nCh )
    !-----------------------------------------------------------
    ! Case: Source or outlet
    case( 1 )
      ch => nwk%channel(nwknode%jCh(1))
      jNode = nwknode%jNode(1)
      seq => ch%seq
      jSeq = jNode2jSeq(jNode, seq%n)
      if( seq%gx(jSeq) /= nwknode%gx .or. seq%gy(jSeq) /= nwknode%gy )then
        call errend('nwknode%nCh: '//str(nwknode%nCh))
      endif
      seq%stat(jSeq) = SEQ_STAT__USED
    !-----------------------------------------------------------
    ! Case: Intermediate node
    case( 2 )
      leng = 0.d0
      n_outflw = 0
      n_inflw = 0
      do iiCh = 1, nwknode%nCh
        ch => nwk%channel(nwknode%jCh(iiCh))
        jNode = nwknode%jNode(iiCh)
        seq => ch%seq
        jSeq = jNode2jSeq(jNode, seq%n)
        if( seq%gx(jSeq) /= nwknode%gx .or. seq%gy(jSeq) /= nwknode%gy )then
          call errend('nwknode%nCh: '//str(nwknode%nCh))
        endif
        call add(leng, seq%leng(jSeq))

        if( jNode == 1 )then
          call add(n_outflw)
        else
          call add(n_inflw)
        endif
      enddo  ! iiCh/

      if( (nwknode%jNode(1) == 1 .and. nwknode%jNode(2) == 1) .or. &
          (nwknode%jNode(1) /= 1 .and. nwknode%jNode(2) /= 1) )then
        call logwrn('nwk%node('//str(iNode)//')'//&
            ' nCh: '//str(nwknode%nCh)//', In: '//str(n_inflw)//', Out: '//str(n_outflw))
        write(un,"(a)") str((/nwknode%nCh,n_inflw,n_outflw/))//&
                        ' '//str((/nwknode%lon,nwknode%lat/),'es20.13')
      endif

      leng_base = dist_sphere(&
          west_of_gx(nwknode%gx)*d2r, south_of_gy(nwknode%gy)*d2r, &
          east_of_gx(nwknode%gx)*d2r, north_of_gy(nwknode%gy)*d2r)

      if( leng / leng_base > THRESH_LENG_RATIO )then
        do iiCh = 1, nwknode%nCh
          ch => nwk%channel(nwknode%jCh(iiCh))
          jNode = nwknode%jNode(iiCh)
          seq => ch%seq
          jSeq = jNode2jSeq(jNode, seq%n)
          seq%stat(jSeq) = SEQ_STAT__USED
        enddo
      endif
    !-----------------------------------------------------------
    ! Case: Junction or bifurcation
    case( 3: )
      n_inflw = 0
      n_outflw = 0
      do iiCh = 1, nwknode%nCh
        ch => nwk%channel(nwknode%jCh(iiCh))
        jNode = nwknode%jNode(iiCh)
        seq => ch%seq
        jSeq = jNode2jSeq(jNode, seq%n)
        if( seq%gx(jSeq) /= nwknode%gx .or. seq%gy(jSeq) /= nwknode%gy )then
          call errend('nwknode%nCh: '//str(nwknode%nCh))
        endif
        seq%stat(jSeq) = SEQ_STAT__USED

        if( jNode == 1 )then
          call add(n_outflw)
        else
          call add(n_inflw)
        endif
      enddo  ! iiCh/

      if( n_outflw == nwknode%nCh .or. n_inflw == nwknode%nCh )then
        call logwrn('nwk%node('//str(iNode)//')'//&
            ' nCh: '//str(nwknode%nCh)//', In: '//str(n_inflw)//', Out: '//str(n_outflw))
        write(un,"(a)") str((/nwknode%nCh,n_inflw,n_outflw/))//&
                        ' '//str((/nwknode%lon,nwknode%lat/),'es20.13')
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('nwknode%nCh', nwknode%nCh))
    endselect
  enddo  ! iNode/

  close(un)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Getting upper area')

  allocate(upamap(gxs:gxe,gys:gye))
  call read_map_from_tile(&
      resolution, 'upa', DTYPE_REAL, UPA_MISS, gxs, gys, upamap)

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    do jNode = 1, 2
      node => ch%node(jNode)
      if( jNode == 1 )then
        node%theta = get_theta(ch%lon(1), ch%lat(1), ch%lon(2), ch%lat(2))
      else
        node%theta = get_theta(ch%lon(ch%n-1), ch%lat(ch%n-1), ch%lon(ch%n), ch%lat(ch%n))
      endif
    enddo  ! jNode/
    !call logmsg('Ch('//str(jCh,dgt(nwk%nCh))//') theta: '//str(ch%node(:)%theta))
  enddo  ! jCh/

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    seq => ch%seq

    ch%upa = CH_UPA_MISS

    if( seq%n < THRESH_SEQ_N ) cycle

    do jSeq = seq%n/3, seq%n/3*2
      ch%upa = max(ch%upa, real(upamap(seq%gx(jSeq),seq%gy(jSeq)),8))
    enddo  ! jSeq/
  enddo  ! jCh/

  do
    n_updated = 0
    do jCh = 1, nwk%nCh
      ch => nwk%channel(jCh)
      seq => ch%seq
      if( ch%upa <= 0.d0 ) cycle

      node => ch%node(2)
      nwknode => nwk%node(node%i)
      do iiCh = 1, nwknode%nCh
        jCh2 = nwknode%jCh(iiCh)
        jNode2 = nwknode%jNode(iiCh)
        if( jCh2 == jCh ) cycle
        if( jNode2 == 2 ) cycle
        ch2 => nwk%channel(jCh2)
        node2 => ch2%node(jNode2)
        angle = londiff_deg(node%theta, node2%theta)
        if( angle < 90.d0 )then
          upa = ch%upa * cos(angle*d2r)
          if( (upa - ch2%upa) / upa > 1d-2 )then
            call add(n_updated)
            !call logmsg('jCh: '//str(jCh,dgt(nwk%nCh))//', jCh2: '//str(jCh2,dgt(nwk%nCh))//&
            !    ' upa: '//str(ch2%upa)//' -> '//str(upa)//' ('//str(upa-ch2%upa)//')'//&
            !    ' angle: '//str(angle))
            ch2%upa = upa
          endif
        endif
      enddo  ! iiCh/
    enddo  ! jCh/

    call logmsg('Updated: '//str(n_updated))
    if( n_updated == 0 ) exit
  enddo  !/

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( ch%upa /= CH_UPA_MISS .and. ch%upa < 0.d0 )then
      call errend('ch%upa < 0')
    endif
  enddo

  open(newunit=un, file='tmp/channel_upa.txt', status='replace')
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    write(un,*) ch%upa
  enddo
  close(un)

  call logext()
  !-------------------------------------------------------------
  ! Calc. channel intersection params.
  !-------------------------------------------------------------
  call logent('Calculating channel intersection params.')

  c_param_width = 6.387d0
  a_param_width = 0.427d0
  c_param_depth = 2.984d0
  a_param_depth = 0.159d0

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( ch%upa == CH_UPA_MISS )then
      ch%width = CH_WIDTH_MIN
      ch%depth = CH_DEPTH_MIN
      ch%levee = 0.d0
    else
      ch%width = max(c_param_width * ch%upa**a_param_width, CH_WIDTH_MIN)
      ch%depth = max(c_param_depth * ch%upa**a_param_depth, CH_DEPTH_MIN)
      ch%levee = 0.d0
    endif
  enddo

  call logmsg('width max: '//str(maxval(nwk%channel(:)%width))//' (m)')
  call logmsg('depth max: '//str(maxval(nwk%channel(:)%depth))//' (m)')
  call logmsg('levee max: '//str(maxval(nwk%channel(:)%levee))//' (m)')

  call logext()
  !-------------------------------------------------------------
  ! Make input file for RRI
  !-------------------------------------------------------------
  call logent('Making input file for RRI')

  f = joint_get_f_chn_rri(resolution, nwk%uid)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(a,1x,i0)") 'channels', nwk%nCh
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    write(un,"(2x,a,1x,i0)") 'channel', jCh
    write(un,"(2x,a,2(1x,l1))") 'node_outlet', ch%node(:)%is_outlet
    write(un,"(2x,a,1x,i0)") 'points', ch%n
    write(un,"(2x,a,"//str(ch%n)//"(1x,f12.7))") 'lon', ch%lon(:)
    write(un,"(2x,a,"//str(ch%n)//"(1x,f12.7))") 'lat', ch%lat(:)
    write(un,"(2x,a,1x,es10.3)") 'width', ch%width
    write(un,"(2x,a,1x,es10.3)") 'depth', ch%depth
    write(un,"(2x,a,1x,es10.3)") 'levee', ch%levee
  enddo
  close(un)

  call logext()
  !-------------------------------------------------------------
  ! Make basin maps
  !-------------------------------------------------------------
  call logent('Making basin maps')

  allocate(bsnmap(gxs:gxe,gys:gye))
  call read_map_from_tile(&
      resolution, 'bsn', DTYPE_INT4, BSN_MISS, gxs, gys, bsnmap)

  allocate(lst_bsn(32))

  nBsn = 0
  bsn_prev = BSN_MISS
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    seq => ch%seq
    do jSeq = 1, seq%n
      bsn = bsnmap(seq%gx(jSeq),seq%gy(jSeq))
      if( bsn == BSN_MISS ) cycle
      if( bsn == bsn_prev ) cycle
      bsn_prev = bsn
      if( nBsn == size(lst_bsn) )then
        call realloc(lst_bsn, nBsn*2, clear=.false.)
      endif
      call add(nBsn)
      lst_bsn(nBsn) = bsn
    enddo  ! jSeq
  enddo  ! jCh/

  call sort(lst_bsn(:nBsn))

  iBsn = 0
  ie = 0
  do while( ie < nBsn )
    is = ie + 1
    ie = is
    do while( ie < nBsn )
      if( lst_bsn(ie+1) /= lst_bsn(is) ) exit
      ie = ie + 1
    enddo
    call add(iBsn)
    lst_bsn(iBsn) = lst_bsn(is)
  enddo

  nBsn = iBsn
  call realloc(lst_bsn, nBsn, clear=.false.)

  agxs = gxs
  agxe = gxe
  agys = gys
  agye = gye
  nBsn = 0
  do iBsn = 1, size(lst_bsn)
    if( access(get_f_dat_basin(resolution, 'range', lst_bsn(iBsn)),' ') /= 0 )then
      call logmsg('bsn '//str(lst_bsn(iBsn),dgt(maxval(lst_bsn)))//&
                  ': Not found')
      cycle
    endif
    call read_basin_range_from_each(resolution, lst_bsn(iBsn), &
        bgxs, bgxe, bgys, bgye, bwest, beast, bsouth, bnorth)
    call logmsg('bsn '//str(lst_bsn(iBsn),dgt(maxval(lst_bsn)))//&
                ': '//sMeshRange(bgxs,bgxe,bgys,bgye))
    agxs = min(agxs, bgxs)
    agxe = max(agxe, bgxe)
    agys = min(agys, bgys)
    agye = max(agye, bgye)

    call add(nBsn)
    lst_bsn(nBsn) = lst_bsn(iBsn)
  enddo

  if( nBsn == 0 )then
    call logwrn('No valid basin exists.')
    call logext()
    call logret(PRCNAM, MODNAM)
    return
  endif

  amx = agxe - agxs + 1
  amy = agye - agys + 1

  awest = west_of_gx(agxs)
  aeast = east_of_gx(agxe)
  asouth = south_of_gy(gye)
  anorth = north_of_gy(gys)

  call logmsg('Basins: '//str(nBsn))
  call logmsg('Mesh range: '//sMeshRange(gxs,gxe,gys,gye)//&
            '\n         -> '//sMeshRange(agxs,agxe,agys,agye))

  call realloc(lst_bsn, nBsn, clear=.false.)

  deallocate(bsnmap)
  allocate(bsnmap(agxs:agxe,agys:agye))
  call read_map_from_tile(&
      resolution, 'bsn', DTYPE_INT4, BSN_MISS, agxs, agys, bsnmap)

  bsn_prev = BSN_MISS
  do igy = agys, agye
  do igx = agxs, agxe
    bsn = bsnmap(igx,igy)
    if( bsn == BSN_MISS ) cycle
    if( bsn /= bsn_prev )then
      bsn_prev = bsn
      call search(bsn, lst_bsn, iBsn)
    endif
    if( iBsn == 0 ) bsnmap(igx,igy) = BSN_MISS
  enddo  ! igx/
  enddo  ! igy/

  ! Make topo. maps (bin)
  !-------------------------------------------------------------
  allocate(elvmap(agxs:agxe,agys:agye))
  allocate(upgmap(agxs:agxe,agys:agye))
  allocate(fdrmap(agxs:agxe,agys:agye))

  call read_map_from_tile(&
      resolution, 'elv', DTYPE_REAL, ELV_MISS, agxs, agys, elvmap)
  where( bsnmap == BSN_MISS ) elvmap = ELV_MISS

  f = joint_get_f_nwk_map(resolution, nwk%uid, 'elv')
  call logmsg('Writing '//str(f))
  call traperr( wbin(elvmap, f, replace=.true.) )

  call read_map_from_tile(&
      resolution, 'upg', DTYPE_INT4, UPG_MISS, agxs, agys, upgmap)
  where( bsnmap == BSN_MISS ) upgmap = UPG_MISS

  f = joint_get_f_nwk_map(resolution, nwk%uid, 'upg')
  call logmsg('Writing '//str(f))
  call traperr( wbin(upgmap, f, replace=.true.) )

  call read_map_from_tile(&
      resolution, 'dir', DTYPE_INT1, FDR_MISS, agxs, agys, fdrmap)
  where( bsnmap == BSN_MISS ) fdrmap = FDR_MISS

  f = joint_get_f_nwk_map(resolution, nwk%uid, 'dir')
  call logmsg('Writing '//str(f))
  call traperr( wbin(fdrmap, f, replace=.true.) )

  allocate(fdrmap_rri(agxs:agxe,agys:agye))
  call joint_conv_fdr_jflw2rri(fdrmap, fdrmap_rri)
  f = joint_get_f_nwk_map(resolution, nwk%uid, 'dir_rri')
  call logmsg('Writing '//str(f))
  call traperr( wbin(fdrmap_rri, f, replace=.true.) )


  ! Make topo. files (RRI)
  !-------------------------------------------------------------
if( .false. )then
  do iVar = 1, 3
    selectcase( iVar )
    case( 1 ); var = 'elv'
    case( 2 ); var = 'upg'
    case( 3 ); var = 'dir'
    endselect

    f = joint_get_f_nwk_map_rri(resolution, nwk%uid, var)
    call logmsg('Writing '//str(f))
    open(newunit=un, file=f, status='replace')
    write(un,*) 'nx', amx
    write(un,*) 'ny', amy
    write(un,*) 'west', awest
    write(un,*) 'south', asouth
    write(un,*) 'cellsize', GRIDSIZE_LON
    write(un,*) 'nodata', ELV_MISS
    selectcase( iVar )
    case( 1 )
      wfmt = "("//str(amx)//"(1x,f8.2))"
      do igy = agys, agye
        write(un,wfmt) elvmap(:,igy)
      enddo
    case( 2 )
      wfmt = "("//str(amx)//"(1x,i"//str(dgt(upgmap,DGT_OPT_MAX))//"))"
      do igy = agys, agye
        write(un,wfmt) upgmap(:,igy)
      enddo
    case( 3 )
      wfmt = "("//str(amx)//"(1x,i4))"
      allocate(fdrmap_rri_row(agxs:agxe))
      do igy = agys, agye
        call joint_conv_fdr_jflw2rri(fdrmap(:,igy), fdrmap_rri_row)
        write(un,wfmt) fdrmap_rri_row(:)
      enddo
      deallocate(fdrmap_rri_row)
    endselect
    close(un)
  enddo  ! iVar/
endif

  deallocate(elvmap)
  deallocate(upgmap)
  deallocate(fdrmap)
  deallocate(fdrmap_rri)
  !-------------------------------------------------------------
  deallocate(bsnmap)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = joint_get_f_nwk_info(resolution, nwk%uid)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(a)") 'nx '//str(amx,DGT_GXY)//' gx '//str((/agxs,agxe/),DGT_GXY)
  write(un,"(a)") 'ny '//str(amy,DGT_GXY)//' gy '//str((/agys,agye/),DGT_GXY)
  write(un,"(a)") 'BBox '//str((/awest,aeast/),'f12.7')//' '//str((/asouth,anorth/),'f11.7')
  write(un,"(a)") 'elv_miss '//str(ELV_MISS)
  write(un,"(a)") 'fdr_miss '//str(FDR_MISS)
  write(un,"(a)") 'upg_miss '//str(UPG_MISS)
  write(un,"(a)")
  close(un)
  !-------------------------------------------------------------
  ! Modify flwdir map
  !-------------------------------------------------------------
if( .false. )then
  call logent('Modifying flwdir map')

  dct_fdr%n = 0
  sz = 1024
  allocate(dct_fdr%gx_up(sz))
  allocate(dct_fdr%gy_up(sz))
  allocate(dct_fdr%gx_dn(sz))
  allocate(dct_fdr%gy_dn(sz))

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    seq => ch%seq
    jSeq = 1
    do while( seq%stat(jSeq) == SEQ_STAT__SKIPPED )
      jSeq = jSeq + 1
      if( jSeq > seq%n )then
        call errend('jSeq > seq%n')
      endif
    enddo
    seq%jmin_used = jSeq

    jSeq = seq%n
    do while( seq%stat(jSeq) == SEQ_STAT__SKIPPED )
      jSeq = jSeq - 1
      if( jSeq < 1 )then
        call errend('jSeq < 1')
      endif
    enddo
    seq%jmax_used = jSeq
  enddo  ! jCh/

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    seq => ch%seq

    jSeq1 = seq%jmin_used
    do while( jSeq1 <= seq%n )
      jSeq0 = jSeq1
      jSeq1 = jSeq1 + 1
      if( jSeq1 > seq%n ) exit
      do while( seq%stat(jSeq1) == SEQ_STAT__SKIPPED )
        jSeq1 = jSeq1 + 1
        if( jSeq1 > seq%n ) exit
      enddo

      if( jSeq1 > seq%n ) exit

      call update_dct_fdr(&
          dct_fdr, seq%gx(jSeq0), seq%gy(jSeq0), &
          seq%gx(jSeq1), seq%gy(jSeq1))
    enddo

    if( jSeq0 /= seq%jmax_used )then
      call errend('jSeq0 /= seq%jmax_used')
    endif
    jSeq = jSeq0

    iNode = ch%node(2)%i
    nwknode => nwk%node(iNode)

    selectcase( nwknode%nCh )
    !-----------------------------------------------------------
    ! Case: Outlet
    case( 1 )
      fdrmap(gx,gy) = FDR_RIVERMOUTH
    !-----------------------------------------------------------
    ! Case: Intermediate node
    case( 2 )
      do iiCh = 1, nwknode%nCh
        jCh2 = nwknode%jCh(iiCh)
        if( jCh2 == jCh ) cycle
        seq2 => nwk%channel(jCh2)%seq
        jNode2 = nwknode%jNode(iiCh)
        if( jNode2 == 1 )then
          call update_dct_fdr(&
              dct_fdr, seq%gx(jSeq), seq%gy(jSeq), &
              seq2%gx(seq2%jmin_used), seq2%gy(seq2%jmin_used))
        endif
      enddo  ! iiCh/
    !-----------------------------------------------------------
    ! Case: Junction or bifurcation
    case( 3: )
      call update_dct_fdr(&
          dct_fdr, seq%gx(jSeq), seq%gy(jSeq), &
          nwknode%gx, nwknode%gy)

      do iiCh = 1, nwknode%nCh
        jCh2 = nwknode%jCh(iiCh)
        if( jCh2 == jCh ) cycle
        seq2 => nwk%channel(jCh2)%seq
        jNode2 = nwknode%jNode(iiCh)
        if( jNode2 == 1 )then
          call update_dct_fdr(&
              dct_fdr, seq%gx(jSeq), seq%gy(jSeq), &
              seq2%gx(seq2%jmin_used), seq2%gy(seq2%jmin_used))
        endif
      enddo  ! iiCh/
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('nwknode%nCh', nwknode%nCh))
    endselect
  enddo  ! jCh/

  call logmsg('Table length: '//str(dct_fdr%n))

  call realloc(dct_fdr%gx_up, dct_fdr%n, clear=.false.)
  call realloc(dct_fdr%gy_up, dct_fdr%n, clear=.false.)
  call realloc(dct_fdr%gx_dn, dct_fdr%n, clear=.false.)
  call realloc(dct_fdr%gy_dn, dct_fdr%n, clear=.false.)

  ! Sort by gx_up and gy_up
  allocate(arg(dct_fdr%n))
  call argsort(dct_fdr%gx_up, arg)
  call sort(dct_fdr%gx_up, arg)
  call sort(dct_fdr%gy_up, arg)
  call sort(dct_fdr%gx_dn, arg)
  call sort(dct_fdr%gy_dn, arg)
  ie = 0
  do while( ie < dct_fdr%n )
    is = ie + 1
    ie = is
    do while( ie < dct_fdr%n )
      if( dct_fdr%gx_up(ie+1) /= dct_fdr%gx_up(is) ) exit
      ie = ie + 1
    enddo
    call argsort(dct_fdr%gy_up(is:ie), arg(is:ie))
    call sort(dct_fdr%gy_up(is:ie), arg(is:ie))
    call sort(dct_fdr%gx_dn(is:ie), arg(is:ie))
    call sort(dct_fdr%gy_dn(is:ie), arg(is:ie))
  enddo  ! is, ie/
  deallocate(arg)

  ! Check
  do igy = gys, gye
  do igx = gxs, gxe
    if( fdrmap(igx,igy) /= FDR_NEW__TABLE ) cycle
    if( search_2(igx, igy, dct_fdr%gx_up, dct_fdr%gy_up, iFdrs, iFdre) /= 0 )then
      call errend('(gx,gy) not found in dct_fdr.')
    endif
  enddo  ! igx/
  enddo  ! igy/

  do iFdr = 1, dct_fdr%n
    if( fdrmap(dct_fdr%gx_up(iFdr),dct_fdr%gy_up(iFdr)) /= FDR_NEW__TABLE )then
      call errend('fdrmap /= NEW__TABLE')
    endif
  enddo  ! iFdr/

  call logext()
endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
if( .false. )then
  call logent('Calculating upper area')

  allocate(upgmap(gxs:gxe,gys:gye))
  call read_map_from_tile(&
      resolution, 'upg', DTYPE_INT4, UPG_MISS, gxs, gys, upgmap)

  allocate(upamap(gxs:gxe,gys:gye))
  upamap(:,:) = 0.d0

  allocate(grdara(gys:gye))
  allocate(lat(gys-1:gye))
  do igy = gys-1, gye
    lat(igy) = north_of_gy(igy)
  enddo
  grdara(:) = area_sphere_rect(lat(gys-1:gye-1), lat(gys:gye))
  deallocate(lat)

  call init_upd(upd_this, 1024)

  do igy = gys, gye
  do igx = gxs, gxe
    if( upgmap(igx,igy) /= 1 ) cycle

    selectcase( fdrmap(igx,igy) )
    !-----------------------------------------------------------
    ! Case: Normal
    case( 1: )
      call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)
      if( gxx <= 0 )then
        call errend('gxx <= 0')
      endif
      if( gxx < gxs .or. gxx > gxe .or. &
          gyy < gys .or. gyy > gye )then
        cycle
      endif
      call update_upd(upd_this, gxx, gyy, grdara(igy))
    !-----------------------------------------------------------
    ! Case:
    case( FDR_NEW__TABLE )

    !-----------------------------------------------------------
    ! Case: Single grid-basin
    case( FDR_RIVERMOUTH, FDR_INLAND )
      continue
    !-----------------------------------------------------------
    ! Case: ERROR (miss)
    case( FDR_MISS )
      call errend(msg_invalid_value('fdr', 'MISS'))
    !-----------------------------------------------------------
    ! Case: ERROR (undef)
    case( FDR_UNDEF )
      call errend(msg_invalid_value('fdr', 'UNDEF'))
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('fdr', fdrmap(igx,igy)))
    endselect
  enddo  ! igx/
  enddo  ! igy/
  call logmsg('Updated grids: '//str(upd_this%n))

  call init_upd(upd_next, max(2,upd_this%n/10))
  allocate(arg(upd_this%n))

  do while( upd_this%n > 0 )
    upd_next%n = 0
    do iUpd = 1, upd_this%n
      gx = upd_this%gx(iUpd)
      gy = upd_this%gy(iUpd)

      call add(upamap(gx,gy), real(upd_this%area(iUpd),4))
      
      selectcase( fdrmap(gx,gy) )
      !---------------------------------------------------------
      ! Case: 8-direction
      case( FDR_EAST, FDR_SOUTHEAST, FDR_SOUTH, FDR_SOUTHWEST, &
            FDR_WEST, FDR_NORTHWEST, FDR_NORTH, FDR_NORTHEAST )
        call get_nextxy(gx, gy, fdrmap(gx,gy), gxx, gyy)

        if( gxx < gxs .or. gxx > gxe .or. &
            gyy < gys .or. gyy > gye )then
          cycle
        endif

        call update_upd(upd_next, gxx, gyy, grdara(gy))
      !---------------------------------------------------------
      ! Case: Junction or bifurcation
      case( FDR_NEW__TABLE )
        if( search_2(gx, gy, dct_fdr%gx_up, dct_fdr%gy_up, iFdrs, iFdre) /= 0 )then
          call errend('(gx,gy) not found in dct_fdr.')
        endif
        do iFdr = iFdrs, iFdre
          call update_upd(upd_next, dct_fdr%gx_dn(iFdr), dct_fdr%gy_dn(iFdr), grdara(gy))
        enddo
      !---------------------------------------------------------
      ! Case: Outlet
      case( FDR_RIVERMOUTH, &
            FDR_INLAND )
        continue
      !---------------------------------------------------------
      ! Case: Go out of domain
      case( FDR_MISS )
        cycle
      !---------------------------------------------------------
      ! Case: ERROR (undef)
      case( FDR_UNDEF )
        call errend(msg_invalid_value('fdr', 'UNDEF'))
      !---------------------------------------------------------
      ! Case: ERROR
      case default
        call errend(msg_invalid_value('fdr', fdrmap(gx,gy)))
      endselect
    enddo  ! iUpd/

    if( upd_next%n == 0 )then
      upd_this%n = 0
      call logmsg('Updated grids: '//str(upd_this%n))
      exit
    endif

    if( upd_next%n > size(arg) ) call realloc(arg, upd_next%n, clear=.true.)

    upd_this%n = 0
    call argsort(upd_next%gy(:upd_next%n), arg(:upd_next%n))
    call sort(upd_next%gy(:upd_next%n), arg(:upd_next%n))
    call sort(upd_next%gx(:upd_next%n), arg(:upd_next%n))
    call sort(upd_next%area(:upd_next%n), arg(:upd_next%n))
    ie = 0
    do while( ie < upd_next%n )
      is = ie + 1
      ie = is
      do while( ie < upd_next%n )
        if( upd_next%gy(ie+1) /= upd_next%gy(is) ) exit
        ie = ie + 1
      enddo
      call argsort(upd_next%gx(is:ie), arg(is:ie))
      call sort(upd_next%gx(is:ie), arg(is:ie))
      call sort(upd_next%area(is:ie), arg(is:ie))
      iie = is - 1
      do while( iie < ie )
        iis = iie + 1
        iie = iis
        do while( iie < ie )
          if( upd_next%gx(iie+1) /= upd_next%gx(iis) ) exit
          iie = iie + 1
        enddo
        call add(upd_this%n)
        upd_this%gx(upd_this%n) = upd_next%gx(iis)
        upd_this%gy(upd_this%n) = upd_next%gy(iis)
        upd_this%area(upd_this%n) = sum(upd_next%area(iis:iie))
      enddo  ! iis, iie/
    enddo  ! is, ie/
    call logmsg('Updated grids: '//str(upd_this%n))
  enddo  ! while( upd_this%n > 0 )/

  deallocate(arg)

  call clear_upd(upd_this)
  call clear_upd(upd_next)

  call traperr( wbin(upamap, 'tmp/upa.bin') )

  deallocate(upgmap)

  call logext()
endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine update_seq(gx, gy, leng)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__update_seq'
  integer, intent(in) :: gx, gy
  real(8), intent(in) :: leng

  integer :: gxx, gyy
  integer :: gx_prev, gy_prev
  real(8) :: theta_fdr, theta_seq

  if( seq%gx(seq%n) /= gx .or. seq%gy(seq%n) /= gy )then
    if( seq%n == seq%sz )then
      call mul(seq%sz, 2)
      call realloc(seq%gx   , (/0/), (/seq%sz/), clear=.false.)
      call realloc(seq%gy   , (/0/), (/seq%sz/), clear=.false.)
      call realloc(seq%leng , (/0/), (/seq%sz/), clear=.false.)
      call realloc(seq%angle, (/0/), (/seq%sz/), clear=.false.)
    endif
    call add(seq%n)
    seq%gx(seq%n) = gx
    seq%gy(seq%n) = gy
    seq%leng(seq%n) = leng
    seq%angle(seq%n) = SEQ_ANGLE_UNDEF

    if( seq%n == 1 ) return

    ! Calc. angle b/w fdr of J-FlwDir
    gx_prev = seq%gx(seq%n-1)
    gy_prev = seq%gy(seq%n-1)

    selectcase( fdrmap(gx_prev,gy_prev) )
    case( 1: )
      continue
    case( FDR_RIVERMOUTH, FDR_INLAND, FDR_UNDEF, FDR_MISS )
      return
    case default
      call errend(msg_invalid_value('fdr', fdrmap(gx_prev,gy_prev)))
    endselect

    call get_nextxy(gx_prev, gy_prev, fdrmap(gx_prev,gy_prev), gxx, gyy)

    theta_fdr = atan2(center_of_gy(gyy) - center_of_gy(gy_prev), &
                      center_of_gx(gxx) - center_of_gx(gx_prev))
    theta_seq = atan2(center_of_gy(seq%gy(seq%n)) - center_of_gy(gy_prev), &
                      center_of_gx(seq%gx(seq%n)) - center_of_gx(gx_prev))
    seq%angle(seq%n-1) = londiff_rad(theta_fdr, theta_seq) * r2d
if( debug )then
  call logmsg('vec fdr: '//str((/center_of_gy(gyy) - center_of_gy(gy_prev),&
              center_of_gx(gxx) - center_of_gx(gx_prev)/),'es20.13',', ')//&
            '\n    seq: '//str((/center_of_gy(seq%gy(seq%n)) - center_of_gy(gy_prev),&
              center_of_gx(seq%gx(seq%n)) - center_of_gx(gx_prev)/),'es20.13',', ')//&
            '\ntheta fdr: '//str(theta_fdr*r2d)//', seq: '//str(theta_seq*r2d)//&
            '\nangle: '//str(seq%angle(seq%n-1)))
endif
  else
    call add(seq%leng(seq%n), leng)
  endif
end subroutine update_seq
!---------------------------------------------------------------
subroutine update_dct_fdr(&
    dct, gx_up, gy_up, gx_dn, gy_dn)
  implicit none
  type(dct_fdr_), intent(inout) :: dct
  integer, intent(in) :: gx_up, gy_up
  integer, intent(in) :: gx_dn, gy_dn

  if( gx_up == gx_dn .and. gy_up == gy_dn ) return

  fdrmap(gx_up,gy_up) = FDR_NEW__TABLE

  if( dct%n == size(dct%gx_up) )then
    call realloc(dct%gx_up, dct%n*2, clear=.false.)
    call realloc(dct%gy_up, dct%n*2, clear=.false.)
    call realloc(dct%gx_dn, dct%n*2, clear=.false.)
    call realloc(dct%gy_dn, dct%n*2, clear=.false.)
  endif
  call add(dct%n)
  dct%gx_up(dct%n) = gx_up
  dct%gy_up(dct%n) = gy_up
  dct%gx_dn(dct%n) = gx_dn
  dct%gy_dn(dct%n) = gy_dn
end subroutine update_dct_fdr
!---------------------------------------------------------------
subroutine init_upd(upd, sz)
  implicit none
  type(dct_updated_), intent(out) :: upd
  integer, intent(in) :: sz

  upd%n = 0
  allocate(upd%gx(sz))
  allocate(upd%gy(sz))
  allocate(upd%area(sz))
end subroutine init_upd
!---------------------------------------------------------------
subroutine update_upd(upd, gx, gy, area)
  implicit none
  type(dct_updated_), intent(inout) :: upd
  integer, intent(in) :: gx, gy
  real(8), intent(in) :: area

  if( upd%n == size(upd%gx) )then
    call realloc(upd%gx, upd%n*2, clear=.false.)
    call realloc(upd%gy, upd%n*2, clear=.false.)
    call realloc(upd%area, upd%n*2, clear=.false.)
  endif
  call add(upd%n)
  upd%gx(upd%n) = gx
  upd%gy(upd%n) = gy
  upd%area(upd%n) = area
end subroutine update_upd
!---------------------------------------------------------------
subroutine clear_upd(upd)
  implicit none
  type(dct_updated_), intent(inout) :: upd

  upd%n = 0
  deallocate(upd%gx)
  deallocate(upd%gy)
  deallocate(upd%area)
end subroutine clear_upd
!---------------------------------------------------------------
real(8) function get_theta(x0, y0, x1, y1) result(theta)
  implicit none
  real(8), intent(in) :: x0, y0, x1, y1

  theta = atan2(y1-y0, x1-x0) * r2d
end function get_theta
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
integer function jNode2jSeq(jNode, nSeq) result(jSeq)
  implicit none
  integer, intent(in) :: jNode, nSeq

  if( jNode == 1 )then
    jSeq = 1
  else
    jSeq = nSeq
  endif
end function jNode2jSeq
!===============================================================
!
!===============================================================
end module mod_modify_flwdir
