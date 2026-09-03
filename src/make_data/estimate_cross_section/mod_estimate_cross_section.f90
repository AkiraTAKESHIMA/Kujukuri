module mod_estimate_cross_section
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type
  use c1_util, only: &
        slonlat, &
        sBBox
  use c2_nlni_const, only: &
        DGT_WSCODE
  use c2_strnk_const, only: &
        DGT_NWKUID
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: determineChannelScale
  public :: estimateCrossSection
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_estimate_cross_section'

  type, extends(cmn_node_) :: node_
    logical :: is_outlet
    integer :: nCh_down
    integer, pointer :: jCh_down(:)
    integer :: nCh_down_angle1, nCh_down_angle2, nCh_down_angle3
    type(channel_down_), pointer :: down_angle1(:), down_angle2(:), down_angle3(:)
  end type

  type, extends(cmn_channel_) ::  channel_
    logical :: is_valid
    type(node_), pointer :: node(:)
    real(8) :: upa
    real(8) :: scale
    logical :: is_updated
  end type

  type, extends(cmn_watsys_) :: watsys_
  end type

  type channel_down_
    integer :: jCh
    real(8) :: angle
  end type

  type, extends(cmn_nwknode_) :: nwknode_
    integer :: gx, gy
  end type

  type, extends(cmn_network_) :: network_
    type(watsys_), pointer :: wsys(:)
    type(channel_), pointer :: channel(:)
    type(nwknode_), pointer :: node(:)
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  real(8), parameter :: RATIO_CUT_TAIL = 0.1d0
  real(8), parameter :: LENG_CUT_MIN = 1.d2  ! [m]
  real(8), parameter :: THRESH_RATIO_CUT = 0.5d0
  real(8), parameter :: RATIO_EFFECTIVE_LENG = 0.5d0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
recursive subroutine determineChannelScale(uid)
  use c1_io, only: &
    read_network
  use c1_util, only: &
    clear_cmn_network
  use c2_strnk_io, only: &
    get_f_lst_networks_channel, &
    get_f_network_channel     , &
    get_f_network_mesh        , &
    get_f_network_channelscale, &
    read_network_mesh_domain
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c2_jflw_grid, only: &
    gxs_of_lon, &
    gxe_of_lon, &
    gys_of_lat, &
    gye_of_lat, &
    south_of_gy, &
    get_nextxy, &
    calc_lineleng_in_pixels
  use c2_jflw_io, only: &
    read_map_from_tile
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'determineChannelScale'
  character(*), intent(in) :: uid

  type(cmn_network_) :: cmnnwk
  type(network_) :: nwk
  type(channel_), pointer :: ch, ch2
  type(channel_down_), pointer :: down
  type(node_), pointer :: node
  type(nwknode_), pointer :: nwknode
  character(DGT_NWKUID) :: uid_this
  real(8), allocatable :: lst_lon(:), lst_lat(:)
  integer, allocatable :: lst_jCh(:), lst_jNode(:)
  integer, allocatable :: arg(:)
  real(8) :: lon, lat
  real(8) :: leng_cut, leng_sum, leng
  real(8) :: pos
  real(8) :: err_norm
  integer, pointer :: lst_gx(:), lst_gy(:)
  real(8), pointer :: lst_leng(:)
  integer :: nPix, iPix
  integer :: jPt
  integer :: mPt, iPt
  integer :: nNwk, jNwk
  integer :: jCh, jCh2
  integer :: iiCh
  integer :: jNode, jNode2
  integer :: iNode
  integer :: is, ie, iis, iie
  real(8), allocatable :: upamap(:,:)
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixlen(:)
  integer :: gxs, gxe, gys, gye
  integer :: igy
  real(8) :: west, east, south, north
  real(8) :: theta, theta1, theta2
  real(8) :: distrb_sum, distrb
  real(8) :: scale_min
  logical :: is_updated

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  real(8), allocatable, save :: pixara(:)
  logical, save :: is_raster_resolution_defined = .false.

  integer :: jCh_debug = 658
  logical :: debug_this = .true.

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Run for all networks and return
  !-------------------------------------------------------------
  if( uid == '' )then
    f = get_f_lst_networks_channel()
    open(newunit=un, file=f, status='old')
    read(un,*) c_, nNwk
    do jNwk = 1, nNwk
      read(un,*) c_, uid_this
      call determineChannelScale(uid_this)
    enddo
    close(un)

    if( is_raster_resolution_defined )then
      is_raster_resolution_defined = .false.
      deallocate(pixara)
      deallocate(pixlen)
    endif

    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! Prepare static data common for all networks
  !-------------------------------------------------------------
  if( .not. is_raster_resolution_defined )then
    is_raster_resolution_defined = .true.
    call set_resolution(RESOLUTION_1SEC)

    ! Calc. pixel area
    allocate(pixara(1:NGY))
    allocate(pixlen(1:NGY))
    allocate(pixlat(0:NGY))

    pixlat(0) = REGION_NORTH * d2r
    do igy = 1, NGY
      pixlat(igy) = south_of_gy(igy) * d2r
    enddo
    !call logmsg('pixlat: '//str(pixlat(:2)*r2d,'f11.7',', ')//&
    !            ', ..., '//str(pixlat(NGY-2:)*r2d,'f11.7',', '))

    pixara(:) = area_sphere_rect(pixlat(0:NGY-1), pixlat(1:NGY)) * (GRIDSIZE_LON*d2r) * EARTH_R**2

    ! Prepare pixel length data
    do igy = 1, NGY
      pixlen(igy) = dist_sphere(&
        0.d0, pixlat(igy-1)*d2r, GRIDSIZE_LON*d2r, pixlat(igy)*d2r &
      ) * EARTH_R
    enddo

    deallocate(pixlat)
  endif
  !-------------------------------------------------------------
  ! Prepare static data of this network
  !-------------------------------------------------------------
  call logmsg('Network '//str(uid))

  call logent('Preparing static data')

  ! Read network data
  !-------------------------------------------------------------
  cmnnwk%uid = uid

  f = get_f_network_channel(uid, 'sbin')
  call read_network(f, cmnnwk)

  call copy_cmn2nwk(cmnnwk, nwk)

  call clear_cmn_network(cmnnwk)

  ! Make a list of nodes
  !-------------------------------------------------------------
  nwk%nNode = nwk%nCh * 2
  allocate(nwk%node(nwk%nNode))

  allocate(lst_jCh(nwk%nNode))
  allocate(lst_jNode(nwk%nNode))
  allocate(lst_lon(nwk%nNode))
  allocate(lst_lat(nwk%nNode))
  allocate(arg(nwk%nNode))

  iNode = 0

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)

    do jNode = 1, 2
      node => ch%node(jNode)

      call add(iNode)
      lst_jCh(iNode) = jCh
      lst_jNode(iNode) = jNode
      lst_lon(iNode) = node%lon
      lst_lat(iNode) = node%lat
    enddo  ! jNode/
  enddo  ! jCh/

  call argsort(lst_lon, arg)
  call sort(lst_lon, arg)
  call sort(lst_lat, arg)
  call sort(lst_jCh, arg)
  call sort(lst_jNode, arg)

  iNode = 0

  ie = 0
  do while( ie < nwk%nNode )
    is = ie + 1
    ie = is
    do while( ie < nwk%nNode )
      if( lst_lon(ie+1) /= lst_lon(is) ) exit
      ie = ie + 1
    enddo
    call argsort(lst_lat(is:ie), arg(is:ie))
    call argsort(lst_jCh(is:ie), arg(is:ie))
    call argsort(lst_jNode(is:ie), arg(is:ie))
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
      nwknode%jCh(:) = lst_jCh(iis:iie)
      nwknode%jNode(:) = lst_jNode(iis:iie)

      do iiCh = 1, nwknode%nCh
        nwk%channel(nwknode%jCh(iiCh))%node(nwknode%jNode(iiCh))%iNode = iNode
      enddo
    enddo
  enddo

  nwk%nNode = iNode
  call logmsg('Nodes: '//str(nwk%nNode))

  deallocate(lst_lon)
  deallocate(lst_lat)
  deallocate(lst_jCh)
  deallocate(lst_jNode)
  deallocate(arg)

  ! Search for downstream channels for each node
  !-------------------------------------------------------------
  do jCh = 1, nwk%nCh
    debug_this = jCh == jCh_debug

    ch => nwk%channel(jCh)

    ch%node(:)%nCh_down = 0

    ! Only the downstream node is investigated
    ! since the upstream node has no downstream channel
    jNode = 2
    node => ch%node(jNode)
    nwknode => nwk%node(node%iNode)

    allocate(node%jCh_down(nwknode%nCh-1))

    do iiCh = 1, nwknode%nCh
      jCh2 = nwknode%jCh(iiCh)
      jNode2 = nwknode%jNode(iiCh)
      if( jCh2 == jCh .or. jNode2 == 2 ) cycle
      call add(node%nCh_down)
      node%jCh_down(node%nCh_down) = jCh2
    enddo  ! iiCh/

  enddo  ! jCh/

  !
  !-------------------------------------------------------------
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)

    debug_this = jCh == jCh_debug

    do jNode = 1, 2
      node => ch%node(jNode)

      node%nCh_down_angle1 = 0
      node%nCh_down_angle2 = 0
      allocate(node%down_angle1(node%nCh_down))
      allocate(node%down_angle2(node%nCh_down))

      do iiCh = 1, node%nCh_down
        jCh2 = node%jCh_down(iiCh)
        if( jCh2 == jCh ) cycle
        ch2 => nwk%channel(jCh2)

        theta1 = atan2(ch%lat(ch%n)-ch%lat(ch%n-1), ch%lon(ch%n)-ch%lon(ch%n-1))
        theta2 = atan2(ch2%lat(ch2%n)-ch2%lat(ch2%n-1), ch2%lon(ch2%n)-ch2%lon(ch2%n-1))
        theta = abs(theta2 - theta1)
        if( theta > rad_180deg ) theta = rad_360deg - theta

        if( theta < rad_90deg )then
          !if( debug_this )then
          !  call logmsg('down 1: ch#'//str(jCh2,dgt(nwk%nCh))//' theta '//str(theta*r2d))
          !endif
          call add(node%nCh_down_angle1)
          down => node%down_angle1(node%nCh_down_angle1)
        else
          call add(node%nCh_down_angle2)
          down => node%down_angle2(node%nCh_down_angle2)
        endif
        down%jCh = jCh2
        down%angle = theta
      enddo  ! iiCh/
    enddo  ! jNode/
  enddo  ! jCh/

  ! Get domain
  !-------------------------------------------------------------
  call read_network_mesh_domain(&
    RESOLUTION_1SEC, nwk%uid, &
    gxs, gxe, gys, gye, &
    west, east, south, north &
  )

  ! Read upper area map
  !-------------------------------------------------------------
  allocate(upamap(gxs:gxe,gys:gye))

  f = get_f_network_mesh(RESOLUTION_1SEC, 'upa', nwk%uid)
  call traperr( rbin(upamap, f) )

  call logext()
  !-------------------------------------------------------------
  ! Determine upper area of each channel
  !-------------------------------------------------------------
  call logent('Determining upper area of each channel')

  nullify(lst_gx)
  nullify(lst_gy)
  nullify(lst_leng)

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    leng_cut = max(ch%leng * RATIO_CUT_TAIL, LENG_CUT_MIN)
    !call logmsg('ch #'//str(jCh,dgt(nwk%nCh))//' leng '//str(ch%leng)//' cut '//str(leng_cut)//&
    !  ' ('//str(leng_cut/ch%leng,'f5.3')//')')
    if( leng_cut > ch%leng * THRESH_RATIO_CUT )then
      cycle
    endif

    leng_sum = 0.d0
    do jPt = ch%n, 2, -1
      leng = dist_sphere(&
        ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, ch%lon(jPt-1)*d2r, ch%lat(jPt-1)*d2r &
      ) * EARTH_R

      if( leng_sum + leng >= leng_cut )then
        pos = (leng_cut - leng_sum) / leng
        if( abs(pos-1.d0) < 1d-12 )then
          pos = 1.d0
        elseif( abs(pos) < 1d-12 )then
          pos = 0.d0
        endif
        lon = ch%lon(jPt)*(1.d0-pos) + ch%lon(jPt-1)*pos
        lat = ch%lat(jPt)*(1.d0-pos) + ch%lat(jPt-1)*pos
        exit
      endif

      call add(leng_sum, leng)
    enddo  ! jPt/

    err_norm = abs(leng_sum + leng*pos - leng_cut) / leng_cut
    if( err_norm > 1d-12 )then
      call errend('err_norm: '//str(err_norm))
    endif

    allocate(lst_lon(ch%n))
    allocate(lst_lat(ch%n))

    if( pos == 0.d0 )then
      lst_lon(1:jPt) = ch%lon(1:jPt)
      lst_lat(1:jPt) = ch%lat(1:jPt)
      mPt = jPt
    elseif( pos == 1.d0 )then
      lst_lon(1:jPt-1) = ch%lon(1:jPt-1)
      lst_lat(1:jPt-1) = ch%lat(1:jPt-1)
      mPt = jPt - 1
    else
      lst_lon(1:jPt-1) = ch%lon(1:jPt-1)
      lst_lon(jPt) = lon
      lst_lat(1:jPt-1) = ch%lat(1:jPt-1)
      lst_lat(jPt) = lat
      mPt = jPt
    endif

    ! Check
    leng_sum = 0.d0
    do iPt = 1, mPt-1
      call add(leng_sum, dist_sphere(&
        lst_lon(iPt)*d2r, lst_lat(iPt)*d2r, lst_lon(iPt+1)*d2r, lst_lat(iPt+1)*d2r &
      ) * EARTH_R)
    enddo
    !call logmsg('leng this '//str(leng_sum)//' target '//str(ch%leng-leng_cut)//&
    !            ' err_norm  '//str((leng_sum - (ch%leng-leng_cut)) / leng_cut))

    ! Calc. length in pixels
    ch%upa = 0.d0
    do iPt = 1, mPt-1
      call calc_lineleng_in_pixels(&
        lst_lon(iPt), lst_lat(iPt), lst_lon(iPt+1), lst_lat(iPt+1), &
        nPix, lst_gx, lst_gy, lst_leng &
      )
      lst_leng(:) = lst_leng(:) * EARTH_R

      do iPix = 1, nPix
        if( lst_leng(iPix) < pixlen(lst_gy(iPix)) * RATIO_EFFECTIVE_LENG ) cycle
        ch%upa = max(ch%upa, upamap(lst_gx(iPix), lst_gy(iPix)))
      enddo  ! iPix/
    enddo  ! iPt/

    deallocate(lst_lon)
    deallocate(lst_lat)
  enddo ! jCh/

  call realloc(lst_gx, 0)
  call realloc(lst_gy, 0)
  call realloc(lst_leng, 0)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(upamap)
  !-------------------------------------------------------------
  ! Estimate scale of each channel
  !-------------------------------------------------------------
  call logent('Estimating scale of each channel')

  nwk%channel(:)%scale = nwk%channel(:)%upa

  is_updated = .true.

  do while( is_updated )

    is_updated = .false.

    do jCh = 1, nwk%nCh
      debug_this = jCh == jCh_debug

      ch => nwk%channel(jCh)

      do jNode = 1, 2
        node => ch%node(jNode)

        !-------------------------------------------------------
        ! Case: No downstream channel
        if( node%nCh_down == 0 )then
          cycle

        !-------------------------------------------------------
        ! Case: (a) θ < 90: n (>=1), (b) θ >= 90: m (>=0)
        !  - Scales of channels of group (a) can be adjusted
        elseif( node%nCh_down_angle1 >= 1 )then

          !distrb_sum = sum(cos(node%down_angle1(:)%angle)) / node%nCh_down_angle1**0.5d0
          distrb_sum = sum(cos(node%down_angle1(:)%angle)**0.5) / node%nCh_down_angle1
          do iiCh = 1, node%nCh_down_angle1
            down => node%down_angle1(iiCh)
            ch2 => nwk%channel(down%jCh)
            distrb = cos(down%angle)**0.5
            scale_min = ch%scale * (distrb / distrb_sum)
            if( (debug_this .or. down%jCh == jCh_debug) .and. ch2%scale < scale_min )then
              call logmsg('ch#'//str(down%jCh,dgt(nwk%nCh))//&
                ' scale: '//str(ch2%scale)//' -> '//str(scale_min))
            endif
            call update_scale(ch2, scale_min, is_updated)
          enddo  ! iiCh/

        !-------------------------------------------------------
        ! Case: (a) θ < 90: 0, (b) 90 <= θ: m (>0)
        elseif( node%nCh_down_angle1 == 0 .and. node%nCh_down_angle2 >= 1 )then
          distrb_sum = sum(cos(node%down_angle2(:)%angle))
          do iiCh = 1, node%nCh_down_angle2
            down => node%down_angle2(iiCh)
            ch2 => nwk%channel(down%jCh)
            distrb = cos(down%angle)
            scale_min = ch%scale * (distrb / distrb_sum)
            if( (debug_this .or. down%jCh == jCh_debug) .and. ch2%scale < scale_min )then
              call logmsg('ch#'//str(down%jCh,dgt(nwk%nCh))//&
                ' scale: '//str(ch2%scale)//' -> '//str(scale_min))
            endif
            call update_scale(ch2, scale_min, is_updated)
          enddo  ! iiCh/
        endif

      enddo  ! jNode/
    enddo  ! iNode/

  enddo  ! while is_updated/

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f = get_f_network_channelscale(nwk%uid)
  call logmsg('Writing '//str(f))
  call traperr( wbin(nwk%channel(:)%upa, f, rec=1, replace=.true.) )
  call traperr( wbin(nwk%channel(:)%scale, f, rec=2) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine update_scale(ch, scale_min, is_updated)
  implicit none
  type(channel_), intent(inout) :: ch
  real(8), intent(in) :: scale_min
  logical, intent(inout) :: is_updated

  if( ch%scale >= scale_min ) return

  !call logmsg('scale '//str(ch%scale)//' -> '//str(scale_min))

  ch%scale = scale_min
  ch%is_updated = .true.
  is_updated = .true.
end subroutine update_scale
!---------------------------------------------------------------
end subroutine determineChannelScale
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
recursive subroutine estimateCrossSection(&
  f_conf, uid, overwrite &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'estimateCrossSection'
  character(*), intent(in) :: f_conf
  character(*), intent(in) :: uid
  logical, intent(in) :: overwrite

  character(CLEN_KEY) :: method
  character(CLEN_PATH) :: id
  real(8) :: w_c, w_s, w_l, h_c, h_s, h_l, d_c, d_s, d_l

  integer :: un
  
  character(CLEN_KEY), parameter :: METHOD__UPA = 'upa'
  character(CLEN_KEY), parameter :: METHOD__UPA_DOWNLENG = 'upa_downleng'

  namelist/nml_product/ id
  namelist/nml_method/ method
  namelist/nml_params_upa/ w_c, w_s, w_l, h_c, h_s, h_l, d_c, d_s, d_l
  namelist/nml_params_upa_downleng/ w_c, w_s, w_l, h_c, h_s, h_l, d_c, d_s, d_l

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Read config
  !-------------------------------------------------------------
  open(newunit=un, file=f_conf, status='old')

  read(un,nml=nml_method)

  read(un,nml=nml_product)

  selectcase( method )

  case( METHOD__UPA )
    read(un,nml=nml_params_upa)

  case( METHOD__UPA_DOWNLENG )
    read(un,nml=nml_params_upa_downleng)

  case default
    call errend(msg_invalid_value('method', str(method)))
  endselect

  close(un)
  !-------------------------------------------------------------
  ! Estimate
  !-------------------------------------------------------------
  selectcase( method )

  case( METHOD__UPA )
    !call estimate_by_upa(&
    !  uid, &
    !  w_c, w_s, w_l, h_c, h_s, h_l, d_c, d_s, d_l, &
    !  overwrite
    !)

  case( METHOD__UPA_DOWNLENG )
    call estimate_by_upa_downleng(&
      id, uid, &
      w_c, w_s, w_l, h_c, h_s, h_l, d_c, d_s, d_l, &
      overwrite &
    )

  case default
    call errend(msg_invalid_value('method', str(method)))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine estimateCrossSection
!===============================================================
!
!===============================================================
subroutine estimate_by_upa_downleng(&
    productId, uid, &
    w_c, w_s, w_l, h_c, h_s, h_l, d_c, d_s, d_l, &
    overwrite &
)
  use c1_io, only: &
    read_network_size
  use c2_strnk_io, only: &
    get_f_network_channel     , &
    get_f_network_channelscale, &
    get_f_network_crosssection
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'estimate_by_upa_downleng'
  character(*), intent(in) :: productId
  character(*), intent(in) :: uid
  real(8), intent(in) :: w_c, w_s, w_l, h_c, h_s, h_l, d_c, d_s, d_l
  logical, intent(in) :: overwrite

  integer :: nCh, jCh
  real(8), allocatable :: scale(:)
  real(8), allocatable :: width(:), hight(:), depth(:), levee(:)
  character(CLEN_PATH) :: productName
  character(CLEN_PATH) :: f, f_scale
  character(CLEN_PATH) :: f_width, f_hight, f_depth, f_levee

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  productName = 'upa_downleng_'//str(productId)
  f_width = get_f_network_crosssection(productName, 'width', uid)
  f_hight = get_f_network_crosssection(productName, 'height', uid)
  f_depth = get_f_network_crosssection(productName, 'depth', uid)
  f_levee = get_f_network_crosssection(productName, 'levee', uid)

  if( .not. overwrite )then
    if( if_return(f_width) ) return
    if( if_return(f_hight) ) return
    if( if_return(f_depth) ) return
    if( if_return(f_levee) ) return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_network_channel(uid, 'sbin')
  call read_network_size(f, nCh=nCh)

  allocate(scale(nCh))
  allocate(width(nCh))
  allocate(hight(nCh))
  allocate(depth(nCh))
  allocate(levee(nCh))

  f_scale = get_f_network_channelscale(uid)
  call logmsg('Reading '//str(f_scale))
  call traperr( rbin(scale, f_scale, rec=2) )
  scale = scale * 1d-6  ! [m2] -> [km2]

  do jCh = 1, nCh
    width(jCh) = max(w_c * scale(jCh) ** w_s, w_l)
    hight(jCh) = max(h_c * scale(jCh) ** h_s, h_l)
    depth(jCh) = min(max(d_c * scale(jCh) ** d_s, d_l), hight(jCh))
    levee(jCh) = hight(jCh) - depth(jCh)
  enddo

  call print_stats(scale, 'scale')
  call print_stats(width, 'width')
  call print_stats(hight, 'hight')
  call print_stats(depth, 'depth')
  call print_stats(levee, 'levee')

  call logmsg('Writing '//str(f_width))
  call logmsg('Writing '//str(f_hight))
  call logmsg('Writing '//str(f_depth))
  call logmsg('Writing '//str(f_levee))
  call traperr( wbin(width, f_width, replace=.true.) )
  call traperr( wbin(hight, f_hight, replace=.true.) )
  call traperr( wbin(depth, f_depth, replace=.true.) )
  call traperr( wbin(levee, f_levee, replace=.true.) )

  deallocate(scale)
  deallocate(width)
  deallocate(hight)
  deallocate(depth)
  deallocate(levee)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
logical function if_return(f) result(res)
  implicit none
  character(*), intent(in) :: f

  integer :: access

  res = .false.
  if( access(f, ' ') == 0 )then
    res = .true.
    call logmsg('File already exists: '//str(f))
    call logret(PRCNAM, MODNAM)
  endif
end function if_return
!---------------------------------------------------------------
subroutine print_stats(x, name)
  implicit none
  real(8), intent(in) :: x(:)
  character(*), intent(in) :: name

  call logmsg(name//' mean '//str(sum(x)/size(x))//&
    ' min '//str(minval(x))//' max '//str(maxval(x)))
end subroutine print_stats
!---------------------------------------------------------------
end subroutine estimate_by_upa_downleng
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
subroutine copy_cmn2nwk(cmn, nwk)
  implicit none
  type(cmn_network_), intent(in) :: cmn
  type(network_), intent(out) :: nwk

  type(cmn_watsys_), pointer :: cwsys
  type(cmn_channel_), pointer :: cch
  type(cmn_node_), pointer :: cnode
  type(watsys_), pointer :: wsys
  type(channel_), pointer :: ch
  type(node_), pointer :: node
  integer :: jWsys
  integer :: jCh
  integer :: jNode

  allocate(character(1) :: nwk%uid)
  nwk%uid = cmn%uid

  nwk%nWsys = cmn%nWsys
  allocate(nwk%wsys(nwk%nWsys))
  do jWsys = 1, nwk%nWsys
    cwsys => cmn%wsys_(jWsys)
    wsys => nwk%wsys(jWsys)

    allocate(character(1) :: wsys%wsCode)
    wsys%wsCode = cwsys%wsCode

    wsys%leng = cwsys%leng
  enddo  ! jWsys/

  nwk%nCh = cmn%nCh
  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    cch => cmn%channel_(jCh)
    ch => nwk%channel(jCh)

    allocate(character(1) :: ch%wsCode)
    allocate(character(1) :: ch%rvCode)
    allocate(character(1) :: ch%rvName)
    ch%wsCode = cch%wsCode
    ch%rvCode = cch%rvCode
    ch%rvName = cch%rvName

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
end subroutine copy_cmn2nwk
!===============================================================
!
!===============================================================
end module mod_estimate_cross_section
