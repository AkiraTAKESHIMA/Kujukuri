module mod_connect_channels
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_util, only: &
        slonlat, &
        sBBox
  use c2_strnk_const, only: &
        CLEN_NODEID
  use mod_util, only: &
        nwk_rgn_
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: connectChannels
  public :: postConnectChannels
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_connect_channels'
  !-------------------------------------------------------------
  ! Private module variables (types)
  !-------------------------------------------------------------
  type rgn_tile_
    integer :: mEnt
    integer, pointer :: irEnt(:)  !(mEnt)
    integer, pointer :: itEnt(:)  !(mEnt)
    integer, pointer :: uid(:)  !(mEnt)
  end type

  type tbl_rgn_tile_
    type(rgn_tile_), pointer :: region(:)  !(NREGION)
  end type

  type point_conn_
    integer :: nPoint_conn
    integer, pointer :: itEnt_conn(:)  !(nPoint_conn) index in the tile
    integer, pointer :: iPt_conn(:)  !(nPoint_conn) index in the part
  end type

  type dct_point_
    integer :: n
    real(8), pointer :: lon(:), lat(:) !
    integer, pointer :: itEnt(:) ! Local index in each tile
    integer, pointer :: jEnt(:) ! Local index in entity
    integer, pointer :: jPt(:)  ! Local index in part
    integer, pointer :: kPt(:)  ! Local index in network
    integer, pointer :: iPt(:)  ! Global index
    integer, pointer :: nodetyp(:)
    integer, pointer :: nodetyp_new(:)
  end type

  type node_
    integer :: typ
    integer :: typ_new
    character(CLEN_NODEID) :: id
  end type

  type tbl_ent_
    integer :: iRegion  ! global index
    integer :: irEnt  ! index in region
    character(:), allocatable :: wsCode
    character(:), allocatable :: rvCode
    character(:), allocatable :: rvName
    type(point_conn_), pointer :: point(:)  !(part%nPoint)
    type(shp_entity_) :: ent
    type(dct_point_) :: dct_point
    integer :: uid
    logical :: is_updated
    type(node_), pointer :: node(:)  !(2)
    integer :: nDiv
    integer, pointer :: jPt_div(:)  !(nDiv)
  end type

  type region_
    integer :: nrEnt
    integer, pointer :: uid(:)  !(nrEnt)
  end type

  type dct_ent_
    integer :: ngEnt
    integer, pointer :: uid(:)
    integer, pointer :: iRegion(:)
    integer, pointer :: irEnt(:)
    real(8), pointer :: leng(:)
  end type

  type network_
    integer :: uid
    integer :: mRegion
    type(nwk_rgn_), pointer :: region(:)  !(mRegion)
    integer :: nEnt
    type(tbl_ent_), pointer :: tbl_ent(:)  !(nEnt)
    real(8) :: west, east, south, north
    type(dct_point_) :: dct_point
    real(8) :: leng
  end type

  type dct_node_
    integer :: n
    real(8), pointer :: lon(:), lat(:)
    integer, pointer :: typ(:)
    character(CLEN_NODEID), pointer :: id(:)
    integer, pointer :: i(:)
    integer, pointer :: jCh(:)
    integer, pointer :: jEnt(:)
    integer, pointer :: jNode(:)
  end type

  type ch_node_
    real(8) :: lon, lat
    integer :: typ
    real(8) :: elv
  end type

  type channel_
    integer :: jEnt
    integer :: n
    real(8), pointer :: lon(:), lat(:)
    type(ch_node_), pointer :: node(:)  !(2)
    real(8) :: leng
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer :: dgt_irEnt
  integer :: dgt_uid
  integer :: dgt_jPt

  integer, parameter :: CLEN_STAT_UPA_NEXTCHANNEL = 12
  character(12) :: STAT_UPA_NEXTCHANNEL__FOUND        = 'Found'
  character(12) :: STAT_UPA_NEXTCHANNEL__DISTINCT     = 'Distinct'
  character(12) :: STAT_UPA_NEXTCHANNEL__NOCONNECTION = 'NoConnection'

  real(8), parameter :: ELV__FORMISS = 0.d0

  logical :: debug = .false.
  integer :: uid_debug = 00014931
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
subroutine connectChannels()
  use c3_nlni_const
  use c2_nlni_const, only: &
        nlni_set_resolution => set_resolution
  use c2_nlni_io, only: &
        nlni_tilename => tilename
  use c2_strnk_const
  use c2_strnk_io, only: &
        region_str2idx        , &
        region_idx2str        , &
        get_f_stream_shp      , &
        get_f_lst_tiled_idx   , &
        get_f_lst_tiled_uid   , &
        get_f_tmp_networks_fmt
  use mod_util, only: &
        make_uid_int
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'connectChannels'

  type(tbl_rgn_tile_), pointer :: tbl_rgn_tile(:,:)  !(TXMIN:TXMAX,TYMIN:TYMAX)
  type(tbl_rgn_tile_), pointer :: tbl_rgn_tile_this
  type(rgn_tile_), pointer :: rgn_tile

  integer :: itx, ity
  character(CLEN_VAR) :: regionName
  integer :: iRegion
  integer :: mRegion, iiRegion
  integer :: ntEnt
  integer :: mEnt, iiEnt
  logical, allocatable :: is_tile_exist(:,:)
  logical, allocatable :: is_tile_updated(:,:)
  logical :: is_local_tile_updated
  logical :: is_updated
  integer :: nIter
  integer :: iTile
  integer :: irEnt_max, uid_max

  character(CLEN_PATH) :: f
  integer :: un
  character(CLEN_WFMT) :: wfmt
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call nlni_set_resolution(RESOLUTION_100M)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(is_tile_exist(NLNI_TXMIN:NLNI_TXMAX,NLNI_TYMIN:NLNI_TYMAX))
  allocate(is_tile_updated(NLNI_TXMIN:NLNI_TXMAX,NLNI_TYMIN:NLNI_TYMAX))
  is_tile_exist(:,:) = .true.
  is_tile_updated(:,:) = .true.

  do ity = NLNI_TYMIN, NLNI_TYMAX
  do itx = NLNI_TXMIN, NLNI_TXMAX
    f = get_f_lst_tiled_idx(nlni_tilename(itx,ity))
    if( access(f, ' ') /= 0 )then
      is_tile_exist(itx,ity) = .false.
      is_tile_updated(itx,ity) = .false.
    endif
  enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  ! Set index and uid
  !-------------------------------------------------------------
  irEnt_max = 0
  uid_max = 0

  allocate(tbl_rgn_tile(NLNI_TXMIN:NLNI_TXMAX,NLNI_TYMIN:NLNI_TYMAX))
  do ity = NLNI_TYMIN, NLNI_TYMAX
  do itx = NLNI_TXMIN, NLNI_TXMAX
    if( .not. is_tile_exist(itx,ity) ) cycle

    tbl_rgn_tile_this => tbl_rgn_tile(itx,ity)
    allocate(tbl_rgn_tile_this%region(NREGION))
    do iRegion = 1, NREGION
      tbl_rgn_tile_this%region(iRegion)%mEnt = 0
    enddo

    f = get_f_lst_tiled_idx(nlni_tilename(itx,ity))
    open(newunit=un, file=f, status='old')
    read(un,*) c_, mRegion
    ntEnt = 0
    do iiRegion = 1, mRegion
      read(un,*) regionName, mEnt
      rgn_tile => tbl_rgn_tile_this%region(region_str2idx(regionName))
      rgn_tile%mEnt = mEnt
      allocate(rgn_tile%irEnt(rgn_tile%mEnt))
      allocate(rgn_tile%itEnt(rgn_tile%mEnt))
      allocate(rgn_tile%uid(rgn_tile%mEnt))
      do iiEnt = 1, rgn_tile%mEnt
        read(un,*) rgn_tile%irEnt(iiEnt)
        call add(ntEnt)
        rgn_tile%itEnt(iiEnt) = ntEnt
        rgn_tile%uid(iiEnt) = make_uid_int(itx, ity, ntEnt)
      enddo  ! iiEnt/
      irEnt_max = max(irEnt_max, maxval(rgn_tile%irEnt))
      uid_max = max(uid_max, maxval(rgn_tile%uid))
    enddo  ! iiRegion/
    close(un)
  enddo  ! itx/
  enddo  ! ity/

  dgt_irEnt = dgt(irEnt_max)
  dgt_uid = dgt(uid_max)

  f = get_f_tmp_networks_fmt()
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(1x,a,1x,i0)") 'dgt_irEnt', dgt_irEnt
  write(un,"(1x,a,1x,i0)") 'dgt_uid', dgt_uid
  close(un)
  !-------------------------------------------------------------
  ! Main
  !-------------------------------------------------------------
  is_local_tile_updated = .true.
  is_updated = .true.
  nIter = 0
  do while( is_updated )
    is_updated = .false.
    is_local_tile_updated = .false.
    call add(nIter)

    call logmsg(&
           '---------------------------------------------------\n'//&
           'Iteration '//str(nIter)//&
           ' Number of tiles to update: '//str(count(is_tile_updated))//'\n'//&
           '---------------------------------------------------')

    do ity = NLNI_TYMIN, NLNI_TYMAX
    do itx = NLNI_TXMIN, NLNI_TXMAX
      if( .not. is_tile_updated(itx,ity) ) cycle

      call connectChannels_core(&
             itx, ity, is_tile_exist, & ! in
             tbl_rgn_tile           , & ! inout
             is_tile_updated        , & ! inout
             is_local_tile_updated)     ! out
      if( is_local_tile_updated ) is_updated = .true.
    enddo  ! itx/
    enddo  ! ity/
  enddo  ! is_updated/

  call logmsg('Finished after '//str(nIter)//' iterations.')
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  wfmt = "(5x,i"//str(dgt_irEnt)//",1x,i"//str(dgt_uid)//")"
  iTile = 0

  do ity = NLNI_TYMIN, NLNI_TYMAX
  do itx = NLNI_TXMIN, NLNI_TXMAX
    if( .not. is_tile_exist(itx,ity) ) cycle
    call add(iTile)

    tbl_rgn_tile_this => tbl_rgn_tile(itx,ity)
    
    f = get_f_lst_tiled_idx(nlni_tilename(itx,ity))
    open(newunit=un, file=f, status='old')
    read(un,*) c_, mRegion
    close(un)

    f = get_f_lst_tiled_uid(nlni_tilename(itx,ity))
    if( iTile <= 2 .or. iTile >= count(is_tile_exist)-1 )then
      call logmsg('Writing '//str(f))
    elseif( iTile == 3 )then
      call logmsg('...')
    endif
    open(newunit=un, file=f, status='replace')

    write(un,"(1x,a,1x,i0)") 'regions', mRegion
    do iRegion = 1, NREGION
      rgn_tile => tbl_rgn_tile_this%region(iRegion)
      if( rgn_tile%mEnt == 0 ) cycle
      write(un,"(3x,a,1x,i0)") region_idx2str(iRegion), rgn_tile%mEnt
      write(un,"(5x,a)") 'irEnt uid'
      do iiEnt = 1, rgn_tile%mEnt 
        write(un,wfmt) rgn_tile%irEnt(iiEnt), rgn_tile%uid(iiEnt)
      enddo  ! iiEnt/
    enddo  ! iRegion/
    close(un)
  enddo  ! itx/
  enddo  ! ity/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(is_tile_exist)
  deallocate(is_tile_updated)

  nullify(rgn_tile)
  nullify(tbl_rgn_tile_this)
  deallocate(tbl_rgn_tile)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine connectChannels
!===============================================================
!
!===============================================================
subroutine postConnectChannels()
  use lib_math
  use c1_const
  use c3_nlni_const
  use c2_nlni_const, only: &
        nlni_set_resolution => set_resolution
  use c2_nlni_io, only: &
        nlni_tilename => tilename
  use c3_jflw_const, &
        jflw_set_resolution => jflw_set_resolution
  use c3_jflw_io, only: &
        jflw_read_map_from_tile
  use c3_jflw_grid, only: &
        jflw_center_of_gx, &
        jflw_center_of_gy, &
        jflw_gxs_of_lon  , &
        jflw_gxe_of_lon  , &
        jflw_gys_of_lat  , &
        jflw_gye_of_lat
  use c2_strnk_const
  use c2_strnk_io, only: &
        region_str2idx          , &
        region_idx2str          , &
        get_f_stream_shp        , &
        get_f_rivernode_shp     , &
        get_f_lst_tiled_idx     , &
        get_f_lst_tiled_uid     , &
        get_f_tmp_networks_lst  , &
        get_f_tmp_network_entity, &
        get_f_tmp_network_channel
  use mod_util, only: &
        jPt2jNode        , &
        jNode2jPt        , &
        search_2         , &
        get_entity_length, &
        calc_elv         , &
        get_fmt_network  , &
        comma_json
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'postConnectChannels'

  type(tbl_rgn_tile_), pointer :: tbl_rgn_tile(:,:)  !(TXMIN:TXMAX,TYMIN:TYMAX)
  type(tbl_rgn_tile_), pointer :: tbl_rgn_tile_this
  type(rgn_tile_), pointer :: rgn_tile
  type(tbl_ent_), pointer :: tbe, tbe2
  type(dct_ent_) :: dct_ent
  type(shp_part_), pointer :: part, part2
  type(shp_entity_) :: ent
  type(region_), pointer :: region(:), rgn
  type(network_), pointer :: network(:), nwk, nwk2
  type(nwk_rgn_), pointer :: nwkrgn
  type(dct_point_), pointer :: dcp, dcp2
  type(dct_point_), pointer :: nwkdcp
  type(dct_node_) :: dct_node
  type(dct_node_) :: dct_node_identical
  type(node_), pointer :: node
  type(shp_) :: shp_node
  type(dbf_) :: dbf_node
  type(dbf_record_), pointer :: rec

  integer :: itx, ity
  logical, allocatable :: is_tile_exist(:,:)
  character(CLEN_VAR) :: regionName
  integer :: iRegion
  integer :: mRegion, iiRegion
  integer :: irEnt    ! Local index in region
  integer :: iEnt    ! Global index
  integer :: mEnt, iiEnt
  integer :: jEnt, jEnt2    ! Local index in network
  integer :: jNode, jNode2
  integer :: iNode, iNodes, iNodee
  integer :: irNode
  integer :: jPt, jPt2, jPts, jPte ! Local index in part
  integer :: jjPts2, jjPte2
  integer :: kPts, kPte, kPt, kPt2  ! Local index in network
  integer :: nNwk, iNwk, iNwk2
  integer :: iTile
  integer :: is, ie, iis, iie
  integer, pointer :: arg(:)
  real(8) :: plon, plat
  logical :: is_connected
  integer :: mNode, mNode_src, mNode_out, mNode_inter, &
             mNode_notfound, mNode_unknown

  integer :: gxs, gxe, gys, gye
  real(4), allocatable :: elvmap(:,:)

  integer :: nCh, jCh, jCh2
  integer :: jDiv
  type(channel_), pointer :: lst_ch(:), ch
  type(ch_node_), pointer :: chnode
  logical :: is_found

  character(CLEN_PATH) :: f
  character(CLEN_PATH) :: f_shp
  integer :: un
  ! formatting
  character(CLEN_WFMT) :: wfmt

  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call nlni_set_resolution(RESOLUTION_100M)
  call jflw_set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  allocate(is_tile_exist(NLNI_TXMIN:NLNI_TXMAX,NLNI_TYMIN:NLNI_TYMAX))

  do ity = NLNI_TYMIN, NLNI_TYMAX
  do itx = NLNI_TXMIN, NLNI_TXMAX
    f = get_f_lst_tiled_idx(nlni_tilename(itx,ity))
    is_tile_exist(itx,ity) = access(f, ' ') == 0
  enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  ! Read tiled uid data
  !-------------------------------------------------------------
  call logent('Reading tiled global index data')

  call get_fmt_network(dgt_irEnt, dgt_uid)

  allocate(tbl_rgn_tile(NLNI_TXMIN:NLNI_TXMAX,NLNI_TYMIN:NLNI_TYMAX))

  iTile = 0

  do ity = NLNI_TYMIN, NLNI_TYMAX
  do itx = NLNI_TXMIN, NLNI_TXMAX
    if( .not. is_tile_exist(itx,ity) ) cycle
    call add(iTile)

    tbl_rgn_tile_this => tbl_rgn_tile(itx,ity)
    allocate(tbl_rgn_tile_this%region(NREGION))
    tbl_rgn_tile_this%region(:)%mEnt = 0
    
    f = get_f_lst_tiled_uid(nlni_tilename(itx,ity))
    if( iTile <= 2 .or. iTile >= count(is_tile_exist)-1 )then
      call logmsg('Reading '//str(f))
    elseif( iTile == 3 )then
      call logmsg('...')
    endif
    open(newunit=un, file=f, status='old')

    read(un,*) c_, mRegion
    do iiRegion = 1, mRegion
      read(un,*) regionName, mEnt
      rgn_tile => tbl_rgn_tile_this%region(region_str2idx(regionName))
      rgn_tile%mEnt = mEnt
      allocate(rgn_tile%irEnt(rgn_tile%mEnt))
      allocate(rgn_tile%uid(rgn_tile%mEnt))
      read(un,*)
      do iiEnt = 1, rgn_tile%mEnt 
        read(un,*) rgn_tile%irEnt(iiEnt), rgn_tile%uid(iiEnt)
      enddo  ! iiEnt/
    enddo  ! iRegion/
    close(un)
  enddo  ! itx/
  enddo  ! ity/

  call logext()
  !-------------------------------------------------------------
  ! Make network data
  !-------------------------------------------------------------
  call logent('Making network data')

  allocate(region(NREGION))
  do iRegion = 1, NREGION
    rgn => region(iRegion)

    f_shp = get_f_stream_shp(region_idx2str(iRegion))
    call traperr( shp_open(f_shp) )
    call traperr( shp_get_info(nEntity=rgn%nrEnt) )
    call traperr( shp_close() )

    allocate(rgn%uid(rgn%nrEnt))
  enddo

  do ity = NLNI_TYMIN, NLNI_TYMAX
  do itx = NLNI_TXMIN, NLNI_TXMAX
    if( .not. is_tile_exist(itx,ity) ) cycle

    tbl_rgn_tile_this => tbl_rgn_tile(itx,ity)
    do iRegion = 1, NREGION
      rgn => region(iRegion)
      rgn_tile => tbl_rgn_tile_this%region(iRegion)
      if( rgn_tile%mEnt == 0 ) cycle
      do iiEnt = 1, rgn_tile%mEnt
        rgn%uid(rgn_tile%irEnt(iiEnt)) = rgn_tile%uid(iiEnt)
      enddo  ! iiEnt/
    enddo
  enddo  ! itx/
  enddo  ! ity/

  ! Make entity dictionary
  !-------------------------------------------------------------
  dct_ent%ngEnt = sum(region(:)%nrEnt)
  allocate(dct_ent%uid(dct_ent%ngEnt))
  allocate(dct_ent%iRegion(dct_ent%ngEnt))
  allocate(dct_ent%irEnt(dct_ent%ngEnt))
  allocate(dct_ent%leng(dct_ent%ngEnt))

  iEnt = 0
  do iRegion = 1, NREGION
    rgn => region(iRegion)

    f_shp = get_f_stream_shp(region_idx2str(iRegion))
    call traperr( shp_open(f_shp) )

    do irEnt = 1, rgn%nrEnt
      call add(iEnt)
      dct_ent%iRegion(iEnt) = iRegion
      dct_ent%irEnt(iEnt) = irEnt
      dct_ent%uid(iEnt) = rgn%uid(irEnt)

      call traperr( shp_get_entity(irEnt, ent) )
      dct_ent%leng(iEnt) = get_entity_length(ent)
    enddo  ! irEnt/

    call traperr( shp_close() )
  enddo  ! iRegion/

  call shp_clear_entity(ent)

  ! Sort by (1) uid (2) iRegion 
  ! and later by (3) irEnt (keeping sorted by (1), (2))
  allocate(arg(dct_ent%ngEnt))
  call argsort(dct_ent%uid, arg)
  call sort(dct_ent%uid, arg)
  call sort(dct_ent%iRegion, arg)
  call sort(dct_ent%irEnt, arg)
  call sort(dct_ent%leng, arg)

  nNwk = 0
  ie = 0
  do while( ie < dct_ent%ngEnt )
    call add(nNwk)

    is = ie + 1
    ie = is
    do while( ie < dct_ent%ngEnt )
      if( dct_ent%uid(ie+1) /= dct_ent%uid(is) ) exit
      ie = ie + 1
    enddo  ! ie/

    if( ie == is ) cycle

    call argsort(dct_ent%iRegion(is:ie), arg(is:ie))
    call sort(dct_ent%iRegion(is:ie), arg(is:ie))
    call sort(dct_ent%irEnt(is:ie), arg(is:ie))
    call sort(dct_ent%leng(is:ie), arg(is:ie))
  enddo  ! ie/

  deallocate(arg)

  call logmsg('Networks '//str(nNwk))

  ! Make table of index for each network
  !-------------------------------------------------------------
  allocate(network(nNwk))

  iNwk = 0
  ie = 0
  do while( ie < dct_ent%ngEnt )
    call add(iNwk)
    nwk => network(iNwk)
    nwk%uid = dct_ent%uid(ie+1)

    is = ie + 1
    ie = is
    do while( ie < dct_ent%ngEnt )
      if( dct_ent%uid(ie+1) /= dct_ent%uid(is) ) exit
      ie = ie + 1
    enddo  ! ie/

    nwk%leng = sum(dct_ent%leng(is:ie))

    nwk%mRegion = 0
    iie = is-1
    do while( iie < ie )
      call add(nwk%mRegion)
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( dct_ent%iRegion(iie+1) /= dct_ent%iRegion(iis) ) exit
        iie = iie + 1
      enddo  ! iie/
      if( iie == iis ) cycle
      call sort(dct_ent%irEnt(iis:iie))
    enddo

    allocate(nwk%region(nwk%mRegion))

    iiRegion = 0
    iie = is-1
    do while( iie < ie )
      call add(iiRegion)
      nwkrgn => nwk%region(iiRegion)
      nwkrgn%iRegion = dct_ent%iRegion(iie+1)

      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( dct_ent%iRegion(iie+1) /= dct_ent%iRegion(iis) ) exit
        iie = iie + 1
      enddo  ! iie/

      nwkrgn%mEnt = iie - iis + 1
      allocate(nwkrgn%irEnt(nwkrgn%mEnt))
      nwkrgn%irEnt(:) = dct_ent%irEnt(iis:iie)
    enddo  ! iie/

    nwk%nEnt = sum(nwk%region(:)%mEnt)
  enddo  ! ie/

  dct_ent%ngEnt = 0
  deallocate(dct_ent%uid)
  deallocate(dct_ent%iRegion)
  deallocate(dct_ent%irEnt)

  nullify(rgn)
  deallocate(region)

  nullify(tbl_rgn_tile_this)
  deallocate(tbl_rgn_tile)

  call logext()
  !-------------------------------------------------------------
  ! Check separation of networks 
  !-------------------------------------------------------------
if( .false. )then
  call logent('Checking separation of networks')

  do iNwk = 1, nNwk
    nwk => network(iNwk)

    call read_network_shp(nwk)
    call make_entity_dct_point(nwk)

    ! Check that no connection to other network exists
    do iNwk2 = iNwk+1, nNwk
      nwk2 => network(iNwk2)

      if( nwk%west > nwk2%east .or. nwk%east < nwk2%west .or. &
          nwk%south > nwk2%north .or. nwk%north < nwk2%south ) cycle

      call read_network_shp(nwk2)
      call make_entity_dct_point(nwk2)

      do jEnt = 1, nwk%nEnt
        tbe => nwk%tbl_ent(jEnt)
        part => tbe%ent%part(1)

        do jEnt2 = 1, nwk2%nEnt
          tbe2 => nwk2%tbl_ent(jEnt2)

          if( tbe%ent%xmin > tbe2%ent%xmax .or. tbe%ent%xmax < tbe2%ent%xmin .or. &
              tbe%ent%ymin > tbe2%ent%ymax .or. tbe%ent%ymax < tbe2%ent%ymin )then
            cycle
          endif

          dcp2 => tbe2%dct_point

          ! Check connection
          do jPt = 1, part%nPoint
            plon = part%x(jPt)
            plat = part%y(jPt)

            if( search_2(plon, plat, dcp2%lon, dcp2%lat, jjPts2, jjPte2) /= 0 )then
              call errend('Connection between different networks was detected.')
            endif
          enddo  ! jPt/
        enddo  ! jEnt2/
      enddo  ! jEnt/

      call free_entity_dct_point(nwk2)
      call free_network_tbl_ent(nwk2)
    enddo  ! iNwk2/

    call free_entity_dct_point(nwk)
    call free_network_tbl_ent(nwk)
  enddo  ! iNwk/

  call logext()
endif
  !-------------------------------------------------------------
  ! Make a list of all nodes
  !-------------------------------------------------------------
  call logent('Making a list of all nodes')

  dct_node%n = 0
  do iRegion = 1, NREGION
    f_shp = get_f_rivernode_shp(region_idx2str(iRegion))
    call traperr( shp_open(f_shp) )
    call traperr( shp_get_info(shp_node%nEntity) )
    call add(dct_node%n, shp_node%nEntity)
    call traperr( shp_close() )
  enddo  ! iRegion/
  call mul(dct_node%n, 2)

  call logmsg('Nodes: '//str(dct_node%n))
  allocate(dct_node%lon(dct_node%n))
  allocate(dct_node%lat(dct_node%n))
  allocate(dct_node%typ(dct_node%n))
  allocate(dct_node%id(dct_node%n))

  iNode = 0
  do iRegion = 1, NREGION
    f_shp = get_f_rivernode_shp(region_idx2str(iRegion))
    call traperr( shp_open(f_shp) )
    call traperr( dbf_open(f_shp) )
    call traperr( shp_get_all(shp_node) )
    call traperr( dbf_get_all(dbf_node) )

    do irNode = 1, shp_node%nEntity
      part => shp_node%entity(irNode)%part(1)
      rec => dbf_node%record(irNode)

      do jNode = 1, 2
        jPt = jNode2jPt(jNode, part%nPoint)

        call add(iNode)
        dct_node%lon(iNode) = part%x(jPt)
        dct_node%lat(iNode) = part%y(jPt)
        dct_node%id(iNode) = rec%value(IDX_FIELD_RIVERNODE__NODEID)%s
        dct_node%typ(iNode) = int4_char(rec%value(IDX_FIELD_RIVERNODE__ENDPOINT)%s)
      enddo  ! jNode/
    enddo  ! irNode/
    deallocate(shp_node%entity)

    call shp_clear_all(shp_node)
    call dbf_clear_all(dbf_node)
    call traperr( shp_close() )
    call traperr( dbf_close() )
  enddo  ! iRegion/

  ! Sort by lon, lat
  ! Make a list of identical nodes
  allocate(dct_node_identical%lon(dct_node%n))
  allocate(dct_node_identical%lat(dct_node%n))
  allocate(dct_node_identical%typ(dct_node%n))
  dct_node_identical%n = 0

  allocate(arg(dct_node%n))
  call argsort(dct_node%lon, arg)
  call sort(dct_node%lon, arg)
  call sort(dct_node%lat, arg)
  call sort(dct_node%id, arg)
  call sort(dct_node%typ, arg)
  ie = 0
  do while( ie < dct_node%n )
    is = ie + 1
    ie = is
    do while( ie < dct_node%n )
      if( dct_node%lon(ie+1) /= dct_node%lon(is) ) exit
      ie = ie + 1
    enddo
    call argsort(dct_node%lat(is:ie), arg(is:ie))
    call sort(dct_node%lat(is:ie), arg(is:ie))
    call sort(dct_node%typ(is:ie), arg(is:ie))
    call sort(dct_node%id(is:ie), arg(is:ie))
    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( dct_node%lat(iie+1) /= dct_node%lat(iis) ) exit
        iie = iie + 1
      enddo  ! iie/

      call add(dct_node_identical%n)
      dct_node_identical%lon(dct_node_identical%n) = dct_node%lon(iis)
      dct_node_identical%lat(dct_node_identical%n) = dct_node%lat(iis)

      if( iis == iie )then
        iNode = iis
        selectcase( dct_node%typ(iNode) )
        case( NODETYPE__SOURCE )
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__SRC
        case( NODETYPE__OUTLET )
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__OUT
        case( NODETYPE__INTERMEDIATE )
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__INTER
        case default
          call errend(msg_invalid_value('dct_node%typ('//str(iNode)//&
                      ')', dct_node%typ(iNode)))
        endselect
      else
        mNode = iie - iis + 1
        mNode_src   = 0
        mNode_out   = 0
        mNode_inter = 0
        do iNode = iis, iie
          selectcase( dct_node%typ(iNode) )
          case( NODETYPE__SOURCE )
            call add(mNode_src)
          case( NODETYPE__OUTLET )
            call add(mNode_out)
          case( NODETYPE__INTERMEDIATE )
            call add(mNode_inter)
          case default
            call errend(msg_invalid_value('dct_node%typ('//str(iNode)//&
                        ')', dct_node%typ(iNode)))
          endselect
        enddo

        if( mNode_src == mNode )then
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__SRC
        elseif( mNode_out == mNode )then
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__OUT
        elseif( mNode_inter == mNode )then
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__INTER
        elseif( mNode_src > 0 .and. mNode_out == 0 .and. mNode_inter > 0 )then
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__SRC_INTER
        elseif( mNode_src == 0 .and. mNode_out > 0 .and. mNode_inter > 0 )then
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__OUT_INTER
        elseif( mNode_src > 0 .and. mNode_out > 0 .and. mNode_inter == 0 )then
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__SRC_OUT
        elseif( mNode_src > 0 .and. mNode_out > 0 .and. mNode_inter > 0 )then
          dct_node_identical%typ(dct_node_identical%n) = NODETYPE_NEW__ALLMIXED
        else
          call errend('Not matched any case.')
        endif
      endif
    enddo  ! iis, iie/
  enddo  ! is, ie/
  deallocate(arg)

  call logmsg('Nodes w/o overlap: '//str(dct_node_identical%n))

  call realloc(dct_node_identical%lon, dct_node_identical%n, clear=.false.)
  call realloc(dct_node_identical%lat, dct_node_identical%n, clear=.false.)
  call realloc(dct_node_identical%typ, dct_node_identical%n, clear=.false.)

  allocate(dct_node_identical%i(dct_node_identical%n))
  do iNode = 1, dct_node_identical%n
    dct_node_identical%i(iNode) = iNode
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Investigate connections in each network
  !-------------------------------------------------------------
  call logent('Investigating connections in each network')

  do iNwk = 1, nNwk
    nwk => network(iNwk)
    if( debug .and. nwk%uid /= uid_debug ) cycle

    !call logmsg(str(iNwk,dgt(nNwk))//'/'//str(nNwk))

    call read_network_shp(nwk, read_node_attr=.true.)
    call get_network_bbox(nwk)
    call make_network_dct_point(nwk)
    nwkdcp => nwk%dct_point

    if( nwk%nEnt == 1 )then
      tbe => nwk%tbl_ent(1)
      do jNode = 1, 2
        node => tbe%node(jNode)
        selectcase( node%typ )
        case( NODETYPE__INTERMEDIATE )
          node%typ_new = NODETYPE_NEW__INTER
        case( NODETYPE__SOURCE )
          node%typ_new = NODETYPE_NEW__SRC
        case( NODETYPE__OUTLET )
          node%typ_new = NODETYPE_NEW__OUT
        case( NODETYPE__NOTFOUND )
          node%typ_new = NODETYPE_NEW__UNKNOWN
        case default
          call errend(msg_invalid_value('node%typ', node%typ)//&
                    '\n  iNwk: '//str(iNwk)//' (nEnt==1), jNode: '//str(jNode))
        endselect
      enddo  ! jNode/
      cycle
    endif

    dgt_jPt = dgt(maxval(nwk%tbl_ent(:)%ent%nPoint))

    do jEnt = 1, nwk%nEnt
      tbe => nwk%tbl_ent(jEnt)
      part => tbe%ent%part(1)

      is_connected = .false.
      tbe%nDiv = 0
      nullify(tbe%jPt_div)
      !---------------------------------------------------------
      ! Set new type for nodes
      !---------------------------------------------------------
      do jNode = 1, 2
        node => tbe%node(jNode)

        jPt = jNode2jPt(jNode, part%nPoint)
        plon = part%x(jPt)
        plat = part%y(jPt)
        if( search_2(plon, plat, nwkdcp%lon, nwkdcp%lat, kPts, kPte) /= 0 )then
          call errend('search_2: (plon,plat) not found in nwkdcp%lon, nwkdcp%lat')
        endif

        mNode_src      = 0
        mNode_out      = 0
        mNode_inter    = 0
        mNode_notfound = 0
        kPt = 0
        do kPt2 = kPts, kPte
          if( nwkdcp%jEnt(kPt2) == jEnt .and. nwkdcp%jPt(kPt2) == jPt )then
            if( kPt /= 0 )then
              print*, 'kPt', kPts, kPte
              print*, 'jEnt', nwkdcp%jEnt(kPts:kPte)
              print*, 'jPt ', nwkdcp%jPt(kPts:kPte)
              call errend('Point is duplicated.')
            endif
            kPt = kPt2
          else
            is_connected = .true.
          endif

          selectcase( nwkdcp%nodetyp(kPt2) )
          case( NODETYPE__SOURCE )
            call add(mNode_src)
          case( NODETYPE__OUTLET )
            call add(mNode_out)
          case( NODETYPE__INTERMEDIATE )
            call add(mNode_inter)
          case( NODETYPE__NOTNODE )
            call add(mNode_inter)
          case( NODETYPE__NOTFOUND )
            call add(mNode_notfound)
          case default
            call errend(msg_invalid_value('nwkdcp%nodetyp', nwkdcp%nodetyp(kPt2)))
          endselect
        enddo  ! kPt2/

        if( kPt == 0 )then
          call errend('Point was not found.')
        endif

        if( mNode_notfound > 0 )then
          nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__UNKNOWN
        else
          if( mNode_src > 0 )then
            if( mNode_inter == 0 .and. mNode_out == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__SRC
            elseif( mNode_out == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__SRC_INTER
            elseif( mNode_inter == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__SRC_OUT
            else
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__ALLMIXED
            endif
          elseif( mNode_out > 0 )then
            if( mNode_inter == 0 .and. mNode_src == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__OUT
            elseif( mNode_src == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__OUT_INTER
            elseif( mNode_inter == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__SRC_OUT
            else
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__ALLMIXED
            endif
          elseif( mNode_inter > 0 )then
            if( mNode_src == 0 .and. mNode_out == 0 )then
              if( mNode_inter == 1 )then
                nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__UNKNOWN  ! Not connected
              else
                nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__INTER
              endif
            elseif( mNode_out == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__SRC_INTER
            elseif( mNode_src == 0 )then
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__OUT_INTER
            else
              nwkdcp%nodetyp_new(kPt) = NODETYPE_NEW__ALLMIXED
            endif
          else
            call errend('Not matched any case.')
          endif
        endif            

        node%typ_new = nwkdcp%nodetyp_new(kPt)
      enddo  ! jNode/
      !---------------------------------------------------------
      ! Find connections of intermediate points (not node)
      !---------------------------------------------------------
      do jPt = 2, part%nPoint-1
        plon = part%x(jPt)
        plat = part%y(jPt)

        if( search_2(plon, plat, nwkdcp%lon, nwkdcp%lat, kPts, kPte) /= 0 )then
          print*,search_2(plon, plat, nwkdcp%lon, nwkdcp%lat, kPts, kPte)
          
          call errend('search_2: (plon,plat) not found in nwkdcp%lon, nwkdcp%lat.')
        endif

        do kPt = kPts, kPte
          jEnt2 = nwkdcp%jEnt(kPt)
          if( jEnt2 == jEnt ) cycle

          tbe2 => nwk%tbl_ent(jEnt2)
          part2 => tbe2%ent%part(1)
          jPt2 = nwkdcp%jPt(kPt)
          call logmsg(&
              'Inter. point is connected. Nwk('//str(iNwk,dgt(nNwk))//&
              ') Ent('//str(jEnt,dgt(nwk%nEnt))//') p('//&
              str((/jPt,part%nPoint/),dgt_jPt,'/')//') -> '//&
              'Ent('//str(jEnt2)//') p('//&
              str((/jPt2,part2%nPoint/),dgt_jPt,'/')//')')

          is_found = .false.
          do jDiv = 1, tbe%nDiv
            if( tbe%jPt_div(jDiv) == jPt )then
              is_found = .true.
              exit
            endif
          enddo
          if( .not. is_found )then
            if( tbe%nDiv == 0 )then
              allocate(tbe%jPt_div(4))
            elseif( tbe%nDiv == size(tbe%jPt_div) )then
              call realloc(tbe%jPt_div, tbe%nDiv*2, clear=.false.)
            endif
            call add(tbe%nDiv)
            jDiv = tbe%nDiv
          endif
          tbe%jPt_div(jDiv) = jPt
        enddo  ! kPt/
      enddo  ! jPt/
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      if( .not. is_connected )then
        call errend('Network('//str(iNwk)//') Ent('//str(jEnt)//'): Isolated channel.')
      endif
      !---------------------------------------------------------
    enddo  ! jEnt/

    if( any(nwk%tbl_ent(:)%nDiv > 0) )then
      call logmsg('Channels: '//str(nwk%nEnt)//' -> '//&
                  str(nwk%nEnt+sum(nwk%tbl_ent(:)%nDiv)))
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do jEnt = 1, nwk%nEnt
      tbe => nwk%tbl_ent(jEnt)
      do jNode = 1, 2
        node => tbe%node(jNode)
        if( node%typ_new /= node%typ )then
          part => tbe%ent%part(1)
          jPt = jNode2jPt(jNode, part%nPoint)
          call logmsg(&
              'Node type was updated. Nwk('//str(iNwk,dgt(nNwk))//&
              ') Ent('//str(jEnt)//') node('//str(jNode)//') '//&
              slonlat(part%x(jPt),part%y(jPt))//' '//&
              str(node%typ,2)//' -> '//str(node%typ_new,2))
        endif
      enddo  ! jNode/
    enddo  ! jEnt/
    !-----------------------------------------------------------
    call free_network_dct_point(nwk)

    !call free_network_tbl_ent(nwk)
    ! Free only entity
    do jEnt = 1, nwk%nEnt
      tbe => nwk%tbl_ent(jEnt)
      call shp_clear_entity(tbe%ent)
    enddo  ! jEnt/
  enddo  ! iNwk/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dct_node%n = 0
  deallocate(dct_node%lon)
  deallocate(dct_node%lat)
  deallocate(dct_node%typ)
  deallocate(dct_node%id)

  dct_node_identical%n = 0
  deallocate(dct_node_identical%lon)
  deallocate(dct_node_identical%lat)
  deallocate(dct_node_identical%typ)
  deallocate(dct_node_identical%i)
  !-------------------------------------------------------------
  ! Output node types and connections
  !-------------------------------------------------------------
!if( .false. )then
  call logent('Outputting node types and connections')

  wfmt = "(3x,i"//str(dgt(nNwk))//&
          ",1x,i"//str(dgt_uid)//"."//str(dgt_uid)//&
          ",1x,i"//str(dgt(maxval(network(:)%nEnt)))//&
          ",1x,es10.3"//&
          ",2(1x,f12.7),2(1x,f11.7))"

  f = get_f_tmp_networks_lst()
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(1x,a,1x,i0)") 'networks', nNwk
  write(un,"(3x,a)") 'i uid mEnt leng west east south north'
  do iNwk = 1, nNwk
    nwk => network(iNwk)
    if( debug .and. nwk%uid /= uid_debug ) cycle

    write(un,wfmt) &
        iNwk, nwk%uid, nwk%nEnt, nwk%leng, &
        nwk%west, nwk%east, nwk%south, nwk%north
  enddo  ! iNwk/
  close(un)

  wfmt = "(5x,i"//str(dgt_irEnt)//",5(1x,i2))"

  do iNwk = 1, nNwk
    nwk => network(iNwk)
    if( debug .and. nwk%uid /= uid_debug ) cycle

    f = get_f_tmp_network_entity(str(nwk%uid,-dgt_uid))
    if( iNwk <= 2 .or. iNwk >= nNwk-1 )then
      call logmsg('Writing '//str(f))
    elseif( iNwk == 3 )then
      call logmsg('...')
    endif
    open(newunit=un, file=f, status='replace')

    jEnt = 0

    write(un,"(1x,a,1x,i0)") 'regions', nwk%mRegion
    do iiRegion = 1, nwk%mRegion
      nwkrgn => nwk%region(iiRegion)
      write(un,"(3x,a,1x,i0)") &
        region_idx2str(nwkrgn%iRegion), nwkrgn%mEnt
      write(un,"(5x,a)") &
          'irEnt upNodeType downNodeType upNodeTypeNew downNodeTypeNew '//&
          'nDiv'
      do iiEnt = 1, nwkrgn%mEnt
        call add(jEnt)
        tbe => nwk%tbl_ent(jEnt)
        write(un,wfmt) &
            nwkrgn%irEnt(iiEnt), tbe%node(1)%typ, tbe%node(2)%typ, &
            tbe%node(1)%typ_new, tbe%node(2)%typ_new, tbe%nDiv
        if( tbe%nDiv > 0 )then
          write(un,"(7x,a,"//str(tbe%nDiv)//"(1x,i0)))") 'div', tbe%jPt_div(:tbe%nDiv)
        endif
      enddo  ! iiEnt/
    enddo  ! iiRegion/

    close(un)
  enddo  ! iNwk/

  call logext()
!endif
  !-------------------------------------------------------------
  ! Remake networks
  !-------------------------------------------------------------
  call logent('Remaking networks')

  do iNwk = 1, nNwk
    !-----------------------------------------------------------
    ! Read shape data
    ! *** Node info. is not updated
    !-----------------------------------------------------------
    nwk => network(iNwk)

    call read_network_shp(nwk, read_node_attr=.false., read_stream_codes=.true.)
    !-----------------------------------------------------------
    ! Make channels
    !-----------------------------------------------------------
    call logent('Making channels', opt='-p -x2')

    nCh = nwk%nEnt + sum(nwk%tbl_ent(:)%nDiv)
    allocate(lst_ch(nCh))

    dct_node%n = nCh * 2
    allocate(dct_node%lon(dct_node%n))
    allocate(dct_node%lat(dct_node%n))
    allocate(dct_node%jCh(dct_node%n))
    allocate(dct_node%jEnt(dct_node%n))
    allocate(dct_node%jNode(dct_node%n))

    jCh = 0
    iNode = 0
    do jEnt = 1, nwk%nEnt
      tbe => nwk%tbl_ent(jEnt)
      part => tbe%ent%part(1)

      if( tbe%nDiv == 0 )then
        call add(jCh)
        ch => lst_ch(jCh)

        ch%jEnt = jEnt

        ch%n = part%nPoint
        ch%lon => part%x(:)
        ch%lat => part%y(:)

        allocate(ch%node(2))
        do jNode = 1, 2
          chnode => ch%node(jNode)
          jPt = jNode2jPt(jNode, ch%n)
          chnode%lon = ch%lon(jPt)
          chnode%lat = ch%lat(jPt)
          chnode%typ = tbe%node(jNode)%typ_new

          call add(iNode)
          dct_node%lon(iNode) = chnode%lon
          dct_node%lat(iNode) = chnode%lat
          dct_node%jCh(iNode) = jCh
          dct_node%jEnt(iNode) = jEnt
          dct_node%jNode(iNode) = jNode
        enddo
      else
        jPte = 1
        do jDiv = 1, tbe%nDiv+1
          jPts = jPte
          if( jDiv <= tbe%nDiv )then
            jPte = tbe%jPt_div(jDiv)
          else
            jPte = part%nPoint
          endif

          call add(jCh)
          ch => lst_ch(jCh)

          ch%jEnt = jEnt

          ch%n = jPte - jPts + 1
          allocate(ch%lon(ch%n))
          allocate(ch%lat(ch%n))
          ch%lon = part%x(jPts:jPte)
          ch%lat = part%y(jPts:jPte)

          allocate(ch%node(2))
          do jNode = 1, 2
            chnode => ch%node(jNode)
            jPt = jNode2jPt(jNode, ch%n)
            chnode%lon = ch%lon(jPt)
            chnode%lat = ch%lat(jPt)

            if( jNode == 1 )then
              if( jPts == 1 )then
                chnode%typ = tbe%node(jNode)%typ_new
              else
                chnode%typ = NODETYPE_NEW__NEWNODE
              endif
            else
              if( jPte == part%nPoint )then
                chnode%typ = tbe%node(jNode)%typ_new
              else
                chnode%typ = NODETYPE_NEW__NEWNODE
              endif
            endif

            call add(iNode)
            dct_node%lon(iNode) = chnode%lon
            dct_node%lat(iNode) = chnode%lat
            dct_node%jCh(iNode) = jCh
            dct_node%jEnt(iNode) = jEnt
            dct_node%jNode(iNode) = jNode
          enddo  ! jNode/
        enddo  ! jDiv/
      endif
    enddo  ! jEnt/

    if( jCh /= nCh )then
      call errend(msg_unexpected_condition()//&
                '\n  jCh /= nCh')
    endif

    allocate(arg(dct_node%n))
    call argsort(dct_node%lon, arg)
    call sort(dct_node%lon, arg)
    call sort(dct_node%lat, arg)
    call sort(dct_node%jCh, arg)
    call sort(dct_node%jEnt, arg)
    call sort(dct_node%jNode, arg)
    ie = 0
    do while( ie < dct_node%n )
      is = ie + 1
      ie = is
      do while( ie < dct_node%n )
        if( dct_node%lon(ie+1) /= dct_node%lon(is) ) exit
        ie = ie + 1
      enddo
      call argsort(dct_node%lat(is:ie), arg(is:ie))
      call sort(dct_node%lat(is:ie), arg(is:ie))
      call sort(dct_node%jCh(is:ie), arg(is:ie))
      call sort(dct_node%jEnt(is:ie), arg(is:ie))
      call sort(dct_node%jNode(is:ie), arg(is:ie))
    enddo
    deallocate(arg)

    call logext()
    !-----------------------------------------------------------
    ! Calc. elevation of nodes
    !-----------------------------------------------------------
    call logent('Calculating elevation of nodes', opt='-p -x2')

    gxs = jflw_gxs_of_lon(nwk%west)
    gxe = jflw_gxe_of_lon(nwk%east)
    gys = jflw_gys_of_lat(nwk%north)
    gye = jflw_gye_of_lat(nwk%south)

    allocate(elvmap(gxs-2:gxe+2,gys-2:gye+2))
    call jflw_read_map_from_tile(&
        RESOLUTION_1SEC, 'elv', DTYPE_REAL, JFLW_ELV_MISS, gxs-2, gys-2, elvmap)

    do jCh = 1, nCh
      ch => lst_ch(jCh)
      do jNode = 1, 2
        chnode => ch%node(jNode)
        chnode%elv =  calc_elv(&
            chnode%lon, chnode%lat, &
            gxs, gxe, gys, gye, elvmap, ELV__FORMISS)
      enddo  ! jNode/
    enddo  ! jCh/

    deallocate(elvmap)

    call logext()
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call logent('Calculating channel length', opt='-p -x2')

    do jCh = 1, nCh
      ch => lst_ch(jCh)
      ch%leng = 0.d0
      do jPt = 1, ch%n-1
        call add(ch%leng, &
            dist_sphere(ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, &
                        ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r) * EARTH_R)
      enddo  ! jPt/
    enddo  ! jCh/

    call logext()
    !-----------------------------------------------------------
    ! Update node type
    !-----------------------------------------------------------
    call logent('Updating node type', opt='-p -x2')

    do jCh = 1, nCh
      ch => lst_ch(jCh)
      do jNode = 1, 2
        chnode => ch%node(jNode)
        if( search_2(chnode%lon, chnode%lat, &
            dct_node%lon, dct_node%lat, iNodes, iNodee) /= 0 )then
          cycle
        endif

        mNode_src = 0
        mNode_out = 0
        mNode_inter = 0
        mNode_unknown = 0
        do iNode = iNodes, iNodee
          jCh2 = dct_node%jCh(iNode)
          jNode2 = dct_node%jNode(iNode)
          !if( jCh2 == jCh .and. jNode2 == jNode ) cycle
          selectcase( lst_ch(jCh2)%node(jNode2)%typ )
          case( NODETYPE_NEW__SRC )
            call add(mNode_src)
          case( NODETYPE_NEW__OUT )
            call add(mNode_out)
          case( NODETYPE_NEW__INTER )
            call add(mNode_inter)
          case( NODETYPE_NEW__SRC_INTER )
            call add(mNode_src)
            call add(mNode_inter)
          case( NODETYPE_NEW__OUT_INTER )
            call add(mNode_out)
            call add(mNode_inter)
          case( NODETYPE_NEW__SRC_OUT )
            call add(mNode_src)
            call add(mNode_out)
          case( NODETYPE_NEW__ALLMIXED )
            call add(mNode_src)
            call add(mNode_out)
            call add(mNode_inter)
          case( NODETYPE_NEW__UNKNOWN )
            call add(mNode_unknown)
          case( NODETYPE_NEW__NEWNODE )
            call add(mNode_unknown)
          case( NODETYPE_NEW__UNDEF )
            call errend(msg_unexpected_condition()//&
                      '\n  node%typ == NODETYPE_NEW__UNDEF'//&
                      '\n  jEnt: '//str(dct_node%jEnt(iNode))//&
                        ', jNode: '//str(jNode2))
          case default
            call errend(msg_invalid_value('node%typ', lst_ch(jCh2)%node(jNode2)%typ))
          endselect
        enddo  ! iNode/

        if( mNode_unknown > 0 )then
          chnode%typ = NODETYPE_NEW__UNKNOWN
        elseif( mNode_src >  0 .and. mNode_out == 0 .and. mNode_inter == 0 )then
          chnode%typ = NODETYPE_NEW__SRC
        elseif( mNode_src >  0 .and. mNode_out >  0 .and. mNode_inter == 0 )then
          chnode%typ = NODETYPE_NEW__SRC_OUT
        elseif( mNode_src >  0 .and. mNode_out == 0 .and. mNode_inter >  0 )then
          chnode%typ = NODETYPE_NEW__SRC_INTER
        elseif( mNode_src == 0 .and. mNode_out >  0 .and. mNode_inter == 0 )then
          chnode%typ = NODETYPE_NEW__OUT
        elseif( mNode_src == 0 .and. mNode_out >  0 .and. mNode_inter >  0 )then
          chnode%typ = NODETYPE_NEW__OUT_INTER
        elseif( mNode_src == 0 .and. mNode_out == 0 .and. mNode_inter >  0 )then
          chnode%typ = NODETYPE_NEW__INTER
        elseif( mNode_src >  0 .and. mNode_out >  0 .and. mNode_inter >  0 )then
          chnode%typ = NODETYPE_NEW__ALLMIXED
        else
          call errend('Not matched any case for mNode.'//&
                    '\n  src: '//str(mNode_src)//', out: '//str(mNode_out)//&
                      ', inter: '//str(mNode_inter)//', unknown: '//str(mNode_unknown))
        endif
      enddo  ! jNode/
    enddo  ! jCh/

    call logext()
    !-------------------------------------------------------------
    ! Output
    !-------------------------------------------------------------
    call logent('Outputting', opt='-p -x2')

    ! For Fortran (unformatted, sequential)
    f = get_f_tmp_network_channel(str(nwk%uid,-dgt_uid), 'sbin')
    open(newunit=un, file=f, form='unformatted', access='sequential', status='replace')
    write(un) nCh, nwk%west, nwk%east, nwk%south, nwk%north
    do jCh = 1, nCh
      ch => lst_ch(jCh)
      tbe => nwk%tbl_ent(ch%jEnt)
      write(un) minval(ch%lon), maxval(ch%lon), minval(ch%lat), maxval(ch%lat)
      write(un) len_trim(tbe%wsCode), len_trim(tbe%rvCode), len_trim(tbe%rvName)
      write(un) tbe%wsCode
      write(un) tbe%rvCode
      write(un) tbe%rvName
      write(un) ch%n
      write(un) ch%lon
      write(un) ch%lat
      write(un) ch%leng
      do jNode = 1, 2
        chnode => ch%node(jNode)
        write(un) chnode%typ, chnode%elv
      enddo  ! jNode/
    enddo  ! jCh/
    close(un)

    ! For Python (JSON)
    f = get_f_tmp_network_channel(str(nwk%uid,-dgt_uid), 'json')
    open(newunit=un, file=f, status='replace')
    write(un,"(a)") '{'
    write(un,"(2x,a)") '"bbox": ['//str((/nwk%west,nwk%east,nwk%south,nwk%north/),&
                       'es20.13',',')//'],'
    write(un,"(2x,a)") '"channel": ['
    do jCh = 1, nCh
      ch => lst_ch(jCh)
      tbe => nwk%tbl_ent(ch%jEnt)

      write(un,"(4x,a)")  '{'
      write(un,"(6x,a)")  '"wsCode": "'//tbe%wsCode//'",'
      write(un,"(6x,a)")  '"rvCode": "'//tbe%rvCode//'",'
      write(un,"(6x,a)")  '"rvName": "'//tbe%rvName//'",'
      write(un,"(6x,a)")  '"bbox": ['//str((/minval(ch%lon),maxval(ch%lon),&
                          minval(ch%lat),maxval(ch%lat)/),'es18.11',',')//'],'
      write(un,"(6x,a)")  '"lon": ['//str(ch%lon,'es18.11',',')//'],'
      write(un,"(6x,a)")  '"lat": ['//str(ch%lat,'es18.11',',')//'],'
      write(un,"(6x,a)")  '"length": '//str(ch%leng,'es10.3')//','
      write(un,"(6x,a)")  '"node": ['
      do jNode = 1, 2
        chnode => ch%node(jNode)
        write(un,"(8x,a)")  '{'
        write(un,"(10x,a)")   '"type": '//str(chnode%typ)//','
        write(un,"(10x,a)")   '"elevation": '//str(chnode%elv,'es10.3')
        write(un,"(8x,a)")  '}'//comma_json(jNode,2)
      enddo
      write(un,"(6x,a)")  ']'
      write(un,"(4x,a)")  '}'//comma_json(jCh,nCh)
    enddo  ! jCh/
    write(un,"(2x,a)") ']'
    write(un,"(a)") '}'
    close(un)

    call logext()
    !-------------------------------------------------------------
    !
    !-------------------------------------------------------------
    dct_node%n = 0
    deallocate(dct_node%lon)
    deallocate(dct_node%lat)
    deallocate(dct_node%jCh)
    deallocate(dct_node%jEnt)
    deallocate(dct_node%jNode)

    nullify(ch)
    deallocate(lst_ch)

    ! Free only entity
    call free_network_tbl_ent(nwk)
  enddo  ! iNwk/

  call logmsg('Output files (sequential unformatted):')
  do iNwk = 1, nNwk
    if( iNwk <= 2 .or. iNwk >= nNwk-1 )then
      call logmsg('  '//get_f_tmp_network_channel(str(network(iNwk)%uid,-dgt_uid), 'sbin'))
    elseif( iNwk == 3  )then
      call logmsg('  ...')
    endif
  enddo

  call logmsg('Output files (JSON):')
  do iNwk = 1, nNwk
    if( iNwk <= 2 .or. iNwk >= nNwk-1 )then
      call logmsg('  '//get_f_tmp_network_channel(str(network(iNwk)%uid,-dgt_uid), 'json'))
    elseif( iNwk == 3  )then
      call logmsg('  ...')
    endif
  enddo

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(nwk)
  deallocate(network)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine get_network_bbox(nwk)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__get_network_bbox'
  type(network_), intent(inout) :: nwk

  real(8) :: west, east, south, north

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nwk%west = NLNI_REGION_EAST
  nwk%east = NLNI_REGION_WEST
  nwk%south = NLNI_REGION_NORTH
  nwk%north = NLNI_REGION_SOUTH

  jEnt = 0
  do iiRegion = 1, nwk%mRegion
    nwkrgn => nwk%region(iiRegion)
    regionName = region_idx2str(nwkrgn%iRegion)
    f_shp = get_f_stream_shp(regionName)
    call traperr( shp_open(f_shp) )
    do iiEnt = 1, nwkrgn%mEnt
      call traperr( shp_get_entity_info(&
             nwkrgn%irEnt(iiEnt), &
             xmin=west, xmax=east, ymin=south, ymax=north) )

      nwk%west = min(nwk%west, west)
      nwk%east = max(nwk%east, east)
      nwk%south = min(nwk%south, south)
      nwk%north = max(nwk%north, north)
    enddo  ! iiEnt/
    call traperr( shp_close() )
  enddo  ! iiRegion/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_network_bbox
!---------------------------------------------------------------
subroutine read_network_shp(nwk, read_node_attr, read_stream_codes)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__read_network_shp'
  type(network_), intent(inout) :: nwk
  logical, intent(in), optional :: read_node_attr
  logical, intent(in), optional :: read_stream_codes

  type(dbf_) :: dbf
  logical :: read_node_attr_
  logical :: read_stream_codes_
  logical :: read_dbf

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  read_node_attr_ = .false.
  read_stream_codes_ = .false.
  if( present(read_node_attr) ) read_node_attr_ = read_node_attr
  if( present(read_stream_codes) ) read_stream_codes_ = read_stream_codes
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  read_dbf = read_node_attr_ .or. read_stream_codes_

  if( .not. associated(nwk%tbl_ent) )then
    allocate(nwk%tbl_ent(nwk%nEnt))
  endif

  jEnt = 0
  do iiRegion = 1, nwk%mRegion
    nwkrgn => nwk%region(iiRegion)

    regionName = region_idx2str(nwkrgn%iRegion)
    f_shp = get_f_stream_shp(regionName)
    call traperr( shp_open(f_shp) )

    if( read_dbf )then
      call traperr( dbf_open(f_shp) )
      call traperr( dbf_get_info(dbf) )
    endif

    do iiEnt = 1, nwkrgn%mEnt
      call add(jEnt)
      tbe => nwk%tbl_ent(jEnt)

      tbe%iRegion = nwkrgn%iRegion
      tbe%irEnt = nwkrgn%irEnt(iiEnt)
      call traperr( shp_get_entity(tbe%irEnt, tbe%ent) )

      if( read_dbf )then
        call traperr( dbf_get_record(tbe%irEnt, dbf%field, rec) )
      endif

      if( read_node_attr_ )then
        allocate(tbe%node(2))
        tbe%node(1)%id = rec%value(IDX_FIELD_STREAM__NDUPSTR)%s(2:)
        tbe%node(2)%id = rec%value(IDX_FIELD_STREAM__NDDOWNSTR)%s(2:)
        tbe%node(:)%typ = NODETYPE__UNDEF
        tbe%node(:)%typ_new = NODETYPE_NEW__UNDEF
      endif

      if( read_stream_codes_ )then
        allocate(character(1) :: tbe%wsCode)
        allocate(character(1) :: tbe%rvCode)
        allocate(character(1) :: tbe%rvName)
        tbe%wsCode = rec%value(IDX_FIELD_STREAM__WSCODE)%s
        tbe%rvCode = rec%value(IDX_FIELD_STREAM__RIVCODE)%s
        tbe%rvName = rec%value(IDX_FIELD_STREAM__RIVNAME)%s
      endif
    enddo  ! iiEnt/

    call traperr( shp_close() )

    if( read_dbf )then
      call traperr( dbf_close() )
      call dbf_clear_all(dbf)
    endif
  enddo  ! iiRegion/

  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_network_shp
!---------------------------------------------------------------
subroutine make_entity_dct_point(nwk)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__make_entity_dct_point'
  type(network_), intent(inout) :: nwk

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(arg(1))

  do jEnt = 1, nwk%nEnt
    tbe => nwk%tbl_ent(jEnt)
    part => tbe%ent%part(1)

    dcp => tbe%dct_point
    call init_dct_point(dcp)

    dcp%n = part%nPoint
    allocate(dcp%lon(dcp%n))
    allocate(dcp%lat(dcp%n))
    allocate(dcp%jPt(dcp%n))
    dcp%lon(:) = part%x(:)
    dcp%lat(:) = part%y(:)
    do jPt = 1, dcp%n
      dcp%jPt(jPt) = jPt
    enddo

    if( dcp%n > size(arg) )then
      call realloc(arg, dcp%n)
    endif
    call argsort(dcp%lon, arg(:dcp%n))
    call sort(dcp%lon, arg(:dcp%n))
    call sort(dcp%lat, arg(:dcp%n))
    call sort(dcp%jPt, arg(:dcp%n))
    ie = 0
    do while( ie < dcp%n )
      is = ie + 1
      ie = is
      do while( ie < dcp%n )
        if( dcp%lon(ie+1) /= dcp%lon(is) ) exit
        ie = ie + 1
      enddo
      if( ie == is ) cycle
      call argsort(dcp%lat(is:ie), arg(is:ie))
      call sort(dcp%lat(is:ie), arg(is:ie))
      call sort(dcp%jPt(is:ie), arg(is:ie))
    enddo
  enddo  ! jEnt/

  deallocate(arg)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_entity_dct_point
!---------------------------------------------------------------
subroutine make_network_dct_point(nwk)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__make_network_dct_point'
  type(network_), intent(inout), target :: nwk

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !  
  !-------------------------------------------------------------
  dcp => nwk%dct_point
  call init_dct_point(dcp)

  dcp%n = 0
  do jEnt = 1, nwk%nEnt
    call add(dcp%n, nwk%tbl_ent(jEnt)%ent%part(1)%nPoint)
  enddo  ! jEnt/

  allocate(dcp%lon(dcp%n))
  allocate(dcp%lat(dcp%n))
  allocate(dcp%jEnt(dcp%n))
  allocate(dcp%jPt(dcp%n))
  allocate(dcp%nodetyp(dcp%n))
  allocate(dcp%nodetyp_new(dcp%n))

  dcp%nodetyp(:) = NODETYPE__NOTNODE
  dcp%n = 0
  do jEnt = 1, nwk%nEnt
    tbe => nwk%tbl_ent(jEnt)
    part => tbe%ent%part(1)

    dcp%lon(dcp%n+1:dcp%n+part%nPoint) = part%x(:)
    dcp%lat(dcp%n+1:dcp%n+part%nPoint) = part%y(:)
    dcp%jEnt(dcp%n+1:dcp%n+part%nPoint) = jEnt
    do jPt = 1, part%nPoint
      dcp%jPt(dcp%n+jPt) = jPt
    enddo

    do jNode = 1, 2
      node => tbe%node(jNode)
      jPt = jNode2jPt(jNode, part%nPoint)

      !-------------------------------------------------------
      ! Case: Node was not found in the global list
      if( search_2(&
          part%x(jPt), part%y(jPt), dct_node%lon, dct_node%lat, &
          iNodes, iNodee) /= 0 )then
        !print*, trim(region_idx2str(tbe%iRegion)), tbe%irEnt, jPt
        !print*, part%x(jPt), part%y(jPt)
        !call errend('Ent('//str(jEnt)//') node('//str(jNode)//'): '//&
        !            'Node was not found in the global list.')
        call logwrn('Ent('//str(jEnt)//') node('//str(jNode)//'): '//&
                    'Node was not found in the global list.', &
                    opt='-p')
        dcp%nodetyp(dcp%n+jPt) = NODETYPE__NOTFOUND
        node%typ = NODETYPE__NOTFOUND
        cycle
      endif
      !---------------------------------------------------------
      ! Case: Node was found in the global list
      do iNode = iNodes, iNodee
        if( node%id /= dct_node%id(iNode) ) cycle

        if( dcp%nodetyp(dcp%n+jPt) /= NODETYPE__NOTNODE )then
          if( dcp%nodetyp(dcp%n+jPt) /= dct_node%typ(iNode) )then
            call errend('dcp%nodetyp /= dct_node%typ')
          endif
        endif

        if( node%typ /= NODETYPE__UNDEF )then
          if( node%typ /= dct_node%typ(iNode) )then
            call errend('node%typ /= dct_node%typ')
          endif
        endif

        dcp%nodetyp(dcp%n+jPt) = dct_node%typ(iNode)
        node%typ = dct_node%typ(iNode)
      enddo  ! iNode/

      if( dcp%nodetyp(dcp%n+jPt) == NODETYPE__NOTNODE )then
        call errend(&
              'Node was not found in the global list. '//&
              'Nwk('//str(iNwk,dgt(nNwk))//') Ent('//str(jEnt)//') node('//str(jNode)//')')
      endif
    enddo  ! jNode/

    call add(dcp%n, part%nPoint)
  enddo  ! jEnt/

  allocate(arg(dcp%n))
  call argsort(dcp%lon, arg)
  call sort(dcp%lon, arg)
  call sort(dcp%lat, arg)
  call sort(dcp%jEnt, arg)
  call sort(dcp%jPt, arg)
  call sort(dcp%nodetyp, arg)

  ie = 0
  do while( ie < size(dcp%lon) )
    is = ie + 1
    ie = is
    do while( ie < size(dcp%lon) )
      if( dcp%lon(ie+1) /= dcp%lon(is) ) exit
      ie = ie + 1
    enddo

    call argsort(dcp%lat(is:ie), arg(is:ie))
    call sort(dcp%lat(is:ie), arg(is:ie))
    call sort(dcp%jEnt(is:ie), arg(is:ie))
    call sort(dcp%jPt(is:ie), arg(is:ie))
    call sort(dcp%nodetyp(is:ie), arg(is:ie))
  enddo

  dcp%nodetyp_new(:) = NODETYPE_NEW__UNDEF

  deallocate(arg)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_network_dct_point
!---------------------------------------------------------------
subroutine free_network_tbl_ent(nwk)
  implicit none
  type(network_), intent(inout) :: nwk

  deallocate(nwk%tbl_ent)
  nullify(nwk%tbl_ent)
end subroutine free_network_tbl_ent
!---------------------------------------------------------------
subroutine free_entity_dct_point(nwk)
  implicit none
  type(network_), intent(inout) :: nwk

  do iEnt = 1, nwk%nEnt
    call free_dct_point(nwk%tbl_ent(iEnt)%dct_point)
  enddo
end subroutine free_entity_dct_point
!---------------------------------------------------------------
subroutine free_network_dct_point(nwk)
  implicit none
  type(network_), intent(inout) :: nwk

  call free_dct_point(nwk%dct_point)
end subroutine free_network_dct_point
!---------------------------------------------------------------
end subroutine postConnectChannels
!===============================================================
!
!===============================================================
subroutine connectChannels_core(&
    tx, ty, is_tile_exist, &
    tbl_rgn_tile, &
    is_tile_updated, is_local_tile_updated)
  use c3_nlni_const
  use c2_nlni_io, only: &
        nlni_tilename => tilename
  use c2_nlni_grid, only: &
        nlni_west_of_tx   => west_of_tx, &
        nlni_east_of_tx   => east_of_tx, &
        nlni_south_of_ty  => south_of_ty, &
        nlni_north_of_ty  => north_of_ty, &
        nlni_txs_of_lon   => txs_of_lon, &
        nlni_txe_of_lon   => txe_of_lon, &
        nlni_tys_of_lat   => tys_of_lat, &
        nlni_tye_of_lat   => tye_of_lat
  use c2_strnk_const
  use c2_strnk_io, only: &
        region_idx2str     , &
        region_str2idx     , &
        get_f_stream_shp   , &
        get_f_lst_tiled_idx
  use mod_util, only: &
        search_2
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'connectChannels_core'

  integer            , intent(in)    :: tx, ty
  logical            , intent(in)    :: is_tile_exist(NLNI_TXMIN:,NLNI_TYMIN:)
  type(tbl_rgn_tile_), pointer       :: tbl_rgn_tile(:,:)  ! inout
  logical            , intent(inout) :: is_tile_updated(NLNI_TXMIN:,NLNI_TYMIN:)
  logical            , intent(out)   :: is_local_tile_updated

  ! iRegion: Global region index (only global for region index)
  ! irEnt: Entity index in the region
  ! itEnt: Entity index in the tile

  type(tbl_rgn_tile_), pointer :: tbl_rgn_tile_self, &
                                  tbl_rgn_tile_other
  type(rgn_tile_), pointer :: rgn_tile     , &
                              rgn_tile_self, &
                              rgn_tile_other

  type(tbl_ent_), pointer :: tbl_ent(:), tbe
  type(shp_part_), pointer :: part
  type(point_conn_), pointer :: ptc
  type(dct_point_) :: dcp

  character(CLEN_VAR) :: regionName
  integer :: mRegion, iiRegion, iRegion
  integer :: ntEnt   ! Number of entity in the tile
  integer :: itEnt   ! Counter for $ntEnt
  integer :: jtEnt   ! Counter for $ntEnt
  integer :: mEnt    ! Local number of entity
  integer :: iiEnt   ! Local counter of entity
  integer :: jjEnt   ! Local counter of entity
  real(8) :: twest, teast, tsouth, tnorth
  real(8) :: west, east, south, north
  integer :: txs, txe, tys, tye, itx, ity
  integer :: is, ie
  integer, allocatable :: arg(:)
  integer :: iPt
  integer :: iiPt
  logical :: is_updated
  integer :: uid_min
  integer :: jPt, jPts, jPte
  real(8) :: plon, plat

  integer :: iLoop

  character(CLEN_PATH) :: f_lst_tiled_idx, f_shp
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM, '-ar')
  !-------------------------------------------------------------
  ! Initial setting
  !-------------------------------------------------------------
  is_local_tile_updated = .false.

  twest = nlni_west_of_tx(tx)
  teast = nlni_east_of_tx(tx)
  tsouth = nlni_south_of_ty(ty)
  tnorth = nlni_north_of_ty(ty)
  call logmsg('Tile ('//str((/tx,ty/),NLNI_DGT_TXY,', ')//')')
  call logmsg('BBox: ['//str((/twest,teast,tsouth,tnorth/),'f12.7',', ')//']')
  !-------------------------------------------------------------
  ! Get channels whose bbox intersect the bbox of this tile
  !-------------------------------------------------------------
  call logent('Getting channels potentially intersecting the tile')

  f_lst_tiled_idx = get_f_lst_tiled_idx(nlni_tilename(tx,ty))
  if( access(f_lst_tiled_idx,' ') /= 0 )then
    call logmsg('File not found: '//str(f_lst_tiled_idx))
    call logret(PRCNAM, MODNAM)
    return
  endif

  ! Allocate $tbl_ent
  ntEnt = 0
  open(newunit=un, file=f_lst_tiled_idx, status='old')
  read(un,*) c_, mRegion
  do iiRegion = 1, mRegion
    read(un,*) regionName, mEnt
    do iiEnt = 1, mEnt
      read(un,*)
    enddo
    call add(ntEnt, mEnt)
  enddo

  if( ntEnt == 0 )then
    call logmsg('No entity.')
    close(un)
    call logret(PRCNAM, MODNAM)
    return
  endif
  call logmsg('Entities: '//str(ntEnt))

  allocate(tbl_ent(ntEnt))
  do itEnt = 1, ntEnt
    tbe => tbl_ent(itEnt)
    tbe%is_updated = .false.
  enddo
  west = NLNI_REGION_EAST
  east = NLNI_REGION_WEST
  south = NLNI_REGION_NORTH
  north = NLNI_REGION_SOUTH

  ! Read entity data
  ntEnt = 0
  rewind(un)
  read(un,*) c_, mRegion
  do iiRegion = 1, mRegion
    read(un,*) regionName, mEnt
    iRegion = region_str2idx(regionName)

    f_shp = get_f_stream_shp(region_idx2str(iRegion))
    call traperr( shp_open(f_shp) )

    ! Get %irEnt, %iRegion, %ent
    do iiEnt = 1, mEnt
      call add(ntEnt)
      tbe => tbl_ent(ntEnt)

      read(un,*) tbe%irEnt
      tbe%iRegion = iRegion
      call traperr( shp_get_entity(tbe%irEnt, tbe%ent) )

      west  = min(west , tbe%ent%xmin)
      east  = max(east , tbe%ent%xmax)
      south = min(south, tbe%ent%ymin)
      north = max(north, tbe%ent%ymax)

      if( tbe%ent%nPart > 1 )then
        call errend(msg_unexpected_condition()//&
                  '\n  nPart of entity > 1')
      endif
    enddo  ! iiEnt/

    call traperr( shp_close() )
  enddo  ! iiRegion/

  close(un)

  call logmsg('Comprehensive BBox of entities: ['//&
         str((/west,east/),'f12.7',',')//','//&
         str((/south,north/),'f11.7',',')//']')

  call logext()
  !-------------------------------------------------------------
  ! Make a list of points
  !-------------------------------------------------------------
  call logent('Making a list of points')

  dcp%n = 0
  do itEnt = 1, ntEnt
    tbe => tbl_ent(itEnt)
    part => tbe%ent%part(1)
    allocate(tbe%point(part%nPoint))
    call add(dcp%n, part%nPoint)
  enddo  ! itEnt/
  call logmsg('Points: '//str(dcp%n))

  allocate(dcp%lon(dcp%n), &
           dcp%lat(dcp%n), &
           dcp%itEnt(dcp%n), &
           dcp%iPt(dcp%n))

  dcp%n = 0
  do itEnt = 1, ntEnt
    tbe => tbl_ent(itEnt)
    part => tbe%ent%part(1)
    dcp%lon(dcp%n+1:dcp%n+part%nPoint) = part%x(:)
    dcp%lat(dcp%n+1:dcp%n+part%nPoint) = part%y(:)
    dcp%itEnt(dcp%n+1:dcp%n+part%nPoint) = itEnt
    do iPt = dcp%n+1, dcp%n+part%nPoint
      dcp%iPt(iPt) = iPt
    enddo  ! iPt/
    call add(dcp%n, part%nPoint)
  enddo  ! itEnt/

  allocate(arg(dcp%n))
  call argsort(dcp%lon, arg)
  call sort(dcp%lon, arg)
  call sort(dcp%lat, arg)
  call sort(dcp%itEnt, arg)
  call sort(dcp%iPt, arg)

  ie = 0
  do while( ie < dcp%n )
    is = ie + 1
    ie = is
    do while( ie < dcp%n )
      if( dcp%lon(ie+1) /= dcp%lon(is) ) exit
      ie = ie + 1
    enddo
    if( is == ie ) cycle
    call argsort(dcp%lat(is:ie), arg(is:ie))
    call sort(dcp%lon(is:ie), arg(is:ie))
    call sort(dcp%lat(is:ie), arg(is:ie))
    call sort(dcp%itEnt(is:ie), arg(is:ie))
    call sort(dcp%iPt(is:ie), arg(is:ie))
  enddo
  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  ! Search for connections
  !-------------------------------------------------------------
  call logent('Searching for connections')

  do itEnt = 1, ntEnt
    tbe => tbl_ent(itEnt)
    part => tbe%ent%part(1)

    do iPt = 1, part%nPoint
      ptc => tbe%point(iPt)
      ptc%nPoint_conn = 0

      plon = part%x(iPt)
      plat = part%y(iPt)
      if( search_2(plon, plat, dcp%lon, dcp%lat, jPts, jPte) /= 0 )then
        call errend('Coordinate was not found in the list.')
      endif

      ! CYCLE: No connection
      if( jPts == jPte )then
        if( dcp%itEnt(jPts) /= itEnt )then
          call errend('Node was not found in the list.')
        endif

        cycle
      endif

      ptc%nPoint_conn = jPte - jPts + 1 - 1
      if( ptc%nPoint_conn == 0 ) cycle

      allocate(ptc%itEnt_conn(ptc%nPoint_conn), &
               ptc%iPt_conn(ptc%nPoint_conn))

      ptc%nPoint_conn = 0
      do jPt = jPts, jPte
        jtEnt = dcp%itEnt(jPt)
        if( jtEnt == itEnt ) cycle
        call add(ptc%nPoint_conn)
        ptc%itEnt_conn(ptc%nPoint_conn) = jtEnt
        ptc%iPt_conn(ptc%nPoint_conn) = dcp%iPt(jPt)
      enddo  ! jPt/
    enddo  ! iPoint/
  enddo  ! itEnt/

  call logext()
  !-------------------------------------------------------------
  ! Get minimum global ids
  !-------------------------------------------------------------
  call logent('Getting minimum global ids')

  tbl_rgn_tile_self => tbl_rgn_tile(tx,ty)

  do itEnt = 1, ntEnt
    tbe => tbl_ent(itEnt)
    rgn_tile_self => tbl_rgn_tile_self%region(tbe%iRegion)
    call search(tbe%irEnt, rgn_tile_self%irEnt, iiEnt)
    tbe%uid = rgn_tile_self%uid(iiEnt)
  enddo

  txs = nlni_txs_of_lon(west)
  txe = nlni_txe_of_lon(east)
  tys = nlni_tys_of_lat(south)
  tye = nlni_tye_of_lat(north)
  do ity = tys, tye
  do itx = txs, txe
    if( .not. is_tile_exist(itx,ity) ) cycle
    if( itx == tx .and. ity == ty ) cycle

    tbl_rgn_tile_other => tbl_rgn_tile(itx,ity)
    do iRegion = 1, NREGION
      rgn_tile_self => tbl_rgn_tile_self%region(iRegion)
      rgn_tile_other => tbl_rgn_tile_other%region(iRegion)
      if( rgn_tile_self%mEnt == 0 .or. rgn_tile_other%mEnt == 0 ) cycle
      do jjEnt = 1, rgn_tile_other%mEnt
        call search(rgn_tile_other%irEnt(jjEnt), rgn_tile_self%irEnt, iiEnt)
        if( iiEnt == 0 ) cycle
        tbe => tbl_ent(rgn_tile_self%itEnt(iiEnt))
        if( tbe%uid > rgn_tile_other%uid(jjEnt) )then
          tbe%uid = rgn_tile_other%uid(jjEnt)
          tbe%is_updated = .true.
        endif
      enddo  ! iRegion/
    enddo  ! jjEnt/
  enddo  ! itx/
  enddo  ! ity/

  nullify(rgn_tile_self)
  nullify(rgn_tile_other)
  nullify(tbl_rgn_tile_self)
  nullify(tbl_rgn_tile_other)
  nullify(tbe)

  call logext()
  !-------------------------------------------------------------
  ! Contruct networks
  !-------------------------------------------------------------
  call logent('Constructing networks')

  iLoop = 0
  is_updated = .true.
  do while( is_updated )
    is_updated = .false.
    call add(iLoop)

    do itEnt = 1, ntEnt
      tbe => tbl_ent(itEnt)
      part => tbe%ent%part(1)

      uid_min = tbe%uid
      do iPt = 1, part%nPoint
        ptc => tbe%point(iPt)
        if( ptc%nPoint_conn == 0 ) cycle

        ! Find smallest global id among connected entities 
        do iiPt = 1, ptc%nPoint_conn
          jtEnt = ptc%itEnt_conn(iiPt)
          uid_min = min(uid_min, tbl_ent(jtEnt)%uid)
        enddo  ! iiPt/
      enddo  ! iPt/

      do iPt = 1, part%nPoint
        ptc => tbe%point(iPt)
        if( ptc%nPoint_conn == 0 ) cycle

        ! Update global ids of connected entities
        do iiPt = 1, ptc%nPoint_conn
          jtEnt = ptc%itEnt_conn(iiPt)
          if( tbl_ent(jtEnt)%uid > uid_min )then
            is_updated = .true.
            tbl_ent(jtEnt)%uid = uid_min
            tbl_ent(jtEnt)%is_updated = .true.
          endif
        enddo  ! iiPt/

      enddo  ! iNode/
    enddo  ! itEnt/
  enddo  ! is_updated/

  ! Mark updated tiles
  do itEnt = 1, ntEnt
    tbe => tbl_ent(itEnt)
    if( tbe%is_updated )then
      txs = nlni_txs_of_lon(tbe%ent%xmin)
      txe = nlni_txe_of_lon(tbe%ent%xmax)
      tys = nlni_tys_of_lat(tbe%ent%ymin)
      tye = nlni_tye_of_lat(tbe%ent%ymax)
      is_tile_updated(txs:txe,tys:tye) = .true.
      is_local_tile_updated = .true.
    endif
  enddo  ! itEnt/

  if( .not. is_local_tile_updated )then
    is_tile_updated(tx,ty) = .false.
  endif

  do ity = NLNI_TYMIN, NLNI_TYMAX
  do itx = NLNI_TXMIN, NLNI_TXMAX
    if( .not. is_tile_exist(itx,ity) )then
      is_tile_updated(itx,ity) = .false.
    endif
  enddo  ! itx/
  enddo  ! ity/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_local_tile_updated )then
    call logmsg('Global id was updated in any tile.')
    if( is_tile_updated(tx,ty) )then
      call logmsg('Global id was updated in this tile.')
    else
      call logmsg('Global id was NOT updated in this tile.')
    endif
  else
    call logmsg('Global id was NOT updated.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Copying global id from list to tiles')

  do itEnt = 1, ntEnt
    tbe => tbl_ent(itEnt)
    if( .not. tbe%is_updated ) cycle

    txs = nlni_txs_of_lon(tbe%ent%xmin)
    txe = nlni_txe_of_lon(tbe%ent%xmax)
    tys = nlni_tys_of_lat(tbe%ent%ymin)
    tye = nlni_tye_of_lat(tbe%ent%ymax)
    do ity = tys, tye
    do itx = txs, txe
      rgn_tile_other => tbl_rgn_tile(itx,ity)%region(tbe%iRegion)
      call search(tbe%irEnt, rgn_tile_other%irEnt, iiEnt)
      if( iiEnt == 0 )then
        call errend('tbe%irEnt not found.')
      endif
      rgn_tile_other%uid(iiEnt) = tbe%uid
    enddo  ! itx/
    enddo  ! ity/
  enddo

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(dcp%lon)
  deallocate(dcp%lat)
  deallocate(dcp%itEnt)
  deallocate(dcp%iPt)

  nullify(part)
  nullify(ptc)
  nullify(tbe)
  deallocate(tbl_ent)

  nullify(rgn_tile)
  nullify(rgn_tile_self)
  nullify(rgn_tile_other)
  nullify(tbl_rgn_tile_self)
  nullify(tbl_rgn_tile_other)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine connectChannels_core
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
subroutine init_dct_point(dcp)
  implicit none
  type(dct_point_), intent(out) :: dcp

  dcp%n = 0
  nullify(dcp%lon)
  nullify(dcp%lat)
  nullify(dcp%itEnt)
  nullify(dcp%jEnt)
  nullify(dcp%jPt)
  nullify(dcp%kPt)
  nullify(dcp%iPt)
  nullify(dcp%nodetyp)
  nullify(dcp%nodetyp_new)
end subroutine init_dct_point
!===============================================================
!
!===============================================================
subroutine free_dct_point(dcp)
  implicit none
  type(dct_point_), intent(inout) :: dcp

  if( dcp%n == 0 ) return
  call realloc(dcp%lon, 0)
  call realloc(dcp%lat, 0)
  call realloc(dcp%itEnt, 0)
  call realloc(dcp%jEnt, 0)
  call realloc(dcp%jPt, 0)
  call realloc(dcp%kPt, 0)
  call realloc(dcp%iPt, 0)
  call realloc(dcp%nodetyp, 0)
  call realloc(dcp%nodetyp_new, 0)
end subroutine free_dct_point
!===============================================================
!
!===============================================================
end module mod_connect_channels
