module mod_mesh
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
  public :: rasterizeNetworks
  public :: make1secNetworkMask
  public :: make1secNetworkUpperArea
  public :: scaleUpNetworkMask
  public :: trimBasin
  !-------------------------------------------------------------
  ! Private module variables (type)
  !-------------------------------------------------------------
  type, extends(cmn_node_) :: node_
    logical :: is_outlet
  end type

  type, extends(cmn_channel_) :: channel_
    type(node_), pointer :: node(:)
    real(8) :: west, east, south, north
  end type

  type, extends(cmn_watsys_) :: watsys_
  end type

  type, extends(cmn_network_) :: network_
    type(watsys_), pointer :: wsys(:)
    type(channel_), pointer :: channel(:)
    real(8) :: west, east, south, north
    integer :: gxs, gxe, gys, gye
    integer, pointer :: jCh(:)
  end type

  type chpix_
    integer :: n
    integer, pointer :: gx(:), gy(:)
    real(8), pointer :: leng(:)
    integer :: gxs, gxe, gys, gye
  end type

  type nwkattr_
    character(DGT_NWKUID) :: uid
    integer :: nCh
    real(8) :: leng
    real(8) :: west, east, south, north
    type(chpix_) :: chpix
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_mesh'

  integer(1), parameter :: STAT_NWKPIX__ISCT        = 2_1
  integer(1), parameter :: STAT_NWKPIX__REACH       = 1_1
  integer(1), parameter :: STAT_NWKPIX__ISCT_OTHER  = -1_1
  integer(1), parameter :: STAT_NWKPIX__REACH_OTHER = -2_1
  integer(1), parameter :: STAT_NWKPIX__OUT         = -3_1
  integer(1), parameter :: STAT_NWKPIX__OCEAN       = -9_1
  integer(1), parameter :: STAT_NWKPIX__UNKNOWN     = -99_1
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
recursive subroutine rasterizeNetworks(&
    uid_in, jNwk_self, nNwk_in, un_lst &
)
  use c1_util, only: &
    clear_cmn_network
  use c1_io, only: &
    read_network
  use c2_jflw_const, &
    jflw_set_resolution => set_resolution
  use c2_jflw_grid, only: &
    west_of_gx , &
    east_of_gx , &
    south_of_gy, &
    north_of_gy, &
    get_nextxy, &
    calc_lineleng_in_pixels
  use c2_jflw_io, only: &
    read_map_from_tile
  use c2_strnk_io, only: &
    get_f_lst_networks_channel, &
    get_f_network_channel, &
    get_f_lst_networks_raster, &
    get_f_network_raster
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'rasterizeNetworks'
  character(*), intent(in) :: uid_in
  integer, intent(in), optional :: jNwk_self
  integer, intent(in), optional :: nNwk_in
  integer, intent(in), optional :: un_lst

  type(cmn_network_) :: cmnnwk
  type(network_) :: nwk
  type(channel_), pointer :: ch
  character(DGT_NWKUID) :: uid
  integer :: nNwk, jNwk
  integer :: iiCh
  integer :: jPt
  integer :: gxs, gxe, gys, gye

  integer :: nPix, mPix
  integer, pointer :: lst_gx(:), lst_gy(:)
  real(8), pointer :: lst_leng(:)
  integer, pointer :: tmplst_gx(:), tmplst_gy(:)
  real(8), pointer :: tmplst_leng(:)
  integer, allocatable :: arg(:)
  integer :: is, ie, iis, iie

  character(CLEN_PATH) :: f, fout
  integer :: un, unout
  character :: c_

  integer(1), parameter :: STAT_NWKPIX__NO    = 0
  integer(1), parameter :: STAT_NWKPIX__ISCT  = 1
  integer(1), parameter :: STAT_NWKPIX__REACH = 2

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uid_in == '' )then
    call jflw_set_resolution(RESOLUTION_1SEC)

    f = get_f_lst_networks_channel()
    open(newunit=un, file=f, status='old')
    read(un,*) c_, nNwk

    fout = get_f_lst_networks_raster()
    open(newunit=unout, file=fout, status='replace')
    write(unout,"(a)") 'networks '//str(nNwk)
    write(unout,"(a)") 'i uid gxs gxe gys gye'

    read(un,*)
    do jNwk = 1, nNwk
      read(un,*) c_, uid
      call logmsg('uid: '//str(uid))
      call rasterizeNetworks(uid, jNwk, nNwk, unout)
    enddo  ! i/

    close(unout)

    close(un)

    call logret(PRCNAM, MODNAM)
    return
  !-------------------------------------------------------------
  ! Case: Test for a single network
  elseif( uid_in /= '' .and. .not. present(un_lst) )then
    call jflw_set_resolution(RESOLUTION_1SEC)
  endif
  !-------------------------------------------------------------
  ! Read network data
  !-------------------------------------------------------------
  call logent('Reading network data')

  allocate(character(1) :: cmnnwk%uid)
  cmnnwk%uid = uid_in

  f = get_f_network_channel(uid_in, 'sbin')
  call read_network(f, cmnnwk)

  call copy_cmn2nwk(cmnnwk, nwk)

  call clear_cmn_network(cmnnwk)

  call logext()
  !-------------------------------------------------------------
  ! Calc. intersection with mesh
  !-------------------------------------------------------------
  call logent('Calculating intersection with mesh')

  nullify(tmplst_gx, tmplst_gy, tmplst_leng)

  allocate(lst_gx(1024))
  allocate(lst_gy(1024))
  allocate(lst_leng(1024))

  nPix = 0
  do iiCh = 1, nwk%nCh
    ch => nwk%channel(iiCh)
    do jPt = 2, ch%n
      call calc_lineleng_in_pixels(&
        ch%lon(jPt-1), ch%lat(jPt-1), & ! in
        ch%lon(jPt)  , ch%lat(jPt)  , & ! in
        mPix, tmplst_gx, tmplst_gy, tmplst_leng) ! out

      if( nPix+mPix > size(lst_gx) )then
        call realloc(lst_gx, (nPix+mPix)*2, clear=.false.)
        call realloc(lst_gy, (nPix+mPix)*2, clear=.false.)
        call realloc(lst_leng, (nPix+mPix)*2, clear=.false.)
      endif
      lst_gx(nPix+1:nPix+mPix) = tmplst_gx(:)
      lst_gy(nPix+1:nPix+mPix) = tmplst_gy(:)
      lst_leng(nPix+1:nPix+mPix) = tmplst_leng(:)
      call add(nPix, mPix)

      deallocate(tmplst_gx, tmplst_gy, tmplst_leng)
    enddo  ! jPt/
  enddo  ! iiCh/

  call realloc(lst_gx, nPix, clear=.false.)
  call realloc(lst_gy, nPix, clear=.false.)
  call realloc(lst_leng, nPix, clear=.false.)

  call logmsg('Total number of intersecting pixels: '//str(nPix))

  call logext()
  !-------------------------------------------------------------
  ! Sort lists and integrate duplicated elements
  !-------------------------------------------------------------
  call logent('Sorting lists and integrating duplicated elements')

  allocate(arg(nPix))
  call argsort(lst_gy, arg)
  call sort(lst_gy, arg)
  call sort(lst_gx, arg)
  call sort(lst_leng, arg)

  nPix = 0
  ie = 0
  do while( ie < size(arg) )
    is = ie + 1
    ie = is
    do while( ie < size(arg) )
      if( lst_gy(ie+1) /= lst_gy(is) ) exit
      ie = ie + 1
    enddo
    call sort(lst_gx(is:ie))
    call sort(lst_leng(is:ie))
    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( lst_gx(iie+1) /= lst_gx(iis) ) exit
        iie = iie + 1
      enddo
      nPix = nPix + 1
      lst_gx(nPix) = lst_gx(iis)
      lst_gy(nPix) = lst_gy(iis)
      lst_leng(nPix) = sum(lst_leng(iis:iie))
    enddo
  enddo

  call logmsg('Number of intersecting pixels: '//str(nPix))

  deallocate(arg)

  call realloc(lst_gx, nPix, clear=.false.)
  call realloc(lst_gy, nPix, clear=.false.)
  call realloc(lst_leng, nPix, clear=.false.)

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f = get_f_network_raster(nwk%uid)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, form='unformatted', access='sequential', status='replace')
  write(un) nPix
  write(un) lst_gx(:)
  write(un) lst_gy(:)
  write(un) lst_leng(:)
  close(un)

  gxs = minval(lst_gx)
  gxe = maxval(lst_gx)
  gys = minval(lst_gy)
  gye = maxval(lst_gy)
  write(un_lst,"(a)") &
    str(jNwk_self,dgt(nNwk_in))//' '//nwk%uid//' '//str((/gxs,gxe,gys,gye/),DGT_GXY)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(lst_gx)
  deallocate(lst_gy)
  deallocate(lst_leng)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine rasterizeNetworks
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
subroutine make1secNetworkMask(uid)
  use c2_jflw_const, only: &
    set_resolution, &
    DGT_GXY
  use c2_jflw_grid, only: &
    west_of_gx , &
    east_of_gx , &
    south_of_gy, &
    north_of_gy
  use c2_strnk_io, only: &
    get_f_lst_networks_channel, &
    get_f_lst_networks_raster , &
    get_f_lst_networks_mesh
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'make1secNetworkMask'
  character(*), intent(in) :: uid

  type(nwkattr_), pointer :: lst_nwkattr(:), nwkattr
  type(chpix_), pointer :: chpix
  integer :: gxs, gxe, gys, gye
  integer :: n, i

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
  read(un,*) c_, n
  allocate(lst_nwkattr(n))

  read(un,*)
  do i = 1, n
    nwkattr => lst_nwkattr(i)
    read(un,*) c_, nwkattr%uid, &
               nwkattr%nCh, nwkattr%leng, &
               nwkattr%west, nwkattr%east, nwkattr%south, nwkattr%north
  enddo
  close(un)

  f = get_f_lst_networks_raster()
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) c_, n

  read(un,*)
  do i = 1, n
    nwkattr => lst_nwkattr(i)
    chpix => nwkattr%chpix
    read(un,*) c_, c_, &
               chpix%gxs, chpix%gxe, chpix%gys, chpix%gye
  enddo  ! i/
  close(un)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uid == 'all' )then
    f = get_f_lst_networks_mesh(RESOLUTION_1SEC)
    call logmsg('Writing '//str(f))
    open(newunit=un, file=f, status='replace')
    write(un,"(a)") 'networks '//str(n)
    write(un,"(a)") 'i uid gxs gxe gys gye west east south north'

    do i = 1, n
      call make_1sec_network_mask(&
          lst_nwkattr, i, &
          gxs, gxe, gys, gye)

      write(un,"(a)") &
          str(i,dgt(n))//' '//str(lst_nwkattr(i)%uid)//' '//&
          str((/gxs,gxe,gys,gye/),DGT_GXY)//' '//&
          sBBox(west_of_gx(gxs),east_of_gx(gxe),south_of_gy(gye),north_of_gy(gys),&
                d=' ', b='')
    enddo  ! i/

    close(un)
    call logmsg('Saved '//str(f))
  else
    call logmsg('====== DEBUG MODE ======')

    do i = 1, n
      if( lst_nwkattr(i)%uid == uid )then
        call make_1sec_network_mask(&
            lst_nwkattr, i, &
            gxs, gxe, gys, gye)
        exit
      endif
    enddo  ! i/
  endif
  !-------------------------------------------------------------
  deallocate(lst_nwkattr)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make1secNetworkMask
!===============================================================
!
!===============================================================
subroutine make_1sec_network_mask(&
    lst_nwkattr, jNwk_self, &
    gxs, gxe, gys, gye)
  use c2_jflw_const
  use c2_jflw_grid, only: &
    west_of_gx , &
    east_of_gx , &
    south_of_gy, &
    north_of_gy, &
    get_nextxy
  use c2_jflw_io, only: &
    read_map_from_tile
  use c2_strnk_io, only: &
    get_f_network_channel, &
    get_f_network_raster , &
    get_f_network_mesh, &
    write_network_mesh_domain
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'make_1sec_network_mask'
  type(nwkattr_), intent(inout), target :: lst_nwkattr(:)
  integer, intent(in) :: jNwk_self
  integer, intent(out) :: gxs, gxe, gys, gye

  type(nwkattr_), pointer :: nwkattr, nwkattr_self
  type(chpix_), pointer :: chpix, chpix_self
  integer :: nNwk, jNwk

  integer :: iPix
  integer :: nEdgePix
  integer, pointer :: lst_gx(:), lst_gy(:)

  integer :: gxs_next, gxe_next, gys_next, gye_next
  integer :: gxs_outer, gxe_outer, gys_outer, gye_outer
  integer :: igx, igy
  integer :: gxy_ext
  integer(1), pointer :: fdrmap(:,:)
  integer(1), pointer :: nwkmap(:,:)
  logical :: is_ok

  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nNwk = size(lst_nwkattr)

  nwkattr_self => lst_nwkattr(jNwk_self)
  chpix_self => nwkattr_self%chpix
  call read_nwkrst(nwkattr_self%uid, chpix_self)

  call logmsg('('//str(jNwk_self,dgt(nNwk))//') '//&
      str(nwkattr_self%uid)//' pixels: '//str(chpix_self%n))
  !-------------------------------------------------------------
  ! Init. nwkmap
  !-------------------------------------------------------------
  call logent('Initializing network mesh')

  gxs = chpix_self%gxs
  gxe = chpix_self%gxe
  gys = chpix_self%gys
  gye = chpix_self%gye

  gxy_ext = max(max(gxe-gxs+1, gye-gys+1) / 20, 10)
  gxs_next = max(gxs - gxy_ext, 1)
  gxe_next = min(gxe + gxy_ext, NGX)
  gys_next = max(gys - gxy_ext, 1)
  gye_next = min(gye + gxy_ext, NGY)

  gxs_outer = max(gxs_next - 1, 1)
  gxe_outer = min(gxe_next + 1, NGX)
  gys_outer = max(gys_next - 1, 1)
  gye_outer = min(gye_next + 1, NGY)

  allocate(nwkmap(gxs_outer:gxe_outer,gys_outer:gye_outer))
  nwkmap(:,:) = STAT_NWKPIX__UNKNOWN

  allocate(fdrmap(gxs_outer:gxe_outer,gys_outer:gye_outer))
  call read_map_from_tile(&
         RESOLUTION_1SEC, 'dir', DTYPE_INT1, FDR_MISS, &
         gxs_outer, gys_outer, &
         fdrmap)

  where( fdrmap == FDR_MISS )
    nwkmap = STAT_NWKPIX__OCEAN
  endwhere

  call logext()
  !-------------------------------------------------------------
  ! Reflect intersections of channels
  !-------------------------------------------------------------
  call logent('Reflecting intersections of channels')

  do jNwk = 1, nNwk
    if( jNwk == jNwk_self ) cycle
    nwkattr => lst_nwkattr(jNwk)
    chpix => nwkattr%chpix

    if( chpix%gxe < gxs_outer .or. gxe_outer < chpix%gxs .or. &
        chpix%gye < gys_outer .or. gye_outer < chpix%gys ) cycle

    !call logmsg('nwk '//str(nwkattr%uid)//&
    !    ' ('//str((/nwkattr%west,nwkattr%east/),'f12.7',',')//&
    !     ','//str((/nwkattr%south,nwkattr%north/),'f11.7',',')//')'//&
    !     ' nCh: '//str(nwkattr%nCh))

    if( chpix%n == 0 )then
      call read_nwkrst(nwkattr%uid, chpix)
    endif

    do iPix = 1, chpix%n
      if( chpix%gx(iPix) < gxs_outer .or. gxe_outer < chpix%gx(iPix) .or. &
          chpix%gy(iPix) < gys_outer .or. gye_outer < chpix%gy(iPix) ) cycle
      nwkmap(chpix%gx(iPix),chpix%gy(iPix)) = STAT_NWKPIX__ISCT_OTHER
    enddo  ! iPix/
  enddo  ! jNwk/

  do iPix = 1, chpix_self%n
    nwkmap(chpix_self%gx(iPix),chpix_self%gy(iPix)) = STAT_NWKPIX__ISCT
  enddo  ! iPix/

  call logext()
  !-------------------------------------------------------------
  ! Get domain that includes nwk mesh
  !-------------------------------------------------------------
  call logent('Getting the domain that includes the network mesh')

  is_ok = .false.
  do while( .not. is_ok )
    !gxs = 0
    !gxe = 0
    !gys = 0
    !gye = 0
    do while( gxs_next /= gxs .or. gxe_next /= gxe .or. &
              gys_next /= gys .or. gye_next /= gye )
      gxs = gxs_next
      gxe = gxe_next
      gys = gys_next
      gye = gye_next
      call logmsg('['//str((/gxs,gxe/),DGT_GXY,':')//','//str((/gys,gye/),DGT_GXY,':')//']')

      gxs_outer = gxs - 1
      gxe_outer = gxe + 1
      gys_outer = gys - 1
      gye_outer = gye + 1

      if( size(fdrmap,1) /= gxe_outer - gxs_outer + 1 .or. &
          size(fdrmap,2) /= gye_outer - gys_outer + 1 )then
        call realloc(fdrmap, (/gxs_outer,gys_outer/), (/gxe_outer,gye_outer/), clear=.true.)
        call read_map_from_tile(&
               RESOLUTION_1SEC, 'dir', DTYPE_INT1, FDR_MISS, &
               gxs_outer, gys_outer, &
               fdrmap)

        call realloc(nwkmap, (/gxs_outer,gys_outer/), (/gxe_outer,gye_outer/), &
                     clear=.false., fill=STAT_NWKPIX__UNKNOWN)
      endif

      ! upper left
      if( reached_nwk(gxs, gys) )then
        gxs_next = gxs - gxy_ext
        gys_next = gys - gxy_ext
      endif
      ! lower right
      if( reached_nwk(gxe, gye) )then
        gxe_next = gxe + gxy_ext
        gye_next = gye + gxy_ext
      endif
      ! lower left
      if( gxs_next == gxs .or. gye_next == gye )then
        if( reached_nwk(gxs, gye) )then
          gxs_next = gxs - gxy_ext
          gye_next = gye + gxy_ext
        endif
      endif
      ! upper right
      if( gxe_next == gxe .or. gys_next == gys )then
        if( reached_nwk(gxe, gys) )then
          gxe_next = gxe + gxy_ext
          gys_next = gys - gxy_ext
        endif
      endif
      ! upper side
      if( gys_next == gys )then
        do igx = gxs, gxe
          if( reached_nwk(igx, gys) )then
            gys_next = gys - gxy_ext
            exit
          endif
        enddo  ! igx/
      endif
      ! lower side
      if( gye_next == gye )then
        do igx = gxs, gxe
          if( reached_nwk(igx, gye) )then
            gye_next = gye + gxy_ext
            exit
          endif
        enddo  ! igx/
      endif
      ! left side
      if( gxs_next == gxs )then
        do igy = gys+1, gye-1
          if( reached_nwk(gxs, igy) )then
            gxs_next = gxs - gxy_ext
            exit
          endif
        enddo  ! igy/
      endif
      ! right side
      if( gxe_next == gxe )then
        do igy = gys+1, gye-1
          if( reached_nwk(gxe, igy) )then
            gxe_next = gxe + gxy_ext
            exit
          endif
        enddo  ! igy/
      endif
    enddo  ! while gxs_next /= gxs .or. gxe_next /= gxe .or. &
           !       gys_next /= gys .or. gye_next /= gye 

    is_ok = .true.
    loop_outer_horizontal:&
    do igy = gys_outer, gye_outer, gye_outer-gys_outer+1
    do igx = gxs_outer, gxe_outer
      if( reached_nwk(igx, igy) )then
        is_ok = .false.
        exit loop_outer_horizontal
      endif
    enddo  ! igx/
    enddo &! igy/
    loop_outer_horizontal
    loop_outer_vertical:&
    do igy = gys_outer+1, gye_outer-1
    do igx = gxs_outer, gxe_outer, gxe_outer-gxs_outer+1
      if( reached_nwk(igx, igy) )then
        is_ok = .false.
        exit loop_outer_vertical
      endif
    enddo  ! igx/
    enddo &! igy/
    loop_outer_vertical

    gxs_next = max(gxs_next - gxy_ext, 1)
    gxe_next = min(gxe_next + gxy_ext, NGX)
    gys_next = max(gys_next - gxy_ext, 1)
    gye_next = min(gye_next + gxy_ext, NGY)
  enddo  ! while .not. is_ok

  !call logmsg('('//str((/gxe_outer-gxs_outer+1,gye_outer-gys_outer+1/),&
  !            dgt(maxval(shape(nwkmap))),',')//') '//&
  !            '['//str((/gxs_outer,gxe_outer/),DGT_GXY,':')//&
  !            ','//str((/gys_outer,gye_outer/),DGT_GXY,':')//']')
  !call logmsg(sBBox(west_of_gx(gxs_outer),east_of_gx(gxe_outer),&
  !            south_of_gy(gye_outer),north_of_gy(gys_outer)))
  !call traperr( wbin(nwkmap, 'tmp/nwkmap.bin', replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  ! Fill the map with the valid status
  !-------------------------------------------------------------
  call logent('Filling the map with the valid status')

  do igy = gys, gye
  do igx = gxs, gxe
    if( fdrmap(igx,igy) <= 0_1 )then
      nwkmap(igx,igy) = STAT_NWKPIX__OCEAN
    elseif( nwkmap(igx,igy) == STAT_NWKPIX__UNKNOWN )then
      is_ok = reached_nwk(igx, igy)
    endif
  enddo  ! igx/
  enddo  ! igy/

  !print*, gxe-gxs+1, gye-gys+1
  !call traperr( wbin(nwkmap(gxs:gxe,gys:gye), 'tmp/nwkmap.bin', replace=.true.) )

  if( any(nwkmap(gxs:gxe,gys:gye) == STAT_NWKPIX__UNKNOWN) )then
    call errend(msg_unexpected_condition()//&
        '\n  any(nwkmap == UNKNOWN)')
  endif

  call logext()
  !-------------------------------------------------------------
  ! Get the outer edge
  !-------------------------------------------------------------
  call logent('Getting the outer edge of network mesh')

  nEdgePix = max(gxe_outer-gxs_outer+1, gye_outer-gys_outer+1) * 4
  allocate(lst_gx(nEdgePix))
  allocate(lst_gy(nEdgePix))
  !print*, gxs_outer, gxe_outer, gys_outer, gye_outer

  nEdgePix = 0
  do igy = gys_outer+1, gye_outer-1
  do igx = gxs_outer+1, gxe_outer-1
    if( nwkmap(igx,igy) <= 0_1 ) cycle

    if( all(nwkmap(igx-1:igx+1,igy-1:igy+1) > 0_1) ) cycle

    if( nEdgePix == size(lst_gx) )then
      call realloc(lst_gx, nEdgePix*2, clear=.false.)
      call realloc(lst_gy, nEdgePix*2, clear=.false.)
    endif
    call add(nEdgePix)
    lst_gx(nEdgePix) = igx
    lst_gy(nEdgePix) = igy
  enddo  ! igx/
  enddo  ! igy/

  call realloc(lst_gx, nEdgePix, clear=.false.)
  call realloc(lst_gy, nEdgePix, clear=.false.)
  call logmsg('Number of edge pixels: '//str(nEdgePix))

  !call traperr( wbin(lst_gx, 'tmp/edge_x.bin', replace=.true.) )
  !call traperr( wbin(lst_gy, 'tmp/edge_y.bin', replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  call logmsg('('//str((/gxe-gxs+1,gye-gys+1/),dgt(maxval(shape(nwkmap))),', ')//') '//&
              '['//str((/gxs,gxe/),DGT_GXY,':')//&
              ','//str((/gys,gye/),DGT_GXY,':')//']')
  call logmsg(sBBox(west_of_gx(gxs),east_of_gx(gxe),south_of_gy(gye),north_of_gy(gys)))

  call write_network_mesh_domain(&
         RESOLUTION_1SEC, nwkattr_self%uid, &
         gxs, gxe, gys, gye, &
         west_of_gx(gxs), east_of_gx(gxe), south_of_gy(gye), north_of_gy(gys) &
  )

  f = get_f_network_mesh(RESOLUTION_1SEC, 'mask', nwkattr_self%uid)
  call logmsg('Writing '//str(f))
  call traperr( wbin(nwkmap(gxs:gxe,gys:gye), f, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine read_nwkrst(uid, chpix)
  implicit none
  character(*), intent(in) :: uid
  type(chpix_), intent(inout) :: chpix

  character(CLEN_PATH) :: f
  integer :: un

  f = get_f_network_raster(uid)
  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')
  read(un) chpix%n
  allocate(chpix%gx(chpix%n))
  allocate(chpix%gy(chpix%n))
  allocate(chpix%leng(chpix%n))
  read(un) chpix%gx
  read(un) chpix%gy
  read(un) chpix%leng
  close(un)
end subroutine read_nwkrst
!---------------------------------------------------------------
!
!---------------------------------------------------------------
logical function reached_nwk(gx, gy) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'IP__reached_nwk'
  integer, intent(in) :: gx, gy

  integer :: igx, igy, gxx, gyy
  integer :: stat

  res = .false.
  stat = 2

  igx = gx
  igy = gy
  do while( fdrmap(igx,igy) > 0_1 )
    call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)
    igx = gxx
    igy = gyy

    if( gxx < gxs_outer .or. gxx > gxe_outer .or. &
        gyy < gys_outer .or. gyy > gye_outer )then
      res = .false.
      stat = 2
      exit
    else
      selectcase( nwkmap(gxx,gyy) )
      case( STAT_NWKPIX__ISCT, &
            STAT_NWKPIX__REACH )
        res = .true.
        stat = 0
        exit
      case( STAT_NWKPIX__UNKNOWN )
        continue
      case( STAT_NWKPIX__ISCT_OTHER, &
            STAT_NWKPIX__REACH_OTHER )
        res = .false.
        stat = 1
        exit
      endselect
    endif
  enddo  ! while fdrmap > 0

  selectcase( stat )
  !-------------------------------------------------------------
  ! Case: Reaches this network
  case( 0 )
    igx = gx
    igy = gy
    do while( nwkmap(igx,igy) == STAT_NWKPIX__UNKNOWN )
      nwkmap(igx,igy) = STAT_NWKPIX__REACH
      call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)
      igx = gxx
      igy = gyy
    enddo  ! while fdrmap > 0
  !-------------------------------------------------------------
  ! Case: Reaches other network
  case( 1  )
    igx = gx
    igy = gy
    do while( nwkmap(igx,igy) == STAT_NWKPIX__UNKNOWN )
      nwkmap(igx,igy) = STAT_NWKPIX__REACH_OTHER
      call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)
      igx = gxx
      igy = gyy
    enddo  ! while fdrmap > 0
  !-------------------------------------------------------------
  ! Case: Go out of domain
  case( 2 )
    igx = gx
    igy = gy
    do while( fdrmap(igx,igy) /= FDR_MISS )
      nwkmap(igx,igy) = STAT_NWKPIX__OUT
      call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)
      igx = gxx
      igy = gyy
      if( gxx < gxs_outer .or. gxe_outer < gxx .or. &
          gyy < gys_outer .or. gye_outer < gyy ) exit
    enddo  ! while fdrmap > 0
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('stat', stat), &
        '', PRCNAM, MODNAM)
  endselect
end function reached_nwk
!---------------------------------------------------------------
end subroutine make_1sec_network_mask
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
recursive subroutine make1secNetworkUpperArea(uid)
  use c1_io, only: &
    read_network
  use c2_strnk_io, only: &
    get_f_lst_networks_channel, &
    get_f_network_mesh        , &
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
  character(CLEN_PROC), parameter :: PRCNAM = 'make1secNetworkUpperArea'
  character(*), intent(in) :: uid

  type(network_) :: nwk
  character(DGT_NWKUID) :: uid_this
  integer :: nNwk, jNwk
  integer(1), allocatable :: mskmap(:,:)
  integer(1), allocatable :: fdrmap(:,:)
  real(8)   , allocatable :: upamap(:,:)
  real(8)   , allocatable :: addmap(:,:)
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixlen(:)
  integer :: gxs, gxe, gys, gye
  integer :: igx, igy, gxx, gyy
  real(8) :: west, east, south, north
  logical :: is_updated
  real(8) :: bsnara, upa_sum
  real(8) :: err_norm

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  real(8), allocatable, save :: pixara(:)
  logical, save :: is_raster_resolution_defined = .false.

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
      call make1secNetworkUpperArea(uid_this)
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
  ! Read domain info.
  !-------------------------------------------------------------
  call logmsg('Network '//str(uid))
  nwk%uid = uid

  call read_network_mesh_domain(&
    RESOLUTION_1SEC, nwk%uid, &
    gxs, gxe, gys, gye, &
    west, east, south, north &
  )

  allocate(mskmap(gxs-1:gxe+1,gys-1:gye+1))
  allocate(upamap(gxs-1:gxe+1,gys-1:gye+1))

  allocate(fdrmap(gxs-1:gxe+1,gys-1:gye+1))
  allocate(addmap(gxs-1:gxe+1,gys-1:gye+1))
  !-------------------------------------------------------------
  ! Read maps
  !-------------------------------------------------------------
  call logent('Reading topography data')

  f = get_f_network_mesh(RESOLUTION_1SEC, 'mask', nwk%uid)
  mskmap(:,:) = 0_1
  call traperr( rbin(mskmap(gxs:gxe,gys:gye), f) )

  call read_map_from_tile(&
    RESOLUTION_1SEC, 'dir', DTYPE_INT1, FDR_MISS, &
    gxs, gys, &
    fdrmap(gxs:gxe,gys:gye) &
  )

  call logext()
  !-------------------------------------------------------------
  ! Calc. upper area
  !-------------------------------------------------------------
  call logent('Calculating upper area')

  addmap(:,:) = 0.d0
  do igy = gys, gye
  do igx = gxs, gxe
    if( mskmap(igx,igy) < 0_1 ) cycle
    addmap(igx,igy) = pixara(igy)
  enddo
  enddo
  bsnara = sum(addmap)

  upamap(:,:) = 0.d0
  is_updated = .true.
  do while( is_updated )
    is_updated = .false.
    do igy = gys, gye
    do igx = gxs, gxe
      if( mskmap(igx,igy) < 0_1 ) cycle

      if( addmap(igx,igy) == 0.d0 ) cycle

      call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)

      call add(upamap(igx,igy), addmap(igx,igy))

      if( gxx > 0 )then
        call add(addmap(gxx,gyy), addmap(igx,igy))
      endif

      addmap(igx,igy) = 0.d0

      is_updated = .true.
    enddo  ! igx/
    enddo  ! igy/
  enddo  ! while is_updated/

  ! Validate results
  upa_sum = 0.d0
  do igy = gys, gye
  do igx = gxs, gxe
    if( mskmap(igx,igy) < 0_1 ) cycle
    call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)
    if( gxx < 0 ) cycle
    if( mskmap(gxx,gyy) < 0_1 )then
      call add(upa_sum, upamap(igx,igy))
    endif
  enddo  ! igx/
  enddo  ! igy/

  err_norm = (upa_sum - bsnara) / bsnara
  if( err_norm > 1d-8 )then
    call errend('basin area : '//str(bsnara)//&
              '\nsum. of upa: '//str(upa_sum)//&
              '\nnormalized error: '//str(err_norm))
  endif
  !-------------------------------------------------------------
  deallocate(fdrmap)
  deallocate(addmap)
  deallocate(mskmap)

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f = get_f_network_mesh(RESOLUTION_1SEC, 'upa', nwk%uid)
  call logmsg('Writing '//str(f))
  call traperr( wbin(upamap(gxs:gxe,gys:gye), f, dtype=DTYPE_REAL, replace=.true.) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(upamap)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make1secNetworkUpperArea
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
subroutine scaleUpNetworkMask(resl)
  use c1_grid, only: &
    get_cellsize_in_sec
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c2_jflw_grid, only: &
    west_of_gx , &
    east_of_gx , &
    south_of_gy, &
    north_of_gy
  use c2_strnk_io, only: &
    get_f_lst_networks_mesh
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'scaleUpNetworkMask'
  character(*), intent(in) :: resl

  integer :: ratio
  integer :: nNwk, jNwk
  character :: c_
  character(DGT_NWKUID) :: uid
  integer :: gxs_in, gxe_in, gys_in, gye_in
  real(8) :: west_in, east_in, south_in, north_in
  integer :: gxs, gxe, gys, gye

  character(CLEN_PATH) :: f_in, f
  integer :: un_in, un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(resl)
  ratio = get_cellsize_in_sec(resl)
  call logmsg('Ratio to 1sec: '//str(ratio))

  f = get_f_lst_networks_mesh(resl)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(a)") 'networks '//str(nNwk)
  write(un,"(a)") 'i uid gxs gxe gys gye west east south north'

  f_in = get_f_lst_networks_mesh(RESOLUTION_1SEC)
  open(newunit=un_in, file=f_in, status='old')
  read(un_in,*) c_, nNwk
  read(un_in,*)

  do jNwk = 1, nNwk
    read(un_in,*) &
      c_, uid, &
      gxs_in, gxe_in, gys_in, gye_in, &
      west_in, east_in, south_in, north_in

    call logmsg('('//str(jNwk)//') '//str(uid))

    call scale_up_network_mask(&
      resl, ratio, uid, gxs_in, gxe_in, gys_in, gye_in, &
      gxs, gxe, gys, gye)

    write(un,"(a)") &
      str(jNwk,dgt(nNwk))//' '//str(uid)//' '//&
      str((/gxs,gxe,gys,gye/),DGT_GXY)//' '//&
      sBBox(west_of_gx(gxs),east_of_gx(gxe),south_of_gy(gye),north_of_gy(gys),&
            d=' ', b='')
  enddo  ! jNwk/

  close(un_in)
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine scaleUpNetworkMask
!===============================================================
!
!===============================================================
subroutine scale_up_network_mask(&
    resl, ratio, uid, ghxs, ghxe, ghys, ghye, &
    gxs, gxe, gys, gye)
  use c2_jflw_grid, only: &
    west_of_gx, &
    east_of_gx, &
    south_of_gy, &
    north_of_gy
  use c2_strnk_io, only: &
    get_f_network_mesh, &
    write_network_mesh_domain
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'scale_up_network_mask'
  character(*), intent(in) :: resl
  integer, intent(in) :: ratio
  character(*), intent(in) :: uid
  integer, intent(in) :: ghxs, ghxe, ghys, ghye
  integer, intent(out) :: gxs, gxe, gys, gye

  integer(1), allocatable :: nwkmap(:,:)
  integer(1), allocatable :: nwkmap_in(:,:)
  integer :: igx, igy
  integer :: ghxs_this, ghxe_this, ghys_this, ghye_this
  character(CLEN_PATH) :: f_in, f

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  gxs = (ghxs-1) / ratio + 1
  gxe = (ghxe-1) / ratio + 1
  gys = (ghys-1) / ratio + 1
  gye = (ghye-1) / ratio + 1
  call logmsg(str(RESOLUTION_1SEC)//' ['//str((/ghxs,ghxe/),':')//', '//str((/ghys,ghye/),':')//']')
  call logmsg(str(resl)//' ['//str((/gxs,gxe/),':')//', '//str((/gys,gye/),':')//']')

  allocate(nwkmap_in(ghxs:ghxe,ghys:ghye))
  f_in = get_f_network_mesh(RESOLUTION_1SEC, 'mask', uid)
  call traperr( rbin(nwkmap_in, f_in) )

  allocate(nwkmap(gxs:gxe,gys:gye))
  do igy = gys, gye
    call get_domain(igy, ghys_this, ghye_this, ghys, ghye)
    do igx = gxs, gxe
      call get_domain(igx, ghxs_this, ghxe_this, ghxs, ghxe)
      nwkmap(igx,igy) = maxval(nwkmap_in(ghxs_this:ghxe_this,ghys_this:ghye_this))
    enddo  ! igx/
  enddo  ! igy/

  f = get_f_network_mesh(resl, 'domain', uid)
  call write_network_mesh_domain(&
         resl, uid, &
         gxs, gxe, gys, gye, &
         west_of_gx(gxs), east_of_gx(gxe), south_of_gy(gye), north_of_gy(gys) &
  )

  f = get_f_network_mesh(resl, 'mask', uid)
  call traperr( wbin(nwkmap, f, replace=.true.) )

  deallocate(nwkmap)
  deallocate(nwkmap_in)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine get_domain(igx, ghxs_this, ghxe_this, ghxs, ghxe)
  implicit none
  integer, intent(in) :: igx
  integer, intent(out) :: ghxs_this, ghxe_this
  integer, intent(in) :: ghxs, ghxe

  ghxs_this = max((igx-1) * ratio + 1, ghxs)
  ghxe_this = min(igx * ratio, ghxe)
end subroutine get_domain
!---------------------------------------------------------------
end subroutine scale_up_network_mask
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
subroutine makeNetworkSet(uid)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeNetworkSet'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------

  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeNetworkSet
!===============================================================
!
!===============================================================
subroutine make_network_set(uid)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_network_set'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------


  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_network_set
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
subroutine trimBasin(basinType, resl, uid, varName, outfmt)
  use c3_jflw_const, &
        jflw_set_resolution => jflw_set_resolution
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'trimBasin'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: uid
  character(*), intent(in) :: outfmt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call jflw_set_resolution(resl)

  call trim_basin(basinType, resl, uid, varName, outfmt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine trimBasin
!===============================================================
!
!===============================================================
subroutine trim_basin(basinType, resl, uid, varName, outfmt)
  use c3_jflw_const
  use c3_jflw_io, only: &
    jflw_intId                      , &
    jflw_get_f_map_basin            , &
    jflw_read_basin_domain_from_each, &
    jflw_read_map_from_tile         , &
    jflw_read_basin_map_from_tile
  use c3_strnk_io, only: &
    strnk_get_f_network_mesh      , &
    strnk_read_network_mesh_domain
  use c3_rri_io, only: &
    rri_get_f_data, &
    rri_write_map
  use c3_joint_const
  use c3_joint_util, only: &
    joint_conv_fdr_jflw2rri => conv_fdr_jflw2rri, &
    joint_get_miss          => get_miss
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'trim_basin'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid
  character(*), intent(in) :: varName
  character(*), intent(in) :: outfmt

  integer(4), allocatable :: bsnmap(:,:)
  integer(1), allocatable :: nwkmap(:,:)
  integer(4), allocatable :: i4map(:,:)
  real(4)   , allocatable :: r4map(:,:)
  logical(1), allocatable :: mskmap(:,:)
  integer(1) :: i1miss
  integer(4) :: i4miss
  real(4)    :: r4miss
  integer :: gxs, gxe, gys, gye
  real(8) :: west, east, south, north
  integer :: igx, igy
  character(CLEN_VAR) :: inputDataName
  character(CLEN_KEY) :: dtype
  character(CLEN_PATH) :: f_msk, f_var

  character(CLEN_KEY), parameter :: OUTFMT__DEFAULT = 'default'
  character(CLEN_KEY), parameter :: OUTFMT__RRI     = 'rri'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Setup
  !-------------------------------------------------------------
  selectcase( lower(outfmt) )
  case( OUTFMT__DEFAULT )
    inputDataName = DATANAME__JFLW
  case( OUTFMT__RRI )
    inputDataName = DATANAME__RRI
  case default
    call errend(msg_invalid_value('outfmt', outfmt))
  endselect
  !-------------------------------------------------------------
  ! Make a mask
  !-------------------------------------------------------------
  selectcase( lower(basinType) )
  !-------------------------------------------------------------
  ! Case: J-FlwDir basin map
  case( BASINTYPE__BASIN )
    call jflw_read_basin_domain_from_each(&
        resl, uid, &
        gxs, gxe, gys, gye, west, east, south, north)

    call logmsg('Basin '//str(uid))
    call logmsg('(x,y): ('//str((/gxe-gxs+1,gye-gys+1/),JFLW_DGT_GXY,',')//&
        ') ['//str((/gxs,gxe/),JFLW_DGT_GXY,':')//','//&
        str((/gys,gye/),JFLW_DGT_GXY,':')//']')
    call logmsg('BBox: '//sBBox(west,east,south,north))

    allocate(mskmap(gxs:gxe,gys:gye))
    allocate(bsnmap(gxs:gxe,gys:gye))

    f_msk = jflw_get_f_map_basin(resl, 'bsn', uid)
    f_var = jflw_get_f_map_basin(resl, varName, uid)

    call jflw_read_map_from_tile(&
        resl, 'bsn', DTYPE_INT4, JFLW_BSN_MISS, gxs, gys, bsnmap)

    where( bsnmap == jflw_intId(uid) )
      mskmap = .true.
    elsewhere
      mskmap = .false.
    endwhere

    deallocate(bsnmap)
  !-------------------------------------------------------------
  ! Case: Network mesh
  case( BASINTYPE__NETWORK )
    call strnk_read_network_mesh_domain(&
      resl, uid, &
      gxs, gxe, gys, gye, west, east, south, north &
    )

    allocate(mskmap(gxs:gxe,gys:gye))
    allocate(nwkmap(gxs:gxe,gys:gye))

    f_msk = strnk_get_f_network_mesh(resl, 'mask', uid)
    f_var = strnk_get_f_network_mesh(resl, varName, uid)

    call traperr( rbin(nwkmap, f_msk) )

    where( nwkmap > 0_1 )
      mskmap = .true.
    elsewhere
      mskmap = .false.
    endwhere

    deallocate(nwkmap)
  !-------------------------------------------------------------
  ! Case: Network set mesh
  case( BASINTYPE__NETWORKSET )
    call errend(msg_not_implemented()//&
      '\n  basinType: '//str(basinType))
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('basinType', basinType))
  endselect
  !-------------------------------------------------------------
  ! Trim map and output
  !-------------------------------------------------------------
  selectcase( varName )
  !-------------------------------------------------------------
  ! Case: Int1 (dir, landuse)
  case( VARNAME__FDR, VARNAME__LANDUSE )
    dtype = DTYPE_INT1

    call joint_get_miss(inputDataName, varName, i1miss)
    i4miss = int(i1miss, 4)

    allocate(i4map(gxs:gxe,gys:gye))

    call jflw_read_map_from_tile(&
        resl, varName, DTYPE_INT1, i4miss, gxs, gys, i4map)

    where( .not. mskmap ) i4map = i4miss
  !-------------------------------------------------------------
  ! Case: Int4 (upg)
  case( VARNAME__UPG )
    dtype = DTYPE_INT4

    call joint_get_miss(inputDataName, varName, i4miss)

    allocate(i4map(gxs:gxe,gys:gye))

    call jflw_read_map_from_tile(&
        resl, varName, DTYPE_INT4, i4miss, gxs, gys, i4map)

    where( .not. mskmap ) i4map = i4miss
  !-------------------------------------------------------------
  ! Case: Real (elv, upa, wth)
  case( VARNAME__ELV, VARNAME__UPA, VARNAME__WTH )
    dtype = DTYPE_REAL

    call joint_get_miss(inputDataName, varName, r4miss)

    allocate(r4map(gxs:gxe,gys:gye))

    call jflw_read_map_from_tile(&
      resl, varName, DTYPE_REAL, r4miss, gxs, gys, r4map)

    where( .not. mskmap ) r4map = r4miss
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('varName', varName))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( varName == VARNAME__FDR .and. outfmt == OUTFMT__RRI )then
    do igy = gys, gye
    do igx = gxs, gxe
      call joint_conv_fdr_jflw2rri(int(i4map(igx,igy),1), i4map(igx,igy))
    enddo  ! igx/
    enddo  ! igy/
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  selectcase( lower(outfmt) )
  !-------------------------------------------------------------
  ! Case: Default (plain binary)
  case( OUTFMT__DEFAULT )
    call logmsg('Writing '//str(f_var))

    selectcase( dtype )
    case( DTYPE_INT1, DTYPE_INT4 )
      call traperr( wbin(i4map, f_var, replace=.true.) )
    case( DTYPE_REAL )
      call traperr( wbin(r4map, f_var, replace=.true.) )
    case default
      call errend(msg_invalid_value('dtype', dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: RRI
  case( OUTFMT__RRI )
    selectcase( dtype )
    case( DTYPE_INT1, DTYPE_INT4 )
      call rri_write_map(&
        basinType, resl, varName, uid, &
        i4map, i4miss, &
        west, south, JFLW_GRIDSIZE_LON &
      )
    case( DTYPE_REAL )
      call rri_write_map(&
        basinType, resl, varName, uid, &
        r4map, r4miss, &
        west, south, JFLW_GRIDSIZE_LON &
      )
    case default
      call errend(msg_invalid_value('dtype', dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('outfmt', outfmt))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine trim_basin
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
end module mod_mesh
