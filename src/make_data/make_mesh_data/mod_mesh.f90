module mod_mesh
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
  use c2_strnk_const
  use c2_strnk_type
  use c3_joint_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calcMeshIntersections
  public :: makeHiresNetworkMask
  public :: makeHiresNetworkUpperArea
  public :: scaleUpNetworkMask
  public :: trimBasin
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_mesh'
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type node_
    real(8) :: lon, lat
  end type

  type isct_
    integer :: n
    integer, pointer :: gx(:), gy(:)
    real(8), pointer :: leng(:)
    integer :: gxs, gxe, gys, gye
  end type

  type channel_
    integer :: nPt
    real(8), pointer :: lon(:), lat(:)
    type(node_) :: node(2)
    type(isct_) :: isct
  end type

  type network_
    character(DGT_NWKID) :: id
    integer :: nCh
    type(channel_), pointer :: channel(:)
    real(8) :: leng
    real(8) :: west, east, south, north
    type(isct_) :: isct
  end type
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calcMeshIntersections(resl, id)
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c2_strnk_io, only: &
    get_f_lst_networks_channel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calcMeshIntersections'
  character(*), intent(in) :: resl
  character(*), intent(in) :: id

  character(DGT_NWKID) :: id_
  integer :: nNwk, iNwk
  character :: c_
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Setup J-FlwDir grid
  !-------------------------------------------------------------
  call set_resolution(resl)
  !-------------------------------------------------------------
  ! Run
  !-------------------------------------------------------------
  if( id == '' )then
    f = get_f_lst_networks_channel()
    call logmsg('Reading '//str(f))
    open(newunit=un, file=f, status='old')
    read(un,*) c_, nNwk
    read(un,*)
    do iNwk = 1, nNwk
      read(un,*) c_, id_
      call calc_mesh_intersections(resl, id_)
    enddo
    close(un)
  else
    call calc_mesh_intersections(resl, id)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calcMeshIntersections
!===============================================================
!
!===============================================================
subroutine calc_mesh_intersections(resl, id)
  use c1_grid, only: &
    apprx_isct_with_meridian, &
    apprx_isct_with_parallel
  use c2_jflw_const
  use c2_jflw_grid, only: &
    gxs_of_lon , &
    gxe_of_lon , &
    gys_of_lat , &
    gye_of_lat , &
    west_of_gx , &
    east_of_gx , &
    south_of_gy, &
    north_of_gy
  use c2_strnk_io, only: &
    read_network          , &
    get_f_network_channel , &
    get_f_network_meshisct
  use c2_strnk_util, only: &
    clear_cmn_network
  use c3_joint_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_mesh_intersections'
  character(*), intent(in) :: resl
  character(*), intent(in) :: id

  type(cmn_network_) :: cmnnwk
  type(network_) :: nwk
  type(channel_), pointer :: ch
  type(isct_), pointer :: ci
  integer :: jCh
  integer :: jPt
  integer :: xs, xe, ys, ye
  integer :: ix, iy
  integer :: xwest, xeast, ysouth, ynorth
  real(8) :: clon1, clat1, clon2, clat2
  real(8) :: dlon1, dlat1, dlon2, dlat2
  integer :: nMesh_tmp
  real(8) :: leng
  real(8) :: leng_nwk, leng_isct

  character(CLEN_PATH) :: f
  integer :: un

  integer :: jCh_debug = 0
  logical :: debug_this

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Read StrRank network data
  !-------------------------------------------------------------
  call logmsg('Network '//str(id))

  allocate(character(len_trim(id)) :: cmnnwk%id)
  cmnnwk%id = id

  f = get_f_network_channel(id, 'sbin')

  call read_network(f, cmnnwk)

  call copy_cmnnwk_to_nwk(cmnnwk, nwk)

  call clear_cmn_network(cmnnwk)
  !-------------------------------------------------------------
  ! Calc. intersection
  !-------------------------------------------------------------
  leng_nwk = 0.d0
  leng_isct = 0.d0

  do jCh = 1, nwk%nCh
    debug_this = jCh == jCh_debug

    ch => nwk%channel(jCh)
    ci => ch%isct

    xs = gxs_of_lon(minval(ch%node(:)%lon))
    xe = gxe_of_lon(maxval(ch%node(:)%lon))
    ys = gys_of_lat(maxval(ch%node(:)%lat))
    ye = gye_of_lat(minval(ch%node(:)%lat))
    nMesh_tmp = (xe - xs + 1) * (ye - ys + 1) * 4
    if( nMesh_tmp < 0 )then
      call errend('nMesh_tmp: '//str(nMesh_tmp))
    endif
    allocate(ci%gx(nMesh_tmp))
    allocate(ci%gy(nMesh_tmp))
    allocate(ci%leng(nMesh_tmp))

    ci%n = 0

    if( debug_this )then
      call logmsg('ch#'//str(jCh)//' nPt '//str(ch%nPt))
      call logmsg('  x '//str((/xs,xe/),' - ')//' y '//str((/ys,ye/),' - '))
    endif

    do jPt = 1, ch%nPt-1

      if( debug_this )then
        call logmsg('pt#'//str(jPt)//' '//&
          slonlat(ch%lon(jPt),ch%lat(jPt))//' - '//slonlat(ch%lon(jPt+1),ch%lat(jPt+1)))
      endif

      ! north to south
      if( ch%lat(jPt) > ch%lat(jPt+1) )then
        ynorth = gys_of_lat(ch%lat(jPt))
        ysouth = gye_of_lat(ch%lat(jPt+1))

        if( debug_this )then
          call logmsg('y n2s '//str((/ynorth,ysouth/),DGT_GXY))
        endif

        if( ynorth > ysouth )then
          if( ch%lat(jPt) /= ch%lat(jPt+1) )then
            call errend('ysouth < ynorth. ch#'//str(jCh))
          endif
          ynorth = ysouth
        endif

        do iy = ynorth, ysouth
          if( iy == ynorth )then
            clat1 = ch%lat(jPt)
            clon1 = ch%lon(jPt)
          else
            clat1 = clat2
            clon1 = clon2
          endif
          if( iy == ysouth )then
            clat2 = ch%lat(jPt+1)
            clon2 = ch%lon(jPt+1)
          else
            clat2 = south_of_gy(iy)
            clon2 = apprx_isct_with_parallel(&
              ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), clat2)
          endif

          call loop_for_x()
        enddo  ! iy/

      ! south to north
      else
        ysouth = gye_of_lat(ch%lat(jPt))
        ynorth = gys_of_lat(ch%lat(jPt+1))

        if( debug_this )then
          call logmsg('y s2n '//str((/ysouth,ynorth/),DGT_GXY))
        endif

        if( ysouth < ynorth )then
          if( ch%lat(jPt) /= ch%lat(jPt+1) )then
            call errend('ysouth < ynorth. ch#'//str(jCh))
          endif
          ynorth = ysouth
        endif      


        do iy = ysouth, ynorth, -1
          if( iy == ysouth )then
            clat1 = ch%lat(jPt)
            clon1 = ch%lon(jPt)
          else
            clat1 = clat2
            clon1 = clon2
          endif
          if( iy == ynorth )then
            clat2 = ch%lat(jPt+1)
            clon2 = ch%lon(jPt+1)
          else
            clat2 = north_of_gy(iy)
            clon2 = apprx_isct_with_parallel(&
              ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), clat2)
          endif

          call loop_for_x()
        enddo  ! iy/
      endif

    enddo  ! jPt/

    if( ci%n == 0 )then
      call logmsg('ch#'//str(jCh))
      call logmsg('  '//slonlat(ch%node(1)%lon,ch%node(1)%lat)//&
                  ' - '//slonlat(ch%node(2)%lon,ch%node(2)%lat))
      call logmsg('  x '//str((/xs,xe/),' - ')//' y '//str((/ys,ye/),' - '))
      call logmsg('  n '//str(ci%n))
    endif
    call realloc(ci%gx, ci%n, clear=.false.)
    call realloc(ci%gy, ci%n, clear=.false.)
    call realloc(ci%leng, ci%n, clear=.false.)

    call add(leng_isct, sum(ci%leng))

    do jPt = 1, ch%nPt-1
      call add(leng_nwk, &
        dist_sphere(ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, &
          ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r) * EARTH_R &
      )
    enddo  ! jPt/
  enddo  ! jCh/

  call logmsg('Length nwk: '//str(leng_nwk,'es20.13')//&
    ' isct: '//str(leng_isct,'es20.13')//&
    ' (err: '//str((leng_isct-leng_nwk)/leng_nwk*1d2,'f10.5')//' %)')
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f = get_f_network_meshisct(MESHTYPE__JFLW, resl, id)
  call logmsg('Writing '//str(f))

  open(newunit=un, file=f, form='unformatted', access='sequential', status='replace')

  write(un) nwk%nCh

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    ci => ch%isct

    write(un) ci%n
    write(un) ci%gx(:)
    write(un) ci%gy(:)
    write(un) ci%leng(:)
  enddo  ! jCh/

  close(un)
  !-------------------------------------------------------------
  ! Clear
  !-------------------------------------------------------------
  nullify(ch)
  deallocate(nwk%channel)
  nwk%nCh = 0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine loop_for_x()
  implicit none

  ! west to east
  if( ch%lon(jPt) < ch%lon(jPt+1) )then
    xwest = gxs_of_lon(clon1)
    xeast = gxe_of_lon(clon2)
    if( debug_this )then
      call logmsg('y '//str(iy,DGT_GXY)//' x w2e '//str((/xwest,xeast/),DGT_GXY))
    endif

    do ix = xwest, xeast
      if( ix == xwest )then
        dlon1 = clon1
        dlat1 = clat1
      else
        dlon1 = dlon2
        dlat1 = dlat2
      endif
      if( ix == xeast )then
        dlon2 = clon2
        dlat2 = clat2
      else
        dlon2 = east_of_gx(ix)
        dlat2 = apprx_isct_with_meridian(&
          ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), dlon2)
      endif

      call update()
    enddo  ! ix/

  ! east to west
  else
    xwest = gxs_of_lon(clon2)
    xeast = gxe_of_lon(clon1)
    if( debug_this )then
      call logmsg('y '//str(iy,DGT_GXY)//' x e2w '//str((/xeast,xwest/),DGT_GXY))
    endif

    do ix = xeast, xwest, -1
      if( ix == xeast )then
        dlon1 = clon1
        dlat1 = clat1
      else
        dlon1 = dlon2
        dlat1 = dlat2
      endif
      if( ix == xwest )then
        dlon2 = clon2
        dlat2 = clat2
      else
        dlon2 = west_of_gx(ix)
        dlat2 = apprx_isct_with_meridian(&
          ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), dlon2)
      endif

      call update()
    enddo  ! ix/
  endif
end subroutine loop_for_x
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine update()
  implicit none

  !leng = dist_sphere(dlon1*d2r, dlat1*d2r, dlon2*d2r, dlat2*d2r) * EARTH_R
  leng = dist_sphere_2(dlon1*d2r, dlat1*d2r, dlon2*d2r, dlat2*d2r) * EARTH_R

  if( ci%n > 0 )then
    if( ci%gx(ci%n) == ix .and. ci%gy(ci%n) == iy )then
      call add(ci%leng(ci%n), leng)
      return
    endif

    if( abs(ci%gx(ci%n) - ix) > 1 .or. &
        abs(ci%gy(ci%n) - iy) > 1 )then
      call errend('Discontinuity was found in the intersection. ch#'//str(jCh)//&
      '\n  isct#'//str(ci%n)//' ('//str((/ci%gx(ci%n),ci%gy(ci%n)/),DGT_GXY,',')//')'//&
      '\n  isct#'//str(ci%n+1)//' ('//str((/ix,iy/),DGT_GXY,',')//')')
    endif
  endif

  if( size(ci%gx) == ci%n )then
    call realloc(ci%gx, ci%n*2, clear=.false.)
    call realloc(ci%gy, ci%n*2, clear=.false.)
    call realloc(ci%leng, ci%n*2, clear=.false.)
  endif

  call add(ci%n)
  ci%gx(ci%n) = ix
  ci%gy(ci%n) = iy
  ci%leng(ci%n) = leng
end subroutine update
!---------------------------------------------------------------
end subroutine calc_mesh_intersections
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
subroutine makeHiresNetworkMask(id)
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
    get_f_lst_networks_mesh
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'makeHiresNetworkMask'
  character(*), intent(in) :: id

  type(network_), pointer :: lst_nwk(:), nwk
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
  allocate(lst_nwk(n))

  read(un,*)
  do i = 1, n
    nwk => lst_nwk(i)
    read(un,*) c_, &
               nwk%id, &
               c_, c_, & ! nwk%tmpid, nwk%jNwk
               nwk%nCh, nwk%leng
  enddo
  close(un)
  !-------------------------------------------------------------
  ! Setup J-FlwDir mesh
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( id == '' )then
    f = get_f_lst_networks_mesh(RESOLUTION_1SEC)
    call logmsg('Writing '//str(f))
    open(newunit=un, file=f, status='replace')
    write(un,"(a)") 'networks '//str(n)
    write(un,"(a)") 'i id gxs gxe gys gye west east south north'

    do i = 1, n
      call make_hires_network_mask(&
        lst_nwk, i, &
        gxs, gxe, gys, gye &
      )

      write(un,"(a)") &
          str(i,dgt(n))//' '//str(lst_nwk(i)%id)//' '//&
          str((/gxs,gxe,gys,gye/),DGT_GXY)//' '//&
          sBBox(west_of_gx(gxs),east_of_gx(gxe),south_of_gy(gye),north_of_gy(gys),&
                d=' ', b='')
    enddo  ! i/

    close(un)
    call logmsg('Saved '//str(f))
  else
    call logmsg('====== DEBUG MODE ======')

    do i = 1, n
      if( lst_nwk(i)%id == id )then
        call make_hires_network_mask(&
          lst_nwk, i, &
          gxs, gxe, gys, gye &
        )
        exit
      endif
    enddo  ! i/
  endif
  !-------------------------------------------------------------
  deallocate(lst_nwk)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeHiresNetworkMask
!===============================================================
!
!===============================================================
subroutine make_hires_network_mask(&
  lst_nwk, jNwk_self, &
  gxs, gxe, gys, gye &
)
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
    write_network_mesh_domain, &
    get_f_network_channel    , &
    get_f_network_meshisct   , &
    get_f_network_mesh       
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'make_hires_network_mask'
  type(network_), intent(inout), target :: lst_nwk(:)
  integer, intent(in) :: jNwk_self
  integer, intent(out) :: gxs, gxe, gys, gye

  type(network_), pointer :: nwk, nwk_self
  type(isct_), pointer :: isct, isct_self
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
  nNwk = size(lst_nwk)

  nwk_self => lst_nwk(jNwk_self)
  isct_self => nwk_self%isct
  call read_isct_with_mesh(nwk_self)

  call logmsg('('//str(jNwk_self,dgt(nNwk))//') '//&
      str(nwk_self%id)//' meshes: '//str(isct_self%n))
  !-------------------------------------------------------------
  ! Init. nwkmap
  !-------------------------------------------------------------
  call logent('Initializing network mesh')

  gxs = isct_self%gxs
  gxe = isct_self%gxe
  gys = isct_self%gys
  gye = isct_self%gye

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
  nwkmap(:,:) = STAT_NWKMESH__UNKNOWN

  allocate(fdrmap(gxs_outer:gxe_outer,gys_outer:gye_outer))
  call read_map_from_tile(&
    RESOLUTION_1SEC, 'dir', DTYPE_INT1, FDR_MISS, &
    gxs_outer, gys_outer, &
    fdrmap &
  )

  where( fdrmap == FDR_MISS )
    nwkmap = STAT_NWKMESH__OCEAN
  endwhere

  call logext()
  !-------------------------------------------------------------
  ! Aggregate intersections of channels
  !-------------------------------------------------------------
  call logent('Aggregating intersections of channels')

  do jNwk = 1, nNwk
    if( jNwk == jNwk_self ) cycle
    nwk => lst_nwk(jNwk)
    isct => nwk%isct

    if( isct%gxe < gxs_outer .or. gxe_outer < isct%gxs .or. &
        isct%gye < gys_outer .or. gye_outer < isct%gys ) cycle

    !call logmsg('nwk '//str(nwk%id)//&
    !    ' ('//str((/nwk%west,nwk%east/),'f12.7',',')//&
    !     ','//str((/nwk%south,nwk%north/),'f11.7',',')//')'//&
    !     ' nCh: '//str(nwk%nCh))

    if( isct%n == 0 )then
      call read_isct_with_mesh(nwk)
    endif

    do iPix = 1, isct%n
      if( isct%gx(iPix) < gxs_outer .or. gxe_outer < isct%gx(iPix) .or. &
          isct%gy(iPix) < gys_outer .or. gye_outer < isct%gy(iPix) ) cycle
      nwkmap(isct%gx(iPix),isct%gy(iPix)) = STAT_NWKMESH__ISCT_OTHER
    enddo  ! iPix/
  enddo  ! jNwk/

  do iPix = 1, isct_self%n
    nwkmap(isct_self%gx(iPix),isct_self%gy(iPix)) = STAT_NWKMESH__ISCT
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
          fdrmap &
        )

        call realloc(nwkmap, (/gxs_outer,gys_outer/), (/gxe_outer,gye_outer/), &
                     clear=.false., fill=STAT_NWKMESH__UNKNOWN)
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
      nwkmap(igx,igy) = STAT_NWKMESH__OCEAN
    elseif( nwkmap(igx,igy) == STAT_NWKMESH__UNKNOWN )then
      is_ok = reached_nwk(igx, igy)
    endif
  enddo  ! igx/
  enddo  ! igy/

  !print*, gxe-gxs+1, gye-gys+1
  !call traperr( wbin(nwkmap(gxs:gxe,gys:gye), 'tmp/nwkmap.bin', replace=.true.) )

  if( any(nwkmap(gxs:gxe,gys:gye) == STAT_NWKMESH__UNKNOWN) )then
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
    RESOLUTION_1SEC, nwk_self%id, &
    gxs, gxe, gys, gye, &
    west_of_gx(gxs), east_of_gx(gxe), south_of_gy(gye), north_of_gy(gys) &
  )

  f = get_f_network_mesh(RESOLUTION_1SEC, 'mask', nwk_self%id)
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
subroutine read_isct_with_mesh(nwk)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__read_isct_with_mesh'
  type(network_), intent(inout), target :: nwk

  type(isct_), pointer :: ni, ci
  integer :: jCh, nCh
  integer :: is, ie, iis, iie, imax
  integer, allocatable :: arg(:)
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_network_meshisct(MESHTYPE__JFLW, RESOLUTION_1SEC, nwk%id)
  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')
  read(un) nCh
  if( nCh /= nwk%nCh )then
    call errend('The number of channels mismatch. Record in the file: '//str(nCh)//&
      ', stored value: '//str(nwk%nCh))
  endif

  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    ci => nwk%channel(jCh)%isct
    read(un) ci%n
    allocate(ci%gx(ci%n))
    allocate(ci%gy(ci%n))
    allocate(ci%leng(ci%n))
    read(un) ci%gx
    read(un) ci%gy
    read(un) ci%leng
  enddo  ! jCh
  close(un)

  ni => nwk%isct
  ni%n = sum(nwk%channel(:)%isct%n)
  allocate(ni%gx(ni%n))
  allocate(ni%gy(ni%n))
  allocate(ni%leng(ni%n))
  ni%n = 0
  do jCh = 1, nwk%nCh
    ci => nwk%channel(jCh)%isct
    ni%gx(ni%n+1:ni%n+ci%n) = ci%gx(:)
    ni%gy(ni%n+1:ni%n+ci%n) = ci%gy(:)
    ni%leng(ni%n+1:ni%n+ci%n) = ci%leng(:)
    call add(ni%n, ci%n)
  enddo  ! jCh

  allocate(arg(ci%n))
  call argsort(ci%gx, arg)
  call sort(ci%gx, arg)
  call sort(ci%gy, arg)
  call sort(ci%leng, arg)

  imax = ni%n
  ni%n = 0
  ie = 0
  do while( ie < imax )
    is = ie + 1
    ie = is
    do while( ie < imax )
      if( ni%gx(ie+1) /= ni%gx(is) ) exit
      ie = ie + 1
    enddo

    if( any(ni%gx(is:ie) /= ni%gx(is)) )then
      call errend('Sorting failed (1)')
    endif
    call argsort(ci%gy(is:ie), arg(is:ie))
    call sort(ci%gy(is:ie), arg(is:ie))
    call sort(ci%leng(is:ie), arg(is:ie))

    iie = is - 1
    do while( iie < ie )
      iis = iie + 1
      iie = iis
      do while( iie < ie )
        if( ni%gy(iie+1) /= ni%gy(iis) ) exit
        iie = iie + 1
      enddo
      if( any(ni%gy(iis:iie) /= ni%gy(iis)) )then
        call errend('Sorting failed (2)')
      endif

      call add(ni%n)
      ni%gx(ni%n) = ni%gx(iis)
      ni%gy(ni%n) = ni%gy(iis)
      ni%leng(ni%n) = sum(ni%leng(iis:iie))
    enddo  ! iis,iie
  enddo  ! is,ie
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_isct_with_mesh
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
      case( STAT_NWKMESH__ISCT, &
            STAT_NWKMESH__REACH )
        res = .true.
        stat = 0
        exit
      case( STAT_NWKMESH__UNKNOWN )
        continue
      case( STAT_NWKMESH__ISCT_OTHER, &
            STAT_NWKMESH__REACH_OTHER )
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
    do while( nwkmap(igx,igy) == STAT_NWKMESH__UNKNOWN )
      nwkmap(igx,igy) = STAT_NWKMESH__REACH
      call get_nextxy(igx, igy, fdrmap(igx,igy), gxx, gyy)
      igx = gxx
      igy = gyy
    enddo  ! while fdrmap > 0
  !-------------------------------------------------------------
  ! Case: Reaches other network
  case( 1  )
    igx = gx
    igy = gy
    do while( nwkmap(igx,igy) == STAT_NWKMESH__UNKNOWN )
      nwkmap(igx,igy) = STAT_NWKMESH__REACH_OTHER
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
      nwkmap(igx,igy) = STAT_NWKMESH__OUT
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
end subroutine make_hires_network_mask
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
recursive subroutine makeHiresNetworkUpperArea(id)
  use c2_strnk_io, only: &
    read_network              , &
    read_network_mesh_domain  , &
    get_f_lst_networks_channel, &
    get_f_network_mesh        
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c2_jflw_grid, only: &
    gxs_of_lon , &
    gxe_of_lon , &
    gys_of_lat , &
    gye_of_lat , &
    south_of_gy, &
    get_nextxy
  use c2_jflw_io, only: &
    read_map_from_tile
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeHiresNetworkUpperArea'
  character(*), intent(in) :: id

  character(DGT_NWKID) :: id_this
  integer :: nNwk, jNwk
  integer(1), allocatable :: mskmap(:,:)
  integer(1), allocatable :: fdrmap(:,:)
  real(8)   , allocatable :: upamap(:,:)
  real(8)   , allocatable :: addmap(:,:)
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixlen(:)
  integer :: nx, ny, gxs, gxe, gys, gye
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
  if( id == '' )then
    f = get_f_lst_networks_channel()
    open(newunit=un, file=f, status='old')
    read(un,*) c_, nNwk
    do jNwk = 1, nNwk
      read(un,*) c_, id_this
      call makeHiresNetworkUpperArea(id_this)
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
  call logmsg('Network '//str(id))

  call read_network_mesh_domain(&
    RESOLUTION_1SEC, id, &
    nx, ny, gxs, gxe, gys, gye, &
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

  f = get_f_network_mesh(RESOLUTION_1SEC, 'mask', id)
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
  f = get_f_network_mesh(RESOLUTION_1SEC, 'upa', id)
  call logmsg('Writing '//str(f))
  call traperr( wbin(upamap(gxs:gxe,gys:gye), f, dtype=DTYPE_REAL, replace=.true.) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(upamap)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeHiresNetworkUpperArea
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
  character(DGT_NWKID) :: id
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
  write(un,"(a)") 'i id gxs gxe gys gye west east south north'

  f_in = get_f_lst_networks_mesh(RESOLUTION_1SEC)
  open(newunit=un_in, file=f_in, status='old')
  read(un_in,*) c_, nNwk
  read(un_in,*)

  do jNwk = 1, nNwk
    read(un_in,*) &
      c_, id, &
      gxs_in, gxe_in, gys_in, gye_in, &
      west_in, east_in, south_in, north_in

    call logmsg('('//str(jNwk)//') '//str(id))

    call scale_up_network_mask(&
      resl, ratio, id, gxs_in, gxe_in, gys_in, gye_in, &
      gxs, gxe, gys, gye)

    write(un,"(a)") &
      str(jNwk,dgt(nNwk))//' '//str(id)//' '//&
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
  resl, ratio, id, &
  ghxs, ghxe, ghys, ghye, &
  gxs, gxe, gys, gye &
)
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
  character(*), intent(in) :: id
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
  f_in = get_f_network_mesh(RESOLUTION_1SEC, 'mask', id)
  call traperr( rbin(nwkmap_in, f_in) )

  allocate(nwkmap(gxs:gxe,gys:gye))
  do igy = gys, gye
    call get_domain(igy, ghys_this, ghye_this, ghys, ghye)
    do igx = gxs, gxe
      call get_domain(igx, ghxs_this, ghxe_this, ghxs, ghxe)
      nwkmap(igx,igy) = maxval(nwkmap_in(ghxs_this:ghxe_this,ghys_this:ghye_this))
    enddo  ! igx/
  enddo  ! igy/

  f = get_f_network_mesh(resl, 'domain', id)
  call write_network_mesh_domain(&
         resl, id, &
         gxs, gxe, gys, gye, &
         west_of_gx(gxs), east_of_gx(gxe), south_of_gy(gye), north_of_gy(gys) &
  )

  f = get_f_network_mesh(resl, 'mask', id)
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
subroutine trimBasin(&
  catmType, resl, id, varName, outfmt, overwrite &
)
  use c3_jflw_const, &
        jflw_set_resolution => jflw_set_resolution
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'trimBasin'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: id
  character(*), intent(in) :: outfmt
  logical     , intent(in) :: overwrite

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call jflw_set_resolution(resl)

  call trim_basin(&
    catmType, resl, id, varName, outfmt, overwrite &
  )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine trimBasin
!===============================================================
!
!===============================================================
subroutine trim_basin(&
  catmType, resl, id, varName, outfmt, overwrite &
)
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
    rri_read_map  , &
    rri_write_map
  use c3_joint_util, only: &
    joint_conv_fdr_jflw2rri => conv_fdr_jflw2rri, &
    joint_get_miss          => get_miss
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'trim_basin'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl
  character(*), intent(in) :: id
  character(*), intent(in) :: varName
  character(*), intent(in) :: outfmt
  logical     , intent(in) :: overwrite

  integer(1), allocatable :: i1map(:,:)
  integer(4), allocatable :: i4map(:,:)
  real(4)   , allocatable :: r4map(:,:)
  logical(1), allocatable :: mskmap(:,:)
  integer(1) :: i1miss
  integer(4) :: i4miss
  real(4)    :: r4miss
  integer :: nx, ny, gxs, gxe, gys, gye
  real(8) :: west, east, south, north
  integer :: igx, igy
  character(CLEN_VAR) :: inputDataName
  character(CLEN_KEY) :: dtype
  character(CLEN_PATH) :: f_msk, f_var

  character(CLEN_KEY), parameter :: OUTFMT__DEFAULT = 'default'
  character(CLEN_KEY), parameter :: OUTFMT__RRI     = 'rri'

  integer :: access

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Setup
  !-------------------------------------------------------------
  selectcase( outfmt )
  case( OUTFMT__DEFAULT )
    inputDataName = DATANAME__JFLW

    selectcase( catmType )
    case( CATMTYPE__BASIN )
      f_var = jflw_get_f_map_basin(resl, varName, id)
    case( CATMTYPE__NETWORK )
      f_var = strnk_get_f_network_mesh(resl, varName, id)
    case( CATMTYPE__NETWORKSET )
      call errend(msg_not_implemented()//&
        '\n  catmType: '//str(catmType))
    case default
      call errend(msg_invalid_value('catmType', catmType))
    endselect
  case( OUTFMT__RRI )
    inputDataName = DATANAME__RRI

    f_var = rri_get_f_data(catmType, resl, varName, id)
  case default
    call errend(msg_invalid_value('outfmt', outfmt))
  endselect

  if( .not. overwrite .and. access(f_var,' ') == 0 )then
    call logmsg('File already exists: '//str(f_var))
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! Make a mask
  !-------------------------------------------------------------
  selectcase( catmType )
  !-------------------------------------------------------------
  ! Case: J-FlwDir basin map
  case( CATMTYPE__BASIN )
    call jflw_read_basin_domain_from_each(&
        resl, id, &
        gxs, gxe, gys, gye, west, east, south, north)

    call logmsg('Basin '//str(id))
    call logmsg('(x,y): ('//str((/gxe-gxs+1,gye-gys+1/),JFLW_DGT_GXY,',')//&
        ') ['//str((/gxs,gxe/),JFLW_DGT_GXY,':')//','//&
        str((/gys,gye/),JFLW_DGT_GXY,':')//']')
    call logmsg('BBox: '//sBBox(west,east,south,north))

    allocate(mskmap(gxs:gxe,gys:gye))

    if( resl == RESOLUTION_1SEC )then
      allocate(i4map(gxs:gxe,gys:gye))
      call jflw_read_map_from_tile(&
        resl, 'bsn', DTYPE_INT4, JFLW_BSN_MISS, gxs, gys, i4map)
      where( i4map == jflw_intId(id) )
        mskmap = .true.
      elsewhere
        mskmap = .false.
      endwhere
      deallocate(i4map)
    else
      allocate(r4map(gxs:gxe,gys:gye))
      call rri_read_map(&
        catmType, resl, 'elv', id, r4map, r4miss)
      where( r4map == r4miss )
        mskmap = .false.
      elsewhere
        mskmap = .true.
      endwhere
      deallocate(r4map)
    endif
  !-------------------------------------------------------------
  ! Case: Network mesh
  case( CATMTYPE__NETWORK )
    call strnk_read_network_mesh_domain(&
      resl, id, &
      nx, ny, gxs, gxe, gys, gye, &
      west, east, south, north &
    )

    allocate(mskmap(gxs:gxe,gys:gye))

    allocate(i1map(gxs:gxe,gys:gye))

    f_msk = strnk_get_f_network_mesh(resl, 'mask', id)
    call traperr( rbin(i1map, f_msk) )

    where( i1map > 0_1 )
      mskmap = .true.
    elsewhere
      mskmap = .false.
    endwhere

    deallocate(i1map)
  !-------------------------------------------------------------
  ! Case: Network set mesh
  case( CATMTYPE__NETWORKSET )
    call errend(msg_not_implemented()//&
      '\n  catmType: '//str(catmType))
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('catmType', catmType))
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
  selectcase( outfmt )
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
        catmType, resl, varName, id, &
        i4map, i4miss, &
        west, south, JFLW_GRIDSIZE_LON &
      )
    case( DTYPE_REAL )
      call rri_write_map(&
        catmType, resl, varName, id, &
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
subroutine copy_cmnnwk_to_nwk(cmnnwk, nwk)
  implicit none
  type(cmn_network_), intent(in) :: cmnnwk
  type(network_), intent(out) :: nwk

  type(cmn_channel_), pointer :: cmnch
  type(channel_), pointer :: ch
  integer :: jCh

  nwk%nCh = cmnnwk%nCh
  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    cmnch => cmnnwk%channel_(jCh)

    ch%nPt = cmnch%n
    allocate(ch%lon(ch%nPt))
    allocate(ch%lat(ch%nPt))
    ch%lon(:) = cmnch%lon(:)
    ch%lat(:) = cmnch%lat(:)

    ch%node(:)%lon = cmnch%node_(:)%lon
    ch%node(:)%lat = cmnch%node_(:)%lat
  enddo  ! jCh/
end subroutine copy_cmnnwk_to_nwk
!===============================================================
!
!===============================================================
end module mod_mesh
