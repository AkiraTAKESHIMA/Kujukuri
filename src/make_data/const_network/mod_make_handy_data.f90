module mod_make_handy_data
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_math
  use lib_array
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: makeWscodeMasks
  public :: divideStrrankIntoTiles
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_make_handy_data'
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
subroutine makeWscodeMasks()
  use c2_nlni_const, &
        set_resolution => set_resolution
  use c2_nlni_io, only: &
        get_f_lst_wsCode     , &
        get_f_lst_wsCodeRange, &
        get_f_wsCodeMask     , &
        get_f_map_tile       , &
        read_map_from_tile
  use c2_nlni_grid, only: &
        gx_of_x    , &
        gy_of_y    , &
        west_of_gx , &
        east_of_gx , &
        south_of_gy, &
        north_of_gy
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeWscodeMasks'

  type tbl_wsCode_
    integer :: gxs, gxe, gys, gye
  end type

  type(tbl_wsCode_), pointer :: tbl_ws(:), ws
  integer, pointer :: lst_wsCode(:)
  integer :: nWsCode
  integer, pointer :: wsCode(:,:)
  integer :: wsCode_rvunknown
  integer :: wsCode_prev
  integer :: i, j
  integer :: itx, ity, ix, iy, igx, igy
  integer :: gx, gy

  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_100M)
  !-------------------------------------------------------------
  ! Make wsCode list
  !-------------------------------------------------------------
  call logent('Making wsCode list')

  open(newunit=un, file=get_f_lst_wsCode(), status='old')
  read(un,*) nWsCode
  allocate(lst_wsCode(nWsCode))
  do i = 1, nWsCode
    read(un,*) lst_wsCode(i)
  enddo
  close(un)

  call realloc(lst_wsCode, &
    nWsCode + maxval(lst_wsCode)/DIV_WSCODE_RVUNKNOWN, clear=.false.)

  do j = 1, maxval(lst_wsCode) / DIV_WSCODE_RVUNKNOWN
    wsCode_rvunknown = j * DIV_WSCODE_RVUNKNOWN

    call search(wsCode_rvunknown, lst_wsCode, i)
    if( i /= 0 )then
      call errend('wsCode_rvunknown '//str(wsCode_rvunknown)//&
             ' was found in the list.')
    endif

    call add(nWsCode)
    lst_wsCode(nWsCode) = wsCode_rvunknown
  enddo

  call sort(lst_wsCode)

  call logext()
  !-------------------------------------------------------------
  ! Get range of each wsCode
  !-------------------------------------------------------------
  call logent('Getting range of each wsCode')

  allocate(wsCode(NX,NY))

  allocate(tbl_ws(nWsCode))
  do i = 1, nWsCode
    ws => tbl_ws(i)
    ws%gxs = NGX
    ws%gxe = 1
    ws%gys = NGY
    ws%gye = 1
  enddo

  do ity = TYMIN, TYMAX
  do itx = TXMIN, TXMAX
    f = get_f_map_tile('wsCode', itx, ity)
    if( access(f, ' ') /= 0 ) cycle

    call traperr( rbin(wsCode, f) )

    wsCode_prev = WSCODE_MISS_I
    do iy = 1, NY
    do ix = 1, NX
      if( wsCode(ix,iy) == WSCODE_MISS_I ) cycle

      if( mod(wsCode(ix,iy), 10000) == 0 )then
      endif

      if( wsCode(ix,iy) /= wsCode_prev )then
        wsCode_prev = wsCode(ix,iy)
        
        call search(wsCode(ix,iy), lst_wsCode, i)
        if( i == 0 )then
          call errend('wsCode '//str(wsCode(ix,iy))//' was not found in the list.')
        endif
      endif
      ws => tbl_ws(i)

      gx = gx_of_x(itx, ix)
      gy = gy_of_y(ity, iy)

      ws%gxs = min(ws%gxs, gx)
      ws%gxe = max(ws%gxe, gx)
      ws%gys = min(ws%gys, gy)
      ws%gye = max(ws%gye, gy)
    enddo  ! ix/
    enddo  ! iy/
  enddo  ! itx/
  enddo  ! ity/

  do i = 1, nWsCode
    ws => tbl_ws(i)
    if( ws%gxs > ws%gxe )then
      ws%gxs = 0
      ws%gxe = 0
      ws%gys = 0
      ws%gye = 0
    endif
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Make masks
  !-------------------------------------------------------------
  call logent('Makine masks')

  call realloc(wsCode, 0)

  f = get_f_lst_wsCodeRange()
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(1x,a,1x,i0)") 'watsys', nWsCode
  write(un,"(a)") 'wsCode gxs gxe gys gye west east south north'

  do i = 1, nWsCode
    ws => tbl_ws(i)

    if( ws%gxs == 0 )then
      write(un,"(1x,i6,4(1x,i"//str(DGT_GXY)//"),4(1x,f12.7))") &
        lst_wsCode(i), ws%gxs, ws%gxe, ws%gys, ws%gye, &
        0., 0., 0., 0.
      cycle
    else
      write(un,"(1x,i6,4(1x,i"//str(DGT_GXY)//"),4(1x,f12.7))") &
        lst_wsCode(i), ws%gxs, ws%gxe, ws%gys, ws%gye, &
        west_of_gx(ws%gxs), east_of_gx(ws%gxe), &
        south_of_gy(ws%gys), north_of_gy(ws%gye)
    endif

    print*, lst_wsCode(i), ws%gxs, ws%gxe, ws%gys, ws%gye

    call realloc(wsCode, (/ws%gxs,ws%gys/), (/ws%gxe,ws%gye/), clear=.true.)

    call read_map_from_tile('wsCode', ws%gxs, ws%gys, wsCode)

    do igy = ws%gys, ws%gye
    do igx = ws%gxs, ws%gxe
      if( wsCode(igx,igy) == lst_wsCode(i) )then
        wsCode(igx,igy) = 1
      else
        wsCode(igx,igy) = 0
      endif
    enddo
    enddo

    call traperr( wbin(wsCode, get_f_wsCodeMask(lst_wsCode(i)), dtype=DTYPE_INT1) )
  enddo  ! i/
  close(un)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(wsCode)
  deallocate(tbl_ws)
  deallocate(lst_wsCode)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeWscodeMasks
!===============================================================
! Divide StrRank into NLNI tiles
!===============================================================
subroutine divideStrrankIntoTiles()
  use c2_nlni_const, &
        set_resolution => set_resolution
  use c2_nlni_grid, only: &
        gxs_of_lon, &
        gxe_of_lon, &
        gys_of_lat, &
        gye_of_lat, &
        tx_of_gx, &
        ty_of_gy
  use c2_nlni_io, only: &
        tilename
  use c3_strnk_const
  use c2_strnk_io, only: &
        strnk_region_idx2str      => region_idx2str  , &
        strnk_get_f_stream_shp    => get_f_stream_shp, &
        strnk_get_f_lst_tiled_idx => get_f_lst_tiled_idx
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'divideStrrankIntoTiles'

  type region_
    integer :: sz
    integer :: n
    integer, pointer :: lst_iEnt(:)
  end type

  type tile_
    type(region_), pointer :: region(:)
  end type

  type(shp_) :: shp
  type(shp_entity_), pointer :: ent

  type(tile_), pointer :: tile(:,:)
  type(region_), pointer :: tr
  integer :: txs, txe, tys, tye, itx, ity
  integer :: is, ie
  integer :: n, i
  integer :: iRegion, nRegion
  character(CLEN_VAR) :: region
  integer :: iEnt, iEnt_max

  character(CLEN_PATH) :: f
  integer :: un_tile
  character(CLEN_WFMT) :: wfmt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_100M)

  allocate(tile(TXMIN:TXMAX, TYMIN:TYMAX))
  do ity = TYMIN, TYMAX
  do itx = TXMIN, TXMAX
    allocate(tile(itx,ity)%region(STRNK_NREGION))
    do iRegion = 1, STRNK_NREGION
      tr => tile(itx,ity)%region(iRegion)
      tr%sz = 1024
      tr%n = 0
      allocate(tr%lst_iEnt(tr%sz))
    enddo  ! iRegion/
  enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  ! Divide into tiles
  !-------------------------------------------------------------
  call logent('Dividing streams into tiles')

  do iRegion = 1, STRNK_NREGION
    region = strnk_region_idx2str(iRegion)
    f = strnk_get_f_stream_shp(region)
    call logmsg('Reading '//str(f))
    call traperr( shp_open(f) )
    call traperr( shp_get_all(shp) )
    call traperr( shp_close() )

    do iEnt = 1, shp%nEntity
      ent => shp%entity(iEnt)
      txs = tx_of_gx(gxs_of_lon(ent%xmin))
      txe = tx_of_gx(gxe_of_lon(ent%xmax))
      tys = ty_of_gy(gys_of_lat(ent%ymin))
      tye = ty_of_gy(gye_of_lat(ent%ymax))
      if( txs < TXMIN .or. txe > TXMAX .or. &
          tys < TYMIN .or. tye > TYMAX )then
        call logwrn('Tile index is out of range. iEnt: '//str(iEnt))
        txs = max(txs, TXMIN)
        txe = min(txe, TXMAX)
        tys = max(tys, TYMIN)
        tye = min(tye, TYMAX)
      endif

      do ity = tys, tye
      do itx = txs, txe
        tr => tile(itx,ity)%region(iRegion)
        if( tr%n == tr%sz )then
          tr%sz = tr%sz * 2
          call realloc(tr%lst_iEnt, tr%sz, clear=.false.)
        endif
        call add(tr%n)
        tr%lst_iEnt(tr%n) = iEnt
      enddo  ! itx/
      enddo  ! ity/
    enddo  ! iEnt/
  enddo  ! iRegion/

  call logext()
  !-------------------------------------------------------------
  ! Sort and remove duplications
  !-------------------------------------------------------------
  call logent('Sorting and removing duplications')

  do ity = TYMIN, TYMAX
  do itx = TXMIN, TXMAX
    do iRegion = 1, STRNK_NREGION
      tr => tile(itx,ity)%region(iRegion)
      if( tr%n == 0 ) cycle
      call sort(tr%lst_iEnt(:tr%n))
      n = 0
      ie = 0
      do while( ie < tr%n )
        is = ie + 1
        ie = is
        do while( ie < tr%n )
          if( tr%lst_iEnt(ie+1) /= tr%lst_iEnt(is) ) exit
          ie = ie + 1
        enddo
        n = n + 1
        tr%lst_iEnt(n) = tr%lst_iEnt(is)
      enddo
      tr%n = n
      call realloc(tr%lst_iEnt, tr%n, clear=.false.)
    enddo  ! iRegion/
  enddo  ! itx/
  enddo  ! ity/

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  do ity = TYMIN, TYMAX
  do itx = TXMIN, TXMAX
    nRegion = 0
    iEnt_max = 0
    do iRegion = 1, STRNK_NREGION
      tr => tile(itx,ity)%region(iRegion)
      if( tr%n == 0 ) cycle
      call add(nRegion)
      iEnt_max = max(iEnt_max, maxval(tr%lst_iEnt))
    enddo
    if( iEnt_max == 0 ) cycle
    wfmt = "(5x,i"//str(dgt(iEnt_max))//")"

    f = strnk_get_f_lst_tiled_idx(tilename(itx,ity))
    call logmsg('Writing '//str(f))
    open(newunit=un_tile, file=f, status='replace')
    write(un_tile,"(1x,a,1x,i0)") 'regions', nRegion
    do iRegion = 1, STRNK_NREGION
      tr => tile(itx,ity)%region(iRegion)
      if( tr%n == 0 ) cycle
      write(un_tile,"(3x,a,1x,i0)") trim(strnk_region_idx2str(iRegion)), tr%n
      do i = 1, tr%n
        write(un_tile,wfmt) tr%lst_iEnt(i)
      enddo
    enddo  ! iRegion/
  enddo  ! itx/
  enddo  ! ity/

  call logext()
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  call shp_clear_all(shp)
  nullify(tr)
  deallocate(tile)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine divideStrrankIntoTiles
!===============================================================
!
!===============================================================
end module mod_make_handy_data
