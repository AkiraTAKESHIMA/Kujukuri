module c2_nlni_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c2_nlni_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: tilename
  public :: strWsCode
  public :: strRvCode

  public :: get_wsName

  public :: read_map_from_tile

  public :: clip_from_tile

  public :: dirname_resolution
  public :: get_f_lst_wsCode
  public :: get_f_lst_wsCodeRange
  public :: get_f_wsCodeMask
  public :: get_f_map_tile
  public :: get_dir_bsnara_tile
  public :: get_f_dat_channel
  public :: get_f_shp_lake
  public :: get_f_tbl_landuse
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_map_from_tile
    module procedure read_map_from_tile__int4
  end interface

  interface clip_from_tile
    module procedure clip_from_tile__int1
  end interface

  interface
    integer function access(f, mode)
      character(*), intent(in) :: f
      character(*), intent(in) :: mode
    end function access
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_nlni_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
character(4) function tilename(tx,ty)
  implicit none
  integer, intent(in) :: tx, ty

  tilename = str(ty,-2)//str(tx,-2)
end function tilename
!===============================================================
!
!===============================================================
character(DGT_WSCODE) function strWsCode(wsCode) result(res)
  implicit none
  integer(4), intent(in) :: wsCode

  if( wsCode > 0 )then
    res = str(wsCode, -DGT_WSCODE)
  else
    res = str(wsCode, DGT_WSCODE)
  endif
end function strWsCode
!===============================================================
!
!===============================================================
character(DGT_RVCODE) function strRvCode(rvCode) result(res)
  implicit none
  integer(8), intent(in) :: rvCode

  if( rvCode > 0_8 )then
    res = str(rvCode, -DGT_RVCODE)
  else
    res = str(rvCode, DGT_RVCODE)
  endif
end function strRvCode
!===============================================================
!
!===============================================================
character(CLEN_VAR) function get_wsName(wsCode) result(res)
  implicit none
  integer, intent(in) :: wsCode

  integer :: n, i
  integer :: wsCode_
  character(CLEN_VAR) :: wsName
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'get_wsName'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  res = ''

  open(newunit=un, file=get_f_lst_wsCode(), status='old')
  read(un,*) n
  do i = 1, n
    read(un,*) wsCode_, wsName
    if( wsCode_ == wsCode )then
      res = wsName
      exit
    endif
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_wsName
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
subroutine read_map_from_tile__int4(&
    var, gxs, gys, dat)
  use c2_nlni_grid, only: &
        tx_of_gx   , &
        ty_of_gy   , &
        gxs_of_tx  , &
        gxe_of_tx  , &
        gys_of_ty  , &
        gye_of_ty  , &
        gx_of_x    , &
        gy_of_y    , &
        west_of_gx , &
        east_of_gx , &
        south_of_gy, &
        north_of_gy
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_map_from_tile__int4'
  character(*), intent(in)  :: var
  integer     , intent(in)  :: gxs, gys
  integer(4)  , intent(out) :: dat(gxs:,gys:)

  integer :: gxe, gye
  integer :: txs, txe, tys, tye
  integer :: gxs_tile, gxe_tile, gys_tile, gye_tile
  integer :: xs, xe, ys, ye
  integer :: gxs_this, gxe_this, gys_this, gye_this
  integer :: itx, ity
  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  gxe = gxs + size(dat,1) - 1
  gye = gys + size(dat,2) - 1

  txs = tx_of_gx(gxs)
  txe = tx_of_gx(gxe)
  tys = ty_of_gy(gys)
  tye = ty_of_gy(gye)

  do ity = tys, tye
    gys_tile = gys_of_ty(ity)
    gye_tile = gye_of_ty(ity)
    ys = max(gys, gys_tile) - gys_tile + 1
    ye = min(gye, gye_tile) - gys_tile + 1
    gys_this = gy_of_y(ity, ys)
    gye_this = gy_of_y(ity, ye)

    do itx = txs, txe
      gxs_tile = gxs_of_tx(itx)
      gxe_tile = gxe_of_tx(itx)
      xs = max(gxs, gxs_tile) - gxs_tile + 1
      xe = min(gxe, gxe_tile) - gxs_tile + 1
      gxs_this = gx_of_x(itx, xs)
      gxe_this = gx_of_x(itx, xe)

      f = get_f_map_tile(var, itx, ity)
      call logmsg('f: '//str(f))
      call logmsg('tile '//str(tilename(itx,ity))//&
             ' g ('//str((/gxs_this,gxe_this/),DGT_GXY,':')//&
             ','//str((/gys_this,gye_this/),DGT_GXY,':')//')'//&
             ' l ('//str((/xs,xe/),DGT_XY,':')//&
             ','//str((/ys,ye/),DGT_XY,':')//')')
      if( access(f,' ') /= 0 ) cycle
      call traperr( rbin(&
               dat(gxs_this:gxe_this,gys_this:gye_this), &
               f, sz=int((/NX,NY/),8), lb=int((/xs,ys/),8)) )
    enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_map_from_tile__int4
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! west, east, south, north: coordinates of the bounding box
!===============================================================
subroutine clip_from_tile__int1(&
    dat, &
    varName, resolution_org, resolution, &
    west, east, south, north, &
    miss, mask)
  use c2_nlni_grid, only: &
        txs_of_lon, txe_of_lon, &
        tys_of_lat, tye_of_lat, &
        gxs_of_lon, gxe_of_lon, &
        gys_of_lat, gye_of_lat
  implicit none
  integer(1)  , intent(out) :: dat(:,:)
  character(*), intent(in) :: varName
  character(*), intent(in) :: resolution_org
  character(*), intent(in) :: resolution  ! of this data
  real(8)     , intent(in) :: west, east, south, north
  integer(1)  , intent(in) :: miss
  logical(1)  , intent(in), optional :: mask(:,:)

  integer :: txs, txe, tys, tye, itx, ity
  integer :: gxs, gxe, gys, gye
  integer :: lxs, lxe, lys, lye
  integer :: gxs_, gxe_, gys_, gye_
  integer :: ix, iy
  character(CLEN_PATH) :: f

  character(CLEN_PROC), parameter :: PRCNAM = 'clip_from_tile__int1'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  txs = txs_of_lon(west + cellsize_lon*0.5)
  txe = txe_of_lon(east - cellsize_lon*0.5)
  tys = tys_of_lat(south + cellsize_lat*0.5)
  tye = tye_of_lat(north - cellsize_lat*0.5)

  gxs = gxs_of_lon(west + cellsize_lon*0.5)
  gxe = gxe_of_lon(east - cellsize_lon*0.5)
  gys = gys_of_lat(south + cellsize_lat*0.5)
  gye = gye_of_lat(north - cellsize_lat*0.5)

  if( gxe-gxs+1 /= size(dat,1) .or. gye-gys+1 /= size(dat,2) )then
    call errend('Unexpected condition: '//&
              '\n  gxe - gxs + 1 /= size(dat,1) .or. gye - gys + 1 /= size(dat,2)')
  endif

  dat(:,:) = miss
  do ity = tys, tye
    lys = max( 1, gys-(ity-tymin)*ny)
    lye = min(ny, gye-(ity-tymin)*ny)

    gys_ = max(gys, (ity-tymin)*ny+1)
    gye_ = min(gye, (ity-tymin+1)*ny)

    do itx = txs, txe
      lxs = max( 1, gxs-(itx-txmin)*nx)
      lxe = min(nx, gxe-(itx-txmin)*nx)

      gxs_ = max(gxs, (itx-txmin)*nx+1)
      gxe_ = min(gxe, (itx-txmin+1)*nx)

      f = get_f_map_tile_remapped(varName, resolution_org, resolution, itx, ity)
      if( access(f,' ') /= 0 ) cycle
print*, trim(f)
      call traperr( rbin(&
             dat(gxs_-gxs+1:gxe_-gxs+1,gys_-gys+1:gye_-gys+1), &
             f, sz=int((/nx,ny/),8), lb=int((/lxs,lys/),8)) )
    enddo  ! itx/
  enddo  ! ity/

  if( present(mask) )then
    do iy = 1, size(dat,2)
    do ix = 1, size(dat,1)
      if( .not. mask(ix,iy) ) dat(ix,iy) = miss
    enddo
    enddo
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine clip_from_tile__int1
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
character(CLEN_PATH) function dirname_resolution(&
    resolution_org, resolution_rmp) result(res)
  implicit none
  character(*), intent(in) :: resolution_org, resolution_rmp

  if( resolution_org == resolution_rmp )then
    res = resolution_org
  else
    res = trim(resolution_rmp)//'_from_'//trim(resolution_org)
  endif
end function dirname_resolution
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_wsCode() result(res)
  implicit none

  res = joined(DIR_DL, 'wsCode/all.lst')
end function get_f_lst_wsCode
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_wsCodeRange() result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_wsCodeRange'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  res = joined(DIR_PRD, 'wsCodeMask/range.txt')

  call traperr( mkdir(dirname(res)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_wsCodeRange
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_wsCodeMask(wsCode) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_wsCodeMask'
  integer, intent(in) :: wsCode

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  res = joined(DIR_PRD, 'wsCodeMask/'//strWsCode(wsCode)//'.bin')

  call traperr( mkdir(dirname(res)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_wsCodeMask
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_dir_bsnara_tile(tx,ty) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_dir_bsnara_tile'
  integer, intent(in) :: tx, ty

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  res = joined(DIR_PRD, 'bsnara/'//tilename(tx,ty))

  call traperr( mkdir(res) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_dir_bsnara_tile
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_dat_channel(bsnId) result(res)
  implicit none
  integer, intent(in) :: bsnId

  res = joined(DIR_PRD, 'channel/'//str(bsnId,-6)//'.txt')
end function get_f_dat_channel
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_shp_lake(ext) result(res)
  implicit none
  character(*), intent(in) :: ext

  res = joined(DIR_DL, 'lake/W09-05-g_Lake.'//trim(ext))
end function get_f_shp_lake
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_tbl_landuse() result(res)
  implicit none

  res = joined(DIR_PRD, 'landuse/categories.tbl')
end function get_f_tbl_landuse
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_map_tile(&
    var, tx, ty) result(res)
  implicit none
  character(*), intent(in) :: var
  integer     , intent(in) :: tx, ty

  res = joined(DIR_PRD, trim(var)//'/'//tilename(tx,ty)//'.bin')
end function get_f_map_tile
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_map_tile_remapped(&
    varName, resolution_org, resolution_rmp, tx, ty) &
    result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_map_tile_remapped'
  character(*), intent(in) :: varName
  character(*), intent(in) :: resolution_org, resolution_rmp
  integer     , intent(in) :: tx, ty

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( varName )
  case( 'landuse' )
    dir = joined(DIR_PRD, varName)
  case default
    call errend(msg_invalid_value('varName', varName))
  endselect

  res = joined(dir, trim(dirname_resolution(resolution_org, resolution_rmp))//&
               '/'//tilename(tx,ty)//'.bin')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_map_tile_remapped
!===============================================================
!
!===============================================================
end module c2_nlni_io
