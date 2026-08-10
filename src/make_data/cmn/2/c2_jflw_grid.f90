module c2_jflw_grid
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use c2_jflw_const
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! lonlat <-> txy
  public :: west_of_tx
  public :: east_of_tx
  public :: south_of_ty
  public :: north_of_ty
  public :: txs_of_lon
  public :: txe_of_lon
  public :: tys_of_lat
  public :: tye_of_lat

  ! lonlat <-> gxy
  public :: west_of_gx
  public :: east_of_gx
  public :: south_of_gy
  public :: north_of_gy
  public :: center_of_gx
  public :: center_of_gy
  public :: gxs_of_lon
  public :: gxe_of_lon
  public :: gys_of_lat
  public :: gye_of_lat
  public :: lonlat_to_gxy
  public :: lonlat_to_xy

  ! txy, xy <-> gxy
  public :: tx_of_gx
  public :: ty_of_gy
  public :: gxs_of_tx
  public :: gxe_of_tx
  public :: gys_of_ty
  public :: gye_of_ty
  public :: gx_of_x
  public :: gy_of_y
  public :: xy_to_gxy
  public :: gx_to_x
  public :: gy_to_y
  public :: gxy_to_xy

  ! Others
  public :: mean_dist_from_center

  public :: get_nextxy
  public :: get_fdr

  public :: calc_lineleng_in_pixels
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_jflw_grid'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function west_of_tx(tx) result(res)
  implicit none
  integer, intent(in) :: tx

  res = REGION_WEST + (tx-1)*TILESIZE_LON
end function west_of_tx
!===============================================================
!
!===============================================================
real(8) function east_of_tx(tx) result(res)
  implicit none
  integer, intent(in) :: tx

  res = west_of_tx(tx+1)
end function east_of_tx
!===============================================================
!
!===============================================================
real(8) function south_of_ty(ty) result(res)
  implicit none
  integer, intent(in) :: ty

  res = REGION_NORTH - ty*TILESIZE_LAT
end function south_of_ty
!===============================================================
!
!===============================================================
real(8) function north_of_ty(ty) result(res)
  implicit none
  integer, intent(in) :: ty

  res = south_of_ty(ty-1)
end function north_of_ty
!===============================================================
!
!===============================================================
integer function txs_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = floor((lon-REGION_WEST) / TILESIZE_LON) + 1
end function txs_of_lon
!===============================================================
!
!===============================================================
integer function txe_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = ceiling((lon-REGION_WEST) / TILESIZE_LON)
end function txe_of_lon
!===============================================================
!
!===============================================================
integer function tys_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  !res = floor((lat-REGION_SOUTH) / TILESIZE_LAT) + 1
  res = floor((REGION_NORTH-lat) / TILESIZE_LAT) + 1
end function tys_of_lat
!===============================================================
!
!===============================================================
integer function tye_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  !res = ceiling((lat-REGION_SOUTH) / TILESIZE_LAT)
  res = ceiling((REGION_NORTH-lat) / TILESIZE_LAT)
end function tye_of_lat
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
real(8) function west_of_gx(gx) result(res)
  implicit none
  integer, intent(in) :: gx

  res = REGION_WEST + TILESIZE_LON*((gx-1)/NX) + GRIDSIZE_LON*mod(gx-1,NX)
end function west_of_gx
!===============================================================
!
!===============================================================
real(8) function east_of_gx(gx) result(res)
  implicit none
  integer, intent(in) :: gx

  res = west_of_gx(gx+1)
end function east_of_gx
!===============================================================
!
!===============================================================
real(8) function south_of_gy(gy) result(res)
  implicit none
  integer, intent(in) :: gy

  res = REGION_NORTH - TILESIZE_LAT*(gy/NY) - GRIDSIZE_LAT*mod(gy,NY)
end function south_of_gy
!===============================================================
!
!===============================================================
real(8) function north_of_gy(gy) result(res)
  implicit none
  integer, intent(in) :: gy

  res = south_of_gy(gy-1)
end function north_of_gy
!===============================================================
!
!===============================================================
real(8) function center_of_gx(gx) result(res)
  implicit none
  integer, intent(in) :: gx

  res = REGION_WEST + TILESIZE_LON*(gx/NX) &
        + GRIDSIZE_LON*(mod(gx,NX)-1+0.5d0)
end function center_of_gx
!===============================================================
!
!===============================================================
real(8) function center_of_gy(gy) result(res)
  implicit none
  integer, intent(in) :: gy

  res = REGION_NORTH - TILESIZE_LAT*(gy/NY) &
        - GRIDSIZE_LAT*(mod(gy,NY)-1+0.5d0)
end function center_of_gy
!===============================================================
!
!===============================================================
integer function gxs_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = floor((lon-REGION_WEST) / GRIDSIZE_LON) + 1
end function gxs_of_lon
!===============================================================
!
!===============================================================
integer function gxe_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = ceiling((lon-REGION_WEST) / GRIDSIZE_LON)
end function gxe_of_lon
!===============================================================
!
!===============================================================
integer function gys_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = floor((REGION_NORTH-lat) / GRIDSIZE_LAT) + 1
end function gys_of_lat
!===============================================================
!
!===============================================================
integer function gye_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = ceiling((REGION_NORTH-lat) / GRIDSIZE_LAT)
end function gye_of_lat
!===============================================================
!
!===============================================================
subroutine lonlat_to_gxy(lon, lat, gx, gy)
  implicit none
  real(8), intent(in) :: lon, lat
  integer, intent(out) :: gx, gy

  gx = int((lon - REGION_WEST) / GRIDSIZE_LON) + 1
  gy = int((REGION_NORTH - lat) / GRIDSIZE_LAT) + 1
end subroutine lonlat_to_gxy
!===============================================================
!
!===============================================================
subroutine lonlat_to_xy(lon, lat, tx, ty, x, y)
  implicit none
  real(8), intent(in) :: lon, lat
  integer, intent(out) :: tx, ty, x, y

  tx = int((lon-REGION_WEST) / TILESIZE_LON) + 1
  ty = int((REGION_NORTH-lat) / TILESIZE_LAT) + 1
  x = int((lon - (REGION_WEST+TILESIZE_LON*(tx-1))) / GRIDSIZE_LON) + 1
  y = int(((REGION_NORTH-TILESIZE_LAT*(ty-1))-lat) / GRIDSIZE_LAT) + 1
end subroutine lonlat_to_xy
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
integer function tx_of_gx(gx) result(tx)
  implicit none
  integer, intent(in) :: gx

  tx = (gx-1) / NX + 1
end function tx_of_gx
!===============================================================
!
!===============================================================
integer function ty_of_gy(gy) result(ty)
  implicit none
  integer, intent(in) :: gy

  ty = (gy-1) / NY + 1
end function ty_of_gy
!===============================================================
!
!===============================================================
integer function gxs_of_tx(tx) result(gxs)
  implicit none
  integer, intent(in) :: tx

  gxs = NX*(tx-1) + 1
end function gxs_of_tx
!===============================================================
!
!===============================================================
integer function gxe_of_tx(tx) result(gxe)
  implicit none
  integer, intent(in) :: tx

  gxe = NX*tx
end function gxe_of_tx
!===============================================================
!
!===============================================================
integer function gys_of_ty(ty) result(gys)
  implicit none
  integer, intent(in) :: ty

  gys = NY*(ty-1) + 1
end function gys_of_ty
!===============================================================
!
!===============================================================
integer function gye_of_ty(ty) result(gye)
  implicit none
  integer, intent(in) :: ty

  gye = NY*ty
end function gye_of_ty
!===============================================================
!
!===============================================================
integer function gx_of_x(tx, x) result(gx)
  implicit none
  integer, intent(in) :: tx, x

  gx = NX*(tx-1) + x
end function gx_of_x
!===============================================================
!
!===============================================================
integer function gy_of_y(ty, y) result(gy)
  implicit none
  integer, intent(in) :: ty, y

  gy = NY*(ty-1) + y
end function gy_of_y
!===============================================================
!
!===============================================================
subroutine xy_to_gxy(tx, ty, x, y, gx, gy)
  implicit none
  integer, intent(in) :: tx, ty, x, y
  integer, intent(out) :: gx, gy

  gx = gx_of_x(tx, x)
  gy = gy_of_y(ty, y)
end subroutine xy_to_gxy
!===============================================================
!
!===============================================================
subroutine gx_to_x(gx, tx, x)
  implicit none
  integer, intent(in) :: gx
  integer, intent(out) :: tx, x

  tx = tx_of_gx(gx)
  x = gx - (tx-1)*NX
end subroutine gx_to_x
!===============================================================
!
!===============================================================
subroutine gy_to_y(gy, ty, y)
  implicit none
  integer, intent(in) :: gy
  integer, intent(out) :: ty, y

  ty = ty_of_gy(gy)
  y = gy - (ty-1)*NY
end subroutine gy_to_y
!===============================================================
!
!===============================================================
subroutine gxy_to_xy(gx, gy, tx, x, ty, y)
  implicit none
  integer, intent(in) :: gx, gy
  integer, intent(out) :: tx, x, ty, y

  call gx_to_x(gx, tx, x)
  call gy_to_y(gy, ty, y)
end subroutine gxy_to_xy
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
real(8) function mean_dist_from_center(gx, gy) result(d)
  implicit none
  integer, intent(in) :: gx, gy

  real(8) :: rwest, reast, rsouth, rnorth
  real(8) :: rclon, rclat

  rwest  = west_of_gx(gx) * d2r
  reast  = east_of_gx(gx) * d2r
  rnorth = south_of_gy(gy) * d2r
  rsouth = north_of_gy(gy) * d2r
  rclon  = (rwest + reast)*0.5d0
  rclat  = (rsouth + rnorth)*0.5d0
  d = &
    (dist_sphere(rclon, rclat, rwest, rnorth) + &
     dist_sphere(rclon, rclat, rclon, rnorth) + &
     dist_sphere(rclon, rclat, reast, rnorth) + &
     dist_sphere(rclon, rclat, reast, rclat ) + &
     dist_sphere(rclon, rclat, reast, rsouth) + &
     dist_sphere(rclon, rclat, rclon, rsouth) + &
     dist_sphere(rclon, rclat, rwest, rsouth) + &
     dist_sphere(rclon, rclat, rwest, rclat )) / 8.d0
end function mean_dist_from_center
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
subroutine get_nextxy(ix, iy, dir, xx, yy)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_nextxy'
  integer   , intent(in)  :: ix, iy
  integer(1), intent(in)  :: dir
  integer   , intent(out) :: xx, yy

  selectcase( dir )
  case( FDR_EAST )
    xx = ix + 1
    yy = iy
  case( FDR_SOUTHEAST )
    xx = ix + 1
    yy = iy + 1
  case( FDR_SOUTH )
    xx = ix
    yy = iy + 1
  case( FDR_SOUTHWEST )
    xx = ix - 1
    yy = iy + 1
  case( FDR_WEST )
    xx = ix - 1
    yy = iy
  case( FDR_NORTHWEST )
    xx = ix - 1
    yy = iy - 1
  case( FDR_NORTH )
    xx = ix
    yy = iy - 1
  case( FDR_NORTHEAST )
    xx = ix + 1
    yy = iy - 1
  case( FDR_RIVERMOUTH )
    xx = XY_RIVERMOUTH
    yy = XY_RIVERMOUTH
  case( FDR_INLAND )
    xx = XY_INLAND
    yy = XY_INLAND
  case( FDR_UNDEF )
    call errend('Unexpected condition. Reached to ocean.', &
                '', PRCNAM, MODNAM)
  case default
    call errend(msg_invalid_value('dir', dir), &
                '', PRCNAM, MODNAM)
  endselect
end subroutine get_nextxy
!===============================================================
!
!===============================================================
integer(1) function get_fdr(gx, gy, gxx, gyy) result(fdr)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_fdr'
  integer, intent(in) :: gx, gy, gxx, gyy

  selectcase( gxx - gx )
  case( 0 )
    selectcase( gyy - gy )
    case( 0 )
      fdr = FDR_UNDEF
    case( 1 )
      fdr = FDR_SOUTH
    case( -1 )
      fdr = FDR_NORTH
    case default
      call errend(msg_unexpected_condition()//&
                '\n  abs(gyy-gy) > 1'//&
                '\n  gy: '//str(gy)//', gyy: '//str(gyy), &
                  '', PRCNAM, MODNAM)
    endselect
  case( 1 )
    selectcase( gyy - gy )
    case( 0 )
      fdr = FDR_UNDEF
    case( 1 )
      fdr = FDR_SOUTHWEST
    case( -1 )
      fdr = FDR_NORTHWEST
    case default
      call errend(msg_unexpected_condition()//&
                '\n  abs(gyy-gy) > 1'//&
                '\n  gy: '//str(gy)//', gyy: '//str(gyy), &
                  '', PRCNAM, MODNAM)
    endselect
  case( -1 )
    selectcase( gyy - gy )
    case( 0 )
      fdr = FDR_UNDEF
    case( 1 )
      fdr = FDR_SOUTHEAST
    case( -1 )
      fdr = FDR_NORTHEAST
    case default
      call errend(msg_unexpected_condition()//&
                '\n  abs(gyy-gy) > 1'//&
                '\n  gy: '//str(gy)//', gyy: '//str(gyy), &
                  '', PRCNAM, MODNAM)
    endselect
  case default
    call errend(msg_unexpected_condition()//&
              '\n  abs(gxx-gx) > 1'//&
              '\n  gx: '//str(gx)//', gxx: '//str(gxx), &
                '', PRCNAM, MODNAM)
  endselect
end function get_fdr
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
subroutine calc_lineleng_in_pixels(&
    lon1, lat1, lon2, lat2, &
    n, lst_gx, lst_gy, lst_leng)
  use c1_grid, only: &
        apprx_isct_with_meridian, &
        apprx_isct_with_parallel
  use c2_jflw_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_lineleng_in_pixels'
  real(8), intent(in) :: lon1, lat1, lon2, lat2
  integer, pointer :: lst_gx(:), lst_gy(:) ! out
  real(8), pointer :: lst_leng(:)  ! out
  integer, intent(out) :: n

  real(8) :: wlon, wlat, elon, elat
  integer :: gxs, gxe, gys, gye, igx, igy
  integer :: sgn_gy
  real(8) :: clon_west, clat_west, clon_east, clat_east
  real(8) :: dlon_west, dlat_west, dlon_east, dlat_east
  integer :: nn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( lon1 < lon2 )then
    wlon = lon1
    wlat = lat1
    elon = lon2
    elat = lat2
  else
    wlon = lon2
    wlat = lat2
    elon = lon1
    elat = lat1
  endif

  gxs = gxs_of_lon(wlon)
  gxe = gxe_of_lon(elon)

  gys = gys_of_lat(wlat)
  gye = gye_of_lat(elat)
  if( wlat == elat ) gye = gys
  sgn_gy = int(sign(1.d0, wlat-elat))

  !call logmsg('('//str((/lon1,lat1/),'f12.7',',')//') - ('//str((/lon2,lat2/),'f12.7',',')//')')
  !call logmsg('('//str((/wlon,wlat/),'f12.7',',')//') - ('//str((/elon,elat/),'f12.7',',')//')')
  !call logmsg('gx: '//str((/gxs,gxe/),DGT_GXY,' - ')//', gy: '//str((/gys,gye/),DGT_GXY,' - ')//&
  !    ' (sgn_y: '//str(sgn_gy)//')')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n = 0
  call realloc(lst_gx, abs(gye-gys)+1, clear=.true.)
  call realloc(lst_gy, abs(gye-gys)+1, clear=.true.)
  call realloc(lst_leng, abs(gye-gys)+1, clear=.true.)

  do igy = gys, gye, sgn_gy
    if( igy == gys )then
      clat_west = wlat
      clon_west = wlon
    else
      clat_west = clat_east
      clon_west = clon_east
    endif
    if( igy == gye )then
      clat_east = elat
      clon_east = elon
    else
      if( sgn_gy == 1 )then
        clat_east = south_of_gy(igy)
      else
        clat_east = north_of_gy(igy)
      endif
      clon_east = apprx_isct_with_parallel(&
          wlon, wlat, elon, elat, clat_east)
    endif

    gxs = gxs_of_lon(clon_west)
    gxe = gxe_of_lon(clon_east)
    if( clon_west == clon_east ) gxe = gxs

    nn = n + (gxe - gxs + 1)
    if( nn > size(lst_gx) )then
      call realloc(lst_gx, nn*2, clear=.false.)
      call realloc(lst_gy, nn*2, clear=.false.)
      call realloc(lst_leng, nn*2, clear=.false.)
    endif

    do igx = gxs, gxe
      if( igx == gxs )then
        dlon_west = clon_west
        dlat_west = clat_west
      else
        dlon_west = dlon_east
        dlat_west = dlat_east
      endif
      if( igx == gxe )then
        dlon_east = clon_east
        dlat_east = clat_east
      else
        dlon_east = east_of_gx(igx)
        !call traperr( intersection_sphere_normal_meridian(&
        !       wlon*d2r, wlat*d2r, elon*d2r, elat*d2r, dlon_east*d2r, dlat_east) )
        !dlat_east = dlat_east * r2d
        dlat_east = apprx_isct_with_meridian(&
            wlon, wlat, elon, elat, dlon_east)
      endif

      call add(n)
      lst_gx(n) = igx
      lst_gy(n) = igy
      lst_leng(n) = dist_sphere(wlon*d2r, wlat*d2r, elon*d2r, elat*d2r)
    enddo  ! igx/
  enddo  ! igy/

  call realloc(lst_gx, n, clear=.false.)
  call realloc(lst_gy, n, clear=.false.)
  call realloc(lst_leng, n, clear=.false.)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calc_lineleng_in_pixels
!===============================================================
!
!===============================================================
end module c2_jflw_grid
