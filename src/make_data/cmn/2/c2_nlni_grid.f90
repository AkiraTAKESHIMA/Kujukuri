module c2_nlni_grid
  use lib_const
  use lib_base
  use lib_log
  use c2_nlni_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
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
  public :: gxs_of_lon
  public :: gxe_of_lon
  public :: gys_of_lat
  public :: gye_of_lat

  ! txy, xy <-> gxy
  public :: tx_of_gx
  public :: ty_of_gy
  public :: gxs_of_tx
  public :: gxe_of_tx
  public :: gys_of_ty
  public :: gye_of_ty
  public :: gx_of_x
  public :: gy_of_y

  ! iTile <-> txy
  public :: iTile_of_txy
  public :: iTile_to_txy
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function west_of_tx(tx) result(res)
  implicit none
  integer, intent(in) :: tx

  res = REGION_WEST + (tx-TXMIN)*TILESIZE_LON
end function west_of_tx
!===============================================================
!
!===============================================================
real(8) function east_of_tx(tx) result(res)
  implicit none
  integer, intent(in) :: tx

  res = REGION_WEST + (tx-TXMIN+1)*TILESIZE_LON
end function east_of_tx
!===============================================================
!
!===============================================================
real(8) function south_of_ty(ty) result(res)
  implicit none
  integer, intent(in) :: ty

  res = REGION_SOUTH + (ty-TYMIN)/3*2 + mod(ty-TYMIN,3)*TILESIZE_LAT
end function south_of_ty
!===============================================================
!
!===============================================================
real(8) function north_of_ty(ty) result(res)
  implicit none
  integer, intent(in) :: ty

  res = REGION_SOUTH + (ty-TYMIN+1)/3*2 + mod(ty-TYMIN+1,3)*TILESIZE_LAT
end function north_of_ty
!===============================================================
!
!===============================================================
integer function txs_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = (TXMIN-1) + floor((lon-REGION_WEST) / TILESIZE_LON) + 1
end function txs_of_lon
!===============================================================
!
!===============================================================
integer function txe_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = (TXMIN-1) + ceiling((lon-REGION_WEST) / TILESIZE_LON)
end function txe_of_lon
!===============================================================
!
!===============================================================
integer function tys_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = (TYMIN-1) + floor((lat-REGION_SOUTH) / TILESIZE_LAT) + 1
end function tys_of_lat
!===============================================================
!
!===============================================================
integer function tye_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = (TYMIN-1) + ceiling((lat-REGION_SOUTH) / TILESIZE_LAT)
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

  res = REGION_WEST + (gx-1)/NX*TILESIZE_LON + mod(gx-1,NX)*CELLSIZE_LON
end function west_of_gx
!===============================================================
!
!===============================================================
real(8) function east_of_gx(gx) result(res)
  implicit none
  integer, intent(in) :: gx

  res = REGION_WEST + gx/NX*TILESIZE_LON + mod(gx,NX)*CELLSIZE_LON
end function east_of_gx
!===============================================================
!
!===============================================================
real(8) function south_of_gy(gy) result(res)
  implicit none
  integer, intent(in) :: gy

  res = REGION_SOUTH + (gy-1)/NY*TILESIZE_LAT + mod(gy-1,NY)*CELLSIZE_LAT
end function south_of_gy
!===============================================================
!
!===============================================================
real(8) function north_of_gy(gy) result(res)
  implicit none
  integer, intent(in) :: gy

  res = REGION_SOUTH + gy/NY*TILESIZE_LAT + mod(gy,NY)*CELLSIZE_LAT
end function north_of_gy
!===============================================================
!
!===============================================================
integer function gxs_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = floor((lon - REGION_WEST) / CELLSIZE_LON) + 1
end function gxs_of_lon
!===============================================================
!
!===============================================================
integer function gxe_of_lon(lon) result(res)
  implicit none
  real(8), intent(in) :: lon

  res = ceiling((lon - REGION_WEST) / CELLSIZE_LON)
end function gxe_of_lon
!===============================================================
!
!===============================================================
integer function gys_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = floor((lat - REGION_SOUTH) / CELLSIZE_LAT) + 1
end function gys_of_lat
!===============================================================
!
!===============================================================
integer function gye_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

  res = ceiling((lat - REGION_SOUTH) / CELLSIZE_LAT)
end function gye_of_lat
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

  tx = (gx-1) / NX + TXMIN
end function tx_of_gx
!===============================================================
!
!===============================================================
integer function ty_of_gy(gy) result(ty)
  implicit none
  integer, intent(in) :: gy

  ty = (gy-1) / NY + TYMIN
end function ty_of_gy
!===============================================================
!
!===============================================================
integer function gxs_of_tx(tx) result(gxs)
  implicit none
  integer, intent(in) :: tx

  gxs = NX*(tx-TXMIN) + 1
end function gxs_of_tx
!===============================================================
!
!===============================================================
integer function gxe_of_tx(tx) result(gxe)
  implicit none
  integer, intent(in) :: tx

  gxe = NX*(tx-TXMIN+1)
end function gxe_of_tx
!===============================================================
!
!===============================================================
integer function gys_of_ty(ty) result(gys)
  implicit none
  integer, intent(in) :: ty

  gys = NY*(ty-TYMIN) + 1
end function gys_of_ty
!===============================================================
!
!===============================================================
integer function gye_of_ty(ty) result(gye)
  implicit none
  integer, intent(in) :: ty

  gye = NY*(ty-TYMIN+1)
end function gye_of_ty
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
integer function gx_of_x(tx, x) result(gx)
  implicit none
  integer, intent(in) :: tx, x

  gx = gxs_of_tx(tx) + x - 1
end function gx_of_x
!===============================================================
!
!===============================================================
integer function gy_of_y(ty, y) result(gy)
  implicit none
  integer, intent(in) :: ty, y

  gy = gys_of_ty(ty) + y - 1
end function gy_of_y
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
integer function iTile_of_txy(tx, ty) result(iTile)
  implicit none
  integer, intent(in) :: tx, ty

  iTile = NTX*((ty-TYMIN+1)-1) + (tx-TXMIN+1)
end function iTile_of_txy
!===============================================================
!
!===============================================================
subroutine iTile_to_txy(iTile, tx, ty)
  implicit none
  integer, intent(in) :: iTile
  integer, intent(out) :: tx, ty

  ty = (iTile-1) / NTX + TYMIN
  tx = iTile - NTX*(ty-TYMIN) + TXMIN - 1
end subroutine iTile_to_txy
!===============================================================
!
!===============================================================
end module c2_nlni_grid
