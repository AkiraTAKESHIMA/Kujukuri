module mod_mesh
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  ! lonlat <-> xy
  public :: west_of_x
  public :: east_of_x
  public :: south_of_y
  public :: north_of_y
  public :: lon_center_of_x
  public :: lat_center_of_y
  public :: xs_of_lon
  public :: xe_of_lon
  public :: ys_of_lat
  public :: ye_of_lat

  ! intersection
  public :: apprx_isct_with_meridian
  public :: apprx_isct_with_parallel
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_mesh'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function west_of_x(grid, x) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  integer, intent(in) :: x

  res = grid%west + grid%cellsize_lon * (x-1)
end function west_of_x
!===============================================================
!
!===============================================================
real(8) function east_of_x(grid, x) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  integer, intent(in) :: x

  res = grid%west + grid%cellsize_lon * x
end function east_of_x
!===============================================================
!
!===============================================================
real(8) function south_of_y(grid, y) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  integer, intent(in) :: y

  res = grid%north - grid%cellsize_lat * y
end function south_of_y
!===============================================================
!
!===============================================================
real(8) function north_of_y(grid, y) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  integer, intent(in) :: y

  res = grid%north - grid%cellsize_lat * (y-1)
end function north_of_y
!===============================================================
!
!===============================================================
real(8) function lon_center_of_x(grid, x) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  integer, intent(in) :: x

  res = grid%west + grid%cellsize_lon * (x-0.5d0)
end function lon_center_of_x
!===============================================================
!
!===============================================================
real(8) function lat_center_of_y(grid, y) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  integer, intent(in) :: y

  res = grid%north - grid%cellsize_lat * (y-0.5d0)
end function lat_center_of_y
!===============================================================
!
!===============================================================
integer function xs_of_lon(grid, lon) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  real(8), intent(in) :: lon

  res = floor((lon - grid%west) / grid%cellsize_lon) + 1
end function xs_of_lon
!===============================================================
!
!===============================================================
integer function xe_of_lon(grid, lon) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  real(8), intent(in) :: lon

  res = ceiling((lon - grid%west) / grid%cellsize_lon)
end function xe_of_lon
!===============================================================
!
!===============================================================
integer function ys_of_lat(grid, lat) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  real(8), intent(in) :: lat

  res = floor((grid%north - lat) / grid%cellsize_lat) + 1
end function ys_of_lat
!===============================================================
!
!===============================================================
integer function ye_of_lat(grid, lat) result(res)
  implicit none
  type(grid_), intent(in) :: grid
  real(8), intent(in) :: lat

  res = ceiling((grid%north - lat) / grid%cellsize_lat)
end function ye_of_lat
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
real(8) function apprx_isct_with_meridian(&
    lon0, lat0, lon1, lat1, lon) result(lat)
  implicit none
  real(8), intent(in) :: lon0, lat0, lon1, lat1
  real(8), intent(in) :: lon

  if( lon == lon0 )then
    lat = lat0
  else
    lat = (lat1-lat0)*((lon-lon0)/(lon1-lon0)) + lat0
  endif
end function apprx_isct_with_meridian
!===============================================================
!
!===============================================================
real(8) function apprx_isct_with_parallel(&
    lon0, lat0, lon1, lat1, lat) result(lon)
  implicit none
  real(8), intent(in) :: lon0, lat0, lon1, lat1
  real(8), intent(in) :: lat

  if( lat == lat0 )then
    lon = lon0
  else
    lon = (lon1-lon0)*((lat-lat0)/(lat1-lat0)) + lon0
  endif
end function apprx_isct_with_parallel
!===============================================================
!
!===============================================================
end module mod_mesh
