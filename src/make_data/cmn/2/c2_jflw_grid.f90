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
  public :: standard_dist_from_center

  public :: is_fdr_land
  public :: is_fdr_direction
  public :: is_fdr_outlet
  public :: get_nextxy
  public :: get_fdr
  public :: get_angle_fdr
  public :: all_sources_reach_outlet

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

  res = floor((REGION_NORTH-lat) / TILESIZE_LAT) + 1
end function tys_of_lat
!===============================================================
!
!===============================================================
integer function tye_of_lat(lat) result(res)
  implicit none
  real(8), intent(in) :: lat

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

  res = REGION_WEST + TILESIZE_LON*((gx-1)/MX) + GRIDSIZE_LON*mod(gx-1,MX)
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

  res = REGION_NORTH - TILESIZE_LAT*(gy/MY) - GRIDSIZE_LAT*mod(gy,MY)
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

  res = REGION_WEST + TILESIZE_LON*(gx/MX) &
        + GRIDSIZE_LON*(mod(gx,MX)-1+0.5d0)
end function center_of_gx
!===============================================================
!
!===============================================================
real(8) function center_of_gy(gy) result(res)
  implicit none
  integer, intent(in) :: gy

  res = REGION_NORTH - TILESIZE_LAT*(gy/MY) &
        - GRIDSIZE_LAT*(mod(gy,MY)-1+0.5d0)
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

  tx = (gx-1) / MX + 1
end function tx_of_gx
!===============================================================
!
!===============================================================
integer function ty_of_gy(gy) result(ty)
  implicit none
  integer, intent(in) :: gy

  ty = (gy-1) / MY + 1
end function ty_of_gy
!===============================================================
!
!===============================================================
integer function gxs_of_tx(tx) result(gxs)
  implicit none
  integer, intent(in) :: tx

  gxs = MX*(tx-1) + 1
end function gxs_of_tx
!===============================================================
!
!===============================================================
integer function gxe_of_tx(tx) result(gxe)
  implicit none
  integer, intent(in) :: tx

  gxe = MX*tx
end function gxe_of_tx
!===============================================================
!
!===============================================================
integer function gys_of_ty(ty) result(gys)
  implicit none
  integer, intent(in) :: ty

  gys = MY*(ty-1) + 1
end function gys_of_ty
!===============================================================
!
!===============================================================
integer function gye_of_ty(ty) result(gye)
  implicit none
  integer, intent(in) :: ty

  gye = MY*ty
end function gye_of_ty
!===============================================================
!
!===============================================================
integer function gx_of_x(tx, x) result(gx)
  implicit none
  integer, intent(in) :: tx, x

  gx = MX*(tx-1) + x
end function gx_of_x
!===============================================================
!
!===============================================================
integer function gy_of_y(ty, y) result(gy)
  implicit none
  integer, intent(in) :: ty, y

  gy = MY*(ty-1) + y
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
  x = gx - (tx-1)*MX
end subroutine gx_to_x
!===============================================================
!
!===============================================================
subroutine gy_to_y(gy, ty, y)
  implicit none
  integer, intent(in) :: gy
  integer, intent(out) :: ty, y

  ty = ty_of_gy(gy)
  y = gy - (ty-1)*MY
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
real(8) function standard_dist_from_center(gy) result(dist)
  implicit none
  integer, intent(in) :: gy

  real(8) :: clon, west, east
  real(8) :: clat, north, south
  real(8) :: d1, d2, d3

  north = north_of_gy(gy) * d2r
  south = south_of_gy(gy) * d2r
  clat = (south + north) * 0.5d0
  west = 0.d0 * d2r
  east = GRIDSIZE_LON * d2r
  clon = (west + east) * 0.5d0

  d1 = dist_ellips_hubeny(west, clat, east, clat, EARTH_E)
  d2 = dist_ellips_hubeny(clon, north, clon, south, EARTH_E)
  d3 = dist_ellips_hubeny(west, north, east, south, EARTH_E)
  dist = (d1 + d2 + d3*2) / 8
end function standard_dist_from_center
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
logical function is_fdr_land(fdr) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_fdr_land'
  integer(1), intent(in) :: fdr

  selectcase( fdr )
  case( 1_8:, &
        FDR__RIVERMOUTH, &
        FDR__INLAND )
    res = .true.
  case( FDR__MISS )
    res = .false.
  case default
    call errend(ERROR_CODE__VALUE, 'fdr == '//str(fdr), &
      MODNAM, PRCNAM)
  endselect
end function is_fdr_land
!===============================================================
!
!===============================================================
logical function is_fdr_direction(fdr) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_fdr_direction'
  integer(1), intent(in) :: fdr

  selectcase( fdr )
  case( 1_8: )
    res = .true.
  case( FDR__RIVERMOUTH, &
        FDR__INLAND, &
        FDR__MISS )
    res = .false.
  case default
    call errend(ERROR_CODE__VALUE, 'fdr == '//str(fdr), &
      MODNAM, PRCNAM)
  endselect
end function is_fdr_direction
!===============================================================
!
!===============================================================
logical function is_fdr_outlet(fdr) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_fdr_outlet'
  integer(1), intent(in) :: fdr

  selectcase( fdr )
  case( FDR__RIVERMOUTH, &
        FDR__INLAND )
    res = .true.
  case( 1_8:, &
        FDR__MISS )
    res = .false.
  case default
    call errend(ERROR_CODE__VALUE, 'fdr == '//str(fdr), &
      MODNAM, PRCNAM)
  endselect
end function is_fdr_outlet
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
  case( FDR__EAST )
    xx = ix + 1
    yy = iy
  case( FDR__SOUTHEAST )
    xx = ix + 1
    yy = iy + 1
  case( FDR__SOUTH )
    xx = ix
    yy = iy + 1
  case( FDR__SOUTHWEST )
    xx = ix - 1
    yy = iy + 1
  case( FDR__WEST )
    xx = ix - 1
    yy = iy
  case( FDR__NORTHWEST )
    xx = ix - 1
    yy = iy - 1
  case( FDR__NORTH )
    xx = ix
    yy = iy - 1
  case( FDR__NORTHEAST )
    xx = ix + 1
    yy = iy - 1
  case( FDR__RIVERMOUTH )
    xx = XY__RIVERMOUTH
    yy = XY__RIVERMOUTH
  case( FDR__INLAND )
    xx = XY__INLAND
    yy = XY__INLAND
  case( FDR__UNDEF )
    call errend(ERROR_CODE__CONDITION, 'Unexpected condition. Reached to ocean.', &
                MODNAM, PRCNAM)
  case default
    call errend(ERROR_CODE__VALUE, 'dir == '//str(dir), &
                MODNAM, PRCNAM)
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
      fdr = FDR__UNDEF
    case( 1 )
      fdr = FDR__SOUTH
    case( -1 )
      fdr = FDR__NORTH
    case default
      call errend(ERROR_CODE__CONDITION, &
        'abs(gyy-gy) > 1'//&
      '\n  gy: '//str(gy)//', gyy: '//str(gyy), &
        MODNAM, PRCNAM)
    endselect
  case( 1 )
    selectcase( gyy - gy )
    case( 0 )
      fdr = FDR__EAST
    case( 1 )
      fdr = FDR__SOUTHEAST
    case( -1 )
      fdr = FDR__NORTHEAST
    case default
      call errend(ERROR_CODE__CONDITION, &
        'abs(gyy-gy) > 1'//&
      '\n  gy: '//str(gy)//', gyy: '//str(gyy), &
        MODNAM, PRCNAM)
    endselect
  case( -1 )
    selectcase( gyy - gy )
    case( 0 )
      fdr = FDR__WEST
    case( 1 )
      fdr = FDR__SOUTHWEST
    case( -1 )
      fdr = FDR__NORTHWEST
    case default
      call errend(ERROR_CODE__CONDITION, &
        'abs(gyy-gy) > 1'//&
      '\n  gy: '//str(gy)//', gyy: '//str(gyy), &
        MODNAM, PRCNAM)
    endselect
  case default
    call errend(ERROR_CODE__CONDITION, &
      'abs(gxx-gx) > 1'//&
    '\n  gx: '//str(gx)//', gxx: '//str(gxx), &
      MODNAM, PRCNAM)
  endselect
end function get_fdr
!===============================================================
! Get angle on the lonlat plane
!===============================================================
real(8) function get_angle_fdr(fdr) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_angle_fdr'
  integer(1), intent(in) :: fdr

  selectcase( fdr )
  case( FDR__EAST )
    res = 0.d0
  case( FDR__NORTHEAST )
    res = 45.d0
  case( FDR__NORTH )
    res = 90.d0
  case( FDR__NORTHWEST )
    res = 135.d0
  case( FDR__WEST )
    res = 180.d0
  case( FDR__SOUTHWEST )
    res = 225.d0
  case( FDR__SOUTH )
    res = 270.d0
  case( FDR__SOUTHEAST )
    res = 315.d0
  case default
    call errend(ERROR_CODE__VALUE, &
      'Angle is not defined for fdr = '//str(fdr), &
      MODNAM, PRCNAM)
  endselect
end function get_angle_fdr
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
integer function all_sources_reach_outlet(&
  fdr, &
  un_report &
) result(res)
  use c2_jflw_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'all_sources_reach_outlet'
  integer(1), intent(in) :: fdr(:,:)
  integer, intent(in), optional :: un_report

  logical :: report

  integer(1), allocatable :: reach(:,:)
  logical(1), allocatable :: passed(:,:)
  integer :: nx, ny, ix, iy, x, y, xx, yy, x_, y_
  integer, pointer :: lst_x(:), lst_y(:)
  integer :: n, i
  integer :: nSource, iSource

  integer :: dgt_xy

  integer(1), parameter :: REACH__OUTLET = 1
  integer(1), parameter :: REACH__OCEAN = 2
  integer(1), parameter :: REACH__SOURCE = 11
  integer(1), parameter :: REACH__UNKNOWN = 12
  integer(1), parameter :: REACH__MISS = -9

  integer, parameter :: RES__SUCCESS = 0
  integer, parameter :: RES__OCEAN = 1
  integer, parameter :: RES__CYCLIC = 2

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  report = present(un_report)

  nx = size(fdr,1)
  ny = size(fdr,2)
  dgt_xy = dgt(max(nx,ny))
  allocate(reach(nx,ny))
  allocate(passed(nx,ny))

  reach(:,:) = REACH__SOURCE
  passed(:,:) = .false.

  do iy = 1, ny
  do ix = 1, nx
    if( .not. is_fdr_direction(fdr(ix,iy)) ) cycle
    call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)

    if( fdr(xx,yy) == FDR__MISS )then
      reach(ix,iy) = REACH__OCEAN

    ! do not overwrite status `ocean`
    elseif( reach(xx,yy) /= REACH__OCEAN )then
      reach(xx,yy) = REACH__UNKNOWN

    endif
  enddo
  enddo

  do iy = 1, ny
  do ix = 1, nx
    selectcase( fdr(ix,iy) )
    case( 1_8: )
      continue
    case( FDR__RIVERMOUTH, FDR__INLAND )
      reach(ix,iy) = REACH__OUTLET
    case( FDR__MISS )
      reach(ix,iy) = REACH__MISS
    case default
      call errend(ERROR_CODE__VALUE, 'fdr=='//str(fdr(ix,iy)))
    endselect
  enddo
  enddo

  nSource = count(reach == REACH__SOURCE)

  allocate(lst_x(nx+ny))
  allocate(lst_y(nx+ny))
  res = RES__SUCCESS

  iSource = 0
  loop_outer:&
  do iy = 1, ny
  do ix = 1, nx
    if( reach(ix,iy) /= REACH__SOURCE ) cycle

    call add(iSource)
    !call logmsg('source '//str(iSource)//' ('//str((/ix,iy/),dgt_xy,',')//')')

    x = ix
    y = iy
    n = 0
    do 
      passed(x,y) = .true.

      call get_nextxy(x, y, fdr(x,y), xx, yy)
      x = xx
      y = yy
      !call logmsg('  -> ('//str((/x,y/),dgt_xy,',')//') reach '//str(reach(x,y)))

      if( n == size(lst_x) )then
        call realloc(lst_x, n*2, clear=.false.)
        call realloc(lst_y, n*2, clear=.false.)
      endif
      call add(n)
      lst_x(n) = x
      lst_y(n) = y

      if( passed(x,y) )then
        res = RES__CYCLIC
        if( report )then
          write(un_report,"(a)") 'source #'//str(iSource)//&
            ' cyclic '
        endif
        exit
      endif

      selectcase( reach(x,y) )
      !---------------------------------------------------------
      ! Case:  Reaches outlet
      case( REACH__OUTLET )
        exit
      !---------------------------------------------------------
      ! Case: Directly flows into ocean
      case( REACH__OCEAN )
        res = RES__OCEAN
        if( report )then
          x_ = lst_x(n-1)
          y_ = lst_y(n-1)
          call logmsg('source #'//str(iSource)//' directly flows into ocean.'//&
          '\n('//str((/x_,y_/),',')//') fdr: '//str(fdr(x_,y_))//' reach: '//str(reach(x_,y_))//&
          '\n('//str((/x,y/),',')//') fdr: '//str(fdr(x,y))//' reach: '//str(reach(x,y)), &
            un_report)
        endif
        exit
      !---------------------------------------------------------
      ! Case: Ongoing
      case( REACH__UNKNOWN )
        continue
      !---------------------------------------------------------
      ! Case: ERROR (miss, source)
      case( REACH__MISS, &
            REACH__SOURCE )
        call errnew(ERROR_CODE__CONDITION, 'source #'//str(iSource)//&
          ' ('//str((/ix,iy/),',')//') stopped at ('//&
          str((/x,y/),',')//') reach: '//str(reach(x,y)))
        if( n > 1 )then
          call erradd('up ('//str((/lst_x(n-1),lst_y(n-1)/),',')//&
            ') fdr: '//str(fdr(lst_x(n-1),lst_y(n-1)))//&
            ' reach: '//str(reach(lst_x(n-1),lst_y(n-1))))
        endif
        call errend()
      !---------------------------------------------------------
      ! Case: ERROR
      case default
        call errend(ERROR_CODE__VALUE, 'reach=='//str(reach(x,y)))
      endselect
    enddo

    if( res /= RES__SUCCESS )then
      exit loop_outer
    endif

    do i = 1, n
      reach(lst_x(i),lst_y(i)) = reach(x,y)
      passed(lst_x(i),lst_y(i)) = .false.
    enddo
  enddo  ! ix
  enddo& ! iy
  loop_outer
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function all_sources_reach_outlet
!===============================================================
!
!===============================================================
integer function all_sources_reach_outlet_(&
  fdr, &
  un_report &
) result(res)
  use c2_jflw_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'all_sources_reach_outlet'
  integer(1), intent(in) :: fdr(:,:)
  integer, intent(in), optional :: un_report

  logical :: report
  integer(1), allocatable :: grid(:,:)
  logical(1), allocatable :: passed(:,:)
  integer :: nx, ny, ix, iy, x, y, xx, yy
  integer :: nSource, iSource

  integer :: dgt_xy

  integer(1), parameter :: GRID__SOURCE  = 1
  integer(1), parameter :: GRID__REACH   = 2
  integer(1), parameter :: GRID__UNKNOWN = 3
  integer(1), parameter :: GRID__OCEAN   = -9

  integer, parameter :: RES__SUCCESS     = 0
  integer, parameter :: RES__OCEAN       = 1
  integer, parameter :: RES__CIRCULATION = 2
  integer, parameter :: RES__UNKNOWN     = 3

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  report = present(un_report)

  res = .true.

  nx = size(fdr,1)
  ny = size(fdr,2)
  dgt_xy = dgt(max(nx,ny))

  allocate(grid(nx,ny))
  allocate(passed(nx,ny))

  grid(:,:) =  GRID__SOURCE
  do iy = 1, ny
  do ix = 1, nx
    selectcase( fdr(ix,iy) )
    case( 1_8: )
      call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
      grid(xx,yy) = GRID__UNKNOWN
    case( FDR__MISS )
      grid(ix,iy) = GRID__OCEAN
      cycle
    case( FDR__RIVERMOUTH, FDR__INLAND )
      grid(ix,iy) = GRID__REACH
    case default
      call errend(ERROR_CODE__VALUE, 'fdr=='//str(fdr(ix,iy)))
    endselect
  enddo
  enddo

  nSource = 0
  do iy = 1, ny
  do ix = 1, nx
    if( is_fdr_outlet(fdr(ix,iy)) )then
      grid(ix,iy) = GRID__REACH
    endif
    if( grid(ix,iy) == GRID__SOURCE ) call add(nSource)
  enddo
  enddo

  iSource = 0
  res = RES__SUCCESS
  do iy = 1, ny
  do ix = 1, nx
    if( grid(ix,iy) /= GRID__SOURCE ) cycle
    call add(iSource)
if( iSource /= 9854115 ) cycle
    call logmsg('source '//str(iSource)//' / '//str(nSource)//&
      ' ('//str((/ix,iy/),dgt_xy,',')//')')

    x = ix
    y = iy
    do while( grid(x,y) /= GRID__REACH )
      passed(x,y) = .true.
      if( .not. is_fdr_direction(fdr(x,y)) ) exit
      call get_nextxy(x, y, fdr(x,y), xx, yy)
      if( passed(xx,yy) )then
        res = RES__CIRCULATION
        if( report )then
          write(un_report,"(a)") 'Source #'//str(iSource)//&
            ': circulation was detected.'
        endif
        call finalize()
        return
      endif
      x = xx
      y = yy
    enddo

    if( grid(x,y) == GRID__REACH )then
      x = ix
      y = iy
      do while( grid(x,y) /= GRID__REACH )
        grid(x,y) = GRID__REACH
        passed(x,y) = .false.
        call get_nextxy(x, y, fdr(x,y), xx, yy)
        x = xx
        y = yy
      enddo
    elseif( fdr(x,y) == FDR__MISS )then
      res = RES__OCEAN
      if( report )then
        write(un_report,"(a)") 'Source #'//str(iSource)//&
          ': directly reached ocean'
      endif
      call finalize()
      return
    else
      res = RES__UNKNOWN
      if( report )then
        write(un_report,"(a)") 'Source #'//str(iSource)//&
          ': unknown error. (x,y)=('//str((/x,y/),',')//&
          ') fdr='//str(fdr(x,y))//' grid='//str(grid(x,y))
      endif
      call finalize()
      return
    endif

if( iSource == 1 )then
  if( any(passed) )then
    print*, 'passed'
    stop
  endif
endif
  enddo  ! ix
  enddo  ! iy

  call finalize()
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine finalize()
  implicit none

  deallocate(grid)
  deallocate(passed)
  call logret(PRCNAM, MODNAM)
end subroutine 
!---------------------------------------------------------------
end function all_sources_reach_outlet_
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
  use lib_math, &
    dist_sphere => dist_ellips_hubeny
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_lineleng_in_pixels'
  real(8), intent(in) :: lon1, lat1, lon2, lat2
  integer, intent(out) :: n
  integer, pointer :: lst_gx(:), lst_gy(:) ! out
  real(8), pointer :: lst_leng(:)  ! out

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
  nn = (abs(gye-gys)+1) * (gxe-gxs+1)
  allocate(lst_gx(nn))
  allocate(lst_gy(nn))
  allocate(lst_leng(nn))

  n = 0

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
