module lib_math_sphere
  use lib_const
  use lib_base
  use lib_log
  use lib_util, only: &
    str_arctyp_long, &
    str_arc_rel_lat, &
    str_convex_long
  use lib_math_linalg_util, only: &
    calc_cross_product
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_modvar_lib_math_sphere

  public :: which_is_western
  public :: western
  public :: eastern

  public :: londiff_rad
  public :: londiff_deg

  public :: dir_lon

  public :: bboxes_intersect
  public :: included_in_bbox

  public :: spherical_to_cartesian_rad
  public :: spherical_to_cartesian_deg
  public :: cartesian_to_spherical_rad
  public :: cartesian_to_spherical_deg

  public :: calc_coefs_large_arc

  public :: intersection_sphere_normal_normal
  public :: intersection_sphere_normal_meridian
  public :: intersection_sphere_normal_parallel1
  public :: intersection_sphere_normal_parallel2
  public :: intersection_sphere_normal_parallel3

  public :: calc_lat_range_large_arc

  public :: calc_lon_range_shared

  public :: area_sphere_rect
  public :: area_ellips_rect

  public :: area_sphere_polarrect

  public :: area_sphere_tri

  public :: area_sphere_righttri_south_bottom
  public :: area_sphere_righttri_north_bottom

  public :: area_sphere_polartri

  public :: area_sphere_polygon

  public :: area_sphere_intersection_polygon_polygon
  public :: area_sphere_intersection_latlon_polygon

  public :: dist_sphere
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface londiff_rad
    module procedure londiff_rad__0d
    module procedure londiff_rad__1d
  end interface

  interface londiff_deg
    module procedure londiff_deg__0d
    module procedure londiff_deg__1d
  end interface

  interface spherical_to_cartesian_rad
    module procedure spherical_to_cartesian_rad__0d_miss_unspecified
    module procedure spherical_to_cartesian_rad__0d_miss_specified
    module procedure spherical_to_cartesian_rad__1d_miss_unspecified
    module procedure spherical_to_cartesian_rad__1d_miss_specified
    module procedure spherical_to_cartesian_rad__2d_miss_unspecified
    module procedure spherical_to_cartesian_rad__2d_miss_specified
  end interface

  interface spherical_to_cartesian_deg
    module procedure spherical_to_cartesian_deg__0d_miss_unspecified
    module procedure spherical_to_cartesian_deg__0d_miss_specified
    module procedure spherical_to_cartesian_deg__1d_miss_unspecified
    module procedure spherical_to_cartesian_deg__1d_miss_specified
    module procedure spherical_to_cartesian_deg__2d_miss_unspecified
    module procedure spherical_to_cartesian_deg__2d_miss_specified
  end interface

  interface cartesian_to_spherical_rad
    module procedure cartesian_to_spherical_rad__0d_miss_unspecified
    module procedure cartesian_to_spherical_rad__0d_miss_specified
    module procedure cartesian_to_spherical_rad__1d_miss_unspecified
    module procedure cartesian_to_spherical_rad__1d_miss_specified
    module procedure cartesian_to_spherical_rad__2d_miss_unspecified
    module procedure cartesian_to_spherical_rad__2d_miss_specified
  end interface

  interface cartesian_to_spherical_deg
    module procedure cartesian_to_spherical_deg__0d_miss_unspecified
    module procedure cartesian_to_spherical_deg__0d_miss_specified
    module procedure cartesian_to_spherical_deg__1d_miss_unspecified
    module procedure cartesian_to_spherical_deg__1d_miss_specified
    module procedure cartesian_to_spherical_deg__2d_miss_unspecified
    module procedure cartesian_to_spherical_deg__2d_miss_specified
  end interface

  interface intersection_sphere_normal_normal
    module procedure intersection_sphere_normal_normal_confirmed
  end interface

  interface intersection_sphere_normal_meridian
    module procedure intersection_sphere_normal_meridian_0d
    module procedure intersection_sphere_normal_meridian_1d
  end interface

  interface calc_coefs_large_arc
    module procedure calc_coefs_large_arc_spherical
    module procedure calc_coefs_large_arc_cartesian
  end interface

  interface area_sphere_rect
    module procedure area_sphere_rect_lat0d
    module procedure area_sphere_rect_lat1d
  end interface

  interface area_ellips_rect
    module procedure area_ellips_rect_lat0d
    module procedure area_ellips_rect_lat1d
  end interface

  interface area_sphere_polartri
    module procedure area_sphere_polartri_spherical
  end interface

  interface area_sphere_polygon
    module procedure area_sphere_polygon__spherical
    module procedure area_sphere_polygon__cartesian
  end interface

  interface dist_sphere
    module procedure dist_sphere_0d
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_math_sphere'

  logical, save :: debug = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_modvar_lib_math_sphere(debug)
  implicit none
  logical, intent(in), optional :: debug

  if( present(debug) ) call set_var_debug(debug)
end subroutine set_modvar_lib_math_sphere
!===============================================================
!
!===============================================================
subroutine set_var_debug(val)
  implicit none
  logical, intent(in) :: val

  debug = val
end subroutine set_var_debug
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Return
!  0 when lon1 == lon2,
!  1 when lon1 is western of lon2,
!  2 when lon2 is western of lon1
!===============================================================
integer function which_is_western(lon1, lon2) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'which_is_western'
  real(8), intent(in) :: lon1, lon2

  if( lon1 == lon2 )then
    res = 0
  else
    if( abs(lon1 - lon2) < rad_180deg .eqv. lon1 < lon2 )then
      res = 1
    else
      res = 2
    endif
  endif
end function which_is_western
!===============================================================
!
!===============================================================
real(8) function western(lon1, lon2)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'western'
  real(8), intent(in) :: lon1, lon2

  if( abs(lon1-lon2) < rad_180deg )then
    western = min(lon1, lon2)
  else
    western = max(lon1, lon2)
  endif
end function western
!===============================================================
!
!===============================================================
real(8) function eastern(lon1, lon2)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'eastern'
  real(8), intent(in) :: lon1, lon2

  if( abs(lon1-lon2) < rad_180deg )then
    eastern = max(lon1, lon2)
  else
    eastern = min(lon1, lon2)
  endif
end function eastern
!===============================================================
!
!===============================================================
integer(1) function get_stat_lon_between(lon, west, east) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_stat_lon_between'
  real(8), intent(in) :: lon
  real(8), intent(in) :: west, east

  if( lon == west .or. abs(west-lon) == rad_360deg )then
    res = STAT_LON_BETWEEN_WEST
  elseif( lon == east .or. abs(east-lon) == rad_360deg )then
    res = STAT_LON_BETWEEN_EAST
  else
    if( abs(east-west) < rad_180deg )then
      if( west < lon .and. lon < east )then
        res = STAT_LON_BETWEEN_INSIDE
      else
        res = STAT_LON_BETWEEN_OUTSIDE
      endif
    else
      if( west < lon .or. lon < east )then
        res = STAT_LON_BETWEEN_INSIDE
      else
        res = STAT_LON_BETWEEN_OUTSIDE
      endif
    endif
  endif
end function get_stat_lon_between
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
real(8) function londiff_rad__0d(lon1, lon2) result(lon)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'londiff_rad__0d'
  real(8), intent(in) :: lon1, lon2

  lon = abs(lon1 - lon2)
  if( lon > rad_180deg ) lon = rad_360deg - lon
end function londiff_rad__0d
!===============================================================
!
!===============================================================
function londiff_rad__1d(lon1, lon2) result(lon)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'londiff_rad__1d'
  real(8), intent(in) :: lon1(:), lon2(:)
  real(8)             :: lon(size(lon1))

  integer :: i

  do i = 1, size(lon1)
    lon(i) = londiff_rad__0d(lon1(i), lon2(i))
  enddo
end function londiff_rad__1d
!===============================================================
!
!===============================================================
real(8) function londiff_deg__0d(lon1, lon2) result(lon)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'londiff_deg__0d'
  real(8), intent(in) :: lon1, lon2

  lon = abs(lon1 - lon2)
  if( lon > 1.8d2 ) lon = 3.6d2 - lon
end function londiff_deg__0d
!===============================================================
!
!===============================================================
function londiff_deg__1d(lon1, lon2) result(lon)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'londiff_deg__1d'
  real(8), intent(in) :: lon1(:), lon2(:)
  real(8)             :: lon(size(lon1))

  integer :: i

  do i = 1, size(lon1)
    lon(i) = londiff_deg__0d(lon1(i), lon2(i))
  enddo
end function londiff_deg__1d
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
integer function dir_lon(lon1, lon2) result(sgn)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dir_lon'
  real(8), intent(in) :: lon1, lon2

  if( abs(lon1-lon2) < rad_180deg )then
    if( lon1 < lon2 )then
      sgn = 1
    else
      sgn = -1
    endif
  else
    if( lon1 > lon2 )then
      sgn = 1
    else
      sgn = -1
    endif
  endif
end function dir_lon
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
logical function bboxes_intersect(&
    ssouth, snorth, swest, seast, slon0, &
    tsouth, tnorth, twest, teast, tlon0  &
) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'bboxes_intersect'
  real(8), intent(in) :: ssouth, snorth, swest, seast, &
                         tsouth, tnorth, twest, teast  ![rad]
  logical, intent(in) :: slon0, tlon0

  if( snorth <= tsouth .or. ssouth >= tnorth )then
    res = .false.
  elseif( .not. slon0 .and. .not. tlon0 )then
    res = ( .not. (swest >= teast .or. seast <= twest) )
  elseif( .not. tlon0 )then
    res = ( swest < teast .or. twest < seast )
  elseif( .not. slon0 )then
    res = ( twest < seast .or. swest < teast )
  else
    res = .true.
  endif
end function bboxes_intersect
!===============================================================
!
!===============================================================
logical function included_in_bbox(&
    west, east, south, north, lon0, plon, plat &
) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'included_in_bbox'
  real(8), intent(in) :: west, east, south, north
  real(8), intent(in) :: plon, plat
  logical, intent(in) :: lon0

  if( plat < south .or. plat > north )then
    res = .false.
    return
  endif

  res = ( (lon0 .and. (west < plon .or. plon < east)) .or. &
          (.not. lon0 .and. (west < plon .and. plon < east)) )
end function included_in_bbox
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
integer(4) function spherical_to_cartesian_deg__0d_miss_unspecified(&
    lon, lat, x, y, z) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_deg__0d_miss_unspecified'
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z

  info = 0
  !-------------------------------------------------------------
  if( abs(lat) > 9.d1 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\n  abs(lat) > 90.0', &
                PRCNAM, MODNAM)
    return
  endif

  if( abs(lat) == 9.d1 )then
    x = 0.d0
    y = 0.d0
    z = sign(1.d0, lat)
  else
    x = cos(lat*d2r) * cos(lon*d2r)
    y = cos(lat*d2r) * sin(lon*d2r)
    z = sin(lat*d2r)
  endif
end function spherical_to_cartesian_deg__0d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_deg__0d_miss_specified(&
    lon, lat, x, y, z, miss_s, miss_c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_deg__0d_miss_specified'
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z
  real(8), intent(in)  :: miss_s, miss_c

  info = 0
  !-------------------------------------------------------------
  if( lat == miss_s )then
    x = miss_c
    y = miss_c
    z = miss_c
  else
    if( spherical_to_cartesian_deg__0d_miss_unspecified(&
          lon, lat, x, y, z) /= 0 )then
      info = 1
      call errret('', &
                  PRCNAM, MODNAM)
      return
    endif
  endif
end function spherical_to_cartesian_deg__0d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_deg__1d_miss_unspecified(&
    lon, lat, x, y, z) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_deg__1d_miss_unspecified'
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(lon)
    if( spherical_to_cartesian_deg__0d_miss_unspecified(&
          lon(i), lat(i), x(i), y(i), z(i)) /= 0 )then
      info = 1
      call errret('@ i = '//str(i), &
                  PRCNAM, MODNAM)
      return
    endif
  enddo
end function spherical_to_cartesian_deg__1d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_deg__1d_miss_specified(&
    lon, lat, x, y, z, miss_s, miss_c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_deg__1d_miss_specified'
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(lon)
    if( lat(i) == miss_s )then
      x(i) = miss_c
      y(i) = miss_c
      z(i) = miss_c
    else
      if( spherical_to_cartesian_deg__0d_miss_unspecified(&
            lon(i), lat(i), x(i), y(i), z(i)) /= 0 )then
        info = 1
        call errret('@ i = '//str(i), &
                    PRCNAM, MODNAM)
        return
      endif
    endif
  enddo
end function spherical_to_cartesian_deg__1d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_deg__2d_miss_unspecified(&
    lon, lat, x, y, z) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_deg__2d_miss_unspecified'
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      if( spherical_to_cartesian_deg__0d_miss_unspecified(&
            lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j)) /= 0 )then
        info = 1
        call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                    PRCNAM, MODNAM)
        return
      endif
    enddo
  enddo
end function spherical_to_cartesian_deg__2d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_deg__2d_miss_specified(&
    lon, lat, x, y, z, miss_s, miss_c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_deg__2d_miss_specified'
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      if( lat(i,j) == miss_s )then
        x(i,j) = miss_c
        y(i,j) = miss_c
        z(i,j) = miss_c
      else
        if( spherical_to_cartesian_deg__0d_miss_unspecified(&
              lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j)) /= 0 )then
          info = 1
          call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                      PRCNAM, MODNAM)
          return
        endif
      endif
    enddo
  enddo
end function spherical_to_cartesian_deg__2d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_rad__0d_miss_unspecified(&
    lon, lat, x, y, z) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_rad__0d_miss_unspecified'
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z

  info = 0
  !-------------------------------------------------------------
  if( abs(lat) > rad_90deg )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\n  abs(lat) > rad_90deg', &
                PRCNAM, MODNAM)
    return
  endif

  if( abs(lat) == rad_90deg )then
    x = 0.d0
    y = 0.d0
    z = sign(1.d0, lat)
  else
    x = cos(lat) * cos(lon)
    y = cos(lat) * sin(lon)
    z = sin(lat)
  endif
end function spherical_to_cartesian_rad__0d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_rad__0d_miss_specified(&
    lon, lat, x, y, z, miss_s, miss_c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_rad__0d_miss_specified'
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z
  real(8), intent(in)  :: miss_s, miss_c

  info = 0
  !-------------------------------------------------------------
  if( lat == miss_s )then
    x = miss_c
    y = miss_c
    z = miss_c
  else
    if( spherical_to_cartesian_rad__0d_miss_unspecified(&
          lon, lat, x, y, z) /= 0 )then
      info = 1
      call errret('', &
                  PRCNAM, MODNAM)
      return
    endif
  endif
end function spherical_to_cartesian_rad__0d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_rad__1d_miss_unspecified(&
    lon, lat, x, y, z) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_rad__1d_miss_unspecified'
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(lon)
    if( spherical_to_cartesian_rad__0d_miss_unspecified(&
          lon(i), lat(i), x(i), y(i), z(i)) /= 0 )then
      info = 1
      call errret('@ i = '//str(i), &
                  PRCNAM, MODNAM)
      return
    endif
  enddo
end function spherical_to_cartesian_rad__1d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_rad__1d_miss_specified(&
    lon, lat, x, y, z, miss_s, miss_c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_rad__1d_miss_specified'
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(lon)
    if( lat(i) == miss_s )then
      x(i) = miss_c
      y(i) = miss_c
      z(i) = miss_c
    else
      if( spherical_to_cartesian_rad__0d_miss_unspecified(&
            lon(i), lat(i), x(i), y(i), z(i)) /= 0 )then
        info = 1
        call errret('@ i = '//str(i), &
                    PRCNAM, MODNAM)
        return
      endif
    endif
  enddo
end function spherical_to_cartesian_rad__1d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_rad__2d_miss_unspecified(&
    lon, lat, x, y, z) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_rad__2d_miss_unspecified'
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      if( spherical_to_cartesian_rad__0d_miss_unspecified(&
            lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j)) /= 0 )then
        info = 1
        call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                    PRCNAM, MODNAM)
        return
      endif
    enddo
  enddo
end function spherical_to_cartesian_rad__2d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function spherical_to_cartesian_rad__2d_miss_specified(&
    lon, lat, x, y, z, miss_s, miss_c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spherical_to_cartesian_rad__2d_miss_specified'
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      if( lat(i,j) == miss_s )then
        x(i,j) = miss_c
        y(i,j) = miss_c
        z(i,j) = miss_c
      else
        if( spherical_to_cartesian_rad__0d_miss_unspecified(&
              lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j)) /= 0 )then
          info = 1
          call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                      PRCNAM, MODNAM)
          return
        endif
      endif
    enddo
  enddo
end function spherical_to_cartesian_rad__2d_miss_specified
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
integer(4) function cartesian_to_spherical_deg__0d_miss_unspecified(&
    x, y, z, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_deg__0d_miss_unspecified'
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat

  info = 0
  !-------------------------------------------------------------
  if( x == 0.d0 .and. y == 0.d0 )then
    if( z == 0.d0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  (x,y,z) == (0,0,0)', &
                  PRCNAM, MODNAM)
      return
    endif

    lat = sign(9.d1, z)
    lon = 0.d0
  else
    lat = asin( z / sqrt(x**2+y**2+z**2) ) * r2d
    lon = atan2( y, x ) * r2d
    if( lon < 0.d0 ) lon = lon + 3.6d2
  endif
end function cartesian_to_spherical_deg__0d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_deg__0d_miss_specified(&
    x, y, z, lon, lat, miss_c, miss_s) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_deg__0d_miss_specified'
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  info = 0
  !-------------------------------------------------------------
  if( x == miss_c )then
    lon = miss_s
    lat = miss_s
  else
    if( cartesian_to_spherical_deg__0d_miss_unspecified(&
          x, y, z, lon, lat) /= 0 )then
      info = 1
      call errret('', &
                  PRCNAM, MODNAM)
      return
    endif
  endif
end function cartesian_to_spherical_deg__0d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_deg__1d_miss_unspecified(&
    x, y, z, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_deg__1d_miss_unspecified'
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(x)
    if( cartesian_to_spherical_deg__0d_miss_unspecified(&
          x(i), y(i), z(i), lon(i), lat(i)) /= 0 )then
      info = 1
      call errret('@ i = '//str(i), &
                  PRCNAM, MODNAM)
      return
    endif
  enddo
end function cartesian_to_spherical_deg__1d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_deg__1d_miss_specified(&
    x, y, z, lon, lat, miss_c, miss_s) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_deg__1d_miss_specified'
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(x)
    if( x(i) == miss_c )then
      lon(i) = miss_s
      lat(i) = miss_s
    else
      if( cartesian_to_spherical_deg__0d_miss_unspecified(&
            x(i), y(i), z(i), lon(i), lat(i)) /= 0 )then
        info = 1
        call errret('@ i = '//str(i), &
                    PRCNAM, MODNAM)
        return
      endif
    endif
  enddo
end function cartesian_to_spherical_deg__1d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_deg__2d_miss_unspecified(&
    x, y, z, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_deg__2d_miss_unspecified'
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(x,2)
    do i = 1, size(x,1)
      if( cartesian_to_spherical_deg__0d_miss_unspecified(&
            x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j)) /= 0 )then
        info = 1
        call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                    PRCNAM, MODNAM)
        return
      endif
    enddo
  enddo
end function cartesian_to_spherical_deg__2d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_deg__2d_miss_specified(&
    x, y, z, lon, lat, miss_c, miss_s) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_deg__2d_miss_specified'
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(x,2)
    do i = 1, size(x,1)
      if( x(i,j) == miss_c )then
        lon(i,j) = miss_s
        lat(i,j) = miss_s
      else
        if( cartesian_to_spherical_deg__0d_miss_unspecified(&
              x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j)) /= 0 )then
          info = 1
          call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                      PRCNAM, MODNAM)
          return
        endif
      endif
    enddo
  enddo
end function cartesian_to_spherical_deg__2d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_rad__0d_miss_unspecified(&
    x, y, z, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_rad__0d_miss_unspecified'
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat

  real(8) :: r

  info = 0
  !-------------------------------------------------------------
  r = sqrt(x**2 + y**2 + z**2)

  if( abs(z) > r .or. r == 0.d0 )then
    info = 1
    call errret('x: '//str(x,'es20.13')//&
              '\ny: '//str(y,'es20.13')//&
              '\nz: '//str(z,'es20.13')//&
              '\nr: '//str(r,'es20.13'), &
                PRCNAM, MODNAM)
    return
  endif

  lon = atan2(y,x)
  lat = asin(z/r)

  if( x == 0.d0 .and. y == 0.d0 )then
    lat = sign(rad_90deg, z)
    lon = 0.d0
  else
    lat = asin( z / sqrt(x**2+y**2+z**2) )
    lon = atan2( y, x )
    if( lon < rad_0deg ) lon = lon + rad_360deg
  endif
end function cartesian_to_spherical_rad__0d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_rad__0d_miss_specified(&
    x, y, z, lon, lat, miss_c, miss_s) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_rad__0d_miss_specified'
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  info = 0
  !-------------------------------------------------------------
  if( x == miss_c )then
    lon = miss_s
    lat = miss_s
  else
    if( cartesian_to_spherical_rad__0d_miss_unspecified(&
          x, y, z, lon, lat) /= 0 )then
      info = 1
      call errret('', &
                  PRCNAM, MODNAM)
      return
    endif
  endif
end function cartesian_to_spherical_rad__0d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_rad__1d_miss_unspecified(&
    x, y, z, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_rad__1d_miss_unspecified'
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(x)
    if( cartesian_to_spherical_rad__0d_miss_unspecified(&
          x(i), y(i), z(i), lon(i), lat(i)) /= 0 )then
      info = 1
      call errret('@ i = '//str(i), &
                  PRCNAM, MODNAM)
      return
    endif
  enddo
end function cartesian_to_spherical_rad__1d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_rad__1d_miss_specified(&
    x, y, z, lon, lat, miss_c, miss_s) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_rad__1d_miss_specified'
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i

  info = 0
  !-------------------------------------------------------------
  do i = 1, size(x)
    if( x(i) == miss_c )then
      lon(i) = miss_s
      lat(i) = miss_s
    else
      if( cartesian_to_spherical_rad__0d_miss_unspecified(&
            x(i), y(i), z(i), lon(i), lat(i)) /= 0 )then
        info = 1
        call errret('@ i = '//str(i), &
                    PRCNAM, MODNAM)
        return
      endif
    endif
  enddo
end function cartesian_to_spherical_rad__1d_miss_specified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_rad__2d_miss_unspecified(&
    x, y, z, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_rad__2d_miss_unspecified'
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(x,2)
    do i = 1, size(x,1)
      if( cartesian_to_spherical_rad__0d_miss_unspecified(&
            x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j)) /= 0 )then
        info = 1
        call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                    PRCNAM, MODNAM)
        return
      endif
    enddo
  enddo
end function cartesian_to_spherical_rad__2d_miss_unspecified
!===============================================================
!
!===============================================================
integer(4) function cartesian_to_spherical_rad__2d_miss_specified(&
    x, y, z, lon, lat, miss_c, miss_s) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cartesian_to_spherical_rad__2d_miss_specified'
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i, j

  info = 0
  !-------------------------------------------------------------
  do j = 1, size(x,2)
    do i = 1, size(x,1)
      if( x(i,j) == miss_c )then
        lon(i,j) = miss_s
        lat(i,j) = miss_s
      else
        if( cartesian_to_spherical_rad__0d_miss_unspecified(&
               x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j)) /= 0 )then
          info = 1
          call errret('@ (i,j) = ('//str((/i,j/),',')//')', &
                      PRCNAM, MODNAM)
          return
        endif
      endif
    enddo
  enddo
end function cartesian_to_spherical_rad__2d_miss_specified
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Calc. coefs. of plane OAB (ax + by + cz = 0)
!===============================================================
integer(4) function calc_coefs_large_arc_spherical(&
    lon1, lat1, lon2, lat2, a, b, c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_coefs_large_arc_spherical'
  real(8), intent(in)  :: lon1, lat1, lon2, lat2
  real(8), intent(out) :: a, b, c
  real(8) :: x1, y1, z1, x2, y2, z2

  info = 0
  !-------------------------------------------------------------
  x1 = cos(lat1) * cos(lon1)
  y1 = cos(lat1) * sin(lon1)
  z1 = sin(lat1)

  x2 = cos(lat2) * cos(lon2)
  y2 = cos(lat2) * sin(lon2)
  z2 = sin(lat2)

  call calc_cross_product(x1, y1, z1, x2, y2, z2, a, b, c)
end function calc_coefs_large_arc_spherical
!===============================================================
!
!===============================================================
integer(4) function calc_coefs_large_arc_cartesian(&
    x1, y1, z1, x2, y2, z2, a, b, c) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_coefs_large_arc_cartesian'
  real(8), intent(in)  :: x1, y1, z1, x2, y2, z2
  real(8), intent(out) :: a, b, c

  info = 0
  !-------------------------------------------------------------
  call calc_cross_product(x1, y1, z1, x2, y2, z2, a, b, c)
end function calc_coefs_large_arc_cartesian
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Calc. longit. and latit. of the intersection.
! Intersection is confirmed.
!===============================================================
integer(4) function intersection_sphere_normal_normal_confirmed(&
    sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
    tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
    west, east, plon, plat, stat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_sphere_normal_normal_confirmed'
  real(8), intent(in) :: sx1, sy1, sz1, sx2, sy2, sz2
  real(8), intent(in) :: sa, sb, sc
  real(8), intent(in) :: tx1, ty1, tz1, tx2, ty2, tz2
  real(8), intent(in) :: ta, tb, tc
  real(8), intent(in) :: west, east
  real(8), intent(out) :: plon, plat
  integer, intent(out) :: stat

  real(8) :: dp_S1_IT, dp_S2_IT, dp_T1_IS, dp_T2_IS
  real(8) :: px, py, pz
  real(8) :: plon1, plat1, plon2, plat2
  real(8) :: diff_plon1_west, diff_plon1_east, diff_plon2_west, diff_plon2_east
  real(8) :: diff_min

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  stat = 0

  dp_S1_IT = sx1*ta + sy1*tb + sz1*tc  ! dot products
  dp_S2_IT = sx2*ta + sy2*tb + sz2*tc
  dp_T1_IS = tx1*sa + ty1*sb + tz1*sc
  dp_T2_IS = tx2*sa + ty2*sb + tz2*sc

!  call logmsg('s1 ('//str((/sx1,sy1,sz1/),'es10.3',',')//')')
!  call logmsg('s2 ('//str((/sx2,sy2,sz2/),'es10.3',',')//')')
!  call logmsg('t1 ('//str((/tx1,ty1,tz1/),'es10.3',',')//')')
!  call logmsg('t2 ('//str((/tx2,ty2,tz2/),'es10.3',',')//')')
!  call logmsg('IS ('//str((/sa,sb,sc/),'es15.8',',')//')')
!  call logmsg('IT ('//str((/ta,tb,tc/),'es15.8',',')//')')
!  call logmsg('S1_IT '//str(dp_S1_IT,'es15.8')//' is zero: '//str(dp_S1_IT==0.d0))
!  call logmsg('S2_IT '//str(dp_S2_IT,'es15.8')//' is zero: '//str(dp_S2_IT==0.d0))
!  call logmsg('T1_IS '//str(dp_T1_IS,'es15.8')//' is zero: '//str(dp_T1_IS==0.d0))
!  call logmsg('T2_IS '//str(dp_T2_IS,'es15.8')//' is zero: '//str(dp_T2_IS==0.d0))

  call calc_cross_product(sa, sb, sc, ta, tb, tc, px, py, pz)

!  call logmsg('P  ('//str((/px,py,pz/),'es15.8',',')//')')
  !-------------------------------------------------------------
  ! Case: Two lines are parallel (same normal vector)
  !-------------------------------------------------------------
  if( px**2+py**2+pz**2 == 0.d0 )then
    plon = 0.d0
    plat = 0.d0
    stat = 1
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: P = IA x IB
  if( dp_S1_IT > 0.d0 .and. dp_S2_IT < 0.d0 .and. &
      dp_T1_IS < 0.d0 .and. dp_T2_IS > 0.d0 )then
    !call logmsg('P = IA x IB')
    if( cartesian_to_spherical_rad(px, py, pz, plon, plat) /= 0 )then
      info = 1; call errret('', PRCNAM, MODNAM); return
    endif
  !-------------------------------------------------------------
  ! Case: P = IB x IA
  elseif( dp_S1_IT < 0.d0 .and. dp_S2_IT > 0.d0 .and. &
          dp_T1_IS > 0.d0 .and. dp_T2_IS < 0.d0 )then
    !call logmsg('P = IB x IA')
    if( cartesian_to_spherical_rad(-px, -py, -pz, plon, plat) /= 0 )then
      info = 1; call errret('', PRCNAM, MODNAM); return
    endif
  !-------------------------------------------------------------
  ! Case: Others
  else
    if( cartesian_to_spherical_rad(px, py, pz, plon1, plat1) /= 0 )then
      info = 1; call errret('', PRCNAM, MODNAM); return
    endif

    if( plon1 > rad_180deg )then
      plon2 = plon1 - rad_180deg
    else
      plon2 = plon1 + rad_180deg
    endif
    plat2 = -plat1

    !call logmsg('west: '//str(west*r2d,'f12.8')//' east: '//str(east*r2d,'f12.8'))
    !call logmsg('P1: ('//str((/plon1,plat1/)*r2d,'f12.8',', ')//')')
    !call logmsg('P2: ('//str((/plon2,plat2/)*r2d,'f12.8',', ')//')')

    if( which_is_western(plon1,west) /= 1 .and. which_is_western(plon1,east) /= 2 )then
      !call logmsg('P = P1')
      plon = plon1
      plat = plat1
    elseif( which_is_western(plon2,west) /= 1 .and. which_is_western(plon2,east) /= 2 )then
      !call logmsg('P = P2')
      plon = plon2
      plat = plat2
    else
      diff_plon1_west = abs(londiff_rad(plon1,west))
      diff_plon1_east = abs(londiff_rad(plon1,east))
      diff_plon2_west = abs(londiff_rad(plon2,west))
      diff_plon2_east = abs(londiff_rad(plon2,east))
      diff_min = min(diff_plon1_west, diff_plon1_east, diff_plon2_west, diff_plon2_east)

      if( diff_min == diff_plon1_west )then
        !call logmsg('P = (west,Plat1)')
        plon = west
        plat = plat1
      elseif( diff_min == diff_plon1_east )then
        !call logmsg('P = (east,Plat1)')
        plon = east
        plat = plat1
      elseif( diff_min == diff_plon2_west )then
        !call logmsg('P = (west,Plat2)')
        plon = west
        plat = plat2
      elseif( diff_min == diff_plon2_east )then
        !call logmsg('P = (east,Plat2)')
        plon = east
        plat = plat2
      endif

      !call logmsg('Calculated intersection is out of range.')
    endif
  endif
  !-------------------------------------------------------------
end function intersection_sphere_normal_normal_confirmed
!===============================================================
!
!===============================================================
integer(4) function intersection_sphere_normal_meridian_0d(&
    wlon, wlat, elon, elat, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_sphere_normal_meridian_0d'
  real(8), intent(in)  :: wlon, wlat, elon, elat
  real(8), intent(in)  :: lon
  real(8), intent(out) :: lat

  info = 0
  !-------------------------------------------------------------
  if( wlon < elon )then
    lat = atan( (tan(wlat)*sin(elon-lon) + tan(elat)*sin(lon-wlon)) / sin(elon-wlon) )
  else
    lat = atan( (tan(wlat)*sin(londiff_rad(elon,lon)) + tan(elat)*sin(londiff_rad(wlon,lon))) &
                  / sin(rad_360deg-wlon+elon) )
  endif
end function intersection_sphere_normal_meridian_0d
!===============================================================
!
!===============================================================
integer(4) function intersection_sphere_normal_meridian_1d(&
    wlon, wlat, elon, elat, lon, lat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_sphere_normal_meridian_1d'
  real(8), intent(in)  :: wlon, wlat, elon, elat
  real(8), intent(in)  :: lon(:)
  real(8), intent(out) :: lat(:)

  integer :: i, imax

  info = 0
  !-------------------------------------------------------------
  imax = size(lon)

  if( wlon < elon )then
    do i = 1, imax
      lat(i) = atan( (tan(wlat)*sin(elon-lon(i)) + tan(elat)*sin(lon(i)-wlon)) / sin(elon-wlon) )
    enddo
  else
    do i = 1, imax
      lat(i) = atan( (tan(wlat)*sin(londiff_rad(elon,lon(i))) &
                        + tan(elat)*sin(londiff_rad(wlon,lon(i)))) &
                       / sin(rad_360deg-wlon+elon) )
    enddo
  endif
end function intersection_sphere_normal_meridian_1d
!===============================================================
! Calc. longit. of the intersection.
! 交差するかどうか事前に分かっていない場合に使う。
! 経度の範囲を指定しない。
!===============================================================
integer(4) function intersection_sphere_normal_parallel1(&
    sa, sb, sc, ssgn, tz, lon, stat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_sphere_normal_parallel1'
  real(8), intent(in)  :: sa, sb, sc
  integer, intent(in)  :: ssgn
  real(8), intent(in)  :: tz
  real(8), intent(out) :: lon
  integer, intent(out) :: stat

  real(8) :: d2
  real(8) :: x_numer, y_numer

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  d2 = (sa**2 + sb**2) - (sa**2 + sb**2 + sc**2) * tz**2

  if( d2 > 0.d0 )then
    stat = STAT_INTERSECTION_YES

    x_numer = -sa*sc*tz - ssgn*sb*sqrt(d2)
    y_numer = -sb*sc*tz + ssgn*sa*sqrt(d2)

    lon = atan2(y_numer, x_numer)
  else
    stat = STAT_INTERSECTION_NO
    lon = 0.d0
  endif
  !-------------------------------------------------------------
end function intersection_sphere_normal_parallel1
!===============================================================
! Calc. longit. of the intersection.
! 交差するかどうか事前に分かっていない場合に使う。
! 経度の範囲を指定する。
!===============================================================
integer(4) function intersection_sphere_normal_parallel2(&
    sa, sb, sc, ssgn, tz, wlon, elon, plon, stat) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_sphere_normal_parallel2'
  real(8), intent(in)  :: sa, sb, sc
  integer, intent(in)  :: ssgn
  real(8), intent(in)  :: tz
  real(8), intent(in)  :: wlon, elon
  real(8), intent(out) :: plon
  integer, intent(out) :: stat

  real(8) :: cos_lat
  real(8) :: tan_wlat, tan_elat
  real(8) :: d
  real(8) :: x_numer, y_numer
  logical :: intersect

  info = 0
  !-------------------------------------------------------------
  cos_lat = sqrt(1.d0 - tz**2)  ! > 0

  tan_wlat = - (sa*cos(wlon) + sb*sin(wlon)) / sc
  tan_elat = - (sa*cos(elon) + sb*sin(elon)) / sc

  selectcase( ssgn )
  case( 1 )
    ! tan_wlat < tan_lat .and. tan_lat < tan_elat
    intersect = tan_wlat*cos_lat < tz .and. tz < tan_elat*cos_lat
  case( -1 )
    ! tan_wlat > tan_lat .and. tan_lat > tan_elat
    intersect = tan_wlat*cos_lat > tz .and. tz > tan_elat*cos_lat
  case default
    info = 1
    call errret(msg_invalid_value('ssgn', ssgn), &
                PRCNAM, MODNAM)
    return
  endselect

  if( intersect )then
    stat = STAT_INTERSECTION_YES

    d = sqrt(max(0.d0, (sa**2+sb**2) - (sa**2+sb**2+sc**2) * tz**2))

    x_numer = -sa*sc*tz - ssgn*sb*d
    y_numer = -sb*sc*tz + ssgn*sa*d

    plon = atan2(y_numer, x_numer)

    if( which_is_western(plon,wlon) /= 2 )then
      plon = wlon
    elseif( which_is_western(plon,elon) /= 1 )then
      plon = elon
    endif
  else
    stat = STAT_INTERSECTION_NO
    plon = 0.d0
  endif
end function intersection_sphere_normal_parallel2
!===============================================================
! Calc. longit. of the intersection.
! 交差することが事前に分かっている場合に使う。
! 交点が (wlon, elon) の範囲外になった場合、
! その値を wlon または elon に補正する。
!===============================================================
integer(4) function intersection_sphere_normal_parallel3(&
    sa, sb, sc, ssgn, tz, wlon, elon, plon) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_sphere_normal_parallel3'
  real(8), intent(in)  :: sa, sb, sc
  integer, intent(in)  :: ssgn
  real(8), intent(in)  :: tz
  real(8), intent(in)  :: wlon, elon
  real(8), intent(out) :: plon

  real(8) :: d
  real(8) :: x_numer, y_numer

  info = 0
  !-------------------------------------------------------------
  d = sqrt(max(0.d0, (sa**2+sb**2) - (sa**2+sb**2+sc**2) * tz**2))

  x_numer = -sa*sc*tz - ssgn*sb*d
  y_numer = -sb*sc*tz + ssgn*sa*d

  plon = atan2(y_numer, x_numer)
  if( plon < rad_0deg ) plon = plon + rad_360deg

  if( which_is_western(plon,wlon) /= 2 )then
    plon = wlon
  elseif( which_is_western(plon,elon) /= 1 )then
    plon = elon
  endif
end function intersection_sphere_normal_parallel3
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
integer(4) function calc_lat_range_large_arc(&
    lon1, lat1, lon2, lat2, &
    a, b, c, &
    south, north, convex, lontop, lattop) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_lat_range_large_arc'
  real(8)   , intent(in)  :: lon1, lat1, lon2, lat2
  real(8)   , intent(in)  :: a, b, c
  real(8)   , intent(out) :: south, north
  integer(1), intent(out) :: convex
  real(8)   , intent(out) :: lontop, lattop

  real(8) :: plon, plat
  real(8) :: wlon, wlat, elon, elat

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  south = min(lat1, lat2)
  north = max(lat1, lat2)

  convex = CONVEX_MONOTONE
  lontop = 0.d0
  lattop = 0.d0
  !-------------------------------------------------------------
  ! Case: Arc intersects with the equator
  if( lat1 == rad_0deg .or. lat2 == rad_0deg .or. &
      (lat1 > rad_0deg .neqv. lat2 > rad_0deg) )then
    continue
  !-------------------------------------------------------------
  ! Case: Arc is meridian
  elseif( lon1 == lon2 .or. c == 0.d0 )then
    continue
  !-------------------------------------------------------------
  ! Case: Others
  else
    !-----------------------------------------------------------
    ! Calc. lon. of the top supposing it is convex
    !-----------------------------------------------------------
    ! Case: The normal (a,b,c) is upward
    !   lat < 0 @ (a,b) and lat > 0 @ (-a,-b)
    if( c > 0.d0 )then
      if( lat1 < rad_0deg )then
        plon = atan2(b, a)
      else
        plon = atan2(-b, -a)
      endif
    !-----------------------------------------------------------
    ! Case: The normal (a,b,c) is downward
    !   lat > 0 @ (a,b) and lat < 0 @ (-a,-b)
    else
      if ( lat1 > rad_0deg )then
        plon = atan2(b, a)
      else
        plon = atan2(-b, -a)
      endif
    endif

    if( plon < rad_0deg ) plon = plon + rad_360deg

!    call logmsg('A1: '//str((/lon1,lat1/)*r2d,'es20.13'))
!    call logmsg('A2: '//str((/lon2,lat2/)*r2d,'es20.13'))
!    call logmsg('plon: '//str(plon*r2d,'es20.13'))
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( abs(lon1-lon2) < rad_180deg )then
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

      if( wlon < plon .and. plon < elon )then
        if( intersection_sphere_normal_meridian(&
              wlon, wlat, elon, elat, plon, plat) /= 0 )then
          info = 1; call errret('', PRCNAM, MODNAM); return
        endif

        if( lat1 < rad_0deg )then
          if( plat < south )then
            convex = CONVEX_DOWNWARD
            lontop = plon
            lattop = plat
            south  = plat
          endif
        else
          if( plat > north )then
            convex = CONVEX_UPWARD
            lontop = plon
            lattop = plat
            north  = plat
          endif
        endif
      endif
    else
      if( lon1 > lon2 )then
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

      if( wlon < plon .or. plon < elon )then
        if( intersection_sphere_normal_meridian(&
              wlon, wlat, elon, elat, plon, plat) /= 0 )then
          info = 1; call errret('', PRCNAM, MODNAM); return
        endif

        if( lat1 < rad_0deg )then
          if( plat < south )then
            convex = CONVEX_DOWNWARD
            lontop = plon
            lattop = plat
            south  = plat
          endif
        else
          if( plat > north )then
            convex = CONVEX_UPWARD
            lontop = plon
            lattop = plat
            north  = plat
          endif
        endif
      endif
    endif
  endif
end function calc_lat_range_large_arc
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
integer(4) function calc_lon_range_shared(&
    slon1, slon2, tlon1, tlon2, &
    sdir, tdir, id_west, id_east, west, east) result(info)
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_lon_range_shared'
  real(8), intent(in)  :: slon1, slon2
  real(8), intent(in)  :: tlon1, tlon2
  integer, intent(out) :: sdir, tdir
  integer, intent(out) :: id_west, id_east
  real(8), intent(out) :: west, east

  integer :: s_id_west, t_id_west
  real(8) :: swest, seast, twest, teast

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  id_west = 0
  id_east = 0
  west = 0.d0
  east = 0.d0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  s_id_west = which_is_western(slon1, slon2)
  t_id_west = which_is_western(tlon1, tlon2)

  selectcase( s_id_west )
  case( 1 )
    swest = slon1
    seast = slon2
    sdir = 1
  case( 2 )
    swest = slon2
    seast = slon1
    sdir = -1
  case default
    info = 1
    call errret(msg_invalid_value('s_id_west', s_id_west), &
                PRCNAM, MODNAM)
    return
  endselect

  selectcase( t_id_west )
  case( 1 )
    twest = tlon1
    teast = tlon2
    tdir = 1
  case( 2 )
    twest = tlon2
    teast = tlon1
    tdir = -1
  case default
    info = 1
    call errret(msg_invalid_value('t_id_west', t_id_west), &
                PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( abs(tlon2-tlon1) > rad_180deg )then
    if( twest <= swest .or. swest <= teast )then
      west = swest
      if( sdir == 1 )then
        id_west = 1  ! west == swest == slon1
      else
        id_west = 2  ! west == swest == slon2
      endif
    endif

    if( twest <= seast .or. seast <= teast )then
      east = seast
      if( sdir == 1 )then
        id_east = 2  ! east = seast == slon2
      else
        id_east = 1  ! east == seast == slon1
      endif
    endif
  else
    if( twest <= swest .and. swest <= teast )then
      west = swest
      selectcase( sdir )
      case( 1 )
        id_west = 1  ! west == swest == slon1
      case( -1 )
        id_west = 2  ! west == swest == slon2
      case default
        info = 1
        call errret(msg_invalid_value('sdir', sdir), &
                    PRCNAM, MODNAM)
        return
      endselect
    endif

    if( twest <= seast .and. seast <= teast )then
      east = seast
      selectcase( sdir )
      case( 1 )
        id_east = 2  ! east = seast == slon2
      case( -1 )
        id_east = 1  ! east == seast == slon1
      case default
        info = 1
        call errret(msg_invalid_value('sdir', sdir), &
                    PRCNAM, MODNAM)
        return
      endselect
    endif
  endif

  if( abs(slon2-slon1) > rad_180deg )then
    if( id_west == 0 )then
      if( swest <= twest .or. twest <= seast )then
        west = twest
        selectcase( tdir )
        case( 1 )
          id_west = 3  ! west == twest == tlon1
        case( -1 )
          id_west = 4  ! west == twest == tlon2
        case default
          info = 1
          call errret(msg_invalid_value('tdir', tdir), &
                      PRCNAM, MODNAM)
          return
        endselect
      endif
    endif

    if( id_east == 0 )then
      if( swest <= teast .or. teast <= seast )then
        east = teast
        selectcase( tdir )
        case( 1 )
          id_east = 4  ! east == teast == tlon2
        case( -1 )
          id_east = 3  ! east == teast == tlon1
        case default
          info = 1
          call errret(msg_invalid_value('tdir', tdir), &
                      PRCNAM, MODNAM)
          return
        endselect
      endif
    endif
  else
    if( id_west == 0 )then
      if( swest <= twest .and. twest <= seast )then
        west = twest
        selectcase( tdir )
        case( 1 )
          id_west = 3  ! west == twest == tlon1
        case( -1 )
          id_west = 4  ! west == twest == tlon2
        case default
          info = 1
          call errret(msg_invalid_value('tdir', tdir), &
                      PRCNAM, MODNAM)
          return
        endselect
      endif
    endif

    if( id_east == 0 )then
      if( swest <= teast .and. teast <= seast )then
        east = teast
        selectcase( tdir )
        case( 1 )
          id_east = 4  ! east == teast == tlon2
        case( -1 )
          id_east = 3  ! east == teast == tlon1
        case default
          info = 1
          call errret(msg_invalid_value('tdir', tdir), &
                      PRCNAM, MODNAM)
          return
        endselect
      endif
    endif
  endif
end function calc_lon_range_shared
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Area of rectangular on the sphere
!===============================================================
function area_sphere_rect_lat0d(lat1, lat2) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_rect_lat0d'
  real(8), intent(in) :: lat1, lat2
  real(8)             :: area

  area = abs(sin(lat1) - sin(lat2))
end function area_sphere_rect_lat0d
!===============================================================
!
!===============================================================
function area_sphere_rect_lat1d(lat1, lat2) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_rect_lat1d'
  real(8), intent(in) :: lat1(:), lat2(:)
  real(8)             :: area(size(lat1))

  area(:) = abs(sin(lat1(:)) - sin(lat2(:)))
end function area_sphere_rect_lat1d
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Area of rectangular on the ellipsoid
!===============================================================
function area_ellips_rect_lat0d(lat1, lat2, e2) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_ellips_rect_lat0d'
  real(8), intent(in) :: lat1, lat2  ![rad]
  real(8), intent(in) :: e2   ! Square of the eccentricity
  real(8)             :: area
  real(8) :: e

  e = sqrt(e2)
  area = (1.d0-e2) * abs(s(lat1)-s(lat2))
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
function s(lat) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 's'
  real(8), intent(in) :: lat  ![rad]
  real(8)             :: res
  real(8) :: z

  z = sin(lat)
  res = 0.5d0 * z/(1.d0-e2*z**2) &
        + 0.25d0 / e * log(abs((1.d0+e*z)/(1.d0-e*z)))
end function
!---------------------------------------------------------------
end function area_ellips_rect_lat0d
!===============================================================
!
!===============================================================
function area_ellips_rect_lat1d(lat1, lat2, e2) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_ellips_rect_lat1d'
  real(8), intent(in) :: lat1(:), lat2(:)  ![rad]
  real(8), intent(in) :: e2   ! Square of the eccentricity
  real(8)             :: area(size(lat1))
  real(8) :: e

  e = sqrt(e2)
  area(:) = (1.d0-e2) * abs(s(lat1(:))-s(lat2(:)))
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
function s(lat) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 's'
  real(8), intent(in) :: lat(:)  ![rad]
  real(8)             :: res(size(lat))
  real(8) :: z(size(lat))

  z(:) = sin(lat(:))
  res(:) = 0.5d0 * z(:)/(1.d0-e2*z(:)**2) &
           + 0.25d0 / e * log(abs((1.d0+e*z(:))/(1.d0-e*z(:))))
end function
!---------------------------------------------------------------
end function area_ellips_rect_lat1d
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
real(8) function area_sphere_polarrect(lon, sin_lat, sgn_pole) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_polarrect'
  real(8), intent(in) :: lon, sin_lat
  integer, intent(in) :: sgn_pole

  area = abs(sgn_pole - sin_lat) * lon
end function area_sphere_polarrect
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
function area_sphere_tri(lonA, latA, lonB, latB, lonC, latC) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_tri'
  real(8), intent(in)  :: lonA, latA, lonB, latB, lonC, latC  ![rad]
  real(8)              :: area
  real(8) :: OA(3), OB(3), OC(3)
  real(8) :: OAxOB(3), OBxOC(3), OCxOA(3)
  real(8) :: cosA_denom2, cosB_denom2, cosC_denom2
  real(8) :: cosA_numer, cosB_numer, cosC_numer
  real(8) :: cosA, cosB, cosC

  OA(:) = (/cos(latA)*cos(lonA), cos(latA)*sin(lonA), sin(latA)/)
  OB(:) = (/cos(latB)*cos(lonB), cos(latB)*sin(lonB), sin(latB)/)
  OC(:) = (/cos(latC)*cos(lonC), cos(latC)*sin(lonC), sin(latC)/)

  ! cos
  call calc_cross_product(OA, OB, OAxOB)
  call calc_cross_product(OB, OC, OBxOC)
  call calc_cross_product(OC, OA, OCxOA)

  cosA_denom2 = sum(OCxOA(:)**2) * sum(OAxOB(:)**2)
  cosB_denom2 = sum(OAxOB(:)**2) * sum(OBxOC(:)**2)
  cosC_denom2 = sum(OBxOC(:)**2) * sum(OCxOA(:)**2)
  if( cosA_denom2 == 0.d0 .or. cosB_denom2 == 0.d0 .or. cosC_denom2 == 0.d0 )then
    area = 0.d0
    return
  endif

  cosA_numer = sum(-OCxOA(:)*OAxOB(:))
  cosB_numer = sum(-OAxOB(:)*OBxOC(:))
  cosC_numer = sum(-OBxOC(:)*OCxOA(:))

  cosA = cosA_numer / sqrt(cosA_denom2)
  cosB = cosB_numer / sqrt(cosB_denom2)
  cosC = cosC_numer / sqrt(cosC_denom2)

  area = max(0.d0, acos(cosA) + acos(cosB) + acos(cosC) - pi)
end function area_sphere_tri
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
real(8) function area_sphere_righttri_south_bottom(lat1, lat2) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_righttri_south_bottom'
  real(8), intent(in) :: lat1, lat2

  real(8) :: south, north
  real(8) :: clat, dlat

  south = min(lat1,lat2)
  north = max(lat1,lat2)
  dlat = (north - south) * 0.5d0
  clat = (north + south) * 0.5d0

  if( dlat < 1d-16 )then
    area = 0.d0
  else
    area = -sin(south) + sin(clat)*(sin(dlat)/dlat)
  endif
end function area_sphere_righttri_south_bottom
!===============================================================
!
!===============================================================
real(8) function area_sphere_righttri_north_bottom(lat1, lat2) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_righttri_north_bottom'
  real(8), intent(in) :: lat1, lat2

  real(8) :: south, north
  real(8) :: clat, dlat

  south = min(lat1,lat2)
  north = max(lat1,lat2)
  dlat = (north - south) * 0.5d0
  clat = (north + south) * 0.5d0

  if( dlat < 1d-16 )then
    area = 0.d0
  else
    area = sin(north) - sin(clat)*(sin(dlat)/dlat)
  endif
end function area_sphere_righttri_north_bottom
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
real(8) function area_sphere_polartri_spherical(&
    lon, lat1, lat2, sgn_pole) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_polartri_spherical'
  real(8), intent(in) :: lon, lat1, lat2
  integer, intent(in) :: sgn_pole

  integer :: imax, i
  real(8) :: c1lon, c1lat, c2lon, c2lat
  real(8) :: area_add

  if( lon > rad_30deg )then
    imax = ceiling(lon / rad_30deg)

    area = 0.d0
    c1lon = 0.d0
    c1lat = lat1
    do i = 1, imax
      if( i == imax )then
        c2lon = lon
      else
        c2lon = lon * i / imax
      endif

      if( intersection_sphere_normal_meridian(&
            0.d0, lat1, lon, lat2, c2lon, c2lat) /= 0 )then
        call logwrn('Abnormal end of the function '//&
                    '`intersection_sphere_normal_meridian` '//&
                    'was detected.')
      else
        area_add = area_sphere_polartri_lt90deg(lon/imax, c1lat, c2lat, sgn_pole)
        area = area + area_add
      endif

      c1lon = c2lon
      c1lat = c2lat
    enddo
  else
    area = area_sphere_polartri_lt90deg(lon, lat1, lat2, sgn_pole)
  endif
end function area_sphere_polartri_spherical
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Fixed version
!===============================================================
real(8) function area_sphere_polartri_lt90deg(&
    lon, lat1, lat2, sgn_pole &
) result(area)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_polartri_lt90deg'
  real(8), intent(in) :: lon, lat1, lat2
  integer, intent(in) :: sgn_pole

  real(8) :: k, eta2, kappa, chi
  real(8), parameter :: eta2_thresh = 0.d0
  real(8), parameter :: chi_diff_thresh = 1d-10

  k = cos(lat1)*cos(lat2)*sin(lon*0.5d0)**2
  eta2 = sin(lat1-lat2)**2 + 4*k*(cos(lat1-lat2) - k)

  kappa = sin(lon) * (sin(lat1)+sin(lat2)) &
          * 2.d0 * (sin((lat1-lat2)*0.5d0)**2 + cos(lat1)*cos(lat2)*sin(lon*0.5d0)**2)

  if( eta2 <= eta2_thresh )then
    chi = 0.d0
  else
    chi = sgn_pole * kappa / eta2

    if( abs(chi) -1.d0 > chi_diff_thresh )then
      area = 0.d0
      call logerr(msg_unexpected_condition()//&
                '\n  abs(chi) - 1.0 > '//str(chi_diff_thresh)//&
                '\n  chi  : '//str(chi,'es20.13')//&
                '\n  eta2 : '//str(eta2,'es20.13')//&
                '\n  kappa: '//str(kappa,'es20.13'), &
                  '', PRCNAM, MODNAM)
      return
    endif
  endif

  area = lon - asin(chi)
end function area_sphere_polartri_lt90deg
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
integer(4) function area_sphere_polygon__spherical(&
    lon, lat, arctyp, area) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_polygon__spherical'
  real(8)   , intent(in)  :: lon(:), lat(:)
  integer(1), intent(in)  :: arctyp(:)
  real(8)   , intent(out) :: area

  integer :: nmax, n1, n2
  integer :: sgn_pole
  real(8) :: area_add

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( lat(1) > rad_0deg )then
    sgn_pole = 1
  else
    sgn_pole = -1
  endif

  nmax = size(lon)

  n1 = nmax
  n2 = 1

  area = 0.d0

  do while( n2 <= nmax )

    selectcase( arctyp(n1) )
    case( ARC_TYPE_NORMAL )
      area_add = area_sphere_polartri(londiff_rad(lon(n1),lon(n2)), lat(n1), lat(n2), sgn_pole)
      area = area + area_add * dir_lon(lon(n1),lon(n2)) * sgn_pole
    case( ARC_TYPE_PARALLEL )
      area_add = area_sphere_polarrect(londiff_rad(lon(n1),lon(n2)), sin(lat(n1)), sgn_pole)
      area = area + area_add * dir_lon(lon(n1),lon(n2)) * sgn_pole
    case( ARC_TYPE_MERIDIAN )
      continue
    case default
      info = 1
      call errret(msg_invalid_value('arctyp('//str(n1)//')', arctyp(n1)), &
                  PRCNAM, MODNAM)
      return
    endselect

    n1 = n2
    n2 = n2 + 1
  enddo
  !-------------------------------------------------------------
end function area_sphere_polygon__spherical
!===============================================================
!
!===============================================================
integer(4) function area_sphere_polygon__cartesian(&
    x, y, z, arctyp, area) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_polygon__cartesian'
  real(8)   , intent(in)  :: x(:), y(:), z(:)
  integer(1), intent(in)  :: arctyp(:)
  real(8)   , intent(out) :: area

  integer :: nmax, n1, n2
  real(8) :: lon1, lon2
  real(8) :: lat1, lat2
  integer :: sgn_pole

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( z(1) > 0.d0 )then
    sgn_pole = 1
  else
    sgn_pole = -1
  endif

  nmax = size(x)

  n1 = nmax
  n2 = 1

  lon1 = atan2(y(n1), x(n1))
  lon2 = atan2(y(n2), x(n2))

  lat1 = asin(z(n1))
  lat2 = asin(z(n2))

  area = 0.d0

  do while( n2 <= nmax )

    selectcase( arctyp(n1) )
    case( ARC_TYPE_NORMAL )
      area = area + area_sphere_polartri(londiff_rad(lon1,lon2), lat1, lat2, sgn_pole)&
                      * dir_lon(lon1,lon2) * sgn_pole
    case( ARC_TYPE_PARALLEL )
      area = area + area_sphere_polarrect(londiff_rad(lon1,lon2), z(n1), sgn_pole)&
                      * dir_lon(lon1,lon2) * sgn_pole
    case( ARC_TYPE_MERIDIAN )
      continue
    case default
      info = 1
      call errret(msg_invalid_value('arctyp('//str(n1)//')', arctyp(n1)), &
                  PRCNAM, MODNAM)
      return
    endselect

    n1 = n2
    n2 = n2 + 1

    lon1 = lon2
    lon2 = atan2(y(n2), x(n2))

    lat1 = lat2
    lat2 = asin(z(n2))
  enddo
end function area_sphere_polygon__cartesian
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
integer(4) function calc_area_sphere_parallel_to_parallel(&
    slon1, slon2, slat, tlon1, tlon2, tlat, &
    sgn_pole, &
    area, arc_rel &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_area_sphere_parallel_to_parallel'
  real(8)   , intent(in)  :: slon1, slon2, slat
  real(8)   , intent(in)  :: tlon1, tlon2, tlat
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  integer :: id_west, id_east
  integer :: sgn
  integer :: sdir_lon, tdir_lon
  real(8) :: west, east
  real(8) :: lon

  info = 0

  if( debug )then
    call logbgn(PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = ARC_REL_LAT_PARA_PARA_UNDEF
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  case( 1 )
    if( slat >= tlat )then
      arc_rel = ARC_REL_LAT_PARA_PARA_ABOVE

      if( debug )then
        call logmsg('s is above t')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  case( -1 )
    if( slat <= tlat )then
      arc_rel = ARC_REL_LAT_PARA_PARA_BELOW

      if( debug )then
        call logmsg('s is below t')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole))
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( calc_lon_range_shared(&
        slon1, slon2, tlon1, tlon2, &
        sdir_lon, tdir_lon, id_west, id_east, west, east) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call logmsg('Ranges of longit. not intersect')
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  case( 1 )
    arc_rel = ARC_REL_LAT_PARA_PARA_BELOW
  case( -1 )
    arc_rel = ARC_REL_LAT_PARA_PARA_ABOVE
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  lon = londiff_rad(west, east)
  area = area_sphere_rect(slat, tlat) * lon * sgn
  !-------------------------------------------------------------
  if( debug )then
    call logret(PRCNAM, MODNAM)
  endif
end function calc_area_sphere_parallel_to_parallel
!===============================================================
!
!===============================================================
integer(4) function calc_area_sphere_normal_to_parallel(&
    slon1, slat1, slon2, slat2, &
    sa, sb, sc, sconvex, slontop, slattop, &
    tlon1, tlon2, tlat, &
    sgn_pole, &
    area, arc_rel) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_area_sphere_normal_to_parallel'
  real(8)   , intent(in)  :: slon1, slat1, slon2, slat2
  real(8)   , intent(in)  :: sa, sb, sc
  integer(1), intent(in)  :: sconvex
  real(8)   , intent(in)  :: slontop, slattop
  real(8)   , intent(in)  :: tlon1, tlon2, tlat
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  real(8)    :: ssouth, snorth
  integer(1) :: sconvex_this
  integer(1) :: stat_lon_between
  integer    :: sdir_lon, tdir_lon
  integer    :: sgn
  integer    :: id_west, id_east
  real(8)    :: west, east
  real(8)    :: clon, clon1, clon2
  real(8)    :: lon
  real(8)    :: slat_west, slat_east
  real(8)    :: sarea, sarea1, sarea2
  real(8)    :: tarea, tarea1, tarea2
  real(8)    :: area1, area2

  info = 0
  if( debug )then
    call logbgn(PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  ! Return if area == 0
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = ARC_REL_LAT_NORM_PARA_UNDEF

  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Return if s is above t
  case( 1 )
    selectcase( sconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_UPWARD )
      ssouth = min(slat1,slat2)
    case( CONVEX_DOWNWARD )
      ssouth = slattop
    case default
      info = 1
      call errret(msg_invalid_value('sconvex', sconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    if( ssouth >= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ABOVE

      if( debug )then
        call logmsg('s is above t')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !       Return if s is below t
  case( -1 )
    selectcase( sconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_DOWNWARD )
      snorth = max(slat1,slat2)
    case( CONVEX_UPWARD )
      snorth = slattop
    case default
      info = 1
      call errret(msg_invalid_value('sconvex', sconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    if( snorth <= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_BELOW

      if( debug )then
        call logmsg('s is below t')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  ! Calc. shared range of longit.
  !-------------------------------------------------------------
  if( calc_lon_range_shared(&
        slon1, slon2, tlon1, tlon2, &
        sdir_lon, tdir_lon, id_west, id_east, west, east) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call logmsg('Ranges of longit. not intersect')
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif

  selectcase( id_west )
  case( 1 )
    slat_west = slat1
  case( 2 )
    slat_west = slat2
  case( 3, 4 )
    selectcase( sdir_lon )
    case( 1 )
      if( intersection_sphere_normal_meridian(&
            slon1, slat1, slon2, slat2, west, slat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )
      if( intersection_sphere_normal_meridian(&
            slon2, slat2, slon1, slat1, west, slat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('sdir_lon', sdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case default
    info = 1
    call errret(msg_invalid_value('id_west', id_west), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  selectcase( id_east )
  case( 1 )
    slat_east = slat1
  case( 2 )
    slat_east = slat2
  case( 3, 4 )
    selectcase( sdir_lon )
    case( 1 )
      if( intersection_sphere_normal_meridian(&
            slon1, slat1, slon2, slat2, east, slat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )
       if( intersection_sphere_normal_meridian(&
             slon2, slat2, slon1, slat1, east, slat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('sdir_lon', sdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case default
    info = 1
    call errret(msg_invalid_value('id_east', id_east), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  if( debug )then
    call logmsg('  west: ('//str((/west,slat_west/)*r2d,'f12.7',', ')//')')
    call logmsg('  east: ('//str((/east,slat_east/)*r2d,'f12.7',', ')//')')
  endif
  !-------------------------------------------------------------
  ! Judge if convex in the shared range of longit.
  !-------------------------------------------------------------
  selectcase( sconvex )
  case( CONVEX_MONOTONE )
    sconvex_this = sconvex
  case( CONVEX_UPWARD, &
        CONVEX_DOWNWARD )
    stat_lon_between = get_stat_lon_between(slontop, west, east)

    selectcase( stat_lon_between )
    case( STAT_LON_BETWEEN_INSIDE )
      sconvex_this = sconvex
    case( STAT_LON_BETWEEN_WEST, &
          STAT_LON_BETWEEN_EAST, &
          STAT_LON_BETWEEN_OUTSIDE )
      sconvex_this = CONVEX_MONOTONE
    case default
      info = 1
      call errret(msg_invalid_value('stat_lon_between', stat_lon_between), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case default
    info = 1
    call errret(msg_invalid_value('sconvex', sconvex), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  ! Judge relations of arcs
  !-------------------------------------------------------------
  selectcase( sconvex_this )
  case( CONVEX_MONOTONE )
    if( min(slat_west, slat_east) >= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ABOVE
    elseif( max(slat_west, slat_east) <= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_BELOW
    elseif( slat_west < tlat .and. tlat < slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD
    elseif( slat_west > tlat .and. tlat > slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  case( CONVEX_UPWARD )
    if( min(slat_west, slat_east) >= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ABOVE
    elseif( slattop <= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_BELOW
    elseif( slat_west < tlat .and. tlat <= slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD
    elseif( slat_west >= tlat .and. tlat > slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD
    elseif( slat_west < tlat .and. tlat > slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_UPWARD
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  case( CONVEX_DOWNWARD )
    if( slattop >= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ABOVE
    elseif( max(slat_west, slat_east) <= tlat )then
      arc_rel = ARC_REL_LAT_NORM_PARA_BELOW
    elseif( slat_west <= tlat .and. tlat < slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD
    elseif( slat_west > tlat .and. tlat >= slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD
    elseif( slat_west > tlat .and. tlat < slat_east )then
      arc_rel = ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_DOWNWARD
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  case default
    info = 1
    call errret(msg_invalid_value('sconvex_this', sconvex_this), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  if( debug )then
    call logmsg('arc_rel: '//str(str_arc_rel_lat(arc_rel)))
  endif
  !-------------------------------------------------------------
  ! Calc. area
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Calc. area(s) of the zone(s) above s and below t
  case( 1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is above t
    !       No area
    case( ARC_REL_LAT_NORM_PARA_ABOVE )
      area = 0.d0

      call pdbg_no('Case: s is above t')
    !-----------------------------------------------------------
    ! Case: s is below t
    !       Area is west to east
    case( ARC_REL_LAT_NORM_PARA_BELOW )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is below t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is west to intersection
    case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t downward
    !       Area is intersection to east
    case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s (convex upward) intersects with t twice
    !       Area is between intersections
    case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polartri(lon, tlat, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex upward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s (convex downward) intersects with t twice
    !       Area is edges to intersections
    case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea1 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea2 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Case: Two intersections (convex downward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('arc_rel', arc_rel), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Southward
  !       Calc. area(s) of the zone(s) below s and above t
  case( -1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is below t
    !       No area
    case( ARC_REL_LAT_NORM_PARA_BELOW )
      area = 0.d0

      call pdbg_no('Case: s is below t')
    !-----------------------------------------------------------
    ! Case: s is above t
    !       Area is west to east
    case( ARC_REL_LAT_NORM_PARA_ABOVE )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is above t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t downward
    !       Area is west to intersection
    case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is intersection to east
    case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s (convex upward) intersects with t twice
    !       Area is between intersections
    case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polartri(lon, tlat, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex upward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: Two intersections (downward)
    !       Area is edges to intersections
    case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea1 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      if( intersection_sphere_normal_parallel3(&
            sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea2 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Case: Two intersections (convex downward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('arc_rel', arc_rel), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  if( debug )then
    call logret(PRCNAM, MODNAM)
  endif
end function calc_area_sphere_normal_to_parallel
!===============================================================
!
!===============================================================
integer(4) function calc_area_sphere_parallel_to_normal(&
    slon1, slon2, slat, &
    tlon1, tlat1, tlon2, tlat2, &
    ta, tb, tc, tconvex, tlontop, tlattop, &
    sgn_pole, &
    area, arc_rel &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_area_sphere_parallel_to_normal'
  real(8)   , intent(in)  :: slon1, slon2, slat
  real(8)   , intent(in)  :: tlon1, tlat1, tlon2, tlat2
  real(8)   , intent(in)  :: ta, tb, tc
  integer(1), intent(in)  :: tconvex
  real(8)   , intent(in)  :: tlontop, tlattop
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  real(8)    :: tsouth, tnorth
  integer(1) :: tconvex_this
  integer(1) :: stat_lon_between
  integer    :: sdir_lon, tdir_lon
  integer    :: sgn
  integer    :: id_west, id_east
  real(8)    :: west, east
  real(8)    :: clon, clon1, clon2
  real(8)    :: lon
  real(8)    :: tlat_west, tlat_east
  real(8)    :: sarea, sarea1, sarea2
  real(8)    :: tarea, tarea1, tarea2
  real(8)    :: area1, area2

  info = 0
  if( debug )then
    call logbgn(PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = ARC_REL_LAT_PARA_NORM_UNDEF
  !-------------------------------------------------------------
  !
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Return if s is above t
  case( 1 )
    selectcase( tconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_DOWNWARD )
      tnorth = max(tlat1,tlat2)
    case( CONVEX_UPWARD )
      tnorth = tlattop
    case default
      info = 1
      call errret(msg_invalid_value('tconvex', tconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    if( slat >= tnorth )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ABOVE

      if( debug )then
        call logmsg('s is above t')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !       Return if s is below t
  case( -1 )
    selectcase( tconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_UPWARD )
      tsouth = min(tlat1,tlat2)
    case( CONVEX_DOWNWARD )
      tsouth = tlattop
    case default
      info = 1
      call errret(msg_invalid_value('tconvex', tconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    if( slat <= tsouth )then
      arc_rel = ARC_REL_LAT_PARA_NORM_BELOW

      if( debug )then
        call logmsg('t is above s')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  ! Calc. shared range of longit.
  !-------------------------------------------------------------
  if( calc_lon_range_shared(&
        slon1, slon2, tlon1, tlon2, &
        sdir_lon, tdir_lon, id_west, id_east, west, east) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call logmsg('Ranges of longit. not intersect')
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif

  selectcase( id_west )
  case( 1, 2 )
    selectcase( tdir_lon )
    case( 1 )
      if( intersection_sphere_normal_meridian(&
            tlon1, tlat1, tlon2, tlat2, west, tlat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )
      if( intersection_sphere_normal_meridian(&
            tlon2, tlat2, tlon1, tlat1, west, tlat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('tdir_lon', tdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case( 3 )
    tlat_west = tlat1
  case( 4 )
    tlat_west = tlat2
  case default
    info = 1
    call errret(msg_invalid_value('id_west', id_west), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  selectcase( id_east )
  case( 1, 2 )
    selectcase( tdir_lon )
    case( 1 )
      if( intersection_sphere_normal_meridian(&
            tlon1, tlat1, tlon2, tlat2, east, tlat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )
      if( intersection_sphere_normal_meridian(&
            tlon2, tlat2, tlon1, tlat1, east, tlat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('tdir_lon', tdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case( 3 )
    tlat_east = tlat1
  case( 4 )
    tlat_east = tlat2
  case default
    info = 1
    call errret(msg_invalid_value('id_east', id_east), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  if( debug )then
    call logmsg('  west: ('//str((/west,tlat_west/)*r2d,'f12.7',', ')//')')
    call logmsg('  east: ('//str((/east,tlat_east/)*r2d,'f12.7',', ')//')')
  endif
  !-------------------------------------------------------------
  ! Judge if convex in the shared range of longit.
  !-------------------------------------------------------------
  selectcase( tconvex )
  case( CONVEX_MONOTONE )
    tconvex_this = tconvex
  case( CONVEX_UPWARD, &
        CONVEX_DOWNWARD )
    stat_lon_between = get_stat_lon_between(tlontop, west, east)

    selectcase( stat_lon_between )
    case( STAT_LON_BETWEEN_INSIDE )
      tconvex_this = tconvex
    case( STAT_LON_BETWEEN_WEST, &
          STAT_LON_BETWEEN_EAST, &
          STAT_LON_BETWEEN_OUTSIDE )
      tconvex_this = CONVEX_MONOTONE
    case default
      info = 1
      call errret(msg_invalid_value('stat_lon_between', stat_lon_between), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case default
    info = 1
    call errret(msg_invalid_value('tconvex', tconvex), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  ! Judge relations of arcs
  !-------------------------------------------------------------
  selectcase( tconvex_this )
  case( CONVEX_MONOTONE )
    if( min(tlat_west, tlat_east) >= slat )then
      arc_rel = ARC_REL_LAT_PARA_NORM_BELOW
    elseif( max(tlat_west, tlat_east) <= slat )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ABOVE
    elseif( tlat_west < slat .and. slat < tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD
    elseif( tlat_west > slat .and. slat > tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  case( CONVEX_UPWARD )
    if( min(tlat_west, tlat_east) >= slat )then
      arc_rel = ARC_REL_LAT_PARA_NORM_BELOW
    elseif( tlattop <= slat )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ABOVE
    elseif( tlat_west < slat .and. slat <= tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD
    elseif( tlat_west >= slat .and. slat > tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD
    elseif( tlat_west < slat .and. slat > tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_UPWARD
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  case( CONVEX_DOWNWARD )
    if( tlattop >= slat )then
      arc_rel = ARC_REL_LAT_PARA_NORM_BELOW
    elseif( max(tlat_west, tlat_east) <= slat )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ABOVE
    elseif( tlat_west <= slat .and. slat < tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD
    elseif( tlat_west > slat .and. slat >= tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD
    elseif( tlat_west > slat .and. slat < tlat_east )then
      arc_rel = ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_DOWNWARD
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  case default
    info = 1
    call errret(msg_invalid_value('tconvex_this', tconvex_this), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  ! Calc. area
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Calc. area(s) of the zone(s) above s and below t
  case( 1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is above t
    !       No area
    case( ARC_REL_LAT_PARA_NORM_ABOVE )
      area = 0.d0

      call pdbg_no('Case: s is above t')
    !-----------------------------------------------------------
    ! Case: s is below t
    !       Area is west to east
    case( ARC_REL_LAT_PARA_NORM_BELOW )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is below t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s downward
    !       Area is west to intersection
    case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, -tdir_lon, sin(slat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s upward
    !       Area is intersection to east
    case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, +tdir_lon, sin(slat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex upward) intersects with s twice
    !       Area is between intersections
    case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, +tdir_lon, sin(slat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, -tdir_lon, sin(slat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, slat, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex upward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex downward) intersects with s twice
    !       Area is edges to intersections
    case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, -tdir_lon, sin(slat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea1 = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, +tdir_lon, sin(slat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea2 = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Case: Two intersections (convex downward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('arc_rel', arc_rel), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Southward
  !       Calc. area(s) of the zone(s) below s and above t
  case( -1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is below t
    !       No area
    case( ARC_REL_LAT_PARA_NORM_BELOW )
      area = 0.d0

      call pdbg_no('Case: s is below t')
    !-----------------------------------------------------------
    ! Case: s is above t
    !       Area is west to east
    case( ARC_REL_LAT_PARA_NORM_ABOVE )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is above t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s upward
    !       Area is west to intersection
    case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, +tdir_lon, sin(slat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s downward
    !       Area is intersection to east
    case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, -tdir_lon, sin(slat), west, east, clon) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex downward) intersects with s twice
    !       Area is between intersections
    case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, -tdir_lon, sin(slat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, +tdir_lon, sin(slat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, slat, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex downward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex upward) intersects with s twice
    !       Area is edges to intersections
    case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_UPWARD )
      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, +tdir_lon, sin(slat), west, east, clon1) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea1 = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      if( intersection_sphere_normal_parallel3(&
            ta, tb, tc, -tdir_lon, sin(slat), west, east, clon2) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea2 = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Two intersections (convex upward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('arc_rel', arc_rel), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  if( debug )then
    call logret(PRCNAM, MODNAM)
  endif
end function calc_area_sphere_parallel_to_normal
!===============================================================
! Calc. area above s and below t
!===============================================================
integer(4) function calc_area_sphere_normal_to_normal(&
    sx1, sy1, sz1, sx2, sy2, sz2, &
    slon1, slat1, slon2, slat2, &
    sa, sb, sc, &
    sconvex, slattop, &
    tx1, ty1, tz1, tx2, ty2, tz2, &
    tlon1, tlat1, tlon2, tlat2, &
    ta, tb, tc, &
    tconvex, tlattop, &
    sgn_pole, &
    area, arc_rel) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_area_sphere_normal_to_normal'
  real(8)   , intent(in)  :: sx1, sy1, sz1, sx2, sy2, sz2
  real(8)   , intent(in)  :: tx1, ty1, tz1, tx2, ty2, tz2
  real(8)   , intent(in)  :: slon1, slat1, slon2, slat2
  real(8)   , intent(in)  :: tlon1, tlat1, tlon2, tlat2
  real(8)   , intent(in)  :: sa, sb, sc
  real(8)   , intent(in)  :: ta, tb, tc
  integer(1), intent(in)  :: sconvex
  real(8)   , intent(in)  :: slattop
  integer(1), intent(in)  :: tconvex
  real(8)   , intent(in)  :: tlattop
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  real(8) :: ssouth, snorth, tsouth, tnorth
  real(8) :: west, east
  real(8) :: lon
  integer :: id_west, id_east
  real(8) :: slat_west, slat_east
  real(8) :: tlat_west, tlat_east
  integer :: sdir_lon, tdir_lon
  integer :: sgn
  real(8) :: clon, clat
  real(8) :: sarea, tarea
  integer :: stat

  info = 0
  if( debug )then
    call logbgn(PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = ARC_REL_LAT_NORM_NORM_UNDEF
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Return if s is above t
  case( 1 )
    selectcase( sconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_UPWARD )
      ssouth = min(slat1,slat2)
    case( CONVEX_DOWNWARD )
      ssouth = slattop
    case default
      info = 1
      call errret(msg_invalid_value('sconvex', sconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    selectcase( tconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_DOWNWARD )
      tnorth = max(tlat1,tlat2)
    case( CONVEX_UPWARD )
      tnorth = tlattop
    case default
      info = 1
      call errret(msg_invalid_value('tconvex', tconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    if( ssouth >= tnorth )then
      arc_rel = ARC_REL_LAT_NORM_NORM_ABOVE

      if( debug )then
        call logmsg('s is above t')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !   Return if s is below t
  case( -1 )
    selectcase( sconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_DOWNWARD )
      snorth = max(slat1,slat2)
    case( CONVEX_UPWARD )
      snorth = slattop
    case default
      info = 1
      call errret(msg_invalid_value('sconvex', sconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    selectcase( tconvex )
    case( CONVEX_MONOTONE, &
          CONVEX_UPWARD )
      tsouth = min(tlat1,tlat2)
    case( CONVEX_DOWNWARD )
      tsouth = tlattop
    case default
      info = 1
      call errret(msg_invalid_value('tconvex', tconvex), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect

    if( snorth <= tsouth )then
      arc_rel = ARC_REL_LAT_NORM_NORM_BELOW

      if( debug )then
        call logmsg('s is below t')
        call logret(PRCNAM, MODNAM)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  ! Calc. shared range of longit.
  !-------------------------------------------------------------
  if( calc_lon_range_shared(&
        slon1, slon2, tlon1, tlon2, &
        sdir_lon, tdir_lon, id_west, id_east, west, east) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call logmsg('Ranges of longit. not intersect')
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif

  selectcase( id_west )
  case( 1, 2 )
    if( id_west == 1 )then
      slat_west = slat1
    else
      slat_west = slat2
    endif

    selectcase( tdir_lon )
    case( 1 )  ! tlon1 < tlon2
      if( intersection_sphere_normal_meridian(&
            tlon1, tlat1, tlon2, tlat2, west, tlat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )  ! tlon2 < tlon1
      if( intersection_sphere_normal_meridian(&
            tlon2, tlat2, tlon1, tlat1, west, tlat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('tdir_lon', tdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case( 3, 4 )
    if( id_west == 3 )then
      tlat_west = tlat1
    else
      tlat_west = tlat2
    endif

    selectcase( sdir_lon )
    case( 1 )  ! slon1 < slon2
      if( intersection_sphere_normal_meridian(&
            slon1, slat1, slon2, slat2, west, slat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )  ! slon2 < slon1
      if( intersection_sphere_normal_meridian(&
            slon2, slat2, slon1, slat1, west, slat_west) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('sdir_lon', sdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case default
    info = 1
    call errret(msg_invalid_value('id_west', id_west), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  selectcase( id_east )
  case( 1, 2 )
    if( id_east == 1 )then
      slat_east = slat1
    else
      slat_east = slat2
    endif

    selectcase( tdir_lon )
    case( 1 )  ! tlon1 < tlon2
      if( intersection_sphere_normal_meridian(&
            tlon1, tlat1, tlon2, tlat2, east, tlat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )  ! tlon2 < tlon1
      if( intersection_sphere_normal_meridian(&
            tlon2, tlat2, tlon1, tlat1, east, tlat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('tdir_lon', tdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case( 3, 4 )
    if( id_east == 3 )then
      tlat_east = tlat1
    else
      tlat_east = tlat2
    endif

    selectcase( sdir_lon )
    case( 1 )  ! slon1 < slon2
      if( intersection_sphere_normal_meridian(&
            slon1, slat1, slon2, slat2, east, slat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case( -1 )  ! slon2 < slon1
      if( intersection_sphere_normal_meridian(&
            slon2, slat2, slon1, slat1, east, slat_east) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('sdir_lon', sdir_lon), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
  case default
    info = 1
    call errret(msg_invalid_value('id_east', id_east), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  if( debug )then
    call logmsg('id_west: '//str(id_west)//' id_east: '//str(id_east))
    call logmsg('west: '//str(west*r2d,'f12.7')//' east: '//str(east*r2d,'f12.7'))
    call logmsg('slat: '//str((/slat_west,slat_east/)*r2d,'f12.7',', '))
    call logmsg('tlat: '//str((/tlat_west,tlat_east/)*r2d,'f12.7',', '))
    call logmsg('dir_lon s: '//str(sdir_lon)//' t: '//str(tdir_lon))
  endif
  !-------------------------------------------------------------
  ! Calc. area
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Calc. area of the zone above s and below t
  case( 1 )
    !-----------------------------------------------------------
    ! Case: s is above t
    !       No area
    if( slat_west >= tlat_west .and. slat_east >= tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_ABOVE

      call pdbg_no('Case: s is above t')
    !-----------------------------------------------------------
    ! Case: s is below t
    !       Area is west to east
    elseif( slat_west <= tlat_west .and. slat_east <= tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_BELOW

      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is below t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is west to intersection
    elseif( slat_west < tlat_west .and. slat_east > tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_INTERSECTION_UPWARD

      if( intersection_sphere_normal_normal(&
            sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
            tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
            west, east, clon, clat, stat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( stat == 0 )then
        lon = londiff_rad(west, clon)
        sarea = area_sphere_polartri(lon, slat_west, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_west, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (upward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: s intersects with s downward
    !       Area is intersection to east
    elseif( slat_west > tlat_west .and. slat_east < tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_INTERSECTION_DOWNWARD

      if( intersection_sphere_normal_normal(&
            sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
            tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
            west, east, clon, clat, stat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( stat == 0 )then
        lon = londiff_rad(east, clon)
        sarea = area_sphere_polartri(lon, slat_east, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_east, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (downward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !       Calc. area of the zone below s and above t
  case( -1 )
    !-----------------------------------------------------------
    ! Case: s is below t
    !       No area
    if( slat_west <= tlat_west .and. slat_east <= tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_BELOW

      call pdbg_no('Case: s is below t')
    !-----------------------------------------------------------
    ! Case: s is above t
    !       Area is west to east
    elseif( slat_west >= tlat_west .and. slat_east >= tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_ABOVE

      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is above t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t downward
    !       Area is west to intersection
    elseif( slat_west > tlat_west .and. slat_east < tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_INTERSECTION_DOWNWARD

      if( intersection_sphere_normal_normal(&
            sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
            tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
            west, east, clon, clat, stat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( stat == 0 )then
        lon = londiff_rad(west, clon)
        sarea = area_sphere_polartri(lon, slat_west, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_west, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (downward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is intersection to east
    elseif( slat_west < tlat_west .and. slat_east > tlat_east )then
      arc_rel = ARC_REL_LAT_NORM_NORM_INTERSECTION_UPWARD

      if( intersection_sphere_normal_normal(&
            sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
            tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
            west, east, clon, clat, stat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( stat == 0 )then
        lon = londiff_rad(east, clon)
        sarea = area_sphere_polartri(lon, slat_east, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_east, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (upward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nNone of the cases applied.', &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole), &
                PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect
  !-------------------------------------------------------------
  if( debug )then
    call logret(PRCNAM, MODNAM)
  endif
end function calc_area_sphere_normal_to_normal
!===============================================================
!
!===============================================================
subroutine pdbg_no(msg_case)
  implicit none
  character(*), intent(in) :: msg_case

  if( debug )then
    call logmsg(msg_case)
  endif
end subroutine pdbg_no
!===============================================================
!
!===============================================================
subroutine pdbg_isct0(msg_case, sarea, tarea, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: sarea, tarea
  real(8)     , intent(in) :: area

  if( debug )then
    call logmsg(msg_case//&
              '\n  sarea: '//str(sarea,'es20.13')//&
              '\n  tarea: '//str(tarea,'es20.13')//&
              '\n  area : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct0
!===============================================================
!
!===============================================================
subroutine pdbg_isct1(msg_case, clon, sarea, tarea, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: clon
  real(8)     , intent(in) :: sarea, tarea
  real(8)     , intent(in) :: area

  if( debug )then
    call logmsg(msg_case//&
              '\n  clon : '//str(clon*r2d,'f12.7')//&
              '\n  sarea: '//str(sarea,'es20.13')//&
              '\n  tarea: '//str(tarea,'es20.13')//&
              '\n  area : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct1
!===============================================================
!
!===============================================================
subroutine pdbg_isct2_centered(&
    msg_case, clon1, clon2, sarea, tarea, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: clon1, clon2
  real(8)     , intent(in) :: sarea, tarea
  real(8)     , intent(in) :: area

  if( debug )then
    call logmsg(msg_case//&
              '\n  clon1: '//str(clon1*r2d,'12.7')//&
              '\n  clon2: '//str(clon2*r2d,'12.7')//&
              '\n  sarea: '//str(sarea,'es20.13')//&
              '\n  tarea: '//str(tarea,'es20.13')//&
              '\n  area : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct2_centered
!===============================================================
!
!===============================================================
subroutine pdbg_isct2_splitted(&
    msg_case, &
    clon1, sarea1, tarea1, area1, &
    clon2, sarea2, tarea2, area2, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: clon1, clon2
  real(8)     , intent(in) :: sarea1, sarea2, tarea1, tarea2
  real(8)     , intent(in) :: area1, area2
  real(8)     , intent(in) :: area

  if( debug )then
    call logmsg(msg_case//&
              '\n  clon1 : '//str(clon1*r2d,'12.7')//&
              '\n  sarea1: '//str(sarea1,'es20.13')//&
              '\n  tarea1: '//str(tarea1,'es20.13')//&
              '\n  area1 : '//str(area1,'es20.13')//&
              '\n  clon2 : '//str(clon2*r2d,'12.7')//&
              '\n  sarea2: '//str(sarea2,'es20.13')//&
              '\n  tarea2: '//str(tarea2,'es20.13')//&
              '\n  area2 : '//str(area2,'es20.13')//&
              '\n  area  : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct2_splitted
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
integer(4) function area_sphere_intersection_polygon_polygon(&
    spos, sx, sy, sz, slon, slat, styp, sa, sb, sc, &
    sn_pole, sconvex, slontop, slattop, sarea, &
    tpos, tx, ty, tz, tlon, tlat, ttyp, ta, tb, tc, &
    tn_pole, tconvex, tlontop, tlattop, tarea, &
    area &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_intersection_polygon_polygon'
  integer(1), intent(in) :: spos
  real(8)   , intent(in) :: sx(:), sy(:), sz(:)
  real(8)   , intent(in) :: slon(:), slat(:)
  integer(1), intent(in) :: styp(:)
  real(8)   , intent(in) :: sa(:), sb(:), sc(:)
  integer(4), intent(in) :: sn_pole
  integer(1), intent(in) :: sconvex(:)
  real(8)   , intent(in) :: slontop(:), slattop(:)
  real(8)   , intent(in) :: sarea
  integer(1), intent(in) :: tpos
  real(8)   , intent(in) :: tx(:), ty(:), tz(:)
  real(8)   , intent(in) :: tlon(:), tlat(:)
  integer(1), intent(in) :: ttyp(:)
  real(8)   , intent(in) :: ta(:), tb(:), tc(:)
  integer(4), intent(in) :: tn_pole
  integer(1), intent(in) :: tconvex(:)
  real(8)   , intent(in) :: tlontop(:), tlattop(:)
  real(8)   , intent(in) :: tarea
  real(8)   , intent(out) :: area

  integer    :: sgn_pole
  integer    :: snmax, sn, sn_
  integer    :: tnmax, tn, tn_
  real(8)    :: slon1, slon2, slat_pole
  real(8)    :: tlon1, tlon2, tlat_pole
  integer(1) :: arc_rel_lat
  real(8)    :: area_add
  logical    :: is_s_above_t, is_s_below_t
  logical    :: is_confirmed

  info = 0
  if( debug )then
    call logbgn(PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( slat(1) > rad_0deg )then
    sgn_pole = 1
  else
    sgn_pole = -1
  endif

  area = 0.d0
  is_s_above_t = .true.
  is_s_below_t = .true.

  tnmax = size(tlon)
  snmax = size(slon)

  if( debug )then
    call logmsg('sgn_pole: '//str(sgn_pole))
  endif
  !-------------------------------------------------------------
  ! Calc. intersection area
  !-------------------------------------------------------------
  if( debug )then
    call logent('Calculating intersection area', PRCNAM, MODNAM)
  endif

  tn_ = tnmax
  tn  = 1
  do while( tn <= tnmax )
    call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                      tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

    selectcase( ttyp(tn_) )
    !-----------------------------------------------------------
    ! Case: t is normal
    case( ARC_TYPE_NORMAL )
      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_normal_to_normal(&
                sx(sn_), sy(sn_), sz(sn_), sx(sn), sy(sn), sz(sn), &
                slon(sn_), slat(sn_), slon(sn), slat(sn), &
                sa(sn_), sb(sn_), sc(sn_), &
                sconvex(sn_), slattop(sn_), &
                tx(tn_), ty(tn_), tz(tn_), tx(tn), ty(tn), tz(tn), &
                tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                ta(tn_), tb(tn_), tc(tn_), &
                tconvex(tn_), tlattop(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          if( update_rel_lat_polygons_norm_norm(&
                arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_normal(&
                slon(sn_), slon(sn), slat(sn_), &
                tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                ta(tn_), tb(tn_), tc(tn_), &
                tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          if( update_rel_lat_polygons_para_norm(&
                arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('styp(sn_)', styp(sn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/
    !-----------------------------------------------------------
    ! Case: t is parallel
    case( ARC_TYPE_PARALLEL )
      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_normal_to_parallel(&
                slon(sn_), slat(sn_), slon(sn), slat(sn), &
                sa(sn_), sb(sn_), sc(sn_), &
                sconvex(sn_), slontop(sn_), slattop(sn_), &
                tlon(tn_), tlon(tn), tlat(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          if( update_rel_lat_polygons_norm_para(&
                arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon(sn_), slon(sn), slat(sn_), &
                tlon(tn_), tlon(tn), tlat(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          if( update_rel_lat_polygons_para_para(&
                arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('styp('//str(sn_)//')', styp(sn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/
    !-----------------------------------------------------------
    ! Case: t is meridian
    case( ARC_TYPE_MERIDIAN )
      continue
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('ttyp('//str(tn_)//')', ttyp(tn_)), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
    !-----------------------------------------------------------
    tn_ = tn
    tn  = tn + 1

    call pdbg_ext_arc()
  enddo  ! tn/

  if( debug )then
    call logmsg('is_s_below_t: '//str(is_s_below_t))
    call logmsg('is_s_above_t: '//str(is_s_above_t))
    call logmsg('area: '//str(area,'es20.13'))
    call logext('Calculating intersection area', PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  ! Confirm area for special cases that
  !   one is above, below, inside or outside the other
  !-------------------------------------------------------------
  if( debug )then
    call logent('Confirming intersection area for special cases'//&
                ' that arcs do not intersect', PRCNAM, MODNAM)
  endif

  is_confirmed = .false.

  if( is_s_below_t )then
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( sn_pole /= 0 .and. slat(1) > rad_0deg )then
      continue
    !-----------------------------------------------------------
    ! Case: s has south pole
    elseif( sn_pole /= 0 .and. slat(1) < rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has south pole
      if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      elseif( tpos == POLYGON_POSITION_POLAR .and. &
              tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: s includes north pole
    !       s includes t
    elseif( spos == POLYGON_POSITION_POLAR .and. &
            slat(1) > rad_0deg )then
      area = tarea
      is_confirmed = .true.
    !-----------------------------------------------------------
    ! Case: s includes south pole
    elseif( spos == POLYGON_POSITION_POLAR .and. &
            slat(1) < rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has south pole
      if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      elseif( tpos == POLYGON_POSITION_POLAR .and. &
              tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t has south pole
      if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      elseif( tpos == POLYGON_POSITION_POLAR .and. &
              tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
      !---------------------------------------------------------
    endif

  elseif( is_s_above_t )then
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( sn_pole /= 0 .and. slat(1) > rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has north pole
      if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      elseif( tpos == POLYGON_POSITION_POLAR .and. &
              tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: s has south pole
    elseif( sn_pole /= 0 .and. slat(1) < rad_0deg )then
      continue
    !-----------------------------------------------------------
    ! Case: s includes north pole
    elseif( spos == POLYGON_POSITION_POLAR .and. &
            slat(1) > rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has north pole
      if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      elseif( tpos == POLYGON_POSITION_POLAR .and. &
              tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: s includes south pole
    elseif( spos == POLYGON_POSITION_POLAR .and. &
            slat(1) < rad_0deg )then
      area = tarea
      is_confirmed = .true.
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t has north pole
      if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      elseif( tpos == POLYGON_POSITION_POLAR .and. &
              tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    endif
  endif

  if( debug )then
    if( is_confirmed )then
      call logmsg('Confirmed: '//str(area,'es20.13'))
    else
      call logmsg('Not confirmed')
    endif
    call logext('Confirming intersection area for special cases'//&
                ' that arcs do not intersect', PRCNAM, MODNAM)
  endif

  if( is_confirmed )then
    if( debug )then
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! Update area for special case that
  ! grid includes pole or has pole on the vertex
  !-------------------------------------------------------------
  if( debug )then
    call logent('Updating intersection area for special case that'//&
                ' pole is on the polygon', PRCNAM, MODNAM)
  endif

  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  case( 1 )
    !-----------------------------------------------------------
    ! Case: t has north pole on its vertex
    if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
      if( debug )then
        call logent('t has north pole on its vertex', PRCNAM, MODNAM)
      endif

      tlat_pole = rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_normal_to_parallel(&
                slon(sn_), slat(sn_), slon(sn), slat(sn), &
                sa(sn_), sb(sn_), sc(sn_), &
                sconvex(sn_), slontop(sn_), slattop(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon(sn_), slon(sn), slat(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('styp('//str(sn_)//')', styp(sn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call logext('t has north pole on its vertex', PRCNAM, MODNAM)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: t includes north pole
    if( tpos == POLYGON_POSITION_POLAR .and. &
        tlat(1) > rad_0deg )then
      if( debug )then
        call logent('t includes north pole', PRCNAM, MODNAM)
      endif

      tlat_pole = rad_90deg

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        tlon1 = eastern(slon(sn_), slon(sn))
        tlon2 = western(slon(sn_), slon(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_normal_to_parallel(&
                slon(sn_), slat(sn_), slon(sn), slat(sn), &
                sa(sn_), sb(sn_), sc(sn_), &
                sconvex(sn_), slontop(sn_), slattop(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon(sn_), slon(sn), slat(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('styp('//str(sn_)//')', styp(sn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call logext('t includes north pole', PRCNAM, MODNAM)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s has south pole
    if( sn_pole /= 0 .and. slat(1) < rad_0deg )then
      if( debug )then
        call logent('s has south pole on its vertex', PRCNAM, MODNAM)
      endif

      slat_pole = -rad_90deg

      if( sn_pole == 1 )then
        slon1 = slon(snmax)
        slon2 = slon(sn_pole+1)
      elseif( sn_pole == snmax )then
        slon1 = slon(sn_pole-1)
        slon2 = slon(1)
      else
        slon1 = slon(sn_pole-1)
        slon2 = slon(sn_pole+1)
      endif

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_parallel_to_normal(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                ta(tn_), tb(tn_), tc(tn_), &
                tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlon(tn), tlat(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('ttyp('//str(tn_)//')', ttyp(tn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo ! tn/

      if( debug )then
        call logext('s has south pole on its vertex', PRCNAM, MODNAM)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s includes south pole
    if( spos == POLYGON_POSITION_POLAR .and. &
        slat(1) < rad_0deg )then
      if( debug )then
        call logent('s includes south pole', PRCNAM, MODNAM)
      endif

      slat_pole = -rad_90deg

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        slon1 = western(tlon(tn_), tlon(tn))
        slon2 = eastern(tlon(tn_), tlon(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_parallel_to_normal(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                ta(tn_), tb(tn_), tc(tn_), &
                tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlon(tn), tlat(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('ttyp('//str(tn_)//')', ttyp(tn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo  ! tn/

      if( debug )then
        call logext('s includes south pole', PRCNAM, MODNAM)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  case( -1 )
    !-----------------------------------------------------------
    ! Case: t has south pole
    if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
      if( debug )then
        call logent('t has south pole on its vertex', PRCNAM, MODNAM)
      endif

      tlat_pole = -rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_normal_to_parallel(&
                slon(sn_), slat(sn_), slon(sn), slat(sn), &
                sa(sn_), sb(sn_), sc(sn_), &
                sconvex(sn_), slontop(sn_), slattop(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon(sn_), slon(sn), slat(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('styp('//str(sn_)//')', styp(sn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call logext('t has south pole on its vertex', PRCNAM, MODNAM)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: t includes south pole
    if( tpos == POLYGON_POSITION_POLAR .and. &
        tlat(1) < rad_0deg )then
      if( debug )then
        call logent('t includes south pole', PRCNAM, MODNAM)
      endif

      tlat_pole = -rad_90deg

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        tlon1 = western(slon(sn_), slon(sn))
        tlon2 = eastern(slon(sn_), slon(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_normal_to_parallel(&
                slon(sn_), slat(sn_), slon(sn), slat(sn), &
                sa(sn_), sb(sn_), sc(sn_), &
                sconvex(sn_), slontop(sn_), slattop(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon(sn_), slon(sn), slat(sn_), &
                tlon1, tlon2, tlat_pole, &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('styp('//str(sn_)//')', styp(sn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call logext('t includes south pole', PRCNAM, MODNAM)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( sn_pole /= 0 .and. slat(1) > rad_0deg )then
      if( debug )then
        call logent('s has north pole on its vertex', PRCNAM, MODNAM)
      endif

      slat_pole = rad_90deg

      if( sn_pole == 1 )then
        slon1 = slon(snmax)
        slon2 = slon(sn_pole+1)
      elseif( sn_pole == snmax )then
        slon1 = slon(sn_pole-1)
        slon2 = slon(1)
      else
        slon1 = slon(sn_pole-1)
        slon2 = slon(sn_pole+1)
      endif

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_parallel_to_normal(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                ta(tn_), tb(tn_), tc(tn_), &
                tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlon(tn), tlat(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('ttyp('//str(tn_)//')', ttyp(tn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo ! tn/

      if( debug )then
        call logext('s has north pole on its vertex', PRCNAM, MODNAM)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s includes north pole
    if( spos == POLYGON_POSITION_POLAR .and. &
        slat(1) > rad_0deg )then
      if( debug )then
        call logent('s includes north pole', PRCNAM, MODNAM)
      endif

      slat_pole = rad_90deg

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        slon1 = eastern(tlon(tn_), tlon(tn))
        slon2 = western(tlon(tn_), tlon(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( ARC_TYPE_NORMAL )
          if( calc_area_sphere_parallel_to_normal(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                ta(tn_), tb(tn_), tc(tn_), &
                tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( ARC_TYPE_PARALLEL )
          if( calc_area_sphere_parallel_to_parallel(&
                slon1, slon2, slat_pole, &
                tlon(tn_), tlon(tn), tlat(tn_), &
                sgn_pole, &
                area_add, arc_rel_lat) /= 0 )then
            info = 1
            call errret('', PRCNAM, MODNAM)
            if( debug ) call logret(PRCNAM, MODNAM)
            return
          endif

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( ARC_TYPE_MERIDIAN )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          info = 1
          call errret(msg_invalid_value('ttyp('//str(tn_)//')', ttyp(tn_)), &
                      PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo  ! tn/

      if( debug )then
        call logext('s includes north pole', PRCNAM, MODNAM)
      endif
    endif
  endselect

  if( debug )then
    call logmsg('area: '//str(area,'es20.13'))
    call logext('s includes north pole', PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  if( debug )then
    call logret(PRCNAM, MODNAM)
  endif
end function area_sphere_intersection_polygon_polygon
!===============================================================
!
!===============================================================
integer(4) function area_sphere_intersection_latlon_polygon(&
    swest, seast, ssouth, snorth, sarea, &
    tpos, tlon, tlat, ttyp, ta, tb, tc, &
    tn_pole, tconvex, tlontop, tlattop, &
    area &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'area_sphere_intersection_latlon_polygon'
  real(8)   , intent(in) :: swest, seast, ssouth, snorth
  real(8)   , intent(in) :: sarea
  integer(1), intent(in) :: tpos
  real(8)   , intent(in) :: tlon(:), tlat(:)  ! (tnmax)
  integer(1), intent(in) :: ttyp(:)
  real(8)   , intent(in) :: ta(:), tb(:), tc(:)
  integer(4), intent(in) :: tn_pole
  integer(1), intent(in) :: tconvex(:)
  real(8)   , intent(in) :: tlontop(:), tlattop(:)
  real(8)   , intent(out) :: area

  integer    :: sgn_pole
  integer    :: tnmax, tn, tn_
  real(8)    :: tlon1, tlon2, tlat_pole
  integer(1) :: arc_rel_lat
  real(8)    :: area_add_south, area_add_north
  logical    :: is_s_above_t, is_s_below_t
  logical    :: is_confirmed

  info = 0
  if( debug )then
    call logbgn(PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sgn_pole = 1

  area = 0.d0
  is_s_below_t = .true.
  is_s_above_t = .true.

  tnmax = size(tlon)

  if( debug )then
    call logmsg('sgn_pole: '//str(sgn_pole))
  endif
  !-------------------------------------------------------------
  ! Calc. intersection area
  !-------------------------------------------------------------
  if( debug )then
    call logent('Calculating intersection area', PRCNAM, MODNAM)
  endif

  tn_ = tnmax
  tn  = 1
  do while( tn <= tnmax )
    call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                      tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

    selectcase( ttyp(tn_) )
    !-----------------------------------------------------------
    ! Case: t is normal
    case( ARC_TYPE_NORMAL )
      !---------------------------------------------------------
      ! South
      !---------------------------------------------------------
      call pdbg_ent_arc('s', 1, arc_type_parallel, CONVEX_MONOTONE, &
                        swest, ssouth, seast, ssouth)

      if( calc_area_sphere_parallel_to_normal(&
            swest, seast, ssouth, &
            tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
            ta(tn_), tb(tn_), tc(tn_), &
            tconvex(tn_), tlontop(tn_), tlattop(tn_), &
            sgn_pole, &
            area_add_south, arc_rel_lat) /= 0 )then
          info = 1
          call errret('', PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endif

      if( update_rel_lat_polygons_para_norm(&
            arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      call pdbg_ext_arc()
      !---------------------------------------------------------
      ! North
      !---------------------------------------------------------
      call pdbg_ent_arc('s', 3, arc_type_parallel, CONVEX_MONOTONE, &
                        seast, snorth, swest, snorth)

      if( calc_area_sphere_parallel_to_normal(&
            seast, swest, snorth, &
            tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
            ta(tn_), tb(tn_), tc(tn_), &
            tconvex(tn_), tlontop(tn_), tlattop(tn_), &
            sgn_pole, &
            area_add_north, arc_rel_lat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( update_rel_lat_polygons_para_norm(&
            arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      call pdbg_ext_arc()
      !---------------------------------------------------------
      area = area + (area_add_south + area_add_north)
    !-----------------------------------------------------------
    ! Case: Parallel
    case( ARC_TYPE_PARALLEL )
      if( calc_area_sphere_parallel_to_parallel(&
            swest, seast, ssouth, &
            tlon(tn_), tlon(tn), tlat(tn_), &
            sgn_pole, &
            area_add_south, arc_rel_lat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( update_rel_lat_polygons_para_para(&
            arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( calc_area_sphere_parallel_to_parallel(&
            seast, swest, snorth, &
            tlon(tn_), tlon(tn), tlat(tn_), &
            sgn_pole, &
            area_add_north, arc_rel_lat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( update_rel_lat_polygons_para_para(&
            arc_rel_lat, is_s_below_t, is_s_above_t) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      area = area + (area_add_south + area_add_north)
    !-----------------------------------------------------------
    ! Case: Meridian
    case( ARC_TYPE_MERIDIAN )
      continue
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('ttyp('//str(tn_)//')', ttyp(tn_)), &
                  PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endselect
    !-----------------------------------------------------------
    tn_ = tn
    tn  = tn + 1

    call pdbg_ext_arc()
  enddo  ! tn/

  if( debug )then
    call logmsg('area: '//str(area,'es20.13'))
    call logext('Calculating intersection area', PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  ! Confirm area for special cases that
  !   one is above, below, inside or outside the other
  !-------------------------------------------------------------
  if( debug )then
    call logent('Confirming intersection area for special cases'//&
              ' that arcs do not intersect', PRCNAM, MODNAM)
  endif

  is_confirmed = .false.

  if( is_s_below_t )then
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( snorth == rad_90deg )then
      continue
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      if( tpos == POLYGON_POSITION_POLAR .and. &
          tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: t has south pole
      elseif( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    endif
  elseif( is_s_above_t )then
    !-----------------------------------------------------------
    ! Case: s has south pole
    if( ssouth == -rad_90deg )then
      continue
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      if( tpos == POLYGON_POSITION_POLAR .and. &
          tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: t has north pole
      elseif( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    endif
  endif

  if( debug )then
    if( is_confirmed )then
      call logmsg('Confirmed: '//str(area,'es20.13'))
    else
      call logmsg('Not confirmed')
    endif
    call logext('Confirming intersection area for special cases'//&
              ' that arcs do not intersect', PRCNAM, MODNAM)
  endif

  if( is_confirmed )then
    if( debug )then
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! Update area for special case that
  ! grid includes pole or has pole on the vertex
  !-------------------------------------------------------------
  if( debug )then
    call logent('Updating intersection area for special case that'//&
              ' pole is on the polygon', PRCNAM, MODNAM)
  endif

  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  case( 1 )
    !-----------------------------------------------------------
    ! Case: t has north pole
    if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
      if( debug )then
        call logent('Case: t has north pole on its vertex', PRCNAM, MODNAM)
      endif

      tlat_pole = rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      if( calc_area_sphere_parallel_to_parallel(&
            swest, seast, ssouth, &
            tlon1, tlon2, tlat_pole, &
            sgn_pole, &
            area_add_south, arc_rel_lat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( calc_area_sphere_parallel_to_parallel(&
            seast, swest, snorth, &
            tlon1, tlon2, tlat_pole, &
            sgn_pole, &
            area_add_north, arc_rel_lat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      area = area + (area_add_south + area_add_north)

      if( debug )then
        call logext('Case: t has north pole on its vertex', PRCNAM, MODNAM)
      endif
    !-----------------------------------------------------------
    ! Case: t includes north pole
    elseif( tpos == POLYGON_POSITION_POLAR .and. &
            tlat(1) > rad_0deg )then
      if( debug )then
        call logent('t includes north pole', PRCNAM, MODNAM)
      endif

      area = area + sarea

      if( debug )then
        call logext('t includes north pole', PRCNAM, MODNAM)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  case( -1 )
    !-----------------------------------------------------------
    ! Case: t has south pole
    if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
      if( debug )then
        call logent('t has south pole on its vertex', PRCNAM, MODNAM)
      endif

      tlat_pole = -rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      if( calc_area_sphere_parallel_to_parallel(&
            swest, seast, ssouth, &
            tlon1, tlon2, tlat_pole, &
            sgn_pole, &
            area_add_south, arc_rel_lat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      if( calc_area_sphere_parallel_to_parallel(&
            seast, swest, snorth, &
            tlon1, tlon2, tlat_pole, &
            sgn_pole, &
            area_add_north, arc_rel_lat) /= 0 )then
        info = 1
        call errret('', PRCNAM, MODNAM)
        if( debug ) call logret(PRCNAM, MODNAM)
        return
      endif

      area = area + (area_add_south + area_add_north)

      if( debug )then
        call logext('t has south pole on its vertex', PRCNAM, MODNAM)
      endif
    !-----------------------------------------------------------
    ! Case: t includes south pole
    elseif( tpos == POLYGON_POSITION_POLAR .and. &
            tlat(1) < rad_0deg )then
      if( debug )then
        call logent('t includes south pole', PRCNAM, MODNAM)
      endif

      area = area + sarea

      if( debug )then
        call logext('t includes south pole', PRCNAM, MODNAM)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('sgn_pole', sgn_pole))
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endselect

  if( debug )then
    call logext('t includes south pole', PRCNAM, MODNAM)
  endif
  !-------------------------------------------------------------
  if( debug )then
    call logret(PRCNAM, MODNAM)
  endif
end function area_sphere_intersection_latlon_polygon
!===============================================================
!
!===============================================================
integer(4) function update_rel_lat_polygons_norm_norm(&
    arc_rel_lat_norm_norm, is_former_below_latter, is_former_above_latter) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_rel_lat_polygons_norm_norm'
  integer(1), intent(in)    :: arc_rel_lat_norm_norm
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  info = 0
  !-------------------------------------------------------------
  selectcase( arc_rel_lat_norm_norm )
  case( ARC_REL_LAT_NORM_NORM_UNDEF )
    continue
  case( ARC_REL_LAT_NORM_NORM_BELOW )  ! s is below t
    is_former_above_latter = .false.
  case( ARC_REL_LAT_NORM_NORM_ABOVE )  ! s is above t
    is_former_below_latter = .false.
  case( ARC_REL_LAT_NORM_NORM_INTERSECTION_UPWARD, &
        ARC_REL_LAT_NORM_NORM_INTERSECTION_DOWNWARD )
    is_former_below_latter = .false.
    is_former_above_latter = .false.
  case default
    info = 1
    call errret(msg_invalid_value('arc_rel_lat_norm_norm', arc_rel_lat_norm_norm), &
                PRCNAM, MODNAM)
    return
  endselect
end function update_rel_lat_polygons_norm_norm
!===============================================================
!
!===============================================================
integer(4) function update_rel_lat_polygons_para_norm(&
    arc_rel_lat_para_norm, is_former_below_latter, is_former_above_latter) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_rel_lat_polygons_para_norm'
  integer(1), intent(in)    :: arc_rel_lat_para_norm
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  info = 0
  !-------------------------------------------------------------
  selectcase( arc_rel_lat_para_norm )
  case( ARC_REL_LAT_PARA_NORM_UNDEF )
    continue
  case( ARC_REL_LAT_PARA_NORM_BELOW )
    is_former_above_latter = .false.
  case( ARC_REL_LAT_PARA_NORM_ABOVE )
    is_former_below_latter = .false.
  case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD, &
        ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD, &
        ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_UPWARD, &
        ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
    is_former_below_latter = .false.
    is_former_above_latter = .false.
  case default
    info = 1
    call errret(msg_invalid_value('arc_rel_lat_para_norm', arc_rel_lat_para_norm), &
                PRCNAM, MODNAM)
    return
  endselect
end function update_rel_lat_polygons_para_norm
!===============================================================
!
!===============================================================
integer(4) function update_rel_lat_polygons_norm_para(&
    arc_rel_lat_norm_para, is_former_below_latter, is_former_above_latter) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_rel_lat_polygons_norm_para'
  integer(1), intent(in)    :: arc_rel_lat_norm_para
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  info = 0
  !-------------------------------------------------------------
  selectcase( arc_rel_lat_norm_para )
  case( ARC_REL_LAT_NORM_PARA_UNDEF )
    continue
  case( ARC_REL_LAT_NORM_PARA_BELOW )
    is_former_above_latter = .false.
  case( ARC_REL_LAT_NORM_PARA_ABOVE )
    is_former_below_latter = .false.
  case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD, &
        ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD, &
        ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_UPWARD, &
        ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
    is_former_below_latter = .false.
    is_former_above_latter = .false.
  case default
    info = 1
    call errret(msg_invalid_value('arc_rel_lat_norm_para', arc_rel_lat_norm_para), &
                PRCNAM, MODNAM)
    return
  endselect
end function update_rel_lat_polygons_norm_para
!===============================================================
!
!===============================================================
integer(4) function update_rel_lat_polygons_para_para(&
    arc_rel_lat_para_para, is_former_below_latter, is_former_above_latter) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_rel_lat_polygons_para_para'
  integer(1), intent(in)    :: arc_rel_lat_para_para
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  info = 0
  !-------------------------------------------------------------
  selectcase( ARC_REL_LAT_PARA_PARA )
  case( ARC_REL_LAT_PARA_PARA_UNDEF )
    continue
  case( ARC_REL_LAT_PARA_PARA_BELOW )
    is_former_above_latter = .false.
  case( ARC_REL_LAT_PARA_PARA_ABOVE )
    is_former_below_latter = .false.
  case default
    info = 1
    call errret(msg_invalid_value('arc_rel_lat_para_para', arc_rel_lat_para_para), &
                PRCNAM, MODNAM)
    return
  endselect
end function update_rel_lat_polygons_para_para
!===============================================================
!
!===============================================================
subroutine pdbg_ent_arc(nam, n, arctyp, convex, lon1, lat1, lon2, lat2)
  implicit none
  character(*), intent(in) :: nam
  integer     , intent(in) :: n
  integer(1)  , intent(in) :: arctyp
  integer(1)  , intent(in) :: convex
  real(8)     , intent(in) :: lon1, lat1
  real(8)     , intent(in) :: lon2, lat2
  character(26) :: s1, s2

  if( debug )then
    call logent(nam//'('//str(n)//') type '//str(str_arctyp_long(arctyp))//&
                ' convex '//str(str_convex_long(convex)))

    if( abs(lat1) == rad_90deg )then
      s1 = str('-',12)//', '//str(lat1*r2d,'f12.7')
    else
      s1 = str((/lon1,lat1/)*r2d,'f12.7',', ')
    endif

    if( abs(lat2) == rad_90deg )then
      s2 = str('-',12)//', '//str(lat2*r2d,'f12.7')
    else
      s2 = str((/lon2,lat2/)*r2d,'f12.7',', ')
    endif

    call logmsg('('//s1//') - ('//s2//')')
  endif
end subroutine pdbg_ent_arc
!===============================================================
!
!===============================================================
subroutine pdbg_ext_arc()
  implicit none

  if( debug )then
    call logext()
  endif
end subroutine pdbg_ext_arc
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
real(8) function dist_sphere_0d(lon1, lat1, lon2, lat2) result(dist)
  implicit none
  real(8), intent(in) :: lon1, lat1, lon2, lat2  ![rad]

  dist = acos(min(1.d0, max(-1.d0, &
              sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(londiff_rad(lon1, lon2))))) * 2.d0
end function dist_sphere_0d
!===============================================================
!
!===============================================================
end module lib_math_sphere
