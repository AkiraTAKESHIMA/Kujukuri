module lib_proj_sinusoidal
  use lib_const, only: pi
  implicit none
  private
!---------------------------------------------------------------
  public :: convLonLatToSinusoidal
  public :: convSinusoidalToLonLat
!---------------------------------------------------------------
  interface convLonLatToSinusoidal
    module procedure convLonLatToSinusoidal_0d
    module procedure convLonLatToSinusoidal_1d
    module procedure convLonLatToSinusoidal_2d
  end interface

  interface convSinusoidalToLonLat
    module procedure convSinusoidalToLonLat_0d
    module procedure convSinusoidalToLonLat_1d
    module procedure convSinusoidalToLonLat_2d
  end interface
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine convLonLatToSinusoidal_0d(lon, lat, x, y, lon_0, a, e)
  implicit none
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8) :: b

  call calcParams(a, e, b)

  x = a*0.5d0 * (lon - lon_0) * cos(lat)
  y = b*0.5d0 * lat / pi
end subroutine convLonLatToSinusoidal_0d
!===============================================================
!
!===============================================================
subroutine convLonLatToSinusoidal_1d(lon, lat, x, y, lon_0, a, e)
  implicit none
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:)
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8) :: b

  call calcParams(a, e, b)

  x(:) = a*0.5d0 * (lon(:) - lon_0) * cos(lat(:))
  y(:) = b*0.5d0 * lat(:) / pi
end subroutine convLonLatToSinusoidal_1d
!===============================================================
!
!===============================================================
subroutine convLonLatToSinusoidal_2d(lon, lat, x, y, lon_0, a, e)
  implicit none
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:)
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8) :: b

  call calcParams(a, e, b)

  x(:,:) = a*0.5d0 * (lon(:,:) - lon_0) * cos(lat(:,:))
  y(:,:) = b*0.5d0 * lat(:,:) / pi
end subroutine convLonLatToSinusoidal_2d
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
subroutine convSinusoidalToLonLat_0d(x, y, lon, lat, lon_0, a, e)
  implicit none
  real(8), intent(in)  :: x, y
  real(8), intent(out) :: lon, lat
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8) :: b

  call calcParams(a, e, b)

  lat = y * pi / (b*0.5d0)
  lon = x / (cos(lat) * a*0.5d0) + lon_0
end subroutine convSinusoidalToLonLat_0d
!===============================================================
!
!===============================================================
subroutine convSinusoidalToLonLat_1d(x, y, lon, lat, lon_0, a, e)
  implicit none
  real(8), intent(in)  :: x(:), y(:)
  real(8), intent(out) :: lon(:), lat(:)
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8) :: b

  call calcParams(a, e, b)

  lat(:) = y(:) * pi / (b*0.5d0)
  lon(:) = x(:) / (cos(lat(:)) * a*0.5d0) + lon_0
end subroutine convSinusoidalToLonLat_1d
!===============================================================
!
!===============================================================
subroutine convSinusoidalToLonLat_2d(x, y, lon, lat, lon_0, a, e)
  implicit none
  real(8), intent(in)  :: x(:,:), y(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8) :: b

  call calcParams(a, e, b)

  lat(:,:) = y(:,:) * pi / (b*0.5d0)
  lon(:,:) = x(:,:) / (cos(lat(:,:)) * a*0.5d0) + lon_0
end subroutine convSinusoidalToLonLat_2d
!===============================================================
!
!===============================================================





!===============================================================
!
!===============================================================
subroutine calcParams(a, e, b)
  implicit none
  real(8), intent(in)  :: a  ! Semi-major axis
  real(8), intent(in)  :: e  ! Eccentricity
  real(8), intent(out) :: b  ! Semi-minor axis

  b = a * sqrt(1-e**2)
end subroutine calcParams
!===============================================================
!
!===============================================================
end module lib_proj_sinusoidal
