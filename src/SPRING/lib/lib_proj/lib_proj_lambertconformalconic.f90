module lib_proj_lambertconformalconic
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: conv_lonlat_to_lcc
  public :: conv_lcc_to_lonlat
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface conv_lonlat_to_lcc
    module procedure conv_lonlat_to_lcc__0d
    module procedure conv_lonlat_to_lcc__1d
    module procedure conv_lonlat_to_lcc__2d
  end interface

  interface conv_lcc_to_lonlat
    module procedure conv_lcc_to_lonlat__0d
    module procedure conv_lcc_to_lonlat__1d
    module procedure conv_lcc_to_lonlat__2d
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_proj_lambertconformalconic'

  integer, parameter :: iteration_ulim_default = 10
  real(8), parameter :: thresh_convergence_default = 1.d-12
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine conv_lonlat_to_lcc__0d(&
    lon, lat, x, y, lat1, lat2, lon_0, lat_0, a, e)
  implicit none
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y
  real(8), intent(in) :: lat1, lat2
  real(8), intent(in) :: lon_0, lat_0
  real(8), intent(in) :: a
  real(8), intent(in) :: e
  real(8) :: n
  real(8) :: F

  call calc_params(lat1, lat2, lat_0, e, n, F)

  call convert(lon, lat, x, y, lon_0, a, e, n, F)
end subroutine conv_lonlat_to_lcc__0d
!===============================================================
!
!===============================================================
subroutine conv_lonlat_to_lcc__1d(&
    lon, lat, x, y, lat1, lat2, lon_0, lat_0, a, e)
  implicit none
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:)
  real(8), intent(in) :: lat1, lat2
  real(8), intent(in) :: lon_0, lat_0
  real(8), intent(in) :: a
  real(8), intent(in) :: e
  real(8) :: n
  real(8) :: F
  integer :: i

  call calc_params(lat1, lat2, lat_0, e, n, F)

  do i = 1, size(x,1)
    call convert(lon(i), lat(i), x(i), y(i), lon_0, a, e, n, F)
  enddo
end subroutine conv_lonlat_to_lcc__1d
!===============================================================
!
!===============================================================
subroutine conv_lonlat_to_lcc__2d(&
    lon, lat, x, y, lat1, lat2, lon_0, lat_0, a, e)
  implicit none
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:)
  real(8), intent(in) :: lat1, lat2
  real(8), intent(in) :: lon_0, lat_0
  real(8), intent(in) :: a
  real(8), intent(in) :: e
  real(8) :: n
  real(8) :: F
  integer :: i, j

  call calc_params(lat1, lat2, lat_0, e, n, F)

  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      call convert(lon(i,j), lat(i,j), x(i,j), y(i,j), lon_0, a, e, n, F)
    enddo
  enddo
end subroutine conv_lonlat_to_lcc__2d
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
subroutine conv_lcc_to_lonlat__0d(&
    x, y, lon, lat, lat1, lat2, lon_0, lat_0, &
    a, e, iteration, convergence)
  implicit none
  real(8), intent(in)  :: x, y
  real(8), intent(out) :: lon, lat
  real(8), intent(in) :: lat1, lat2
  real(8), intent(in) :: lon_0, lat_0
  real(8), intent(in) :: a
  real(8), intent(in) :: e
  integer, intent(in), optional :: iteration
  real(8), intent(in), optional :: convergence
  real(8) :: n
  real(8) :: F
  integer :: iteration_ulim
  real(8) :: thresh_convergence
  !-------------------------------------------------------------
  if( present(iteration) )then
    iteration_ulim = iteration
  else
    iteration_ulim = ITERATION_ULIM_DEFAULT
  endif

  if( present(convergence) )then
    thresh_convergence = convergence
  else
    thresh_convergence = THRESH_CONVERGENCE_DEFAULT
  endif
  !-------------------------------------------------------------
  call calc_params(lat1, lat2, lat_0, e, n, F)

  call iconvert(x, y, lon, lat, &
                lon_0, a, e, n, F, &
                iteration_ulim, thresh_convergence)
end subroutine conv_lcc_to_lonlat__0d
!===============================================================
!
!===============================================================
subroutine conv_lcc_to_lonlat__1d(&
    x, y, lon, lat, lat1, lat2, lon_0, lat_0, &
    a, e, iteration, convergence)
  implicit none
  real(8), intent(in)  :: x(:), y(:)
  real(8), intent(out) :: lon(:), lat(:)
  real(8), intent(in) :: lat1, lat2
  real(8), intent(in) :: lon_0, lat_0
  real(8), intent(in) :: a
  real(8), intent(in) :: e
  integer, intent(in), optional :: iteration
  real(8), intent(in), optional :: convergence
  real(8) :: n
  real(8) :: F
  integer :: iteration_ulim
  real(8) :: thresh_convergence
  integer :: i
  !-------------------------------------------------------------
  if( present(iteration) )then
    iteration_ulim = iteration
  else
    iteration_ulim = ITERATION_ULIM_DEFAULT
  endif

  if( present(convergence) )then
    thresh_convergence = convergence
  else
    thresh_convergence = THRESH_CONVERGENCE_DEFAULT
  endif
  !-------------------------------------------------------------
  call calc_params(lat1, lat2, lat_0, e, n, F)

  do i = 1, size(x,1)
    call iconvert(x(i), y(i), lon(i), lat(i), &
                  lon_0, a, e, n, F, &
                  iteration_ulim, thresh_convergence)
  enddo
end subroutine conv_lcc_to_lonlat__1d
!===============================================================
!
!===============================================================
subroutine conv_lcc_to_lonlat__2d(&
    x, y, lon, lat, lat1, lat2, lon_0, lat_0, &
    a, e, iteration, convergence)
  implicit none
  real(8), intent(in)  :: x(:,:), y(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)
  real(8), intent(in) :: lat1, lat2
  real(8), intent(in) :: lon_0, lat_0
  real(8), intent(in) :: a
  real(8), intent(in) :: e
  integer, intent(in), optional :: iteration
  real(8), intent(in), optional :: convergence
  real(8) :: n
  real(8) :: F
  integer :: iteration_ulim
  real(8) :: thresh_convergence
  integer :: i, j
  !-------------------------------------------------------------
  if( present(iteration) )then
    iteration_ulim = iteration
  else
    iteration_ulim = ITERATION_ULIM_DEFAULT
  endif

  if( present(convergence) )then
    thresh_convergence = convergence
  else
    thresh_convergence = THRESH_CONVERGENCE_DEFAULT
  endif
  !-------------------------------------------------------------
  call calc_params(lat1, lat2, lat_0, e, n, F)

  do j = 1, size(x,2)
    do i = 1, size(x,1)
      call iconvert(x(i,j), y(i,j), lon(i,j), lat(i,j), &
                    lon_0, a, e, n, F, &
                    iteration_ulim, thresh_convergence)
    enddo
  enddo
end subroutine conv_lcc_to_lonlat__2d
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
subroutine convert(lon, lat, x, y, lon_0, a, e, n, F)
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8), intent(in)  :: n
  real(8), intent(in)  :: F
  real(8) :: c
  real(8) :: r

  c = n * (lon - lon_0)
  r = a * F * t(lat, e)**n

  x = r * cos(c)
  y = r * sin(c)
end subroutine convert
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
subroutine iconvert(&
    x, y, lon, lat, &
    lon_0, a, e, n, F, &
    iteration_ulim, thresh_convergence)
  implicit none
  real(8), intent(in)  :: x, y
  real(8), intent(out) :: lon, lat
  real(8), intent(in)  :: lon_0
  real(8), intent(in)  :: a
  real(8), intent(in)  :: e
  real(8), intent(in)  :: n
  real(8), intent(in)  :: F
  integer, intent(in)  :: iteration_ulim
  real(8), intent(in)  :: thresh_convergence
  real(8) :: r_
  real(8) :: t_
  real(8) :: c_
  real(8) :: lat_prev
  integer :: iter

  r_ = sign( sqrt( y**2 + x**2 ), n )
  t_ = (r_/(a*F)) **(1.d0/n)
  c_ = atan( y / x )

  lon = c_/n + lon_0

  lat = pi*0.5d0 - 2*atan(t_)
  lat_prev = lat * 2
  iter = 0
  do while( abs(lat - lat_prev) > abs(lat)*thresh_convergence .and. iter < iteration_ulim )
    iter = iter + 1
    lat_prev = lat
    lat = pi*0.5d0 - 2*atan( t_ * ( (1-e*sin(lat_prev)) / (1+e*sin(lat_prev)) ) ** (e*0.5d0) )
  enddo
end subroutine iconvert
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
subroutine calc_params(lat1, lat2, lat_0, e, n, F)
  real(8), intent(in)  :: lat1, lat2
  real(8), intent(in)  :: lat_0
  real(8), intent(in)  :: e
  real(8), intent(out) :: n
  real(8), intent(out) :: F
  real(8) :: m1, m2
  real(8) :: t0, t1, t2

  m1 = m(lat1, e)
  m2 = m(lat2, e)

  t0 = t(lat_0, e)
  t1 = t(lat1, e)
  t2 = t(lat2, e)

  n = (log(m1)-log(m2)) / (log(t1)-log(t2))
  F = m1 / (n * t1**n)
end subroutine calc_params
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
real(8) function m(lat, e)
  implicit none
  real(8), intent(in) :: lat
  real(8), intent(in) :: e

  m = cos(lat) / sqrt(1-(e*sin(lat))**2)
end function m
!===============================================================
!
!===============================================================
real(8) function t(lat, e)
  implicit none
  real(8), intent(in) :: lat
  real(8), intent(in) :: e

  t = tan(pi*0.25d0 - lat*0.5d0) / ((1-e*sin(lat))/(1+e*sin(lat)))**(e*0.5d0)
end function t
!===============================================================
!
!===============================================================
end module lib_proj_lambertconformalconic
