module mod_base
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: reshape_slo_ij2idx
  public :: reshape_slo_idx2ij
  public :: reshape_slo_idx2ij4

  public :: int2char
  public :: slonlat
  !-------------------------------------------------------------
contains
!===============================================================
! 2D -> 1D (ij2idx)
!===============================================================
subroutine reshape_slo_ij2idx( a, a_idx )
  implicit none
  real(8), intent(in) :: a(:,:)
  real(8), intent(out) :: a_idx(:)
  integer k

  do k = 1, slo_count
    a_idx(k) = a(slo_idx2i(k), slo_idx2j(k))
  enddo
end subroutine reshape_slo_ij2idx
!===============================================================
! 1D -> 2D (idx2ij)
!===============================================================
subroutine reshape_slo_idx2ij( a_idx, a )
  implicit none
  real(8), intent(in) :: a_idx(:)
  real(8), intent(out) :: a(:,:)

  integer k

  a(:, :) = 0.d0
  do k = 1, slo_count
    a(slo_idx2i(k), slo_idx2j(k)) = a_idx(k)
  enddo
end subroutine reshape_slo_idx2ij
!===============================================================
! 2D -> 1D (ij2idx)
!===============================================================
subroutine reshape_slo_idx2ij4( a_idx, a )
  implicit none
  real(8), intent(in) :: a_idx(:,:)  !(I4,slo_count)
  real(8), intent(out) :: a(:,:,:)  !(I4,nx,ny)

  integer :: i, k

  a(:, :, :) = 0.d0
  do i = 1, I4
    do k = 1, slo_count
      a(i, slo_idx2i(k), slo_idx2j(k)) = a_idx(i, k)
    enddo
  enddo
end subroutine reshape_slo_idx2ij4
!===============================================================
!
!===============================================================
subroutine int2char(n, c)
  implicit none
  integer :: n
  character(6) :: c

  write(c,"(i6.6)") n
end subroutine int2char
!===============================================================
!
!===============================================================
function slonlat(lon, lat) result(s)
  implicit none
  real(8), intent(in) :: lon, lat
  character(:), allocatable :: s

  allocate(character(1) :: s)
  s = str(lon,'f12.7')//','//str(lat,'f11.7')
end function slonlat
!===============================================================
!
!===============================================================
end module mod_base
