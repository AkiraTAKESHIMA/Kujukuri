module mod_base
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: ij2idx
  public :: idx2ij

  public :: hr2vr
  public :: vr2hr
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface ij2idx
    module procedure ij2idx__slope
  end interface

  interface idx2ij
    module procedure idx2ij__slope_single
    module procedure idx2ij__slope_multi
  end interface

  interface hr2vr
    module procedure hr2vr_rect
  end interface

  interface vr2hr
    module procedure vr2hr_rect
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine ij2idx__slope(slope, a, a_idx)
  implicit none
  type(static_slope_), intent(in) :: slope
  real(8), intent(in) :: a(:,:)  !(nx,ny)
  real(8), intent(out) :: a_idx(:)  !(nGrid)

  integer :: k

  do k = 1, slope%nGrid
    a_idx(k) = a(slope%idx2i(k),slope%idx2j(k))
  enddo
end subroutine ij2idx__slope
!===============================================================
!
!===============================================================
subroutine idx2ij__slope_single(slope, a_idx, a)
  implicit none
  type(static_slope_), intent(in) :: slope
  real(8), intent(in) :: a_idx(:)  !(nGrid)
  real(8), intent(out) :: a(:,:)  !(nx,ny)

  integer :: k

  a(:,:) = 0.d0
  do k = 1, slope%nGrid
    a(slope%idx2i(k),slope%idx2j(k)) = a_idx(k)
  enddo
end subroutine idx2ij__slope_single
!===============================================================
!
!===============================================================
subroutine idx2ij__slope_multi(slope, a_idx, a)
  implicit none
  type(static_slope_), intent(in) :: slope
  real(8), intent(in) :: a_idx(:,:)  !(:,nGrid)
  real(8), intent(out) :: a(:,:,:)  !(:,nx,ny)

  integer :: k

  a(:,:,:) = 0.d0
  do k = 1, slope%nGrid
    a(:,slope%idx2i(k),slope%idx2j(k)) = a_idx(:,k)
  enddo
end subroutine idx2ij__slope_multi
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
subroutine hr2vr_rect(area, hr, vr)
  implicit none
  real(8), intent(in) :: area
  real(8), intent(in) :: hr
  real(8), intent(out) :: vr

  vr = hr * area
end subroutine hr2vr_rect
!===============================================================
!
!===============================================================
subroutine vr2hr_rect(area, vr, hr)
  implicit none
  real(8), intent(in) :: area
  real(8), intent(in) :: vr
  real(8), intent(out) :: hr

  hr = vr / area
end subroutine vr2hr_rect
!===============================================================
!
!===============================================================
end module mod_base
