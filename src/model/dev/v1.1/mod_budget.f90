module mod_budget
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use lib_io
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calc_storage
  public :: calc_storage_inbalance
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_budget'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calc_storage(&
  slope, river, &
  hr_idx, hs, hg, gampt_ff, &
  vro_idx, &
  sr, ss, si, sg, sout &
)
  use mod_base, only: &
    hr2vr
  implicit none
  type(static_slope_), intent(in) :: slope
  type(static_river_), intent(in) :: river
  real(8), intent(in)  :: hr_idx(:), hs(:,:), hg(:,:)
  real(8), intent(in)  :: gampt_ff(:,:)
  real(8), intent(in)  :: vro_idx(:)
  real(8), intent(out) :: sr, ss, si, sg, sout

  integer :: k, ix, iy
  real(8) :: vr_temp

  sr = 0.d0
  ss = 0.d0
  si = 0.d0
  sg = 0.d0

  do k = 1, river%nCh
    call hr2vr(river%area_idx(k), hr_idx(k), vr_temp)
    sr = sr + vr_temp
  enddo

  do iy = 1, slope%ny
  do ix = 1, slope%nx
    if( slope%domain_mask(ix,iy) == DOMAIN__OUTSIDE ) cycle

    ss = ss + hs(ix,iy) * slope%area(ix,iy)
    si = si + gampt_ff(ix,iy) * slope%area(ix,iy)
    sg = sg - hg(ix,iy) * slope%gammag_idx(slope%ij2idx(ix,iy)) * slope%area(ix,iy)  ! storage deficit
  enddo  ! ix/
  enddo  ! iy/

  sout = sout + sum(vro_idx(:))
end subroutine calc_storage
!===============================================================
!
!===============================================================
subroutine calc_storage_inbalance(&
  prcp_sum, aevp_sum, &
  sout, sr, ss, si, sg, sinit, &
  sinbl &
)
  implicit none
  real(8), intent(in) :: prcp_sum, aevp_sum
  real(8), intent(in) :: sout, sr, ss, si, sg, sinit
  real(8), intent(out) :: sinbl

  sinbl = (prcp_sum - aevp_sum - sout) + (sinit - (sr + ss + si + sg))
end subroutine calc_storage_inbalance
!===============================================================
!
!===============================================================
end module mod_budget
