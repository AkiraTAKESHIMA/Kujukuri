module mod_slope
  implicit none

contains
!===============================================================
!
!===============================================================
subroutine funcs(static, state, forcing, ptr_y, fs_idx, qs_idx, workspace)
  implicit none
  type(static_), intent(in) :: static
  type(state_), intent(in) :: state
  type(forcing_), intent(in) :: forcing
  real(8), pointer :: ptr_y(:)
  real(8), intent(out) :: fs_idx(:)
  real(8), intent(out) :: qs_idx(:,:)
  type(workspace_), intent(inout) :: workspace

  type(static_slope_), pointer :: sts
  type(workspace_slope_), pointer :: wss
  integer :: k

  sts => static%slope
  wss => workspace%slope

  do k = 1, static%slope%nSlo
    call qs_calc_grid(k, sts%hs_idx, wss%qs_idx(:,k))
  enddo

  do k = 1, sts%nSlo
    wss%fs_idx(k) = forcing%qp_idx(k) - sum(wss%qs_idx(:,k))
  enddo

  do k = 1, sts%nSlo
    do l = 1, sts%lmax
      kk = sts%down_slo_idx(l, k)
      if( kk < 0 ) cycle
      fs_idx(kk) = fs_idx(kk) + qs_idx(l, k) * sts%area_slo_idx(k) / sts%area_slo_idx(kk)
    enddo
  enddo
end subroutine funcs
!===============================================================
!
!===============================================================
subroutine calc_state_sum(state, dy, yt)
  implicit none
  type(state_), intent(in) :: state
  real(8), intent(in) :: dy(:)
  real(8), intent(out) :: yt(:)

  yt(:) = state%slope%hs_idx(:) + dy(:)
end subroutine calc_state_sum
!===============================================================
!
!===============================================================
end module mod_slope
