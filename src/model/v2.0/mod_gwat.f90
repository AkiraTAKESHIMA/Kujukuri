module mod_gwat
  use def_const
  use def_static
  implicit none

contains
!===============================================================
!
!===============================================================
subroutine advance_gwat(&
  time_start, &
  hs_idx, gampt_ff_idx, hg_idx &
)
  implicit none
  real(8), intent(in) :: time_start
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(inout) :: hg_idx(:)

  real(8) :: time, time_end
  real(8) :: ddt
!  real(8) :: errmax
!  integer :: k

  time = time_start
  time_end = time + dt_model
  ddt = dt_model

    !call gw_recharge( hs_idx, gampt_ff_idx, hg_idx, rech_hs_tsas )
    call gw_recharge( hs_idx, gampt_ff_idx, hg_idx )
print*, 'hg', minval(hg_idx), maxval(hg_idx)

!  do while( time < time_end )
!  enddo
end subroutine advance_gwat
!===============================================================
!
!===============================================================
subroutine gw_recharge(&
  hs_idx, gampt_ff_idx, hg_idx & !, rech_hs_tsas &
)
  implicit none
  real(8), intent(inout) :: hs_idx(slo_count)
  real(8), intent(inout) :: gampt_ff_idx(slo_count)
  real(8), intent(inout) :: hg_idx(slo_count)
!  real(8), intent(inout) :: rech_hs_tsas(slo_count)

  real(8) rech, rech_ga, rech_hs
  integer k

  rech = 0.d0
  rech_ga = 0.d0
  rech_hs = 0.d0

  do k = 1, slo_count
    rech = ksg_idx(k) * dt_model
    if( hg_idx(k) .lt. 0.d0 ) cycle

    if( hg_idx(k) * gammag_idx(k) .lt. rech ) rech = hg_idx(k) * gammag_idx(k)

    if( rech .lt. gampt_ff_idx(k) .and. ksv_idx(k) .gt. 0.d0 ) then
      rech_ga = rech
      rech_hs = 0.d0
    else
      if( ksv_idx(k) .gt. 0.d0) rech_ga = gampt_ff_idx(k)
      if( rech - rech_ga .lt. hs_idx(k) ) then
        rech_hs = rech - rech_ga
      else
        rech_hs = hs_idx(k)
      endif
    endif
    rech = rech_ga + rech_hs

    hs_idx(k) = hs_idx(k) - rech_hs
    if( ksv_idx(k) .gt. 0.d0 ) gampt_ff_idx(k) = gampt_ff_idx(k) - rech_ga
    if( gammag_idx(k) .gt. 0.d0) hg_idx(k) = hg_idx(k) - rech / gammag_idx(k)
!    rech_hs_tsas(k) = rech_hs / dble(dt)
  enddo
end subroutine gw_recharge
!===============================================================
!
!===============================================================
end module mod_gwat
