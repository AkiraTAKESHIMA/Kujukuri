module mod_river
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
  public :: prep_river
  public :: advance_river
  public :: outflow_river
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------

  ! Static data
  real(8), allocatable :: riv_area_idx(:)

  ! Workspace
  real(8), allocatable :: vr_idx(:)
  real(8), allocatable :: hr_idx(:)
  real(8), allocatable :: qr_tmp(:,:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_river()
  use def_runge
  implicit none

  allocate(riv_area_idx(riv_count))
  allocate(vr_idx(riv_count))
  allocate(hr_idx(riv_count))
  allocate(qr_tmp(maxval(channel(:)%nCh_down),riv_count))

  riv_area_idx(:) = channel(:)%area

  ! Runge-Kutta
  allocate(fr1(riv_count), fr2(riv_count), fr3(riv_count), &
           fr4(riv_count), fr5(riv_count), fr6(riv_count))
  allocate(vr_tmp(riv_count), vr_err(riv_count), hr_err(riv_count))
end subroutine prep_river
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
subroutine advance_river(&
    time_start, &
    hr_idx &
)
  use def_runge
  use mod_section, only: &
    hr2vr, &
    vr2hr
  use mod_budget, only: &
    update_cpssto_riv
  implicit none
  real(8), intent(in) :: time_start
  real(8), intent(inout) :: hr_idx(:)

  real(8) :: time, time_end
  real(8) :: ddt
  real(8) :: errmax
  real(8) :: cpssto
  integer :: k

  time = time_start
  time_end = time + dt_model
  ddt = dt_riv

  do k = 1, riv_count
    call hr2vr(hr_idx(k), k, vr_idx(k))
  enddo

  do while( time < time_end )
    ddt = min( ddt, time_end - time )

    do
      call funcr(vr_idx, fr1)
      vr_tmp = vr_idx + ddt * b21 * fr1

      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr2)

      vr_tmp = vr_idx + ddt * (b31 * fr1 + b32 * fr2)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr3)

      vr_tmp = vr_idx + ddt * (b41 * fr1 + b42 * fr2 + b43 * fr3)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr4)

      vr_tmp = vr_idx + ddt * (b51 * fr1 + b52 * fr2 + b53 * fr3 + b54 * fr4)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr5)

      vr_tmp = vr_idx + ddt * (b61 * fr1 + b62 * fr2 + b63 * fr3 + b64 * fr4 + b65 * fr5)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr6)

      vr_err = ddt * (dc1 * fr1 + dc3 * fr3 + dc4 * fr4 + dc5 * fr5 + dc6 * fr6)
      hr_err = vr_err / riv_area_idx
      errmax = maxval( hr_err ) / eps

      if( errmax <= 1.d0 .or. ddt <= ddt_min_riv ) exit

      ddt = max( ddt * safety * errmax**pshrnk, ddt * 0.5d0 )
      ddt = max( ddt, ddt_min_riv )
      print*, "shrink (riv): ", ddt, errmax, maxloc( vr_err )
      ddt = min( ddt, time_end - time )
    enddo

    time = time + ddt

    vr_idx = vr_idx + ddt * (c1 * fr1 + c3 * fr3 + c4 * fr4 + c6 * fr6)
    !where( vr_idx < 0.d0 ) vr_idx = 0.d0
    cpssto = 0.d0
    do k = 1, riv_count
      if( vr_idx(k) < 0.d0 )then
        cpssto = cpssto + -vr_idx(k)
        vr_idx(k) = 0.d0
      endif
    enddo
    call update_cpssto_riv(cpssto)
  enddo  ! while time < time_end/

  do k = 1, riv_count
    call vr2hr(vr_idx(k), k, hr_idx(k))
  enddo
end subroutine advance_river
!===============================================================
!
!===============================================================
subroutine funcr(vr_idx, fr_idx)
  use mod_section, only: &
    vr2hr
  implicit none
  real(8), intent(in) :: vr_idx(:)
  real(8), intent(out) :: fr_idx(:)

  type(channel_), pointer :: ch
  integer :: k
  integer :: iiCh_down

  do k = 1, riv_count
    call vr2hr(vr_idx(k), k, hr_idx(k))
  enddo

  call calc_discharge(hr_idx, qr_tmp)

  do k = 1, riv_count
    fr_idx(k) = sum(-qr_tmp(:,k))
  enddo

  do k = 1, riv_count
    ch => channel(k)
    do iiCh_down = 1, ch%nCh_down
      call add(fr_idx(ch%iCh_down(iiCh_down)), qr_tmp(iiCh_down,k))
    enddo
  enddo
end subroutine funcr
!===============================================================
!
!===============================================================
subroutine calc_discharge(hr_idx, qr_tmp)
  implicit none
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(out) :: qr_tmp(:,:)

  integer :: k

  !$omp parallel do
  do k = 1, riv_count
    call calc_discharge_core(k, hr_idx, qr_tmp(:,k))
  enddo 
  !$omp end parallel do
end subroutine calc_discharge
!===============================================================
!
!===============================================================
subroutine calc_discharge_core(k, hr_idx, qr)
  implicit none
  integer, intent(in) :: k
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(out) :: qr(:)

  type(channel_), pointer :: ch_p
  real(8) :: hr_p, zb_p, hr_n, zb_n
  integer :: flow_p
  integer :: kk, iiCh_down
  real(8) :: distance
  real(8) :: dh, hw
  integer :: sgn_dh

  ch_p => channel(k)

  hr_p = hr_idx(k)
  zb_p = ch_p%zb
  flow_p = ch_p%flow

  do iiCh_down = 1, ch_p%nCh_down
    if( ch_p%is_outlet )then
      hr_n = 0.d0
      zb_n = zb_p
    else
      kk = ch_p%iCh_down(iiCh_down)

      hr_n = hr_idx(kk)
      zb_n = channel(kk)%zb
    endif

    distance = ch_p%dist_down(iiCh_down)

    selectcase( flow_p )
    case( FLOW__KINEMATIC )
      dh = max( (zb_p - zb_n) / distance, 0.001d0 )
    case( FLOW__DIFFUSION )
      dh = ((zb_p + hr_p) - (zb_n + hr_n)) / distance
    endselect

    if( dh >= 0.d0 )then
      sgn_dh = 1
      hw = hr_p
      if( zb_p < zb_n ) hw = max(0.d0, zb_p + hr_p - zb_n)
      call grad_to_disc(hw, dh, sgn_dh, k, ch_p%width, qr(iiCh_down))
    else
      sgn_dh = -1
      hw = hr_n
      if( zb_n < zb_p ) hw = max(0.d0, zb_n + hr_n - zb_p)
      call grad_to_disc(hw, -dh, sgn_dh, kk, ch_p%width, qr(iiCh_down))
    endif
  enddo  ! iiCh_down/
end subroutine calc_discharge_core
!===============================================================
!
!===============================================================
subroutine grad_to_disc(h, dh, sgn_dh, k, w, q)
  implicit none
  real(8), intent(in) :: h, dh
  integer, intent(in) :: sgn_dh
  integer, intent(in) :: k
  real(8), intent(in) :: w
  real(8), intent(out) :: q

  real(8) :: a, r

  a = sqrt(dh) / ns_river
  r = (w * h) / (w + 2.d0 * h)
  q = a * r ** (2.d0 / 3.d0) * w * h * sgn_dh
end subroutine grad_to_disc
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
subroutine outflow_river(hr_idx)
  use mod_budget, only: &
    update_accum_out
  use mod_section, only: &
    hr2vr
  implicit none
  real(8), intent(inout) :: hr_idx(:)

  type(channel_), pointer :: ch
  real(8) :: vr
  integer :: k

  do k = 1, riv_count
    ch => channel(k)
    if( .not. ch%is_outlet ) cycle

    call hr2vr(hr_idx(k), k, vr)

    call update_accum_out(vr)

    hr_idx(k) = 0.d0
  enddo
end subroutine outflow_river
!===============================================================
!
!===============================================================
end module mod_river
