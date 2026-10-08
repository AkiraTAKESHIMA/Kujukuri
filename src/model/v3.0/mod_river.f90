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
  character(CLEN_PROC), parameter :: MODNAM = 'mod_river'

  ! Static data
  real(8), allocatable :: riv_area_idx(:)

  ! Workspace
  real(8), allocatable :: vr_idx(:), vr_err(:), vr_tmp(:)
  real(8), allocatable :: hr_idx(:), hr_err(:)
  real(8), allocatable :: qr_tmp(:)
  real(8), allocatable :: fr1(:), fr2(:), fr3(:), fr4(:), fr5(:), fr6(:)
  real(8), allocatable :: qr1(:,:), qr2(:,:), qr3(:,:), qr4(:,:), qr5(:,:), qr6(:,:)
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
  allocate(qr_tmp(nCh_down_max))

  riv_area_idx(:) = channel(:)%area

  ! Runge-Kutta
  allocate(fr1(riv_count), fr2(riv_count), fr3(riv_count), &
           fr4(riv_count), fr5(riv_count), fr6(riv_count))
  allocate(qr1(nCh_down_max,riv_count), &
           qr2(nCh_down_max,riv_count), &
           qr3(nCh_down_max,riv_count), &
           qr4(nCh_down_max,riv_count), &
           qr5(nCh_down_max,riv_count), &
           qr6(nCh_down_max,riv_count))
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
    hr_idx, qr_idx &
)
  use def_runge
  use mod_section, only: &
    hr2vr, &
    vr2hr
  use mod_budget, only: &
    update_cpssto_riv
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_river'
  real(8), intent(in) :: time_start
  real(8), intent(inout) :: hr_idx(:)
  real(8), intent(out) :: qr_idx(:,:)  !(nCh_down_max,riv_count)

  type(channel_), pointer :: ch
  real(8) :: time, time_end
  real(8) :: ddt
  real(8) :: errmax
  real(8) :: cpssto
  integer :: k
  integer :: jCh

  time = time_start
  time_end = time + dt_model
  ddt = dt_riv

  do k = 1, riv_count
    call hr2vr(hr_idx(k), k, vr_idx(k))
  enddo

  qr_idx(:,:) = 0.d0

  do while( time < time_end )
    ddt = min( ddt, time_end - time )

    do
      call funcr(vr_idx, fr1, qr1)
      vr_tmp = vr_idx + ddt * b21 * fr1

      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr2, qr2)

      vr_tmp = vr_idx + ddt * (b31 * fr1 + b32 * fr2)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr3, qr3)

      vr_tmp = vr_idx + ddt * (b41 * fr1 + b42 * fr2 + b43 * fr3)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr4, qr4)

      vr_tmp = vr_idx + ddt * (b51 * fr1 + b52 * fr2 + b53 * fr3 + b54 * fr4)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr5, qr5)

      vr_tmp = vr_idx + ddt * (b61 * fr1 + b62 * fr2 + b63 * fr3 + b64 * fr4 + b65 * fr5)
      where( vr_tmp < 0.d0 ) vr_tmp = 0.d0
      call funcr(vr_tmp, fr6, qr6)

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
    cpssto = 0.d0  ! compensation
    do k = 1, riv_count
      if( vr_idx(k) < 0.d0 )then
        cpssto = cpssto + -vr_idx(k)
        vr_idx(k) = 0.d0
      endif
    enddo
    call update_cpssto_riv(cpssto)

    do k = 1, riv_count
      ch => channel(k)
      do jCh = 1, ch%nCh_down
        call add(qr_idx(jCh,k), qr1(jCh,k)*ddt)
        call add(qr_idx(jCh,k), qr2(jCh,k)*ddt)
        call add(qr_idx(jCh,k), qr3(jCh,k)*ddt)
        call add(qr_idx(jCh,k), qr4(jCh,k)*ddt)
        call add(qr_idx(jCh,k), qr5(jCh,k)*ddt)
        call add(qr_idx(jCh,k), qr6(jCh,k)*ddt)
      enddo
    enddo

  enddo  ! while time < time_end/

  do k = 1, riv_count
    call vr2hr(vr_idx(k), k, hr_idx(k))
  enddo
end subroutine advance_river
!===============================================================
!
!===============================================================
subroutine funcr(vr_idx, fr_idx, qr)
  use mod_section, only: &
    vr2hr
  implicit none
  real(8), intent(in) :: vr_idx(:)
  real(8), intent(out) :: fr_idx(:)
  real(8), intent(out) :: qr(:,:)  !(nCh_down_max,riv_count)

  type(channel_), pointer :: ch
  integer :: k
  integer :: jCh

  do k = 1, riv_count
    call vr2hr(vr_idx(k), k, hr_idx(k))
  enddo

  call calc_discharge(hr_idx, qr)

  do k = 1, riv_count
    if( channel(k)%nCh_down == 0 )then
      fr_idx(k) = 0.d0
    else
      fr_idx(k) = sum(-qr(:channel(k)%nCh_down,k))
    endif
  enddo

  do k = 1, riv_count
    ch => channel(k)
    do jCh = 1, ch%nCh_down
      call add(fr_idx(ch%iCh_down(jCh)), qr(jCh,k))
    enddo
  enddo
end subroutine funcr
!===============================================================
!
!===============================================================
subroutine calc_discharge(hr_idx, qr)
  implicit none
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(out) :: qr(:,:)  !(nCh_down_max,riv_count)

  integer :: k

  !$omp parallel do private(qr_tmp)
  do k = 1, riv_count
    call calc_discharge_core(k, hr_idx, qr_tmp)
    qr(:,k) = qr_tmp(:)
  enddo 
  !$omp end parallel do
end subroutine calc_discharge
!===============================================================
!
!===============================================================
subroutine calc_discharge_core(iCh, hr_idx, qr)
  implicit none
  integer, intent(in) :: iCh
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(out) :: qr(:)

  type(channel_), pointer :: ch_p
  real(8) :: hr_p, zb_p, hr_n, zb_n
  integer :: flow_p
  integer :: iCh2, jCh
  real(8) :: distance
  real(8) :: dh, hw

  ch_p => channel(iCh)

  hr_p = hr_idx(iCh)
  zb_p = ch_p%zb
  flow_p = ch_p%flow

  do jCh = 1, ch_p%nCh_down
    if( ch_p%is_outlet )then
      hr_n = 0.d0
      zb_n = zb_p
    else
      iCh2 = ch_p%iCh_down(jCh)

      hr_n = hr_idx(iCh2)
      zb_n = channel(iCh2)%zb
    endif

    distance = ch_p%dist_down(jCh)

    selectcase( flow_p )
    case( FLOW__KINEMATIC )
      dh = max( (zb_p - zb_n) / distance, 0.001d0 )
    case( FLOW__DIFFUSION )
      dh = ((zb_p + hr_p) - (zb_n + hr_n)) / distance
    endselect

    if( dh >= 0.d0 )then
      hw = hr_p
      if( zb_p < zb_n ) hw = max(0.d0, zb_p + hr_p - zb_n)
      call grad_to_disc(hw, dh, 1, iCh, ch_p%width, qr(jCh))
    else
      hw = hr_n
      if( zb_n < zb_p ) hw = max(0.d0, zb_n + hr_n - zb_p)
      call grad_to_disc(hw, -dh, -1, iCh2, ch_p%width, qr(jCh))
    endif
  enddo  ! jCh/
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
