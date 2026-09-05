module mod_river
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: prep_mod_river
  public :: advance_river
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  real(8), allocatable :: vr_idx(:)
!  real(8), allocatable :: hr_idx(:)
!  real(8), allocatable :: qr_idx(:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_mod_river()
  use def_runge
  implicit none

  allocate(vr_idx(riv_count))
!  allocate(hr_idx(riv_count))
!  allocate(qr_idx(riv_count))

  allocate(fr1(riv_count), fr2(riv_count), fr3(riv_count), &
           fr4(riv_count), fr5(riv_count), fr6(riv_count))
  allocate(vr_tmp(riv_count), vr_err(riv_count), hr_err(riv_count))
end subroutine prep_mod_river
!===============================================================
!
!===============================================================
subroutine advance_river(&
    time_start, time_end, &
    hr_idx &
)
  use def_runge
  use mod_section, only: &
    hr2vr, &
    vr2hr
  implicit none
  real(8), intent(in) :: time_start, time_end
  real(8), intent(inout) :: hr_idx(:)

  real(8) :: time
  real(8) :: ddt
  real(8) :: errmax
  integer :: k

  time = time_start
  ddt = dt_riv

  do k = 1, riv_count
    call hr2vr(hr_idx(k), k, vr_idx(k))
  enddo

  do while( time < time_end )
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

      hr_err = vr_err / (area * area_ratio_idx)
      where( domain_riv_idx == 0 ) hr_err = 0.d0

      errmax = maxval( hr_err, mask=domain_riv_idx/=DOMAIN__OUTSIDE ) / eps

      if( errmax <= 1.d0 .or. ddt <= ddt_min_riv ) exit

      ddt = max( ddt * safety * errmax**pshrnk, ddt * 0.5d0 )
      ddt = min( max( ddt, ddt_min_slo ), time_end - time )
    enddo

    time = time + ddt

    vr_idx = vr_idx + ddt * (c1 * fr1 + c3 * fr3 + c4 * fr4 + c6 * fr6)
    where( vr_idx < 0.d0 ) vr_idx = 0.d0
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

  real(8) :: hr_idx(riv_count)
  real(8) :: qr_idx(riv_count)
  integer :: k, kk

  do k = 1, riv_count
    call vr2hr(vr_idx(k), k, hr_idx(k))
  enddo

  call calc_discharge(hr_idx, qr_idx)

  fr_idx(:) = -qr_idx(:)
  do k = 1, riv_count
    kk = down_riv_idx(k)
    if( kk == 0 ) cycle
    fr_idx(kk) = fr_idx(kk) + qr_idx(k)
  enddo
end subroutine funcr
!===============================================================
!
!===============================================================
subroutine calc_discharge(hr_idx, qr_idx)
  implicit none
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(out) :: qr_idx(:)

  real(8) :: zb_p, zb_n
  real(8) :: hr_p, hr_n
  real(8) :: dh, distance, hw
  real(8) :: qr_tmp
  integer :: dif_p, dif_n
  integer :: k, kk

  !$omp parallel do private(kk,zb_p,hr_p,distance,zb_n,hr_n,dh,hw,qr_tmp,dif_p,dif_n)
  do k = 1, riv_count
    if( domain_riv_idx(k) == DOMAIN__OUTLET ) cycle

    zb_p = zb_riv_idx(k)
    hr_p = hr_idx(k)
    dif_p = dif_riv_idx(k)

    distance = dis_riv_idx(k)

    kk = down_riv_idx(k)
    zb_n = zb_riv_idx(kk)
    hr_n = hr_idx(kk)
    dif_n = dif_riv_idx(kk)

    ! diffusion wave
    dh = ((zb_p + hr_p) - (zb_n + hr_n)) / distance ! diffussion

    ! kinematic wave
    if( dif_p == FLOW__KINEMATIC ) dh = max( (zb_p - zb_n) / distance, 0.001d0 )

    ! the destination cell is outlet (domain = 2)
    if( domain_riv_idx(kk) == DOMAIN__OUTLET ) dh = (zb_p + hr_p - zb_n) / distance

    if( dh >= 0.d0 )then
      hw = hr_p
      if( zb_p < zb_n ) hw = max(0.d0, zb_p + hr_p - zb_n)
      call grad_to_disc(hw, dh, k, width_idx(k), qr_tmp)
      qr_idx(k) = qr_tmp
    else
      call grad_to_disc(hw, -dh, kk, width_idx(k), qr_tmp)
      qr_idx(k) = -qr_tmp
    endif
  enddo
end subroutine calc_discharge
!===============================================================
!
!===============================================================
subroutine grad_to_disc(h, dh, k, w, q)
  implicit none
  real(8), intent(in) :: h, dh, w
  integer, intent(in) :: k
  real(8), intent(out) :: q

  real(8) :: a, r

  a = sqrt(dh) / ns_river
  r = (w * h) / (w + 2.d0 * h)
  q = a * r ** (2.d0 / 3.d0) * w * h
end subroutine grad_to_disc
!===============================================================
!
!===============================================================
end module mod_river
