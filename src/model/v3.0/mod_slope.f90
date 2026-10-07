module mod_slope
  use lib_const
  use lib_base
  use lib_log
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: prep_slope
  public :: advance_slope
  public :: h2lev
  public :: h2sfc
  public :: infilt
  public :: outflow_slope
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------

  ! Workspace
  real(8), allocatable :: qs_idx(:,:)  !(I4,slo_count)
  real(8), allocatable :: rain(:,:)  !(nx,ny)
  real(8), allocatable :: rain_idx(:)  !(slo_count)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_slope()
  use def_runge
  implicit none

  ! Runge-Kutta
  allocate(fs1(slo_count), fs2(slo_count), fs3(slo_count), &
           fs4(slo_count), fs5(slo_count), fs6(slo_count))

  allocate(qs_idx(I4,slo_count))

  allocate(rain(nx,ny))
  allocate(rain_idx(slo_count))
end subroutine prep_slope
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
subroutine advance_slope(&
  time_start, &
  hs_idx &
)
  use def_runge
  use mod_base, only: &
    reshape_slo_ij2idx
  use mod_forcing, only: &
    get_rain
  use mod_budget, only: &
    update_accum_rain, &
    update_cpssto_slo
  implicit none
  real(8), intent(in) :: time_start
  real(8), intent(inout) :: hs_idx(:)

  real(8) :: time, time_end
  real(8) :: ddt
  real(8) :: errmax
  logical :: is_rain_updated
  real(8) :: cpssto
  integer :: k

  time = time_start
  time_end = time + dt_model
  ddt = dt_slo

  do while( time < time_end )
    ddt = min(time_end - time, ddt)

    call get_rain(time, time+ddt, rain, is_rain_updated)
    if( is_rain_updated )then
      call reshape_slo_ij2idx(rain, rain_idx)
    endif

    do
      call funcs( time, ddt, hs_idx, rain_idx, fs1, qs_idx )

      hs_tmp = hs_idx + ddt * b21 * fs1
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, rain_idx, fs2, qs_idx )

      hs_tmp = hs_idx + ddt * (b31 * fs1 + b32 * fs2)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, rain_idx, fs3, qs_idx )

      hs_tmp = hs_idx + ddt * (b41 * fs1 + b42 * fs2 + b43 * fs3)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, rain_idx, fs4, qs_idx )

      hs_tmp = hs_idx + ddt * (b51 * fs1 + b52 * fs2 + b53 * fs3 + b54 * fs4)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, rain_idx, fs5, qs_idx )

      hs_tmp = hs_idx + ddt * (b61 * fs1 + b62 * fs2 + b63 * fs3 + b64 * fs4 + b65 * fs5)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, rain_idx, fs6, qs_idx )

      hs_err = ddt * (dc1 * fs1 + dc3 * fs3 + dc4 * fs4 + dc5 * fs5 + dc6 * fs6)
      errmax = maxval( hs_err, mask=domain_slo_idx/=DOMAIN__OUTSIDE ) / eps

      if( errmax <= 1.d0 .or. ddt <= ddt_min_slo ) exit

      ddt = max( ddt * safety * errmax**pshrnk, ddt * 0.5d0 )
      ddt = max( ddt, ddt_min_riv )
      print*, 'shrink (slo): ', ddt, errmax, maxloc(hs_err)
      ddt = min( ddt, time_end - time )
    enddo

    time = time + ddt

    call update_accum_rain(rain_idx, ddt)

    hs_idx = hs_idx + ddt * (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6)

    !where( hs_idx < 0.d0 ) hs_idx = 0.d0
    cpssto = 0.d0
    do k = 1, slo_count
      if( hs_idx(k) < 0.d0 )then
        cpssto = cpssto + -hs_idx(k) * area
        hs_idx(k) = 0.d0
      endif
    enddo
    call update_cpssto_slo(cpssto)
  enddo  ! while time < time_end/
end subroutine advance_slope
!===============================================================
!
!===============================================================
subroutine funcs(time, ddt, hs_idx, rain_idx, fs_idx, qs_idx )
  implicit none
  real(8), intent(in) :: time, ddt
  real(8), intent(in) :: hs_idx(slo_count), rain_idx(slo_count)
  real(8), intent(out) :: fs_idx(slo_count)
  real(8), intent(out) :: qs_idx(I4,slo_count)

  integer :: k, l, kk, itemp, jtemp

  fs_idx(:) = 0.d0
  qs_idx(:,:) = 0.d0

  call qs_calc(hs_idx, qs_idx)

  ! boundary condition for slope (discharge boundary)
  if( bound_slo_disc_switch .ge. 1 ) then
    !itemp = time / dt_bound_slo + 1
    itemp = -1
    do jtemp = 1, tt_max_bound_slo_disc
      if( t_bound_slo_disc(jtemp-1) < (time + ddt) .and. &
          (time + ddt) <= t_bound_slo_disc(jtemp) &
      ) itemp = jtemp
    enddo
    do k = 1, slo_count
      if( bound_slo_disc_idx(itemp, k) .le. -100.0 ) cycle ! not boundary

      selectcase( dir(slo_idx2i(k), slo_idx2j(k)) )
      ! right
      case( DIR__EAST )
        qs_idx(1, k) = bound_slo_disc_idx(itemp, k) / area
      ! right down
      case( DIR__SOUTHEAST )
        qs_idx(3, k) = bound_slo_disc_idx(itemp, k) / area
      ! down
      case( DIR__SOUTH )
        qs_idx(2, k) = bound_slo_disc_idx(itemp, k) / area
      ! left down
      case( DIR__SOUTHWEST )
        qs_idx(4, k) = bound_slo_disc_idx(itemp, k) / area
      ! left
      case( DIR__WEST )
        qs_idx(1, k) = - bound_slo_disc_idx(itemp, k) / area
      ! left up
      case( DIR__NORTHWEST )
        qs_idx(3, k) = - bound_slo_disc_idx(itemp, k) / area
      ! up
      case( DIR__NORTH )
        qs_idx(2, k) = - bound_slo_disc_idx(itemp, k) / area
      ! right up
      case( DIR__NORTHEAST )
        qs_idx(4, k) = - bound_slo_disc_idx(itemp, k) / area
      endselect
    enddo
  endif

  ! qs_idx > 0 --> discharge flowing out from a cell

  !$omp parallel do
  do k = 1, slo_count
    fs_idx(k) = rain_idx(k) - (qs_idx(1,k) + qs_idx(2,k) + qs_idx(3,k) + qs_idx(4,k))
  enddo
  !$omp end parallel do

  do k = 1, slo_count
    do l = 1, lmax
      if( flow_slo_idx(k) == FLOW__KINEMATIC .and. l == 2 ) exit ! kinematic -> 1-direction
      kk = down_slo_idx(l, k)
      if( flow_slo_idx(k) == FLOW__KINEMATIC ) kk = down_slo_1d_idx(k)
      if( kk == -1 ) cycle
      fs_idx(kk) = fs_idx(kk) + qs_idx(l, k)
    enddo
  enddo
end subroutine funcs
!===============================================================
! lateral discharge (slope)
!===============================================================
subroutine qs_calc(hs_idx, qs_idx)
  implicit none

  real(8) hs_idx(slo_count)
  real(8) qs_idx(i4, slo_count), q

  integer k, kk, l
  real(8) zb_p, hs_p, ns_p, ka_p, da_p, dm_p, b_p
  real(8) zb_n, hs_n, ns_n, ka_n, da_n, dm_n, b_n
  real(8) dh, distance
  real(8) lev_p, lev_n
  real(8) len, hw
  integer flow_p
  !real(8) emb

  qs_idx = 0.d0
  !emb = 0.d0

  !$omp parallel do private(kk,zb_p,hs_p,ns_p,ka_p,da_p,dm_p,b_p,flow_p,l,distance,len, &
  !$omp                     zb_n,hs_n,ns_n,ka_n,da_n,dm_n,b_n,lev_p,lev_n,dh,hw,q)
  do k = 1, slo_count

    zb_p = zb_slo_idx(k)
    hs_p = hs_idx(k)
    ns_p = ns_slo_idx(k)
    ka_p = ka_idx(k)
    da_p = da_idx(k)
    dm_p = dm_idx(k)
    b_p  = beta_idx(k)
    flow_p = flow_slo_idx(k)

    ! 8-direction: lmax = 4, 4-direction: lmax = 2
    do l = 1, lmax ! (1: rightC2: down, 3: right down, 4: left down)
      if( flow_p == FLOW__KINEMATIC )then
        if( l == 2 ) exit
        kk = down_slo_1d_idx(k)
      else
        kk = down_slo_idx(l, k)
      endif
      if( kk .eq. -1 ) cycle

      if( flow_p .eq. FLOW__KINEMATIC )then
        distance = dis_slo_1d_idx(k)
        len = len_slo_1d_idx(k)
      else
        distance = dis_slo_idx(l, k)
        len = len_slo_idx(l, k)
      endif

      ! information of the destination cell
      zb_n = zb_slo_idx(kk)
      hs_n = hs_idx(kk)
      ns_n = ns_slo_idx(kk)
      ka_n = ka_idx(kk)
      da_n = da_idx(kk)
      dm_n = dm_idx(kk)
      b_n = beta_idx(kk)

      call h2lev(hs_p, k, lev_p)
      call h2lev(hs_n, kk, lev_n)

      ! 1-direction : kinematic wave
      if( flow_p .eq. FLOW__KINEMATIC )then
        dh = max( (zb_p - zb_n) / distance, 0.001 )
      else
        dh = ((zb_p + lev_p) - (zb_n + lev_n)) / distance
      endif

      ! embankment
      !if(emb_switch.eq.1) then
      ! if(l.eq.1) emb = emb_r_idx(k)
      ! if(l.eq.2) emb = emb_b_idx(k)
      ! if(l.eq.3) emb = max( emb_r_idx(k), emb_b_idx(k) )
      ! if(l.eq.4) emb = max( emb_r_idx(kk), emb_b_idx(k) )
      !endif

      ! water coming in or going out?
      if( dh .ge. 0.d0 ) then
       ! going out
       hw = hs_p
       !if(emb .gt. 0.d0) hw = max(hs_p - emb, 0.d0)
       if( zb_p .lt. zb_n ) hw = max(0.d0, zb_p + hs_p - zb_n)
       call hq(ns_p, ka_p, da_p, dm_p, b_p, hw, dh, len, q)
       qs_idx(l,k) = q
      else
       ! coming in
       hw = hs_n
       !if(emb .gt. 0.d0) hw = max(hs_n - emb, 0.d0)
       dh = abs(dh)
       if( zb_n .lt. zb_p ) hw = max(0.d0, zb_n + hs_n - zb_p)
       call hq(ns_n, ka_n, da_n, dm_n, b_n, hw, dh, len, q)
       qs_idx(l,k) = -q
      endif
    enddo  ! l = 1, lmax
  enddo  ! k = 1, slo_count
  !$omp end parallel do

end subroutine qs_calc
!===============================================================
! water depth and discharge relationship
!===============================================================
subroutine hq(ns_p, ka_p, da_p, dm_p, b_p, h, dh, len, q)
  implicit none

  real(8) ns_p, da_p, dm_p, ka_p, b_p, h, dh, len, q
  real(8) km, vm, va, al, m

if( b_p .gt. 0.d0 )then
 km = ka_p / b_p
else
 km = 0.d0
endif
vm = km * dh

if( da_p .gt. 0.d0 ) then
 va = ka_p * dh
else
 va = 0.d0
endif

if( dh .lt. 0 ) dh = 0.d0
al = sqrt(dh) / ns_p
m = 5.d0 / 3.d0

if( h .lt. dm_p ) then
 q = vm * dm_p * (h / dm_p) ** b_p
elseif( h .lt. da_p ) then
 q = vm * dm_p + va * (h - dm_p)
else
 q = vm * dm_p + va * (h - dm_p) + al * (h - da_p) ** m
endif

! discharge per unit area
! (q multiply by width and divide by area)
q = q * len / area

! water depth limitter (1 mm)
! note: it can be set to zero
!if( h.le.0.001 ) q = 0.d0

end subroutine hq
!===============================================================
! effective water depth (h) to actual water level (lev)
!===============================================================
subroutine h2lev(h, k, lev)
  implicit none

  integer k
  real(8) h, lev
  real(8) da_temp

  da_temp = soildepth_idx(k) * gammaa_idx(k)

  if( soildepth_idx(k) == 0.d0 ) then
    lev = h
  elseif( h >= da_temp ) then ! including da = 0
    lev = soildepth_idx(k) + (h - da_temp) ! surface water
  else
    !if( soildepth_idx(k) > 0.d0 ) rho = da_temp / soildepth_idx(k)
    !lev = h / rho
    lev = h / gammaa_idx(k)
  endif
end subroutine h2lev
!===============================================================
! effective water depth (h) to surface water level (sfc)
!===============================================================
subroutine h2sfc(h, k, sfc)
  implicit none
  real(8), intent(in) :: h
  integer, intent(in) :: k
  real(8), intent(out) :: sfc

  if( soildepth_idx(k) == 0.d0 )then
    sfc = h
  else
    sfc = max(h - soildepth_idx(k) * gammaa_idx(k), 0.d0)
  endif
end subroutine h2sfc
!===============================================================
!
!===============================================================
subroutine infilt(hs_idx, gampt_ff_idx, gampt_f_idx)
  use mod_budget, only: &
    update_cpssto_infilt
  implicit none
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(out) :: gampt_f_idx(:)
  real(8) :: gampt_ff_temp
  real(8) :: cpssto
  integer :: k

  cpssto = 0.d0

  do k = 1, slo_count
    gampt_f_idx(k) = 0.d0
    gampt_ff_temp = gampt_ff_idx(k)
    if( gampt_ff_temp .le. 0.01d0 ) gampt_ff_temp = 0.01d0

    ! gampt_f_idx(k) : infiltration capacity [m/s]
    ! gampt_ff : accumulated infiltration depth [m]
    gampt_f_idx(k) = ksv_idx(k) * (1.d0 + faif_idx(k) * gammaa_idx(k) / gampt_ff_temp)

    ! gampt_f_idx(k) : infiltration capacity -> infiltration rate [m/s]
    if( gampt_f_idx(k) .ge. hs_idx(k) / dt_model ) gampt_f_idx(k) = hs_idx(k) / dt_model

    ! gampt_ff should not exceeds a certain level
    if( infilt_limit_idx(k) .ge. 0.d0 .and. gampt_ff_idx(k) .ge. infilt_limit_idx(k) ) gampt_f_idx(k) = 0.d0

    ! update gampt_ff [m]
    gampt_ff_idx(k) = gampt_ff_idx(k) + gampt_f_idx(k) * dt_model

    ! hs : hs - infiltration rate * dt [m]
    hs_idx(k) = hs_idx(k) - gampt_f_idx(k) * dt_model
    if( hs_idx(k) < 0.d0 )then
      cpssto = cpssto + -hs_idx(k) * area
      hs_idx(k) = 0.d0
    endif
  enddo

  call update_cpssto_infilt(cpssto)
end subroutine infilt
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
subroutine outflow_slope(hs_idx)
  use mod_budget, only: &
    update_accum_out
  implicit none
  real(8), intent(inout) :: hs_idx(:)

  integer :: k

  do k = 1, slo_count
    if( domain_slo_idx(k) /= DOMAIN__OUTLET ) cycle

    call update_accum_out(hs_idx(k)*area)

    hs_idx(k) = 0.d0
  enddo
end subroutine outflow_slope
!===============================================================
!
!===============================================================
end module mod_slope
