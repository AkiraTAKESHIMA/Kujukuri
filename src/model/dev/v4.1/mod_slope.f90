module mod_slope
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: prep_mod_slope
  public :: advance_slope
  public :: h2lev
  public :: infilt
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  real(8), allocatable :: qs_idx(:,:)
  real(8), allocatable :: prcp_idx_all(:,:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_mod_slope()
  use def_runge
  use mod_base, only: &
    reshape_slo_ij2idx
  use mod_forcing, only: &
    get_prcp
  implicit none
  integer :: it_prcp
  real(8), allocatable :: prcp(:,:)

  allocate(qs_idx(I4,slo_count))

  allocate(fs1(slo_count), fs2(slo_count), fs3(slo_count), &
           fs4(slo_count), fs5(slo_count), fs6(slo_count))

  allocate(prcp(nx,ny))
  allocate(prcp_idx_all(slo_count,nt_prcp))
  do it_prcp = 1, nt_prcp
    call get_prcp(it_prcp, prcp)
    call reshape_slo_ij2idx(prcp, prcp_idx_all(:,it_prcp))
  enddo
  deallocate(prcp)
end subroutine prep_mod_slope
!===============================================================
!
!===============================================================
subroutine advance_slope(&
  time_start, &
  hs_idx &
)
  use def_runge
  use mod_forcing, only: &
    get_timestep_prcp
  implicit none
  real(8), intent(in) :: time_start
  real(8), intent(inout) :: hs_idx(:)

  real(8) :: time, time_end
  real(8) :: ddt
  real(8) :: errmax
  integer :: it_prcp

  time = time_start
  time_end = time + dt_model
  ddt = dt_slo

  do while( time < time_end )
    ddt = min(time_end - time, ddt)

    call get_timestep_prcp(time+ddt, it_prcp)

    do
      call funcs( time, ddt, hs_idx, prcp_idx_all(:,it_prcp), fs1, qs_idx )

      hs_tmp = hs_idx + ddt * b21 * fs1
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, prcp_idx_all(:,it_prcp), fs2, qs_idx )

      hs_tmp = hs_idx + ddt * (b31 * fs1 + b32 * fs2)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, prcp_idx_all(:,it_prcp), fs3, qs_idx )

      hs_tmp = hs_idx + ddt * (b41 * fs1 + b42 * fs2 + b43 * fs3)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, prcp_idx_all(:,it_prcp), fs4, qs_idx )

      hs_tmp = hs_idx + ddt * (b51 * fs1 + b52 * fs2 + b53 * fs3 + b54 * fs4)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, prcp_idx_all(:,it_prcp), fs5, qs_idx )

      hs_tmp = hs_idx + ddt * (b61 * fs1 + b62 * fs2 + b63 * fs3 + b64 * fs4 + b65 * fs5)
      where( hs_tmp < 0.d0 ) hs_tmp = 0.d0
      call funcs( time, ddt, hs_tmp, prcp_idx_all(:,it_prcp), fs6, qs_idx )

      hs_err = ddt * (dc1 * fs1 + dc3 * fs3 + dc4 * fs4 + dc5 * fs5 + dc6 * fs6)
      errmax = maxval( hs_err, mask=domain_slo_idx/=DOMAIN__OUTSIDE ) / eps

      if( errmax <= 1.d0 .or. ddt <= ddt_min_slo ) exit

      ddt = max( ddt * safety * errmax**pshrnk, ddt * 0.5d0 )
      ddt = max( ddt, ddt_min_riv )
      print*, 'shrink (slo): ', ddt, errmax, maxloc(hs_err)
      ddt = min( ddt, time_end - time )
    enddo

    time = time + ddt

    hs_idx = hs_idx + ddt * (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6)
    where( hs_idx < 0.d0 ) hs_idx = 0.d0
  enddo  ! while time < time_end/
end subroutine advance_slope
!===============================================================
!
!===============================================================
subroutine funcs(time, ddt, hs_idx, prcp_idx, fs_idx, qs_idx )
  implicit none
  real(8), intent(in) :: time, ddt
  real(8), intent(in) :: hs_idx(slo_count), prcp_idx(slo_count)
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
    fs_idx(k) = prcp_idx(k) - (qs_idx(1,k) + qs_idx(2,k) + qs_idx(3,k) + qs_idx(4,k))
  enddo
  !$omp end parallel do

  do k = 1, slo_count
    do l = 1, lmax
      if( dif_slo_idx(k) == FLOW__KINEMATIC .and. l == 2 ) exit ! kinematic -> 1-direction
      kk = down_slo_idx(l, k)
      if( dif_slo_idx(k) == FLOW__KINEMATIC ) kk = down_slo_1d_idx(k)
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
  integer dif_p, dif_n
  !real(8) emb

qs_idx = 0.d0
!emb = 0.d0

!$omp parallel do private(kk,zb_p,hs_p,ns_p,ka_p,da_p,dm_p,b_p,dif_p,l,distance,len, &
!$omp                     zb_n,hs_n,ns_n,ka_n,da_n,dm_n,b_n,dif_n,lev_p,lev_n,dh,hw,q)
do k = 1, slo_count

 zb_p = zb_slo_idx(k)
 hs_p = hs_idx(k)
 ns_p = ns_slo_idx(k)
 ka_p = ka_idx(k)
 da_p = da_idx(k)
 dm_p = dm_idx(k)
 b_p  = beta_idx(k)
 dif_p = dif_slo_idx(k)

 ! 8-direction: lmax = 4, 4-direction: lmax = 2
 do l = 1, lmax ! (1: rightC2: down, 3: right down, 4: left down)
  if( dif_p .eq. 0 .and. l .eq. 2 ) exit ! kinematic -> 1-direction
  kk = down_slo_idx(l, k)
  if( dif_p .eq. 0 ) kk = down_slo_1d_idx(k)
  if( kk .eq. -1 ) cycle

  distance = dis_slo_idx(l, k)
  len = len_slo_idx(l, k)
  if( dif_p .eq. 0 ) distance = dis_slo_1d_idx(k)
  if( dif_p .eq. 0 ) len = len_slo_1d_idx(k)

  ! information of the destination cell
  zb_n = zb_slo_idx(kk)
  hs_n = hs_idx(kk)
  ns_n = ns_slo_idx(kk)
  ka_n = ka_idx(kk)
  da_n = da_idx(kk)
  dm_n = dm_idx(kk)
  b_n = beta_idx(kk)
  dif_n = dif_slo_idx(kk)

  call h2lev(hs_p, k, lev_p)
  call h2lev(hs_n, kk, lev_n)

  ! diffusion wave
  dh = ((zb_p + lev_p) - (zb_n + lev_n)) / distance

  ! 1-direction : kinematic wave
  if( dif_p .eq. 0 ) dh = max( (zb_p - zb_n) / distance, 0.001 )

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

 enddo
enddo
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
! water depth (h) to actual water level (lev)
!===============================================================
subroutine h2lev(h, k, lev)
  implicit none

  integer k
  real(8) h, lev
  real(8) rho
  real(8) da_temp

  da_temp = soildepth_idx(k) * gammaa_idx(k)

  if( soildepth_idx(k) == 0.d0 ) then
    lev = h
  elseif( h >= da_temp ) then ! including da = 0
    lev = soildepth_idx(k) + (h - da_temp) ! surface water
  else
    if( soildepth_idx(k) > 0.d0 ) rho = da_temp / soildepth_idx(k)
    lev = h / rho
    ! equivalent to:
    ! lev = h / gammaa_idx(k)
  endif
end subroutine h2lev
!===============================================================
!
!===============================================================
subroutine infilt(hs_idx, gampt_ff_idx, gampt_f_idx)
  implicit none
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(out) :: gampt_f_idx(:)
  real(8) :: gampt_ff_temp
  integer :: k

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
    if( hs_idx(k) .le. 0.d0 ) hs_idx(k) = 0.d0
  enddo
end subroutine infilt
!===============================================================
!
!===============================================================
end module mod_slope
