module mod_slope
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use def_const
  use def_type
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: advance_slope
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  !logical :: debug = .true.
  !integer :: k_debug = 1182
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine advance_slope(&
  static, state, tendency, forcing, time, &
  solver, &
  workspace &
)
  use mod_rk45, only: &
    rk45
  implicit none
  type(static_), intent(in) :: static
  type(state_), intent(inout), target :: state
  type(tendency_), intent(inout) :: tendency
  type(forcing_), intent(in) :: forcing
  type(time_), intent(in) :: time
  type(solver_), intent(in) :: solver
  type(workspace_), intent(inout) :: workspace

!  call rk45(&
!    funcs, increase_wlv, subst_wlv, associate_workspace_rk45, &
!    static, state, forcing, tendency, time, &
!    solver, &
!    workspace &
!  )

  call rk_org(&
    static, state, tendency, forcing, time, &
    solver, &
    workspace &
  )
end subroutine advance_slope
!===============================================================
!
!===============================================================
subroutine rk_org(&
  static, state, tendency, forcing, time, &
  solver, &
  workspace &
)
  use mod_rk_org
  implicit none
  type(static_), intent(in), target :: static
  type(state_), intent(inout), target :: state
  type(tendency_), intent(inout) :: tendency
  type(forcing_), intent(in) :: forcing
  type(time_), intent(in) :: time
  type(solver_), intent(in) :: solver
  type(workspace_), intent(inout), target :: workspace

  type(static_slope_), pointer :: sts
  type(state_slope_), pointer :: slope
  type(workspace_slope_), pointer :: wss
  type(workspace_rk45_submodel_), pointer :: wrk
  real(8) :: t, t_next
  real(8) :: dt, dt_min
  real(8), save :: dt_prev = 0.d0
  real(8) :: tolerance
  real(8) :: errmax
  integer :: k

  sts => static%slope
  slope => state%slope

  wss => workspace%slope
  wrk => workspace%rk45%slope

  tolerance = solver%adaptive_rk45%error_tolerance
  dt_min = solver%adaptive_rk45%dt_min_slope

  t = time%t_now
  t_next = time%t_next

  if( dt_prev == 0.d0 ) dt_prev = time%dt_slope
  dt = min(time%dt_slope, dt_prev*2.d0)

  do while( t < t_next )
!    call logmsg('t now: '//str(t)//' next: '//str(t_next)//&
!      ' wlv min: '//str(minval(slope%hs_idx))//' max: '//str(maxval(slope%hs_idx)))

    do
      call funcs( static, forcing, slope%hs_idx, wrk%k1, wrk%q1, workspace )
      wss%hs_idx = slope%hs_idx + dt * b21 * wrk%k1
      where( wss%hs_idx < 0.d0 ) wss%hs_idx = 0.d0

      call funcs( static, forcing, wss%hs_idx, wrk%k2, wrk%q2, workspace )
      wss%hs_idx = slope%hs_idx + dt * (b31 * wrk%k1 + b32 * wrk%k2)
      where( wss%hs_idx < 0.d0 ) wss%hs_idx = 0.d0

      call funcs( static, forcing, wss%hs_idx, wrk%k3, wrk%q3, workspace )
      wss%hs_idx = slope%hs_idx + dt * (b41 * wrk%k1 + b42 * wrk%k2 + b43 * wrk%k3)
      where( wss%hs_idx < 0.d0 ) wss%hs_idx = 0.d0

      call funcs( static, forcing, wss%hs_idx, wrk%k4, wrk%q4, workspace )
      wss%hs_idx = slope%hs_idx + dt * (b51 * wrk%k1 + b52 * wrk%k2 + b53 * wrk%k3 + b54 * wrk%k4)
      where( wss%hs_idx < 0.d0 ) wss%hs_idx = 0.d0

      call funcs( static, forcing, wss%hs_idx, wrk%k5, wrk%q5, workspace )
      wss%hs_idx = slope%hs_idx + dt * (b61 * wrk%k1 + b62 * wrk%k2 + b63 * wrk%k3 + b64 * wrk%k4 + b65 * wrk%k5)
      where( wss%hs_idx < 0.d0 ) wss%hs_idx = 0.d0

      call funcs( static, forcing, wss%hs_idx, wrk%k6, wrk%q6, workspace )
      wss%hs_idx = slope%hs_idx + dt * (c1 * wrk%k1 + c3 * wrk%k3 + c4 * wrk%k4 + c6 * wrk%k6)
      where( wss%hs_idx < 0.d0 ) wss%hs_idx = 0.d0

      ! error
      wrk%yerr = dt * (dc1 * wrk%k1 + dc3 * wrk%k3 + dc4 * wrk%k4 + dc5 * wrk%k5 + dc6 * wrk%k6)
      where( sts%domain_mask_idx == DOMAIN__OUTSIDE ) wrk%yerr = 0.d0
      errmax = maxval(wrk%yerr)

      if( errmax <= tolerance )then
        exit
      elseif( dt == dt_min )then
        exit
      endif

      dt = max( min(safety * ((errmax/tolerance) ** pshrnk), 0.8d0), 0.5d0) * dt

      k = maxloc(wrk%yerr,1)
      call logmsg('shrink (slo) '//str(dt)//' '//str(errmax)//' ('//&
        str((/sts%idx2i(k),sts%idx2j(k)/),',')//')')

      if( dt < dt_min )then
        call logwrn('Stepsize reached the limit.')
        dt = dt_min
      endif

      dt_prev = dt
    enddo

    if( t + dt > t_next )then
      dt = t_next - t
      t = t_next
    else
      t = t + dt
    endif

    slope%hs_idx = slope%hs_idx + dt * (c1 * wrk%k1 + c3 * wrk%k3 + c4 * wrk%k4 + c6 * wrk%k6)
    where( slope%hs_idx < 0.d0 ) slope%hs_idx = 0.d0
  enddo
end subroutine rk_org
!===============================================================
!
!===============================================================
subroutine funcs(&
  static, forcing, &
  hs_idx, fs_idx, qs_idx, &
  workspace &
)
  implicit none
  type(static_), intent(in), target :: static
  type(forcing_), intent(in) :: forcing
  real(8), intent(in) :: hs_idx(:)  !(nGrid)
  real(8), intent(out) :: fs_idx(:)  !(nGrid)
  real(8), intent(out) :: qs_idx(:,:)  !(nDir,nGrid)
  type(workspace_), intent(inout), target :: workspace

  type(static_slope_), pointer :: sts
  integer :: k, kk, l

!real(8), allocatable :: q_in(:,:)

  sts => static%slope

  call qs_calc(sts, hs_idx, qs_idx)

  do k = 1, sts%nGrid
    !fs_idx(k) = forcing%prcp_idx(k) - sum(qs_idx(:,k))

    !if( sum(qs_idx(:,k)) > hs_idx(k) + forcing%prcp_idx(k) )then
    !  qs_idx(:,k) = qs_idx(:,k) * (hs_idx(k) + forcing%prcp_idx(k)) / sum(qs_idx(:,k))
    !endif
    fs_idx(k) = forcing%prcp_idx(k) - sum(qs_idx(:,k))
  enddo

!if( debug )then
!  k = k_debug
!  call logmsg('fs1 '//str(fs_idx(k)))
!endif

  do k = 1, sts%nGrid
    do l = 1, sts%nDir
      kk = sts%down_idx(l,k)
      if( kk < 0 ) cycle
      fs_idx(kk) = fs_idx(kk) + qs_idx(l,k) * sts%area_idx(k) / sts%area_idx(kk)
    enddo
  enddo

!if( debug )then
!  allocate(q_in(sts%nDir,sts%nGrid))
!  do k = 1, sts%nGrid
!  do l = 1, sts%nDir
!    kk = sts%down_idx(l,k)
!    if( kk < 0 ) cycle
!    q_in(l,k) = qs_idx(l,k) * sts%area_idx(k) / sts%area_idx(kk)
!  enddo
!  enddo
!  k = k_debug
!  call logmsg('prcp '//str(forcing%prcp_idx(k))//' q_out '//str(qs_idx(:,k))//&
!    ' q_in '//str(q_in(:,kk))//' f '//str(fs_idx(k)))
!  deallocate(q_in)
!endif

  nullify(sts)
end subroutine funcs
!===============================================================
!
!===============================================================
subroutine qs_calc(static, hs_idx, qs_idx)
  implicit none
  type(static_slope_), intent(in), target :: static
  real(8), intent(in) :: hs_idx(:)  !(nGrid)
  real(8), intent(out) :: qs_idx(:,:)  !(nDir,nGrid)

  real(8) :: zb_p, hs_p, ns_p, ka_p, da_p, km_p, dm_p, b_p
  real(8) :: zb_n, hs_n, ns_n, ka_n, da_n, km_n, dm_n, b_n
  real(8) :: dh
  real(8) :: lev_p, lev_n
  real(8) :: hw
  integer :: dif_p, dif_n
  real(8) :: dis, len, area
  integer :: k, kk, l

logical :: debug_this

  ! areaは本当は流出側のグリッドの値をすべきでは？

  !$omp parallel do private(kk,l,area,dis,len,dh,hw, &
  !$omp                     hs_p,zb_p,ns_p,ka_p,da_p,km_p,dm_p,b_p,dif_p,lev_p, &
  !$omp                     hs_n,zb_n,ns_n,ka_n,da_n,km_n,dm_n,b_n,dif_n,lev_n)
  do k = 1, static%nGrid
    hs_p = hs_idx(k)
    zb_p = static%zb_idx(k)
    ns_p = static%ns_idx(k)
    ka_p = static%ka_idx(k)
    da_p = static%da_idx(k)
    km_p = static%km_idx(k)
    dm_p = static%dm_idx(k)
    b_p = static%beta_idx(k)
    dif_p = static%diffusion_idx(k)
    area = static%area_idx(k)

    call h2lev(hs_p, lev_p, static%soildepth_idx(k), static%gammaa_idx(k))

!debug_this = debug .and. k == k_debug
!if( debug_this )then
!  call logmsg('hs '//str(hs_p)//' lev '//str(lev_p)//' zb+lev '//str(zb_p+lev_p))
!endif

    do l = 1, static%nDir
      kk = static%down_idx(l,k)
      if( kk < 0 )then
        qs_idx(l,k) = 0.d0
        cycle
      endif

      dis = static%dis_idx(l,k)
      len = static%len_idx(l,k)

      ! information of the destination cell
      hs_n = hs_idx(kk)
      zb_n = static%zb_idx(kk)
      ns_n = static%ns_idx(kk)
      ka_n = static%ka_idx(kk)
      da_n = static%da_idx(kk)
      km_n = static%km_idx(kk)
      dm_n = static%dm_idx(kk)
      b_n = static%beta_idx(kk)
      dif_n = static%diffusion_idx(kk)

      call h2lev(hs_n, lev_n, static%soildepth_idx(kk), static%gammaa_idx(kk))

      dh = ((zb_p + lev_p) - (zb_n + lev_n)) / dis

      ! Case: Water goes out
      if( dh >= 0.d0 )then
        hw = max(hs_p + min(zb_p - zb_n, 0.d0), 0.d0)

        call hq(&
          hw, dh, &
          ns_p, ka_p, da_p, km_p, dm_p, b_p, &
          len, area, &
          qs_idx(l,k) &
        )

      ! Case: Water comes in
      else
        hw = max(hs_n + min(zb_n - zb_p, 0.d0), 0.d0)

        call hq(&
          hw, -dh, &
          ns_n, ka_n, da_n, km_n, dm_n, b_n, &
          -len, area, &
          qs_idx(l,k) &
        )
      endif

!if( debug_this )then
!  call logmsg('next('//str(l)//') h '//str(hs_n)//' lev '//str(lev_n)//&
!      ' zb+lev '//str(zb_n+lev_n)//&
!      ' dh '//str(dh)//' q '//str(qs_idx(l,k)))
!endif

    enddo  ! l/
  enddo  ! k/
  !$omp end parallel do
end subroutine qs_calc
!===============================================================
!
!===============================================================
subroutine hq(&
    h, dh, &
    ns, ka, da, km, dm, beta, &
    len, area, &
    q &
)
  implicit none
  real(8), intent(in)  :: h, dh
  real(8), intent(in)  :: ns, ka, da, km, dm, beta
  real(8), intent(in)  :: len, area
  real(8), intent(out) :: q

  real(8), parameter :: m = 5.d0 / 3.d0

  if( h < dm )then
    q = km * dm * (h / dm) ** beta * dh
  elseif( h <= da )then
    q = (km * dm + ka * (h - dm)) * dh
  else
    q = (km * dm + ka * (h - dm)) * dh + sqrt(dh) / ns * (h - da) ** m
  endif

  q = q * len / area
end subroutine hq
!===============================================================
!
!===============================================================
subroutine h2lev(h, lev, soildepth, gammaa)
  implicit none
  real(8), intent(in)  :: h
  real(8), intent(out) :: lev
  real(8), intent(in)  :: soildepth
  real(8), intent(in)  :: gammaa

  real(8) :: da

  if( soildepth == 0.d0 )then
    lev = h
  else
    da = soildepth * gammaa
    if( h >= da )then
      lev = soildepth + (h - da)
    else
      lev = h / gammaa
    endif
  endif
end subroutine h2lev
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
subroutine increase_wlv(state, dy, yt)
  implicit none
  type(state_), intent(in) :: state
  real(8), intent(in) :: dy(:)
  real(8), intent(out) :: yt(:)

  yt(:) = state%slope%hs_idx(:) + dy(:)
end subroutine increase_wlv
!===============================================================
!
!===============================================================
subroutine subst_wlv(state, y)
  implicit none
  type(state_), intent(inout) :: state
  real(8), intent(in) :: y(:)

  state%slope%hs_idx(:) = y(:)
end subroutine subst_wlv
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
subroutine associate_workspace_rk45(workspace, ws)
  implicit none
  type(workspace_), intent(in), target :: workspace
  type(workspace_rk45_submodel_), pointer :: ws

  ws => workspace%rk45%slope
end subroutine associate_workspace_rk45
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
subroutine allocate_state_slope(static, state)
  implicit none
  type(static_slope_), intent(in) :: static
  type(state_slope_), intent(inout) :: state

  allocate(state%hs_idx(static%nGrid))
  allocate(state%hs(static%nx,static%ny))
end subroutine allocate_state_slope
!===============================================================
!
!===============================================================
subroutine allocate_workspace_slope(static, workspace)
  implicit none
  type(static_slope_), intent(in) :: static
  type(workspace_slope_), intent(inout) :: workspace

  allocate(workspace%hs_idx(static%nGrid))
end subroutine allocate_workspace_slope
!===============================================================
!
!===============================================================
end module mod_slope
