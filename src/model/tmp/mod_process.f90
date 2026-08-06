module mod_process
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_math
  use def_const
  use def_type
  use mod_param
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: open_file_prcp
  public :: close_file_prcp
  public :: get_prcp
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_process'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calc_river(&
    it, &
    hr_idx, & ! inout
    qr_ave_idx)  ! out
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_river'
  integer, intent(in) :: it
  real(8), intent(inout) :: hr_idx(:)
  real(8), intent(out) :: qr_ave_idx(:)

  real(8) :: time_sec, ddt
  real(8) :: errmax
  integer :: k

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  qr_ave_idx(:) = 0.d0

  time_sec = (it-1) * dt
  ddt = dt_riv

  do k = 1, nCh
    call hr2vr(hr_idx(k), k, vr_idx(k))
  enddo

  do while( time_sec < it * dt )

    do
      ! Adaptive Runge-Kutta
      ! (1)
      call funcr( vr_idx, fr1, qr_idx )
      call adaprive_rk_step1(vr_temp, vr_idx, fr1, ddt)
      call update_accum(qr_ave_temp_idx, qr_idx, ddt)

      ! (2)
      call funcr( vr_temp, fr2, qr_idx )
      vr_temp = vr_idx + ddt * (b31 * fr1 + b32 * fr2)
      where(vr_temp .lt. 0) vr_temp = 0.d0
      qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

      ! (3)
      call funcr( vr_temp, fr3, qr_idx )
      vr_temp = vr_idx + ddt * (b41 * fr1 + b42 * fr2 + b43 * fr3)
      where(vr_temp .lt. 0) vr_temp = 0.d0
      qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

      ! (4)
      call funcr( vr_temp, fr4, qr_idx )
      vr_temp = vr_idx + ddt * (b51 * fr1 + b52 * fr2 + b53 * fr3 + b54 * fr4)
      where(vr_temp .lt. 0) vr_temp = 0.d0
      qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

      ! (5)
      call funcr( vr_temp, fr5, qr_idx )
      vr_temp = vr_idx + ddt * (b61 * fr1 + b62 * fr2 + b63 * fr3 + b64 * fr4 + b65 * fr5)
      where(vr_temp .lt. 0) vr_temp = 0.d0
      qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

      ! (6)
      call funcr( vr_temp, fr6, qr_idx )
      vr_temp = vr_idx + ddt * (c1 * fr1 + c3 * fr3 + c4 * fr4 + c6 * fr6)
      where(vr_temp .lt. 0) vr_temp = 0.d0
      qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

      ! Error evaluation
      vr_err = ddt * (dc1 * fr1 + dc3 * fr3 + dc4 * fr4 + dc5 * fr5 + dc6 * fr6)

      hr_err(:) = vr_err(:) / area_riv_idx(:)
      errmax = maxval( hr_err )
      !-------------------------------------------------------
      ! Exit if the condition is fulfilled
      !-------------------------------------------------------
      if( errmax <= eps )then
        exit
      elseif( ddt == ddt_min_riv )then
        call logwrn('Stepsize reached the limit.')
        exit
      endif
      !-------------------------------------------------------
      ! Try smaller ddt
      !-------------------------------------------------------
      ddt = max( safety * ddt * ((errmax/eps) ** pshrnk), 0.5d0 * ddt)

      call logmsg('shrink (riv): '//str(ddt)//' '//str(errmax)//' '//str(maxloc(hr_err,1)))
      if( ddt == 0.d0 )then
        call errend('Stepsize underflow.')
      elseif( ddt == ddt_min_riv )then
        call logwrn('Stepsize reached the limit.')
        ddt = ddt_min_riv
      endif
    enddo
    !---------------------------------------------------------
    ! Calc. again if ddt reached the limit
    !---------------------------------------------------------
    if( ddt == ddt_min_riv )then
      call funcr( vr_temp, fr6, qr_idx )
      qr_ave_temp_idx = qr_idx * ddt * 6.d0
    endif
    !---------------------------------------------------------
    ! Update time
    !---------------------------------------------------------
    if( time_sec + ddt > it * dt )then
      ddt = it * dt - time_sec
      time_sec = it * dt
    else
      time_sec = time_sec + ddt
    endif
    !---------------------------------------------------------
    ! Update state variable
    !---------------------------------------------------------
    vr_idx = vr_idx + ddt * (c1 * fr1 + c3 * fr3 + c4 * fr4 + c6 * fr6)
    where(vr_temp .lt. 0) vr_temp = 0.d0

    qr_ave_idx = qr_ave_idx + qr_ave_temp_idx
  enddo  ! while( time_sec < it * dt )/
  qr_ave_idx = qr_ave_idx / (dt*6.d0)

  do k = 1, nCh
    call vr2hr(vr_idx(k), k, hr_idx(k))
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calc_river
!===============================================================
!
!===============================================================
subroutine calc_slope(&
    it, qp_idx, &
    hs_idx, gampt_ff_idx, &
    qs_ave_idx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_slope'
  integer, intent(in) :: it
  real(8), intent(in) :: qp_idx(:)
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(out) :: qs_ave_idx(:,:)

  real(8) :: time_sec, ddt
  real(8), save :: ddt_prev = 0.d0
  real(8) :: errmax
  integer :: k

  real(8) :: t0, t1

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  time_sec = (it-1) * dt

  !ddt = dt_slo
  if( ddt_prev == 0.d0 ) ddt_prev = dt_slo
  ddt = min(dt_slo, ddt_prev*2.d0)

  qs_ave_idx(:,:) = 0.d0

  time_qs_calc = 0.d0
  time_fs_calc = 0.d0
  time_fs_add  = 0.d0
  time_post_funcs = 0.d0

  ! Calc. slope runoff
  !-------------------------------------------------------------
  do while( time_sec < it * dt )

    do
      qs_ave_temp_idx(:,:) = 0.d0
      !-------------------------------------------------------
      ! Adaptive Runge-Kutta
      !-------------------------------------------------------
      ! (1)
      call funcs( hs_idx, qp_idx, fs1, qs_idx )
      call cpu_time(t0)
      hs_temp = hs_idx + ddt * b21 * fs1
      where(hs_temp .lt. 0) hs_temp = 0.d0
      qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt
      call cpu_time(t1)
      call add(time_post_funcs, t1-t0)

      ! (2)
      call funcs( hs_temp, qp_idx, fs2, qs_idx )
      call cpu_time(t0)
      hs_temp = hs_idx + ddt * (b31 * fs1 + b32 * fs2)
      where(hs_temp .lt. 0) hs_temp = 0.d0
      qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt
      call cpu_time(t1)
      call add(time_post_funcs, t1-t0)

      ! (3)
      call funcs( hs_temp, qp_idx, fs3, qs_idx )
      call cpu_time(t0)
      hs_temp = hs_idx + ddt * (b41 * fs1 + b42 * fs2 + b43 * fs3)
      where(hs_temp .lt. 0) hs_temp = 0.d0
      qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt
      call cpu_time(t1)
      call add(time_post_funcs, t1-t0)

      ! (4)
      call funcs( hs_temp, qp_idx, fs4, qs_idx )
      call cpu_time(t0)
      hs_temp = hs_idx + ddt * (b51 * fs1 + b52 * fs2 + b53 * fs3 + b54 * fs4)
      where(hs_temp .lt. 0) hs_temp = 0.d0
      qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt
      call cpu_time(t1)
      call add(time_post_funcs, t1-t0)

      ! (5)
      call funcs( hs_temp, qp_idx, fs5, qs_idx )
      call cpu_time(t0)
      hs_temp = hs_idx + ddt * (b61 * fs1 + b62 * fs2 + b63 * fs3 + b64 * fs4 + b65 * fs5)
      where(hs_temp .lt. 0) hs_temp = 0.d0
      qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt
      call cpu_time(t1)
      call add(time_post_funcs, t1-t0)

      ! (6)
      call funcs( hs_temp, qp_idx, fs6, qs_idx )
      call cpu_time(t0)
      hs_temp = hs_idx + ddt * (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6)
      where(hs_temp .lt. 0) hs_temp = 0.d0
      qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt
      call cpu_time(t1)
      call add(time_post_funcs, t1-t0)

      ! (e)
      call cpu_time(t0)
      hs_err = ddt * (dc1 * fs1 + dc3 * fs3 + dc4 * fs4 + dc5 * fs5 + dc6 * fs6)

      ! error evaluation
      where( domain_slo_idx == DOMAIN_OUTSIDE ) hs_err = 0.d0
      errmax = maxval( hs_err )

      call cpu_time(t1)
      call add(time_post_funcs, t1-t0)
      !-------------------------------------------------------
      ! Exit if the condition is fulfilled
      !-------------------------------------------------------
      if( errmax <= eps )then
        exit
      !-------------------------------------------------------
      ! Calc. again if ddt reached the limit
      elseif( ddt == ddt_min_slo )then
        call funcs( hs_temp, qp_idx, fs6, qs_idx )
        qs_ave_temp_idx = qs_idx * ddt * 6.d0
        exit
      endif
      !-------------------------------------------------------
      ! Try smaller ddt
      !-------------------------------------------------------
      !ddt = max( safety * ddt * ((errmax/eps) ** pshrnk), 0.5d0 * ddt )
      ddt = max( min(safety * ((errmax/eps) ** pshrnk), 0.8d0), 0.5d0 ) * ddt

      k = maxloc(hs_err,1)
      call logmsg('shrink (slo) '//str(ddt)//' '//str(errmax)//' ('//&
                  str((/slo_idx2i(k),slo_idx2j(k)/),',')//')')

      if( ddt < ddt_min_slo )then
        call logwrn('Stepsize reached the limit.')
        ddt = ddt_min_slo
      endif

      ddt_prev = ddt
    enddo  ! while( errmax > 1.0 .and. ddt > ddt_min_slo )/
    !---------------------------------------------------------
    ! Calc. again if ddt reached the limit
    !---------------------------------------------------------
    if( time_sec + ddt > it * dt )then
      ddt = it * dt - time_sec
      time_sec = it * dt
    else
      time_sec = time_sec + ddt
    endif
    !---------------------------------------------------------
    ! Update state variable
    !---------------------------------------------------------
    hs_idx = hs_idx + ddt * (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6)
    where(hs_temp .lt. 0) hs_temp = 0.d0

    qs_ave_idx(:,:) = qs_ave_idx(:,:) + qs_ave_temp_idx(:,:)
  enddo  ! while( time_sec < it * dt )/

  call logmsg('Time qs_calc   : '//str(time_qs_calc,'f7.3')//&
            '\n     fs_calc   : '//str(time_fs_calc,'f7.3')//&
            '\n     fs_add    : '//str(time_fs_add,'f7.3')//&
            '\n     post_funcs: '//str(time_post_funcs,'f7.3'))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calc_slope
!===============================================================
!
!===============================================================
subroutine funcr(vr_idx, fr_idx, qr_idx)
  implicit none
  real(8), intent(in) :: vr_idx(:)
  real(8), intent(out) :: fr_idx(:)
  real(8), intent(out) :: qr_idx(:)

  real(8), allocatable :: hr_idx(:)
  real(8), allocatable :: qr_sum_idx(:)
  real(8), allocatable :: qr_div_idx(:)
  integer :: k

  allocate(hr_idx(nCh))

  fr_idx(:) = 0.d0
  qr_idx(:) = 0.d0
  qr_sum_idx(:) = 0.d0
  qr_div_idx(:) = 0.d0

  do k = 1, nCh
    call vr2hr(vr_idx(k), k, hr_idx(k))
  enddo

  call qr_calc(&
      hr_idx, &       ! in
      qr_idx, fr_idx) ! out
end subroutine funcr
!===============================================================
!
!===============================================================
subroutine qr_calc(hr_idx, qr_idx, fr_idx)
  implicit none
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(out) :: qr_idx(:)
  real(8), intent(out) :: fr_idx(:)

  type(ch_), pointer :: ch
  type(nd_), pointer :: nd
  integer :: k, kk
  integer :: jNode
  integer :: iNode_conn
  real(8) :: zb_p, hr_p, zb_n, hr_n
  real(8) :: dh
  real(8) :: leng
  real(8) :: hw
  real(8) :: qr_temp

  qr_idx(:) = 0.d0
  fr_idx(:) = 0.d0

  do k = 1, nCh
    ch => lst_ch(k)

    zb_p = zb_riv_idx(k)
    hr_p = hr_idx(k)

    do jNode = 1, 2
      nd => ch%node(jNode)
      do iNode_conn = 1, nd%nNode_conn
        kk = nd%node_conn(iNode_conn)%iCh

        zb_n = zb_riv_idx(kk)
        hr_n = hr_idx(kk)

        leng = (ch%leng + lst_ch(kk)%leng) * 0.5d0
        dh = ((zb_p + hr_p) - (zb_n + hr_n)) / leng

        if( dh <= 0.d0 ) cycle

        if( zb_p < zb_n )then
          hw = max(0.d0, hr_p + zb_p - zb_n)
        else
          hw = hr_p
        endif
        call hq_riv(&
            dh, hw, ch%width, k, & ! in
            qr_temp) ! out
        call add(qr_idx(k), qr_temp)

        call add(fr_idx(k), -qr_temp)
        call add(fr_idx(kk), qr_temp)

if( debug )then
  if( k == k_debug .or. kk == k_debug )then
    call logmsg('qr_calc'//&
     '\n  ch('//str(k,dgt(nCh))//') hr '//str(hr_p)//' zb+hr '//str(zb_p+hr_p)//&
     '\n  ch('//str(kk,dgt(nCh))//') hr '//str(hr_n)//' zb+hr '//str(zb_n+hr_n)//&
     '\n  dh: '//str(dh)//', flux: '//str(qr_temp))
  endif
endif

      enddo  ! iNode_conn/
    enddo  ! jNode/
  enddo  ! k/
end subroutine qr_calc
!===============================================================
!
!===============================================================
subroutine hq_riv(dh, h, w, k, q)
  implicit none
  real(8), intent(in) :: dh, h, w
  integer, intent(in) :: k
  real(8), intent(out) :: q

  real(8) :: a, r
  real(8), parameter :: m = 2.d0 / 3.d0

  a = sqrt(abs(dh)) / ns_river
  r = (w*h) / (w+h*2.d0)
  q = a * r**m * w*h
end subroutine hq_riv
!===============================================================
!
!===============================================================
subroutine hr2vr(hr, k, vr)
  implicit none
  real(8), intent(in) :: hr
  integer, intent(in) :: k
  real(8), intent(out) :: vr

  vr = hr * area_riv_idx(k)
end subroutine hr2vr
!===============================================================
!
!===============================================================
subroutine vr2hr(vr, k, hr)
  implicit none
  real(8), intent(in) :: vr
  integer, intent(in) :: k
  real(8), intent(out) :: hr

  hr = vr / area_riv_idx(k)
end subroutine vr2hr
!===============================================================
!
!===============================================================
subroutine funcs(hs_idx, qp_idx, fs_idx, qs_idx)
  implicit none
  real(8), intent(in)  :: hs_idx(:)  !(nSlo)
  real(8), intent(in)  :: qp_idx(:)  !(nSlo)
  real(8), intent(out) :: fs_idx(:)  !(nSlo)
  real(8), intent(out) :: qs_idx(:,:)  !(lmax,nSlo)

  integer :: k, kk, l

  real(8) :: t0, t1

  call cpu_time(t0)

  !$omp parallel do
  do k = 1, nSlo
    call qs_calc_grid(k, hs_idx, qs_idx(:,k))
  enddo  ! k/
  !$omp end parallel do

  call cpu_time(t1)
  call add(time_qs_calc, t1-t0)

  call cpu_time(t0)

  do k = 1, nSlo
    fs_idx(k) = qp_idx(k) - sum(qs_idx(:,k))
  enddo

  call cpu_time(t1)
  call add(time_fs_calc, t1-t0)

  call cpu_time(t0)

  do k = 1, nSlo
    do l = 1, lmax
      !if( dif_p .eq. 0 .and. l .eq. 2 ) exit ! kinematic -> 1-direction
      kk = down_slo_idx(l,k)
      !if( dif_slo_idx(k) == 0 ) kk = down_slo_1d_idx(k)
      if( kk < 0 ) cycle
      call add(fs_idx(kk), qs_idx(l,k) * area_slo_idx(k) / area_slo_idx(kk))
    enddo
  enddo

  call cpu_time(t1)
  call add(time_fs_add, t1-t0)
end subroutine funcs
!===============================================================
!
!===============================================================
subroutine qs_calc_grid(k, hs_idx, qs)
  implicit none
  integer, intent(in)  :: k
  real(8), intent(in)  :: hs_idx(:)
  real(8), intent(out) :: qs(:)  !(lmax)

  real(8) :: q
  real(8) :: zb_p, hs_p, ns_p, ka_p, da_p, km_p, dm_p, b_p
  real(8) :: zb_n, hs_n, ns_n, ka_n, da_n, km_n, dm_n, b_n
  real(8) :: dh
  real(8) :: lev_p, lev_n
  real(8) :: hw
  integer :: dif_p, dif_n
  real(8) :: dis, len, area
  integer :: kk, l

  zb_p = zb_slo_idx(k)
  hs_p = hs_idx(k)
  ns_p = ns_slo_idx(k)
  ka_p = ka_idx(k)
  da_p = da_idx(k)
  km_p = km_idx(k)
  dm_p = dm_idx(k)
  b_p  = beta_idx(k)
  dif_p = dif_slo_idx(k)
  area = area_slo_idx(k)

  ! 8-direction: lmax = 4, 4-direction: lmax = 2
  do l = 1, lmax ! (1: right, 2: down, 3: right down, 4: left down)
    !if( dif_p .eq. 0 .and. l .eq. 2 ) exit ! kinematic -> 1-direction
    kk = down_slo_idx(l, k)
    !if( dif_p .eq. 0 ) kk = down_slo_1d_idx(k)
    if( kk < 0 )then
      qs(l) = 0.d0
      cycle
    endif

    dis = dis_slo_idx(l,k)
    len = len_slo_idx(l,k)
    !if( dif_p .eq. 0 ) dis = dis_slo_1d_idx(k)
    !if( dif_p .eq. 0 ) len = len_slo_1d_idx(k)

    ! information of the destination cell
    zb_n = zb_slo_idx(kk)
    hs_n = hs_idx(kk)
    ns_n = ns_slo_idx(kk)
    ka_n = ka_idx(kk)
    da_n = da_idx(kk)
    km_n = km_idx(kk)
    dm_n = dm_idx(kk)
    b_n = beta_idx(kk)
    dif_n = dif_slo_idx(kk)

    !hs_n = hs_idx(kk)
    !zb_n = zb_slo_down_idx(l,k)
    !ns_n = ns_slo_down_idx(l,k)
    !ka_n = ka_down_idx(l,k)
    !da_n = da_down_idx(l,k)
    !dm_n = dm_down_idx(l,k)
    !b_n = beta_down_idx(l,k)

    call h2lev(hs_p, k, lev_p)
    call h2lev(hs_n, kk, lev_n)

    !if( dif_p == 0 )then
    !  ! 1-direction : kinematic wave
    !  dh = max( (zb_p - zb_n) / dis, 0.001 )
    !else
    !  ! diffusion wave
    !  dh = ((zb_p + lev_p) - (zb_n + lev_n)) / dis
    !endif
    dh = ((zb_p + lev_p) - (zb_n + lev_n)) / dis

    ! water coming in or going out?
    if( dh >= 0.d0 ) then
      ! going out

      ! MODE A1
      !hw = hs_p
      !if( zb_p .lt. zb_n ) hw = max(0.d0, zb_p + hs_p - zb_n)

      ! MODE A2
      hw = max(hs_p + min(zb_p - zb_n, 0.d0), 0.d0)

      call hq(ns_p, ka_p, da_p, km_p, dm_p, b_p, &
              hw, dh, &
              len, area, &
              q)
      qs(l) = q
    else
      ! coming in

      ! MODE A1
      !hw = hs_n
      !if( zb_n .lt. zb_p ) hw = max(0.d0, zb_n + hs_n - zb_p)

      ! MODE A2
      hw = max(hs_n + min(zb_n - zb_p, 0.d0), 0.d0)

      call hq(ns_n, ka_n, da_n, km_n, dm_n, b_n, &
              hw, -dh, &
              len, area, &
              q)
      qs(l) = -q
    endif
  enddo  ! l/
end subroutine qs_calc_grid
!===============================================================
!
!===============================================================
subroutine hq(&
    ns, ka, da, km, dm, beta, &
    h, dh, &
    len, area, &
    q)
  implicit none
  real(8), intent(in)  :: ns, ka, da, km, dm, beta
  real(8), intent(in)  :: h, dh
  real(8), intent(in)  :: len, area
  real(8), intent(out) :: q

  real(8) :: km1
  real(8) :: vm, va, al
  real(8), parameter :: m = 5.d0 / 3.d0

  ! MODE B1
!  if( beta > 0.d0 )then
!    km1 = ka / beta
!  else
!    km1 = 0.d0
!  endif
!  vm = km1 * dh
!
!  if( da > 0.d0 ) then
!    va = ka * dh
!  else
!    va = 0.d0
!  endif
!
!  al = sqrt(dh) / ns
!
!  if( h < dm ) then
!    q = vm * dm * (h / dm) ** beta
!  elseif( h < da ) then
!    q = vm * dm + va * (h - dm)
!  else
!    q = vm * dm + va * (h - dm) + al * (h - da) ** m
!  endif

  ! MODE B2
!  if( h > da )then
!    q = (km * dm + ka * (h - dm)) * dh + sqrt(dh) / ns * (h - da) ** m
!  elseif( h >= dm )then
!    q = (km * dm + ka * (h - dm)) * dh
!  else
!    q = km * dm * (h / dm) ** beta * dh
!  endif

  ! MODE B3
  if( h < dm )then
    q = km * dm * (h / dm) ** beta * dh
  elseif( h <= da )then
    q = (km * dm + ka * (h - dm)) * dh
  else
    q = (km * dm + ka * (h - dm)) * dh + sqrt(dh) / ns * (h - da) ** m
  endif

  ! discharge per unit area
  ! (q multiply by width and divide by area)
  q = q * len / area
end subroutine hq
!===============================================================
! water depth (h) to actual water level (lev)
!===============================================================
subroutine h2lev(h, k, lev)
  implicit none
  real(8), intent(in)  :: h
  integer, intent(in)  :: k
  real(8), intent(out) :: lev

!  real(8) rho
  real(8) :: da

  if( soildepth_idx(k) == 0.d0 ) then
    lev = h
  else
    da = soildepth_idx(k) * gammaa_idx(k)
    if( h >= da ) then
      lev = soildepth_idx(k) + (h - da) ! surface water
    else
      !rho = da_temp / soildepth_idx(k)
      !lev = h / rho
      lev = h / gammaa_idx(k)
    endif
  endif
end subroutine h2lev
!===============================================================
!
!===============================================================
subroutine funcrs(hr_idx, hs)
  implicit none
  real(8), intent(inout) :: hr_idx(:)
  real(8), intent(inout) :: hs(:,:)

  type(ch_), pointer :: ch
  type(ch_mesh_), pointer :: mesh
  real(8) :: vr_idx(size(hr_idx))
  real(8) :: hsr_mesh, vsr_mesh
  integer :: k, iMesh, x, y

  real(8) :: vs_before, vs_after
  real(8) :: vr_before, vr_after
  real(8) :: vsr, vsr_tot, vsr_inbalance

  vs_before = 0.d0
  vs_after = 0.d0
  vr_before = 0.d0
  vr_after = 0.d0
  vsr_tot = 0.d0

  do k = 1, nCh
    ch => lst_ch(k)

    call hr2vr(hr_idx(k), k, vr_idx(k))

    if( debug )then
      !call logmsg('ch '//str(k)//' area '//str(ch%area))
      vr_before = vr_idx(k)
      vs_before = 0.d0
      do iMesh = 1, ch%nMesh
        mesh => ch%mesh(iMesh)
        if( mesh%is_outside_domain ) cycle
        x = mesh%x
        y = mesh%y
        call add(vs_before, hs(x,y)*area_slo(x,y))
      enddo
      !call logmsg('volume before (slo) '//str(vs_before)//' (riv) '//str(vr_before))
    endif

    vsr = 0.d0
    do iMesh = 1, ch%nMesh
      mesh => ch%mesh(iMesh)
      x = mesh%x
      y = mesh%y

      if( mesh%is_outside_domain ) cycle

!      if( debug )then
!        call logmsg('mesh '//str(iMesh)//' ('//str((/x,y/),',')//')')
!        call setlog('+x2')
!        call logmsg('area (slo) '//str(area_slo(x,y))//' (riv)'//str(mesh%area)//&
!                    ' leng '//str(mesh%leng)//&
!                    ' hs '//str(hs(x,y))//' hr '//str(hr_idx(k)))
!      endif

      call calc_discharge_slo2riv(&
          hs(x,y), hr_idx(k), vr_idx(k)*mesh%area/ch%area, &
          k, ch%depth, ch%levee, &
          mesh%leng, area_slo(x,y), mesh%area, &
          hsr_mesh, vsr_mesh)

!      if( debug )then
!        call logmsg('vsr '//str(vsr_mesh)//' hsr (slo) '//str(hsr_mesh)//&
!                    ' (riv) '//str(vsr_mesh/mesh%area))
!        call setlog('-x2')
!      endif

      call add(hs(x,y), -hsr_mesh)
      call add(vsr, vsr_mesh)

      if( debug )then
        call add(vs_after, hs(x,y)*area_slo(x,y))
      endif
    enddo  ! iMesh/

    call add(vr_idx(k), vsr)
    call vr2hr(vr_idx(k), k, hr_idx(k))

    vsr_tot = vsr_tot + vsr

    ! TMP: rectangular cross section
!    if( debug )then
!      vr_after = vr_idx(k)
!      vs_after = 0.d0
!      do iMesh = 1, ch%nMesh
!        mesh => ch%mesh(iMesh)
!        if( mesh%is_outside_domain ) cycle
!        x = mesh%x
!        y = mesh%y
!        call add(vs_after, hs(x,y)*area_slo(x,y))
!      enddo

      !vsr_inbalance = (vs_after - vs_before) + (vr_after - vr_before)
      !call logmsg('volume after (slo) '//str(vs_after)//' (riv) '//str(vr_after)//&
      !          '\n         inc (slo) '//str(vs_after-vs_before)//&
      !            ' (riv) '//str(vr_after-vr_before))//&
      !            ' inbalance '//str(vsr_inbalance))
      !if( abs(vsr_inbalance) > 1.d0 )then
      !  call logmsg('ch '//str(k)//' volume inbalance '//str(vsr_inbalance))
      !  call logmsg('ch '//str(k)//' vs_inc '//str(vs_after-vs_before)//&
      !              ' vr_inc '//str(vr_after-vr_before)//&
      !              ' vsr '//str(vsr))
      !endif
      !call logmsg('hs '//str(hs(x,y))//' hr '//str(hr_idx(k))//&
      !            ' vr '//str(vr_idx(k)))
      !call logmsg('vsr '//str(vsr)//&
      !            ' hsr (slo) '//str(vsr/area_slo_idx(k))//&
      !            ' (riv) '//str(vsr/area_riv_idx(k)))
!    endif
  enddo  ! k/

  !if( debug )then
  !  call logmsg('vsr_tot: '//str(vsr_tot))
  !endif
end subroutine funcrs
!===============================================================
!
!===============================================================
subroutine calc_discharge_slo2riv(&
    hs_org, hr_org, vr_org, &
    k, depth, levee, &
    leng_isct, area_slo, area_riv, &
    hsr, vsr)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_dicharge_slo2riv'
  real(8), intent(in) :: hs_org
  real(8), intent(in) :: hr_org
  real(8), intent(in) :: vr_org
  integer, intent(in) :: k
  real(8), intent(in) :: depth
  real(8), intent(in) :: levee
  real(8), intent(in) :: leng_isct
  real(8), intent(in) :: area_slo
  real(8), intent(in) :: area_riv
  real(8), intent(out) :: hsr  ![m]
  real(8), intent(out) :: vsr  ![m3]

  real(8) :: hs, hr, vr
  real(8) :: hs_top, hr_top
  !real(8) :: hr_new
  real(8) :: h1, h2

  real(8), parameter :: mu1 = (2.d0/3.d0) ** (3.d0/2.d0)
  real(8), parameter :: mu2 = 0.35d0
  real(8), parameter :: mu3 = 0.91d0

  ! TMP: rectangular cross section

  hs = hs_org
  hr = hr_org
  vr = vr_org

  hs_top = hs
  hr_top = hr - depth
  !-------------------------------------------------------------
  ! Case a: (levee = 0 and hr_top < 0) or &
  !         (levee > 0 and hr_top < 0 and hs_top <= levee)
  ! -> From slope to river: step fall (vsr: positive)
  if( (levee == 0.d0 .and. hr_top < 0.d0) .or. &
      (levee > 0.d0 .and. hr_top < 0.d0 .and. hs_top <= levee) )then

!    if( debug )then
!      call logmsg('Case a')
!    endif

    hsr = min(mu1 * hs_top * sqrt(gravity * hs_top) * leng_isct * 2 / area_slo * dt, hs)
    vsr = hsr * area_slo

    hs = hs - hsr

    vr = vr + vsr
    call vr2hr(vr, k, hr)

    ! Avoid hs_top < hr_top
    if( hs < hr - depth )then
      hs = hs_org
      hr = hr_org

      hsr = hs - (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
      vsr = hsr * area_slo
      hs = hs - hsr
      hr = (hr*area_riv + vsr) / area_riv
    endif
  !-------------------------------------------------------------
  ! Case b: levee > 0 and hr_top <= levee and hr_top >= 0
  ! -> No exchange
  elseif( levee > 0.d0 .and. hr_top >= 0.d0 .and. &
          hs_top <= levee .and. hr_top <= levee )then

!    if( debug )then
!      call logmsg('Case b')
!    endif

    hsr = 0.d0
    vsr = 0.d0
  !-------------------------------------------------------------
  ! Case c: hs <= hr_top and hr_top >= levee
  ! -> From river to slope: overtopping (vsr: negative)
  ! (incl. hs = 0 and hr_top > 0)
  elseif( hs_top <= hr_top .and. hr_top >= levee )then

!    if( debug )then
!      call logmsg('Case c')
!    endif

    h1 = hr_top - levee
    h2 = hs_top - levee
    if( h2/h1 <= 2.d0/3.d0 )then
      vsr = - min(mu2 * h1 * sqrt(2.d0 * gravity * h1) * leng_isct * 2 * dt, vr)
    else
      vsr = - min(mu3 * h2 * sqrt(2.d0 * gravity * (h1-h2)) * leng_isct * 2 * dt, vr)
    endif
    hsr = vsr / area_slo

!    if( debug )then
!      call logmsg('vsr '//str(vsr)//' vr '//str(vr)//' hr '//str(vr/area_riv))
!      call logmsg('hsr (slo) '//str(hsr)//' (riv) '//str(vsr/area_riv))
!    endif

    vr = vr + vsr
    call vr2hr(vr, k, hr)

    hs = hs - hsr

!    if( debug )then
!      call logmsg('hr '//str(hr)//' hs '//str(hs)//' hr_top '//str(hr-depth))
!    endif

    ! Avoid hs_top > hr_top
    if( hs > hr - depth )then
      hs = hs_org
      hr = hr_org

      hsr = hs - (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
      vsr = hsr * area_slo
      hs = hs - hsr
      hr = (hr*area_riv + vsr) / area_riv
    endif
  !-------------------------------------------------------------
  ! Case d: hs > hr_top and hs >= levee
  ! -> From slope to river: overtopping (hsr: positive)
  elseif( hs_top > hr_top .and. hs_top >= levee )then

!    if( debug )then
!      call logmsg('Case d')
!    endif

    h1 = hs_top - levee
    h2 = hr_top - levee
    if( h2/h1 <= 2.d0/3.d0 )then
      hsr = min(mu2 * h1 * sqrt(2.d0 * gravity * h1) * leng_isct * 2 / area_slo * dt, hs)
    else
      hsr = min(mu3 * h2 * sqrt(2.d0 * gravity * (h1-h2)) * leng_isct * 2 / area_slo * dt, hs)
    endif
    vsr = hsr * area_slo

    hs = hs - hsr

    vr = vr + vsr
    call vr2hr(vr, k, hr)

!print*, '(1)', hs, hr-depth, hsr, hr

    ! Avoid hs_top < hr_top
    if( hs < hr - depth )then
      hs = hs_org
      hr = hr_org

!print*, 'hs_new', (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
!print*, hs, hr_top, area_slo, area_riv

      hsr = hs - (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
      vsr = hsr * area_slo
      hs = hs - hsr
      hr = (hr*area_riv + vsr) / area_riv
    endif

!print*, '(2)', hs, hr-depth, hsr, hr
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call errend('Not matched any case.', PRCNAM, MODNAM)
  endif
end subroutine calc_discharge_slo2riv
!===============================================================
!
!===============================================================
subroutine set_wlv_outlet(&
    hr_idx, hs, &
    sout)
  implicit none
  real(8), intent(inout) :: hs(:,:)
  real(8), intent(inout) :: hr_idx(:)
  real(8), intent(inout) :: sout

  type(ch_), pointer :: ch
  type(outlet_), pointer :: outlet
  real(8) :: vr, vr_new, vr_out
  integer :: iOutlet
  integer :: k, x, y

  do iOutlet = 1, nOutlet
    outlet => lst_outlet(iOutlet)
    k = outlet%iCh
    x = outlet%x
    y = outlet%y
    !-----------------------------------------------------------
    ! Slope
    !-----------------------------------------------------------
    ! hs -> 0
    call add(sout, hs(x,y)*area_slo(x,y))
    hs(x,y) = 0.d0

    ch => lst_ch(k)
    !-----------------------------------------------------------
    ! River
    !-----------------------------------------------------------
    ! Case: zb >= 0
    ! hr -> 0 (>= sea level)
    if( ch%zb >= 0.d0 )then
      call hr2vr(hr_idx(k), k, vr_out)
      hr_idx(k) = 0.d0
    !-----------------------------------------------------------
    ! Case: zb < 0
    ! hr -> -zb (= sea level)
    else
      call hr2vr(hr_idx(k), k, vr)
      call hr2vr(-ch%zb, k, vr_new)
      vr_out = vr - vr_new
      hr_idx(k) = -ch%zb
    endif
    call add(sout, vr_out)
  enddo  ! iOutlet/
end subroutine set_wlv_outlet
!===============================================================
!
!===============================================================
subroutine calc_storage(&
    hr_idx, hs, hg, gampt_ff, &
    sr, ss, si, sg)
  implicit none
  real(8), intent(in)  :: hr_idx(:), hs(:,:), hg(:,:)
  real(8), intent(in)  :: gampt_ff(:,:)
  real(8), intent(out) :: sr, ss, si, sg

  integer :: k, ix, iy
  real(8) :: vr_temp

  sr = 0.d0
  ss = 0.d0
  si = 0.d0
  sg = 0.d0

  do k = 1, nCh
    call hr2vr(hr_idx(k), k, vr_temp)
    sr = sr + vr_temp
  enddo

  do iy = 1, ny
  do ix = 1, nx
    if( domain(ix,iy) == DOMAIN_OUTSIDE ) cycle

    ss = ss + hs(ix,iy) * area_slo(ix,iy)
    si = si + gampt_ff(ix,iy) * area_slo(ix,iy)
    sg = sg - hg(ix,iy) * gammag_idx(slo_ij2idx(ix,iy)) * area_slo(ix,iy)  ! storage deficit
  enddo  ! ix/
  enddo  ! iy/
end subroutine calc_storage
!===============================================================
!
!===============================================================
subroutine calc_storage_inbalance(&
    prcp_sum, aevp_sum, sout, sr, ss, si, sg, sinit, &
    sinbl)
  implicit none
  real(8), intent(in) :: prcp_sum, aevp_sum
  real(8), intent(in) :: sout, sr, ss, si, sg, sinit
  real(8), intent(out) :: sinbl

  sinbl = (prcp_sum - aevp_sum - sout) + (sinit - (sr + ss + si + sg))
end subroutine calc_storage_inbalance
!===============================================================
!
!===============================================================
end module mod_process
