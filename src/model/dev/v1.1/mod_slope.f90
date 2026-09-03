module mod_slope
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use lib_io
  use def_const
  use def_type
  use mod_global
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_mod_slope
  public :: advance_slope
  public :: h2sfc
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_slope'

  real(8), allocatable :: hs_tmp(:)  !(nGrid)
  real(8), allocatable :: hs_err(:)  !(nGrid)
  real(8), allocatable :: qs1(:,:)  !(nDir,nGrid)
  real(8), allocatable :: qs2(:,:)  !(nDir,nGrid)
  real(8), allocatable :: qs3(:,:)  !(nDir,nGrid)
  real(8), allocatable :: qs4(:,:)  !(nDir,nGrid)
  real(8), allocatable :: qs5(:,:)  !(nDir,nGrid)
  real(8), allocatable :: qs6(:,:)  !(nDir,nGrid)
  real(8), allocatable :: fs1(:), fs2(:), fs3(:), fs4(:), fs5(:), fs6(:)
  real(8) :: tol
  real(8) :: dt_llim, dt_max

  real(8), allocatable :: qs(:,:)
  real(8), allocatable :: fs(:)
  real(8), allocatable :: hs1_idx(:), hs2_idx(:)
  real(8) :: vol, vol_prev, vol_init
  real(8) :: vol_prcp
  real(8) :: vol_add
  integer :: k_debug = 273935
  integer :: l_debug = 1
  character(8) :: WFMT_T = 'f10.6'
  integer :: dgt_xy
  real(8) :: dt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_mod_slope()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mod_slope'

  dgt_xy = dgt(max(slope%nx,slope%ny))

  selectcase( solver%method )

  case( SOLVER_METHOD__ADAPTIVE_RK45 )

    call init_mod_slope__adaptive_rk45()

  case( SOLVER_METHOD__TEST )

    call init_mod_slope__test()

  endselect
end subroutine init_mod_slope
!===============================================================
!
!===============================================================
subroutine init_mod_slope__adaptive_rk45()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mod_slope__adaptive_rk45'

  type(solver_adaptive_rk45_), pointer :: rk45

  rk45 => solver%adaptive_rk45

  tol = rk45%error_tolerance
  dt_llim = rk45%dt_min_slope
  dt_max = time%dt_slope

  allocate(hs_tmp(slope%nGrid))
  allocate(hs_err(slope%nGrid))
  allocate(qs1(slope%nDir,slope%nGrid))
  allocate(qs2(slope%nDir,slope%nGrid))
  allocate(qs3(slope%nDir,slope%nGrid))
  allocate(qs4(slope%nDir,slope%nGrid))
  allocate(qs5(slope%nDir,slope%nGrid))
  allocate(qs6(slope%nDir,slope%nGrid))
  allocate(fs1(slope%nGrid))
  allocate(fs2(slope%nGrid))
  allocate(fs3(slope%nGrid))
  allocate(fs4(slope%nGrid))
  allocate(fs5(slope%nGrid))
  allocate(fs6(slope%nGrid))

  allocate(qs(slope%nDir,slope%nGrid))
  allocate(fs(slope%nGrid))
end subroutine init_mod_slope__adaptive_rk45
!===============================================================
!
!===============================================================
subroutine init_mod_slope__test()
  implicit none
  type(solver_test_), pointer :: test

  test => solver%test

  tol = test%error_tolerance
  dt_llim = test%dt_min_slope
  dt_max = time%dt_slope

  allocate(qs(slope%nDir,slope%nGrid))
  allocate(fs(slope%nGrid))
end subroutine init_mod_slope__test
!===============================================================
!
!===============================================================
subroutine advance_slope(&
  qp_idx, &
  hs_idx, gampt_ff_idx, &
  q_ave_idx &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_slope'
  real(8), intent(in) :: qp_idx(:)
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(out) :: q_ave_idx(:,:)

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( solver%method )

  case( SOLVER_METHOD__ADAPTIVE_RK45 )
    selectcase( slope%version%advance_slope )
    case( 11 )
      call advance_slope__adaptive_rk45_v11(&
        qp_idx, &
        hs_idx, gampt_ff_idx, &
        q_ave_idx &
      )
    case( 12 )
      call advance_slope__adaptive_rk45_v12(&
        qp_idx, &
        hs_idx, gampt_ff_idx, &
        q_ave_idx &
      )
    case( 13 )
      call advance_slope__adaptive_rk45_v13(&
        qp_idx, &
        hs_idx, gampt_ff_idx, &
        q_ave_idx &
      )
    case default
      call errend(msg_invalid_value('slope%version%advance_slope', slope%version%advance_slope))
    endselect
  case( SOLVER_METHOD__TEST )
    call advance_slope__test(&
      hs_idx, qp_idx &
    )
  case default
    call errend(msg_invalid_value('solver%method', solver%method))

  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine advance_slope
!===============================================================
!
!===============================================================
!
!
!
!
!===============================================================
!
!===============================================================
subroutine advance_slope__adaptive_rk45_v11(&
  qp_idx, &
  hs_idx, gampt_ff_idx, &
  q_ave_idx &
)
  use mod_rk45_org
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_slope__adaptive_rk45'
  real(8), intent(in) :: qp_idx(:)
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(out) :: q_ave_idx(:,:)

  real(8) :: t
!  real(8) :: dt
  real(8) :: err
  real(8) :: shrink

  integer :: k

  integer :: n_qshrink, n_qshrink_max
!  real(8) :: vol_slo, vol_slo_prev, vol_prcp

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  q_ave_idx(:,:) = 0.d0
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
  vol_init = sum(hs_idx * slope%area_idx)
  vol_prev = vol_init
  !-------------------------------------------------------------
  t = 0.d0
  dt = time%dt_model
!  call logmsg('t: '//str(t,'f10.6')//' dt: '//str(dt,'f10.6'))

  n_qshrink_max = 0

  do while( t < time%dt_model )
    call timestep_calc_discharge__adaptiverk45(hs_idx, qp_idx, dt)

    hs_err = dt * abs(dc1 * fs1 + dc3 * fs3 + dc4 * fs4 + dc5 * fs5 + dc6 * fs6)
    err = maxval( hs_err, mask=slope%domain_mask_idx/=DOMAIN__OUTSIDE )

    ! Update states
    if( err <= tol .or. dt <= dt_llim )then

      hs_tmp = (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6) * dt

      k = 0
      if( any(hs_idx + hs_tmp < 0.d0) )then
        k = minloc(hs_idx+hs_tmp,1)
        !call logmsg('h: '//str(hs_idx(k))//' -> '//str(hs_idx(k)+hs_tmp(k)))
        call errend('sl#'//str(k)//&
          ' h: '//str(hs_idx(k))//' -> '//str(hs_idx(k)+hs_tmp(k))//&
          '\n  f1: '//str(fs1(k))//' f2: '//str(fs2(k))//' f3: '//str(fs3(k))//&
          ' f4: '//str(fs4(k))//' f5: '//str(fs5(k))//' f6: '//str(fs6(k)))
      endif
      vol_add = -sum(slope%area_idx*hs_idx, mask=hs_idx<0.d0)

!      hs_idx = hs_idx + (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6) * dt
!      where( hs_idx < 0.d0 ) hs_idx = 0.d0
!
!      t = t + dt
!
!      shrink = 1.1d0

      if( k == 0 .or. dt == dt_llim )then
        hs_idx = hs_idx + (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6) * dt
        where( hs_idx < 0.d0 ) hs_idx = 0.d0

        t = t + dt

        shrink = 1.1d0
      else
        shrink = 0.9d0
      endif
      !---------------------------------------------------------
      ! DEBUG
      !---------------------------------------------------------
      vol = sum(hs_idx * slope%area_idx)
      vol_prcp = sum(qp_idx * slope%area_idx) * dt
!      call logmsg('t: '//str(t,'f10.6')//' dt: '//str(dt,'f10.6')//&
!        ' v slo: '//str(vol_prev)//' -> '//str(vol)//&
!        ' add: '//str(vol_add)//&
!        ' err: '//str((vol-vol_prev)-vol_prcp-vol_add))
      vol_prev = vol
      !---------------------------------------------------------
    else
      shrink = max(0.5d0, SAFETY * (tol / err) ** PSHRNK)
    endif

    ! Timestep control
    dt = min(max(dt * shrink, dt_llim), time%dt_model - t, dt_max)

!    if( shrink < 1.d0 )then
!      k = maxloc(hs_err,1)
!      call logmsg('t: '//str(t,'f10.6')//' dt: '//str(dt,'f10.6')//&
!        ' shrink: '//str(shrink,'es10.3')//&
!        ' err max: '//str(err)//' loc: '//str(k)//&
!        ' h: '//str(hs_idx(k),'f7.3'))
!    endif

  enddo  ! while t < t%dt_model
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
!  vol = sum(slope%area_idx * hs_idx)
!  call logmsg('vol: '//str(vol_init)//' -> '//str(vol))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine advance_slope__adaptive_rk45_v11
!===============================================================
!
!===============================================================
subroutine advance_slope__adaptive_rk45_v12(&
  qp_idx, &
  hs_idx, gampt_ff_idx, &
  q_ave_idx &
)
  use mod_rk45_org
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_slope__adaptive_rk45'
  real(8), intent(in) :: qp_idx(:)
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(out) :: q_ave_idx(:,:)

  real(8) :: t
!  real(8) :: dt
  real(8) :: err
  real(8) :: shrink

  integer :: k, kk, l
  real(8) :: qsout
  real(8) :: qshrnk

  real(8) :: dt_min
  logical :: debug_this

!  real(8) :: vol_slo, vol_slo_prev, vol_prcp

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  q_ave_idx(:,:) = 0.d0
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
!  vol_init = sum(hs_idx * slope%area_idx)
!  vol_prev = vol_init
  !-------------------------------------------------------------
  t = 0.d0
  dt = time%dt_model

  dt_min = dt
!  call logmsg('t: '//str(t,'f10.6')//' dt: '//str(dt,'f10.6'))

  do while( t < time%dt_model )
    call timestep_calc_discharge__adaptiverk45(hs_idx, qp_idx, dt)

    hs_err = dt * abs(dc1 * fs1 + dc3 * fs3 + dc4 * fs4 + dc5 * fs5 + dc6 * fs6)
    err = maxval( hs_err, mask=slope%domain_mask_idx/=DOMAIN__OUTSIDE )

    ! Update states
    if( err <= tol .or. dt <= dt_llim )then

      if( dt < dt_min )then
        call logmsg('t: '//str(t,WFMT_T)//' dt min: '//str(dt,WFMT_T))
        dt_min = dt
      endif

      hs_idx = hs_idx + (c1 * fs1 + c3 * fs3 + c4 * fs4 + c6 * fs6) * dt
      where( hs_idx < 0.d0 ) hs_idx = 0.d0

      t = t + dt

      shrink = 1.1d0
      !---------------------------------------------------------
      ! DEBUG
      !---------------------------------------------------------
!      vol = sum(hs_idx * slope%area_idx)
!      vol_prcp = sum(qp_idx * slope%area_idx) * dt
!      call logmsg('t: '//str(t,'f10.6')//' dt: '//str(dt,'f10.6')//&
!        ' v slo: '//str(vol_prev)//' -> '//str(vol)//&
!        ' add: '//str(vol_add)//&
!        ' err: '//str((vol-vol_prev)-vol_prcp-vol_add))
!      vol_prev = vol
      !---------------------------------------------------------
    else
      shrink = max(0.5d0, SAFETY * (tol / err) ** PSHRNK)
    endif

    ! Timestep control
    dt = min(max(dt * shrink, dt_llim), time%dt_model - t, dt_max)
  enddo  ! while t < t%dt_model
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
!  vol = sum(slope%area_idx * hs_idx)
!  call logmsg('vol: '//str(vol_init)//' -> '//str(vol))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine advance_slope__adaptive_rk45_v12
!===============================================================
!
!===============================================================
subroutine advance_slope__adaptive_rk45_v13(&
  qp_idx, &
  hs_idx, gampt_ff_idx, &
  q_ave_idx &
)
  use mod_rk45_org
  use mod_base
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_slope__adaptive_rk45'
  real(8), intent(in) :: qp_idx(:)
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(inout) :: gampt_ff_idx(:)
  real(8), intent(out) :: q_ave_idx(:,:)

  real(8) :: t
!  real(8) :: dt
  real(8) :: err
  real(8) :: shrink

  integer :: k, kk, l
  real(8) :: qsout
  real(8) :: qshrnk

  integer :: k_errmax
  real(8) :: dt_min
  logical :: debug_this
  logical :: debug_model
  integer :: un_h, un_t
  integer :: count_slope
  real(8), allocatable :: hs(:,:)

!  real(8) :: vol_slo, vol_slo_prev, vol_prcp

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  q_ave_idx(:,:) = 0.d0
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
!  vol_init = sum(hs_idx * slope%area_idx)
!  vol_prev = vol_init
  !-------------------------------------------------------------
  t = 0.d0
  dt = time%dt_model
  dt_min = dt

  debug_model = time%count_model == count_model_debug

  if( debug_model )then
    count_slope = 0
    allocate(hs(slope%nx,slope%ny))
    call idx2ij(slope, hs_idx, hs)

    open(newunit=un_h, file=joined(output%dir,'hs_detail.bin'), &
         form='unformatted', access='direct', recl=8*size(hs), status='replace')
    write(un_h,rec=1) hs

    open(newunit=un_t, file=joined(output%dir,'slope_time.txt'), status='replace')
    write(un_t,"(f10.6)") t
  endif

  do while( t < time%dt_model )
    call timestep_calc_discharge__adaptiverk45(hs_idx, qp_idx, dt)

    hs_err = dt * abs(dc1 * fs1 + dc3 * fs3 + dc4 * fs4 + dc5 * fs5 + dc6 * fs6)
    err = maxval( hs_err, mask=slope%domain_mask_idx/=DOMAIN__OUTSIDE )
    k_errmax = maxloc( hs_err, 1, mask=slope%domain_mask_idx/=DOMAIN__OUTSIDE )

    ! Update states
    if( err <= tol .or. dt <= dt_llim )then

      if( dt < dt_min )then
        call logmsg('t: '//str(t,WFMT_T)//' dt min: '//str(dt,WFMT_T)//&
          ' err max: '//str(err)//' loc: '//str(k_errmax,dgt(slope%nGrid))//&
          ' ('//str((/slope%idx2i(k_errmax),slope%idx2j(k_errmax)/),dgt_xy,',')//')')
        dt_min = dt
      endif

      qs = (c1 * qs1 + c3 * qs3 + c4 * qs4 + c6 * qs6) * dt

!      k = k_debug
!      kk = slope%down_idx(l,k)
!      call logmsg('h: '//str(hs_idx(k))//' h_n: '//str(hs_idx(kk))//&

      do k = 1, slope%nGrid
!        debug_this = k == k_debug
!        debug_this = .false.

        qsout = 0.d0
        do l = 1, slope%nDir
          if( qs(l,k) > 0.d0 ) call add(qsout, qs(l,k))
          kk = slope%up_idx(l,k)
          if( kk < 0 ) cycle
          if( qs(l,kk) < 0.d0 ) call add(qsout, -qs(l,kk)*slope%area_idx(kk)/slope%area_idx(k))
        enddo

!        if( debug_this )then
!          call logmsg('qsout*dt: '//str(qsout*dt)//' hs+qp*dt: '//str(hs_idx(k)+qp_idx(k)*dt))
!        endif

        if( qsout*dt <= hs_idx(k) + qp_idx(k)*dt ) cycle

        qshrnk = (hs_idx(k) + qp_idx(k)*dt) / (qsout*dt)

        do l = 1, slope%nDir
          if( qs(l,k) > 0.d0 ) qs(l,k) = qs(l,k) * qshrnk
          kk = slope%up_idx(l,k)
          if( kk < 0 ) cycle
          if( qs(l,kk) < 0.d0 ) qs(l,kk) = qs(l,kk) * qshrnk
        enddo
      enddo

      do k = 1, slope%nGrid
        fs(k) = qp_idx(k) - sum(qs(:,k))
        do l = 1, slope%nDir
          kk = slope%up_idx(l,k)
          if( kk < 0 ) cycle
          call add(fs(k), qs(l,kk)*slope%area_idx(kk)/slope%area_idx(k))
        enddo
      enddo

      hs_idx = hs_idx + fs * dt
      where( hs_idx < 0.d0 ) hs_idx = 0.d0

      t = t + dt

      shrink = 1.1d0

      call add(count_slope)
      if( debug_model )then
        call idx2ij(slope, hs_idx, hs)
        write(un_h, rec=count_slope+1) hs
        write(un_t,"(f10.6)") t
      endif
    else
      shrink = max(0.5d0, SAFETY * (tol / err) ** PSHRNK)
    endif

    ! Timestep control
    dt = min(max(dt * shrink, dt_llim), time%dt_model - t, dt_max)
  enddo  ! while t < t%dt_model

  if( debug_model )then
    deallocate(hs)
    close(un_h)
    close(un_t)
  endif
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
!  vol = sum(slope%area_idx * hs_idx)
!  call logmsg('vol: '//str(vol_init)//' -> '//str(vol))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine advance_slope__adaptive_rk45_v13
!===============================================================
!
!===============================================================
subroutine timestep_calc_discharge__adaptiverk45(hs_idx, qp_idx, dt)
  use mod_rk45_org
  implicit none
  real(8), intent(in) :: hs_idx(:)
  real(8), intent(in) :: qp_idx(:)
  real(8), intent(in) :: dt

  selectcase( slope%version%calc_discharge )
  case( 1 )
    call calc_discharge_v01(hs_idx, qp_idx, fs1, qs1)
    hs_tmp = hs_idx + dt * b21 * fs1
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v01(hs_tmp, qp_idx, fs2, qs2)
    hs_tmp = hs_idx + dt * (b31 * fs1 + b32 * fs2)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v01(hs_tmp, qp_idx, fs3, qs3)
    hs_tmp = hs_idx + dt * (b41 * fs1 + b42 * fs2 + b43 * fs3)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v01(hs_tmp, qp_idx, fs4, qs4)
    hs_tmp = hs_idx + dt * (b51 * fs1 + b52 * fs2 + b53 * fs3 + b54 * fs4)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v01(hs_tmp, qp_idx, fs5, qs5)
    hs_tmp = hs_idx + dt * (b61 * fs1 + b62 * fs2 + b63 * fs3 + b64 * fs4 + b65 * fs5)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v01(hs_tmp, qp_idx, fs6, qs6)
  case( 2 )
    call calc_discharge_v02(hs_idx, qp_idx, fs1, qs1)
    hs_tmp = hs_idx + dt * b21 * fs1
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v02(hs_tmp, qp_idx, fs2, qs2)
    hs_tmp = hs_idx + dt * (b31 * fs1 + b32 * fs2)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v02(hs_tmp, qp_idx, fs3, qs3)
    hs_tmp = hs_idx + dt * (b41 * fs1 + b42 * fs2 + b43 * fs3)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v02(hs_tmp, qp_idx, fs4, qs4)
    hs_tmp = hs_idx + dt * (b51 * fs1 + b52 * fs2 + b53 * fs3 + b54 * fs4)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v02(hs_tmp, qp_idx, fs5, qs5)
    hs_tmp = hs_idx + dt * (b61 * fs1 + b62 * fs2 + b63 * fs3 + b64 * fs4 + b65 * fs5)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v02(hs_tmp, qp_idx, fs6, qs6)
  case( 3 )
    call calc_discharge_v03(hs_idx, qp_idx, fs1, qs1)
    hs_tmp = hs_idx + dt * b21 * fs1
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v03(hs_tmp, qp_idx, fs2, qs2)
    hs_tmp = hs_idx + dt * (b31 * fs1 + b32 * fs2)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v03(hs_tmp, qp_idx, fs3, qs3)
    hs_tmp = hs_idx + dt * (b41 * fs1 + b42 * fs2 + b43 * fs3)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v03(hs_tmp, qp_idx, fs4, qs4)
    hs_tmp = hs_idx + dt * (b51 * fs1 + b52 * fs2 + b53 * fs3 + b54 * fs4)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v03(hs_tmp, qp_idx, fs5, qs5)
    hs_tmp = hs_idx + dt * (b61 * fs1 + b62 * fs2 + b63 * fs3 + b64 * fs4 + b65 * fs5)
    where( hs_tmp < 0.d0 ) hs_tmp = 0.d0

    call calc_discharge_v03(hs_tmp, qp_idx, fs6, qs6)
  case default
    call errend(msg_invalid_value('slope%version%calc_discharge', slope%version%calc_discharge))
  endselect
end subroutine timestep_calc_discharge__adaptiverk45
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
subroutine advance_slope__test(&
  hs_idx, qp_idx &
)
  implicit none
  real(8), intent(inout) :: hs_idx(:)
  real(8), intent(in) :: qp_idx(:)

  real(8) :: t
  integer :: iter
  real(8) :: err
  integer :: k_errmax
  real(8) :: dt_min
  integer :: nStep

  dt_min = time%dt_model
  dt = time%dt_model / 8.d0
  t = 0.d0
  nStep = 0
  do while( t < time%dt_model )
    dt = dt * 2

    call calc_discharge_v03(hs_idx, qp_idx, fs, qs)
    hs2_idx = hs_idx + fs * dt

    do
      hs1_idx = hs2_idx

      dt = dt / 2
      hs2_idx = hs_idx
      do iter = 1, 2
        call calc_discharge_v03(hs2_idx, qp_idx, fs, qs)
        hs2_idx = hs2_idx + fs * dt
      enddo

      err = maxval(abs(hs1_idx - hs2_idx))
      k_errmax = maxloc(abs(hs1_idx - hs2_idx), 1)
      if( err < tol ) exit
    enddo

    if( dt < dt_min )then
      dt_min = dt
      call logmsg('t: '//str(t,WFMT_T)//' dt min: '//str(dt,WFMT_T)//&
        ' err max: '//str(err)//' loc: '//str(k_errmax)//' ('//&
        str((/slope%idx2i(k_errmax),slope%idx2j(k_errmax)/),dgt_xy,',')//')')
    endif

    t = t + dt * 2
    hs_idx = hs2_idx
    call add(nStep)
  enddo

  call logmsg('t: '//str(t,WFMT_T)//' step: '//str(nStep))
  call logmsg('h max: '//str(maxval(hs_idx))//' loc: '//str(maxloc(hs_idx,1)))
end subroutine advance_slope__test
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
subroutine calc_discharge_v01(hs_idx, qp_idx, fs_idx, q_idx)
  implicit none
  real(8), intent(in)  :: hs_idx(:)  !(nGrid)
  real(8), intent(in)  :: qp_idx(:)  !(nGrid)
  real(8), intent(out) :: fs_idx(:)  !(nGrid)
  real(8), intent(out) :: q_idx(:,:)  !(nDir,nGrid)

  real(8) :: soildepth_p, gammaa_p, zb_p, hs_p, ns_p, ka_p, da_p, km_p, dm_p, b_p
  real(8) :: soildepth_n, gammaa_n, zb_n, hs_n, ns_n, ka_n, da_n, km_n, dm_n, b_n
  integer :: dif_p, dif_n
  real(8) :: area, dis, len
  real(8) :: lev_p, lev_n
  real(8) :: dh
  real(8) :: hw

  integer :: k, kk, l

!  logical :: debug_this

  !$omp parallel do private(kk,l,area,dis,len,dh,hw, &
  !$omp   soildepth_p, gammaa_p, hs_p,zb_p,ns_p,ka_p,da_p,km_p,dm_p,b_p,dif_p,lev_p, &
  !$omp   soildepth_n, gammaa_n, hs_n,zb_n,ns_n,ka_n,da_n,km_n,dm_n,b_n,dif_n,lev_n)
  do k = 1, slope%nGrid

    hs_p = hs_idx(k)
    soildepth_p = slope%soildepth_idx(k)
    gammaa_p = slope%gammaa_idx(k)
    zb_p = slope%zb_idx(k)
    ns_p = slope%ns_idx(k)
    ka_p = slope%ka_idx(k)
    da_p = slope%da_idx(k)
    km_p = slope%km_idx(k)
    dm_p = slope%dm_idx(k)
    b_p  = slope%beta_idx(k)
    dif_p = slope%diffusion_idx(k)
    area = slope%area_idx(k)

    ! 8-direction: lmax = 4, 4-direction: lmax = 2
    ! (1: right, 2: down, 3: right down, 4: left down)
    do l = 1, slope%nDir
      kk = slope%down_idx(l, k)
      if( kk < 0 )then
        q_idx(l,k) = 0.d0
        cycle
      endif
!debug_this = k == k_ .or. kk == k_

      dis = slope%dis_idx(l,k)
      len = slope%len_idx(l,k)

      ! information of the destination cell
      hs_n = hs_idx(kk)
      soildepth_n = slope%soildepth_idx(kk)
      gammaa_n = slope%gammaa_idx(kk)
      zb_n = slope%zb_idx(kk)
      ns_n = slope%ns_idx(kk)
      ka_n = slope%ka_idx(kk)
      da_n = slope%da_idx(kk)
      km_n = slope%km_idx(kk)
      dm_n = slope%dm_idx(kk)
      b_n = slope%beta_idx(kk)
      dif_n = slope%diffusion_idx(kk)

      call h2lev(hs_p, lev_p, soildepth_p, gammaa_p)
      call h2lev(hs_n, lev_n, soildepth_n, gammaa_n)

      dh = ((zb_p + lev_p) - (zb_n + lev_n)) / dis

      ! water coming in or going out?
      if( dh >= 0.d0 ) then
        ! going out
        hw = max(hs_p + min(zb_p - zb_n, 0.d0), 0.d0)

        call hq(ns_p, ka_p, da_p, km_p, dm_p, b_p, &
                hw, dh, &
                len, area, &
                q_idx(l,k))
      else
        ! coming in
        hw = max(hs_n + min(zb_n - zb_p, 0.d0), 0.d0)

        call hq(ns_n, ka_n, da_n, km_n, dm_n, b_n, &
                hw, -dh, &
                -len, area, &
                q_idx(l,k))
      endif

!if( debug_this )then
!  call logmsg('('//str(k)//') lev '//str(lev_p)//' ('//str(kk)//') lev '//str(lev_n)//&
!    ' dh '//str(dh)//' q '//str(q_idx(l,k)))
!endif
    enddo  ! l/
  enddo  ! k/
  !$omp end parallel do

  do k = 1, slope%nGrid
    fs_idx(k) = qp_idx(k) - sum(q_idx(:,k))
  enddo

  do k = 1, slope%nGrid
    do l = 1, slope%nDir
      kk = slope%down_idx(l,k)
      if( kk < 0 ) cycle
      call add(fs_idx(kk), q_idx(l,k) * slope%area_idx(k) / slope%area_idx(kk))
    enddo
  enddo

!call logmsg('fs '//str(fs_idx(k_)))
end subroutine calc_discharge_v01
!===============================================================
!
!===============================================================
subroutine calc_discharge_v02(h, qp, f, qout)
  implicit none
  real(8), intent(in)  :: h(:)  !(nGrid)
  real(8), intent(in)  :: qp(:)  !(nGrid)
  real(8), intent(out) :: f(:)  !(nGrid) dh/dt
  real(8), intent(out) :: qout(:,:)  !(nDir,nGrid)

  integer :: k, kk, l

  !$omp parallel do
  do k = 1, slope%nGrid
    call calc_discharge_grid_v02(k, h, qout(:,k))
  enddo
  !$omp end parallel do

  do k = 1, slope%nGrid
    f(k) = qp(k) - sum(qout(:,k))
  enddo

  do k = 1, slope%nGrid
    do l = 1, slope%nDir
      kk = slope%down_idx(l,k)
      if( kk < 0 ) cycle
      call add(f(kk), qout(l,k) * slope%area_idx(k) / slope%area_idx(kk))
    enddo
  enddo
end subroutine calc_discharge_v02
!===============================================================
!
!===============================================================
subroutine calc_discharge_grid_v02(k, hs_idx, q)
  implicit none
  integer, intent(in) :: k
  real(8), intent(in) :: hs_idx(:)
  real(8), intent(out) :: q(:)

  real(8) :: soildepth_p, gammaa_p, zb_p, hs_p, ns_p, ka_p, da_p, km_p, dm_p, b_p
  real(8) :: soildepth_n, gammaa_n, zb_n, hs_n, ns_n, ka_n, da_n, km_n, dm_n, b_n
  integer :: dif_p, dif_n
  real(8) :: area, dis, len
  real(8) :: lev_p, lev_n
  real(8) :: dh
  real(8) :: hw

  integer :: kk, l

    hs_p = hs_idx(k)
    soildepth_p = slope%soildepth_idx(k)
    gammaa_p = slope%gammaa_idx(k)
    zb_p = slope%zb_idx(k)
    ns_p = slope%ns_idx(k)
    ka_p = slope%ka_idx(k)
    da_p = slope%da_idx(k)
    km_p = slope%km_idx(k)
    dm_p = slope%dm_idx(k)
    b_p  = slope%beta_idx(k)
    dif_p = slope%diffusion_idx(k)
    area = slope%area_idx(k)

    ! 8-direction: lmax = 4, 4-direction: lmax = 2
    ! (1: right, 2: down, 3: right down, 4: left down)
    do l = 1, slope%nDir
      kk = slope%down_idx(l, k)
      if( kk < 0 )then
        q(l) = 0.d0
        cycle
      endif

      dis = slope%dis_idx(l,k)
      len = slope%len_idx(l,k)

      ! information of the destination cell
      hs_n = hs_idx(kk)
      soildepth_n = slope%soildepth_idx(kk)
      gammaa_n = slope%gammaa_idx(kk)
      zb_n = slope%zb_idx(kk)
      ns_n = slope%ns_idx(kk)
      ka_n = slope%ka_idx(kk)
      da_n = slope%da_idx(kk)
      km_n = slope%km_idx(kk)
      dm_n = slope%dm_idx(kk)
      b_n = slope%beta_idx(kk)
      dif_n = slope%diffusion_idx(kk)

      call h2lev(hs_p, lev_p, soildepth_p, gammaa_p)
      call h2lev(hs_n, lev_n, soildepth_n, gammaa_n)

      dh = ((zb_p + lev_p) - (zb_n + lev_n)) / dis

      ! water coming in or going out?
      if( dh >= 0.d0 ) then
        ! going out
        hw = max(hs_p + min(zb_p - zb_n, 0.d0), 0.d0)

        call hq(ns_p, ka_p, da_p, km_p, dm_p, b_p, &
                hw, dh, &
                len, area, &
                q(l))
      else
        ! coming in
        hw = max(hs_n + min(zb_n - zb_p, 0.d0), 0.d0)

        call hq(ns_n, ka_n, da_n, km_n, dm_n, b_n, &
                hw, -dh, &
                -len, area, &
                q(l))
      endif
    enddo  ! l/
end subroutine calc_discharge_grid_v02
!===============================================================
!
!===============================================================
subroutine calc_discharge_v03(hs_idx, qp_idx, fs_idx, q_idx)
  implicit none
  real(8), intent(in)  :: hs_idx(:)  !(nGrid)
  real(8), intent(in)  :: qp_idx(:)  !(nGrid)
  real(8), intent(out) :: fs_idx(:)  !(nGrid)
  real(8), intent(out) :: q_idx(:,:)  !(nDir,nGrid)

  real(8) :: soildepth_p, gammaa_p, zb_p, hs_p, ns_p, ka_p, da_p, km_p, dm_p, b_p
  real(8) :: soildepth_n, gammaa_n, zb_n, hs_n, ns_n, ka_n, da_n, km_n, dm_n, b_n
  integer :: dif_p, dif_n
  real(8) :: area, dis, len
  real(8) :: lev_p, lev_n
  real(8) :: dh
  real(8) :: hw

  integer :: k, kk, l

  logical :: debug_this

  !$omp parallel do private(kk,l,area,dis,len,dh,hw, &
  !$omp   soildepth_p, gammaa_p, hs_p,zb_p,ns_p,ka_p,da_p,km_p,dm_p,b_p,dif_p,lev_p, &
  !$omp   soildepth_n, gammaa_n, hs_n,zb_n,ns_n,ka_n,da_n,km_n,dm_n,b_n,dif_n,lev_n)
  do k = 1, slope%nGrid
    hs_p = hs_idx(k)
    soildepth_p = slope%soildepth_idx(k)
    gammaa_p = slope%gammaa_idx(k)
    zb_p = slope%zb_idx(k)
    ns_p = slope%ns_idx(k)
    ka_p = slope%ka_idx(k)
    da_p = slope%da_idx(k)
    km_p = slope%km_idx(k)
    dm_p = slope%dm_idx(k)
    b_p  = slope%beta_idx(k)
    dif_p = slope%diffusion_idx(k)
    area = slope%area_idx(k)

    ! 8-direction: lmax = 4, 4-direction: lmax = 2
    ! (1: right, 2: down, 3: right down, 4: left down)
    do l = 1, slope%nDir 
      q_idx(l,k) = 0.d0

      kk = slope%down_idx(l, k)
      if( kk < 0 ) cycle

      dis = slope%dis_idx(l,k)
      len = slope%len_idx(l,k)

      ! information of the destination cell
      hs_n = hs_idx(kk)

      if( hs_p < 1d-6 .and. hs_n < 1d-6 ) cycle

      soildepth_n = slope%soildepth_idx(kk)
      gammaa_n = slope%gammaa_idx(kk)
      zb_n = slope%zb_idx(kk)
      ns_n = slope%ns_idx(kk)
      ka_n = slope%ka_idx(kk)
      da_n = slope%da_idx(kk)
      km_n = slope%km_idx(kk)
      dm_n = slope%dm_idx(kk)
      b_n = slope%beta_idx(kk)
      dif_n = slope%diffusion_idx(kk)

      call h2lev(hs_p, lev_p, soildepth_p, gammaa_p)
      call h2lev(hs_n, lev_n, soildepth_n, gammaa_n)

      dh = ((zb_p + lev_p) - (zb_n + lev_n)) / dis

      if( abs(dh) < 1d-5 ) cycle

      ! water coming in or going out?
      if( dh >= 0.d0 ) then
        ! going out
        hw = max(hs_p + min(zb_p - zb_n, 0.d0), 0.d0)

        call hq(ns_p, ka_p, da_p, km_p, dm_p, b_p, &
                hw, dh, &
                len, area, &
                q_idx(l,k))
      else
        ! coming in
        hw = max(hs_n + min(zb_n - zb_p, 0.d0), 0.d0)

        call hq(ns_n, ka_n, da_n, km_n, dm_n, b_n, &
                hw, -dh, &
                -len, area, &
                q_idx(l,k))
      endif

!      debug_this = k == k_debug .and. l == l_debug
!      if( debug_this )then
!        call logmsg(&
!          'self h: '//str(hs_p)//' lev: '//str(lev_p)//' H: '//str(zb_p+lev_p)//&
!        '\ndown h: '//str(hs_n)//' lev: '//str(lev_n)//' H: '//str(zb_n+lev_n)//&
!        '\ndh: '//str(dh)//' q*dt: '//str(q_idx(l,k)*dt))
!      endif
    enddo  ! l/
  enddo  ! k/
  !$omp end parallel do

  do k = 1, slope%nGrid
    fs_idx(k) = qp_idx(k) - sum(q_idx(:,k))
  enddo

  do k = 1, slope%nGrid
    do l = 1, slope%nDir
      kk = slope%down_idx(l,k)
      if( kk < 0 ) cycle
      call add(fs_idx(kk), q_idx(l,k) * slope%area_idx(k) / slope%area_idx(kk))
    enddo
  enddo
end subroutine calc_discharge_v03
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

  real(8), parameter :: m = 5.d0 / 3.d0

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
subroutine h2lev(h, lev, soildepth, gammaa)
  implicit none
  real(8), intent(in)  :: h
  real(8), intent(out) :: lev
  real(8), intent(in)  :: soildepth, gammaa

  real(8) :: da

  if( soildepth == 0.d0 ) then
    lev = h
  else
    da = soildepth * gammaa
    if( h >= da ) then
      lev = soildepth + (h - da) ! surface water
    else
      lev = h / gammaa
    endif
  endif
end subroutine h2lev
!===============================================================
!
!===============================================================
subroutine h2sfc(h_idx, sfc_idx)
  implicit none
  real(8), intent(in) :: h_idx(:)
  real(8), intent(out) :: sfc_idx(:)

  integer :: k

  do k = 1, slope%nGrid
    sfc_idx(k) = max(h_idx(k) - slope%soildepth_idx(k)*slope%gammaa_idx(k), 0.d0)
  enddo
end subroutine h2sfc
!===============================================================
!
!===============================================================
end module mod_slope
