module mod_river
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use def_const
  use def_type
  use mod_global
  use mod_base
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_mod_river
  public :: init_river_storage
  public :: advance_river
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_river'

  real(8), allocatable :: vr(:)
  real(8), allocatable :: vr_tmp(:)
  real(8), allocatable :: dv(:)
  real(8), allocatable :: hr_err(:)
  real(8), allocatable :: q(:)
  real(8), allocatable :: f1(:), f2(:), f3(:), f4(:), f5(:), f6(:)
  real(8) :: tol
  real(8) :: dt_min, dt_max

  real(8), allocatable :: hr_init(:), hr_prev(:)
  logical :: debug = .true.
  integer :: k_ = 1654
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_mod_river()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mod_river'

  selectcase( solver%method )

  case( SOLVER_METHOD__ADAPTIVE_RK45 )

    call init_mod_river__adaptive_rk45()

  case( SOLVER_METHOD__TEST )

    call init_mod_river__test()

  case default

    call errend(msg_invalid_value('solver%method', solver%method))
  endselect
end subroutine init_mod_river
!===============================================================
!
!===============================================================
subroutine init_mod_river__adaptive_rk45()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_mod_river__adaptive_rk45'

  type(solver_adaptive_rk45_), pointer :: rk45

  rk45 => solver%adaptive_rk45

  tol = rk45%error_tolerance
  dt_min = rk45%dt_min_river
  dt_max = time%dt_river

  allocate(vr(river%nCh))
  allocate(vr_tmp(river%nCh))
  allocate(hr_err(river%nCh))
  allocate(q(river%nCh))
  allocate(f1(river%nCh), f2(river%nCh), f3(river%nCh), &
           f4(river%nCh), f5(river%nCh), f6(river%nCh))

  ! DEBUG
  allocate(hr_init(river%nCh))
  allocate(hr_prev(river%nCh))
end subroutine init_mod_river__adaptive_rk45
!===============================================================
!
!===============================================================
subroutine init_mod_river__test()
  implicit none

  type(solver_test_), pointer :: test

  test => solver%test

  tol = test%error_tolerance
  dt_min = test%dt_min_river
  dt_max = time%dt_river

  allocate(vr(river%nCh))
  allocate(vr_tmp(river%nCh))
  allocate(hr_err(river%nCh))
  allocate(q(river%nCh))
  allocate(f1(river%nCh), f2(river%nCh), f3(river%nCh), &
           f4(river%nCh), f5(river%nCh), f6(river%nCh))

  ! DEBUG
  allocate(hr_init(river%nCh))
  allocate(hr_prev(river%nCh))
end subroutine init_mod_river__test
!===============================================================
!
!===============================================================
subroutine init_river_storage(hr_idx)
  implicit none
  real(8), intent(out) :: hr_idx(:)

  type(channel_), pointer :: ch
  type(ch_mesh_), pointer :: chmesh
  real(8) :: vr
  real(8) :: zs, zb
  integer :: k, iMesh

  character(1) :: marker

  hr_idx(:) = 0.d0

  do k = 1, river%nCh
    ch => river%channel(k)

    vr = 0.d0
    do iMesh = 1, ch%nMesh
      chmesh => ch%mesh(iMesh)

      if( chmesh%is_outside_domain )then
        zs = 0.d0
      else
        zs = slope%zs(chmesh%x,chmesh%y)
      endif

      zb = zs - ch%depth

      if( zb < 0.d0 )then
        vr = vr + chmesh%leng * ch%width * -zb
      endif
    enddo  ! iMesh/

    vr = min(vr, ch%volume)

    call vr2hr(ch%area, vr, hr_idx(k))
    hr_idx(k) = min(hr_idx(k), ch%depth)

    !-----------------------------------------------------------
    ! DEBUG
    !-----------------------------------------------------------
!    if( hr_idx(k) > 0.d0 )then
!      marker = ''
!      call logmsg('ch#'//str(k,dgt(river%nCh))//&
!        ' depth: '//str(ch%depth)//' h: '//str(hr_idx(k))//marker)
!    endif
    !-----------------------------------------------------------
  enddo  ! k/
end subroutine init_river_storage
!===============================================================
!
!===============================================================
subroutine advance_river(&
  hr, vro, &
  qr_ave &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_river'
  real(8), intent(inout) :: hr(:)
  real(8), intent(out) :: vro(:)
  real(8), intent(out) :: qr_ave(:)

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call advance_river__adaptive_rk45(&
    hr, vro, &
    qr_ave &
  )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine advance_river
!===============================================================
!
!===============================================================
subroutine advance_river__adaptive_rk45(&
  hr, vro, &
  qr_ave &
)
  use mod_rk45_org
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_river__adaptive_rk45'
  real(8), intent(inout) :: hr(:)
  real(8), intent(out) :: vro(:)  ! discharge from river to ocean
  real(8), intent(out) :: qr_ave(:)

  type(outlet_), pointer :: outlet
  real(8) :: t
  real(8) :: dt
  real(8) :: err
  real(8) :: shrink
  integer :: k
  integer :: iOutlet

  real(8) :: vol, vol_prev

  integer :: kk
  integer :: jNode, iNode_conn
  type(channel_), pointer :: ch
  type(node_), pointer :: nd

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  qr_ave(:) = 0.d0
  vro(:) = 0.d0
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
  hr_init(:) = hr(:)
  hr_prev(:) = hr(:)
  vol_prev = sum(vr)
  !-------------------------------------------------------------

  do k = 1, river%nCh
    call hr2vr(river%area_idx(k), hr(k), vr(k))
  enddo

  t = 0.d0
  dt = time%dt_model
  call logmsg('t: '//str(t,'f15.9')//' dt: '//str(dt,'f15.9')//&
    ' v min: '//str(minval(vr))//' max: '//str(maxval(vr)))

  do while( t < time%dt_model )
    ! (1)
    call calc_discharge(vr, f1, q, hr)
    vr_tmp = vr + b21 * dt * f1
    where( vr_tmp < 0.d0 ) vr_tmp = 0.d0

    ! (2)
    call calc_discharge(vr_tmp, f2, q, hr)
    vr_tmp = vr + dt * (b31 * f1 + b32 * f2)
    where( vr_tmp < 0.d0 ) vr_tmp = 0.d0

    ! (3)
    call calc_discharge(vr_tmp, f3, q, hr)
    vr_tmp = vr + dt * (b41 * f1 + b42 * f2 + b43 * f3)
    where( vr_tmp < 0.d0 ) vr_tmp = 0.d0

    ! (4)
    call calc_discharge(vr_tmp, f4, q, hr)
    vr_tmp = vr + dt * (b51 * f1 + b52 * f2 + b53 * f3 + b54 * f4)
    where( vr_tmp < 0.d0 ) vr_tmp = 0.d0

    ! (5)
    call calc_discharge(vr_tmp, f5, q, hr)
    vr_tmp = vr + dt * (b61 * f1 + b62 * f2 + b63 * f3 + b64 * f4 + b65 * f5)
    where( vr_tmp < 0.d0 ) vr_tmp = 0.d0

    ! (6)
    call calc_discharge(vr_tmp, f6, q, hr)
    !vr_tmp = vr + dt * (c1 * f1 + c3 * f3 + c4 * f4 + c6 * f6)
    !where( vr_tmp < 0.d0 ) vr_tmp = 0.d0

    ! (e)
    hr_err = dt * abs(dc1 * f1 + dc3 * f3 + dc4 * f4 + dc5 * f5 + dc6 * f6) / river%area_idx
    err = maxval( hr_err )

    ! Update the state: time and water storage
    if( err <= tol .or. dt == dt_min )then

      vr = vr + (c1 * f1 + c3 * f3 + c4 * f4 + c6 * f6) * dt
      where( vr < 0.d0 ) vr = 0.d0

      t = t + dt

      shrink = 1.1d0
      !---------------------------------------------------------
      ! DEBUG
      !---------------------------------------------------------
!      do k = 1, river%nCh
!        call vr2hr(river%area_idx(k), vr(k), hr(k))
!      enddo
!      ch => river%channel(k_)
!      call logmsg('ch#'//str(k_)//&
!         ' h: '//str(hr_prev(k_))//' zw: '//str(hr_prev(k_)-river%zb_idx(k_))//&
!         ' -> h: '//str(hr(k_))//' zw: '//str(hr(k_)-river%zb_idx(k_)))
!      do jNode = 1, 2
!        nd => ch%node(jNode)
!        do iNode_conn = 1, nd%nNode_conn
!          kk = nd%node_conn(iNode_conn)%iCh
!          call logmsg('ch#'//str(kk)//&
!            ' h: '//str(hr_prev(kk))//' zw: '//str(hr_prev(kk)-river%zb_idx(kk))//&
!            ' -> h: '//str(hr(kk))//' zw: '//str(hr(kk)-river%zb_idx(kk)))
!        enddo
!      enddo
!      hr_prev = hr
      !---------------------------------------------------------

      ! Update the state of outlet
      do iOutlet = 1, river%nOutlet
        outlet => river%outlet(iOutlet)
        k = outlet%iCh

        call add(vro(k), vr(k) - outlet%v_sealevel)
        vr(k) = outlet%v_sealevel
        !call add(vro(k), vr(k))
        !vr(k) = 0.d0

        !call vr2hr(river%area_idx(k), vr(k), hr(k))
      enddo

    else
      shrink = max(0.5d0, SAFETY * (tol / err) ** PSHRNK)
    endif

    ! Timestep control
    dt = min(max(dt * shrink, dt_min), time%dt_model - t, dt_max)

!    if( shrink < 1.d0 )then
!      k = maxloc(hr_err,1)
!      call vr2hr(river%area_idx(k), vr(k), hr(k))
!      call logmsg('t: '//str(t,'f10.6')//' dt: '//str(dt,'f10.6')//&
!        ' shrink: '//str(shrink,'es10.3')//&
!        ' err max: '//str(err)//' loc: '//str(k)//&
!        ' h: '//str(hr(k),'f7.3'))
!    endif

  enddo  ! while t < time%dt_model

  do k = 1, river%nCh
    call vr2hr(river%area_idx(k), vr(k), hr(k))
  enddo
  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
!  call logmsg('Updates:')
!  ch => river%channel(k_)
!  call logmsg('ch#'//str(k_)//&
!     ' h: '//str(hr_prev(k_))//' zw: '//str(hr_prev(k_)-river%zb_idx(k_))//&
!     ' -> h: '//str(hr(k_))//' zw: '//str(hr(k_)-river%zb_idx(k_)))
!  do jNode = 1, 2
!    nd => ch%node(jNode)
!    do iNode_conn = 1, nd%nNode_conn
!      kk = nd%node_conn(iNode_conn)%iCh
!      call logmsg('ch#'//str(kk)//&
!        ' h: '//str(hr_prev(kk))//' zw: '//str(hr_prev(kk)-river%zb_idx(kk))//&
!        ' -> h: '//str(hr(kk))//' zw: '//str(hr(kk)-river%zb_idx(kk)))
!    enddo
!  enddo

!  vol = sum(vr)
!  call logmsg('vol: '//str(vol_prev)//' -> '//str(vol))

!  do iOutlet = 1, river%nOutlet
!    outlet => river%outlet(iOutlet)
!    k = outlet%iCh
!    call logmsg('outlet#'//str(iOutlet)//' h: '//str(hr_init(k))//' -> '//str(hr(k)))
!  enddo
  call logmsg('discharge to ocean: '//str(sum(vro)))
  !-------------------------------------------------------------
  call logret()
end subroutine advance_river__adaptive_rk45
!===============================================================
!
!===============================================================
subroutine calc_discharge(vr, fr, qr, hr)
  implicit none
  real(8), intent(in) :: vr(:)
  real(8), intent(out) :: fr(:)
  real(8), intent(out) :: qr(:)
  real(8), intent(inout) :: hr(:)  ! workspace

  type(channel_), pointer :: ch
  type(node_), pointer :: nd
  real(8) :: zb_p, hr_p
  real(8) :: zb_n, hr_n
  real(8) :: leng
  real(8) :: dh
  real(8) :: hw
  real(8) :: qr_tmp
  integer :: k, kk
  integer :: jNode
  integer :: iNode_conn

  fr(:) = 0.d0
  qr(:) = 0.d0

  do k = 1, river%nCh
    call vr2hr(river%area_idx(k), vr(k), hr(k))
  enddo

  do k = 1, river%nCh
    ch => river%channel(k)

    zb_p = river%zb_idx(k)
    hr_p = hr(k)

    do jNode = 1, 2
      nd => ch%node(jNode)
      do iNode_conn = 1, nd%nNode_conn
        kk = nd%node_conn(iNode_conn)%iCh

        zb_n = river%zb_idx(kk)
        hr_n = hr(kk)

        leng = (ch%leng + river%channel(kk)%leng) * 0.5d0
        dh = ((zb_p + hr_p) - (zb_n + hr_n)) / leng

        if( dh <= 0.d0 ) cycle

        if( zb_p < zb_n )then
          hw = max(0.d0, hr_p + zb_p - zb_n)
        else
          hw = hr_p
        endif
        call hq_riv(&
            dh, hw, ch%width, river%ns, & ! in
            qr_tmp) ! out

        call add(qr(k), qr_tmp)

        call add(fr(k), -qr_tmp)
        call add(fr(kk), qr_tmp)
      enddo  ! iNode_conn/
    enddo  ! jNode/
  enddo  ! k/
end subroutine calc_discharge
!===============================================================
!
!===============================================================
subroutine hq_riv(dh, h, w, ns, q)
  implicit none
  real(8), intent(in) :: dh, h, w
  real(8), intent(in) :: ns
  real(8), intent(out) :: q

  real(8) :: a, r
  real(8), parameter :: m = 2.d0 / 3.d0

  a = sqrt(abs(dh)) / ns
  r = (w*h) / (w+h*2.d0)
  q = a * r**m * w*h
end subroutine hq_riv
!===============================================================
!
!===============================================================
end module mod_river
