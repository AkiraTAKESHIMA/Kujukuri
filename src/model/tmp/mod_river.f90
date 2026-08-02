module mod_river
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: advance_river
  public :: allocate_state_river
  public :: allocate_workspace_river
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  logical :: debug = .false.
  integer :: k_debug = 0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine advance_river(&
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

  type(state_river_), pointer :: river
  integer :: iCh

  river => state%river

  do iCh = 1, static%river%nCh
    call hr2vr(static%river, river%hr_idx(iCh), iCh, river%vr_idx(iCh))
  enddo

  call rk45(funcr, calc_state_sum, subst_state, associate_workspace_rk45, &
            static, state, forcing, tendency, time, &
            solver, &
            workspace)

  do iCh = 1, static%river%nCh
    call vr2hr(static%river, river%vr_idx(iCh), iCh, river%hr_idx(iCh))
  enddo

  nullify(river)
end subroutine advance_river
!===============================================================
!
!===============================================================
subroutine funcr(static, state, forcing, vr_idx, fr_idx, qr_idx, workspace)
  implicit none
  type(static_), intent(in) :: static
  type(state_), intent(in), target :: state
  type(forcing_), intent(in) :: forcing
  real(8), intent(in) :: vr_idx(:)
  real(8), intent(out) :: fr_idx(:)
  real(8), intent(out) :: qr_idx(:)
  type(workspace_), intent(inout), target :: workspace

  type(state_river_), pointer :: river
  type(workspace_river_), pointer :: wsr
  integer :: iCh

  river => state%river
  wsr => workspace%river

  fr_idx(:) = 0.d0
  qr_idx(:) = 0.d0

  do iCh = 1, static%river%nCh
    call vr2hr(static%river, vr_idx(iCh), iCh, wsr%hr_idx(iCh))
  enddo

  call qr_calc(&
    static%river, wsr%hr_idx, & ! in
    qr_idx, fr_idx) ! out

  nullify(river)
  nullify(wsr)
end subroutine funcr
!===============================================================
!
!===============================================================
subroutine qr_calc(static, hr_idx, qr_idx, fr_idx)
  implicit none
  type(static_river_), intent(in) :: static
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(out) :: qr_idx(:)
  real(8), intent(out) :: fr_idx(:)

  type(channel_), pointer :: ch
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

  do k = 1, static%nCh
    ch => static%channel(k)

    zb_p = static%zb_idx(k)
    hr_p = hr_idx(k)

    do jNode = 1, 2
      nd => ch%node(jNode)
      do iNode_conn = 1, nd%nNode_conn
        kk = nd%node_conn(iNode_conn)%iCh

        zb_n = static%zb_idx(kk)
        hr_n = hr_idx(kk)

        leng = (ch%leng + static%channel(kk)%leng) * 0.5d0
        dh = ((zb_p + hr_p) - (zb_n + hr_n)) / leng

        if( dh <= 0.d0 ) cycle

        if( zb_p < zb_n )then
          hw = max(0.d0, hr_p + zb_p - zb_n)
        else
          hw = hr_p
        endif
        call hq_riv(&
            static, dh, hw, ch%width, k, & ! in
            qr_temp) ! out
        call add(qr_idx(k), qr_temp)

        call add(fr_idx(k), -qr_temp)
        call add(fr_idx(kk), qr_temp)

if( debug )then
  if( k == k_debug .or. kk == k_debug )then
    call logmsg('qr_calc'//&
     '\n  ch('//str(k,dgt(static%nCh))//') hr '//str(hr_p)//' zb+hr '//str(zb_p+hr_p)//&
     '\n  ch('//str(kk,dgt(static%nCh))//') hr '//str(hr_n)//' zb+hr '//str(zb_n+hr_n)//&
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
subroutine hq_riv(static, dh, h, w, k, q)
  implicit none
  type(static_river_), intent(in) :: static
  real(8), intent(in) :: dh, h, w
  integer, intent(in) :: k
  real(8), intent(out) :: q

  real(8) :: a, r
  real(8), parameter :: m = 2.d0 / 3.d0

  a = sqrt(abs(dh)) / static%ns
  r = (w*h) / (w+h*2.d0)
  q = a * r**m * w*h
end subroutine hq_riv
!===============================================================
!
!===============================================================
subroutine hr2vr(static, hr, k, vr)
  implicit none
  type(static_river_), intent(in) :: static
  real(8), intent(in) :: hr
  integer, intent(in) :: k
  real(8), intent(out) :: vr

  vr = hr * static%area_idx(k)
end subroutine hr2vr
!===============================================================
!
!===============================================================
subroutine vr2hr(static, vr, k, hr)
  implicit none
  type(static_river_), intent(in) :: static
  real(8), intent(in) :: vr
  integer, intent(in) :: k
  real(8), intent(out) :: hr

  hr = vr / static%area_idx(k)
end subroutine vr2hr
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
subroutine calc_state_sum(state, dy, yt)
  implicit none
  type(state_), intent(in) :: state
  real(8), intent(in) :: dy(:)
  real(8), intent(out) :: yt(:)

  yt(:) = state%river%hr_idx(:) + dy(:)
end subroutine calc_state_sum
!===============================================================
!
!===============================================================
subroutine subst_state(state, y)
  implicit none
  type(state_), intent(inout) :: state
  real(8), intent(in) :: y(:)

  state%river%hr_idx(:) = y(:)
end subroutine subst_state
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

  ws => workspace%rk45%river
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
subroutine allocate_state_river(static, state)
  implicit none
  type(static_river_), intent(in) :: static
  type(state_river_), intent(inout) :: state

  allocate(state%hr_idx(static%nCh))
  allocate(state%vr_idx(static%nCh))
end subroutine allocate_state_river
!===============================================================
!
!===============================================================
subroutine allocate_workspace_river(static, workspace)
  implicit none
  type(static_river_), intent(in) :: static
  type(workspace_river_), intent(inout) :: workspace

  allocate(workspace%hr_idx(static%nCh))
  allocate(workspace%qr_idx(static%nCh))
end subroutine allocate_workspace_river
!===============================================================
!
!===============================================================
end module mod_river
