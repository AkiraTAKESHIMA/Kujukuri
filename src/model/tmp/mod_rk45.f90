module mod_rk45
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: rk45
  public :: allocate_workspace_rk45
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface
    subroutine func(static, state, forcing, y, dydt, dydt_out, workspace)
      use def_type
      type(static_), intent(in) :: static
      type(state_), intent(in), target :: state
      type(forcing_), intent(in) :: forcing
      real(8), intent(in) :: y(:)
      real(8), intent(out) :: dydt(:)
      real(8), intent(out) :: dydt_out(:)
      type(workspace_), intent(inout), target :: workspace
    end subroutine
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine rk45(&
  func, calc_state_sum, subst_state, associate_workspace_rk45, &
  static, state, forcing, tendency, time, &
  solver, &
  workspace &
)
  implicit none

  interface
    subroutine func(static, state, forcing, y, dydt, dydt_out, workspace)
      use def_type
      type(static_), intent(in) :: static
      type(state_), intent(in), target :: state
      type(forcing_), intent(in) :: forcing
      real(8), intent(in) :: y(:)
      real(8), intent(out) :: dydt(:)
      real(8), intent(out) :: dydt_out(:)
      type(workspace_), intent(inout), target :: workspace
    end subroutine func

    subroutine calc_state_sum(state, dy, yt)
      use def_type
      type(state_), intent(in) :: state
      real(8), intent(in) :: dy(:)
      real(8), intent(out) :: yt(:)
    end subroutine calc_state_sum

    subroutine subst_state(state, y)
      use def_type
      type(state_), intent(inout) :: state
      real(8), intent(in) :: y(:)
    end subroutine subst_state

    subroutine associate_workspace_rk45(workspace, ws)
      use def_type
      type(workspace_), intent(in), target :: workspace
      type(workspace_rk45_submodel_), pointer :: ws
    end subroutine associate_workspace_rk45
  end interface

  type(static_), intent(in) :: static
  type(state_), intent(inout) :: state
  type(forcing_), intent(in) :: forcing
  type(tendency_), intent(inout) :: tendency
  type(time_), intent(in) :: time
  type(solver_), intent(in) :: solver
  type(workspace_), intent(inout) :: workspace

  type(workspace_rk45_submodel_), pointer :: ws
  real(8) :: t, dt
  real(8) :: tol
  real(8) :: err_norm
  real(8) :: factor

  call associate_workspace_rk45(workspace, ws)

  tol = solver%rk45%error_tolerance

  t = time%t_now
  dt = time%dt_model

  do while( t < time%t_next )
    if( t + dt > time%t_next ) dt = time%t_next - t

    call rk45_step(&
      func, calc_state_sum, &
      static, state, forcing, t, dt, &
      workspace, ws)

    err_norm = sum((ws%y5 - ws%y4)**2) / real(ws%n,8)

    if( err_norm <= tol )then
      t = t + dt
      call subst_state(state, ws%y5)
    endif

    ! timestep control
    print*, t, err_norm
    if( err_norm == 0.d0 )then
      factor = 0.5d0
    else
      factor = 0.9d0 * (tol/err_norm) ** 0.2d0
      factor = min(0.5d0, max(0.1d0, factor))
    endif
  enddo

  nullify(ws)
end subroutine rk45
!===============================================================
!
!===============================================================
subroutine rk45_step(&
  func, calc_state_sum, &
  static, state, forcing, t, dt, &
  workspace, ws &
)
  implicit none

  interface
    subroutine func(static, state, forcing, y, dydt, dydt_out, workspace)
      use def_type
      type(static_), intent(in) :: static
      type(state_), intent(in), target :: state
      type(forcing_), intent(in) :: forcing
      real(8), intent(in) :: y(:)
      real(8), intent(out) :: dydt(:)
      real(8), intent(out) :: dydt_out(:)
      type(workspace_), intent(inout), target :: workspace
    end subroutine func

    subroutine calc_state_sum(state, dy, yt)
      use def_type
      type(state_), intent(in) :: state
      real(8), intent(in) :: dy(:)
      real(8), intent(out) :: yt(:)
    end subroutine calc_state_sum
  end interface

  type(static_), intent(in) :: static  ! static variables
  type(state_), intent(in) :: state  ! state variables
  type(forcing_), intent(in) :: forcing  ! forcing
  real(8), intent(in) :: t  ! initial time
  real(8), intent(in) :: dt  ! timestep
  type(workspace_), intent(inout) :: workspace
  type(workspace_rk45_submodel_), intent(inout) :: ws

  ! k1
  ! t1 = t
  call calc_state_sum(state, 0.d0*ws%k1, ws%yt)
  call func(static, state, forcing, ws%yt, ws%k1, ws%q1, workspace)

  ! k2
  ! t2 = t + dt/5.d0
  call calc_state_sum(state, dt*(1.d0/5.d0)*ws%k1, ws%yt)
  call func(static, state, forcing, ws%yt, ws%k2, ws%q2, workspace)

  ! k3
  ! t3 = t + dt*3.d0/10.d0
  call calc_state_sum(state, dt*(3.d0/40.d0*ws%k1 + 9.d0/40.d0*ws%k2), ws%yt)
  call func(static, state, forcing, ws%yt, ws%k3, ws%q3, workspace)

  ! k4
  ! t4 = t + dt * 4.d0 / 5.d0
  call calc_state_sum(state, &
    dt*(44.d0/45.d0*ws%k1 &
      -56.d0/15.d0*ws%k2 &
      +32.d0/9.d0*ws%k3), &
    ws%yt)
  call func(static, state, forcing, ws%yt, ws%k4, ws%q4, workspace)

  ! k5
  ! t5 = t + dt * 8.d0 / 9.d0
  call calc_state_sum(state, &
    dt*(19372.d0/6561.d0*ws%k1 &
      -25360.d0/2187.d0*ws%k2 &
      +64448.d0/6561.d0*ws%k3 &
      -212.d0/729.d0*ws%k4), &
    ws%yt)
  call func(static, state, forcing, ws%yt, ws%k5, ws%q5, workspace)

  ! k6
  ! t6 = t + dt
  call calc_state_sum(state, &
    dt*(9017.d0/3168.d0*ws%k1 &
      -355.d0/33.d0*ws%k2 &
      +46732.d0/5247.d0*ws%k3 &
      +49.d0/176.d0*ws%k4 &
      -5103.d0/18656.d0*ws%k5), &
    ws%yt)
  call func(static, state, forcing, ws%yt, ws%k6, ws%q6, workspace)


  ! 5th order solution
  call calc_state_sum(state, &
    dt*(35.d0/384.d0*ws%k1 &
      +500.d0/1113.d0*ws%k3 &
      +125.d0/192.d0*ws%k4 &
      -2187.d0/6784.d0*ws%k5 &
      +11.d0/84.d0*ws%k6), &
    ws%y5)


  ! 4th order embedded solution
  call calc_state_sum(state, &
    dt*(5179.d0/57600.d0*ws%k1 &
      +7571.d0/16695.d0*ws%k3 &
      +393.d0/640.d0*ws%k4 &
      -92097.d0/339200.d0*ws%k5 &
      +187.d0/2100.d0*ws%k6 &
      +1.d0/40.d0*0.d0), &
    ws%y4)
end subroutine rk45_step
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
subroutine allocate_workspace_rk45(static, workspace)
  implicit none
  type(static_), intent(in) :: static
  type(workspace_), intent(inout) :: workspace

  call allocate_workspace_rk45_submodel(static%river%nCh, workspace%rk45%river)
  call allocate_workspace_rk45_submodel(static%slope%nSlo, workspace%rk45%slope)
end subroutine allocate_workspace_rk45
!===============================================================
!
!===============================================================
subroutine allocate_workspace_rk45_submodel(n, workspace)
  implicit none
  integer, intent(in) :: n
  type(workspace_rk45_submodel_), intent(inout) :: workspace

  workspace%n = n
  allocate(workspace%y5(n))
  allocate(workspace%y4(n))
  allocate(workspace%yt(n))
  allocate(workspace%k1(n))
  allocate(workspace%k2(n))
  allocate(workspace%k3(n))
  allocate(workspace%k4(n))
  allocate(workspace%k5(n))
  allocate(workspace%k6(n))
  allocate(workspace%q1(n))
  allocate(workspace%q2(n))
  allocate(workspace%q3(n))
  allocate(workspace%q4(n))
  allocate(workspace%q5(n))
  allocate(workspace%q6(n))
end subroutine allocate_workspace_rk45_submodel
!===============================================================
!
!===============================================================
end module mod_rk45
