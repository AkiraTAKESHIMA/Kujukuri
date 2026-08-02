module mod_driver
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_math
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: run
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_driver'
  !-------------------------------------------------------------
contains
!=============================================================
!
!=============================================================
subroutine run(config, time, grid, static, state, tendency)
  use mod_river, only: &
    advance_river, &
    allocate_state_river, &
    allocate_workspace_river
  use mod_forcing, only: &
    open_file_prcp, &
    close_file_prcp, &
    read_prcp
  use mod_rk45, only: &
    allocate_workspace_rk45
  implicit none
  type(config_), intent(in) :: config
  type(time_), intent(inout) :: time
  type(grid_), intent(in) :: grid
  type(static_), intent(in) :: static
  type(state_) , intent(inout) :: state
  type(tendency_), intent(inout) :: tendency

  type(forcing_) :: forcing
  type(solver_) :: solver
  type(workspace_) :: workspace
  integer :: it

  solver%rk45%error_tolerance = config%adaptive_rk%error_tolerance

  call open_file_prcp(config, grid)

  time%t_now = time%t_start
  time%t_next = min(time%t_now + time%dt_model, time%t_end)
  it = 1

  call allocate_state_river(static%river, state%river)
  state%river%hr_idx(:) = 0.d0
  state%river%vr_idx(:) = 0.d0

  call allocate_workspace_river(static%river, workspace%river)

  call allocate_workspace_rk45(static, workspace)

  do while( time%t_now < time%t_end )
    print*, it, time%t_now, time%t_end
    call read_prcp(it, forcing%prcp)
    !call sub_slo_ij2idx(forcing%prcp, forcing%prcp_idx)

    call advance_river(&
      static, state, tendency, forcing, time, &
      solver, &
      workspace)

    it = it + 1
    time%t_now = time%t_now + time%dt_model
    time%t_next = min(time%t_now + time%dt_model, time%t_end)
  enddo

  call close_file_prcp()
end subroutine run
!=============================================================
!
!=============================================================
end module mod_driver
