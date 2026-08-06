module mod_driver
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_math
  use lib_io
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
subroutine run(&
  config, time, grid, solver, &
  static, state, tendency &
)
  use mod_slope, only: &
    advance_slope, &
    allocate_state_slope, &
    allocate_workspace_slope
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
  use mod_util, only: &
    sub_slo_idx2ij
  implicit none
  type(config_), intent(in) :: config
  type(time_), intent(inout) :: time
  type(grid_), intent(in) :: grid
  type(solver_), intent(in) :: solver
  type(static_), intent(in) :: static
  type(state_) , intent(inout) :: state
  type(tendency_), intent(inout) :: tendency

  type(forcing_) :: forcing
  type(workspace_) :: workspace

  integer :: un_hs

  call open_file_prcp(config, grid)

  open(newunit=un_hs, file=joined(config%output%dir,'hs.bin'), &
       form='unformatted', access='direct', recl=grid%nx*grid%ny*4, &
       status='replace')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call allocate_state_slope(static%slope, state%slope)
  state%slope%hs_idx(:) = 0.d0

  call allocate_workspace_slope(static%slope, workspace%slope)

  call allocate_state_river(static%river, state%river)
  state%river%hr_idx(:) = 0.d0
  state%river%vr_idx(:) = 0.d0

  call allocate_workspace_river(static%river, workspace%river)

  call allocate_workspace_rk45(static, workspace)

  allocate(forcing%prcp(grid%nx,grid%ny))
  allocate(forcing%prcp_idx(static%slope%nGrid))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('t step_end: '//str((time%t_end-time%t_start)/time%dt_model)//&
       ' end: '//str(time%t_end))

  time%t_now = time%t_start
  time%t_next = min(time%t_now + time%dt_model, time%t_end)
  time%count_model = 1

  time%t_forcing_next = time%t_start
  time%count_forcing = 0

  do while( time%t_now < time%t_end )
    call logmsg('t step: '//str(time%count_model)//&
        ' now: '//str(time%t_now)//' next: '//str(time%t_next))
    call read_prcp(config, static, time, forcing)

    !call advance_river(&
    !  static, state, tendency, forcing, time, &
    !  solver, &
    !  workspace &
    !)

    call advance_slope(&
      static, state, tendency, forcing, time, &
      solver, &
      workspace &
    )

call logmsg('wlv min: '//str(minval(state%slope%hs_idx))//&
  ' ('//str(minloc(state%slope%hs_idx,1))//')'//&
  ' max: '//str(maxval(state%slope%hs_idx))//&
  ' ('//str(maxloc(state%slope%hs_idx,1))//')')
    where( state%slope%hs_idx < 0.d0 )
      state%slope%hs_idx = 0.d0
    endwhere

    time%count_model = time%count_model + 1
    time%t_now = time%t_now + time%dt_model
    time%t_next = min(time%t_now + time%dt_model, time%t_end)

    call sub_slo_idx2ij(static%slope, state%slope%hs_idx, state%slope%hs)
    write(un_hs, rec=time%count_model) real(state%slope%hs,4)
!if( time%count_model == 3 ) exit
  enddo

  call close_file_prcp()

  close(un_hs)
end subroutine run
!=============================================================
!
!=============================================================
end module mod_driver
