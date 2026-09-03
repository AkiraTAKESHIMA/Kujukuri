program main
  use lib_log
  use def_const
  use def_type
  use mod_config
  use mod_driver
  implicit none
!  type(time_) :: time
!  type(grid_) :: grid
!  type(solver_) :: solver
!  type(static_slope_) :: slope
!  type(static_river_) :: river
!  type(forcing_) :: forcing
!  type(output_) :: output

  call logbgn('main', '', '-p -x2 +tr')

  call prepare_static_data()
  !call prepare_static_data(&
  !  time, grid, solver, slope, river, forcing, output &
  !)

  call run()
  !call run(&
  !  time, grid, solver, slope, river, forcing, output &
  !)

  call logret()
end program main
