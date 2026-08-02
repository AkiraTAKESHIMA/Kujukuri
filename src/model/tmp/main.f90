program main
  use lib_const
  use lib_base
  use lib_log
  use def_const
  use def_type
  use mod_config
  use mod_driver
  implicit none
  type(config_) :: config
  type(time_) :: time
  type(grid_) :: grid
  type(static_) :: static
  type(state_) :: state
  type(tendency_) :: tendency

  call prepare_static_data(config, time, grid, static)

   call run(config, time, grid, static, state, tendency)
end program main
