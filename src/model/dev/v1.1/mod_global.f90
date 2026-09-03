module mod_global
  use def_type
  implicit none

  type(time_), target :: time
  type(grid_), target :: grid
  type(solver_), target :: solver
  type(static_slope_), target :: slope
  type(static_river_), target :: river
  type(forcing_), target :: forcing
  type(output_), target :: output

  integer :: count_model_debug = 87
end module mod_global
