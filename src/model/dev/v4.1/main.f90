program main
  use mod_config, only: &
    prep_static_data
  use mod_driver, only: &
    prep_mod_driver, &
    exec_simulation
  implicit none

  call prep_static_data()

  call prep_mod_driver()

  call exec_simulation()
end program main
