program main
  use mod_config, only: &
    prep_static_data
  use mod_driver, only: &
    prep_driver    , &
    exec_simulation, &
    finalize
  implicit none

  call prep_static_data()

  call prep_driver()

  call exec_simulation()

  call finalize()
end program main
