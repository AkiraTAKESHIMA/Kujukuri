! RRI.f90
!
! coded by T.Sayama
!
! ver 1.4.2.7
!
program RRI
  use mod_config, only: &
    prepare_static_data
  use mod_forcing, only: &
    prepare_rain, &
    prepare_evp
  use mod_bound, only: &
    read_bound
  use mod_driver, only: &
    alloc_arrays           , &
    load_initial_conditions, &
    init_storage           , &
    exec_simulation
  implicit none

  call prepare_static_data()

  call prepare_rain()

  call prepare_evp()

  call read_bound()

  call alloc_arrays()

  call load_initial_conditions()

  call init_storage()

  call exec_simulation()
end program RRI
