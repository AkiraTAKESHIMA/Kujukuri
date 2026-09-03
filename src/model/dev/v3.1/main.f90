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
    prepare_forcing
  use mod_bound, only: &
    load_bound
  use mod_driver, only: &
    init_model, &
    exec_simulation
  implicit none

  call prepare_static_data()

  call prepare_forcing()

  call load_bound()

  call init_model()

  call exec_simulation()
end program RRI
