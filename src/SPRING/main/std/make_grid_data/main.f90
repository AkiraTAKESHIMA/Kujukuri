program main
  use lib_log
  use c1_type_opt
  use c1_type_gs
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        make_grid_data
  use mod_finalize, only: &
        finalize
  implicit none
  type(gs_) :: a

  call logbgn('program main', '', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(a)

  call make_grid_data(a)

  call finalize(a)
  !-------------------------------------------------------------
  call logret('program main', '')
end program main
