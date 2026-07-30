program main
  use lib_log
  use c1_type_gs
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        run
  use mod_finalize, only: &
        finalize
  implicit none
  type(gs_)     :: org
  type(gs_)     :: dst
  type(output_) :: output

  call logbgn('program main', '', '+tr')
  !-------------------------------------------------------------
  call read_settings(org, dst, output)

  call run(org, dst, output)

  call finalize(org, dst)
  !-------------------------------------------------------------
  call logret('program main', '')
end program main
