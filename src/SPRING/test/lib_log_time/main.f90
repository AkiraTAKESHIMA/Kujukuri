program main
  use lib_log
  use mod_time, only: &
        test_time
  implicit none

  call logbgn('program main', '')

  call test_time()

  call logret()
end program main
