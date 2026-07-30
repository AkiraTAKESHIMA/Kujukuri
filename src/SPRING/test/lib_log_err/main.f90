program main
  use lib_log
  use mod_msg, only: &
        test_msg
  implicit none

  call logbgn('program main', '')

  call test_msg()

  call logret()
end program main
