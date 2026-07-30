program main
  use lib_const
  use mod_main
  implicit none

  call logbgn('program main', '')

  call sub1

  call logret()
end program main
