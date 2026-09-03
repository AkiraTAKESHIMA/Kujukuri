program main
  use lib_const
  use lib_base
  use lib_log
  use mod_set
  use mod_river
  implicit none

  call setlog('+tr')

  call init()

  call run()
end program main
