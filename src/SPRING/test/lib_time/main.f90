program main
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  implicit none

  call test_datetime()

  call test_operator()

contains
!===============================================================
!
!===============================================================
subroutine test_datetime()
  implicit none
  type(datetime_) :: t1, t2

  call logent('test_datetime')

  t1 = datetime(2006, 1, 12)
  t2 = datetime_now()

  call print_datetime(t1, 't1')
  call print_datetime(t2, 't2')

  call logext()
end subroutine test_datetime
!===============================================================
!
!===============================================================
subroutine test_operator()
  implicit none
  type(datetime_) :: t1, t2
  real(8) :: dt

  call logent('test_operator')

  t1 = datetime(2026, 10, 1, 12, 30,  5)  ! 2026/10/01 12:30:05
  t2 = datetime(2026, 10, 3, 10,  0, 48)  ! 2026/10/03 10:00:48
  dt = t2 - t1
  call print_datetime(t1, 't1')
  call print_datetime(t2, 't2')
  call logmsg('dt = t2 - t1: '//str(dt,'f20.3'))

  call logmsg('t1+dt == t2: '//str(t1+dt == t2))

  call logext()
end subroutine test_operator
!===============================================================
!
!===============================================================
subroutine print_datetime(t, name)
  implicit none
  type(datetime_), intent(in) :: t
  character(*), intent(in) :: name

  call logmsg(str(name)//&
    '\n  datetime: '//&
    str(t%year,-4)//'/'//str(t%month,-2)//'/'//str(t%day,-2)//' '//&
    str(t%hour,-2)//'/'//str(t%minute,-2)//'/'//str(t%second,-2)//'.'//str(t%millisecond,-3)//&
    '\n  zone: '//str(t%zone)//&
    '\n  date: '//str(t%date)//&
    '\n  time: '//str(t%time))
end subroutine print_datetime
!===============================================================
!
!===============================================================
end program main
