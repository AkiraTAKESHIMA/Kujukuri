module mod_main
  use lib_const
  use lib_log
  implicit none
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_main'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine sub1()
  implicit none
  call logbgn('sub1', MODNAM)

  call sub1_1()

  call sub1_2()

  call logret()
end subroutine sub1
!===============================================================
!
!===============================================================
subroutine sub1_1()
  implicit none
  call logbgn('sub1_1', MODNAM)

  call logmsg('step 1')
  call logmsg('inc +2')
  call setlog('+x2')

  call logmsg('step 2')

  call logret()
end subroutine sub1_1
!===============================================================
!
!===============================================================
subroutine sub1_2()
  implicit none
  call logbgn('sub1_2', MODNAM)

  call logmsg('step 1')
  call logmsg('inc +2')
  call setlog('+x2')

  call logmsg('step 2')
  call logmsg('inc +4')
  call setlog('+x4')

  call logmsg('step 3')
  call logmsg('inc -2')
  call setlog('-x2')

  call logmsg('step 4')

  call logret()
end subroutine sub1_2
!===============================================================
!
!===============================================================
end module mod_main
