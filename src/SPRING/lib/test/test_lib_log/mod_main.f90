module mod_main
  use lib_const
  use lib_base
  use lib_log
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: test1
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_main'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine test1()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'test1'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( test1_errret() /= 0 )then
    call errend(ERROR_CODE__CHILDPROCESS, '`test1_errret` returned an error', mod=MODNAM, prc=PRCNAM)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine test1
!===============================================================
!
!===============================================================
integer function test1_errret() result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'test1_errret'

  call logbgn(PRCNAM, MODNAM)
  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 1
  call errret(ERROR_CODE__BASE, 'test errret', MODNAM, PRCNAM)
  return
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function test1_errret
!===============================================================
!
!===============================================================
end module mod_main
