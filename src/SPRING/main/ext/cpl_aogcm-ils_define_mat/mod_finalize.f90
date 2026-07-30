module mod_finalize
  use lib_const
  use lib_log
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: finalize
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_finalize'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine finalize()
  use c1_file, only: &
        close_report_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( close_report_file() )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine finalize
!===============================================================
!
!===============================================================
end module mod_finalize
