module mod_finalize
  use lib_const
  use lib_log
  use c1_type_gs
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
subroutine finalize(a)
  use c1_file, only: &
        close_report_file
  use c1_gs_base, only: &
        clear_mesh
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize'
  type(gs_), intent(inout) :: a

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( clear_mesh(a) )

  call traperr( close_report_file() )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine finalize
!===============================================================
!
!===============================================================
end module mod_finalize
