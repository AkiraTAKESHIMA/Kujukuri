module c1_timer
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use c1_const
  use c1_type_timer
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_ctimer
  public :: clear_ctimer
  public :: start_ctimer
  public :: stop_ctimer
  public :: is_ctimer_active
  public :: point_ctimer
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_timer'

  type(ctimer_), target, save :: ct
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_ctimer(n)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_ctimer'
  integer, intent(in) :: n

  call logbgn(PRCNAM, MODNAM, '-p')

  call init_timer(ct%timer, n)

  call logret(PRCNAM, MODNAM)
end subroutine init_ctimer
!===============================================================
!
!===============================================================
subroutine clear_ctimer()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_ctimer'

  call logbgn(PRCNAM, MODNAM, '-p')

  call clear_timer(ct%timer)

  call logret(PRCNAM, MODNAM)
end subroutine clear_ctimer
!===============================================================
!
!===============================================================
subroutine start_ctimer(proc)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'start_ctimer'
  character(*), intent(in) :: proc

  call logbgn(PRCNAM, MODNAM, '-p')

  call start_timer(ct%timer, proc)

  call logret(PRCNAM, MODNAM)
end subroutine start_ctimer
!===============================================================
!
!===============================================================
subroutine stop_ctimer(proc)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'stop_ctimer'
  character(*), intent(in) :: proc

  call logbgn(PRCNAM, MODNAM, '-p')

  call stop_timer(ct%timer, proc)

  call logret(PRCNAM, MODNAM)
end subroutine stop_ctimer
!===============================================================
!
!===============================================================
logical function is_ctimer_active() result(res)
  implicit none

  res = ct%timer%is_active
end function is_ctimer_active
!===============================================================
!
!===============================================================
subroutine point_ctimer(ptr)
  implicit none
  type(ctimer_), pointer :: ptr

  ptr => ct
end subroutine point_ctimer
!===============================================================
!
!===============================================================
end module c1_timer
