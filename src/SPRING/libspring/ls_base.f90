module ls_base
  use lib_const
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: spring_set_logopt

  public :: set_logopt
  public :: logopt

  public :: assert_initialized
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  character(8), parameter, public :: LOGOPT_DEFAULT = '-pr -cr'
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'ls_base'

  character(:), allocatable :: lopt
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine spring_set_logopt(opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_set_logopt'
  character(*), intent(in) :: opt

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( set_logopt(opt) /= 0 )then
    call errend(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine spring_set_logopt
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer(4) function set_logopt(opt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_logopt'
  character(*), intent(in) :: opt

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lopt = trim(opt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_logopt
!===============================================================
!
!===============================================================
function logopt() result(res)
  implicit none
  character(:), allocatable :: res

  allocate(character(1) :: res)
  if( allocated(lopt) )then
    res = trim(lopt)
  else
    res = trim(LOGOPT_DEFAULT)
  endif
end function logopt
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer(4) function assert_initialized(now, expected) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'assert_initialized'
  logical, intent(in) :: now
  logical, intent(in) :: expected

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Case: $now is True and $expected is False
  if( now .and. .not. expected )then
    info = 1
    call errret('This module has already been initialized.')
    return
  !-------------------------------------------------------------
  ! Case: $now is False and $expected is True
  elseif( .not. now .and. expected )then
    info = 1
    call errret('This module has not yet been initialized or '//&
                'already been finalized.')
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function assert_initialized
!===============================================================
!
!===============================================================
end module ls_base
