module lib_io_system
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_terminal_width
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_system'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function get_terminal_width(n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_terminal_width'
  integer, intent(out) :: n

#ifdef USE_C
  character(32), pointer :: s
  integer(4), pointer :: cl
  integer(4), pointer :: cstat
  integer :: ios
#endif

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
#ifdef USE_C
  allocate(s)
  allocate(cl)
  allocate(cstat)

  call c_system_tput_cols(s, cl, cstat)

  if( cstat == 0 )then
    read(s(:cl),*,iostat=ios) n
    if( ios /= 0 )then
      n = 0
      info = 1
      call errret('Failed to get the terminal width.'//&
                 '\n  s : '//str(s)//&
                 '\n  cl: '//str(cl))
      return
    endif
  else
    n = 0
    info = 1
    call errret('Failed to get the terminal width.'//&
               '\n  cstat == 0')
    return
  endif

  deallocate(s)
  deallocate(cl)
  deallocate(cstat)
#elif TERMINAL_WIDTH <= 0
  n = 0
#elif TERMINAL_WIDTH <= 32
  n = 0
#elif TERMINAL_WIDTH <= 40
  n = 32
#elif TERMINAL_WIDTH <= 48
  n = 40
#elif TERMINAL_WIDTH <= 56
  n = 48
#else
  n = 128
#endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_terminal_width
!===============================================================
!
!===============================================================
end module lib_io_system
