module lib_io_arg_base
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: argnum
  public :: argument
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_arg_base'
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function argnum()
  implicit none

#ifdef OPT_NO_F23
  argnum = iargc()
#else
  argnum = command_argument_count()
#endif
end function argnum
!===============================================================
!
!===============================================================
function argument(i) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'argument'
  integer, intent(in) :: i
  character(:), allocatable :: res

  character(CLEN_LINE) :: s

  if( i > argnum() )then
    call errend('The specified index of argument exceeds the number of arguments.', &
      '', PRCNAM, MODNAM)
  endif
#ifdef OPT_NO_F23
  call getarg(i, s)
#else
  call get_command_argument(i, s)
#endif

  allocate(character(1) :: res)
  res = trim(s)
end function argument
!===============================================================
!
!===============================================================
end module lib_io_arg_base
