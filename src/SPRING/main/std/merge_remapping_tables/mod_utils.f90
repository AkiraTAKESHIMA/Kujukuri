module mod_utils
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: open_file_grid_im
  public :: close_file_grid_im

  public :: remove_im
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_utils'

  integer, save :: un_grid_im = 0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine open_file_grid_im(path, action, un)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'open_file_grid_im'
  character(*), intent(in)  :: path
  character(*), intent(in)  :: action
  integer     , intent(out) :: un

  character(clen_key) :: mode
  character(clen_key) :: status
  character(clen_key) :: position

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( action )
  case( action_read )
    mode = 'r'
    status = status_old
    position = position_rewind
  case( action_write )
    mode = 'w'
    status = status_replace
    position = position_rewind
  case( action_readwrite )
    mode = 'w+'
    status = status_old
    position = position_rewind
  case default
    call errend(msg_invalid_value('action', action))
  endselect

  if( un_grid_im /= 0 )then
    call errend(msg_unexpected_condition()//&
              '\n  un_grid_im /= 0'//&
              '\nFile is already opened.'//&
              '\npath: '//str(path))
  endif

  un = unit_number()
  call logmsg('Open['//str(mode)//'] '//str(un)//' '//str(path))
  open(un, file=path, form='unformatted', access='sequential', &
       action=action, status=status, position=position)

  un_grid_im = un
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine open_file_grid_im
!===============================================================
!
!===============================================================
subroutine close_file_grid_im()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'close_file_grid_im'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( un_grid_im == 0 )then
    call errend(msg_unexpected_condition()//&
              '\n  un_grid_im == 0'//&
              '\nFile is not opened.')
  endif

  call logmsg('Close '//str(un_grid_im))
  close(un_grid_im)

  un_grid_im = 0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine close_file_grid_im
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
subroutine remove_im(path, rm)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remove_im'
  character(*), intent(in) :: path
  logical     , intent(in) :: rm

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. rm )then
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( path /= '' )then
    call traperr( remove(path) )
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine remove_im
!===============================================================
!
!===============================================================
end module mod_utils
