module lib_io_binary_common
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: open_input_file_stream
  public :: open_output_file_stream
  public :: close_file
  public :: action_for_replace
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_binary_common'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function open_input_file_stream(&
    un, path, endian) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'open_input_file_stream'
  integer     , intent(in)  :: un
  character(*), intent(in)  :: path
  character(*), intent(in)  :: endian

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( endian )
  case( ENDIAN_BIG, ENDIAN_BIG_SHORT, &
        ENDIAN_LITTLE, ENDIAN_LITTLE_SHORT )
    open(un, file=path, &
         form='unformatted', access='stream', &
         action='read', status='old', convert=endian, &
         iostat=info)
  case( ENDIAN_UNDEF )
    open(un, file=path, &
         form='unformatted', access='stream', &
         action='read', status='old', &
         iostat=info)
  case default
    info = 1
    call errret(msg_invalid_value('endian', endian))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function open_input_file_stream
!===============================================================
!
!===============================================================
integer(4) function open_output_file_stream(&
    un, path, endian, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'open_output_file_stream'
  integer     , intent(in)  :: un
  character(*), intent(in)  :: path
  character(*), intent(in)  :: endian
  logical     , intent(in)  :: replace

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  open(un, file=path, &
       form='unformatted', access='stream', &
       action=action_for_replace(replace), &
       status='unknown', convert=endian, &
       iostat=info)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function open_output_file_stream
!===============================================================
!
!===============================================================
integer(4) function close_file(un) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'close_file'
  integer, intent(in)  :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  close(un, iostat=info)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function close_file
!===============================================================
!
!===============================================================
character(CLEN_KEY) function action_for_replace(replace) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'action_for_replace'
  logical, intent(in) :: replace

  if( replace )then
    res = ACTION_WRITE
  else
    res = ACTION_READWRITE
  endif
end function action_for_replace
!===============================================================
!
!===============================================================
end module lib_io_binary_common
