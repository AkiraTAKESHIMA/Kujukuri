module c1_file
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c1_const
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: open_report_file
  public :: close_report_file
  public :: is_report_file_opened
  public :: report

  public :: set_opt_old_files
  public :: handle_old_file
  !-------------------------------------------------------------
  interface handle_old_file
    module procedure handle_old_file__file
    module procedure handle_old_file__path
  end interface
  !-------------------------------------------------------------
  ! Private Variables
  !-------------------------------------------------------------
  integer, save :: un_report = 0
  character(clen_path), save :: path_report = ''

  character(clen_key), save :: old_files = ''

  character(CLEN_PROC), parameter :: MODNAM = 'c1_file'
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function open_report_file(path) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'open_report_file'
  character(*), intent(in) :: path

  integer :: ios

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  path_report = path
  un_report = unit_number()

  if( mkdir(dirname(path_report), output=.true., hut='+ ') /= 0 )then
    info = 1; call errret(); return
  endif

  call logmsg('Open[w] '//str(un_report)//' '//str(path))
  open(un_report, file=path, &
       form='formatted', action='write', status='replace', &
       iostat=ios)
  if( ios /= 0 )then
    info = 1
    call errret(msg_io_error()//&
              '\nFailed to open file.')
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function open_report_file
!===============================================================
!
!===============================================================
integer(4) function close_report_file() result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'close_report_file'

  integer :: ios

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Close '//str(un_report)//' '//str(path_report))
  close(un_report, iostat=ios)
  if( ios /= 0 )then
    info = 1
    call errret(msg_io_error()//&
              '\nFailed to open file.')
    return
  endif

  path_report = ''
  un_report   = 0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function close_report_file
!===============================================================
!
!===============================================================
logical function is_report_file_opened() result(res)
  implicit none

  inquire(unit=un_report, opened=res)
end function is_report_file_opened
!===============================================================
!
!===============================================================
subroutine report(s)
  implicit none
  character(*), intent(in) :: s

  call logmsg(s, un=un_report)
end subroutine report
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
integer(4) function set_opt_old_files(opt_old_files) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_opt_old_files'
  character(*), intent(in) :: opt_old_files

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  old_files = opt_old_files
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_opt_old_files
!===============================================================
!
!===============================================================
integer(4) function handle_old_file__file(f) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'handle_old_file__file'
  type(file_), intent(in) :: f

  integer :: access

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( old_files )
  !-------------------------------------------------------------
  ! Case: Stop
  case( OPT_OLD_FILES_STOP )
    if( access(f%path,' ') == 0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nFile already exists.'//&
                '\n  id  : '//str(f%id)//&
                '\n  path: '//str(f%path))
      return
    endif
  !-------------------------------------------------------------
  ! Case: Remove
  case( OPT_OLD_FILES_REMOVE )
    selectcase( f%action )
    case( ACTION_READ )
      info = 1
      call errret(msg_unexpected_condition()//&
               '\nold_files == '//str(old_files)//&
                  ' .and. f%action == '//str(f%action))
      return
    case( ACTION_WRITE, &
          ACTION_READWRITE, &
          ACTION_UNDEF )
      if( access(f%path,' ') == 0 )then
        call logmsg('Remove '//str(f%id)//': "'//str(f%path)//'"')
        if( remove(f%path, output=.false.) /= 0 )then
          info = 1; call errret(); return
        endif
      endif
    case default
      info = 1
      call errret(msg_invalid_value()//&
                '\n  id    : '//str(f%id)//&
                '\n  action: '//str(f%action))
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Overwrite (Check permission)
  case( OPT_OLD_FILES_OVERWRITE )
    if( access(f%path,' ') == 0 )then
      call logmsg('To be updated '//str(f%id)//': "'//str(f%path)//'"')
      if( check_permission(f%path, action_readwrite) /= 0 )then
        info = 1
        call errret(&
          'Option "'//str(OPT_OLD_FILES_OVERWRITE)//&
          '" was selected but no readwrite permission for'//&
          ' the file.')
        return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('old_files', old_files))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function handle_old_file__file
!===============================================================
!
!===============================================================
integer(4) function handle_old_file__path(path, id) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'handle_old_file__path'
  character(*), intent(in) :: path
  character(*), intent(in) :: id

  integer :: access

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( old_files )
  !-------------------------------------------------------------
  ! Case: Stop
  case( OPT_OLD_FILES_STOP )
    if( access(path,' ') == 0 )then
      info = 1
      call errret('File already exists.'//&
                '\n'//str(id)//': "'//str(path)//'"')
      return
    endif
  !-------------------------------------------------------------
  ! Case: Remove
  case( OPT_OLD_FILES_REMOVE )
    if( access(path,' ') == 0 )then
      call logmsg('Remove '//str(id)//': "'//str(path)//'"')
      if( remove(path, output=.false.) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Overwrite (Check permission)
  case( OPT_OLD_FILES_OVERWRITE )
    if( access(path,' ') == 0 )then
      call logmsg('To be updated '//str(id)//': "'//str(path)//'"')
      if( check_permission(path, ACTION_READWRITE) /= 0 )then
        info = 1
        call errret(&
          'Option "'//str(OPT_OLD_FILES_OVERWRITE)//&
          '" was selected but no readwrite permission for'//&
          ' the file.')
        return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('old_files', old_files))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function handle_old_file__path
!===============================================================
!
!===============================================================
end module c1_file
