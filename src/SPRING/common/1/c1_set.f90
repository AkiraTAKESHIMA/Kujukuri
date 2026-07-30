module c1_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  use c1_const
  use c1_type_gs
  use c1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: open_setting_file
  public :: close_setting_file
  public :: line_number
  public :: key

  public :: read_path_report
  public :: get_path_report

  public :: find_block
  public :: back_to_block_head

  public :: read_input
  public :: read_value

  public :: keynum
  public :: alloc_keynum
  public :: free_keynum
  public :: set_keynum
  public :: reset_keynum
  public :: update_keynum
  public :: check_keynum

  public :: check_num_of_key

  public :: set_barlen
  public :: init_barlen
  public :: bar

  public :: raise_error_invalid_key
  public :: raise_warning_invalid_key
  public :: msg_invalid_input
  public :: msg_undesirable_input
  !-------------------------------------------------------------
  ! Interface
  !-------------------------------------------------------------
  interface read_value
    module procedure read_value__char
    module procedure read_value__log
    module procedure read_value__int4
    module procedure read_value__int8
    module procedure read_value__real
    module procedure read_value__dble
    module procedure read_value__fbin
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_set'

  character(CLEN_VAR), parameter :: BLOCK_END = 'end'
  character(CLEN_VAR), parameter :: KEY_PATH_REPORT = 'path_report'

  character(1), parameter :: DELIM_INPUT    = ':'
  character(1), parameter :: DELIM_CONTENTS = ','
  character(1), parameter :: DELIM_POSVAL   = '='

  integer, parameter :: BARLEN_DEFAULT = 64
  integer :: barlen = BARLEN_DEFAULT
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  type input_val_nokey_
    character(CLEN_LINE) :: val
  end type

  type input_val_key_
    character(CLEN_VAR)  :: key
    character(CLEN_LINE) :: val
  end type

  type input_
    character(CLEN_VAR) :: key
    integer :: nval, nval_nokey, nval_key
    type(input_val_nokey_), pointer :: val_nokey(:) !(nval)
    type(input_val_key_)  , pointer :: val_key(:)   !(nval)
  end type

  type keydict_
    character(CLEN_VAR) :: key
    integer :: n_llim, n_ulim
    integer :: n
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer                 :: un
  character(CLEN_PATH)    :: path_report
  integer                 :: iLine
  integer                 :: iLine_block_head
  character(CLEN_VAR)     :: block_name      = ''
  character(CLEN_VAR)     :: block_name_prev = ''
  type(input_)            :: input
  integer                 :: nkey
  type(keydict_), pointer :: keydict(:) !(nkey)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine open_setting_file()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'open_setting_file'

  integer :: cl
  character(:), allocatable :: path

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( argnum() /= 1 )then
    call errend(msg_syntax_error()//&
              '\nUsage: <program> <setting_file>')
    stop
  endif

  allocate(character(1) :: path)
  path = trim(argument(1))

  cl = len_trim(path)
  if( cl == 0 )then
    call errend(msg_syntax_error()//&
              '\nUsage: <program> <setting_file>')
    stop
  endif

  if( path(cl:cl) == '/' )then
    call errend(msg_unexpected_condition()//&
              '\nDirectory "'//str(path)//'" was specified for setting file.')
    stop
  endif

  un = unit_number()
  call logmsg('Open '//str(un)//' '//str(path))
  open(un, file=path, action='read', status='old')

  iLine = 0

  deallocate(path)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine open_setting_file
!===============================================================
!
!===============================================================
subroutine close_setting_file()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'close_setting_file'

  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  close(un, iostat=ios)
  if( ios /= 0 )then
    call errend('Failed to close a file. Unit number: '//str(un))
    stop
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine close_setting_file
!===============================================================
!
!===============================================================
integer function line_number()
  implicit none

  line_number = iLine
end function line_number
!===============================================================
!
!===============================================================
character(CLEN_VAR) function key()
  implicit none

  key = input%key
end function key
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
subroutine read_path_report()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_path_report'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_input()

  if( input%key /= KEY_PATH_REPORT )then
    call errend(msg_io_error(i=iLine)//&
              '\nInvalid input. '//&
                'Path of the report file was not given.')
    stop
  endif

  call read_value(path_report, is_path=.true.)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_path_report
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_path_report() result(res)
  implicit none

  res = path_report
end function get_path_report
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
subroutine find_block(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'find_block'
  character(*), intent(out) :: res

  character(clen_line) :: line
  integer :: cl
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  block_name_prev = block_name

  do
    read(un,"(a)",iostat=ios) line

    selectcase( ios )
    case( 0 )
      call add(iLine)

      if( line == '' ) cycle

      line = adjustl(line)
      cl = len_trim(line)

      if( line(1:1) == '[' .and. line(cl:cl) == ']' )then
        block_name = adjustl(line(2:cl-1))
        exit
      endif
    case( -1 )
      block_name = ''
      exit
    case default
      call errend(msg_io_error(i=iLine)//&
                '\nAn unknown reading error.')
      stop
    endselect
  enddo

  res = block_name
  iLine_block_head = iLine
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine find_block
!===============================================================
!
!===============================================================
subroutine back_to_block_head()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'back_to_block_end'
  integer :: il
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  do il = 1, iLine-iLine_block_head
    backspace(un,iostat=ios)
    if( ios /= 0 )then
      call errend(msg_io_error()//&
                '\n  iLine: '//str(iLine)//&
                '\n  iLine_block_head: '//str(iLine_block_head))
      stop
    endif
    call add(iLine, -1)
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine back_to_block_head
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
subroutine read_input()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_input'

  character(CLEN_LINE) :: line
  character(CLEN_LINE) :: line_concat
  character(CLEN_LINE) :: contents
  character(CLEN_LINE) :: line_
  integer :: cl
  integer :: ival
  integer :: ic0_val, ic1_val, ic
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call clear_input()

  do
    call add(iLine)
    read(un,"(a)",iostat=ios) line

    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      call errend(msg_io_error(i=iLine)//&
                '\nUnexpectedly reached the end of file.')
      stop
    case default
      call errend(msg_io_error(i=iLine)//&
                '\nAn unknown reading error.')
      stop
    endselect
    !-----------------------------------------------------------
    if( remove_comment(line, sgn='#') /= 0 )then
      call errend(msg_io_error(i=iLine))
      stop
    endif

    if( line == '' ) cycle

    line = adjustl(line)
    line_concat = line
    !-----------------------------------------------------------
    ! Successfully reached the end of the block
    if( is_block_tail(line) )then
      call logret(PRCNAM, MODNAM)
      return
    endif
    !-----------------------------------------------------------
    ! A comma at the end of line means this line continues
    ! to the next line. They are concatenated here.
    cl = len_trim(line_concat)
    do while( line_concat(cl:cl) == ',' )
      call add(iLine)
      read(un,"(a)",iostat=ios) line

      selectcase( ios )
      case( 0 )
        continue
      case( -1 )
        call errend(msg_io_error(i=iLine)//&
                  '\nUnexpectedly reached the end of file.')
        stop
      case default
        call errend(msg_io_error(i=iLine)//&
                  '\nAn unknown reading error.')
        stop
      endselect
      !---------------------------------------------------------
      ! ERROR: Unexpectedly reached the end of the block
      if( is_block_tail(line) )then
        call errend(msg_io_error(i=iLine)//&
                  '\nNo line follows the comma.')
        stop
      endif
      !---------------------------------------------------------
      line_concat = trim(line_concat)//adjustl(line)
      cl = len_trim(line_concat)
    enddo

    exit
  enddo

  line = line_concat
  !-------------------------------------------------------------
  ! Separate the line into a key and contents.
  !-------------------------------------------------------------
  line_ = line
  if( split(line_, trim(DELIM_INPUT), 1, QUOTE_BOTH) /= 0 )then
    call errend(msg_io_error(i=iLine))
    stop
  endif
  input%key = lower(line_)

  line_ = line
  if( split(line_, trim(DELIM_INPUT), 2, QUOTE_BOTH) /= 0 )then
    call errend(msg_io_error(i=iLine))
    stop
  endif
  contents = line_
  if( contents == '' )then
    call errret(msg_io_error(i=iLine)//&
              '\nNo value is given for the keyword "'//str(input%key)//'".')
    stop
  endif

  if( count_word(contents, DELIM_CONTENTS, input%nval, quoteIgnored=QUOTE_BOTH) /= 0 )then
    call errend(msg_io_error(i=iLine))
    stop
  endif
  input%nval = input%nval + 1
  allocate(input%val_nokey(input%nval))
  allocate(input%val_key(input%nval))
  input%nval_nokey = 0
  input%nval_key = 0

  ic0_val = 1
  do ival = 1, input%nval
    if( ival < input%nval )then
      ! Find the next comma @ ic1_val+1
      if( search_word(contents(ic0_val:), DELIM_CONTENTS, ic, quoteIgnored=QUOTE_BOTH) /= 0 )then
        call errend(msg_io_error(i=iLine))
        stop
      endif
      ic1_val = ic0_val + ic - 2
    else
      ic1_val = len_trim(contents)
    endif

    if( search_word(contents(ic0_val:ic1_val), DELIM_POSVAL, ic, quoteIgnored=QUOTE_BOTH) /= 0 )then
      call errend(msg_io_error(i=iLine))
      stop
    endif
    !-----------------------------------------------------------
    ! Case: A value without a keyword
    if( ic == 0 )then
      !---------------------------------------------------------
      ! ERROR: Value without keywords after values with keywords
      if( input%nval_key > 0 )then
        call errend(msg_io_error(i=iLine)//&
                  '\nValues without keywords cannot be put '//&
                    'after those with keywords.')
        stop
      endif

      call add(input%nval_nokey)
      input%val_nokey(input%nval_nokey)%val = adjustl(contents(ic0_val:ic1_val))
    !-----------------------------------------------------------
    ! Case: A value with a keyword
    else
      call add(input%nval_key)
      input%val_key(input%nval_key)%key = adjustl(contents(ic0_val:ic0_val+ic-2))
      input%val_key(input%nval_key)%val = adjustl(contents(ic0_val+ic:ic1_val))
    !-----------------------------------------------------------
    endif

    ic0_val = ic1_val + 2
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_input
!===============================================================
!
!===============================================================
subroutine clear_input()
  implicit none

  input%key = ''
  if( input%nval > 0 )then
    deallocate(input%val_nokey)
    deallocate(input%val_key)
  endif
  input%nval = 0
end subroutine clear_input
!===============================================================
!
!===============================================================
logical function is_block_tail(line) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_block_tail'
  character(*), intent(in)  :: line

  character(len_trim(line)) :: line_
  character(len_trim(line)) :: block_name
  integer :: cl

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  line_ = adjustl(line)

  if( remove_comment(line_) /= 0 )then
    call errend(msg_io_error(i=iLine))
    stop
  endif

  cl = len_trim(line_)

  if( line_(1:1) == '[' .and. line_(cl:cl) == ']' )then
    block_name = lower(adjustl(line_(2:cl-1)))
    if( block_name == BLOCK_END )then
      res = .true.
    else
      call errend(msg_io_error(i=iLine)//&
                '\nBlock "'//str(block_name)//'" appeared before '//&
                  'the current block "'//str(block_name_prev)//'" was closed.')
      stop
    endif
  else
    res = .false.
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function is_block_tail
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
subroutine read_value__char(&
    val, pos, key, is_keyword, is_path, dir, found)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value__char'
  character(*), intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(in) , optional :: is_keyword
  logical     , intent(in) , optional :: is_path
  character(*), intent(in) , optional :: dir
  logical     , intent(out), optional :: found

  integer              :: pos_
  character(CLEN_VAR)  :: key_
  logical              :: is_keyword_
  logical              :: is_path_
  character(CLEN_PATH) :: dir_
  logical              :: found_

  integer :: ival
  integer :: cl

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  is_keyword_ = .false.
  is_path_ = .false.
  dir_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  if( present(is_keyword) ) is_keyword_ = is_keyword
  if( present(is_path) ) is_path_ = is_path
  if( present(dir) ) dir_ = dir

  if( is_keyword_ .and. is_path_ )then
    call errend(msg_unexpected_condition()//&
              '\n  is_keyword_ .and. is_path_')
    stop
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    val = input%val_nokey(pos_)%val
  else
    if( key_ == '' )then
      call errend(msg_unexpected_condition()//&
                '\n  pos_ > input%nval_key .and. key_ == ""'//&
                '\n  iLine         : '//str(iLine)//&
                '\n  pos_          : '//str(pos_)//&
                '\n  input%nval_key: '//str(input%nval_key))
      stop
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        val = input%val_key(ival)%val
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call logret(PRCNAM, MODNAM)
        return
      else
        call errend(msg_io_error(i=iLine)//&
                  '\nFailed to get value due to invalid input.'//&
                  '\n  Default position: '//str(pos_)//&
                  '\n  Keyword         : '//str(key_))
        stop
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  ! Modify val
  !-------------------------------------------------------------
  cl = len_trim(val)
  !-------------------------------------------------------------
  ! Case: Path
  if( is_path_ )then
    if( val == '' )then
      call errend(msg_io_error(i=iLine)//&
                '\nPath is empty.')
      stop
    elseif( .not. ( &
              (val(1:1) == "'" .and. val(cl:cl) == "'") .or. &
              (val(1:1) == '"' .and. val(cl:cl) == '"') ) )then
      call errend(msg_io_error(i=iLine)//&
                '\nPaths must be quoted.')
      stop
    endif
    val = val(2:cl-1)
    val = joined(dir_, val)
  !-------------------------------------------------------------
  ! Case: Keyword
  elseif( is_keyword_ )then
    val = lower(val)
    if( remove_quotes(val) /= 0 )then
      call errend(msg_io_error(i=iLine))
      stop
    endif
  !-------------------------------------------------------------
  ! Case: Neither keyword or path
  else
    if( remove_quotes(val) /= 0 )then
      call errend(msg_io_error(i=iLine))
      stop
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value__char
!===============================================================
!
!===============================================================
subroutine read_value__log(val, pos, key, found)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value__log'
  logical     , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_cval(input%val_nokey(pos_)%val, pos_, key_, l=val)
  else
    if( key_ == '' )then
      call errend(msg_unexpected_condition()//&
                '\n  pos_ > input%nval_key .and. key_ == ""'//&
                '\n  iLine         : '//str(iLine)//&
                '\n  pos_          : '//str(pos_)//&
                '\n  input%nval_key: '//str(input%nval_key))
      stop
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_cval(input%val_key(ival)%val, pos_, key_, l=val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call logret(PRCNAM, MODNAM)
        return
      else
        call errend(msg_invalid_input(iLine)//&
                  '\nFailed to get value.'//&
                  '\n  Default position: '//str(pos_)//&
                  '\n  Keyword         : '//str(key_))
        stop
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value__log
!===============================================================
!
!===============================================================
subroutine read_value__int4(val, pos, key, found)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value__int4'
  integer(4)  , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_cval(input%val_nokey(pos_)%val, pos_, key_, i4=val)
  else
    if( key_ == '' )then
      call errend(msg_unexpected_condition()//&
                '\n  pos_ > input%nval_key .and. key_ == ""'//&
                '\n  iLine         : '//str(iLine)//&
                '\n  pos_          : '//str(pos_)//&
                '\n  input%nval_key: '//str(input%nval_key))
      stop
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_cval(input%val_key(ival)%val, pos_, key_, i4=val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call logret(PRCNAM, MODNAM)
        return
      else
        call errend(msg_invalid_input(iLine)//&
                  '\nFailed to get value.'//&
                  '\n  Default position: '//str(pos_)//&
                  '\n  Keyword         : '//str(key_))
        stop
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value__int4
!===============================================================
!
!===============================================================
subroutine read_value__int8(val, pos, key, found)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value__int8'
  integer(8)  , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_cval(input%val_nokey(pos_)%val, pos_, key_, i8=val)
  else
    if( key_ == '' )then
      call errend(msg_unexpected_condition()//&
                '\n  pos_ > input%nval_key .and. key_ == ""'//&
                '\n  iLine         : '//str(iLine)//&
                '\n  pos_          : '//str(pos_)//&
                '\n  input%nval_key: '//str(input%nval_key))
      stop
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_cval(input%val_key(ival)%val, pos_, key_, i8=val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call logret(PRCNAM, MODNAM)
        return
      else
        call errend(msg_invalid_input(iLine)//&
                  '\nFailed to get value.'//&
                  '\n  Default position: '//str(pos_)//&
                  '\n  Keyword         : '//str(key_))
        stop
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value__int8
!===============================================================
!
!===============================================================
subroutine read_value__real(val, pos, key, found)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value__real'
  real(4)     , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_cval(input%val_nokey(pos_)%val, pos_, key_, r4=val)
  else
    if( key_ == '' )then
      call errend(msg_unexpected_condition()//&
                '\n  pos_ > input%nval_key .and. key_ == ""'//&
                '\n  iLine         : '//str(iLine)//&
                '\n  pos_          : '//str(pos_)//&
                '\n  input%nval_key: '//str(input%nval_key))
      stop
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_cval(input%val_key(ival)%val, pos_, key_, r4=val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call logret(PRCNAM, MODNAM)
        return
      else
        call errend(msg_invalid_input(iLine)//&
                  '\nFailed to get value.'//&
                  '\n  Default position: '//str(pos_)//&
                  '\n  Keyword         : '//str(key_))
        stop
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value__real
!===============================================================
!
!===============================================================
subroutine read_value__dble(val, pos, key, found)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value__dble'
  real(8)     , intent(inout) :: val
  integer     , intent(in) , optional :: pos
  character(*), intent(in) , optional :: key
  logical     , intent(out), optional :: found

  integer             :: pos_
  character(CLEN_VAR) :: key_
  logical             :: found_

  integer :: ival

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pos_ = 1
  key_ = ''
  if( present(pos) ) pos_ = pos
  if( present(key) ) key_ = key
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nval_nokey >= pos_ )then
    found_ = .true.
    call read_cval(input%val_nokey(pos_)%val, pos_, key_, r8=val)
  else
    if( key_ == '' )then
      call errend(msg_unexpected_condition()//&
                '\n  pos_ > input%nval_key .and. key_ == ""'//&
                '\n  iLine         : '//str(iLine)//&
                '\n  pos_          : '//str(pos_)//&
                '\n  input%nval_key: '//str(input%nval_key))
      stop
    endif

    found_ = .false.
    do ival = 1, input%nval_key
      if( key_ == input%val_key(ival)%key )then
        found_ = .true.
        call read_cval(input%val_key(ival)%val, pos_, key_, r8=val)
        exit
      endif
    enddo  ! ival/

    if( .not. found_ )then
      if( present(found) )then
        found = found_
        call logret(PRCNAM, MODNAM)
        return
      else
        call errend(msg_invalid_input(iLine)//&
                  '\nFailed to get value.'//&
                  '\n  Default position: '//str(pos_)//&
                  '\n  Keyword         : '//str(key_))
        stop
      endif
    endif
  endif

  if( present(found) ) found = found_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value__dble
!===============================================================
!
!===============================================================
subroutine read_value__fbin(val, dir, getlen)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value__fbin'
  type(file_) , intent(inout) :: val
  character(*), intent(in), optional :: dir
  logical     , intent(in), optional :: getlen

  character(CLEN_PATH) :: dir_
  logical :: getlen_

  logical :: found

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dir_ = ''
  getlen_ = .false.
  if( present(dir) ) dir_ = dir
  if( present(getlen) ) getlen_ = getlen
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_value__char(val%path, is_path=.true., pos=1, key='path', found=found)
  if( .not. found )then
    call errend(msg_io_error(i=iLine)//&
              '\nPath was not given.')
    stop
  endif
  val%path = joined(dir_, val%path)

  call read_value__char(val%dtype, pos=2, key='dtype', found=found, is_keyword=.true.)
  call read_value__int4(val%rec, pos=3, key='rec', found=found)
  call read_value__char(val%endian, pos=4, key='endian', found=found)
  call read_value__int8(val%length, pos=5, key='length', found=found)
  if( .not. getlen_ .and. found )then
    call errend(msg_invalid_input(iLine)//&
              '\nLength of data cannot be given for this argument.')
    stop
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value__fbin
!===============================================================
!
!===============================================================
subroutine read_cval(cval, pos, key, l, i4, i8, r4, r8)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_cval'
  character(*), intent(in) :: cval
  integer     , intent(in) :: pos
  character(*), intent(in) :: key  
  logical     , intent(out), optional :: l
  integer(4)  , intent(out), optional :: i4
  integer(8)  , intent(out), optional :: i8
  real(4)     , intent(out), optional :: r4
  real(8)     , intent(out), optional :: r8

  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: logical
  if( present(l) )then
    selectcase( lower(cval) )
    case( '.true.', 'true', 't' )
      l = .true.
    case( '.false.', 'false', 'f' )
      l = .false.
    case default
      call errend(msg_invalid_input(iLine)//&
                  msg_main()//&
                '\nBoolean value must be given by'//&
                  '".true.", "true" or "t" for true and '//&
                  '".false.", "false" or "f" for false '//&
                  '(not case-sensitive).'//&
                  msg_info())
      stop
    endselect
  !-------------------------------------------------------------
  ! Case: int4
  elseif( present(i4) )then
    read(cval,*,iostat=ios) i4
    if( ios /= 0 )then
      call errend(msg_invalid_input(iLine)//&
                  msg_main()//&
                  msg_info())
      stop
    endif
  !-------------------------------------------------------------
  ! Case: int8
  elseif( present(i8) )then
    read(cval,*,iostat=ios) i8
    if( ios /= 0 )then
      call errend(msg_invalid_input(iLine)//&
                  msg_main()//&
                  msg_info())
      stop
    endif
  !-------------------------------------------------------------
  ! Case: real
  elseif( present(r4) )then
    read(cval,*,iostat=ios) r4
    if( ios /= 0 )then
      call errend(msg_invalid_input(iLine)//&
                  msg_main()//&
                  msg_info())
      stop
    endif
  !-------------------------------------------------------------
  ! Case: dble
  elseif( present(r8) )then
    read(cval,*,iostat=ios) r8
    if( ios /= 0 )then
      call errend(msg_invalid_input(iLine)//&
                  msg_main()//&
                  msg_info())
      stop
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call errend(msg_unexpected_condition()//&
              '\nNot matched any case.')
    stop
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
function msg_main() result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__msg_main'
  character(:), allocatable :: res

  character(32) :: dtype

  if( present(l) )then
    dtype = 'a logical'
  elseif( present(i4) )then
    dtype = 'a 4-byte integer'
  elseif( present(i8) )then
    dtype = 'an 8-byte integer'
  elseif( present(r4) )then
    dtype = 'a 4-byte float'
  elseif( present(r8) )then
    dtype = 'an 8-byte float'
  else
    call errend(msg_unexpected_condition()//&
              '\nNot matched any case.', &
                PRCNAM, MODNAM)
    stop
  endif

  allocate(character(1) :: res)
  res = '\nFailed to read the input value as '//str(dtype)//'.'
end function msg_main
!---------------------------------------------------------------
function msg_info() result(res)
  implicit none
  character(:), allocatable :: res

  res = '\n  Default position: '//str(pos)//&
        '\n  Keyword         : '//str(key)//&
        '\n  Value           : '//str(cval)
end function msg_info
!---------------------------------------------------------------
end subroutine read_cval
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
subroutine alloc_keynum()
  implicit none

  type(keydict_), pointer :: kd
  integer :: ikey
  integer, parameter :: NKEY_INIT = 16

  allocate(keydict(NKEY_INIT))
  nkey = NKEY_INIT

  do ikey = 1, nkey
    kd => keydict(ikey)
    kd%key = ''
    kd%n = 0
    kd%n_llim = 0
    kd%n_ulim = 0
  enddo
end subroutine alloc_keynum
!===============================================================
!
!===============================================================
subroutine free_keynum()
  implicit none

  deallocate(keydict)
  nullify(keydict)
  nkey = 0
end subroutine free_keynum
!===============================================================
!
!===============================================================
subroutine set_keynum(key, llim, ulim)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_keynum'
  character(*), intent(in) :: key
  integer     , intent(in) :: llim, ulim

  type(keydict_), pointer :: kd
  type(keydict_), allocatable :: keydict_copy(:)
  integer :: ikey

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ikey = 1, nkey
    kd => keydict(ikey)
    if( kd%key == trim(key) )then
      call errend(msg_unexpected_condition()//&
                '\n  keydict(ikey)%key == trim(key)'//&
                '\n  block_name: '//str(block_name)//&
                '\n  key: '//str(key))
      stop
    endif

    if( kd%key == '' ) exit
  enddo

  if( ikey == nkey+1 )then
    allocate(keydict_copy(nkey))
    do ikey = 1, nkey
      keydict_copy(ikey) = keydict(ikey)
    enddo
    deallocate(keydict)
    allocate(keydict(nkey*2))
    do ikey = 1, nkey
      keydict(ikey) = keydict_copy(ikey)
    enddo
    deallocate(keydict_copy)

    do ikey = nkey+1, size(keydict)
      kd => keydict(ikey)
      kd%key = ''
      kd%n = 0
      kd%n_llim = 0
      kd%n_ulim = 0
    enddo
    kd => keydict(nkey+1)
    nkey = size(keydict)
  endif

  kd%key = trim(key)
  kd%n_llim = llim
  kd%n_ulim = ulim
  nullify(kd)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_keynum
!===============================================================
!
!===============================================================
subroutine reset_keynum()
  implicit none

  integer :: ikey

  do ikey = 1, nkey
    keydict(ikey)%n = 0
  enddo
end subroutine reset_keynum
!===============================================================
!
!===============================================================
subroutine update_keynum()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_keynum'

  type(keydict_), pointer :: kd
  integer :: ikey

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Update the counter of keywords
  !-------------------------------------------------------------
  if( input%key == '' )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  nullify(kd)
  do ikey = 1, nkey
    if( keydict(ikey)%key /= input%key ) cycle
    kd => keydict(ikey)
  enddo

  if( .not. associated(kd) )then
    call errend(msg_io_error(i=iLine)//&
              '\nKey "'//str(input%key)//'" is invalid.')
    stop
  endif

  call add(kd%n)
  nullify(kd)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_keynum
!===============================================================
!
!===============================================================
subroutine check_keynum()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_keynum'

  type(keydict_), pointer :: kd
  integer :: ikey

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ikey = 1, nkey
    kd => keydict(ikey)
    if( kd%key == '' ) exit

    if( kd%n_llim >= 0 )then
      if( kd%n < kd%n_llim )then
        call errend('Invalid input in block "'//str(block_name)//&
                    '" which starts from line '//str(iLine_block_head)//'.'//&
                  '\nThe number of arguments given by the keyword "'//str(kd%key)//&
                    '" is below the lower limit of '//str(kd%n_llim)//'.')
        stop
      endif
    endif
    if( kd%n_ulim >= 0 )then
      if( kd%n > kd%n_ulim )then
        call errend('Invalid input in block "'//str(block_name)//&
                    '" which starts from line '//str(iLine_block_head)//'.'//&
                  '\nThe number of arguments given by the keyword "'//str(kd%key)//&
                    '" is above the upper limit of '//str(kd%n_ulim)//'.')
        stop
      endif
    endif
  enddo

  nullify(kd)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum
!===============================================================
!
!===============================================================
integer function keynum(key) result(n)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'keynum'
  character(*), intent(in) :: key

  integer :: ikey

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  n = 0
  do ikey = 1, nkey
    if( keydict(ikey)%key == key )then
      n = keydict(ikey)%n
      call logret(PRCNAM, MODNAM)
      return
    endif
  enddo

  call errend(msg_unexpected_condition()//&
            '\nKeyword "'//str(key)//'" is not found.')
  stop
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function keynum
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
subroutine check_num_of_key(n, key, nmin, nmax)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_num_of_key'
  integer     , intent(in) :: n
  character(*), intent(in) :: key
  integer     , intent(in) :: nmin, nmax

  character(32) :: c_nmin, c_nmax

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( (nmin > 0 .and. n < nmin) .or. &
      (nmax > 0 .and. n > nmax) )then
    if( nmin > 0 )then
      c_nmin = str(nmin)
    else
      c_nmin = '(unlimited)'
    endif

    if( nmax > 0 )then
      c_nmax = str(nmax)
    else
      c_nmax = '(unlimited)'
    endif

    call errend(msg_invalid_input()//&
              '\nThe number of times the key was specified is out of range.'//&
              '\n  key          : "'//str(key)//'"'//&
              '\n  num. of times: '//str(n)//&
              '\n  min          : '//str(c_nmin)//&
              '\n  max          : '//str(c_nmax))
    stop
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_num_of_key
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
subroutine set_barlen(cl)
  implicit none
  integer, intent(in) :: cl

  barlen = cl
end subroutine set_barlen
!===============================================================
!
!===============================================================
subroutine init_barlen()
  implicit none

  barlen = BARLEN_DEFAULT
end subroutine init_barlen
!===============================================================
!
!===============================================================
character(CLEN_VAR) function bar(s)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'bar'
  character(*), intent(in) :: s

  integer :: cl_left, cl_right

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Case: Tail
  if( s == '' )then
    bar = str('', barlen, '-')
  !-------------------------------------------------------------
  ! Case: Head
  else
    if( barlen > len_trim(s)+6 )then
      if( mod(barlen-len_trim(s),2) == 0 )then
        cl_left = (barlen - len_trim(s) - 2) / 2
      else
        cl_left = (barlen - len_trim(s) - 1) / 2
      endif
      cl_right = barlen - len_trim(s) - 2 - cl_left
      bar = str('', cl_left, '-')//' '//str(s)//' '//str('', cl_right, '-')
    else
      bar = '-- '//trim(s)//' --'
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function bar
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
subroutine raise_error_invalid_key()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'raise_error_invalid_key'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  call errend(msg_invalid_input(iLine)//&
            '\nKeyword "'//str(input%key)//&
              '" is invalid for block "'//str(block_name)//'".')
  stop
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine raise_error_invalid_key
!===============================================================
!
!===============================================================
subroutine raise_warning_invalid_key()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'raise_warning_invalid_key'

  call logwrn(msg_invalid_input(iLine)//&
            '\nKeyword "'//str(input%key)//&
              '" is invalid for block "'//str(block_name)//'".')
end subroutine raise_warning_invalid_key
!===============================================================
!
!===============================================================
function msg_invalid_input(iLine) result(msg)
  implicit none
  integer, intent(in), optional :: iLine
  character(:), allocatable :: msg

  allocate(character(1) :: msg)
  if( present(iLine) )then
    msg = 'Invalid input @ line '//str(iLine)
  else
    msg = 'Invalid input.'
  endif
end function msg_invalid_input
!===============================================================
!
!===============================================================
function msg_undesirable_input(iLine) result(msg)
  implicit none
  integer, intent(in), optional :: iLine
  character(:), allocatable :: msg

  allocate(character(1) :: msg)
  if( present(iLine) )then
    msg = 'Undesirable input @ line '//str(iLine)
  else
    msg = 'Undesirable input.'
  endif
end function msg_undesirable_input
!===============================================================
!
!===============================================================
end module c1_set
