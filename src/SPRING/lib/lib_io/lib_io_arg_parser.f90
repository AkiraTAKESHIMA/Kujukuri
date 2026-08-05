module lib_io_arg_parser
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io_arg_base
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: addarg

  public :: parsearg

  public :: arg_flag
  public :: arg_char
  public :: arg_int4
  public :: arg_dble

  public :: showarg
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface addarg
    module procedure addarg__positional__char
    module procedure addarg__positional__int4
    module procedure addarg__positional__dble
    module procedure addarg__optional__flag
    module procedure addarg__optional__char
    module procedure addarg__optional__int4
    module procedure addarg__optional__dble
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_arg_parser'

  integer, parameter :: ITYPE_FLAG = 1
  integer, parameter :: ITYPE_CHAR = 2
  integer, parameter :: ITYPE_INT1 = 3
  integer, parameter :: ITYPE_INT2 = 4
  integer, parameter :: ITYPE_INT4 = 5
  integer, parameter :: ITYPE_INT8 = 6
  integer, parameter :: ITYPE_REAL = 7
  integer, parameter :: ITYPE_DBLE = 8
  integer, parameter :: ITYPE_UNDEF = -9

  character(CLEN_KEY), parameter :: STYPE_FLAG = 'flag'
  character(CLEN_KEY), parameter :: STYPE_CHAR = 'string'
  character(CLEN_KEY), parameter :: STYPE_INT1 = '1 byte int'
  character(CLEN_KEY), parameter :: STYPE_INT2 = '2 byte int'
  character(CLEN_KEY), parameter :: STYPE_INT4 = '4 byte int'
  character(CLEN_KEY), parameter :: STYPE_INT8 = '8 byte int'
  character(CLEN_KEY), parameter :: STYPE_REAL = '4 byte float'
  character(CLEN_KEY), parameter :: STYPE_DBLE = '8 byte float'

  character(8), parameter :: KEY_HELP_SHORT = '-h'
  character(8), parameter :: KEY_HELP_LONG  = '--help'

  ! Definitions of arguments
  type arg_flag_
    logical :: is_positional
    character(:), pointer :: name
    character(:), pointer :: key_short
    character(:), pointer :: key_long
    logical, pointer :: is_required
    character(:), pointer :: description
    logical, pointer :: used
    logical :: val
  end type

  type arg_char_
    logical :: is_positional
    character(:), pointer :: name
    character(:), pointer :: key_short
    character(:), pointer :: key_long
    logical, pointer :: is_required
    character(:), pointer :: description
    logical, pointer :: used
    character(:), allocatable :: val
  end type

  type arg_int4_
    logical :: is_positional
    character(:), pointer :: name
    character(:), pointer :: key_short
    character(:), pointer :: key_long
    logical, pointer :: is_required
    character(:), pointer :: description
    logical, pointer :: used
    integer(4) :: val
  end type

  type arg_dble_
    logical :: is_positional
    character(:), pointer :: name
    character(:), pointer :: key_short
    character(:), pointer :: key_long
    logical, pointer :: is_required
    character(:), pointer :: description
    logical, pointer :: used
    real(8) :: val
  end type

  type arg_cmn_
    integer :: typ
    integer :: idx
    character(:), allocatable :: name
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    logical :: is_required
    character(:), allocatable :: description
    logical :: used
  end type

  type arg_
    integer :: nall = 0
    integer :: nflag = 0
    integer :: nchar = 0
    integer :: nint4 = 0
    integer :: ndble = 0
    integer :: n_pos = 0
    integer :: n_opt = 0
    type(arg_flag_), pointer :: lst_flag(:)
    type(arg_char_), pointer :: lst_char(:)
    type(arg_int4_), pointer :: lst_int4(:)
    type(arg_dble_), pointer :: lst_dble(:)
    type(arg_cmn_), pointer :: cmn_opt(:)
    type(arg_cmn_), pointer :: cmn_pos(:)
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  type(arg_) :: ad

  integer :: clenmax_key_short = 0
  integer :: clenmax_key_long  = 0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine addarg__positional__char(&
    name, v, description)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__positional__char'
  character(*), intent(in) :: name
  character(*), intent(in) :: v  ! for distinction
  character(*), intent(in) :: description

  type(arg_char_), pointer :: a
  type(arg_cmn_), pointer :: cmn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call inc_n_arg(.true., ITYPE_CHAR)

  cmn => ad%cmn_pos(ad%n_pos)
  cmn%typ = ITYPE_CHAR
  cmn%idx = ad%nchar
  cmn%name        = trim(name)
  cmn%key_short   = ''
  cmn%key_long    = ''
  cmn%is_required = .true.
  cmn%description = trim(description)

  a => ad%lst_char(ad%nchar)

  a%is_positional = .true.
  a%name        => cmn%name
  a%description => cmn%description
  a%used => cmn%used

  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__positional__char
!===============================================================
!
!===============================================================
subroutine addarg__positional__int4(&
    name, v, description)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__positional__int4'
  character(*), intent(in) :: name
  integer(4)  , intent(in) :: v  ! for distinction
  character(*), intent(in) :: description

  type(arg_int4_), pointer :: a
  type(arg_cmn_), pointer :: cmn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call inc_n_arg(.true., ITYPE_INT4)

  cmn => ad%cmn_pos(ad%n_pos)
  cmn%typ = ITYPE_INT4
  cmn%idx = ad%nint4
  cmn%name        = trim(name)
  cmn%key_short   = ''
  cmn%key_long    = ''
  cmn%is_required = .true.
  cmn%description = trim(description)

  a => ad%lst_int4(ad%nint4)

  a%is_positional = .true.
  a%name        => cmn%name
  a%description => cmn%description
  a%used => cmn%used

  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__positional__int4
!===============================================================
!
!===============================================================
subroutine addarg__positional__dble(&
    name, v, description)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__positional__dble'
  character(*), intent(in) :: name
  real(8)     , intent(in) :: v  ! for distinction
  character(*), intent(in) :: description

  type(arg_dble_), pointer :: a
  type(arg_cmn_), pointer :: cmn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call inc_n_arg(.true., ITYPE_DBLE)

  cmn => ad%cmn_pos(ad%n_pos)
  cmn%typ = ITYPE_DBLE
  cmn%idx = ad%ndble
  cmn%name        = trim(name)
  cmn%key_short   = ''
  cmn%key_long    = ''
  cmn%is_required = .true.
  cmn%description = trim(description)

  a => ad%lst_dble(ad%ndble)

  a%is_positional = .true.
  a%name        => cmn%name
  a%description => cmn%description
  a%used => cmn%used

  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__positional__dble
!===============================================================
!
!===============================================================
subroutine addarg__optional__flag(&
    key_short, key_long, val_default, is_required, description)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__optional__flag'
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: val_default
  logical     , intent(in) :: is_required
  character(*), intent(in) :: description

  type(arg_flag_), pointer :: a
  type(arg_cmn_), pointer :: cmn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_format_key_optional(key_short, key_long)

  call inc_n_arg(.false., ITYPE_FLAG)

  cmn => ad%cmn_opt(ad%n_opt)
  cmn%typ = ITYPE_FLAG
  cmn%idx = ad%nflag
  cmn%name        = upper(get_key_single(key_short, key_long, remove_hyphen=.true.))
  cmn%key_short   = trim(key_short)
  cmn%key_long    = trim(key_long)
  cmn%is_required = is_required
  cmn%description = trim(description)

  a => ad%lst_flag(ad%nflag)

  a%val = val_default

  a%is_positional = .false.
  a%name        => cmn%name
  a%key_short   => cmn%key_short
  a%key_long    => cmn%key_long
  a%is_required => cmn%is_required
  a%description => cmn%description
  a%used => cmn%used

  nullify(a)

  clenmax_key_short = max(len_trim(key_short), clenmax_key_short)
  clenmax_key_long  = max(len_trim(key_long ), clenmax_key_long )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__optional__flag
!===============================================================
!
!===============================================================
subroutine addarg__optional__char(&
    key_short, key_long, val_default, is_required, description)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__optional__char'
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  character(*), intent(in) :: val_default
  logical     , intent(in) :: is_required
  character(*), intent(in) :: description

  type(arg_char_), pointer :: a
  type(arg_cmn_), pointer :: cmn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_format_key_optional(key_short, key_long)

  call inc_n_arg(.false., ITYPE_CHAR)

  cmn => ad%cmn_opt(ad%n_opt)
  cmn%typ = ITYPE_CHAR
  cmn%idx = ad%nchar
  cmn%name        = upper(get_key_single(key_short, key_long, remove_hyphen=.true.))
  cmn%key_short   = trim(key_short)
  cmn%key_long    = trim(key_long)
  cmn%is_required = is_required
  cmn%description = trim(description)

  a => ad%lst_char(ad%nchar)

  ! a%val has already been allocated when initialized
  a%val = val_default

  a%is_positional = .false.
  a%name        => cmn%name
  a%key_short   => cmn%key_short
  a%key_long    => cmn%key_long
  a%is_required => cmn%is_required
  a%description => cmn%description
  a%used => cmn%used

  nullify(a)

  clenmax_key_short = max(len_trim(key_short), clenmax_key_short)
  clenmax_key_long  = max(len_trim(key_long ), clenmax_key_long )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__optional__char
!===============================================================
!
!===============================================================
subroutine addarg__optional__int4(&
    key_short, key_long, val_default, is_required, description)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__optional__int4'
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  integer(4)  , intent(in) :: val_default
  logical     , intent(in) :: is_required
  character(*), intent(in) :: description

  type(arg_int4_), pointer :: a
  type(arg_cmn_), pointer :: cmn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_format_key_optional(key_short, key_long)

  call inc_n_arg(.false., ITYPE_INT4)

  cmn => ad%cmn_opt(ad%n_opt)
  cmn%typ = ITYPE_INT4
  cmn%idx = ad%nint4
  cmn%name        = upper(get_key_single(key_short, key_long, remove_hyphen=.true.))
  cmn%key_short   = trim(key_short)
  cmn%key_long    = trim(key_long)
  cmn%is_required = is_required
  cmn%description = trim(description)

  a => ad%lst_int4(ad%nint4)

  a%val = val_default

  a%is_positional = .false.
  a%name        => cmn%name
  a%key_short   => cmn%key_short
  a%key_long    => cmn%key_long
  a%is_required => cmn%is_required
  a%description => cmn%description
  a%used => cmn%used

  nullify(a)

  clenmax_key_short = max(len_trim(key_short), clenmax_key_short)
  clenmax_key_long  = max(len_trim(key_long ), clenmax_key_long )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__optional__int4
!===============================================================
!
!===============================================================
subroutine addarg__optional__dble(&
    key_short, key_long, val_default, is_required, description)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'addarg__optional__dble'
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  real(8)     , intent(in) :: val_default
  logical     , intent(in) :: is_required
  character(*), intent(in) :: description

  type(arg_dble_), pointer :: a
  type(arg_cmn_), pointer :: cmn

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_format_key_optional(key_short, key_long)

  call inc_n_arg(.false., ITYPE_DBLE)

  cmn => ad%cmn_opt(ad%n_opt)
  cmn%typ = ITYPE_DBLE
  cmn%idx = ad%ndble
  cmn%name        = upper(get_key_single(key_short, key_long, remove_hyphen=.true.))
  cmn%key_short   = trim(key_short)
  cmn%key_long    = trim(key_long)
  cmn%is_required = is_required
  cmn%description = trim(description)

  a => ad%lst_dble(ad%ndble)

  a%val = val_default

  a%is_positional = .false.
  a%name        => cmn%name
  a%key_short   => cmn%key_short
  a%key_long    => cmn%key_long
  a%is_required => cmn%is_required
  a%description => cmn%description
  a%used => cmn%used

  nullify(a)

  clenmax_key_short = max(len_trim(key_short), clenmax_key_short)
  clenmax_key_long  = max(len_trim(key_long ), clenmax_key_long )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine addarg__optional__dble
!===============================================================
!
!===============================================================
subroutine inc_n_arg(is_positional, itype)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'inc_n_arg'
  logical, intent(in) :: is_positional
  integer, intent(in) :: itype

  integer :: i

  integer, parameter :: NMAX_ALL_INIT = 64
  integer, parameter :: NMAX_LST_INIT = 32

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Update length of the list of data types and indices
  !-------------------------------------------------------------
  if( ad%nall == 0 )then
    allocate(ad%cmn_pos(NMAX_ALL_INIT))
    allocate(ad%cmn_opt(NMAX_ALL_INIT))
    do i = 1, NMAX_ALL_INIT
      call init_arg_cmn(ad%cmn_pos(i))
      call init_arg_cmn(ad%cmn_opt(i))
    enddo
  elseif( ad%nall == size(ad%cmn_pos) )then
    call realloc_arg_cmn(ad%cmn_pos, ad%nall*2)
    call realloc_arg_cmn(ad%cmn_opt, ad%nall*2)
  endif
  call add(ad%nall)

  if( is_positional )then
    call add(ad%n_pos)
  else
    call add(ad%n_opt)
  endif
  !-------------------------------------------------------------
  ! Update length of the list correspondent to the type of the 
  !   element
  !-------------------------------------------------------------
  selectcase( itype )

  case( ITYPE_FLAG )
    if( ad%nflag == 0 )then
      allocate(ad%lst_flag(NMAX_LST_INIT))
      do i = 1, NMAX_LST_INIT
        call init_arg_flag(ad%lst_flag(i))
      enddo
    endif
    call add(ad%nflag)

  case( ITYPE_CHAR )
    if( ad%nchar == 0 )then
      allocate(ad%lst_char(NMAX_LST_INIT))
      do i = 1, NMAX_LST_INIT
        call init_arg_char(ad%lst_char(i))
      enddo
    endif
    call add(ad%nchar)

  case( ITYPE_INT4 )
    if( ad%nint4 == 0 )then
      allocate(ad%lst_int4(NMAX_LST_INIT))
      do i = 1, NMAX_LST_INIT
        call init_arg_int4(ad%lst_int4(i))
      enddo
    endif
    call add(ad%nint4)

  case( ITYPE_DBLE )
    if( ad%ndble == 0 )then
      allocate(ad%lst_dble(NMAX_LST_INIT))
      do i = 1, NMAX_LST_INIT
        call init_arg_dble(ad%lst_dble(i))
      enddo
    endif
    call add(ad%ndble)

  case default
    call errend(msg_not_implemented()//&
      '\n  itype == '//str(itype))

  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine inc_n_arg
!===============================================================
!
!===============================================================
subroutine init_arg_flag(a)
  implicit none
  type(arg_flag_), intent(out) :: a

  nullify(a%name)
  nullify(a%key_short)
  nullify(a%key_long)
  nullify(a%is_required)
  nullify(a%description)

  nullify(a%used)

  a%is_positional = .false.
  a%val = .false.
end subroutine init_arg_flag
!===============================================================
!
!===============================================================
subroutine init_arg_char(a)
  implicit none
  type(arg_char_), intent(out) :: a

  nullify(a%name)
  nullify(a%key_short)
  nullify(a%key_long)
  nullify(a%is_required)
  nullify(a%description)

  nullify(a%used)

  a%is_positional = .false.
  allocate(character(1) :: a%val)
  a%val = ''
end subroutine init_arg_char
!===============================================================
!
!===============================================================
subroutine init_arg_int4(a)
  implicit none
  type(arg_int4_), intent(out) :: a

  nullify(a%name)
  nullify(a%key_short)
  nullify(a%key_long)
  nullify(a%is_required)
  nullify(a%description)

  nullify(a%used)

  a%is_positional = .false.
  a%val = 0
end subroutine init_arg_int4
!===============================================================
!
!===============================================================
subroutine init_arg_dble(a)
  implicit none
  type(arg_dble_), intent(out) :: a

  nullify(a%name)
  nullify(a%key_short)
  nullify(a%key_long)
  nullify(a%is_required)
  nullify(a%description)

  nullify(a%used)

  a%is_positional = .false.
  a%val = 0.d0
end subroutine init_arg_dble
!===============================================================
!
!===============================================================
subroutine check_format_key_optional(key_short, key_long)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_format_key_optional'
  character(*), intent(in) :: key_short, key_long

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( key_short == '' .and. key_long == '' )then
    call errend('Key is empty.')
  endif

  if( .not. is_key_optional(key_short) .and. &
      .not. is_key_optional(key_long) )then
    call errend('Invalid format of key for optional arguments.'//&
                ' Put "-" on the head of short key, and'//&
                ' Put "--" on the head of long key.')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_format_key_optional
!===============================================================
!
!===============================================================
logical function is_key_optional(s) result(res)
  implicit none
  character(*), intent(in) :: s

  res = .false.

  if( len_trim(s) == 2 )then
    ! Short key, e.g., -f
    if( s(1:1) == '-' .and. s(2:2) /= '-' )then
      res = .true.
    endif

  elseif( len_trim(s) >= 3 )then
    ! Short key, e.g., -f
    if( s(1:1) == '-' .and. s(2:2) /= '-' )then
      res = .true.

    ! Long key, e.g., --recursive
    elseif( s(1:2) == '--' .and. s(3:3) /= '-' )then
      res = .true.
    endif
  endif
end function is_key_optional
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
subroutine parsearg(istart, iend)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'parsearg'
  integer, intent(in), optional :: istart, iend

  integer :: istart_, iend_

  integer :: narg
  type(arg_cmn_), pointer :: cmn
  type(arg_flag_), pointer :: aflag
  type(arg_char_), pointer :: achar
  type(arg_int4_), pointer :: aint4
  type(arg_dble_), pointer :: adble
  logical :: is_found
  logical :: is_key
  character(:), allocatable :: arg
  character(:), allocatable :: s
  integer :: i
  integer :: j, jj
  integer :: j_pos

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  narg = argnum()

  istart_ = 1
  iend_ = max(ad%n_pos, narg)
  if( present(istart) ) istart_ = istart
  if( present(iend) ) iend_ = iend
  !-------------------------------------------------------------
  ! Print help message and stop if `-h` option is used
  !-------------------------------------------------------------
  do i = istart_, min(narg, iend_)
    if( argument(i) == KEY_HELP_SHORT .or. argument(i) == KEY_HELP_LONG )then
      call showarg()
      stop
    endif
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do i = 1, ad%n_pos
    cmn => ad%cmn_pos(i)
    cmn%used = .false.
  enddo

  do i = 1, ad%n_opt
    cmn => ad%cmn_opt(i)
    cmn%used = .false.
  enddo
  !-------------------------------------------------------------
  ! Read arguments
  !-------------------------------------------------------------
  !j_pos = 0
  j_pos = istart_ - 1

  i = istart_
  do while( i <= iend_ )
    arg = argument(i)
    !-----------------------------------------------------------
    ! Determine if its a key of an optional argument
    !-----------------------------------------------------------
    is_key = .false.

    if( len_trim(arg) > 2 )then
      is_key = arg(1:2) == '--'
    endif

    if( .not. is_key )then
      if( len_trim(arg) > 1 )then
        is_key = arg(1:1) == '-'
      endif
    endif
    !-----------------------------------------------------------
    ! Case: Optional argument
    if( is_key )then
      is_found = .false.
      do j = 1, ad%n_opt
        cmn => ad%cmn_opt(j)
        if( cmn%key_short == arg .or. cmn%key_long == arg )then
          is_found = .true.
          exit
        endif
      enddo

      if( .not. is_found )then
        call errend('Unrecognized argument: '//str(arg))
      endif

      if( cmn%typ /= ITYPE_FLAG )then
        if( i == iend_ )then
          call errend('Argument '//arg//' '//get_keys(cmn%key_short, cmn%key_long)//&
              ': expected one argument')
        endif
      endif

      selectcase( cmn%typ )
      case( ITYPE_FLAG )
        aflag => ad%lst_flag(cmn%idx)
        call update_used_status(aflag%used, aflag%name)
        aflag%val = .not. aflag%val
      case( ITYPE_CHAR )
        call add(i)
        arg = argument(i)
        achar => ad%lst_char(cmn%idx)
        call update_used_status(achar%used, achar%name)
        achar%val = arg
      case( ITYPE_INT4 )
        call add(i)
        arg = argument(i)
        aint4 => ad%lst_int4(cmn%idx)
        call update_used_status(aint4%used, aint4%name)
        if( c2v(arg, aint4%val) /= 0 )then
          call errend_pos_reading_failure(aint4%name, j)
        endif
      case( ITYPE_DBLE )
        call add(i)
        arg = argument(i)
        adble => ad%lst_dble(cmn%idx)
        call update_used_status(adble%used, adble%name)
        if( c2v(arg, adble%val) /= 0 )then
          call errend_pos_reading_failure(adble%name, j)
        endif
      case default
        call errend(msg_invalid_value('ad%cmn_opt('//str(j)//')%typ', cmn%typ))
      endselect
    !-----------------------------------------------------------
    ! Case: Positional argument
    else
      call add(j_pos)
      if( j_pos > ad%n_pos )then
        call errend('Unrecognized argument: '//str(arg))
      endif
      cmn => ad%cmn_pos(j_pos)

      jj = cmn%idx
      selectcase( cmn%typ )
      case( ITYPE_FLAG )
        call errend(msg_unexpected_condition()//&
            'ad%cmn_pos('//str(j_pos)//')%typ == ITYPE_FLAG')
      case( ITYPE_CHAR )
        achar => ad%lst_char(jj)
        call update_used_status(achar%used, achar%name)
        achar%val = arg
      case( ITYPE_INT4 )
        aint4 => ad%lst_int4(jj)
        call update_used_status(aint4%used, aint4%name)
        if( c2v(arg, aint4%val) /= 0 )then
          call errend_pos_reading_failure(aint4%name, j_pos)
        endif
      case( ITYPE_DBLE )
        adble => ad%lst_dble(jj)
        call update_used_status(adble%used, adble%name)
        if( c2v(arg, adble%val) /= 0 )then
          call errend_pos_reading_failure(adble%name, j_pos)
        endif
      case( ITYPE_INT1, ITYPE_INT2, ITYPE_INT8, &
            ITYPE_REAL )
        call errend(msg_not_implemented()//&
          '\nad%cmn_pos('//str(j_pos)//')%typ == '//str(cmn%typ))
      case default
        call errend(msg_invalid_value('ad%cmn_pos('//str(j_pos)//')%typ', cmn%typ))
      endselect
    endif

    call add(i)
  enddo  ! while i <= iend_
  !-------------------------------------------------------------
  ! List missing required arguments
  !-------------------------------------------------------------
  if( .not. (present(istart) .or. present(iend)) )then
    s = ''

    do j = j_pos+1, ad%n_pos
      cmn => ad%cmn_pos(j)
      if( cmn%used ) cycle
      s = s//' '//str(cmn%name)
    enddo

    do j = 1, ad%n_opt
      cmn => ad%cmn_opt(j)
      if( cmn%is_required .and. .not. cmn%used )then
        s = s//' '//get_keys(cmn%key_short, cmn%key_long)
      endif
    enddo

    if( s /= '' )then
      call errend('The following arguments are required: '//str(s))
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine update_used_status(used, name)
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__update_used_status'
  logical, intent(inout) :: used
  character(*), intent(in) :: name

  if( used )then
    call errend('Argument `'//str(name)//'` has already been given.', &
      '', PRCNAM, MODNAM)
  endif

  used = .true.
end subroutine update_used_status
!---------------------------------------------------------------
subroutine errend_pos_reading_failure(name, i)
  implicit none
  character(*), intent(in) :: name
  integer     , intent(in) :: i

  character(CLEN_PROC), parameter :: PRCNAM = 'errend_pos_reading_failure'

  call errend(msg_io_error()//' Failed to read the '//ordinal(i)//&
              ' positional argument `'//trim(name)//'`.', &
              PRCNAM, MODNAM)
end subroutine errend_pos_reading_failure
!---------------------------------------------------------------
end subroutine parsearg
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
logical(4) function arg_flag(s) result(v)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'arg_flag'
  character(*), intent(in) :: s

  type(arg_flag_), pointer :: a
  type(arg_cmn_), pointer :: cmn
  logical :: is_ok
  integer :: i


  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( s == '' )then
    call errend('No input.')
  !-------------------------------------------------------------
  ! Case: Optional
  ! -- $s is key.
  elseif( is_key_optional(s) )then
    is_ok = .false.
    do i = 1, ad%n_opt
      cmn => ad%cmn_opt(i)

      selectcase( cmn%typ )
      case( ITYPE_FLAG )
        a => ad%lst_flag(cmn%idx)
        if( a%key_short == s .or. a%key_long == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_opt('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The key "'//trim(s)//'" is invalid.')
    endif
  !-------------------------------------------------------------
  ! Case: Positional
  ! -- $s is name.
  else
    is_ok = .false.
    do i = 1, ad%n_pos
      cmn => ad%cmn_pos(i)

      selectcase( cmn%typ )
      case( ITYPE_FLAG )
        a => ad%lst_flag(cmn%idx)
        if( a%name == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_pos('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The name "'//trim(s)//'" is invalid.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function arg_flag
!===============================================================
!
!===============================================================
character(CLEN_PATH) function arg_char(s) result(v)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'arg_char'
  character(*), intent(in) :: s

  type(arg_char_), pointer :: a
  type(arg_cmn_), pointer :: cmn
  logical :: is_ok
  integer :: i

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( s == '' )then
    call errend('No input.')
  !-------------------------------------------------------------
  ! Case: Optional
  ! -- $s is key.
  elseif( is_key_optional(s) )then
    is_ok = .false.
    do i = 1, ad%n_opt
      cmn => ad%cmn_opt(i)

      selectcase( cmn%typ )
      case( ITYPE_CHAR )
        a => ad%lst_char(cmn%idx)
        if( a%key_short == s .or. a%key_long == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_opt('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The key "'//trim(s)//'" is invalid.')
    endif
  !-------------------------------------------------------------
  ! Case: Positional
  ! -- $s is name.
  else
    is_ok = .false.
    do i = 1, ad%n_pos
      cmn => ad%cmn_pos(i)

      selectcase( cmn%typ )
      case( ITYPE_CHAR )
        a => ad%lst_char(cmn%idx)
        if( a%name == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_pos('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The name "'//trim(s)//'" is invalid.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function arg_char
!===============================================================
!
!===============================================================
integer(4) function arg_int4(s) result(v)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'arg_int4'
  character(*), intent(in) :: s

  type(arg_int4_), pointer :: a
  type(arg_cmn_), pointer :: cmn
  logical :: is_ok
  integer :: i

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( s == '' )then
    call errend('No input.')
  !-------------------------------------------------------------
  ! Case: Optional
  ! -- $s is key.
  elseif( is_key_optional(s) )then
    is_ok = .false.
    do i = 1, ad%n_opt
      cmn => ad%cmn_opt(i)

      selectcase( cmn%typ )
      case( ITYPE_INT4 )
        a => ad%lst_int4(cmn%idx)
        if( a%key_short == s .or. a%key_long == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_opt('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The key "'//trim(s)//'" is invalid.')
    endif
  !-------------------------------------------------------------
  ! Case: Positional
  ! -- $s is name.
  else
    is_ok = .false.
    do i = 1, ad%n_pos
      cmn => ad%cmn_pos(i)

      selectcase( cmn%typ )
      case( ITYPE_INT4 )
        a => ad%lst_int4(cmn%idx)
        if( a%name == s )then
          if( .not. a%used )then
            call errend('The '//ordinal(i)//' positional argument is missing.')
          endif
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT8, &
            ITYPE_REAL, ITYPE_DBLE )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_pos('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The name "'//trim(s)//'" is invalid.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function arg_int4
!===============================================================
!
!===============================================================
real(8) function arg_dble(s) result(v)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'arg_dble'
  character(*), intent(in) :: s

  type(arg_dble_), pointer :: a
  type(arg_cmn_), pointer :: cmn
  logical :: is_ok
  integer :: i

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( s == '' )then
    call errend('No input.')
  !-------------------------------------------------------------
  ! Case: Optional
  ! -- $s is key.
  elseif( is_key_optional(s) )then
    is_ok = .false.
    do i = 1, ad%n_opt
      cmn => ad%cmn_opt(i)

      selectcase( cmn%typ )
      case( ITYPE_DBLE )
        a => ad%lst_dble(cmn%idx)
        if( a%key_short == s .or. a%key_long == s )then
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_opt('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The key "'//trim(s)//'" is invalid.')
    endif
  !-------------------------------------------------------------
  ! Case: Positional
  ! -- $s is name.
  else
    is_ok = .false.
    do i = 1, ad%n_pos
      cmn => ad%cmn_pos(i)

      selectcase( cmn%typ )
      case( ITYPE_DBLE )
        a => ad%lst_dble(cmn%idx)
        if( a%name == s )then
          if( .not. a%used )then
            call errend('The '//ordinal(i)//' positional argument is missing.')
          endif
          v = a%val
          is_ok = .true.
          exit
        endif
      case( ITYPE_FLAG, &
            ITYPE_CHAR, &
            ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
            ITYPE_REAL )
        continue
      case default
        call errend(msg_invalid_value(&
               'ad%cmn_pos('//str(i)//')%typ', cmn%typ))
      endselect
    enddo

    if( .not. is_ok )then
      call errend('The name "'//trim(s)//'" is invalid.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function arg_dble
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
subroutine showarg()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'showarg'

  type(arg_cmn_), pointer :: cmn
  integer :: i
  character(:), allocatable :: s
  character(1) :: parenthl, parenthr

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: s)
  s = 'usage: ./*.exe [-h]'

  do i = 1, ad%n_opt
    cmn => ad%cmn_opt(i)

    parenthl = ''
    parenthr = ''
    if( .not. cmn%is_required )then
      parenthl = '['
      parenthr = ']'
    endif  

    selectcase( cmn%typ )
    case( ITYPE_FLAG )
      s = trim(s)//' '//trim(parenthl)//&
          get_key_single(cmn%key_short, cmn%key_long)//&
          trim(parenthr)

    case( ITYPE_CHAR, &
          ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
          ITYPE_REAL, ITYPE_DBLE )
      s = trim(s)//' '//trim(parenthl)//&
          get_key_single(cmn%key_short, cmn%key_long)//&
          ' '//trim(cmn%name)//trim(parenthr)

    case default
      call errend(msg_invalid_value('ad%cmn_opt('//str(i)//')%typ', cmn%typ))
    endselect
  enddo  ! i = 1, ad%n_opt/

  do i = 1, ad%n_pos
    cmn => ad%cmn_pos(i)

    selectcase( cmn%typ )
    case( ITYPE_FLAG )
      call errend(msg_unexpected_condition()//&
                '\n  ad%cmn_pos('//str(i)//')%typ == ITYPE_FLAG')

    case( ITYPE_CHAR, &
          ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
          ITYPE_REAL, ITYPE_DBLE )
      s = trim(s)//' '//trim(cmn%name)

    case default
      call errend(msg_invalid_value('ad%cmn_pos('//str(i)//')%typ', cmn%typ))
    endselect
  enddo  ! i = 1, ad%n_pos/

  call logmsg(s, opt='x0')
  !-------------------------------------------------------------
  ! Positional arguments
  !-------------------------------------------------------------
  if( ad%n_pos > 0 )then
    call logmsg('')
    call logmsg('positional arguments:', opt='x0')
  endif

  do i = 1, ad%n_pos
    call logmsg(ad%cmn_pos(i)%name, opt='x2')
  enddo
  !-------------------------------------------------------------
  ! Optional arguments
  !-------------------------------------------------------------
  if( ad%n_opt > 0 )then
    call logmsg('')
    call logmsg('optional arguments:', opt='x0')
  endif

  do i = 1, ad%n_opt
    cmn => ad%cmn_opt(i)

    s = get_keys(cmn%key_short, cmn%key_long)

    selectcase( cmn%typ )
    case( ITYPE_FLAG )
      continue
    case( ITYPE_CHAR, &
          ITYPE_INT1, ITYPE_INT2, ITYPE_INT4, ITYPE_INT8, &
          ITYPE_REAL, ITYPE_DBLE )
      s = trim(s)//' '//trim(cmn%name)
    endselect

    if( cmn%description /= '' )then
      s = s//'    '//cmn%description
    endif

    call logmsg(s, opt='x2')
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine showarg
!===============================================================
!
!===============================================================
function styp(ityp) result(s)
  implicit none
  integer, intent(in) :: ityp
  character(:), allocatable :: s

  character(CLEN_PROC), parameter :: PRCNAM = 'styp'

  allocate(character(1) :: s)

  selectcase( ityp )
  case( ITYPE_FLAG )
    s = trim(STYPE_FLAG)
  case( ITYPE_CHAR )
    s = trim(STYPE_CHAR)
  case( ITYPE_INT1 )
    s = trim(STYPE_INT1)
  case( ITYPE_INT2 )
    s = trim(STYPE_INT2)
  case( ITYPE_INT4 )
    s = trim(STYPE_INT4)
  case( ITYPE_INT8 )
    s = trim(STYPE_INT8)
  case( ITYPE_REAL )
    s = trim(STYPE_REAL)
  case( ITYPE_DBLE )
    s = trim(STYPE_DBLE)
  case default
    call errend(msg_invalid_value('ityp', ityp))
  endselect
end function styp
!===============================================================
!
!===============================================================
function get_key_single(&
    key_short, key_long, get_long, remove_hyphen) result(res)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical, intent(in), optional :: get_long
  logical, intent(in), optional :: remove_hyphen
  character(:), allocatable :: res

  logical :: get_long_
  logical :: remove_hyphen_

  get_long_ = .false.
  remove_hyphen_ = .false.
  if( present(get_long) ) get_long_ = get_long
  if( present(remove_hyphen) ) remove_hyphen_ = remove_hyphen

  allocate(character(1) :: res)

  if( get_long_ )then
    if( key_long == '' )then
      res = trim(key_short)
    else
      res = trim(key_long)
    endif
  else
    if( key_short == '' )then
      res = trim(key_long)
    else
      res = trim(key_short)
    endif
  endif

  if( remove_hyphen_ )then
    if( res(2:2) == '-' )then
      res = res(3:)  ! long
    else
      res = res(2:)  ! short
    endif
  endif
end function get_key_single
!===============================================================
!
!===============================================================
function get_keys(key_short, key_long) result(s)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  character(:), allocatable :: s

  if( key_short == '' )then
    s = trim(key_long)
  elseif( key_long == '' )then
    s = trim(key_short)
  else
    s = trim(key_short)//'/'//trim(key_long)
  endif
end function get_keys
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
subroutine init_arg_cmn(cmn)
  implicit none
  type(arg_cmn_), intent(inout) :: cmn

  integer :: i

  allocate(character(1) :: cmn%name)
  allocate(character(1) :: cmn%key_short)
  allocate(character(1) :: cmn%key_long)
  allocate(character(1) :: cmn%description)

  cmn%used = .false.
  cmn%typ = ITYPE_UNDEF
  cmn%idx = 0
  cmn%name = ''
  cmn%key_short = ''
  cmn%key_long = ''
  cmn%is_required = .false.
  cmn%description = ''
end subroutine init_arg_cmn
!===============================================================
!
!===============================================================
subroutine realloc_arg_cmn(arr, n)
  implicit none
  type(arg_cmn_), pointer :: arr(:)
  integer, intent(in) :: n

  type(arg_cmn_), allocatable :: tmp(:)
  integer :: i

  allocate(tmp(size(arr)))
  tmp(:) = arr(:)
  deallocate(arr)
  allocate(arr(n))
  arr(:size(tmp)) = tmp(:)
  do i = size(tmp)+1, n
    call init_arg_cmn(arr(i))
  enddo
  deallocate(tmp)
end subroutine realloc_arg_cmn
!===============================================================
!
!===============================================================
end module lib_io_arg_parser
