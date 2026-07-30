module lib_io_file
  use lib_const
  use lib_base
  use lib_log
  use lib_array, only: &
    reversed
  use lib_io_base, only: &
    unit_number, &
    byte_of_dtype
  implicit none
  private
  !-------------------------------------------------------------
  ! Public module variables and types
  !-------------------------------------------------------------
  public :: FILEDIM

  public :: file_
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: joined
  public :: dirname
  public :: filename
  public :: path_ins
  public :: filesize

  public :: file
  public :: set_file_default
  public :: reset_file_default

  public :: init_file
  public :: update_file
  public :: set_path
  public :: set_dtype
  public :: set_rec
  public :: set_endian
  public :: set_stat
  public :: set_length
  public :: set_size
  public :: set_lower
  public :: set_upper

  public :: fileinfo

  public :: set_opt_check_permission
  public :: init_opt_check_permission
  public :: check_permission
  public :: check_file_size
  public :: try_make_empty_file
  public :: set_opt_mkdir
  public :: init_opt_mkdir
  public :: mkdir
  public :: remove
  public :: make_empty_file
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface set_path
    module procedure set_path_0d
    module procedure set_path_1d
  end interface

  interface set_dtype
    module procedure set_dtype_0d
    module procedure set_dtype_1d
  end interface

  interface set_rec
    module procedure set_rec_0d
    module procedure set_rec_1d
  end interface

  interface set_endian
    module procedure set_endian_log4_0d
    module procedure set_endian_log4_1d
    module procedure set_endian_char_0d
    module procedure set_endian_char_1d
  end interface

  interface set_stat
    module procedure set_stat_0d
    module procedure set_stat_1d
  end interface

  interface set_length
    module procedure set_length_0d
    module procedure set_length_1d
  end interface

  interface set_size
    module procedure set_size_0d
    module procedure set_size_1d
  end interface

  interface set_lower
    module procedure set_lower_0d
    module procedure set_lower_1d
  end interface

  interface set_upper
    module procedure set_upper_0d
    module procedure set_upper_1d
  end interface

  interface try_make_empty_file
    module procedure try_make_empty_file__file
    module procedure try_make_empty_file__dir
  end interface

  interface check_permission
    module procedure check_permission__file
    module procedure check_permission__path
  end interface
  !-------------------------------------------------------------
  ! Public module variables and types
  !-------------------------------------------------------------
  integer, parameter :: FILEDIM = 3

  type file_
    character(clen_path) :: id         = 'file'
    character(clen_path) :: path       = ''
    character(clen_key)  :: dtype      = DTYPE_REAL
    character(clen_key)  :: endian     = ENDIAN_DEFAULT
    integer              :: rec        = 1
    integer(8)           :: length     = 0_8
    character(clen_key)  :: status     = STATUS_UNKNOWN
    character(clen_key)  :: action     = ACTION_READWRITE
    integer              :: permission = PERMISSION_RW
    integer(8)           :: sz(FILEDIM)  ! Size of each record
    integer(8)           :: lb(FILEDIM)  ! Lower bounds in the records
    integer(8)           :: ub(FILEDIM)  ! Upper bounds       "
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_file'

  type(file_), target, save :: file_default

  character(2)        , parameter :: OPT_DEFAULT_MKDIR__HUT = '+ '
  character(clen_line), save      :: opt_mkdir__hut = OPT_DEFAULT_MKDIR__HUT

  integer, parameter :: OPT_DEFAULT_MKDIR__CLEN_HUT = 2
  integer, save      :: opt_mkdir__clen_hut = OPT_DEFAULT_MKDIR__CLEN_HUT

  logical, parameter :: OPT_DEFAULT_MKDIR__OUTPUT = .false.
  logical, save      :: opt_mkdir__output = OPT_DEFAULT_MKDIR__OUTPUT

  logical, parameter :: OPT_DEFAULT_CHECK_PERMISSION__ALLOW_EMPTY = .false.
  logical, save      :: opt_check_permission__allow_empty &
                          = OPT_DEFAULT_CHECK_PERMISSION__ALLOW_EMPTY
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
pure function joined(path1, path2, is_dir) result(path)
  implicit none
  character(*), intent(in)           :: path1, path2
  logical     , intent(in), optional :: is_dir  ! path2 is dir
  character(len_joined(path1,path2)) :: path
  character(len_trim(path1)) :: path1_
  character(len_trim(path2)) :: path2_
  logical :: is_dir_

  is_dir_ = .false.
  if( present(is_dir) ) is_dir_ = is_dir

  if( len_trim(path2) == 0 )then
    if( is_dir_ )then
      path = path1
    else
      path = ''
    endif

  elseif( len_trim(path1) == 0 .or. path2(1:1) == '/' )then
    path = trim(path2)

  else
    if( path1(len_trim(path1):len_trim(path1)) == '/' )then
      path1_ = path1(:len_trim(path1)-1)
    else
      path1_ = path1
    endif

    path2_ = path2
    do while( index(path2_,'../') == 1 )
      path1_ = path1_(:len_trim(path1_)-index(reversed(trim(path1_)),'/'))
      path2_ = path2_(4:)
    enddo

    path = trim(path1_)//'/'//trim(path2_)
  endif
end function joined
!===============================================================
!
!===============================================================
integer pure function len_joined(dir, path) result(l)
  implicit none
  character(*), intent(in) :: dir
  character(*), intent(in) :: path

  if( len_trim(path) == 0 )then
    l = len_trim(dir)
  elseif( len_trim(dir) == 0 .or. path(1:1) == '/' )then
    l = len_trim(path)
  else
    if( dir(len_trim(dir):len_trim(dir)) == '/' )then
      l = len_trim(dir) + len_trim(path)
    else
      l = len_trim(dir) + len_trim(path) + 1
    endif
  endif

  l = max(1,l)
end function len_joined
!===============================================================
!
!===============================================================
pure function dirname(path) result(dir)
  implicit none
  character(*), intent(in)  :: path
  character(clen_dir(path)) :: dir

  if( index(path,'/') == 0 )then
    dir = './'
  else
    dir = path(:len_trim(path)-index(reversed(trim(path)),'/'))
  endif
end function dirname
!===============================================================
!
!===============================================================
pure function filename(path) result(file)
  implicit none
  character(*), intent(in)   :: path
  character(clen_file(path)) :: file

  if( index(path,'/') == 0 )then
    file = path
  else
    file = path(len_trim(path)-index(reversed(trim(path)),'/')+2:)
  endif
end function filename
!===============================================================
!
!===============================================================
integer pure function clen_dir(path) result(l)
  implicit none
  character(*), intent(in) :: path

  if( index(path,'/') == 0 )then
    l = 0
  else
    l = len_trim(path) - index(reversed(trim(path)),'/')
  endif
end function clen_dir
!===============================================================
!
!===============================================================
integer pure function clen_file(path) result(l)
  implicit none
  character(*), intent(in) :: path

  if( index(path,'/') == 0 )then
    l = len_trim(path)
  else
    l = index(reversed(trim(path)),'/') - 1
  endif
end function clen_file
!===============================================================
!
!===============================================================
integer(8) function filesize(path) result(sz)
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'filesize'
  character(*), intent(in) :: path

  integer :: un
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  un = unit_number()
  open(un, file=path, status='old', action='read', iostat=ios)

  if( ios /= 0 )then
    sz = -1
    return
  endif

  inquire(un, size=sz)
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function filesize
!===============================================================
! When $path = "XX.bin", $s = "_AA", $no_ext = false,
! RESULT is "XX_AA.bin".
!===============================================================
pure function path_ins(path, s, no_ext)
  implicit none
  character(*), intent(in)           :: path
  character(*), intent(in)           :: s
  logical     , intent(in), optional :: no_ext
  character(:), allocatable          :: path_ins

  character(len_trim(path)) :: f
  logical :: no_ext_
  integer :: cl_ext, cl
  integer :: loc

  no_ext_ = .false.
  if( present(no_ext) ) no_ext_ = no_ext

  cl = len_trim(path) + len_trim(s)
  allocate(character(cl) :: path_ins)

  if( no_ext_ )then
    path_ins = trim(path)//trim(s)
  else
    f = filename(path)
    cl_ext = index(reversed(trim(f)),'.') - 1
    if( cl_ext == 0 )then
      path_ins = ''
      return
    endif
    loc = len_trim(path) - (cl_ext + 1)
    path_ins = path(:loc)//trim(s)//path(loc+1:)
  endif
end function path_ins
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
type(file_) function file(&
    path, dtype, endian, rec, &
    length, status, action, permission, id, &
    sz1, sz2, sz3, lb1, lb2, lb3, ub1, ub2, ub3) result(f)
  implicit none
  character(*), intent(in), optional :: path
  character(*), intent(in), optional :: dtype
  character(*), intent(in), optional :: endian
  integer(4)  , intent(in), optional :: rec
  integer(8)  , intent(in), optional :: length
  character(*), intent(in), optional :: status
  character(*), intent(in), optional :: action
  integer     , intent(in), optional :: permission
  character(*), intent(in), optional :: id
  integer(8)  , intent(in), optional :: sz1, sz2, sz3
  integer(8)  , intent(in), optional :: lb1, lb2, lb3
  integer(8)  , intent(in), optional :: ub1, ub2, ub3

  f = file_default

  if( present(path)       ) f%path       = path
  if( present(dtype)      ) f%dtype      = dtype
  if( present(endian)     ) f%endian     = endian
  if( present(rec)        ) f%rec        = rec
  if( present(length)     ) f%length     = length
  if( present(status)     ) f%status     = status
  if( present(action)     ) f%action     = action
  if( present(permission) ) f%permission = permission
  if( present(id)         ) f%id         = id
  if( present(sz1)        ) f%sz(1)      = sz1
  if( present(sz2)        ) f%sz(2)      = sz2
  if( present(sz3)        ) f%sz(3)      = sz3
  if( present(lb1)        ) f%lb(1)      = lb1
  if( present(lb2)        ) f%lb(2)      = lb2
  if( present(lb3)        ) f%lb(3)      = lb3
  if( present(ub1)        ) f%ub(1)      = ub1
  if( present(ub2)        ) f%ub(2)      = ub2
  if( present(ub3)        ) f%ub(3)      = ub3
end function file
!===============================================================
!
!===============================================================
subroutine set_file_default(&
    id, path, dtype, endian, rec, length, &
    status, action, permission, sz, lb, ub)
  implicit none
  character(*), intent(in), optional :: id
  character(*), intent(in), optional :: path
  character(*), intent(in), optional :: dtype
  character(*), intent(in), optional :: endian
  integer     , intent(in), optional :: rec
  integer     , intent(in), optional :: length
  character(*), intent(in), optional :: status
  character(*), intent(in), optional :: action
  integer     , intent(in), optional :: permission
  integer     , intent(in), optional :: sz(:)
  integer     , intent(in), optional :: lb(:)
  integer     , intent(in), optional :: ub(:)

  type(file_), pointer :: f

  f => file_default

  if( present(id)     ) f%id = id
  if( present(path)   ) f%path = path
  if( present(dtype)  ) f%dtype = dtype
  if( present(endian) ) f%endian = endian
  if( present(rec)    ) f%rec = rec
  if( present(length) ) f%length = length
  if( present(status) ) f%status = status
  if( present(action) ) f%action = action
  if( present(permission) ) f%permission = permission
  if( present(sz) ) f%sz = sz
  if( present(lb) ) f%lb = lb
  if( present(ub) ) f%ub = ub

  nullify(f)
end subroutine set_file_default
!===============================================================
!
!===============================================================
subroutine reset_file_default()
  implicit none

  type(file_), pointer :: f

  f => file_default

  f%id         = ID_UNDEF
  f%path       = ''
  f%dtype      = DTYPE_REAL
  f%endian     = ENDIAN_DEFAULT
  f%rec        = 1
  f%length     = 0_8
  f%status     = STATUS_UNKNOWN
  f%action     = ACTION_READWRITE
  f%permission = PERMISSION_RW
  f%sz(:)      = 0_8
  f%lb(:)      = 0_8
  f%ub(:)      = 0_8

  nullify(f)
end subroutine reset_file_default
!===============================================================
!
!===============================================================
subroutine init_file(f)
  implicit none
  type(file_), intent(out) :: f

  f%id         = ID_UNDEF
  f%path       = ''
  f%dtype      = DTYPE_UNDEF
  f%endian     = ENDIAN_UNDEF
  f%rec        = REC_UNDEF
  f%length     = -9999_8
  f%status     = STATUS_UNDEF
  f%action     = ACTION_UNDEF
  f%permission = PERMISSION_UNDEF
  f%sz(:)      = -9999_8
  f%lb(:)      = -9999_8
  f%ub(:)      = -9999_8
end subroutine init_file
!===============================================================
!
!===============================================================
subroutine update_file(&
    f, &
    id, path, dtype, endian, rec, &
    length, status, action, permission, sz, lb, ub)
  implicit none
  type(file_), intent(inout) :: f
  character(*), intent(in), optional :: id
  character(*), intent(in), optional :: path 
  character(*), intent(in), optional :: dtype
  character(*), intent(in), optional :: endian
  integer     , intent(in), optional :: rec
  integer(8)  , intent(in), optional :: length
  character(*), intent(in), optional :: status
  character(*), intent(in), optional :: action
  integer     , intent(in), optional :: permission
  integer(8)  , intent(in), optional :: sz(:)
  integer(8)  , intent(in), optional :: lb(:)
  integer(8)  , intent(in), optional :: ub(:)

  if( present(id) ) f%id = id
  if( present(path) ) f%path = path
  if( present(dtype) ) f%dtype = dtype
  if( present(endian) ) f%endian = endian
  if( present(rec) ) f%rec = rec
  if( present(length) ) f%length = length
  if( present(status) ) f%status = status
  if( present(action) ) f%action = action
  if( present(permission) ) f%permission = permission
  if( present(sz) ) f%sz(:size(sz)) = f%sz(:size(sz))
  if( present(lb) ) f%lb(:size(lb)) = f%lb(:size(lb))
  if( present(ub) ) f%ub(:size(ub)) = f%ub(:size(ub))
end subroutine update_file
!===============================================================
!
!===============================================================
subroutine set_path_0d(path, path_specific, path_general, path_default, dir)
  implicit none
  character(*), intent(inout) :: path
  character(*), intent(in)    :: path_specific
  character(*), intent(in), optional :: path_general
  character(*), intent(in), optional :: path_default
  character(*), intent(in), optional :: dir
  
  if( path == '' )then
    if( present(path_default) )then
      if( path_default /= '' ) path = path_default
    endif

    if( present(path_general) )then
      if( path_general /= '' ) path = path_general
    endif

    if( path_specific /= '' ) path = path_specific
  endif

  if( present(dir) ) path = joined(dir, path)
end subroutine set_path_0d
!===============================================================
!
!===============================================================
subroutine set_path_1d(path, path_specific, path_general, path_default, dir)
  implicit none
  character(*), intent(inout) :: path(:)
  character(*), intent(in)    :: path_specific
  character(*), intent(in), optional :: path_general
  character(*), intent(in), optional :: path_default
  character(*), intent(in), optional :: dir
  integer :: i

  do i = 1, size(path)
    if( path(i) == '' )then
      if( present(path_default) )then
        if( path_default /= '' ) path(i) = path_default
      endif

      if( present(path_general) )then
        if( path_general /= '' ) path(i) = path_general
      endif

      if( path_specific /= '' ) path(i) = path_specific
    endif

    if( present(dir) ) path(i) = joined(dir, path(i))
  enddo
end subroutine set_path_1d
!===============================================================
!
!===============================================================
subroutine set_dtype_0d(dtype, dtype_specific, dtype_general, dtype_default)
  implicit none
  character(*), intent(inout) :: dtype
  character(*), intent(in)    :: dtype_specific
  character(*), intent(in), optional :: dtype_general
  character(*), intent(in), optional :: dtype_default

  if( dtype == '' )then
    if( present(dtype_default) )then
      if( dtype_default /= '' ) dtype = trim(dtype_default)
    endif

    if( present(dtype_general) )then
      if( dtype_general /= '' ) dtype = trim(dtype_general)
    endif

    if( dtype_specific /= '' ) dtype = trim(dtype_specific)
  endif
end subroutine set_dtype_0d
!===============================================================
!
!===============================================================
subroutine set_dtype_1d(dtype, dtype_specific, dtype_general, dtype_default)
  implicit none
  character(*), intent(inout) :: dtype(:)
  character(*), intent(in)    :: dtype_specific
  character(*), intent(in), optional :: dtype_general
  character(*), intent(in), optional :: dtype_default
  integer :: i

  do i = 1, size(dtype)
    if( dtype(i) == '' )then
      if( present(dtype_default) )then
        if( dtype_default /= '' ) dtype(i) = trim(dtype_default)
      endif

      if( present(dtype_general) )then
        if( dtype_general /= '' ) dtype(i) = trim(dtype_general)
      endif

      if( dtype_specific /= '' ) dtype(i) = trim(dtype_specific)
    endif
  enddo
end subroutine set_dtype_1d
!===============================================================
!
!===============================================================
subroutine set_rec_0d(rec, rec_specific, rec_general, rec_default)
  implicit none
  integer, intent(inout) :: rec
  integer, intent(in)    :: rec_specific
  integer, intent(in), optional :: rec_general
  integer, intent(in), optional :: rec_default

  if( rec <= 0 )then
    if( present(rec_default) )then
      if( rec_default > 0 ) rec = rec_default
    endif

    if( present(rec_general) )then
      if( rec_general > 0 ) rec = rec_general
    endif

    if( rec_specific > 0 ) rec = rec_specific
  endif
end subroutine set_rec_0d
!===============================================================
!
!===============================================================
subroutine set_rec_1d(rec, rec_specific, rec_general, rec_default)
  implicit none
  integer, intent(inout) :: rec(:)
  integer, intent(in)    :: rec_specific
  integer, intent(in), optional :: rec_general
  integer, intent(in), optional :: rec_default
  integer :: i

  do i = 1, size(rec)
    if( rec(i) <= 0 )then
      if( present(rec_default) )then
        if( rec_default > 0 ) rec(i) = rec_default
      endif

      if( present(rec_general) )then
        if( rec_general > 0 ) rec(i) = rec_general
      endif

      if( rec_specific > 0 ) rec(i) = rec_specific
    endif
  enddo
end subroutine set_rec_1d
!===============================================================
!
!===============================================================
subroutine set_endian_log4_0d(little, endian_specific, endian_general, endian_default)
  implicit none
  logical     , intent(out) :: little
  character(*), intent(in)  :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default

  if( present(endian_default) )then
    if( endian_default /= '' ) little = ( endian_default == ENDIAN_LITTLE )
  endif

  if( present(endian_general) )then
    if( endian_general /= '' ) little = ( endian_general == ENDIAN_LITTLE )
  endif

  if( endian_specific /= '' ) little = ( endian_specific == ENDIAN_LITTLE )
end subroutine set_endian_log4_0d
!===============================================================
!
!===============================================================
subroutine set_endian_log4_1d(little, endian_specific, endian_general, endian_default)
  implicit none
  logical     , intent(out) :: little(:)
  character(*), intent(in)  :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default

  if( present(endian_default) )then
    if( endian_default /= '' ) little = ( endian_default == ENDIAN_LITTLE )
  endif

  if( present(endian_general) )then
    if( endian_general /= '' ) little = ( endian_general == ENDIAN_LITTLE )
  endif

  if( endian_specific /= '' ) little = ( endian_specific == ENDIAN_LITTLE )
end subroutine set_endian_log4_1d
!===============================================================
!
!===============================================================
subroutine set_endian_char_0d(endian, endian_specific, endian_general, endian_default)
  implicit none
  character(*), intent(inout) :: endian
  character(*), intent(in)    :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default

  if( endian == '' )then
    if( present(endian_default) )then
      if( endian_default /= '' ) endian = endian_default
    endif

    if( present(endian_general) )then
      if( endian_general /= '' ) endian = endian_general
    endif

    if( endian_specific /= '' ) endian = endian_specific
  endif
end subroutine set_endian_char_0d
!===============================================================
!
!===============================================================
subroutine set_endian_char_1d(endian, endian_specific, endian_general, endian_default)
  implicit none
  character(*), intent(inout) :: endian(:)
  character(*), intent(in)    :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default
  integer :: i

  do i = 1, size(endian)
    if( endian(i) == '' )then
      if( present(endian_default) )then
        if( endian_default /= '' ) endian(i) = endian_default
      endif

      if( present(endian_general) )then
        if( endian_general /= '' ) endian(i) = endian_general
      endif

      if( endian_specific /= '' ) endian(i) = endian_specific
    endif
  enddo
end subroutine set_endian_char_1d
!===============================================================
!
!===============================================================
subroutine set_stat_0d(stat, stat_specific, stat_general, stat_default)
  implicit none
  character(*), intent(inout) :: stat
  character(*), intent(in)    :: stat_specific
  character(*), intent(in), optional :: stat_general
  character(*), intent(in), optional :: stat_default

  if( stat == '' )then
    if( present(stat_default) )then
      if( stat_default /= '' ) stat = stat_default
    endif

    if( present(stat_general) )then
      if( stat_general /= '' ) stat = stat_general
    endif

    if( stat_specific /= '' ) stat = stat_specific
  endif
end subroutine set_stat_0d
!===============================================================
!
!===============================================================
subroutine set_stat_1d(stat, stat_specific, stat_general, stat_default)
  implicit none
  character(*), intent(inout) :: stat(:)
  character(*), intent(in)    :: stat_specific
  character(*), intent(in), optional :: stat_general
  character(*), intent(in), optional :: stat_default
  integer :: i

  do i = 1, size(stat)
    if( stat(i) == '' )then
      if( present(stat_default) )then
        if( stat_default /= '' ) stat(i) = stat_default
      endif

      if( present(stat_general) )then
        if( stat_general /= '' ) stat(i) = stat_general
      endif

      if( stat_specific /= '' ) stat(i) = stat_specific
    endif
  enddo
end subroutine set_stat_1d
!===============================================================
!
!===============================================================
subroutine set_length_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_length_0d
!===============================================================
!
!===============================================================
subroutine set_length_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default /= 0 ) v(i) = v_default
      endif

      if( present(v_general) )then
        if( v_general /= 0 ) v(i) = v_general
      endif

      if( v_specific /= 0 ) v(i) = v_specific
    endif
  enddo
end subroutine set_length_1d
!===============================================================
!
!===============================================================
subroutine set_size_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_size_0d
!===============================================================
!
!===============================================================
subroutine set_size_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific(:)
  integer(8), intent(in), optional :: v_general(:)
  integer(8), intent(in), optional :: v_default(:)
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default(i) /= 0 ) v(i) = v_default(i)
      endif

      if( present(v_general) )then
        if( v_general(i) /= 0 ) v(i) = v_general(i)
      endif

      if( v_specific(i) /= 0 ) v(i) = v_specific(i)
    endif
  enddo
end subroutine set_size_1d
!===============================================================
!
!===============================================================
subroutine set_lower_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_lower_0d
!===============================================================
!
!===============================================================
subroutine set_lower_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific(:)
  integer(8), intent(in), optional :: v_general(:)
  integer(8), intent(in), optional :: v_default(:)
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default(i) /= 0 ) v(i) = v_default(i)
      endif

      if( present(v_general) )then
        if( v_general(i) /= 0 ) v(i) = v_general(i)
      endif

      if( v_specific(i) /= 0 ) v(i) = v_specific(i)
    endif
  enddo
end subroutine set_lower_1d
!===============================================================
!
!===============================================================
subroutine set_upper_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_upper_0d
!===============================================================
!
!===============================================================
subroutine set_upper_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific(:)
  integer(8), intent(in), optional :: v_general(:)
  integer(8), intent(in), optional :: v_default(:)
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default(i) /= 0 ) v(i) = v_default(i)
      endif

      if( present(v_general) )then
        if( v_general(i) /= 0 ) v(i) = v_general(i)
      endif

      if( v_specific(i) /= 0 ) v(i) = v_specific(i)
    endif
  enddo
end subroutine set_upper_1d
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
function fileinfo(f) result(res)
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'fileinfo'
  type(file_) , intent(in)  :: f
  character(:), allocatable :: res

  character(clen_key) :: endian
  integer             :: clen_res
  logical             :: is_ok
  character(clen_key) :: opt
  character(4+dgt(INT4_ULIM)) :: str_rec

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( f%path == '' )then
    allocate(character(10) :: res)
    res = '(Not specified)'

  else
    is_ok = .true.
    opt = '-q'
    !-----------------------------------------------------------
    ! Set endian
    !-----------------------------------------------------------
    selectcase( f%endian )
    case( ENDIAN_LITTLE, &
          ENDIAN_LITTLE_SHORT )
      endian = ENDIAN_LITTLE_SHORT
    case( ENDIAN_BIG, &
          ENDIAN_BIG_SHORT )
      endian = ENDIAN_BIG_SHORT
    case default
      call logerr(msg_invalid_value('f%endian', f%endian)//&
                '\nid  : '//str(f%id)//&
                '\npath: '//str(f%path), opt='-q')
    endselect
    !-----------------------------------------------------------
    ! Check dtype
    !-----------------------------------------------------------
    selectcase( f%dtype )
    case( DTYPE_INT1, &
          DTYPE_INT2, &
          DTYPE_INT4, &
          DTYPE_INT8, &
          DTYPE_REAL, &
          DTYPE_DBLE )
      continue
    case( DTYPE_UNDEF )
      continue
    case default
      call logerr(msg_invalid_value('f%dtype', f%dtype)//&
                '\nid  : '//str(f%id)//&
                '\npath: '//str(f%path), opt='-q')
    endselect
    !-----------------------------------------------------------
    ! Check rec
    !-----------------------------------------------------------
    if( f%rec > 0 )then
      str_rec = 'rec:'//str(f%rec)
    elseif( f%rec == REC_UNDEF )then
      str_rec = 'rec:undef'
    else
      call logerr(msg_invalid_value('f%rec', f%rec)//&
                '\nid  : '//str(f%id)//&
                '\npath: '//str(f%path), opt='-q')
    endif
    !-----------------------------------------------------------
    ! Check length
    !-----------------------------------------------------------
    if( f%length < 0 )then
      call logerr(msg_invalid_value('f%length', f%length)//&
                '\nid  : '//str(f%id)//&
                '\npath: '//str(f%path), opt='-q')
    endif
    !-----------------------------------------------------------
    ! Generate the string of file information
    !-----------------------------------------------------------
    if( f%length == 0 )then
      clen_res = len_trim(f%path) &
                 + 2 + len_trim(endian) &
                 + 1 + len_trim(f%dtype) &
                 + 1 + len_trim(str_rec) &
                 + 1
      allocate(character(clen_res) :: res)
      res = str(f%path)//' ('//str(endian)//' '//&
             str(f%dtype)//' '//str(str_rec)//')'
    else
      clen_res = len_trim(f%path) &
                 + 1 + len_trim(endian) &
                 + 1 + len_trim(f%dtype) &
                 + 1 + len_trim(str_rec) &
                 + 5 + dgt(f%length) &
                 + 1
      allocate(character(clen_res) :: res)
      res = str(f%path)//' ('//str(endian)//' '//str(f%dtype)//&
            ' '//str(str_rec)//' length:'//str(f%length)//')'
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function fileinfo
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
integer(4) function set_opt_check_permission(allow_empty) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_opt_check_permission'
  logical, intent(in), optional :: allow_empty

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(allow_empty) )then
    opt_check_permission__allow_empty = allow_empty
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_opt_check_permission
!===============================================================
!
!===============================================================
integer(4) function init_opt_check_permission(key) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_opt_check_permission'
  character(*), intent(in) :: key

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( key )
  case( 'allow_empty' )
    opt_check_permission__allow_empty = OPT_DEFAULT_CHECK_PERMISSION__ALLOW_EMPTY
  case default
    info = 1
    call errret(msg_invalid_value('key', key))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_opt_check_permission
!===============================================================
!
!===============================================================
integer(4) function check_permission__file(&
    f, action, id, allow_empty) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_permission__file'
  type(file_) , intent(in) :: f
  character(*), intent(in), optional :: action
  character(*), intent(in), optional :: id
  logical     , intent(in), optional :: allow_empty

  character(clen_key) :: action_
  character(:), allocatable :: id_
  logical :: allow_empty_

  character(clen_var), parameter :: id_default = 'f%path'

  integer :: cl

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  if( present(action) )then
    selectcase( action )
    case( action_read, &
          action_write, &
          action_readwrite, &
          action_undef )
      action_ = action
    case default
      info = 1
      call errret(msg_invalid_value('action', action))
      return
    endselect
  else
    selectcase( f%action )
    case( action_read, &
          action_write, &
          action_readwrite, &
          action_undef )
      action_ = f%action
    case default
      info = 1
      call errret(msg_invalid_value()//&
                '\n  action: '//str(action)//&
                '\n  id: '//str(f%id)//&
                '\n  path: '//str(f%path))
      return
    endselect
  endif

  if( present(id) )then
    cl = len(id)
    allocate(character(cl) :: id_)
    id_ = id
  else
    if( f%id == '' )then
      cl = len_trim(id_default)
      allocate(character(cl) :: id_)
      id_ = trim(id_default)
    else
      cl = len_trim(f%id)
      allocate(character(cl) :: id_)
      id_ = trim(f%id)
    endif
  endif

  allow_empty_ = opt_check_permission__allow_empty
  if( present(allow_empty) ) allow_empty_ = allow_empty
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    if( .not. allow_empty_ )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nf%path == "" .and. .not. allow_empty'//&
                '\n  id: '//str(f%id))
    endif
    call logret(PRCNAM, MODNAM)
    return
  endif

  selectcase( action_ )
  case( action_undef )
    continue
  case( action_read )
    if( access(f%path,' ') == 0 )then
      if( access(f%path,'r') /= 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nNo read permission for "'//str(f%path)//'".')
        return
      endif
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nFile does not exist: "'//str(f%path)//'".'//&
                '\n  id: '//str(id_))
      return
    endif
  case( action_write )
    if( access(f%path,' ') == 0 )then
      if( access(f%path,'w') /= 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nNo write permission for "'//str(f%path)//'".')
        return
      endif
    else
      if( try_make_empty_file(f) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  case( action_readwrite )
    if( access(f%path,' ') == 0 )then
      if( access(f%path,'r') /= 0 .or. access(f%path,'w') /= 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nNo readwrite permission for "'//str(f%path)//'".')
        return
      endif
    else
      if( try_make_empty_file(f) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  case default
    info = 1
    call errret(msg_invalid_value('action', action_))
    return
  endselect
  !-------------------------------------------------------------
  deallocate(id_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_permission__file
!===============================================================
!
!===============================================================
integer(4) function check_permission__path(&
    path, action, id, allow_empty) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_permission__path'
  character(*), intent(in) :: path
  character(*), intent(in) :: action
  character(*), intent(in) , optional :: id
  logical     , intent(in) , optional :: allow_empty

  character(:), allocatable :: id_
  logical :: allow_empty_
  integer :: cl
  integer :: ios
  integer :: un

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  allow_empty_ = opt_check_permission__allow_empty
  if( present(allow_empty) ) allow_empty_ = allow_empty

  if( present(id) )then
    cl = len_trim(id)
    allocate(character(cl) :: id_)
    id_ = trim(id)
  else
    allocate(character(16) :: id_)
    id_ = id_undef
  endif
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  if( path == '' )then
    if( .not. allow_empty_ )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nPath is an empty string.'//&
                '\n  id: '//str(id_))
      return
    endif

    call finalize()
    call logret(PRCNAM, MODNAM)
    return
  endif

  selectcase( action )
  case( action_read )
    if( access(path, ' ') == 0 )then
      if( access(path, 'r') /= 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nNo read permission for "'//str(path)//'".'//&
                  '\n  id: '//str(id_))
        call finalize()
        return
      endif
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nFile does not exist: "'//str(path)//'".'//&
                '\n  id: '//str(id_))
      call finalize()
      return
    endif
  case( action_write )
    if( access(path, ' ') == 0 )then
      if( access(path, 'w') /= 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nNo write permission for "'//str(path)//'".'//&
                  '\n  id: '//str(id_))
        call finalize()
        return
      endif
    else
      un = unit_number()
      open(un, file=path, status='new', iostat=ios)
      if( ios /= 0 )then
        info = 1
        call errret(msg_io_error()//&
                  '\nFailed to make a new file: "'//str(path)//'".'&
                  '\nCheck if the directory exists and you have a write permission.'//&
                  '\n  id: '//str(id_))
        call finalize()
        return
      endif
      close(un, status='delete')
    endif
  case( action_readwrite )
    if( access(path, ' ') == 0 )then
      if( access(path, 'r') /= 0 .or. access(path, 'w') /= 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nNo readwrite permission for "'//str(path)//'".'//&
                  '\n  id: '//str(id_))
        call finalize()
        return
      endif
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nFile does not exist: "'//str(path)//'".'//&
                '\n  id: '//str(id_))
      call finalize()
      return
    endif
  case default
    info = 1
    call errret(msg_invalid_value()//&
              '\n  action: '//str(action)//&
              '\n  path  : '//str(path)//&
              '\n  id    : '//str(id_))
    call finalize()
    return
  endselect

  call finalize()
  !------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!--------------------------------------------------------------
contains
!--------------------------------------------------------------
subroutine finalize()
  implicit none

  deallocate(id_)
end subroutine finalize
!--------------------------------------------------------------
end function check_permission__path
!===============================================================
!
!===============================================================
integer(4) function check_file_size(&
    f, allow_empty, allow_not_multiple) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_file_size'
  type(file_), intent(in) :: f
  logical, intent(in), optional :: allow_empty
  logical, intent(in), optional :: allow_not_multiple

  logical :: allow_empty_
  logical :: allow_not_multiple_

  integer :: d
  integer(8) :: fs
  integer(8) :: recl

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  allow_empty_ = .false.
  if( present(allow_empty) ) allow_empty_ = allow_empty

  allow_not_multiple_ = .false.
  if( present(allow_not_multiple) ) allow_not_multiple_ = allow_not_multiple
  !-------------------------------------------------------------
  ! Exceptions
  !-------------------------------------------------------------
  if( f%path == '' )then
    if( allow_empty_ )then
      call logret(); return
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nPath is an empty string.'//&
                '\n  id: '//str(f%id))
      return
    endif
  endif

  if( access(f%path, ' ') /= 0 )then
    if( allow_empty_ )then
      call logret(); return
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nFile does not exist: "'//str(f%path)//'".'//&
                '\n  id: '//str(f%id))
      return
    endif
  endif

  selectcase( f%dtype )
  case( DTYPE_INT1, &
        DTYPE_INT2, &
        DTYPE_INT4, &
        DTYPE_INT8, &
        DTYPE_REAL, &
        DTYPE_DBLE )
    continue
  case( DTYPE_UNDEF )
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nf%dtype == DTYPE_UNDEF'//&
              '\n  id: '//str(f%id))
    return
  case default
    info = 1
    call errret(msg_invalid_value('f%dtype', f%dtype)//&
              '\n  id: '//str(f%id))
    return
  endselect

  selectcase( f%rec )
  case( 1: )
    continue
  case( REC_UNDEF )
    continue
  case default
    info = 1
    call errret(msg_invalid_value('f%rec', f%rec)//&
              '\n  id: '//str(f%id))
    return
  endselect

  if( f%length <= 0_8 )then
    info = 1
    call errret(msg_invalid_value('f%length', f%length)//&
              '\n  id: '//str(f%id))
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fs = filesize(f%path)

  recl = byte_of_dtype(f%dtype) * f%length

  if( f%rec /= rec_undef )then
    if( recl*f%rec > fs )then
      d = dgt(recl*f%rec)

      info = 1
      call errret(msg_unexpected_condition()//&
                '\nrecl * rec > fs'//&
                '\nExpected file size exceeds the actual file size.'//&
                '\n  id      : '//str(f%id)//&
                '\n  recl    : '//str(recl,d)//' (recl = byte * length)'//&
                '\n    dtype : '//str(f%dtype)//' (byte: '//str(byte_of_dtype(f%dtype))//')'//&
                '\n    length: '//str(f%length,d)//&
                '\n  rec     : '//str(f%rec,d)//&
                '\n  fs      : '//str(fs,d))
      return
    endif
  endif

  if( .not. allow_not_multiple_ )then
    if( mod(fs, recl) /= 0_8 )then
      d = dgt((/fs,recl/),dgt_opt_max)

      info = 1
      call errret(msg_unexpected_condition()//&
                '\nmod(fs, recl) /= 0'//&
                '\nThe file size (fs) is not a multiple of record length (recl).'//&
                '\n  id      : '//str(f%id)//&
                '\n  fs      : '//str(fs,d)//&
                '\n  recl    : '//str(recl,d)//' (recl = byte * length)'//&
                '\n    dtype : '//str(f%dtype)//' (byte: '//str(byte_of_dtype(f%dtype))//')'//&
                '\n    length: '//str(f%length,d))
      return
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_file_size
!===============================================================
!
!===============================================================
integer(4) function try_make_empty_file__file(f) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'try_make_empty_file__file'
  type(file_), intent(in) :: f

  integer :: un
  integer :: ios

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( f%path == '' )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nPath is an empty string.'//&
              '\n  id: '//str(f%id))
    return
  endif

  if( access(f%path,' ') == 0 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nFile already exists: '//str(f%path)//&
              '\n  id: '//str(f%id))
    return
  endif

  un = unit_number()
  open(un, file=f%path, status='new', iostat=ios)
  if( ios /= 0 )then
    info = 1
    call errret(msg_io_error()//&
              '\nFailed to make an empty file: '//str(f%path)//&
              '\n  id: '//str(f%id))
    return
  endif

  close(un, status='delete')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function try_make_empty_file__file
!===============================================================
!
!===============================================================
integer(4) function try_make_empty_file__dir(dir) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'try_make_empty_file__dir'
  character(*), intent(in) :: dir

  character(len_trim(dir)+32) :: f
  integer, parameter :: IMAX = 1000000
  integer, parameter :: NTRY = 5
  integer :: i
  integer :: iTry

  integer :: un
  integer :: ios
  logical :: opened, exist

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do i = 1, IMAX
    f = joined(dir,'spring.empty.'//str(i))
    if( access(f,' ') == 0 ) cycle

    open(newunit=un, file=f, status='new', iostat=ios)
    if( ios /= 0 ) cycle

    do iTry = 1, NTRY
      close(un, status='delete', iostat=ios)
      if( ios == 0 ) exit

      inquire(file=f, opened=opened, exist=exist)
      if( .not. opened .and. .not. exist )then
        call logwrn(msg_io_error()//&
            '\nFailed to delete the temporal file: '//str(f)//&
            '\nThe file does not already exist.')
        ios = 0
        exit
      elseif( opened .and. exist )then
        if( iTry < NTRY )then
          call logwrn('Failed to remove the temporal file. Trying again...')
        else
          info = 1
          call errret(msg_io_error()//&
              '\nFailed to remove the temporal file: '//str(f))
          return
        endif
      elseif( .not. opened .and. exist )then
        call logwrn(msg_io_error()//&
            '\nFailed to remove the temporal file: '//str(f)//&
            '\nThe file exists and has been closed now.')
        exit
      elseif( opened .and. .not. exist )then
        info = 1
        call logerr(msg_io_error()//&
                  '\nUnknown I/O error.'//&
                  '\n  file: '//str(f)//&
                  '\n  opened: '//str(opened)//&
                  '\n  exist: '//str(exist))
        return
      endif
    enddo

    if( ios == 0 ) exit

    if( i == imax )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nReached the upper limit of $i.')
      return
    endif
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function try_make_empty_file__dir
!===============================================================
!
!===============================================================
integer(4) function set_opt_mkdir(output, hut) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_opt_mkdir'
  logical     , intent(in), optional :: output
  character(*), intent(in), optional :: hut

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( present(output) )then
    opt_mkdir__output = output
  endif

  if( present(hut) )then
    opt_mkdir__clen_hut = len(hut)
    opt_mkdir__hut = hut
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_opt_mkdir
!===============================================================
!
!===============================================================
integer(4) function init_opt_mkdir(key) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_opt_mkdir'
  character(*), intent(in) :: key

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  selectcase( key )
  case( 'output' )
    opt_mkdir__output = opt_default_mkdir__output
  case( 'hut' )
    opt_mkdir__hut = opt_default_mkdir__hut
  case default
    info = 1
    call errret(msg_invalid_value('key', key))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_opt_mkdir
!===============================================================
!
!===============================================================
integer(4) function mkdir(dir, output, hut) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'mkdir'
  character(*), intent(in)           :: dir
  logical     , intent(in), optional :: output
  character(*), intent(in), optional :: hut

  type hist_
    character(clen_path) :: dir
    integer :: order
  end type

  logical :: output_
  character(:), allocatable :: hut_
  character(len_trim(dir)+9) :: command
  integer, parameter :: nmax = 30
  integer, save      :: n = 0
  integer            :: i
  integer            :: i_oldest
  type(hist_), allocatable, save :: hist(:)
  logical, save :: is_first = .true.

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  selectcase( adjustl(dir) )
  case( '', './' )
    call logret(); return
  endselect
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  if( is_first )then
    is_first = .false.
    allocate(hist(nmax))
    hist(:)%dir = ''
    hist(:)%order = 0
  endif
  !------------------------------------------------------------
  ! Options
  !------------------------------------------------------------
  if( present(output) )then
    output_ = output
  else
    output_ = opt_mkdir__output
  endif

  allocate(character(1) :: hut_)
  if( present(hut) )then
    hut_ = hut
  else
    if( opt_mkdir__clen_hut > 0 )then
      hut_ = opt_mkdir__hut(1:opt_mkdir__clen_hut)
    else
      hut_ = ''
    endif
  endif
  !------------------------------------------------------------
  ! Check if already made
  !------------------------------------------------------------
  do i = 1, n
    if( hist(i)%dir == dir )then
      call finalize(); call logret(); return
    endif
  enddo
  !------------------------------------------------------------
  ! Update history
  !------------------------------------------------------------
  selectcase( n )
  case( 0:nmax-1 )
    n = n + 1
    hist(n)%dir = dir
    hist(n)%order = n
  case( nmax )
    i_oldest = 0
    do i = 1, n
      hist(i)%order = hist(i)%order - 1
      if( hist(i)%order == 0 )then
        i_oldest = i
      endif
    enddo

    if( i_oldest == 0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n$i_oldest == 0')
      call finalize()
      return
    endif

    hist(i_oldest)%dir = dir
    hist(i_oldest)%order = n
  case default
    info = 1
    call errret(msg_unexpected_condition()//&
              '\n$n == '//str(n))
    call finalize()
    return
  endselect
  !------------------------------------------------------------
  ! Make
  !------------------------------------------------------------
  command = 'mkdir -p '//str(dir)
  if( output_ )then
    call logmsg(hut_//str(command))
  endif

  call execute_command_line(str(command))
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  call finalize()
  !------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine finalize()
  implicit none

  deallocate(hut_)
end subroutine finalize
!---------------------------------------------------------------
end function mkdir
!===============================================================
!
!===============================================================
integer(4) function remove(path, dir, output) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remove'
  character(*), intent(in)           :: path
  logical     , intent(in), optional :: dir
  logical     , intent(in), optional :: output
  logical :: dir_
  logical :: output_

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !------------------------------------------------------------
  ! Options
  !------------------------------------------------------------
  dir_  = .false.
  output_ = .false.
  if( present(dir)    ) dir_    = dir
  if( present(output) ) output_ = output
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  if( path /= '' )then
    if( dir_ )then
      if( output_ )then
        if( path(len_trim(path):len_trim(path)) == '/' )then
          call logmsg('Remove '//str(path))
        else
          call logmsg('Remove '//str(path)//'/')
        endif
      endif
      call execute_command_line('rm -rf '//str(path))
    else
      if( output_ )then
        call logmsg('Remove '//str(path))
      endif
      call execute_command_line('rm -f '//str(path))
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function remove
!===============================================================
!
!===============================================================
integer(4) function make_empty_file(path, remove) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_empty_file'
  character(*), intent(in) :: path
  logical     , intent(in) , optional :: remove

  logical :: remove_
  integer :: un
  integer :: ios

  integer, parameter :: NTRY = 10
  integer :: iTry
  logical :: opened, exist

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  remove_ = .false.
  if( present(remove) ) remove_ = remove
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( path == '' )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nPath is an empty string.')
    return
  endif

  open(newunit=un, file=path, status='replace', iostat=ios)

  if( ios == 0 )then
    if( remove_ )then
      do iTry = 1, NTRY
        close(un, status='delete', iostat=ios)
        if( ios == 0 ) exit

        inquire(file=path, opened=opened, exist=exist)
        if( .not. opened .and. .not. exist )then
          call logwrn('Deletion received iostat of failure but the file has been removed.')
          ios = 0
          exit
        elseif( opened .neqv. exist )then
          info = 1
          call logerr(msg_io_error()//&
                    '\nUnknown I/O error.'//&
                    '\n  file: '//str(path)//&
                    '\n  opened: '//str(opened)//&
                    '\n  exist: '//str(exist))
          return
        endif

        call logmsg('Failed to remove the temporal file. Trying again...')
      enddo

      if( ios /= 0 )then
        info = 1
        call errret(msg_io_error()//&
                  '\nFailed to remove the temporal file: '//str(path))
        return
      endif
    else
      close(un)
    endif
  else
    info = 1
    call errret(msg_io_error()//&
              '\nFailed to make an empty file: '//str(path))
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function make_empty_file
!===============================================================
!
!===============================================================
end module lib_io_file
