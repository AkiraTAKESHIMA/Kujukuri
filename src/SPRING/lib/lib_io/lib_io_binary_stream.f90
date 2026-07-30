module lib_io_binary_stream
  use lib_const
  use lib_log
  use lib_io_base, only: &
    unit_number, &
    byte_of_dtype, &
    endian_long_name
  use lib_io_file, only: &
    filesize, &
    check_permission, &
    make_empty_file, &
    remove
  use lib_io_binary_common, only: &
    open_input_file_stream, &
    open_output_file_stream, &
    close_file, &
    action_for_replace
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: rbin_stream
  public :: wbin_stream
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface rbin_stream
    module procedure :: rb__dble_1d
  end interface

  interface wbin_stream
    module procedure :: wb__dble_1d
  end interface
  !-------------------------------------------------------------
  ! Pirvate module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_binary_stream'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function rb__dble_1d(&
    dat, path, dtype, endian, pos) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__dble_1d'
  real(8)     , intent(out) :: dat(:)
  character(*), intent(in)  :: path
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer(8)  , intent(in) , optional :: pos

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer(8) :: pos_

  integer :: byte
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dtype_ = dtype_dble
  endian_ = endian_default
  pos_ = 1

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian
  if( present(pos) ) pos_ = pos

  byte = byte_of_dtype(dtype_)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(path, dtype_, size(dat,kind=8), pos_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, path, endian_) /= 0 )then
    info = 1; call errret(); return
  endif

  selectcase( dtype_ )
  case( DTYPE_INT4 )
    dat = real(rb_core__int4_1d(un, pos_, shape(dat,kind=8)), 8)
  case( DTYPE_DBLE )
    dat = rb_core__dble_1d(un, pos_, shape(dat,kind=8))
  endselect

  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__dble_1d
!===============================================================
!
!===============================================================
integer(4) function wb__dble_1d(&
    dat, path, dtype, endian, pos, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__dble_1d'
  real(8)     , intent(in) :: dat(:)
  character(*), intent(in) :: path
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer(8)  , intent(in) , optional :: pos
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer(8) :: pos_
  logical    :: replace_

  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dtype_ = dtype_dble
  endian_ = endian_default
  replace_ = .false.
  pos_ = 1_8

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(replace) ) replace_ = replace
  if( present(pos) ) pos_ = pos
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(path, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, path, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif

  selectcase( dtype_ )
  case( DTYPE_INT4 )
    if( wb_core__int4_1d(int(dat,4), un, pos_) /= 0 )then
      info = 1; call errret(); return
    endif
  case( DTYPE_DBLE )
    if( wb_core__dble_1d(dat, un, pos_) /= 0 )then
      info = 1; call errret(); return
    endif
  endselect

  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__dble_1d
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
integer(4) function check_input_file(&
    path, dtype, length, pos) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input_file'
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: length
  integer(8)  , intent(in)  :: pos

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( check_permission(path, action_read, allow_empty=.false.) /= 0 )then
    info = 1; call errret(); return
  endif

  if( check_input_filesize(path, dtype, length, pos) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_input_file
!===============================================================
!
!===============================================================
integer(4) function prep_output_file(path, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_output_file'
  character(*), intent(in)  :: path
  logical     , intent(in)  :: replace

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  if( access(path,' ') == 0 )then
    if( check_permission(&
           path, action_for_replace(replace), &
           allow_empty=.false.) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( replace .and. access(path,' ') == 0 )then
    if( remove(path, dir=.false., output=.false.) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( access(path,' ') /= 0 )then
    if( make_empty_file(path, .false.) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function prep_output_file
!===============================================================
!
!===============================================================
integer(4) function check_input_filesize(&
    path, dtype, length, pos) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input_filesize'
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: length
  integer(8)  , intent(in)  :: pos

  integer(8) :: fs
  integer    :: byte
  integer(8) :: fs_min

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  fs = filesize(path)
  byte = byte_of_dtype(dtype)
  fs_min = byte * length + pos - 1

  if( fs < byte_of_dtype(dtype) * length + pos - 1 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nFile size is invalid.'//&
              '\n  Path: '//str(path)//&
              '\n  File size                 : '//str(fs)//&
              '\n  Starting position         : '//str(pos)//&
              '\n  Data size                 : '//str(byte*length)//&
              '\n  Expected minumum file size: '//str(fs_min))
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_input_filesize
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
function rb_core__int4_1d(un, pos, shp) result(dat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb_core__int4_1d'
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos
  integer(8), intent(in) :: shp(1)
  integer(4) :: dat(shp(1))

  read(un, pos=pos) dat
end function rb_core__int4_1d
!===============================================================
!
!===============================================================
function rb_core__dble_1d(un, pos, shp) result(dat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb_core__dble_1d'
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos
  integer(8), intent(in) :: shp(1)
  real(8) :: dat(shp(1))

  read(un, pos=pos) dat
end function rb_core__dble_1d
!===============================================================
!
!===============================================================
integer(4) function wb_core__int4_1d(dat, un, pos) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb_core__int4_1d'
  integer(4), intent(in) :: dat(:)
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos

  write(un, pos=pos) dat
end function wb_core__int4_1d
!===============================================================
!
!===============================================================
integer(4) function wb_core__dble_1d(dat, un, pos) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb_core__dble_1d'
  real(8)   , intent(in) :: dat(:)
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos

  write(un, pos=pos) dat
end function wb_core__dble_1d
!===============================================================
!
!===============================================================
end module lib_io_binary_stream
