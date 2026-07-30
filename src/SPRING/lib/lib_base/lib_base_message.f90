module lib_base_message
  use lib_const, only: &
        MAXDGT_INT1, &
        MAXDGT_INT2, &
        MAXDGT_INT4, &
        MAXDGT_INT8
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: msg_invalid_value
  public :: msg_unexpected_condition
  public :: msg_syntax_error
  public :: msg_io_error
  public :: msg_internal_error
  public :: msg_not_implemented
  public :: msg_undesirable_condition
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface msg_invalid_value
    module procedure msg_invalid_value__none
    module procedure msg_invalid_value__i1
    module procedure msg_invalid_value__i2
    module procedure msg_invalid_value__i4
    module procedure msg_invalid_value__i8
    module procedure msg_invalid_value__r4
    module procedure msg_invalid_value__r8
    module procedure msg_invalid_value__l1
    module procedure msg_invalid_value__l2
    module procedure msg_invalid_value__l4
    module procedure msg_invalid_value__l8
    module procedure msg_invalid_value__char
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function msg_invalid_value__none() result(res)
  implicit none
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = 'Invalid value.'
end function msg_invalid_value__none
!===============================================================
!
!===============================================================
function msg_invalid_value__i1(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: val
  character(:), allocatable :: res

  character(MAXDGT_INT1) :: c

  allocate(character(1) :: res)
  write(c,"(i0)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__i1
!===============================================================
!
!===============================================================
function msg_invalid_value__i2(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  integer(2)  , intent(in) :: val
  character(:), allocatable :: res

  character(MAXDGT_INT2) :: c

  allocate(character(1) :: res)
  write(c,"(i0)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__i2
!===============================================================
!
!===============================================================
function msg_invalid_value__i4(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  integer(4)  , intent(in) :: val
  character(:), allocatable :: res

  character(MAXDGT_INT4) :: c

  allocate(character(1) :: res)
  write(c,"(i0)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__i4
!===============================================================
!
!===============================================================
function msg_invalid_value__i8(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: val
  character(:), allocatable :: res

  character(MAXDGT_INT8) :: c

  allocate(character(1) :: res)
  write(c,"(i0)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__i8
!===============================================================
!
!===============================================================
function msg_invalid_value__r4(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  real(4)     , intent(in) :: val
  character(:), allocatable :: res

  character(12) :: c

  allocate(character(1) :: res)
  write(c,"(es12.5)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__r4
!===============================================================
!
!===============================================================
function msg_invalid_value__r8(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  real(8)     , intent(in) :: val
  character(:), allocatable :: res

  character(20) :: c

  allocate(character(1) :: res)
  write(c,"(es20.13)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__r8
!===============================================================
!
!===============================================================
function msg_invalid_value__l1(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  logical(1)  , intent(in) :: val
  character(:), allocatable :: res

  character(1) :: c

  allocate(character(1) :: res)
  write(c,"(l1)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__l1
!===============================================================
!
!===============================================================
function msg_invalid_value__l2(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  logical(2)  , intent(in) :: val
  character(:), allocatable :: res

  character(1) :: c

  allocate(character(1) :: res)
  write(c,"(l1)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__l2
!===============================================================
!
!===============================================================
function msg_invalid_value__l4(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  logical(4)  , intent(in) :: val
  character(:), allocatable :: res

  character(1) :: c

  allocate(character(1) :: res)
  write(c,"(l1)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__l4
!===============================================================
!
!===============================================================
function msg_invalid_value__l8(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  logical(8)  , intent(in) :: val
  character(:), allocatable :: res

  character(1) :: c

  allocate(character(1) :: res)
  write(c,"(l1)") val
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(c)
end function msg_invalid_value__l8
!===============================================================
!
!===============================================================
function msg_invalid_value__char(nam, val) result(res)
  implicit none
  character(*), intent(in) :: nam
  character(*), intent(in) :: val
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = 'Invalid value in `'//trim(nam)//'`: '//trim(val)
end function msg_invalid_value__char
!===============================================================
!
!===============================================================
function msg_unexpected_condition() result(res)
  implicit none
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = 'Unexpected condition.'
end function msg_unexpected_condition
!===============================================================
!
!===============================================================
function msg_syntax_error() result(res)
  implicit none
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = 'Syntax error.'
end function msg_syntax_error
!===============================================================
!
!===============================================================
function msg_io_error(i, f) result(res)
  implicit none
  integer(4)  , intent(in), optional :: i
  character(*), intent(in), optional :: f
  character(:), allocatable :: res

  character(MAXDGT_INT8) :: ci

  allocate(character(1) :: res)

  if( present(i) )then
    write(ci,"(i0)") i
    if( present(f) )then
      res = 'I/O error @ line '//trim(ci)//' in file "'//trim(f)//'".'
    else
      res = 'I/O error @ line '//trim(ci)//'.'
    endif
  else
    if( present(f) )then
      res = 'I/O error in file "'//trim(f)//'".'
    else
      res = 'I/O error.'
    endif
  endif
end function msg_io_error
!===============================================================
!
!===============================================================
function msg_internal_error() result(res)
  implicit none
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = '!!!!!! INTERNAL ERROR !!!!!!'
end function msg_internal_error
!===============================================================
!
!===============================================================
function msg_not_implemented() result(res)
  implicit none
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = 'Not implemented yet.'
end function msg_not_implemented
!===============================================================
!
!===============================================================
function msg_undesirable_condition() result(res)
  implicit none
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = 'Undesirable condition.'
end function msg_undesirable_condition
!===============================================================
!
!===============================================================
end module lib_base_message
