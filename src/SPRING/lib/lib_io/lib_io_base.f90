module lib_io_base
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: unit_number

  public :: byte_of_dtype

  public :: dtype_int
  public :: dtype_float

  public :: int_endian
  public :: endian_name_long
  public :: endian_name_short
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_base'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function unit_number() result(un)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'unit_number'
  integer :: un
  logical :: is_opened

  un = 21
  do while( un <= 9999 )
    inquire(unit=un, opened=is_opened)
    if( .not. is_opened ) exit
    un = un + 1
  enddo

  if( is_opened )then
    call errend('Unopened unit number was not found.', &
                PRCNAM, MODNAM)
    stop 1
  endif
end function unit_number
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
integer function byte_of_dtype(dtype) result(byte)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'byte_of_dtype'
  character(*), intent(in)  :: dtype

  selectcase( dtype )
  case( DTYPE_INT1 )
    byte = 1
  case( DTYPE_INT2 )
    byte = 2
  case( DTYPE_INT4 )
    byte = 4
  case( DTYPE_INT8 )
    byte = 8
  case( DTYPE_REAL )
    byte = 4
  case( DTYPE_DBLE )
    byte = 8
  case( DTYPE_UNDEF )
    byte = 0
  case default
    call errend(msg_invalid_value('dtype', dtype), &
                PRCNAM, MODNAM) 
    stop
  endselect
end function byte_of_dtype
!===============================================================
!
!===============================================================





!===============================================================
!
!===============================================================
character(CLEN_KEY) function dtype_int(byte) result(dtype)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dtype_int'
  integer, intent(in) :: byte

  selectcase( byte )
  case( 1 )
    dtype = DTYPE_INT1
  case( 2 )
    dtype = DTYPE_INT2
  case( 4 )
    dtype = DTYPE_INT4
  case( 8 )
    dtype = DTYPE_INT8
  case default
    call errend(msg_invalid_value('byte', byte), &
                PRCNAM, MODNAM) 
    stop
  endselect
end function dtype_int
!===============================================================
!
!===============================================================
character(CLEN_KEY) function dtype_float(byte) result(dtype)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dtype_float'
  integer, intent(in) :: byte

  selectcase( byte )
  case( 4 )
    dtype = DTYPE_REAL
  case( 8 )
    dtype = DTYPE_DBLE
  case default
    call errend(msg_invalid_value('byte', byte), &
                PRCNAM, MODNAM) 
    stop
  endselect
end function dtype_float
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
integer function int_endian(endian) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'int_endian'
  character(*), intent(in)  :: endian

  selectcase( lower(endian) )
  case( ENDIAN_LITTLE, &
        ENDIAN_LITTLE_SHORT )
    res = INT_ENDIAN_LITTLE
  case( ENDIAN_BIG, &
        ENDIAN_BIG_SHORT )
    res = INT_ENDIAN_BIG
  case default
    call errend(msg_invalid_value('endian', endian), &
                PRCNAM, MODNAM)
    stop
  endselect
end function int_endian
!===============================================================
!
!===============================================================
character(CLEN_KEY) function endian_name_long(endian) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'endian_name_long'
  character(*), intent(in)  :: endian

  selectcase( endian )
  case( ENDIAN_LITTLE, &
        ENDIAN_LITTLE_SHORT )
    res = ENDIAN_LITTLE
  case( ENDIAN_BIG, &
        ENDIAN_BIG_SHORT )
    res = ENDIAN_BIG
  case default
    call errend(msg_invalid_value('endian', endian), &
                PRCNAM, MODNAM)
    stop
  endselect
end function endian_name_long
!===============================================================
!
!===============================================================
character(CLEN_KEY) function endian_name_short(endian) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'endian_name_short'
  character(*), intent(in) :: endian

  selectcase( endian )
  case( ENDIAN_LITTLE, &
        ENDIAN_LITTLE_SHORT )
    res = ENDIAN_LITTLE_SHORT
  case( ENDIAN_BIG, &
        ENDIAN_BIG_SHORT )
    res = ENDIAN_BIG_SHORT
  case default
    call errend(msg_invalid_value('endian', endian), &
                PRCNAM, MODNAM)
    stop
  endselect
end function endian_name_short
!===============================================================
!
!===============================================================
end module lib_io_base
