module lib_math_unit
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: conv_unit
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface conv_unit
    module procedure conv_unit_self_dble_1d
    module procedure conv_unit_self_dble_2d
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_math_unit'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function conv_unit_self_dble_1d(d, uin, uout) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'conv_unit_self_dble_1d'
  real(8), intent(inout)  :: d(:)
  character(*), intent(in) :: uin
  character(*), intent(in) :: uout

  logical :: is_ok

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( uin == uout )then
    call logret(); return
  endif

  is_ok = .true.

  selectcase( uin )
  case( UNIT_SQUARE_METER )
    selectcase( uout )
    case( UNIT_SQUARE_METER )
      ! uin == uout
    case( UNIT_SQUARE_KILOMETER )
      d = d * 1d-6
    case default
      is_ok = .false.
    endselect
  case( UNIT_SQUARE_KILOMETER )
    selectcase( uout )
    case( UNIT_SQUARE_METER )
      d = d * 1d6
    case( UNIT_SQUARE_KILOMETER )
      ! uin == uout
    case default
      is_ok = .false.
    endselect
  case default
    is_ok = .false.
  endselect

  if( .not. is_ok )then
    info = 1
    call errret(msg_invalid_value()//&
              '\n  $uin : '//str(uin)//&
              '\n  $uout: '//str(uout))
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function conv_unit_self_dble_1d
!===============================================================
!
!===============================================================
integer(4) function conv_unit_self_dble_2d(d, uin, uout) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'conv_unit_self_dble_2d'
  real(8), intent(inout)  :: d(:,:)
  character(*), intent(in) :: uin
  character(*), intent(in) :: uout

  logical :: is_ok

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( uin == uout )then
    call logret(); return
  endif

  is_ok = .true.

  selectcase( uin )
  case( UNIT_SQUARE_METER )
    selectcase( uout )
    case( UNIT_SQUARE_METER )
      ! uin == uout
    case( UNIT_SQUARE_KILOMETER )
      d = d * 1d-6
    case default
      is_ok = .false.
    endselect
  case( UNIT_SQUARE_KILOMETER )
    selectcase( uout )
    case( UNIT_SQUARE_METER )
      d = d * 1d6
    case( UNIT_SQUARE_KILOMETER )
      ! uin == uout
    case default
      is_ok = .false.
    endselect
  case default
    is_ok = .false.
  endselect

  if( .not. is_ok )then
    info = 1
    call errret(msg_invalid_value()//&
              '\n  $uin : '//str(uin)//&
              '\n  $uout: '//str(uout))
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function conv_unit_self_dble_2d
!===============================================================
!
!===============================================================
end module lib_math_unit
