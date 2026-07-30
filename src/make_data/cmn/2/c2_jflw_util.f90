module c2_jflw_util
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_miss_int1
  public :: get_miss_int4
  public :: get_miss_real
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_jflw_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(1) function get_miss_int1(varname) result(res)
  use c2_jflw_const
  implicit none
  character(*), intent(in) :: varname

  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss_int1'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'dir' )
    res = FDR_MISS
  case default
    call errend(msg_invalid_value('varname', varname))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_miss_int1
!===============================================================
!
!===============================================================
integer(4) function get_miss_int4(varname) result(res)
  use c2_jflw_const
  implicit none
  character(*), intent(in) :: varname

  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss_int4'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'upg' )
    res = UPG_MISS
  case( 'bsn' )
    res = BSN_MISS
  case default
    call errend(msg_invalid_value('varname', varname))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_miss_int4
!===============================================================
!
!===============================================================
real(4) function get_miss_real(varname) result(res)
  use c2_jflw_const
  implicit none
  character(*), intent(in) :: varname

  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss_int4'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'elv' )
    res = ELV_MISS
  case( 'upa' )
    res = UPA_MISS
  case( 'wth' )
    res = WTH_MISS
  case default
    call errend(msg_invalid_value('varname', varname))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_miss_real
!===============================================================
!
!===============================================================
end module c2_jflw_util
