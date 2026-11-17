module c2_jflw_util
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_id_range

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
subroutine get_id_range(id_range_in, idmin_int, idmax_int)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_id_range'
  character(*), intent(in) :: id_range_in
  integer, intent(out) :: idmin_int, idmax_int

  integer :: loc

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  loc = index(id_range_in, '-')
  if( loc == 0 )then
    idmin_int = int4_char(id_range_in)
    idmax_int = idmin_int
  elseif( loc == 1 )then
    idmin_int = 1
    idmax_int = int4_char(id_range_in(2:))
  elseif( loc == len_trim(id_range_in) )then
    call errend(ERROR_CODE__SYNTAX, &
      'Value must be given after the hyphen: '//str(id_range_in))
  else
    idmin_int = int4_char(id_range_in(:loc-1))
    idmax_int = int4_char(id_range_in(loc+1:))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_id_range
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
integer(1) function get_miss_int1(varname) result(res)
  use c2_jflw_const
  implicit none
  character(*), intent(in) :: varname

  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss_int1'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'dir' )
    res = FDR__MISS
  case default
    call errend(ERROR_CODE__VALUE, 'varname == '//str(varname))
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
    call errend(ERROR_CODE__VALUE, 'varname == '//str(varname))
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
    call errend(ERROR_CODE__VALUE, 'varname == '//str(varname))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_miss_real
!===============================================================
!
!===============================================================
end module c2_jflw_util
