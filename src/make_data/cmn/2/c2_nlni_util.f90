module c2_nlni_util
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use c1_const
  use c2_nlni_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: is_wsCode_temporal
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface is_wsCode_temporal
    module procedure is_wsCode_temporal__char
    module procedure is_wsCode_temporal__int4
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_nlni_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
logical function is_wsCode_temporal__char(wsCode) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_wsCode_temporal__char'
  character(*), intent(in) :: wsCode

  if( len_trim(wsCode) /= DGT_WSCODE )then
    call errend(msg_unexpected_condition()//&
        '\n  len_trim(wsCode) /= DGT_WSCODE')
  endif

  res = wsCode(4:6) == '000'
end function is_wsCode_temporal__char
!===============================================================
!
!===============================================================
logical function is_wsCode_temporal__int4(wsCode) result(res)
  implicit none
  integer, intent(in) :: wsCode

  res = mod(wsCode, 100) == 0
end function is_wsCode_temporal__int4
!===============================================================
!
!===============================================================
end module c2_nlni_util
