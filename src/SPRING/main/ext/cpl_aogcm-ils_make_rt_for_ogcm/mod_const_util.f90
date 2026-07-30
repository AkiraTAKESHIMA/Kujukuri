module mod_const_util
  use lib_const
  use lib_base
  use lib_log
  use def_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: checkval_method_rivwat
  !-------------------------------------------------------------
  ! Pivate module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_const_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine checkval_method_rivwat(s, id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'checkval_method_rivwat'
  character(*), intent(in) :: s
  character(*), intent(in), optional :: id

  character(:), allocatable :: id_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: id_)
  id_ = 'method_rivwat'
  if( present(id) ) id_ = trim(id)

  selectcase( s )
  case( METHOD_RIVWAT__DIST, &
        METHOD_RIVWAT__WEIGHTED_DIST )
    continue
  case default
    call errend(msg_invalid_value(id_, s))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine checkval_method_rivwat
!===============================================================
!
!===============================================================
end module mod_const_util
