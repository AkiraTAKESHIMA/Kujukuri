module c1_const_util
  use lib_const
  use lib_base
  use lib_log
  use c1_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: checkval_idx_condition
  public :: checkval_opt_old_files
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_const_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function checkval_idx_condition(s, id) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'checkval_idx_condition'
  character(*), intent(in) :: s
  character(*), intent(in), optional :: id

  character(:), allocatable :: id_

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: id_)
  id_ = 'idx_condition'
  if( present(id) ) id_ = trim(id)

  selectcase( s )
  case( IDX_CONDITION__UNDEF     , &
        IDX_CONDITION__MATCH     , &
        IDX_CONDITION__GRD_IN_RST, &
        IDX_CONDITION__RST_IN_GRD, &
        IDX_CONDITION__NONE )
    continue
  case default
    info = 1
    call errret(msg_invalid_value(id_, s))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function checkval_idx_condition
!===============================================================
!
!===============================================================
integer(4) function checkval_opt_old_files(s, id) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'checkval_opt_old_files'
  character(*), intent(in) :: s
  character(*), intent(in), optional :: id

  character(:), allocatable :: id_

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: id_)
  id_ = 'opt_old_files'
  if( present(id) ) id_ = trim(id)

  selectcase( s )
  case( OPT_OLD_FILES_STOP, &
        OPT_OLD_FILES_REMOVE, &
        OPT_OLD_FILES_OVERWRITE )
    continue
  case default
    info = 1
    call errret(msg_invalid_value(id_, s))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function checkval_opt_old_files
!===============================================================
!
!===============================================================
end module c1_const_util
