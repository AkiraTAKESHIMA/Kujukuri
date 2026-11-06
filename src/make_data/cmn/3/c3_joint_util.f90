module c3_joint_util
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c1_const
  use c3_joint_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_miss
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface get_miss
    module procedure get_miss__i1
    module procedure get_miss__i4
    module procedure get_miss__r4
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_joint_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine get_miss__i1(dataName, varName, miss)
  use c3_jflw_const
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss__i1'
  character(*), intent(in) :: dataName
  character(*), intent(in) :: varName
  integer(1), intent(out) :: miss

  selectcase( dataName )
  case( DATANAME__JFLW )
    selectcase( varName )
    case( VARNAME__FDR )
      miss = JFLW_FDR__MISS
    case( VARNAME__LANDUSE )
      miss = JFLW_LANDUSE_MISS
    case default
      call errend(msg_invalid_value('varName', varName), &
        '', PRCNAM, MODNAM)
    endselect
  case( DATANAME__RRI )
    selectcase( varName )
    case( VARNAME__FDR )
      miss = RRI_FDR__MISS
    case( VARNAME__LANDUSE )
      miss = RRI_LANDUSE_MISS
    case default
      call errend(msg_invalid_value('varName', varName), &
        '', PRCNAM, MODNAM)
    endselect
  case default
    call errend(msg_invalid_value('dataName', dataName), &
      '', PRCNAM, MODNAM)
  endselect
end subroutine get_miss__i1
!===============================================================
!
!===============================================================
subroutine get_miss__i4(dataName, varName, miss)
  use c3_jflw_const
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss__i4'
  character(*), intent(in) :: dataName
  character(*), intent(in) :: varName
  integer(4), intent(out) :: miss

  selectcase( dataName )
  case( DATANAME__JFLW )
    selectcase( varName )
    case( VARNAME__UPG )
      miss = JFLW_UPG_MISS
    case default
      call errend(msg_invalid_value('varName', varName), &
        '', PRCNAM, MODNAM)
    endselect
  case( DATANAME__RRI )
    selectcase( varName )
    case( VARNAME__UPG )
      miss = RRI_ACC_MISS
    case default
      call errend(msg_invalid_value('varName', varName), &
        '', PRCNAM, MODNAM)
    endselect
  case default
    call errend(msg_invalid_value('dataName', dataName), &
      '', PRCNAM, MODNAM)
  endselect
end subroutine get_miss__i4
!===============================================================
!
!===============================================================
subroutine get_miss__r4(dataName, varName, miss)
  use c3_jflw_const
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss__r4'
  character(*), intent(in) :: dataName
  character(*), intent(in) :: varName
  real(4), intent(out) :: miss

  selectcase( dataName )
  case( DATANAME__JFLW )
    selectcase( varName )
    case( VARNAME__ELV )
      miss = JFLW_ELV_MISS
    case( VARNAME__UPA )
      miss = JFLW_UPA_MISS
    case( VARNAME__WTH )
      miss = JFLW_WTH_MISS
    case default
      call errend(msg_invalid_value('varName', varName), &
        '', PRCNAM, MODNAM)
    endselect
  case( DATANAME__RRI )
    selectcase( varName )
    case( VARNAME__ELV )
      miss = RRI_ELV_MISS
    case( VARNAME__UPA )
      miss = RRI_UPA_MISS
    case( VARNAME__WTH )
      miss = real(RRI_WIDTH_MISS,4)
    case default
      call errend(msg_invalid_value('varName', varName), &
        '', PRCNAM, MODNAM)
    endselect
  case default
    call errend(msg_invalid_value('dataName', dataName), &
      '', PRCNAM, MODNAM)
  endselect
end subroutine get_miss__r4
!===============================================================
!
!===============================================================
end module c3_joint_util
