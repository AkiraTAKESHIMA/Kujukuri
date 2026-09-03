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

  public :: conv_fdr_jflw2rri
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
      miss = JFLW_FDR_MISS
    case( VARNAME__LANDUSE )
      miss = JFLW_LANDUSE_MISS
    case default
      call errend(msg_invalid_value('varName', varName), &
        '', PRCNAM, MODNAM)
    endselect
  case( DATANAME__RRI )
    selectcase( varName )
    case( VARNAME__FDR )
      miss = RRI_FDR_MISS
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine conv_fdr_jflw2rri(fj, fr)
  use c2_jflw_const
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'conv_fdr_jflw2rri'
  integer(1), intent(in) :: fj
  integer(4), intent(out) :: fr

  selectcase( fj )
  case( FDR_EAST )
    fr = RRI_FDR_EAST
  case( FDR_SOUTHEAST )
    fr = RRI_FDR_SOUTHEAST
  case( FDR_SOUTH )
    fr = RRI_FDR_SOUTH
  case( FDR_SOUTHWEST )
    fr = RRI_FDR_SOUTHWEST
  case( FDR_WEST )
    fr = RRI_FDR_WEST
  case( FDR_NORTHWEST )
    fr = RRI_FDR_NORTHWEST
  case( FDR_NORTH )
    fr = RRI_FDR_NORTH
  case( FDR_NORTHEAST )
    fr = RRI_FDR_NORTHEAST
  case( FDR_RIVERMOUTH )
    fr = RRI_FDR_RIVERMOUTH
  case( FDR_MISS )
    fr = RRI_FDR_MISS
  case( FDR_INLAND, &
        FDR_UNDEF )
    call errend('Failed to convert: '//str(fj), &
           '', PRCNAM, MODNAM)
  case default
    call errend(msg_invalid_value('fj', fj), &
           '', PRCNAM, MODNAM)
  endselect
end subroutine conv_fdr_jflw2rri
!===============================================================
!
!===============================================================
end module c3_joint_util
