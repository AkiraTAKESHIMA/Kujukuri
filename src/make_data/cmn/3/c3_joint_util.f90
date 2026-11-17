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
  public :: get_varopt

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
subroutine get_varopt(&
  catmType, mode, task, &
  opt_elv_in, opt_dir_in, opt_var_out &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_varopt'
  character(*), intent(in) :: catmType
  integer, intent(in) :: mode
  character(*), intent(in) :: task
  character(*), intent(out) :: opt_elv_in, opt_dir_in, opt_var_out

  integer :: mode_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( mode == MODE_TOPOMODIFICATION__AUTO )then
    selectcase( catmType )
    case( CATMTYPE__BASIN )
      mode_ = 1
    case( CATMTYPE__NETWORK, &
          CATMTYPE__NETWORKSET )
      mode_ = 2
    case default
      call errend(ERROR_CODE__VALUE, 'catmType == '//str(catmType))
    endselect
  else
    mode_ = mode
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( mode_ )
  !-------------------------------------------------------------
  ! Case: No modification
  case( 0 )
    selectcase( task )
    case( 'removeRiverChannelEffect', &
          'fillDepressions'  )
      call errend(ERROR_CODE__CONDITION, &
        'Inconsistency: no modification mode was selected in the task `'//str(task)//'`')
    case( 'calcUpperArea' )
      opt_elv_in = ''
      opt_dir_in = ''
      opt_var_out = ''
    case( 'scaleUp' )
      opt_elv_in = ''
      opt_dir_in = ''
      opt_var_out = ''
    case default
      call errend(ERROR_CODE__VALUE, 'task == '//str(task))
    endselect
  !-------------------------------------------------------------
  ! Case: For J-FlwDir catchment
  ! - rmriv -> fildep
  case( 1 )
    if( catmType /= CATMTYPE__BASIN )then
      call errend(ERROR_CODE__CONDITION, 'Modification mode '//&
        str(mode_)//' cannot be selected when catmType == '//str(catmType))
    endif

    selectcase( task )
    case( 'calcUpperArea' )
      opt_elv_in = ''
      opt_dir_in = ''
      opt_var_out = ''
    case( 'removeRiverChannelEffect' )
      opt_elv_in = ''
      opt_dir_in = ''
      opt_var_out = '_rmriv'
    case( 'fillDepressions' )
      opt_elv_in = '_rmriv'
      opt_dir_in = ''
      opt_var_out = '_fildep'
    case( 'scaleUp' )
      opt_elv_in = '_fildep'
      opt_dir_in = ''
      opt_var_out = ''
    case default
      call errend(ERROR_CODE__VALUE, 'task == '//str(task))
    endselect

  !-------------------------------------------------------------
  ! Case: For StrRank catchment
  ! - fitdir -> rmriv -> fildep
  case( 2 )
    if( catmType /= CATMTYPE__NETWORK .and. &
        catmType /= CATMTYPE__NETWORKSET )then
      call errend(ERROR_CODE__CONDITION, 'Modification mode '//&
        str(mode_)//' cannot be selected when catmType == '//str(catmType))
    endif

    selectcase( task )
    case( 'fitFlwDirToRiverNetwork' )
      opt_elv_in = ''  ! not used
      opt_dir_in = ''
      opt_var_out = '_fitdir'
    case( 'calcUpperArea' )
      opt_elv_in = '_fitdir'  ! not used
      opt_dir_in = '_fitdir'
      opt_var_out = '_fitdir'
    case( 'removeRiverChannelEffect' )
      opt_elv_in = '_fitdir'
      opt_dir_in = '_fitdir'
      opt_var_out = '_rmriv'
    case( 'fillDepressions' )
      opt_elv_in = '_rmriv'
      opt_dir_in = '_fitdir'
      opt_var_out = '_fildep'
    case( 'scaleUp' )
      opt_elv_in = '_fildep'
      opt_dir_in = '_fitdir'
      opt_var_out = '_fitdir'
    case default
      call errend(ERROR_CODE__VALUE, 'task == '//str(task))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(ERROR_CODE__VALUE, 'mode == '//str(mode_))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('modification='//str(mode_)//&
    ' elv_in='//str(opt_elv_in)//' dir_in='//str(opt_dir_in)//&
    ' var_out='//str(opt_var_out))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_varopt
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
subroutine get_miss__i1(dataName, varName, miss)
  use c3_jflw_const
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_miss__i1'
  character(*), intent(in) :: dataName
  character(*), intent(in) :: varName
  integer(1), intent(out) :: miss

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( dataName )
  case( DATANAME__JFLW )
    selectcase( varName )
    case( VARNAME__FDR )
      miss = JFLW_FDR__MISS
    case( VARNAME__LANDUSE )
      miss = JFLW_LANDUSE_MISS
    case default
      call errend(ERROR_CODE__VALUE, 'varName == '//str(varName))
    endselect
  case( DATANAME__RRI )
    selectcase( varName )
    case( VARNAME__FDR )
      miss = RRI_FDR__MISS
    case( VARNAME__LANDUSE )
      miss = RRI_LANDUSE_MISS
    case default
      call errend(ERROR_CODE__VALUE, 'varName == '//str(varName))
    endselect
  case default
    call errend(ERROR_CODE__VALUE, 'dataName == '//str(dataName))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
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

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( dataName )
  case( DATANAME__JFLW )
    selectcase( varName )
    case( VARNAME__UPG )
      miss = JFLW_UPG_MISS
    case default
      call errend(ERROR_CODE__VALUE, 'varName == '//str(varName))
    endselect
  case( DATANAME__RRI )
    selectcase( varName )
    case( VARNAME__UPG )
      miss = RRI_ACC_MISS
    case default
      call errend(ERROR_CODE__VALUE, 'varName == '//str(varName))
    endselect
  case default
    call errend(ERROR_CODE__VALUE, 'dataName == '//str(dataName))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
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

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
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
      call errend(ERROR_CODE__VALUE, 'varName == '//str(varName))
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
      call errend(ERROR_CODE__VALUE, 'varName == '//str(varName))
    endselect
  case default
    call errend(ERROR_CODE__VALUE, 'dataName == '//str(dataName))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_miss__r4
!===============================================================
!
!===============================================================
end module c3_joint_util
