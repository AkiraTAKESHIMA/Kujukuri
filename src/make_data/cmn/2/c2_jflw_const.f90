module c2_jflw_const
  use lib_const
  use lib_base
  use lib_log
  use c1_const
  implicit none
  public
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_resolution
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  character(CLEN_PATH), parameter :: DIR_JFLW    = trim(DIR_DAT)//'/J-FlwDir'
  character(CLEN_PATH), parameter :: DIR_ORG     = trim(DIR_JFLW)//'/v1.4/bin'
  character(CLEN_PATH), parameter :: DIR_PRD     = trim(DIR_JFLW)//'/dat'
  character(CLEN_PATH), parameter :: DIR_TILED   = trim(DIR_PRD)//'/tiled'
  character(CLEN_PATH), parameter :: DIR_BASIN   = trim(DIR_PRD)//'/basin'
  character(CLEN_PATH), parameter :: DIR_ALL     = trim(DIR_PRD)//'/all'

  integer(1), parameter :: FDR_EAST       = 1_1
  integer(1), parameter :: FDR_SOUTHEAST  = 2_1
  integer(1), parameter :: FDR_SOUTH      = 3_1
  integer(1), parameter :: FDR_SOUTHWEST  = 4_1
  integer(1), parameter :: FDR_WEST       = 5_1
  integer(1), parameter :: FDR_NORTHWEST  = 6_1
  integer(1), parameter :: FDR_NORTH      = 7_1
  integer(1), parameter :: FDR_NORTHEAST  = 8_1
  integer(1), parameter :: FDR_RIVERMOUTH = 0_1
  integer(1), parameter :: FDR_INLAND     = -1_1
  integer(1), parameter :: FDR_MISS       = -9_1
  integer(1), parameter :: FDR_UNDEF      = -99_1

  integer(1), parameter :: FDRTYPE__NORMAL      = 1_1
  integer(1), parameter :: FDRTYPE__BIFURCATION = 2_1
  integer(1), parameter :: FDRTYPE__OUTLET      = -1_1
  integer(1), parameter :: FDRTYPE__MISS        = -99_1

  integer, parameter :: XY_RIVERMOUTH = -1
  integer, parameter :: XY_INLAND     = -2

  integer(4), parameter :: UPG_MISS = -9999
  real(4)   , parameter :: ELV_MISS = -9999.0
  real(4)   , parameter :: UPA_MISS = -9999.0
  real(4)   , parameter :: WTH_MISS = -9999.0

  integer(4), parameter :: BSN_MISS  = 0
  integer(4), parameter :: BSN_UNDEF = -9

  integer(1), parameter :: LNDUSE_MISS = -9_1

  integer, parameter :: NX_1SEC = 3600
  integer, parameter :: NY_1SEC = 3600
  integer            :: NX
  integer            :: NY
  integer, parameter :: NTX = 27
  integer, parameter :: NTY = 22
  integer            :: NGX
  integer            :: NGY
  real(8), parameter :: REGION_SOUTH = 24.d0
  real(8), parameter :: REGION_NORTH = 46.d0
  real(8), parameter :: REGION_WEST = 122.d0
  real(8), parameter :: REGION_EAST = 149.d0
  real(8), parameter :: TILESIZE_LON = 1.d0
  real(8), parameter :: TILESIZE_LAT = 1.d0
  real(8)            :: GRIDSIZE_LON
  real(8)            :: GRIDSIZE_LAT

  integer, parameter :: DGT_XY  = 4
  integer, parameter :: DGT_TXY = 2
  integer, parameter :: DGT_GXY = 6
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  private :: MODNAM

  character(CLEN_PROC), parameter :: MODNAM = 'c2_jflw_const'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_resolution(resl)
  use c1_grid, only: &
        get_cellsize_ratio
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_resolution'
  character(*), intent(in) :: resl

  integer :: scale

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  scale = get_cellsize_ratio( RESOLUTION_1SEC, resl )
  if( mod(NX_1SEC, scale) /= 0 )then
    call errend(msg_unexpected_condition()//&
        '\n  scale is not a divisor of NX_1SEC.'//&
        '\n  NX_1SEC: '//str(NX_1SEC)//&
        '\n  scale: '//str(scale))
  endif

  NX = NX_1SEC / scale
  NY = NY_1SEC / scale
  NGX = NX * NTX
  NGY = NY * NTY
  GRIDSIZE_LON = TILESIZE_LON / NX
  GRIDSIZE_LAT = TILESIZE_LAT / NY
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_resolution
!===============================================================
!
!===============================================================
end module c2_jflw_const
