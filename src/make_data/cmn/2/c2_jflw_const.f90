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
  character(CLEN_PATH), parameter :: DIR_JFLW  = trim(DIR_DAT)//'/J-FlwDir'
  character(CLEN_PATH), parameter :: DIR_ORG   = trim(DIR_JFLW)//'/original/v1.4/bin'
  character(CLEN_PATH), parameter :: DIR_PRD   = trim(DIR_JFLW)//'/products'
  character(CLEN_PATH), parameter :: DIR_TILED = trim(DIR_PRD)//'/tiled'
  character(CLEN_PATH), parameter :: DIR_BASIN = trim(DIR_PRD)//'/basin'
  character(CLEN_PATH), parameter :: DIR_ALL   = trim(DIR_PRD)//'/all'

  integer(1), parameter :: FDR__EAST       = 1_1
  integer(1), parameter :: FDR__SOUTHEAST  = 2_1
  integer(1), parameter :: FDR__SOUTH      = 3_1
  integer(1), parameter :: FDR__SOUTHWEST  = 4_1
  integer(1), parameter :: FDR__WEST       = 5_1
  integer(1), parameter :: FDR__NORTHWEST  = 6_1
  integer(1), parameter :: FDR__NORTH      = 7_1
  integer(1), parameter :: FDR__NORTHEAST  = 8_1
  integer(1), parameter :: FDR__RIVERMOUTH = 0_1
  integer(1), parameter :: FDR__INLAND     = -1_1
  integer(1), parameter :: FDR__MISS       = -9_1
  integer(1), parameter :: FDR__UNDEF      = -99_1

  integer(1), parameter :: FDRTYPE__NORMAL      = 1_1
  integer(1), parameter :: FDRTYPE__BIFURCATION = 2_1
  integer(1), parameter :: FDRTYPE__OUTLET      = -1_1
  integer(1), parameter :: FDRTYPE__MISS        = -99_1

  integer, parameter :: XY__RIVERMOUTH = -1
  integer, parameter :: XY__INLAND     = -2

  integer(4), parameter :: UPG_MISS = -9999
  real(4)   , parameter :: ELV_MISS = -9999.0
  real(4)   , parameter :: UPA_MISS = -9999.0
  real(4)   , parameter :: WTH_MISS = -9999.0

  integer(4), parameter :: BSN_MISS  = 0
  integer(4), parameter :: BSN_UNDEF = -9

  integer(1), parameter :: LANDUSE_MISS = -9_1

  integer, parameter :: MX_1SEC = 3600
  integer, parameter :: MY_1SEC = 3600
  integer            :: MX
  integer            :: MY
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
  integer, parameter :: DGT_BSNID_MAX = 7
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
  if( mod(MX_1SEC, scale) /= 0 )then
    call errend(ERROR_CODE__CONDITION, &
      'scale is not a divisor of MX_1SEC.'//&
    '\n  MX_1SEC: '//str(MX_1SEC)//&
    '\n  scale: '//str(scale))
  endif

  MX = MX_1SEC / scale
  MY = MY_1SEC / scale
  NGX = MX * NTX
  NGY = MY * NTY
  GRIDSIZE_LON = TILESIZE_LON / MX
  GRIDSIZE_LAT = TILESIZE_LAT / MY
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_resolution
!===============================================================
!
!===============================================================
end module c2_jflw_const
