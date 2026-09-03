module c2_nlni_const
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
  character(CLEN_PATH), parameter :: DIR_NLNI    = trim(DIR_DAT)//'/NLNI'
  character(CLEN_PATH), parameter :: DIR_DL      = trim(DIR_NLNI)//'/dl'
  character(CLEN_PATH), parameter :: DIR_PRD     = trim(DIR_NLNI)//'/dat'

  character(CLEN_KEY), parameter :: VARNAME_LANDUSE = 'landuse'

  integer, parameter :: DGT_WSCODE = 6
  integer, parameter :: DGT_RVCODE = 10
  integer(4)   , parameter :: WSCODE_MISS_I = -9999
  integer(4)   , parameter :: DIV_WSCODE_RVUNKNOWN = 10000
  character(16), parameter :: RVNAME_UNKNOWN = '名称不明'

  integer(1), parameter :: LANDUSE_MISS = -9_1

  real(8), parameter :: REGION_WEST  = 122.d0
  real(8), parameter :: REGION_EAST  = 149.d0
  real(8), parameter :: REGION_SOUTH =  20.d0
  real(8), parameter :: REGION_NORTH =  46.d0
  integer, parameter :: TXMIN = 22
  integer, parameter :: TXMAX = 48
  integer, parameter :: TYMIN = 30
  integer, parameter :: TYMAX = 68
  integer, parameter :: NTX = TXMAX - TXMIN + 1
  integer, parameter :: NTY = TYMAX - TYMIN + 1
  integer            :: NX
  integer            :: NY
  integer            :: NGX
  integer            :: NGY
  real(8), parameter :: TILESIZE_LON = 1.d0
  real(8), parameter :: TILESIZE_LAT = 2.d0/3.d0
  real(8)            :: CELLSIZE_LON
  real(8)            :: CELLSIZE_LAT

  integer, parameter :: DGT_TXY = 2
  integer            :: DGT_XY  = 0
  integer            :: DGT_GXY = 0
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  private :: MODNAM

  character(CLEN_VAR), parameter :: MODNAM = 'c2_nlni_const'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_resolution(resolution)
  implicit none
  character(*), intent(in) :: resolution

  character(CLEN_PROC), parameter :: PRCNAM = 'set_resolution'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( resolution )
  case( RESOLUTION_100M )
    NX = 800
    NY = 800
  case( RESOLUTION_3SEC )
    NX = 1200
    NY =  800
  case default
    call errend(msg_invalid_value('resolution', resolution))
  endselect

  NGX = NX * NTX
  NGY = NY * NTY
  CELLSIZE_LON = TILESIZE_LON / NX
  CELLSIZE_LAT = TILESIZE_LAT / NY

  DGT_XY = dgt(max(NX,NY))
  DGT_GXY = dgt(max(NGX,NGY))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_resolution
!===============================================================
!
!===============================================================
end module c2_nlni_const
