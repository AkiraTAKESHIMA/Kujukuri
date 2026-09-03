module c2_rri_const
  use lib_const
  use c1_const
  implicit none
  public

  character(CLEN_PATH), parameter :: DIR_RRI   = trim(DIR_DAT)//'/RRI'

  integer(4), parameter :: FDR_EAST      =   1
  integer(4), parameter :: FDR_SOUTHEAST =   2
  integer(4), parameter :: FDR_SOUTH     =   4
  integer(4), parameter :: FDR_SOUTHWEST =   8
  integer(4), parameter :: FDR_WEST      =  16
  integer(4), parameter :: FDR_NORTHWEST =  32
  integer(4), parameter :: FDR_NORTH     =  64
  integer(4), parameter :: FDR_NORTHEAST = 128
  integer(4), parameter :: FDR_RIVERMOUTH = 0

  ! Missing values of topographic data
  integer(4), parameter :: FDR_MISS = -9
  integer(4), parameter :: ACC_MISS = -9999
  real(8)   , parameter :: ELV_MISS = -9999.d0
  real(8)   , parameter :: UPA_MISS = -9999.d0

  ! Missing values of land use category
  integer(1), parameter :: LANDUSE_MISS = -9_1

  ! Missing values of river parameters
  real(8), parameter :: WIDTH_MISS = -9999.d0
  real(8), parameter :: HIGHT_MISS = -9999.d0
  real(8), parameter :: DEPTH_MISS = -9999.d0
  real(8), parameter :: LEVEE_MISS = -9999.d0
end module c2_rri_const
