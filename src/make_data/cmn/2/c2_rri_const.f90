module c2_rri_const
  use lib_const
  use c1_const
  implicit none
  public

  character(CLEN_PATH), parameter :: DIR_RRI   = trim(DIR_DAT)//'/RRI'
  character(CLEN_PATH), parameter :: DIR_TOPO  = trim(DIR_RRI)//'/topo'

  integer(4), parameter :: FDR_EAST      =   1
  integer(4), parameter :: FDR_SOUTHEAST =   2
  integer(4), parameter :: FDR_SOUTH     =   4
  integer(4), parameter :: FDR_SOUTHWEST =   8
  integer(4), parameter :: FDR_WEST      =  16
  integer(4), parameter :: FDR_NORTHWEST =  32
  integer(4), parameter :: FDR_NORTH     =  64
  integer(4), parameter :: FDR_NORTHEAST = 128
  integer(4), parameter :: FDR_RIVERMOUTH = 0

  ! Missing values for topography
  integer(4), parameter :: FDR_MISS = -9
  integer(4), parameter :: ACC_MISS = -9999
  integer(4), parameter :: UPG_MISS = -9999
  real(4)   , parameter :: ELV_MISS = -9999.0
  real(4)   , parameter :: UPA_MISS = -9999.0

  ! Missing values for river parameters
  real(4), parameter :: WIDTH_MISS = -9999.0
  real(4), parameter :: HIGHT_MISS = -9999.0
  real(4), parameter :: DEPTH_MISS = -9999.0
  real(4), parameter :: LEVEE_MISS = -9999.0
end module c2_rri_const
