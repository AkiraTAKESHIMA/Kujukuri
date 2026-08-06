module c1_const
  use lib_const
  implicit none

  ! Directory
  character(CLEN_PATH), parameter :: DIR_TOP = '/data10/atakeshima/Kujukuri'
  character(CLEN_PATH), parameter :: DIR_DAT = trim(DIR_TOP)//'/dat'

  ! SPRING
  character(CLEN_PATH), parameter :: DIR_SPRING_MAIN = trim(DIR_TOP)//'/src/SPRING/main/std'
  character(CLEN_PATH), parameter :: &
     PROG_SPRING_MAKE_GRID_DATA = trim(DIR_SPRING_MAIN)//'/make_grid_data/main.exe'
  character(CLEN_PATH), parameter :: &
     PROG_SPRING_REMAP          = trim(DIR_SPRING_MAIN)//'/remap/main.exe'
  character(CLEN_PATH), parameter :: &
     PROG_SPRING_MERGE          = trim(DIR_SPRING_MAIN)//'/merge_remapping_tables/main.exe'
  character(CLEN_PATH), parameter :: &
     PROG_SPRING_RASTERIZE      = trim(DIR_SPRING_MAIN)//'/rasterize/main.exe'

  ! Resolution
  character(CLEN_KEY), parameter :: RESOLUTION_100M = '100m'
  character(CLEN_KEY), parameter :: RESOLUTION_3SEC = '3sec'
  character(CLEN_KEY), parameter :: RESOLUTION_1SEC = '1sec'

  ! Catchment number
  integer, parameter :: CAT_MISS = -9999

  ! Coordinates
  real(8), parameter :: LONLAT_MISS = -999.d0

  ! Eearth's shape
  character(CLEN_VAR), parameter :: EARTH_GEOSYS = EARTH_GEOSYS__WGS84
  character(CLEN_VAR), parameter :: EARTH_RTYP = EARTH_RTYP__AUTHALIC
  real(8), parameter :: EARTH_R  = EARTH_CONST__WGS84_R_AUTHALIC
  real(8), parameter :: EARTH_E2 = 0.d0

  ! Format
  integer, parameter :: DGT_BSNID_MAX = 7
end module c1_const
