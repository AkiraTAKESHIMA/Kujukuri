module def_const
  use lib_const
  implicit none

  integer, parameter :: I4 = 4

  real(8), parameter :: GRAVITY = 9.81d0
  real(8), parameter :: EARTH_R = EARTH_CONST__WGS84_R_VOLMETRIC

  integer, parameter :: DOMAIN__OUTSIDE = 0
  integer, parameter :: DOMAIN__INSIDE  = 1
  integer, parameter :: DOMAIN__OUTLET  = 2

  integer, parameter :: FLOW__KINEMATIC = 0
  integer, parameter :: FLOW__DIFFUSION = 1

  integer, parameter :: NODE_STAT_UPDOWN__UP = 1
  integer, parameter :: NODE_STAT_UPDOWN__DOWN = 2
  integer, parameter :: NODE_STAT_UPDOWN__UNKNOWN = -9

  integer, parameter :: DIR__EAST = 1
  integer, parameter :: DIR__SOUTHEAST = 2
  integer, parameter :: DIR__SOUTH = 4
  integer, parameter :: DIR__SOUTHWEST = 8
  integer, parameter :: DIR__WEST = 16
  integer, parameter :: DIR__NORTHWEST = 32
  integer, parameter :: DIR__NORTH = 64
  integer, parameter :: DIR__NORTHEAST = 128
  integer, parameter :: DIR__MOUTH = 0
  integer, parameter :: DIR__INLAND = -1

  integer, parameter :: SWITCH_RIV_CRSSCT__REGIME    = 0
  integer, parameter :: SWITCH_RIV_CRSSCT__RECTANGLE = 1
  integer, parameter :: SWITCH_RIV_CRSSCT__ARBITRARY = 2

  real(8), parameter :: ZS_THRESH = -100.d0

  real(8), parameter :: LENG_DOMAIN_THRESH = 1d-10
end module def_const
