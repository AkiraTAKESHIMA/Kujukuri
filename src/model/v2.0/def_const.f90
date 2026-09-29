module def_const
  implicit none

  real(8), parameter :: PI = acos(-1.d0)
  integer, parameter :: I4 = 4

  integer, parameter :: DOMAIN__OUTSIDE = 0
  integer, parameter :: DOMAIN__INSIDE  = 1
  integer, parameter :: DOMAIN__OUTLET  = 2

  integer, parameter :: FLOW__KINEMATIC = 0
  integer, parameter :: FLOW__DIFFUSION = 1

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
end module def_const
