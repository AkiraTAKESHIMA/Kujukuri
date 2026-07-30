module lib_util_str_const
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: str_arctyp_long
  public :: str_arctyp_short

  public :: str_arcpos_long
  public :: str_arcpos_short

  public :: str_arc_rel_lat

  public :: str_convex_long
  public :: str_convex_short

  public :: str_polygon_pos_long

  public :: str_region_type_long
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface str_arctyp_long
    module procedure str_arctyp_long__0d
    module procedure str_arctyp_long__1d
  end interface

  interface str_arctyp_short
    module procedure str_arctyp_short__0d
    module procedure str_arctyp_short__1d
  end interface

  interface str_arcpos_long
    module procedure str_arcpos_long__0d
    module procedure str_arcpos_long__1d
  end interface

  interface str_arcpos_short
    module procedure str_arcpos_short__0d
    module procedure str_arcpos_short__1d
  end interface

  interface str_convex_long
    module procedure str_convex_long__0d
    module procedure str_convex_long__1d
  end interface

  interface str_convex_short
    module procedure str_convex_short__0d
    module procedure str_convex_short__1d
  end interface

  interface str_polygon_pos_long
    module procedure str_polygon_pos_long__0d
  end interface

  interface str_region_type_long
    module procedure str_region_type_long__0d
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_util_str_const'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function str_arctyp_long__0d(typ) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arctyp_long__0d'
  integer(1), intent(in) :: typ
  character(:), allocatable :: res
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( typ )
  case( ARC_TYPE_UNDEF )
    res = 'undef'
  case( ARC_TYPE_NORMAL )
    res = 'normal'
  case( ARC_TYPE_PARALLEL )
    res = 'parallel'
  case( ARC_TYPE_MERIDIAN )
    res = 'meridian'
  case default
    call errend(msg_invalid_value('typ', typ), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_arctyp_long__0d
!===============================================================
!
!===============================================================
function str_arctyp_long__1d(typ) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arctyp_long__1d'
  integer(1), intent(in) :: typ(:)
  character(clen_key) :: res(size(typ))

  integer :: i
  !-------------------------------------------------------------
  do i = 1, size(typ)
    res(i) = str_arctyp_long__0d(typ(i))
  enddo
  !-------------------------------------------------------------
end function str_arctyp_long__1d
!===============================================================
!
!===============================================================
function str_arctyp_short__0d(typ) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arctyp_short__0d'
  integer(1), intent(in) :: typ
  character(1) :: res
  !-------------------------------------------------------------
  selectcase( typ )
  case( ARC_TYPE_UNDEF )
    res = 'u'
  case( ARC_TYPE_NORMAL )
    res = 'n'
  case( ARC_TYPE_PARALLEL )
    res = 'p'
  case( ARC_TYPE_MERIDIAN )
    res = 'm'
  case default
    call errend(msg_invalid_value('typ', typ), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_arctyp_short__0d
!===============================================================
!
!===============================================================
function str_arctyp_short__1d(typ) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arctyp_short__1d'
  integer(1), intent(in) :: typ(:)
  character(1) :: res(size(typ))

  integer :: i
  !-------------------------------------------------------------
  do i = 1, size(typ)
    res(i) = str_arctyp_short__0d(typ(i))
  enddo
  !-------------------------------------------------------------
end function str_arctyp_short__1d
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
function str_arcpos_long__0d(pos) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arcpos_long__0d'
  integer(1), intent(in) :: pos
  character(:), allocatable :: res
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( pos )
  case( ARC_POSITION_UNDEF )
    res = 'undef'
  case( ARC_POSITION_NORMAL )
    res = 'normal'
  case( ARC_POSITION_LON0 )
    res = 'lon0'
  case( ARC_POSITION_POLAR )
    res = 'polar'
  case default
    call errend(msg_invalid_value('pos', pos), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_arcpos_long__0d
!===============================================================
!
!===============================================================
function str_arcpos_long__1d(pos) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arcpos_long__1d'
  integer(1), intent(in) :: pos(:)
  character(clen_key) :: res(size(pos))

  integer :: i
  !-------------------------------------------------------------
  do i = 1, size(pos)
    res(i) = str_arcpos_long__0d(pos(i))
  enddo
  !-------------------------------------------------------------
end function str_arcpos_long__1d
!===============================================================
!
!===============================================================
function str_arcpos_short__0d(pos) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arcpos_short__0d'
  integer(1), intent(in) :: pos
  character(1) :: res
  !-------------------------------------------------------------
  selectcase( pos )
  case( ARC_POSITION_UNDEF )
    res = 'u'
  case( ARC_POSITION_NORMAL )
    res = 'n'
  case( ARC_POSITION_LON0 )
    res = 'l'
  case( ARC_POSITION_POLAR )
    res = 'p'
  case default
    call errend(msg_invalid_value('pos', pos), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_arcpos_short__0d
!===============================================================
!
!===============================================================
function str_arcpos_short__1d(pos) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arcpos_short__1d'
  integer(1), intent(in) :: pos(:)
  character(1) :: res(size(pos))

  integer :: i
  !-------------------------------------------------------------
  do i = 1, size(pos)
    res(i) = str_arcpos_short__0d(pos(i))
  enddo
  !-------------------------------------------------------------
end function str_arcpos_short__1d
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
function str_arc_rel_lat(arc_rel) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_arc_rel_lat'
  integer(1), intent(in) :: arc_rel
  character(:), allocatable :: res
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( arc_rel )
  case( ARC_REL_LAT_PARA_PARA_UNDEF )
    res = 'para_para_undef'
  case( ARC_REL_LAT_PARA_PARA_BELOW )
    res = 'para_para_below'
  case( ARC_REL_LAT_PARA_PARA_ABOVE )
    res = 'para_para_above'
  case( ARC_REL_LAT_PARA_NORM_UNDEF )
    res = 'para_norm_undef'
  case( ARC_REL_LAT_PARA_NORM_BELOW )
    res = 'para_norm_below'
  case( ARC_REL_LAT_PARA_NORM_ABOVE )
    res = 'para_norm_above'
  case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_UPWARD )
    res = 'para_norm_one_intersection_upward'
  case( ARC_REL_LAT_PARA_NORM_ONE_INTERSECTION_DOWNWARD )
    res = 'para_norm_one_intersection_downward'
  case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_UPWARD )
    res = 'para_norm_two_intersections_convex_upward'
  case( ARC_REL_LAT_PARA_NORM_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
    res = 'para_norm_two_intersections_convex_downward'
  case( ARC_REL_LAT_NORM_PARA_UNDEF )
    res = 'norm_para_undef'
  case( ARC_REL_LAT_NORM_PARA_BELOW )
    res = 'norm_para_below'
  case( ARC_REL_LAT_NORM_PARA_ABOVE )
    res = 'norm_para_above'
  case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_UPWARD )
    res = 'norm_para_one_intersection_upward'
  case( ARC_REL_LAT_NORM_PARA_ONE_INTERSECTION_DOWNWARD )
    res = 'norm_para_one_intersection_downward'
  case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_UPWARD )
    res = 'norm_para_two_intersections_convex_upward'
  case( ARC_REL_LAT_NORM_PARA_TWO_INTERSECTIONS_CONVEX_DOWNWARD )
    res = 'norm_para_two_intersections_convex_downward'
  case( ARC_REL_LAT_NORM_NORM_UNDEF )
    res = 'norm_norm_undef'
  case( ARC_REL_LAT_NORM_NORM_BELOW )
    res = 'norm_norm_below'
  case( ARC_REL_LAT_NORM_NORM_ABOVE )
    res = 'norm_norm_above'
  case( ARC_REL_LAT_NORM_NORM_INTERSECTION_UPWARD )
    res = 'norm_norm_intersection_upward'
  case( ARC_REL_LAT_NORM_NORM_INTERSECTION_DOWNWARD )
    res = 'norm_norm_intersection_downward'
  case default
    call errend(msg_invalid_value('arc_rel', arc_rel), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_arc_rel_lat
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
function str_convex_long__0d(convex) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_convex_long__0d'
  integer(1), intent(in) :: convex
  character(:), allocatable :: res
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( convex )
  case( CONVEX_UNDEF )
    res = 'undef'
  case( CONVEX_MONOTONE )
    res = 'monotone'
  case( CONVEX_UPWARD )
    res = 'upward'
  case( CONVEX_DOWNWARD )
    res = 'downward'
  case default
    call errend(msg_invalid_value('convex', convex), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_convex_long__0d
!===============================================================
!
!===============================================================
function str_convex_long__1d(convex) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_convex_long__1d'
  integer(1), intent(in) :: convex(:)
  character(clen_key) :: res(size(convex))

  integer :: i
  !-------------------------------------------------------------
  do i = 1, size(convex)
    res(i) = str_convex_long__0d(convex(i))
  enddo
  !-------------------------------------------------------------
end function str_convex_long__1d
!===============================================================
!
!===============================================================
function str_convex_short__0d(convex) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_convex_short__0d'
  integer(1), intent(in) :: convex
  character(:), allocatable :: res
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( convex )
  case( CONVEX_UNDEF )
    res = '-'
  case( CONVEX_MONOTONE )
    res = 'm'
  case( CONVEX_DOWNWARD )
    res = 'd'
  case( CONVEX_UPWARD )
    res = 'u'
  case default
    call errend(msg_invalid_value('convex', convex), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_convex_short__0d
!===============================================================
!
!===============================================================
function str_convex_short__1d(convex) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_convex_short__1d'
  integer(1), intent(in) :: convex(:)
  character(clen_key) :: res(size(convex))

  integer :: i
  !-------------------------------------------------------------
  do i = 1, size(convex)
    res(i) = str_convex_short__0d(convex(i))
  enddo
  !-------------------------------------------------------------
end function str_convex_short__1d
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
function str_polygon_pos_long__0d(pos) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_polygon_pos_long__0d'
  integer(1), intent(in) :: pos
  character(:), allocatable :: res
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( pos )
  case( POLYGON_POSITION_UNDEF )
    res = 'undef'
  case( POLYGON_POSITION_NORMAL )
    res = 'normal'
  case( POLYGON_POSITION_LON0 )
    res = 'lon0'
  case( POLYGON_POSITION_POLAR )
    res = 'polar'
  case default
    call errend(msg_invalid_value('pos', pos), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_polygon_pos_long__0d
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
function str_region_type_long__0d(typ) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'str_region_type_long__0d'
  integer(1), intent(in) :: typ
  character(:), allocatable :: res
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( typ )
  case( REGION_TYPE_UNDEF )
    res = 'undef'
  case( REGION_TYPE_GLOBAL )
    res = 'global'
  case( REGION_TYPE_CYCLIC )
    res = 'cyclic'
  case( REGION_TYPE_REGIONAL )
    res = 'regional'
  case default
    call errend(msg_invalid_value('typ', typ), &
                '', PRCNAM, MODNAM)
  endselect
  !-------------------------------------------------------------
end function str_region_type_long__0d
!===============================================================
!
!===============================================================
end module lib_util_str_const
