module c2_rri_grid
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: ratio_resolution
  public :: get_nextxy
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rri_grid'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function ratio_resolution(resolution) result(res)
  use c1_const
  implicit none
  character(*), intent(in) :: resolution

  character(CLEN_PROC), parameter :: PRCNAM = 'ratio_resolution'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( resolution )
  case( RESOLUTION_1SEC )
    res = 1
  case( RESOLUTION_3SEC )
    res = 3
  case default
    call errend(msg_invalid_value('resolution', resolution))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function ratio_resolution
!===============================================================
!
!===============================================================
subroutine get_nextxy(x, y, fdr, xx, yy)
  use c2_rri_const
  implicit none
  integer, intent(in)  :: x, y
  integer, intent(in)  :: fdr
  integer, intent(out) :: xx, yy

  character(CLEN_PROC), parameter :: PRCNAM = 'get_nextxy'

  selectcase( fdr )
  case( FDR__WEST )
    xx = x - 1
    yy = y
  case( FDR__SOUTHWEST )
    xx = x - 1
    yy = y + 1
  case( FDR__SOUTH )
    xx = x
    yy = y + 1
  case( FDR__SOUTHEAST )
    xx = x + 1
    yy = y + 1
  case( FDR__EAST )
    xx = x + 1
    yy = y
  case( FDR__NORTHEAST )
    xx = x + 1
    yy = y - 1
  case( FDR__NORTH )
    xx = x
    yy = y - 1
  case( FDR__NORTHWEST )
    xx = x - 1
    yy = y - 1
  case( FDR__RIVERMOUTH )
    xx = 0
    yy = 0
  case default
    call errend(msg_invalid_value('fdr', fdr), &
                PRCNAM, MODNAM)
  endselect
end subroutine get_nextxy
!===============================================================
!
!===============================================================
end module c2_rri_grid
