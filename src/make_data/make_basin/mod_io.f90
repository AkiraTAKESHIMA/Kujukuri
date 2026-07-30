module mod_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c2_jflw_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_map_tile_margin

  public :: tileinfo
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_map_tile_margin
    module procedure read_map_tile_margin__int1
    module procedure read_map_tile_margin__int4
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_map_tile_margin__int1(dat, miss, var, tx, ty, stat)
  use c2_jflw_io, only: &
        get_f_map_tile
  implicit none
  integer(1)  , intent(out) :: dat(0:,0:)
  integer(1)  , intent(in)  :: miss
  character(*), intent(in)  :: var
  integer     , intent(in)  :: tx, ty
  integer     , intent(out) :: stat

  integer :: nx, ny
  character(CLEN_PATH) :: f
  integer :: access

  character(CLEN_PROC), parameter :: PRCNAM = 'read_map_tile_margin__int1'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  stat = 0

  nx = size(dat,1) - 2
  ny = size(dat,2) - 2

  f = get_f_map_tile(RESOLUTION_1SEC, var, tx, ty)
  if( access(f,' ') /= 0 )then
    stat = 1
    dat(:,:) = miss
    call logret(PRCNAM, MODNAM)
    return
  endif
  call traperr( rbin(dat(1:nx,1:ny), f) )

  dat(0   ,:) = miss
  dat(nx+1,:) = miss
  dat(:,0   ) = miss
  dat(:,ny+1) = miss

  ! Northwest
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx-1, ty-1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(0:0,0:0), f, sz=int((/nx,ny/),8), lb=int((/nx,ny/),8)) )
  endif

  ! North
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx, ty-1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(1:nx,0:0), f, sz=int((/nx,ny/),8), lb=int((/1,ny/),8)) )
  endif

  ! Northeast
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx+1, ty-1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(nx+1:nx+1,0:0), f, sz=int((/nx,ny/),8), lb=int((/1,ny/),8)) )
  endif

  ! Southwest
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx-1, ty+1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(0:0,ny+1:ny+1), f, sz=int((/nx,ny/),8), lb=int((/nx,1/),8)) )
  endif

  ! South
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx, ty+1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(1:nx,ny+1:ny+1), f, sz=int((/nx,ny/),8), lb=int((/1,1/),8)) )
  endif

  ! Southeast
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx+1, ty+1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(nx+1:nx+1,ny+1:ny+1), f, sz=int((/nx,ny/),8), lb=int((/1,1/),8)) )
  endif

  ! West
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx-1, ty)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(0:0,1:ny), f, sz=int((/nx,ny/),8), lb=int((/nx,1/),8)) )
  endif

  ! East
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx+1, ty)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(nx+1:nx+1,1:ny), f, sz=int((/nx,ny/),8), lb=int((/1,1/),8)) )
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_map_tile_margin__int1
!===============================================================
!
!===============================================================
subroutine read_map_tile_margin__int4(dat, miss, var, tx, ty, stat)
  use c2_jflw_io, only: &
        get_f_map_tile
  implicit none
  integer(4)  , intent(out) :: dat(0:,0:)
  integer(4)  , intent(in)  :: miss
  character(*), intent(in)  :: var
  integer     , intent(in)  :: tx, ty
  integer     , intent(out) :: stat

  integer :: nx, ny
  character(CLEN_PATH) :: f
  integer :: access

  character(CLEN_PROC), parameter :: PRCNAM = 'read_map_tile_margin__int4'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  stat = 0

  nx = size(dat,1) - 2
  ny = size(dat,2) - 2

  f = get_f_map_tile(RESOLUTION_1SEC, var, tx, ty)
  if( access(f,' ') /= 0 )then
    stat = 1
    dat(:,:) = miss
    call logret(PRCNAM, MODNAM)
    return
  endif
  call traperr( rbin(dat(1:nx,1:ny), f) )

  dat(0   ,:) = miss
  dat(nx+1,:) = miss
  dat(:,0   ) = miss
  dat(:,ny+1) = miss

  ! Northwest
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx-1, ty-1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(0:0,0:0), f, sz=int((/nx,ny/),8), lb=int((/nx,ny/),8)) )
  endif

  ! North
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx, ty-1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(1:nx,0:0), f, sz=int((/nx,ny/),8), lb=int((/1,ny/),8)) )
  endif

  ! Northeast
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx+1, ty-1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(nx+1:nx+1,0:0), f, sz=int((/nx,ny/),8), lb=int((/1,ny/),8)) )
  endif

  ! Southwest
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx-1, ty+1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(0:0,ny+1:ny+1), f, sz=int((/nx,ny/),8), lb=int((/nx,1/),8)) )
  endif

  ! South
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx, ty+1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(1:nx,ny+1:ny+1), f, sz=int((/nx,ny/),8), lb=int((/1,1/),8)) )
  endif

  ! Southeast
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx+1, ty+1)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(nx+1:nx+1,ny+1:ny+1), f, sz=int((/nx,ny/),8), lb=int((/1,1/),8)) )
  endif

  ! West
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx-1, ty)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(0:0,1:ny), f, sz=int((/nx,ny/),8), lb=int((/nx,1/),8)) )
  endif

  ! East
  f = get_f_map_tile(RESOLUTION_1SEC, var, tx+1, ty)
  if( access(f,' ') == 0 )then
    call traperr( rbin(dat(nx+1:nx+1,1:ny), f, sz=int((/nx,ny/),8), lb=int((/1,1/),8)) )
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_map_tile_margin__int4
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
character(32) function tileinfo(tx,ty)
  use c2_jflw_io, only: &
        tilename
  implicit none
  integer, intent(in) :: tx, ty

  tileinfo = 'tile '//str(tilename(tx,ty))//&
             ' ('//str((/tx,ty/),DGT_TXY,',')//')'
end function tileinfo
!===============================================================
!
!===============================================================
end module mod_io
