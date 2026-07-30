module c1_grid
  use lib_const
  use lib_base
  use lib_log
  use c1_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_cellsize_in_sec
  public :: get_cellsize_ratio

  public :: apprx_isct_with_meridian
  public :: apprx_isct_with_parallel
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_grid'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function get_cellsize_in_sec(resl) result(n)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_cellsize_in_sec'
  character(*), intent(in) :: resl

  character(8) :: unit
  integer :: loc
  integer :: iUnit
  integer :: ios

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n = 0
  do iUnit = 1, 2
    selectcase( iUnit )
    case( 1 )
      unit = 'sec'
    case( 2 )
      unit = 'min'
    endselect

    loc = index(resl, trim(unit))
    if( loc == 0 ) cycle

    if( loc == 1 )then
      call errend('Invalid format: '//str(resl))
    endif
    read(resl(:loc-1), *, iostat=ios) n
    if( ios /= 0 )then
      call errend('Reading failed: '//str(resl))
    endif

    selectcase( unit )
    case( 'sec' )
      continue
    case( 'min' )
      n = n * 60
    endselect

    exit
  enddo

  if( n == 0 )then
    call errend('Failed to get size: '//str(resl))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_cellsize_in_sec
!===============================================================
!
!===============================================================
integer function get_cellsize_ratio(resl_in, resl_out) result(ratio)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_cellsize_ratio'
  character(*), intent(in) :: resl_in
  character(*), intent(in) :: resl_out

  integer :: sz_in, sz_out

  sz_in = get_cellsize_in_sec(resl_in)
  sz_out = get_cellsize_in_sec(resl_out)
  if( mod(sz_out, sz_in) /= 0 )then
    call errend('Cell size of '//str(resl_out)//&
        ' is not a multiple of that of '//str(resl_in)//'.', &
        '', PRCNAM, MODNAM)
  endif
  ratio = sz_out / sz_in
end function get_cellsize_ratio
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
real(8) function apprx_isct_with_meridian(&
    lon0, lat0, lon1, lat1, lon) result(lat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'apprx_isct_with_meridian'
  real(8), intent(in) :: lon0, lat0, lon1, lat1
  real(8), intent(in) :: lon

  if( lon0 == lon1 )then
    call errend('lon0 == lon1', &
        '', PRCNAM, MODNAM)
  elseif( lon < min(lon0,lon1) .or. lon > max(lon0,lon1) )then
    call errend('lon is out of range', &
        '', PRCNAM, MODNAM)
  endif

  if( lon == lon0 )then
    lat = lat0
  else
    lat = (lat1-lat0)*((lon-lon0)/(lon1-lon0)) + lat0
  endif
end function apprx_isct_with_meridian
!===============================================================
!
!===============================================================
real(8) function apprx_isct_with_parallel(&
    lon0, lat0, lon1, lat1, lat) result(lon)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'apprx_isct_with_parallel'
  real(8), intent(in) :: lon0, lat0, lon1, lat1
  real(8), intent(in) :: lat

  if( lat0 == lat1 )then
    call errend('lat0 == lat1', &
        '', PRCNAM, MODNAM)
  elseif( lat < min(lat0,lat1) .or. lat > max(lat0,lat1) )then
    call errend('lat is out of range', &
        '', PRCNAM, MODNAM)
  endif

  if( lat == lat0 )then
    lon = lon0
  else
    lon = (lon1-lon0)*((lat-lat0)/(lat1-lat0)) + lon0
  endif
end function apprx_isct_with_parallel
!===============================================================
!
!===============================================================
end module c1_grid
