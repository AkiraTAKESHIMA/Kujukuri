module c1_util
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_io
  use c1_const
  use c1_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: slonlat
  public :: sBBox

  public :: clear_cmn_network
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_util'
  !-------------------------------------------------------------

!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function slonlat(lon, lat) result(s)
  implicit none
  real(8), intent(in) :: lon, lat
  character(:), allocatable :: s

  allocate(character(1) :: s)
  s = '('//str(lon,'f12.7')//','//str(lat,'f11.7')//')'
end function slonlat
!===============================================================
!
!===============================================================
function sBBox(&
    west, east, south, north, &
    dc, alon, alat, &
    d, b)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'sBBox'
  real(8), intent(in) :: west, east, south, north
  integer, intent(in), optional :: dc
  integer, intent(in), optional :: alon, alat
  character(*), intent(in), optional :: d
  character(*), intent(in), optional :: b
  character(:), allocatable :: sBBox

  integer :: dc_, alon_, alat_
  character(:), allocatable :: d_, b_, b2
  character(CLEN_WFMT) :: wfmt_lon, wfmt_lat

  dc_ = 7
  alon_ = 4
  alat_ = 3
  allocate(character(2) :: d_)
  d_ = ', '
  allocate(character(1) :: b_)
  b_ = '('
  if( present(dc) ) dc_ = dc
  if( present(alon) ) alon_ = alon
  if( present(alat) ) alat_ = alat
  if( present(d) ) d_ = d
  if( present(b) ) b_ = b

  selectcase( b_ )
  case( '' )
    b2 = ''
  case( '(' )
    b2 = ')'
  case( '[' )
    b2 = ']'
  case default
    call errend(msg_invalid_value('b', b))
  endselect

  wfmt_lon = 'f'//str(dc_+alon_+1)//'.'//str(dc_)
  wfmt_lat = 'f'//str(dc_+alat_+1)//'.'//str(dc_)
  allocate(character(1) :: sBBox)
  sBBox = b_//str((/west,east/),wfmt_lon,d_)//d_//&
          str((/south,north/),wfmt_lat,d_)//b2
end function sBBox
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
subroutine clear_cmn_network(nwk)
  implicit none
  type(cmn_network_), intent(inout) :: nwk

  nwk%uid = ''
  deallocate(nwk%uid)

  nwk%nCh = 0
  nwk%nWsys = 0
  nwk%nNode = 0
  deallocate(nwk%wsys_)
  deallocate(nwk%channel_)
  if( associated(nwk%node_) ) deallocate(nwk%node_)
end subroutine clear_cmn_network
!===============================================================
!
!===============================================================
end module c1_util
