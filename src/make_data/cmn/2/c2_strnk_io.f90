module c2_strnk_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c2_strnk_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: region_idx2str
  public :: region_str2idx

  public :: read_strrank_all
  public :: show_strrank_all

  public :: get_f_stream_shp
  public :: get_f_stream_dbf
  public :: get_f_rivernode_shp
  public :: get_f_rivernode_dbf
  public :: get_f_lst_tiled_idx
  public :: get_f_lst_tiled_uid
  public :: get_f_tmp_networks_fmt
  public :: get_f_tmp_networks_lst
  public :: get_f_tmp_network_entity
  public :: get_f_tmp_network_channel
  public :: get_f_tmp_network_separation
  public :: get_f_lst_networks_channel
  public :: get_f_lst_networks_chpix
  public :: get_f_lst_networks_mesh
  public :: get_f_network_channel
  public :: get_f_network_chpix
  public :: get_f_network_mesh
  public :: get_f_entdown
  public :: get_f_isct_basin
  public :: get_f_eval_basin
  public :: get_f_lst_eval_basin
  public :: get_f_isct_wsys
  public :: get_f_incons_isct_wsys

  public :: slonlat
  public :: sBBox
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  character(8), parameter :: WFMT_LON = 'f12.7'
  character(8), parameter :: WFMT_LAT = 'f11.7'
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_strrank_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function region_idx2str(i) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'region_idx2str'
  integer, intent(in) :: i
  character(:), allocatable :: res

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: res)

  selectcase( i )
  case( 0 )
    res = trim(REGION_ALL)
  case( 1 )
    res = trim(REGION_HOKKAIDO)
  case( 2 )
    res = trim(REGION_HONSHU)
  case( 3 )
    res = trim(REGION_SHIKOKU)
  case( 4 )
    res = trim(REGION_KYUSHU)
  case( 5 )
    res = trim(REGION_OKINAWA)
  case default
    call errend(msg_invalid_value('i', i))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function region_idx2str
!===============================================================
!
!===============================================================
integer function region_str2idx(s) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'region_str2idx'
  character(*), intent(in) :: s

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( s )
  case( REGION_ALL )
    res = 0
  case( REGION_HOKKAIDO )
    res = 1
  case( REGION_HONSHU )
    res = 2
  case( REGION_SHIKOKU )
    res = 3
  case( REGION_KYUSHU )
    res = 4
  case( REGION_OKINAWA )
    res = 5
  case default
    call errend(msg_invalid_value('s', s))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function region_str2idx
!===============================================================
!
!===============================================================
subroutine read_strrank_all(wsCode, shp)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_strrank_all'
  character(*), intent(in) :: wsCode
  type(shp_)  , intent(out) :: shp

  character(CLEN_PATH) :: f_str

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_str = joined(DIR_STR, trim(wsCode)//'/Stream.shp')
  call logmsg('Reading '//str(f_str))
  call traperr( shp_open(f_str) )
  call traperr( shp_get_all(shp) )
  call traperr( shp_close() )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_strrank_all
!===============================================================
!
!===============================================================
subroutine show_strrank_all(s)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'show_strrank_all'
  type(shp_), intent(in) :: s

  type(shp_entity_), pointer :: ent
  type(shp_part_), pointer :: part
  integer :: iEnt, iPart
  character(CLEN_WFMT), parameter :: WFMT_LONLAT = 'f12.7'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iEnt = 1, s%nEntity
    if( s%nEntity > 7 .and. iEnt > 3 .and. iEnt < s%nEntity-2 )then
      if( iEnt == 4 )then
        call logmsg('...')
      endif
      cycle
    endif

    ent => s%entity(iEnt)
    call logmsg('Entity '//str(iEnt)//': nPart='//str(ent%nPart))
    call setlog('+x2')
    do iPart = 1, ent%nPart
      part => ent%part(iPart)
      call logmsg('Part '//str(iPart)//': nPoint='//str(part%nPoint))
      if( part%nPoint > 7 )then
        call logmsg('  lon '//str(part%x(:3),WFMT_LONLAT,', ')//&
                   ', ..., '//str(part%x(part%nPoint-2:),WFMT_LONLAT,', '))
        call logmsg('  lat '//str(part%y(:3),WFMT_LONLAT,', ')//&
                   ', ..., '//str(part%y(part%nPoint-2:),WFMT_LONLAT,', '))
      else
        call logmsg(str(part%x,WFMT_LONLAT,', '))
        call logmsg('  lon '//str(part%x,WFMT_LONLAT,', '))
        call logmsg('  lat '//str(part%y,WFMT_LONLAT,', '))
      endif
    enddo  ! iPart/
    call setlog('-x2')
  enddo  ! iEnt/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine show_strrank_all
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
character(CLEN_PATH) function get_f_stream_shp(region) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_stream_shp'
  character(*), intent(in) :: region

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( region )
  case( REGION_HOKKAIDO, &
        REGION_HONSHU  , &
        REGION_SHIKOKU , &
        REGION_KYUSHU  , &
        REGION_OKINAWA )
    f = joined(DIR_ORG, str(region)//'/StrRank-'//str(region)//'_Stream.shp')
  case default
    call errend(msg_invalid_value('region', region))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_stream_shp
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_stream_dbf(region) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_stream_dbf'
  character(*), intent(in) :: region

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( region )
  case( REGION_HOKKAIDO, &
        REGION_HONSHU  , &
        REGION_SHIKOKU , &
        REGION_KYUSHU  , &
        REGION_OKINAWA )
    f = joined(DIR_ORG, str(region)//'/StrRank-'//str(region)//'_Stream.dbf')
  case default
    call errend(msg_invalid_value('region', region))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_stream_dbf
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_rivernode_shp(region) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_rivernode_shp'
  character(*), intent(in) :: region

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( region )
  case( REGION_HOKKAIDO, &
        REGION_HONSHU  , &
        REGION_SHIKOKU , &
        REGION_KYUSHU  , &
        REGION_OKINAWA )
    f = joined(DIR_ORG, str(region)//'/StrRank-'//str(region)//'_RiverNode.shp')
  case default
    call errend(msg_invalid_value('region', region))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_rivernode_shp
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_rivernode_dbf(region) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_rivernode_dbf'
  character(*), intent(in) :: region

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( region )
  case( REGION_HOKKAIDO, &
        REGION_HONSHU  , &
        REGION_SHIKOKU , &
        REGION_KYUSHU  , &
        REGION_OKINAWA )
    f = joined(DIR_ORG, str(region)//'/StrRank-'//str(region)//'_RiverNode.dbf')
  case default
    call errend(msg_invalid_value('region', region))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_rivernode_dbf
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_tiled_idx(tileName) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_tiled_idx'
  character(*), intent(in) :: tileName

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'lst_tiled_idx/'//trim(tileName)//'.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_tiled_idx
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_tiled_uid(tileName) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_tiled_uid'
  character(*), intent(in) :: tileName

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'lst_tiled_uid/'//trim(tileName)//'.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_tiled_uid
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_tmp_networks_fmt() result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_tmp_networks_fmt'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'tmp_network/format.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_tmp_networks_fmt
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_tmp_networks_lst() result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_tmp_networks_lst'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'tmp_network/all.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_tmp_networks_lst
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_tmp_network_entity(uid) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_tmp_network_entity'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'tmp_network_entity_node/'//str(uid)//'.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_tmp_network_entity
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_tmp_network_channel(uid, typ) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_tmp_network_channel'
  character(*), intent(in) :: uid
  character(*), intent(in) :: typ

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( typ )
  case( 'sbin', 'json' )
    f = joined(DIR_PRD, 'tmp_network_channel/'//str(typ)//'/'//str(uid)//'.'//str(typ))
  case default
    call errend(msg_invalid_value('typ', typ))
  endselect

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_tmp_network_channel
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_tmp_network_separation(uid) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_network_separation'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'tmp_network_separation/'//str(uid)//'.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_tmp_network_separation
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_networks_channel() result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_networks_channel'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'network_channel/all.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_networks_channel
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_networks_chpix() result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_networks_chpix'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'network_chpix/all.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_networks_chpix
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_networks_mesh(&
    resl) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_networks_mesh'
  character(*), intent(in) :: resl

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'network_mesh/'//str(resl)//'/all.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_networks_mesh
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_network_channel(uid, typ) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_network_channel'
  character(*), intent(in) :: uid
  character(*), intent(in) :: typ

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( typ )
  case( 'sbin', 'json' )
    f = joined(DIR_PRD, 'network_channel/'//str(typ)//'/'//str(uid)//'.'//str(typ))
  case default
    call errend(msg_invalid_value('typ', typ))
  endselect

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_network_channel
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_network_chpix(uid) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_network_chpix'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'network_chpix/'//str(uid)//'.sbin')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_network_chpix
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_network_mesh(&
    resl, uid) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_network_chpix'
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'network_mesh/'//str(resl)//'/'//str(uid)//'.bin')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_network_mesh
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_entdown(uid) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_entdown'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'entdown/'//str(uid)//'.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_entdown
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_isct_basin(uid) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_isct_basin'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'isct_basin/'//str(uid)//'.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_isct_basin
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_eval_basin(uid) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_eval_basin'
  character(*), intent(in) :: uid

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'eval_basin/'//str(uid)//'.json')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_eval_basin
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_eval_basin() result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_eval_basin'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_PRD, 'eval_basin/summary.txt')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_eval_basin
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_isct_wsys(region) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_isct_wsys'
  character(*), intent(in) :: region

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( region )
  case( REGION_HOKKAIDO, &
        REGION_HONSHU  , &
        REGION_SHIKOKU , &
        REGION_KYUSHU  , &
        REGION_OKINAWA , &
        REGION_ALL     )
    f = joined(DIR_PRD, 'isct_wsys/'//str(region)//'.txt')
    call traperr( mkdir(dirname(f)) )
  case default
    call errend(msg_invalid_value('region', region))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_isct_wsys
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_incons_isct_wsys(region) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_incons_isct_wsys'
  character(*), intent(in) :: region

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( region )
  case( REGION_HOKKAIDO, &
        REGION_HONSHU  , &
        REGION_SHIKOKU , &
        REGION_KYUSHU  , &
        REGION_OKINAWA , &
        REGION_ALL     )
    f = joined(DIR_PRD, 'incons_isct_wsys/'//str(region)//'.txt')
    call traperr( mkdir(dirname(f)) )
  case default
    call errend(msg_invalid_value('region', region))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_incons_isct_wsys
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
function slonlat(lon, lat) result(res)
  implicit none
  real(8), intent(in) :: lon, lat
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = '('//str(lon,WFMT_LON)//','//str(lat,WFMT_LAT)//')'
end function slonlat
!===============================================================
!
!===============================================================
function sBBox(west, east, south, north, dlm) result(res)
  implicit none
  real(8), intent(in) :: west, east, south, north
  character(*), intent(in), optional :: dlm
  character(:), allocatable :: res

  character(:), allocatable :: dlm_

  allocate(character(1) :: dlm_)
  dlm_ = ','
  if( present(dlm) ) dlm_ = dlm

  allocate(character(1) :: res)
  res = str((/west,east/),WFMT_LON,dlm_)//&
        dlm_//str((/south,north/),WFMT_LAT,dlm_)
end function sBBox
!===============================================================
!
!===============================================================
end module c2_strnk_io
