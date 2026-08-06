module mod_util
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_uid_int
  public :: get_dgt_uid
  public :: get_fmt_network

  public :: jNode2jPt
  public :: jPt2jNode
  public :: get_entity_length
  public :: intersection_hline
  public :: intersection_vline
  public :: calc_elv

  public :: search_2
  public :: search_nearest_2

  public :: comma_json
  public :: slonlat
  public :: sBBox
  public :: sMeshRange
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface search_2
    module procedure search_2__int4
    module procedure search_2__dble
  end interface

  interface search_nearest_2
    module procedure search_nearest_2__int4
    module procedure search_nearest_2__dble
  end interface
  !-------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------
  public :: nwk_rgn_

  type nwk_rgn_
    character(CLEN_VAR) :: regionName
    integer :: iRegion
    integer :: mEnt
    integer, pointer :: irEnt(:)  !(mEnt)
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Tentative global id: aaaaaaxxyy
! a = index of entity, xx = x-index of tile, yy = y-index of tile
! Digit of entity index is not determined.
! Global ids are finally modified.
!===============================================================
integer(8) function make_uid_int(tx, ty, itEnt) result(uid)
  implicit none
  integer, intent(in) :: tx, ty, itEnt

  uid = itEnt*10000 + ty*100 + tx
end function make_uid_int
!===============================================================
!
!===============================================================
integer function get_dgt_uid(ntEnt) result(dgt_uid)
  implicit none
  integer, intent(in) :: ntEnt

  dgt_uid = dgt(ntEnt) + 4
end function get_dgt_uid
!===============================================================
!
!===============================================================
subroutine get_fmt_network(dgt_irEnt, dgt_uid)
  use c2_strnk_io, only: &
        get_f_tmp_networks_fmt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_fmt_network'
  integer, intent(out) :: dgt_irEnt
  integer, intent(out) :: dgt_uid

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  f = get_f_tmp_networks_fmt()
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) c_, dgt_irEnt
  read(un,*) c_, dgt_uid
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_fmt_network
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
integer function jNode2jPt(jNode, nPt) result(jPt)
  implicit none
  integer, intent(in) :: jNode, nPt

  if( jNode == 1 )then
    jPt = 1
  else
    jPt = nPt
  endif
end function jNode2jPt
!===============================================================
!
!===============================================================
integer function jPt2jNode(jPt) result(jNode)
  implicit none
  integer, intent(in) :: jPt

  if( jPt == 1 )then
    jNode = 1
  else
    jNode = 2
  endif
end function jPt2jNode
!===============================================================
!
!===============================================================
real(8) function get_entity_length(ent) result(leng)
  use c1_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_entity_length'
  type(shp_entity_), intent(in) :: ent

  type(shp_part_), pointer :: part
  integer :: i

  if( ent%nPart > 1 )then
    call errend(msg_unexpected_condition()//&
              '\n  ent%nPart > 1', PRCNAM, MODNAM)
  endif

  part => ent%part(1)

  leng = 0.d0
  do i = 1, part%nPoint-1
    call add(leng, &
             dist_sphere(&
               part%x(i)*d2r, part%y(i)*d2r, &
               part%x(i+1)*d2r, part%y(i+1)*d2r) * EARTH_R)
  enddo
end function get_entity_length
!===============================================================
!
!===============================================================
real(8) function intersection_hline(&
    x0, y0, x1, y1, y) result(x)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_hline'
  real(8), intent(in) :: x0, y0, x1, y1
  real(8), intent(in) :: y

  if( y0 == y1 )then
    call errend('y0 == y1', PRCNAM, MODNAM)
  elseif( y < min(y0,y1) .or. y > max(y0,y1) )then
    call errend('y is out of range', PRCNAM, MODNAM)
  endif

  x = x0 + (x1-x0) * (y-y0) / (y1-y0)
end function intersection_hline
!===============================================================
!
!===============================================================
real(8) function intersection_vline(&
    x0, y0, x1, y1, x) result(y)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intersection_vline'
  real(8), intent(in) :: x0, y0, x1, y1
  real(8), intent(in) :: x

  if( x0 == x1 )then
    call errend('x0 == x1', PRCNAM, MODNAM)
  elseif( x < min(x0,x1) .or. x > max(x0,x1) )then
    call errend('x is out of range', PRCNAM, MODNAM)
  endif

  y = y0 + (y1-y0) * (x-x0) / (x1-x0)
end function intersection_vline
!===============================================================
!
!===============================================================
real(8) function calc_elv(&
    lon, lat, gxs, gxe, gys, gye, elvmap, miss &
) result(elv)
  use lib_math
  use c2_jflw_const
  use c2_jflw_grid, only: &
        center_of_gx, &
        center_of_gy, &
        gxs_of_lon, &
        gxe_of_lon, &
        gys_of_lat, &
        gye_of_lat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_elv'
  real(8), intent(in) :: lon, lat
  integer, intent(in) :: gxs, gxe, gys, gye
  real(4), intent(in) :: elvmap(gxs-2:,gys-2:)
  real(8), intent(in) :: miss

  integer :: gx, gy
  real(8) :: dlon, dlat
  real(4) :: elv_ll, elv_lr, elv_ul, elv_ur
  logical :: mask(3,3)
  integer :: n
  character(32) :: str_elvmin, str_elvmax

  gx = gxs_of_lon(lon)
  gy = gys_of_lat(lat)
  if( lon < center_of_gx(gx) ) gx = gx - 1
  if( lat < center_of_gy(gy) ) gy = gy + 1
  dlon = lon - center_of_gx(gx)
  dlat = lat - center_of_gy(gy)
  if( dlon < 0.d0 .or. dlon > GRIDSIZE_LON .or. &
      dlat < 0.d0 .or. dlat > GRIDSIZE_LAT )then
    call errend(msg_unexpected_condition()//&
      '\n  dlon not in [0, GRIDSIZE_LON] or dlat not in [0, GRIDSIZE_LAT]'//&
      '\ndlon: '//str(dlon)//' dlat: '//str(dlat)//&
      '\nGRIDSIZE_LON: '//str(GRIDSIZE_LON)//' GRIDSIZE_LAT: '//str(GRIDSIZE_LAT), &
        '', PRCNAM, MODNAM)
  endif

  elv_ll = elvmap(gx  , gy+1)
  elv_lr = elvmap(gx+1, gy+1)
  elv_ul = elvmap(gx  , gy  )
  elv_ur = elvmap(gx+1, gy  )

  if( elv_ll == ELV_MISS )then
    mask = elvmap(gx-1:gx+1,gy:gy+2) /= ELV_MISS
    if( any(mask) )then
      elv_ll = sum(elvmap(gx-1:gx+1,gy:gy+2), mask=mask) / count(mask)
    endif
  endif
  if( elv_lr == ELV_MISS )then
    mask = elvmap(gx:gx+2,gy:gy+2) /= ELV_MISS
    if( any(mask) )then
      elv_lr = sum(elvmap(gx:gx+2,gy:gy+2), mask=mask) / count(mask)
    endif
  endif
  if( elv_ul == ELV_MISS )then
    mask = elvmap(gx-1:gx+1,gy-1:gy+1) /= ELV_MISS
    if( any(mask) )then
      elv_ul = sum(elvmap(gx-1:gx+1,gy-1:gy+1), mask=mask) / count(mask)
    endif
  endif
  if( elv_ur == ELV_MISS )then
    mask = elvmap(gx:gx+2,gy-1:gy+1) /= ELV_MISS
    if( any(mask) )then
      elv_ur = sum(elvmap(gx:gx+2,gy-1:gy+1), mask=mask) / count(mask)
    endif
  endif

  n = count((/elv_ll, elv_lr, elv_ul, elv_ur/) /= ELV_MISS)
  if( n == 0 )then
    elv = miss
  elseif( n <= 3 )then
    elv = 0.d0
    if( elv_ll /= ELV_MISS ) call add(elv, real(elv_ll,8))
    if( elv_lr /= ELV_MISS ) call add(elv, real(elv_lr,8))
    if( elv_ul /= ELV_MISS ) call add(elv, real(elv_ul,8))
    if( elv_ur /= ELV_MISS ) call add(elv, real(elv_ur,8))
    call mul(elv, 1.d0/n)
  else
    elv = interp_bilinear(&
        real(elv_ll,8), real(elv_lr,8), real(elv_ul,8), real(elv_ur,8), &
        GRIDSIZE_LON, GRIDSIZE_LAT, dlon, dlat)
    if( elv < min(real(elv_ll,8), real(elv_lr,8), &
                  real(elv_ul,8), real(elv_ur,8)) .or. &
        elv > max(real(elv_ll,8), real(elv_lr,8), &
                  real(elv_ul,8), real(elv_ur,8)) )then
      if( all(elvmap == ELV_MISS) )then
        str_elvmin = '(miss)'
        str_elvmax = '(miss)'
      else
        str_elvmin = str(minval(elvmap, mask=elvmap/=ELV_MISS))
        str_elvmax = str(maxval(elvmap, mask=elvmap/=ELV_MISS))
      endif
      call errend('elv('//str((/elv_ll,elv_lr,elv_ul,elv_ur/),'es10.3',',')//&
                  ') -> '//str(elv,'es10.3')//&
          ' (dlon,dlat): ('//str((/dlon,dlat/),'es10.3',',')//')'//&
          ' (lonsize,latsize): ('//str((/GRIDSIZE_LON,GRIDSIZE_LAT/),'es10.3',',')//')'//&
        '\nxy: '//str((/gxs,gxe,gys,gye/),8,', ')//&
        '\nelv min: '//str(str_elvmin)//', max: '//(str_elvmax), &
          '', PRCNAM, MODNAM)
    endif
  endif
end function calc_elv
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
integer function search_2__int4(&
      x, y, arrx, arry, is, ie) result(info)
  implicit none
  integer(4), intent(in) :: x, y
  integer(4), intent(in) :: arrx(:), arry(:)
  integer, intent(out) :: is, ie

  integer :: iis, iie, i0

  info = 0

  call search_nearest(x, arrx, is, ie)
  if( is == 0 .or. ie == size(arrx)+1 )then
    info = 1
    return
  elseif( .not. any(arrx(is:ie) == x) )then
    info = 2
    return
  endif

  call search_nearest(y, arry(is:ie), iis, iie)
  if( iis == 0 .or. iie == ie-is+2 )then
    info = 3
    return
  elseif( .not. any(arry(iis+is-1:iie+is-1) == y) )then
    info = 4
    return
  endif
  i0 = is - 1
  is = iis + i0
  ie = iie + i0
end function search_2__int4
!===============================================================
!
!===============================================================
integer function search_2__dble(&
      x, y, arrx, arry, is, ie) result(info)
  implicit none
  real(8), intent(in) :: x, y
  real(8), intent(in) :: arrx(:), arry(:)
  integer, intent(out) :: is, ie

  integer :: iis, iie, i0

  info = 0

  call search_nearest(x, arrx, is, ie)
  if( is == 0 .or. ie == size(arrx)+1 )then
    info = 1
    return
  elseif( .not. any(arrx(is:ie) == x) )then
    info = 2
    return
  endif

  call search_nearest(y, arry(is:ie), iis, iie)
  if( iis == 0 .or. iie == ie-is+2 )then
    info = 3
    return
  elseif( .not. any(arry(iis+is-1:iie+is-1) == y) )then
    info = 4
    return
  endif
  i0 = is - 1
  is = iis + i0
  ie = iie + i0
end function search_2__dble
!===============================================================
! arrx and arry has been sorted by (1) x and (2) y
!===============================================================
subroutine search_nearest_2__int4(&
    x, y, arrx, arry, is, ie, stat)
  implicit none
  integer(4), intent(in) :: x, y
  integer(4), intent(in) :: arrx(:), arry(:)
  integer, intent(out) :: is, ie
  integer, intent(out) :: stat

  integer :: iis, iie

  stat = 0

  call search_nearest(x, arrx, is, ie)
  if( ie < 1 .or. is > size(arrx) )then
    stat = 1
    return
  endif

  call search_nearest(y, arry(is:ie), iis, iie)
  if( iie < 1 .or. iis > ie-is+1 )then
    stat = 2
    return
  endif
  iis = iis + is - 1
  iie = iie + is - 1
  is = iis
  ie = iie
end subroutine search_nearest_2__int4
!===============================================================
!
!===============================================================
subroutine search_nearest_2__dble(&
    x, y, arrx, arry, is, ie, stat)
  implicit none
  real(8), intent(in) :: x, y
  real(8), intent(in) :: arrx(:), arry(:)
  integer, intent(out) :: is, ie
  integer, intent(out) :: stat

  integer :: iis, iie

  stat = 0

  call search_nearest(x, arrx, is, ie)
  if( ie < 1 .or. is > size(arrx) )then
    stat = 1
    return
  endif

  call search_nearest(y, arry(is:ie), iis, iie)
  if( iie < 1 .or. iis > ie-is+1 )then
    stat = 2
    return
  endif
  iis = iis + is - 1
  iie = iie + is - 1
  is = iis
  ie = iie
end subroutine search_nearest_2__dble
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
function comma_json(i, imax) result(res)
  implicit none
  integer, intent(in) :: i, imax
  character(:), allocatable :: res

  allocate(character(1) :: res)
  if( i == imax )then
    res = ''
  else
    res = ','
  endif
end function comma_json
!===============================================================
!
!===============================================================
character(26) function slonlat(lon, lat) result(res)
  implicit none
  real(8), intent(in) :: lon, lat

  res = '('//str(lon,'f12.7')//','//str(lat,'f11.7')//')'
end function slonlat
!===============================================================
!
!===============================================================
function sBBox(west, east, south, north) result(res)
  implicit none
  real(8), intent(in) :: west, east, south, north
  character(:), allocatable ::res

  allocate(character(1) :: res)
  res = '['//str((/west,east/),'f12.7',', ')//&
        ', '//str((/south,north/),'f11.7',', ')//']'
end function sBBox
!===============================================================
!
!===============================================================
function sMeshRange(gxs, gxe, gys, gye) result(res)
  use c2_jflw_const
  implicit none
  integer, intent(in) :: gxs, gxe, gys, gye
  character(:), allocatable :: res

  res = '['//str((/gxs,gxe/),DGT_GXY,':')//&
        ','//str((/gys,gye/),DGT_GXY,':')//']'
end function sMeshRange
!===============================================================
!
!===============================================================
end module mod_util
