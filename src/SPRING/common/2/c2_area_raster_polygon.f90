module c2_area_raster_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use c1_const
  use c1_type_gs
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize
  public :: initialize_zone
  public :: finalize_zone
  public :: get_dhv_polygon

  public :: calc_iarea

  public :: update_rt1d
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_area_raster_polygon'

  ! Flags
  !-------------------------------------------------------------
  logical :: is_initialized      = .false.
  logical :: is_initialized_zone = .false.

  character(:), allocatable :: sname, tname

  ! For the whole area
  !-------------------------------------------------------------
  ! For raster
  integer(8) :: ndh, dhi, dhf, ndv, dvi, dvf
  real(8)    :: dlon, dlat

  real(8), allocatable :: dlons_all(:) !(-1:ndh+1)
  real(8), allocatable :: dlats_all(:) !(-1:ndv+1)

  real(8), allocatable :: dara_1rad(:) !(ndv)
  real(8), allocatable :: dara(:)

  ! Missing values
  integer(8) :: sidx_miss
  integer(8) :: tidx_miss
  real(8)    :: lonlat_miss

  ! For updating a remapping table
  integer(8), allocatable :: sidx_prev(:)
  integer(8), allocatable :: ij_prev(:)

  ! For each zone
  !-------------------------------------------------------------
  real(8), allocatable :: dlons(:) !(sdhi-2:sdhf+1)
  real(8), allocatable :: dlats(:) !(sdvi-2:sdvf+1)

  real(8), allocatable :: cos_dlons(:) !(sdhi-2:sdhf+1)
  real(8), allocatable :: sin_dlons(:)

  real(8), allocatable :: cos_dlats(:) !(sdvi-2:sdvf+1)
  real(8), allocatable :: sin_dlats(:)

  real(8), allocatable :: clons(:) !(sdhi-2:sdhf+1)
  real(8), allocatable :: clats(:)

  real(8), allocatable :: cos_clons(:) !(sdhi-2:sdhf+1)
  real(8), allocatable :: sin_clons(:)

  real(8), allocatable :: clons_diff(:) !(sdhi-2:sdhf+1)

  integer(1), save :: s_region_type
  real(8)   , save :: swest, seast, ssouth, snorth
  integer(8), save :: sdhi, sdhf, sdvi, sdvf
  integer(8), save :: tdhi_buf, tdhf_buf, tdvi_buf, tdvf_buf
  integer(8), save :: tdhi, tdhf, tdvi, tdvf

  integer(8) :: tidx
  !-------------------------------------------------------------
  ! For debugging
  !-------------------------------------------------------------
  logical :: debug
  logical :: debug_s
  logical :: debug_t
  integer(8) :: sidx_debug
  integer(8) :: tidx_debug
  character(clen_wfmt), parameter :: wfmt_deg = 'f12.7'
  character(clen_wfmt), parameter :: wfmt_dble = 'es24.16'
  integer, save :: dgt_hv
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function initialize(sr, tp, make_rt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'initialize'
  type(gs_raster_) , intent(in) :: sr
  type(gs_polygon_), intent(in) :: tp
  logical, intent(in), optional :: make_rt

  logical :: make_rt_

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( is_initialized )then
    info = 1
    call errret('The module has already been initialized.')
    return
  endif

  is_initialized = .true.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_rt_ = .false.
  if( present(make_rt) ) make_rt_ = make_rt
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: sname, tname)
  sname = trim(sr%nam)
  tname = trim(tp%nam)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dlon = sr%lonwidth(sr%hi)
  dlat = sr%latwidth(sr%vi)

  dhi = sr%hi
  dhf = sr%hf
  dvi = sr%vi
  dvf = sr%vf

  allocate(dlons_all(sr%hi-1_8:sr%hf))
  allocate(dlats_all(sr%vi-1_8:sr%vf))
  call cpval(sr%lon, dlons_all)
  call cpval(sr%lat, dlats_all)

  allocate(dara_1rad(sr%vi:sr%vf))
  allocate(dara(sr%vi:sr%vf))
  dara_1rad(:) = area_sphere_rect(dlats_all(sr%vi-1:sr%vf-1), dlats_all(sr%vi:sr%vf))
  dara(:) = dara_1rad(:) * dlon

  ! For updating a remapping table
  if( make_rt_ )then
    allocate(sidx_prev(tp%nij))
    sidx_prev(:) = sr%idx_miss

    allocate(ij_prev(tp%nij))
  endif

  ! Missing values
  sidx_miss = sr%idx_miss
  tidx_miss = tp%idx_miss
  lonlat_miss = tp%coord_miss_s

  ! For debugging
  debug_s = sr%debug
  sidx_debug = sr%idx_debug

  debug_t = tp%debug
  tidx_debug = tp%idx_debug

  debug = debug_s .or. debug_t
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function initialize
!===============================================================
!
!===============================================================
integer(4) function finalize() result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize'

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. is_initialized )then
    info = 1
    call errret('The module has not been initialized.')
    return
  endif

  is_initialized = .false.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(sname, tname)

  deallocate(dlons_all)
  deallocate(dlats_all)

  deallocate(dara_1rad)
  deallocate(dara)

  if( allocated(sidx_prev) ) deallocate(sidx_prev)
  if( allocated(ij_prev) ) deallocate(ij_prev)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function finalize
!===============================================================
!
!===============================================================
integer(4) function initialize_zone(srz) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'initialize_zone'
  type(raster_zone_), intent(in) :: srz

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0
  if( is_initialized_zone )then
    info = 1
    return
  endif
  is_initialized_zone = .true.
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_hv = dgt(max(srz%nh,srz%nv))
  !-------------------------------------------------------------
  ! Set const.
  !-------------------------------------------------------------
  call logmsg('Setting the constant values')

  ! module
  ndh = srz%nh
  ndv = srz%nv

  swest  = srz%west
  seast  = srz%east
  ssouth = srz%south
  snorth = srz%north

  sdhi = int(srz%hi,4)
  sdhf = int(srz%hf,4)
  sdvi = int(srz%vi,4)
  sdvf = int(srz%vf,4)

  s_region_type = srz%region_type

  ! Coords. of pixel lines
  allocate(dlons(sdhi-2:sdhf+1))
  allocate(dlats(sdvi-2:sdvf+1))
  dlons(sdhi:sdhf-1) = dlons_all(sdhi:sdhf-1)
  dlats(sdvi:sdvf-1) = dlats_all(sdvi:sdvf-1)
  dlons(sdhi-1) = swest
  dlons(sdhf  ) = seast
  dlats(sdvi-1) = ssouth
  dlats(sdvf  ) = snorth
  dlons(sdhi-2) = -9999.d0
  dlons(sdhf+1) =  9999.d0
  dlats(sdvi-2) = -9999.d0
  dlats(sdvf+1) =  9999.d0

  allocate(cos_dlons(sdhi-2:sdhf+1))
  allocate(sin_dlons(sdhi-2:sdhf+1))
  cos_dlons(:) = 0.d0
  sin_dlons(:) = 0.d0
  cos_dlons(sdhi-1:sdhf) = cos(dlons(sdhi-1:sdhf))
  sin_dlons(sdhi-1:sdhf) = sin(dlons(sdhi-1:sdhf))

  allocate(cos_dlats(sdvi-1:sdvf))
  allocate(sin_dlats(sdvi-1:sdvf))
  cos_dlats(:) = cos(dlats(sdvi-1:sdvf))
  sin_dlats(:) = sin(dlats(sdvi-1:sdvf))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Setting the changable fields')

  allocate(clons(sdhi-2:sdhf+1))
  allocate(clons_diff(sdhi-2:sdhf+1))
  allocate(cos_clons(sdhi-2:sdhf+1))
  allocate(sin_clons(sdhi-2:sdhf+1))
  allocate(clats(sdhi-2:sdhf+1))

  clons(:) = dlons(:)
  clons_diff(:) = dlon
  cos_clons(:) = cos_dlons(:)
  sin_clons(:) = sin_dlons(:)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function initialize_zone
!===============================================================
!
!===============================================================
integer(4) function finalize_zone() result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize_zone'

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. is_initialized_zone )then
    info = 1
    call errret('The zone has already been initialized.')
    return
  endif
  is_initialized_zone = .false.
  !-------------------------------------------------------------
  deallocate(dlons)
  deallocate(dlats)

  deallocate(cos_dlons)
  deallocate(sin_dlons)

  deallocate(cos_dlats)
  deallocate(sin_dlats)

  deallocate(clons)
  deallocate(clons_diff)
  deallocate(cos_clons)
  deallocate(sin_clons)
  deallocate(clats)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function finalize_zone
!===============================================================
!
!===============================================================
subroutine get_dhv_polygon(dhi, dhf, dvi, dvf)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_dhv_polygon'
  integer(8), intent(out) :: dhi, dhf, dvi, dvf

  dhi = tdhi
  dhf = tdhf
  dvi = tdvi
  dvf = tdvf
end subroutine get_dhv_polygon
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
integer(4) function calc_iarea(&
    iarea, tp, is_iarea_updated, smskmap) result(info)
  use c1_gs_util, only: &
        print_polygon
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_iarea'
  real(8)       , pointer     :: iarea(:,:)  ! out
  type(polygon_), intent(in)  :: tp
  logical       , intent(out) :: is_iarea_updated
  logical(1)    , pointer, optional :: smskmap(:,:)  ! in

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_iarea_updated = .false.

  if( tp%n == 0 )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  tidx = tp%idx
  if( tidx == tidx_miss )then
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! [DEBUG]
  !-------------------------------------------------------------
  if( debug_t )then
    if( tidx /= tidx_debug )then
      call logret(PRCNAM, MODNAM)
      return
    endif
  endif
  !-----------------------------------------------------------
  ! Judge if bboxes intersect
  !-----------------------------------------------------------
  selectcase( s_region_type )
  case( REGION_TYPE_GLOBAL )
    continue
  case( REGION_TYPE_CYCLIC )
    if( tp%north <= ssouth .or. tp%south >= snorth )then
      if( debug_t )then
        call logmsg('Polygon does not intersect with the raster grid.')
      endif
      call logret(PRCNAM, MODNAM)
      return
    endif
  case( REGION_TYPE_REGIONAL )
    if( .not. bboxes_intersect(&
                tp%south, tp%north, tp%west, tp%east, (tp%pos == polygon_position_lon0), &
                ssouth, snorth, swest, seast, .false.) )then
      if( debug_t )then
        call logmsg('Polygon does not intersect with the raster grid.')
      endif
      call logret(PRCNAM, MODNAM)
      return
    endif
  case( REGION_TYPE_UNDEF )
    info = 1
    call errret(str(msg_unexpected_condition())//&
              '\ns_region_type == '//str_region_type_long(s_region_type))
    return
  case default
    info = 1
    call errret(msg_invalid_value('s_region_type', s_region_type))
    return
  endselect
  !-------------------------------------------------------------
  ! Calc. ranges of raster with buffer
  ! dlons(tdhi_buf-1) <= tp%west  <  dlons(tdhi_buf)
  ! dlons(tdhf_buf-1) <  tp%east  <= dlons(tdhf_buf)
  ! dlats(tdvi_buf-1) <= tp%south <  dlats(tdvi_buf)
  ! dlats(tdvf_buf-1) <  tp%north <= dlats(tdvf_buf)
  !-------------------------------------------------------------
  selectcase( tp%pos )
  case( POLYGON_POSITION_NORMAL, &
        POLYGON_POSITION_LON0 )
    if( swest <= tp%west .and. tp%west <= seast )then
      tdhi_buf = dh_ge_west_lt_east(tp%west, sdhi-1_8, sdhf+1_8)
    else
      tdhi_buf = sdhi - 1_8
    endif

    if( swest <= tp%east .and. tp%east <= seast )then
      tdhf_buf = dh_gt_west_le_east(tp%east, sdhi-1_8, sdhf+1_8)
    else
      tdhf_buf = sdhf + 1_8
    endif
  case( POLYGON_POSITION_POLAR )
    tdhi_buf = sdhi - 1_8
    tdhf_buf = sdhf + 1_8
  case default
    info = 1
    call errret(msg_invalid_value('tp%pos', tp%pos))
    return
  endselect

  tdvi_buf = max(dv_ge_south_lt_north(tp%south, sdvi-1_8, sdvf+1_8), sdvi-1_8)
  tdvf_buf = min(dv_gt_south_le_north(tp%north, sdvi-1_8, sdvf+1_8), sdvf+1_8)
  !-------------------------------------------------------------
  ! Calc. ranges of raster without buffer
  !-------------------------------------------------------------
  tdhi = max(tdhi_buf, sdhi)
  tdhf = min(tdhf_buf, sdhf)
  tdvi = max(tdvi_buf, sdvi)
  tdvf = min(tdvf_buf, sdvf)
  !-------------------------------------------------------------
  ! [DEBUG] Skip if no raster grid was a target of debugging
  !-------------------------------------------------------------
  if( debug_s )then
    if( .not. any(smskmap(tdhi:tdhf,tdvi:tdvf)) )then
      call logret(PRCNAM, MODNAM)
      return
    endif
  endif
  !-------------------------------------------------------------
  ! [DEBUG] Print
  !-------------------------------------------------------------
  if( debug )then
    if( .not. debug_t )then
      call print_polygon(tp, lonlat_miss)
    endif
    call logmsg('s ('//str((/sdhi,sdhf/),':')//','//str((/sdvi,sdvf/),':')//')')
    call debug_print_range('Range with buffer', tdhi_buf, tdhf_buf, tdvi_buf, tdvf_buf)
    call debug_print_range('Range without buffer', tdhi, tdhf, tdvi, tdvf)
    call logmsg('s is valid: '//str(any(smskmap(tdhi:tdhf,tdvi:tdvf))))
  endif
  !-------------------------------------------------------------
  ! Skip if there was no valid raster
  !-------------------------------------------------------------
  if( present(smskmap) )then
    if( .not. any(smskmap(tdhi:tdhf,tdvi:tdvf)) )then
      call logret(PRCNAM, MODNAM)
      return
    endif
  endif
  !-------------------------------------------------------------
  ! Calc. area of the intersection
  !-------------------------------------------------------------
  is_iarea_updated = .true.

  if( calc_iarea_core(iarea, tp) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_iarea
!===============================================================
!
!===============================================================
integer(4) function calc_iarea_core(iarea, tp) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_iarea_core'
  real(8)       , pointer    :: iarea(:,:)  ! out
  type(polygon_), intent(in) :: tp

  integer(4) :: n, nn

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Update for all sides
  !-------------------------------------------------------------
  iarea(tdhi_buf:tdhf_buf,tdvi_buf:tdvf_buf) = 0.d0

  do n = 1, tp%n
    nn = next_cyclic(n,tp%n)

    if( debug )then
      call logmsg('('//str(n)//') '//str(str_arctyp_long(tp%arctyp(n)))//&
                 ' ('//str(str_coords((/tp%lon(n ),tp%lat(n )/),r2d,lonlat_miss,'f12.7'))//&
              ') - ('//str(str_coords((/tp%lon(nn),tp%lat(nn)/),r2d,lonlat_miss,'f12.7'))//')')
    endif

    selectcase( tp%arctyp(n) )
    !-----------------------------------------------------------
    ! Meridian
    case( ARC_TYPE_MERIDIAN )
      continue
    !-----------------------------------------------------------
    ! Normal
    case( ARC_TYPE_NORMAL )
      if( update_area_normal(&
            iarea, &
            tp%lon(n), tp%lat(n), tp%lon(nn), tp%lat(nn), &
            tp%a(n), tp%b(n), tp%c(n), tp%arcpos(n), &
            tp%convex(n), tp%lontop(n), tp%lattop(n)) /= 0 )then
        info = 1
        call errret('@ n = '//str(n))
        return
      endif
    !-----------------------------------------------------------
    ! Case: Parallel
    case( ARC_TYPE_PARALLEL )
      if( update_area_parallel(&
            iarea, tp%lon(n), tp%lon(nn), tp%lat(n), &
            tp%arcpos(n)) /= 0 )then
        info = 1
        call errret('@ n = '//str(n))
        return
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('tp%arctyp('//str(n)//')', tp%arctyp(n)))
      return
    endselect
  enddo  ! n/
  !-------------------------------------------------------------
  ! Update for south pole
  !-------------------------------------------------------------
  if( tp%south == -rad_90deg )then
    if( debug )then
      call logmsg('Updating for south pole')
    endif

    selectcase( tp%pos )
    case( POLYGON_POSITION_POLAR )
      if( update_area_parallel(&
            iarea, &
            rad_0deg, rad_360deg, -rad_90deg, &
            ARC_POSITION_NORMAL) /= 0 )then
        info = 1; call errret(); return
      endif
    case( POLYGON_POSITION_NORMAL )
      if( tp%n_pole == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nSouth bound is -90 deg but vertex is not on the south pole.'//&
                  '\n  idx: '//str(tp%idx))
        return
      endif

      if( update_area_parallel(&
            iarea, &
            tp%lon(prev_cyclic(tp%n_pole,tp%n)), &
            tp%lon(next_cyclic(tp%n_pole,tp%n)), &
            -rad_90deg, &
            ARC_POSITION_NORMAL) /= 0 )then
        info = 1; call errret(); return
      endif
    case( POLYGON_POSITION_LON0 )
      if( tp%n_pole == 0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nSouth bound is -90 deg but vertex is not on the south pole.'//&
                  '\n  idx: '//str(tp%idx))
        return
      endif

      if( update_area_parallel(&
            iarea, &
            tp%lon(tp%n_pole-1), tp%lon(tp%n_pole+1), -rad_90deg, &
            ARC_POSITION_LON0) /= 0 )then
        info = 1; call errret(); return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('tp%pos', tp%pos))
      return
    endselect
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_iarea_core
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
integer(4) function update_area_normal(&
    iarea, &
    lon1, lat1, lon2, lat2, a, b, c, arcstat, &
    convex, lontop, lattop &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_normal'
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: lon1, lat1, lon2, lat2
  real(8)   , intent(in) :: a, b, c
  integer(1), intent(in) :: arcstat
  integer(1), intent(in) :: convex
  real(8)   , intent(in) :: lontop, lattop

  logical :: is_convex
  integer :: sgn_lon
  real(8) :: wlon, wlat, elon, elat
  real(8) :: a_, b_, c_
  integer :: nZones, iZone
  integer(8) :: dh_top
  integer(8) :: ttdhi_zone(2), ttdhf_zone(2)  ! range of arc
  integer(8) :: ttdhi_buf, ttdhf_buf  ! range with buffer
  integer(8) :: ttdhi, ttdhf          ! range without buffer
  integer(8) :: idh

  info = 0
  if( debug ) call logbgn(PRCNAM, MODNAM, '')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_convex = .false.
  dh_top = 0_8

  selectcase( convex )
  case( CONVEX_MONOTONE )
    continue
  case( CONVEX_UPWARD, &
        CONVEX_DOWNWARD )
    if( which_is_western(swest, lontop) == 1 .and. &
        which_is_western(seast, lontop) == 2 )then
      is_convex = .true.
      dh_top = int((lontop - swest) / dlon) + sdhi
    endif
  case( CONVEX_UNDEF )
    call errend(str(msg_unexpected_condition())//&
              '\nconvex == '//str_convex_long(convex))
  case default
    call errend(msg_invalid_value('convex', convex))
  endselect

  if( debug )then
    call logmsg('is_convex '//str(is_convex))
    if( is_convex )then
      call logmsg('  top('//str((/lontop,lattop/)*r2d,'f20.13',', ')//')')
      call logmsg('  dh '//str(dh_top)//' ('//str(dlons(dh_top-1_8:dh_top)*r2d,'f20.13',' ~ ')//')')
    endif
  endif
  !-------------------------------------------------------------
  ! Calc. sign and range of arc.
  !-------------------------------------------------------------
  if( calc_arc_sign_range(&
        arcstat, lon1, lat1, lon2, lat2, &
        sgn_lon, wlon, wlat, elon, elat, &
        nZones, ttdhi_zone, ttdhf_zone) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  if( debug )then
    call logmsg('lon1 '//str(lon1*r2d,'f12.7')//' lon2 '//str(lon2*r2d,'f12.7'))
    call logmsg('wlon '//str(wlon*r2d,'f12.7')//' elon '//str(elon*r2d,'f12.7'))
    call logmsg('nZones '//str(nZones))
    do iZone = 1, nZones
      call logmsg('  ttdh '//str((/ttdhi_zone(iZone),ttdhf_zone(iZone)/),' - '))
    enddo
  endif

  if( nZones == 0 )then
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  a_ = a * sgn_lon
  b_ = b * sgn_lon
  c_ = c * sgn_lon
  !-------------------------------------------------------------
  ! Update area of the intersection
  !-------------------------------------------------------------
  do iZone = 1, nZones
    !-----------------------------------------------------------
    ! Calc. ranges of raster with and without buffer
    !-----------------------------------------------------------
    ttdhi_buf = max(ttdhi_zone(iZone), tdhi_buf)
    ttdhf_buf = min(ttdhf_zone(iZone), tdhf_buf)

    ttdhi = max(ttdhi_buf, tdhi)
    ttdhf = min(ttdhf_buf, tdhf)

    if( debug_s .or. debug_t )then
      call logmsg('sgn_lon '//str(sgn_lon))
      call logmsg('west '//str(wlon*r2d,'f15.10')//' east '//str(elon*r2d,'f15.10'))
      call logmsg('Range of h w/ buffer : ['//str((/ttdhi_buf,ttdhf_buf/),dgt(dhf+1),':')//']')
      call logmsg('           w/o buffer: ['//str((/ttdhi,ttdhf/),dgt(dhf+1),':')//']')
    endif
    !-----------------------------------------------------------
    ! Calc. coords. of intersections with meridians
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = wlon
    clons(ttdhf_buf  ) = elon
    clons_diff(ttdhi_buf) = londiff_rad(clons(ttdhi_buf), clons(ttdhi_buf-1))
    clons_diff(ttdhf_buf) = londiff_rad(clons(ttdhf_buf), clons(ttdhf_buf-1))

    cos_clons(ttdhi_buf-1) = cos(wlon)
    cos_clons(ttdhf_buf)   = cos(elon)
    sin_clons(ttdhi_buf-1) = sin(wlon)
    sin_clons(ttdhf_buf)   = sin(elon)

    if( intersection_sphere_normal_meridian(&
          wlon, wlat, elon, elat, clons(ttdhi_buf:ttdhf_buf-1_8), &
          clats(ttdhi_buf:ttdhf_buf-1_8)) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    clats(ttdhi_buf-1) = wlat
    clats(ttdhf_buf)   = elat

    if( debug )then
      call logmsg('ax + by + cz'//&
                  ' west '//str(a*cos(wlat)*cos(wlon)+b*cos(wlat)*sin(wlon)+c*sin(wlat),'es20.13')//&
                  ' east '//str(a*cos(elat)*cos(elon)+b*cos(elat)*sin(elon)+c*sin(elat),'es20.13'))
      call logmsg('Intersections with meridians'//&
                '\n  west ('//str((/wlon,wlat/)*r2d,'f18.13',', ')//')'//&
                '\n  east ('//str((/elon,elat/)*r2d,'f18.13',', ')//')')
      do idh = ttdhi_buf-1, ttdhf_buf
        call logmsg('  dh '//str(idh)//' ('//str((/clons(idh),clats(idh)/)*r2d,'f18.13',',')//')')
      enddo
    endif
    !-----------------------------------------------------------
    ! Update areas
    !-----------------------------------------------------------
    if( is_convex )then
      do idh = ttdhi_buf, dh_top-1_8
        if( update_area_normal_monotonous(&
              iarea, a_, b_, c_, sgn_lon, idh) /= 0 )then
          info = 1
          call errret('', PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endif
      enddo

      if( lattop > rad_0deg )then
        if( update_area_normal_convex_upward(&
              iarea, a_, b_, c_, sgn_lon, lontop, lattop, dh_top) /= 0 )then
          info = 1
          call errret('', PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endif
      else
        if( update_area_normal_convex_downward(&
              iarea, a_, b_, c_, sgn_lon, lontop, lattop, dh_top) /= 0 )then
          info = 1
          call errret('', PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endif
      endif

      do idh = dh_top+1_8, ttdhf_buf
        if( update_area_normal_monotonous(&
              iarea, a_, b_, c_, sgn_lon, idh) /= 0 )then
          info = 1
          call errret('', PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endif
      enddo
    else
      do idh = ttdhi_buf, ttdhf_buf
        if( update_area_normal_monotonous(&
              iarea, a_, b_, c_, sgn_lon, idh) /= 0 )then
          info = 1
          call errret('', PRCNAM, MODNAM)
          if( debug ) call logret(PRCNAM, MODNAM)
          return
        endif
      enddo
    endif
    !-----------------------------------------------------------
    ! Restore module variables
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = dlons(ttdhi_buf-1)
    clons(ttdhf_buf  ) = dlons(ttdhf_buf)
    clons_diff(ttdhi_buf) = dlon
    clons_diff(ttdhf_buf) = dlon

    cos_clons(ttdhi_buf-1) = cos_dlons(ttdhi_buf-1)
    cos_clons(ttdhf_buf)   = cos_dlons(ttdhf_buf)
    sin_clons(ttdhi_buf-1) = sin_dlons(ttdhi_buf-1)
    sin_clons(ttdhf_buf)   = sin_dlons(ttdhf_buf)
    !-----------------------------------------------------------
  enddo  ! iZone/
  !-------------------------------------------------------------
  if( debug ) call logret(PRCNAM, MODNAM)
end function update_area_normal
!===============================================================
!
!===============================================================
integer(4) function update_area_normal_monotonous(&
    iarea, a, b, c, sgn_lon, dh &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_normal_monotonous'
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh

  integer(8) :: idv
  integer(8) :: dv_west, dv_east

  info = 0
  if( debug ) call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Calc. latitudinal raster indices of the intersections
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <= clats(dh-1) <  dlats(dv_west)
  ! dlats(dv_east-1) <  clats(dh)   <= dlats(dv_east)
  !-------------------------------------------------------------
  if( clats(dh-1_8) < clats(dh) )then
    dv_west = dv_ge_south_lt_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
    dv_east = dv_gt_south_le_north(clats(dh)    , tdvi_buf, tdvf_buf)
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <  clats(dh-1) <= dlats(dv_west)
  ! dlats(dv_east-1) <= clats(dh)   <  dlats(dv_east)
  !-------------------------------------------------------------
  elseif( clats(dh-1_8) > clats(dh) )then
    dv_west = dv_gt_south_le_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
    dv_east = dv_ge_south_lt_north(clats(dh    ), tdvi_buf, tdvf_buf)
  !-------------------------------------------------------------
  else
    dv_west = floor((clats(dh-1_8)-ssouth)/dlat) + sdvi
    dv_east = dv_west
  endif

  dv_west = min(max(dv_west,tdvi_buf),tdvf_buf)
  dv_east = min(max(dv_east,tdvi_buf),tdvf_buf)
  !-------------------------------------------------------------
  ! Return if arc is out of the range of latit.
  !-------------------------------------------------------------
  if( min(dv_west,dv_east) > tdvf )then
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  elseif( max(dv_west,dv_east) < tdvi )then
    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
    enddo
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! Update intersections
  !-------------------------------------------------------------
  if( dv_west == dv_east )then
    if( update_area_normal_monotonous_same(&
          iarea, sgn_lon, dh, dv_west) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  elseif( dv_west < dv_east )then
    if( update_area_normal_monotonous_upward(&
          iarea, a, b, c, sgn_lon, dh, dv_west, dv_east) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  else
    if( update_area_normal_monotonous_downward(&
          iarea, a, b, c, sgn_lon, dh, dv_west, dv_east) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  endif
  !-------------------------------------------------------------
  if( debug ) call logret(PRCNAM, MODNAM)
end function update_area_normal_monotonous
!===============================================================
!
!===============================================================
integer(4) function update_area_normal_monotonous_same(&
    iarea, sgn_lon, dh, dv &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_normal_monotonous_same'
  real(8)   , pointer    :: iarea(:,:)  ! inout
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh
  integer(8), intent(in) :: dv

  integer(8) :: idv
  real(8) :: area_tri, area_rect
  real(8) :: area_isct

  logical :: debug_this  ! for switching

  debug_this = debug

  info = 0
  if( debug_this ) call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( debug_this )then
    call logmsg('dh '//str(dh)//' '//str(clons(dh-1:dh)*r2d,'f20.15',' ~ '))
    call logmsg('dv '//str(dv)//' '//str(dlats(dv-1:dv)*r2d,'f20.15',' ~ '))
    call logmsg('lon_diff '//str(clons_diff(dh)*r2d,'f20.15'))
  endif

  if( dv < tdvi .or. tdvf < dv )then
    if( debug_this )then
      call logmsg('Case: Arc is outside the zone')
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! dv
  !-------------------------------------------------------------
  area_tri = area_sphere_righttri_north_bottom(clats(dh-1_8), clats(dh)) * clons_diff(dh)
  area_rect = area_sphere_rect(max(clats(dh-1_8),clats(dh)), dlats(dv)) * clons_diff(dh)
  area_isct = area_tri + area_rect

  if( debug_this )then
    call logmsg('iarea (+) tri  '//str(area_tri,wfmt_dble))
    call logmsg('      (+) rect '//str(area_rect,wfmt_dble))
    call debug_print_area_change(dh, dv, iarea, area_isct*sgn_lon, '')
  endif

  call add(iarea(dh,dv), area_isct*sgn_lon)
  !-------------------------------------------------------------
  ! dv+1 ~
  !-------------------------------------------------------------
  do idv = dv+1_8, tdvf
    call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
  enddo
  !-------------------------------------------------------------
  if( debug_this ) call logret(PRCNAM, MODNAM)
end function update_area_normal_monotonous_same
!===============================================================
!
!===============================================================
integer(4) function update_area_normal_monotonous_upward(&
    iarea, a, b, c, sgn_lon, dh, dv_west, dv_east &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_normal_monotonous_upward'
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh
  integer(8), intent(in) :: dv_west, dv_east

  integer(8) :: idv
  real(8) :: p1lon, p2lon
  real(8) :: area_tri, area_rect
  real(8) :: area_isct

  logical :: debug_this  ! for switching

  debug_this = debug

  info = 0
  if( debug_this ) call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( debug_this )then
    call logmsg('dh '//str(dh)//' '//str(clons(dh-1:dh)*r2d,'f20.15',' ~ '))
    call logmsg('lat west '//str(clats(dh-1)*r2d,'f20.15')//&
                 ' east '//str(clats(dh)  *r2d,'f20.15'))
    call logmsg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' ~ '))
    call logmsg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' ~ '))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Arc is above the zone
  if( dv_west > tdvf )then
    if( debug_this )then
      call logmsg('Case: arc is above the zone')
      call logret(PRCNAM, MODNAM)
    endif
    return
  !-------------------------------------------------------------
  ! Case: Arc is below the zone
  elseif( dv_east < tdvi )then
    if( debug_this )then
      call logmsg('Case: arc is below the zone')
    endif

    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
    enddo

    if( debug_this ) call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! dv_west
  !-------------------------------------------------------------
  if( intersection_sphere_normal_parallel3(&
        a, b, c, +1, &
        sin_dlats(dv_west), clons(dh-1_8), clons(dh), p2lon) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  if( dv_west >= tdvi )then
    area_tri = area_sphere_righttri_north_bottom(clats(dh-1_8), dlats(dv_west)) * (p2lon-clons(dh-1_8))
    area_isct = area_tri

    if( debug_this )then
      call logmsg('dv_west '//str(dv_west))
      call logmsg('  lon '//str((p2lon-clons(dh-1))*r2d,'f20.15'))
      call logmsg('  C1('//str((/clons(dh-1_8),clats(dh-1_8) /)*r2d,'f20.15',', ')//')')
      call logmsg('  C2('//str((/p2lon        ,dlats(dv_west)/)*r2d,'f20.15',', ')//')')
      call logmsg('  iarea (+) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_west, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_west), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_west+1 ~ dv_east-1
  !-------------------------------------------------------------
  do idv = max(dv_west+1_8,tdvi), min(dv_east-1_8,tdvf)
    p1lon = p2lon
    if( intersection_sphere_normal_parallel3(&
          a, b, c, +1, &
          sin_dlats(idv), clons(dh-1_8), clons(dh), p2lon) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    area_rect = dara_1rad(idv) * (p1lon - clons(dh-1_8))
    area_tri = area_sphere_righttri_north_bottom(dlats(idv-1_8), dlats(idv)) * (p2lon-p1lon)
    area_isct = area_rect + area_tri

    if( debug_this )then
      call logmsg('dv '//str(idv))
      call logmsg('  lon '//str((p2lon-p1lon)*r2d,'f20.15'))
      call logmsg('  C1('//str((/p1lon,dlats(idv-1_8)/)*r2d,'f20.15',', ')//')')
      call logmsg('  C2('//str((/p2lon,dlats(idv)    /)*r2d,'f20.15',', ')//')')
      call logmsg('  iarea (+) rect '//str(area_rect,'es15.8'))
      call logmsg('        (+) tri  '//str(area_tri ,'es15.8'))
      call debug_print_area_change(dh, idv, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,idv), area_isct*sgn_lon)
  enddo  ! idv/
  !-------------------------------------------------------------
  ! dv_east
  !-------------------------------------------------------------
  if( dv_east <= tdvf )then
    p1lon = p2lon

    area_tri = area_sphere_righttri_south_bottom(&
                   dlats(dv_east-1_8), clats(dh)) * (clons(dh)-p1lon)
    area_rect = dara_1rad(dv_east) * clons_diff(dh)
    area_isct = area_rect - area_tri

    if( debug_this )then
      call logmsg('dv_east '//str(dv_east))
      call logmsg('  lon '//str((clons(dh)-p1lon)*r2d,'f20.15'))
      call logmsg('  C1('//str((/p1lon    ,dlats(dv_east-1_8)/)*r2d,'f20.15',', ')//')')
      call logmsg('  C2('//str((/clons(dh),clats(dh)         /)*r2d,'f20.15',', ')//')')
      call logmsg('  iarea (+) rect '//str(area_rect,'es15.8'))
      call logmsg('        (-) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_east, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_east), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_east+1 ~
  !-------------------------------------------------------------
  do idv = dv_east+1_8, tdvf
    call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
  enddo
  !-------------------------------------------------------------
  if( debug_this ) call logret(PRCNAM, MODNAM)
end function update_area_normal_monotonous_upward
!===============================================================
!
!===============================================================
integer(4) function update_area_normal_monotonous_downward(&
    iarea, a, b, c, sgn_lon, dh, dv_west, dv_east &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_normal_monotonous_downward'
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  integer(8), intent(in) :: dh
  integer(8), intent(in) :: dv_west, dv_east

  integer(8) :: idv
  real(8) :: p1lon, p2lon
  real(8) :: area_tri, area_rect
  real(8) :: area_isct

  logical :: debug_this  ! for switching

  debug_this = debug

  info = 0
  if( debug_this ) call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( debug_this )then
    call logmsg('dh '//str(dh)//' '//str(clons(dh-1:dh)*r2d,'f20.15',' ~ '))
    call logmsg('lat west '//str(clats(dh-1)*r2d,'f20.15')//&
                 ' east '//str(clats(dh)  *r2d,'f20.15'))
    call logmsg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' ~ '))
    call logmsg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' ~ '))
  endif
  !-------------------------------------------------------------
  ! dv_west
  !-------------------------------------------------------------
  if( intersection_sphere_normal_parallel3(&
        a, b, c, -1, &
        sin_dlats(dv_west-1_8), clons(dh-1_8), clons(dh), p2lon) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  if( dv_west <= tdvf )then
    area_tri = area_sphere_righttri_south_bottom(&
                   clats(dh-1_8), dlats(dv_west-1_8)) * (p2lon-clons(dh-1_8))
    area_rect = dara_1rad(dv_west) * clons_diff(dh)
    area_isct = area_rect - area_tri

    if( debug_this )then
      call logmsg('dv_west '//str(dv_west))
      call logmsg('  lon '//str((p2lon-clons(dh-1))*r2d,'f20.15'))
      call logmsg('  C1('//str((/clons(dh-1_8),clats(dh-1_8)     /)*r2d,'f20.15',', ')//')')
      call logmsg('  C2('//str((/p2lon        ,dlats(dv_west-1_8)/)*r2d,'f20.15',', ')//')')
      call logmsg('  iarea (+) rect '//str(area_rect,'es15.8'))
      call logmsg('        (-) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_west, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_west), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_west-1 ~ dv_east+1
  !-------------------------------------------------------------
  do idv = min(dv_west-1_8,tdvf), max(dv_east+1_8,tdvi), -1_8
    p1lon = p2lon
    if( intersection_sphere_normal_parallel3(&
          a, b, c, -1, &
          sin_dlats(idv-1_8), clons(dh-1_8), clons(dh), p2lon) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    area_tri = area_sphere_righttri_north_bottom(dlats(idv), dlats(idv-1_8)) * (p2lon-p1lon)
    area_rect = dara_1rad(idv) * (clons(dh) - p2lon)
    area_isct = area_tri + area_rect

    if( debug_this )then
      call logmsg('dv '//str(idv))
      call logmsg('  lon '//str((p2lon-p1lon)*r2d,'f20.15'))
      call logmsg('  C1('//str((/p1lon,dlats(idv)    /)*r2d,'f20.15',', ')//')')
      call logmsg('  C2('//str((/p2lon,dlats(idv-1_8)/)*r2d,'f20.15',', ')//')')
      call logmsg('  iarea (+) tri  '//str(area_tri,'es15.8'))
      call logmsg('        (+) rect '//str(area_rect,'es15.8'))
      call debug_print_area_change(dh, idv, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,idv), area_isct*sgn_lon)
  enddo  ! idv/
  !-------------------------------------------------------------
  ! dv_east
  !-------------------------------------------------------------
  if( dv_east >= tdvi )then
    p1lon = p2lon

    area_tri = area_sphere_righttri_north_bottom(dlats(dv_east), clats(dh)) * (clons(dh)-p1lon)
    area_isct = area_tri

    if( debug_this )then
      call logmsg('dv_east '//str(dv_east))
      call logmsg('  lon '//str((clons(dh)-p1lon)*r2d,'f20.15'))
      call logmsg('  C1('//str((/p1lon    ,dlats(dv_east)/)*r2d,'f20.15',', ')//')')
      call logmsg('  C2('//str((/clons(dh),clats(dh)     /)*r2d,'f20.15',', ')//')')
      call logmsg('  iarea (+) tri  '//str(area_tri,'es15.8'))
      call debug_print_area_change(dh, dv_east, iarea, area_isct*sgn_lon, '  ')
    endif

    call add(iarea(dh,dv_east), area_isct*sgn_lon)
  endif
  !-------------------------------------------------------------
  ! dv_west+1 ~
  !-------------------------------------------------------------
  do idv = dv_west+1_8, tdvf
    call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
  enddo
  !-------------------------------------------------------------
  if( debug_this ) call logret(PRCNAM, MODNAM)
end function update_area_normal_monotonous_downward
!===============================================================
!
!===============================================================
integer(4) function update_area_normal_convex_upward(&
    iarea, a, b, c, sgn_lon, lontop, lattop, dh &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_normal_convex_upward'
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  real(8)   , intent(in) :: lontop, lattop
  integer(8), intent(in) :: dh

  integer(8) :: idv
  integer(8) :: dv_west, dv_east, dv_top
  real(8) :: clat, clon, clon_diff

  info = 0
  if( debug ) call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Arc is below the zone.
  if( lattop <= dlats(tdvi-1_8) )then
    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
    enddo
    if( debug )then
      call logmsg('Arc is below the zone.')
      call logret(PRCNAM, MODNAM)
    endif
    return
  !-------------------------------------------------------------
  ! Case: Arc is above the zone.
  elseif( min(clats(dh-1_8),clats(dh)) >= dlats(tdvf) )then
    if( debug )then
      call logmsg('Arc is above the zone.')
      call logret(PRCNAM, MODNAM)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <= clats(dh-1) <  dlats(dv_west)
  ! dlats(dv_east-1) <= clats(dh)   <  dlats(dv_east)
  ! dlats(dv_top-1)  <  lattop      <= dlats(dv_top)
  !-------------------------------------------------------------
  dv_west = dv_ge_south_lt_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
  dv_east = dv_ge_south_lt_north(clats(dh    ), tdvi_buf, tdvf_buf)
  dv_top  = dv_gt_south_le_north(lattop       , tdvi_buf, tdvf_buf)

!  call check_dv_west()
!  call check_dv_east()
!  call check_dv_top()

  if( debug )then
    call logmsg('c1  ('//str((/clons(dh-1),clats(dh-1)/)*r2d,'f20.15',',')//')')
    call logmsg('c2  ('//str((/clons(dh)  ,clats(dh)  /)*r2d,'f20.15',',')//')')
    call logmsg('top ('//str((/lontop     ,lattop     /)*r2d,'f20.15',',')//')')
    call logmsg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' ~ '))
    call logmsg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' ~ '))
    call logmsg('   top  '//str(dv_top) //' '//str(dlats(dv_top-1:dv_top)*r2d  ,'f20.15',' ~ '))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Monotonous and same
  if( dv_top == dv_west .and. dv_top == dv_east )then
    if( update_area_normal_monotonous_same(&
          iarea, sgn_lon, dh, dv_top) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: Upward
  elseif( dv_top > dv_west .and. dv_top == dv_east )then
    if( update_area_normal_monotonous_upward(&
          iarea, a, b, c, sgn_lon, dh, dv_west, dv_top) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: Downward
  elseif( dv_top == dv_west .and. dv_top > dv_east )then
    if( update_area_normal_monotonous_downward(&
          iarea, a, b, c, sgn_lon, dh, dv_top, dv_east) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: Convex
  elseif( dv_top > dv_west .and. dv_top > dv_east )then
    !-----------------------------------------------------------
    ! Western side of the top
    !-----------------------------------------------------------
    clat = clats(dh)
    clon = clons(dh)
    clon_diff = clons_diff(dh)
    clats(dh) = lattop
    clons(dh) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    if( update_area_normal_monotonous_upward(&
          iarea, a, b, c, sgn_lon, dh, dv_west, dv_top) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    clats(dh) = clat
    clons(dh) = clon
    clons_diff(dh) = clon_diff
    !-----------------------------------------------------------
    ! Eastern side of the top
    !-----------------------------------------------------------
    clat = clats(dh-1_8)
    clon = clons(dh-1_8)
    clon_diff = clons_diff(dh)
    clats(dh-1_8) = lattop
    clons(dh-1_8) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    if( update_area_normal_monotonous_downward(&
          iarea, a, b, c, sgn_lon, dh, dv_top, dv_east) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    clats(dh-1_8) = clat
    clons(dh-1_8) = clon
    clons_diff(dh) = clon_diff
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nNot matched to any of the following cases:'//&
              '\n  [1] dv_top == dv_west .and. dv_top == dv_east'//&
              '\n  [2] dv_top >  dv_west .and. dv_top >  dv_east'//&
              '\n  [3] dv_top >  dv_west .and. dv_top == dv_east'//&
              '\n  [4] dv_top == dv_west .and. dv_top >  dv_east'//&
              '\n  dv_top : '//str(dv_top)//&
              '\n  dv_west: '//str(dv_west)//&
              '\n  dv_east: '//str(dv_east), &
               PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  if( debug ) call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_dv_west()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_dv_west'

  if( .not. (dlats(dv_west-1) <= clats(dh-1) .and. clats(dh-1) < dlats(dv_west)) )then
    call errend(msg_unexpected_condition()//&
              '\n.not. (dlats(dv_west-1) <= clats(dh-1) .and. clats(dh-1) < dlats(dv_west))'//&
              '\n  clats(dh-1)     : '//str(clats(dh-1)*r2d,'f20.13')//&
              '\n  dlats(dv_west-1): '//str(dlats(dv_west-1)*r2d,'f20.13')//&
              '\n  dlats(dv_west)  : '//str(dlats(dv_west)*r2d,'f20.13')//&
              '\n  dv_west: '//str(dv_west)//&
              '\n  dh-1   : '//str(dh-1))
  endif
end subroutine check_dv_west
!---------------------------------------------------------------
subroutine check_dv_east()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_dv_east'

  if( .not. (dlats(dv_east-1) <= clats(dh) .and. clats(dh) < dlats(dv_east)) )then
    call errend(msg_unexpected_condition()//&
              '\n.not. (dlats(dv_east-1) <= clats(dh) .and. clats(dh) < dlats(dv_east))'//&
              '\n  clats(dh)       : '//str(clats(dh)*r2d,'f20.13')//&
              '\n  dlats(dv_east-1): '//str(dlats(dv_east-1)*r2d,'f20.13')//&
              '\n  dlats(dv_east)  : '//str(dlats(dv_east)*r2d,'f20.13')//&
              '\n  dv_east: '//str(dv_east)//&
              '\n  dh     : '//str(dh))
  endif
end subroutine check_dv_east
!---------------------------------------------------------------
subroutine check_dv_top()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_dv_top'

  if( .not. (dlats(dv_top-1) < lattop .and. lattop <= dlats(dv_top)) )then
    call errend(msg_unexpected_condition()//&
              '\n.not. (dlats(dv_top-1) < lattop .and. lattop <= dlats(dv_top))'//&
              '\n  lattop         : '//str(lattop*r2d,'f20.13')//&
              '\n  dlats(dv_top-1): '//str(dlats(dv_top-1)*r2d,'f20.13')//&
              '\n  dlats(dv_top)  : '//str(dlats(dv_top)*r2d,'f20.13')//&
              '\n  dv_top: '//str(dv_top)//&
              '\n  dh    : '//str(dh))
  endif
end subroutine check_dv_top
!---------------------------------------------------------------
end function update_area_normal_convex_upward
!===============================================================
!
!===============================================================
integer(4) function update_area_normal_convex_downward(&
    iarea, a, b, c, sgn_lon, lontop, lattop, dh &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_normal_convex_downward'
  real(8)   , pointer    :: iarea(:,:)  ! inout
  real(8)   , intent(in) :: a, b, c
  integer   , intent(in) :: sgn_lon
  real(8)   , intent(in) :: lontop, lattop
  integer(8), intent(in) :: dh

  integer(8) :: idv
  integer(8) :: dv_west, dv_east, dv_top
  real(8) :: clat, clon, clon_diff

  info = 0
  if( debug ) call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Arc is above the zone.
  if( lattop >= dlats(tdvf) )then
    if( debug )then
      call logmsg('Arc is above the zone.')
      call logret(PRCNAM, MODNAM)
    endif
    return
  !-------------------------------------------------------------
  ! Case: Arc is below the zone.
  elseif( max(clats(dh-1_8), clats(dh)) <= dlats(tdvi-1_8) )then
    do idv = tdvi, tdvf
      call add(iarea(dh,idv), dara_1rad(idv)*clons_diff(dh)*sgn_lon)
    enddo
    if( debug )then
      call logmsg('Arc is below the zone.')
      call logret(PRCNAM, MODNAM)
    endif
  endif
  !-------------------------------------------------------------
  ! dlats(dv_west-1) <  clats(dh-1) <= dlats(dv_west)
  ! dlats(dv_east-1) <  clats(dh)   <= dlats(dv_east)
  ! dlats(dv_top)    <= lattop      <  dlats(dv_top)
  !-------------------------------------------------------------
  dv_west = dv_gt_south_le_north(clats(dh-1_8), tdvi_buf, tdvf_buf)
  dv_east = dv_gt_south_le_north(clats(dh)    , tdvi_buf, tdvf_buf)
  dv_top  = dv_ge_south_lt_north(lattop       , tdvi_buf, tdvf_buf)

!  call check_dv_west()
!  call check_dv_east()
!  call check_dv_top()

  if( debug )then
    call logmsg('c1  ('//str((/clons(dh-1),clats(dh-1)/)*r2d,'f20.15',',')//')')
    call logmsg('c2  ('//str((/clons(dh)  ,clats(dh)  /)*r2d,'f20.15',',')//')')
    call logmsg('top ('//str((/lontop     ,lattop     /)*r2d,'f20.15',',')//')')
    call logmsg('dv west '//str(dv_west)//' '//str(dlats(dv_west-1:dv_west)*r2d,'f20.15',' - '))
    call logmsg('   east '//str(dv_east)//' '//str(dlats(dv_east-1:dv_east)*r2d,'f20.15',' - '))
    call logmsg('   top  '//str(dv_top) //' '//str(dlats(dv_top-1:dv_top)*r2d  ,'f20.15',' - '))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Monotonous and same
  if( dv_top == dv_west .and. dv_top == dv_east )then
    if( update_area_normal_monotonous_same(&
          iarea, sgn_lon, dh, dv_top) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: Downward
  elseif( dv_top < dv_west .and. dv_top == dv_east )then
    if( update_area_normal_monotonous_downward(&
          iarea, a, b, c, sgn_lon, dh, dv_west, dv_top) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: Upward
  elseif( dv_top == dv_west .and. dv_top < dv_east )then
    if( update_area_normal_monotonous_upward(&
          iarea, a, b, c, sgn_lon, dh, dv_top, dv_east) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif
  !-------------------------------------------------------------
  ! Case: Convex
  elseif( dv_top < dv_west .and. dv_top < dv_east )then
    !-----------------------------------------------------------
    ! Western side of the top
    !-----------------------------------------------------------
    clat = clats(dh)
    clon = clons(dh)
    clon_diff = clons_diff(dh)
    clats(dh) = lattop
    clons(dh) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    if( update_area_normal_monotonous_downward(&
          iarea, a, b, c, sgn_lon, dh, dv_west, dv_top) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    clats(dh) = clat
    clons(dh) = clon
    clons_diff(dh) = clon_diff
    !-----------------------------------------------------------
    ! Eastern side of the top
    !-----------------------------------------------------------
    clat = clats(dh-1_8)
    clon = clons(dh-1_8)
    clon_diff = clons_diff(dh)
    clats(dh-1_8) = lattop
    clons(dh-1_8) = lontop
    clons_diff(dh) = londiff_rad(clons(dh), clons(dh-1_8))

    if( update_area_normal_monotonous_upward(&
          iarea, a, b, c, sgn_lon, dh, dv_top, dv_east) /= 0 )then
      info = 1
      call errret('', PRCNAM, MODNAM)
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    clats(dh-1_8) = clat
    clons(dh-1_8) = clon
    clons_diff(dh) = clon_diff
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nNot matched to any of the following cases:'//&
              '\n  [1] dv_top < dv_west .and. dv_top < dv_east'//&
              '\n  [2] dv_top < dv_west .and. dv_top == dv_east'//&
              '\n  [3] dv_top == dv_west .and. dv_top < dv_east'//&
              '\n  dv_top : '//str(dv_top)//&
              '\n  dv_west: '//str(dv_west)//&
              '\n  dv_east: '//str(dv_east), &
               PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  if( debug ) call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_dv_west()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_dv_west'

  if( .not. (dlats(dv_west-1) < clats(dh-1) .and. clats(dh-1) <= dlats(dv_west)) )then
    call errend(msg_unexpected_condition()//&
              '\n.not. (dlats(dv_west-1) < clats(dh-1) .and. clats(dh-1) <= dlats(dv_west))'//&
              '\n  clats(dh-1)     : '//str(clats(dh-1)*r2d,'f20.13')//&
              '\n  dlats(dv_west-1): '//str(dlats(dv_west-1)*r2d,'f20.13')//&
              '\n  dlats(dv_west)  : '//str(dlats(dv_west)*r2d,'f20.13')//&
              '\n  dh     : '//str(dh)//&
              '\n  dv_west: '//str(dv_west))
  endif
end subroutine check_dv_west
!---------------------------------------------------------------
subroutine check_dv_east()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_dv_east'

  if( .not. (dlats(dv_east-1) < clats(dh) .and. clats(dh) <= dlats(dv_east)) )then
    call errend(msg_unexpected_condition()//&
              '\n.not. (dlats(dv_east-1) < clats(dh) .and. clats(dh) <= dlats(dv_east))'//&
              '\n  clats(dh)       : '//str(clats(dh)*r2d,'f20.13')//&
              '\n  dlats(dv_east-1): '//str(dlats(dv_east-1)*r2d,'f20.13')//&
              '\n  dlats(dv_east)  : '//str(dlats(dv_east)*r2d,'f20.13')//&
              '\n  dh     : '//str(dh)//&
              '\n  dv_east: '//str(dv_east))
  endif
end subroutine check_dv_east
!---------------------------------------------------------------
subroutine check_dv_top()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_dv_top'

  if( .not. (dlats(dv_top-1) <= lattop .and. lattop < dlats(dv_top)) )then
    call errend(msg_unexpected_condition()//&
              '\n.not. (dlats(dv_top-1) <= lattop .and. lattop < dlats(dv_top))'//&
              '\n  lattop         : '//str(lattop*r2d,'f20.13')//&
              '\n  dlats(dv_top-1): '//str(dlats(dv_top-1)*r2d,'f20.13')//&
              '\n  dlats(dv_top)  : '//str(dlats(dv_top)*r2d,'f20.13')//&
              '\n  dh    : '//str(dh)//&
              '\n  dv_top: '//str(dv_top))
  endif
end subroutine check_dv_top
!---------------------------------------------------------------
end function update_area_normal_convex_downward
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
integer(4) function update_area_parallel(&
    iarea, lon1, lon2, lat, arcpos &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_area_parallel'
  real(8)   , pointer    :: iarea(:,:)
  real(8)   , intent(in) :: lon1, lon2, lat
  integer(1), intent(in) :: arcpos

  integer :: sgn_lon
  real(8) :: wlon, wlat, elon, elat
  integer :: iZone, nZones
  integer(8) :: ttdhi_zone(2), ttdhf_zone(2)
  integer(8) :: ttdhi_buf, ttdhf_buf, ttdhi, ttdhf
  integer(8) :: idh
  integer(8) :: dv, idv

  info = 0
  if( debug ) call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Calc. sign and range of arc.
  !-------------------------------------------------------------
  if( calc_arc_sign_range(&
        arcpos, lon1, lat, lon2, lat, &
        sgn_lon, wlon, wlat, elon, elat, &
        nZones, ttdhi_zone, ttdhf_zone) /= 0 )then
    info = 1
    call errret('', PRCNAM, MODNAM)
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  if( debug )then
    call logmsg('lat: '//str(lat*r2d))
    call logmsg('lon1: '//str(lon1*r2d)//', lon2: '//str(lon2*r2d))
    call logmsg('w: ('//str((/wlon,wlat/)*r2d,'f12.7',',')//')')
    call logmsg('e: ('//str((/elon,elat/)*r2d,'f12.7',',')//')')
    call logmsg('sgn_lon: '//str(sgn_lon))
    call logmsg('nZones: '//str(nZones))
  endif

  if( nZones == 0 )then
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dv = dv_gt_south_le_north(lat, sdvi, sdvf)

  if( dv > sdvf )then
    if( debug ) call logret(PRCNAM, MODNAM)
    return
  endif

  if( debug )then
    call logmsg('lat: '//str(lat*r2d,'f12.7')//', dv: '//str((/sdvi,sdvf/),' - '))
  endif

  do iZone = 1, nZones
    !-----------------------------------------------------------
    ! Calc. ranges of raster with and without buffer
    !-----------------------------------------------------------
    ttdhi_buf = max(ttdhi_zone(iZone), tdhi_buf)
    ttdhf_buf = min(ttdhf_zone(iZone), tdhf_buf)

    ttdhi = max(ttdhi_buf, tdhi)
    ttdhf = min(ttdhf_buf, tdhf)

    if( debug )then
      call logmsg('sgn_lon '//str(sgn_lon))
      call logmsg('lat: '//str(lat*r2d,'f12.7')//', v: '//str(dv))
      call logmsg('Range of h w/ buffer : ['//str((/ttdhi_buf,ttdhf_buf/),dgt(ndh),':')//']')
      call logmsg('           w/o buffer: ['//str((/ttdhi,ttdhf/),dgt(ndh),':')//']')
    endif
    !-----------------------------------------------------------
    ! Calc. coords. of intersections with meridians
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = wlon
    clons(ttdhf_buf)   = elon
    clons_diff(ttdhi_buf) = clons(ttdhi_buf) - clons(ttdhi_buf-1)
    clons_diff(ttdhf_buf) = clons(ttdhf_buf) - clons(ttdhf_buf-1)
    !-----------------------------------------------------------
    ! Update areas
    !-----------------------------------------------------------
    do idh = ttdhi, ttdhf
      call add(iarea(idh,dv), &
               area_sphere_rect(max(dlats(dv-1_8),lat), dlats(dv))*clons_diff(idh)*sgn_lon)
    enddo  ! idh/

    do idv = dv+1_8, tdvf
      do idh = ttdhi, ttdhf
        call add(iarea(idh,idv), dara_1rad(idv)*clons_diff(idh)*sgn_lon)
      enddo
    enddo
    !-----------------------------------------------------------
    ! Restore clons
    !-----------------------------------------------------------
    clons(ttdhi_buf-1) = dlons(ttdhi_buf-1)
    clons(ttdhf_buf)   = dlons(ttdhf_buf)
    clons_diff(ttdhi_buf) = dlon
    clons_diff(ttdhf_buf) = dlon
  enddo  ! iZone/
  !-------------------------------------------------------------
  if( debug ) call logret(PRCNAM, MODNAM)
end function update_area_parallel
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
integer(4) function calc_arc_sign_range(&
    arcpos, lon1, lat1, lon2, lat2, &
    sgn_lon, wlon, wlat, elon, elat, &
    nZones, ttdhi_zone, ttdhf_zone &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_arc_sign_range'
  integer(1), intent(in)  :: arcpos
  real(8)   , intent(in)  :: lon1, lat1, lon2, lat2
  integer   , intent(out) :: sgn_lon
  real(8)   , intent(out) :: wlon, wlat, elon, elat
  integer   , intent(out) :: nZones
  integer(8), intent(out) :: ttdhi_zone(:), ttdhf_zone(:)

  info = 0
  if( debug ) call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nZones = 0

  selectcase( arcpos )
  !-------------------------------------------------------------
  ! Case: Normal
  case( ARC_POSITION_NORMAL )
    if( abs(lon1 - lon2) > rad_180deg )then
      if( (lon1 == rad_0deg   .and. lon2 == rad_360deg) .or. &
          (lon1 == rad_360deg .and. lon2 == rad_0deg  ) )then
        if( abs(lat1) /= rad_90deg .or. abs(lat2) /= rad_90deg )then
          call errend(msg_unexpected_condition()//&
                    '\nabs(lat1) /= 90 .or. abs(lat2) /= 90 (deg)'//&
                    '\n  lat1: '//str(lat1*r2d,'f12.7')//' (deg)'//&
                    '\n  lat2: '//str(lat2*r2d,'f12.7')//' (deg)')
        endif
        sgn_lon = int(sign(1.d0,lon2-lon1))
        wlon = rad_0deg
        wlat = lat1
        elon = rad_360deg
        elat = lat2
      elseif( lon1 == rad_0deg .and. lon2 > rad_180deg )then
        sgn_lon = -1
        wlon = lon2
        wlat = lat2
        elon = rad_360deg
        elat = lat1
      elseif( lon2 == rad_0deg .and. lon1 > rad_180deg )then
        sgn_lon = 1
        wlon = lon1
        wlat = lat1
        elon = rad_360deg
        elat = lat2
      elseif( lon2 == rad_360deg .and. lon1 < rad_180deg )then
        sgn_lon = -1
        wlon = rad_0deg
        wlat = lat2
        elon = lon1
        elat = lat1
      elseif( lon1 == rad_360deg .and. lon2 < rad_180deg )then
        sgn_lon = 1
        wlon = rad_0deg
        wlat = lat1
        elon = lon2
        elat = lat2
      else
        call errend(msg_unexpected_condition()//&
                  '\nNot matched to any cases.')
      endif
    else
      if( lon1 < lon2 )then
        sgn_lon = 1
        wlon = lon1
        wlat = lat1
        elon = lon2
        elat = lat2
      else
        sgn_lon = -1
        wlon = lon2
        wlat = lat2
        elon = lon1
        elat = lat1
      endif
    endif

    if( elon <= swest .or. seast <= wlon )then
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    nZones = 1
    ttdhi_zone(nZones) = dh_ge_west_lt_east(wlon, tdhi_buf, tdhf_buf)
    ttdhf_zone(nZones) = dh_gt_west_le_east(elon, tdhi_buf, tdhf_buf)
  !-------------------------------------------------------------
  ! Case: Lon0
  case( ARC_POSITION_LON0 )
    if( lon1 > lon2 )then
      sgn_lon = 1
      wlon = lon1
      wlat = lat1
      elon = lon2
      elat = lat2
    else
      sgn_lon = -1
      wlon = lon2
      wlat = lat2
      elon = lon1
      elat = lat1
    endif

    if( elon <= swest .and. seast <= wlon )then
      if( debug ) call logret(PRCNAM, MODNAM)
      return
    endif

    nZones = 0
    if( wlon < seast )then
      nZones = nZones + 1
      ttdhi_zone(nZones) = dh_ge_west_lt_east(wlon, tdhi_buf, tdhf_buf)
      ttdhf_zone(nZones) = sdhf + 1
    endif
    if( elon > swest )then
      nZones = nZones + 1
      ttdhi_zone(nZones) = sdhi - 1
      ttdhf_zone(nZones) = dh_gt_west_le_east(elon, tdhi_buf, tdhf_buf)
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('arcpos', arcpos))
  endselect
  !-------------------------------------------------------------
  if( debug ) call logret(PRCNAM, MODNAM)
end function calc_arc_sign_range
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! dlons(dh-1) <= lon < dlons(dh)
!===============================================================
integer(8) function dh_ge_west_lt_east(&
    lon, dh_llim, dh_ulim) result(dh)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dh_ge_west_lt_east'
  real(8), intent(in) :: lon
  integer(8), intent(in) :: dh_llim, dh_ulim

  dh = min(max(floor((lon-swest)/dlon)-1_8,dh_llim),dh_ulim)

  do while( lon >= dlons(dh) )
    dh = dh + 1_8
  enddo
end function dh_ge_west_lt_east
!===============================================================
! dlons(dh-1) < lon <= dlons(dh)
!===============================================================
integer(8) function dh_gt_west_le_east(&
    lon, dh_llim, dh_ulim) result(dh)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dh_gt_west_le_east'
  real(8), intent(in) :: lon
  integer(8), intent(in) :: dh_llim, dh_ulim

  dh = min(max(floor((lon-swest)/dlon)-1_8,dh_llim),dh_ulim)

  do while( lon > dlons(dh) )
    dh = dh + 1_8
  enddo
end function dh_gt_west_le_east
!===============================================================
! dlats(dv-1) <= lat < dlats(dv)
!===============================================================
integer(8) function dv_ge_south_lt_north(&
    lat, dv_llim, dv_ulim) result(dv)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dv_ge_south_lt_north'
  real(8), intent(in) :: lat
  integer(8), intent(in) :: dv_llim, dv_ulim

  dv = min(max(floor((lat-ssouth)/dlat)-1_8,dv_llim),dv_ulim)

  do while( lat >= dlats(dv) )
    dv = dv + 1_8
  enddo
end function dv_ge_south_lt_north
!===============================================================
! dlats(dv-1) < lat <= dlats(dv)
!===============================================================
integer(8) function dv_gt_south_le_north(&
    lat, dv_llim, dv_ulim) result(dv)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dv_gt_south_le_north'
  real(8), intent(in) :: lat
  integer(8), intent(in) :: dv_llim, dv_ulim

  dv = min(max(floor((lat-ssouth)/dlat)-1_8,dv_llim),dv_ulim)

  do while( lat > dlats(dv) )
    dv = dv + 1_8
  enddo
end function dv_gt_south_le_north
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
subroutine debug_print_range(nam, tdhi, tdhf, tdvi, tdvf)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'debug_print_range'
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: tdhi, tdhf, tdvi, tdvf

  call logmsg(str(nam)//':\n'//&
              '  dh: '//str(tdhi,dgt_hv)//' ('//&
              str(str_coords(dlons(tdhi-1:tdhi),r2d,lonlat_miss,wfmt_deg,' - '))//')'//&
                 ' - '//str(tdhf,dgt_hv)//' ('//&
              str(str_coords(dlons(tdhf-1:tdhf),r2d,lonlat_miss,wfmt_deg,' - '))//')\n'//&
              '  dv: '//str(tdvi,dgt_hv)//' ('//&
              str(str_coords(dlats(tdvi-1:tdvi),r2d,lonlat_miss,wfmt_deg,' - '))//')'//&
                 ' - '//str(tdvf,dgt_hv)//' ('//&
              str(str_coords(dlats(tdvf-1:tdvf),r2d,lonlat_miss,wfmt_deg,' - '))//')')
end subroutine debug_print_range
!===============================================================
!
!===============================================================
subroutine debug_print_area_change(dh, dv, iarea, area_add, indent)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'debug_print_area_change'
  integer(8)  , intent(in) :: dh, dv
  real(8)     , pointer    :: iarea(:,:)  ! in
  real(8)     , intent(in) :: area_add
  character(*), intent(in) :: indent

  integer :: cl

  cl = 7 + dgt(dh) + dgt(dv)

  call logmsg(indent//'iarea('//str((/dh,dv/),',')//') '//&
              str(iarea(dh,dv)+area_add,wfmt_dble)//' = '//&
              str(iarea(dh,dv),wfmt_dble)//' + '//str(area_add,wfmt_dble)//&
            '\n'//indent//str('',cl)//&
              '('//str((iarea(dh,dv)+area_add)/dara(dv),wfmt_dble)//' = '//&
              str(iarea(dh,dv)/dara(dv),wfmt_dble)//' + '//str(area_add/dara(dv),wfmt_dble)//')')
end subroutine debug_print_area_change
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
integer(4) function update_rt1d(&
    rt1, tij, &
    iarea, smskmap, sidxmap) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_rt1d'
  type(rt1d_), intent(inout) :: rt1
  integer(8) , intent(in)    :: tij
  real(8)    , pointer       :: iarea(:,:)    ! in
  logical(1) , pointer       :: smskmap(:,:)  ! in
  integer(8) , pointer       :: sidxmap(:,:)  ! in

  integer(8) :: sidx
  integer(8) :: idh, idv
  integer(8) :: ij, ij1, ij2

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do idv = tdvi, tdvf
    do idh = tdhi, tdhf
      if( .not. smskmap(idh,idv) ) cycle

      sidx = sidxmap(idh,idv)

      if( debug_s )then
        call logmsg('iarea('//str((/idh,idv/),dgt(max(ndh,ndv)),',')//'): '//str(iarea(idh,idv)))
      endif

      if( iarea(idh,idv) == 0.d0 ) cycle

      if( debug_s ) call logmsg('rt1d is updated.')

      if( sidx == sidx_prev(tij) )then
        call add(rt1%ara(ij_prev(tij)), iarea(idh,idv))
      elseif( rt1%mij == 0_8 )then
        rt1%mij = 1_8
        rt1%idx(1) = sidx
        rt1%ara(1) = iarea(idh,idv)
        sidx_prev(tij) = sidx
        ij_prev(tij) = 1_8
      else
        call search_nearest(sidx, rt1%idx(:rt1%mij), ij1, ij2)
        if( ij1 == ij2 )then
          call add(rt1%ara(ij1), iarea(idh,idv))
        else
          if( rt1%mij == rt1%ijsize )then
            call mul(rt1%ijsize, 2)
            call realloc(rt1%idx, rt1%ijsize, clear=.false.)
            call realloc(rt1%ara, rt1%ijsize, clear=.false.)
          endif
          do ij = rt1%mij, ij2, -1_8
            rt1%idx(ij+1_8) = rt1%idx(ij)
            rt1%ara(ij+1_8) = rt1%ara(ij)
          enddo
          rt1%idx(ij2) = sidx
          rt1%ara(ij2) = iarea(idh,idv)
          call add(rt1%mij)
        endif
        sidx_prev(tij) = sidx
        ij_prev(tij) = ij2
      endif
      !---------------------------------------------------------
    enddo  ! idh/
  enddo  ! idv/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function update_rt1d
!===============================================================
!
!===============================================================
end module c2_area_raster_polygon
