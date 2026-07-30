module c1_gs_util
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use c1_const
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_gs_debug

  public :: mass_to_dens
  public :: dens_to_mass

  public :: print_gs_latlon
  public :: print_gs_raster
  public :: print_gs_polygon
  public :: print_latlon
  public :: print_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_gs_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function set_gs_debug(&
    debug, idx_debug, idx_miss, do_debug) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_gs_debug'
  logical   , intent(out)   :: debug
  integer(8), intent(inout) :: idx_debug
  integer(8), intent(in)    :: idx_miss
  logical   , intent(in)    :: do_debug

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( do_debug )then
    debug = idx_debug /= idx_miss
  else
    debug = .false.
    idx_debug = idx_miss
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_gs_debug
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
integer(4) function mass_to_dens(dens, mass, ara, idx) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'mass_to_dens'
  real(8)   , intent(out) :: dens(:)
  real(8)   , intent(in)  :: mass(:)
  real(8)   , intent(in)  :: ara(:)
  integer(8), intent(in)  :: idx(:)

  integer(8) :: nij, ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(dens)

  dens(:) = 0.d0
  do ij = 1_8, nij
    if( idx(ij) > 0_8 ) dens(ij) = mass(ij) / ara(ij)
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function mass_to_dens
!===============================================================
!
!===============================================================
integer(4) function dens_to_mass(mass, dens, ara, idx) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'dens_to_mass'
  real(8)   , intent(out) :: mass(:)
  real(8)   , intent(in)  :: dens(:)
  real(8)   , intent(in)  :: ara(:)
  integer(8), intent(in)  :: idx(:)

  integer(8) :: nij, ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(dens)

  mass(:) = 0.d0
  do ij = 1_8, nij
    if( idx(ij) > 0_8 ) mass(ij) = dens(ij) * ara(ij)
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function dens_to_mass
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
subroutine print_gs_latlon(&
    str_source_target, nam, &
    region_type, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_gs_latlon'
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: region_type
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call logmsg(str_source_target//' grid (latlon) '//&
            '\n  name: '//str(nam))
  call logmsg('  region_type: '//str(str_region_type_long(region_type)))
  call logmsg('  (h,v): ('//str((/hi,hf/),dgt(hf),':')//&
                      ', '//str((/vi,vf/),dgt(vf),':')//')')
  call logmsg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
            '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_latlon
!===============================================================
!
!===============================================================
subroutine print_gs_raster(&
    str_source_target, nam, &
    region_type, &
    hi, hf, vi, vf, &
    west, east, south, north)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_gs_raster'
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(1)  , intent(in) :: region_type
  integer(8)  , intent(in) :: hi, hf, vi, vf
  real(8)     , intent(in) :: west, east, south, north

  call logmsg(str_source_target//' grid (raster) '//&
            '\n  name: '//str(nam))
  call logmsg('  region_type: '//str(str_region_type_long(region_type)))
  call logmsg('  (h,v): ('//str((/hi,hf/),dgt(hf),':')//&
                      ', '//str((/vi,vf/),dgt(vf),':')//')')
  call logmsg('  lon: '//str((/west,east/)*r2d,'f12.7',' ~ ')//&
            '\n  lat: '//str((/south,north/)*r2d,'f12.7',' ~ '))
end subroutine print_gs_raster
!===============================================================
!
!===============================================================
subroutine print_gs_polygon(&
    str_source_target, nam, &
    ijs, ije)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_gs_polygon'
  character(*), intent(in) :: str_source_target
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: ijs, ije

  call logmsg(str_source_target//' grid (polygon) '//&
          '\n  name: '//str(nam)//&
          '\n  ij: ('//str((/ijs,ije/),dgt(ije),':')//')')
end subroutine print_gs_polygon
!===============================================================
!
!===============================================================
subroutine print_latlon(nam, ugl, ih, iv)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_latlon'
  character(*)    , intent(in) :: nam
  type(gs_latlon_), intent(in) :: ugl
  integer(8)      , intent(in) :: ih, iv

  integer(8) :: idx
  integer(8) :: loc
  integer(8) :: ij

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  idx = ugl%idxmap(ih,iv)
  call search(idx, ugl%grid%idx, ugl%grid%idxarg, loc)
  if( loc == 0_8 )then
    call errend(msg_unexpected_condition()//&
              '\nIndex '//str(idx)//' was not found in the list of indices.'//&
              '\n  nam: '//str(nam))
  endif
  ij = ugl%grid%idxarg(loc)

  call logmsg(nam//' latlon('//str((/ih,iv/),', ')//') idx: '//str(ugl%idxmap(ih,iv)))
  call logmsg('  lon: '//str(ugl%lon(ih-1_8:ih)*r2d,'f12.7',' - '))
  call logmsg('  lat: '//str(ugl%lat(iv-1_8:iv)*r2d,'f12.7',' - '))
  call logmsg('  area: '//str(ugl%grid%ara(ij)))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine print_latlon
!===============================================================
!
!===============================================================
subroutine print_polygon(p, lonlat_miss)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_polygon'
  type(polygon_), intent(in) :: p
  real(8), intent(in) :: lonlat_miss

  character(8), parameter :: WFMT_LONLAT = 'f12.7'
  character(8), parameter :: WFMT_FLOAT = 'es12.5'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('polygon '//str(p%idx))
  call logmsg('  n: '//str(p%n))
  call logmsg('  pos: '//str(str_polygon_pos_long(p%pos)))
  call logmsg('  bbox: '//str((/p%west,p%east,p%south,p%north/)*r2d,WFMT_LONLAT,','))
  call logmsg('  n_west: '//str(p%n_west)//', n_east: '//str(p%n_east)//&
            ', n_pole: '//str(p%n_pole))
  call logmsg('  lon   : '//str_coords(p%lon, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call logmsg('  lat   : '//str_coords(p%lat, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call logmsg('  lontop: '//str_coords(p%lontop, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call logmsg('  lattop: '//str_coords(p%lattop, r2d, lonlat_miss, WFMT_LONLAT, ',', p%n))
  call logmsg('  convex: '//str(str_convex_long(p%convex),-cl(WFMT_LONLAT),','))
  call logmsg('  arctyp: '//str(str_arctyp_long(p%arctyp),-cl(WFMT_LONLAT),','))
  call logmsg('  arcpos: '//str(str_arcpos_long(p%arcpos),-cl(WFMT_LONLAT),','))
  if( associated(p%a) )then
    call logmsg('  a     : '//str(p%a,WFMT_FLOAT))
    call logmsg('  b     : '//str(p%b,WFMT_FLOAT))
    call logmsg('  c     : '//str(p%c,WFMT_FLOAT))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine print_polygon
!===============================================================
!
!===============================================================
end module c1_gs_util
