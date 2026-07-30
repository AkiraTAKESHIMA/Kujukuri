module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: run
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_main'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine run(s, t, output)
  use c1_gs_define, only: &
        set_gs
  use c1_gs_grid_core, only: &
        make_idxmap, &
        make_wgtmap, &
        make_grdidx, &
        make_grduwa, &
        make_grdara, &
        make_grdwgt
  use mod_rasterize_latlon, only: &
        rasterize_latlon
  use mod_rasterize_polygon, only: &
        rasterize_polygon
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'run'
  type(gs_)    , intent(inout) :: s  ! origin
  type(gs_)    , intent(inout) :: t  ! raster
  type(output_), intent(in)    :: output

  type(gs_latlon_) , pointer :: sl
  type(gs_polygon_), pointer :: sp
  type(gs_raster_) , pointer :: tr
  type(file_grid_in_), pointer :: fg_in

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tr => t%raster

  call traperr( set_gs(tr) )

  fg_in => s%cmn%f_grid_in

  selectcase( s%typ )
  !-------------------------------------------------------------
  ! Case: LatLon
  case( MESHTYPE__LATLON )
    sl => s%latlon

    call traperr( set_gs(sl) )
    call traperr( make_idxmap(sl) )
    if( sl%f_grid_in%wgt%path /= '' )then
      call traperr( make_grdidx(sl) )
      call traperr( make_grduwa(sl) )
      call traperr( make_grdara(sl) )
      call traperr( make_grdwgt(sl) )
      call traperr( make_wgtmap(sl) )
    endif

    call rasterize_latlon(s, t, output)
  !-------------------------------------------------------------
  ! Case: Raster
  case( MESHTYPE__RASTER )
    call errend(msg_unexpected_condition()//&
              '\n  s%typ: '//str(s%typ))
  !-------------------------------------------------------------
  ! Case: Polygon
  case( MESHTYPE__POLYGON )
    sp => s%polygon
    call traperr( make_grdidx(sp) )
    call traperr( set_gs(sp) )
    if( fg_in%wgt%path /= '' )then
      call traperr( make_grduwa(sp) )
      call traperr( make_grdara(sp) )
      call traperr( make_grdwgt(sp) )
    endif

    call rasterize_polygon(s, t, output)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('s%typ', s%typ))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(sl, sp, tr, fg_in)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine run
!===============================================================
!
!===============================================================
end module mod_main
