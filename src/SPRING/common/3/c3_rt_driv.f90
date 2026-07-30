module c3_rt_driv
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c1_type_timer
  use c1_timer, only: &
        start_ctimer, &
        stop_ctimer
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_rt
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_rt_driv'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function make_rt(&
    s, t, rt, calc_coef, make_vrf, output &
) result(info)
  use c1_file, only: &
        is_report_file_opened, &
        report
  use c1_gs_grid_core, only: &
        make_idxmap, &
        make_wgtmap, &
        make_grdidx, &
        make_grdara
  use c2_rt_main_finish, only: &
        finish_rt_main
  use c2_rt_vrf_driv, only: &
        make_rt_vrf
  use c2_rt_main_io, only: &
        write_rt_main
  use c2_rt_vrf_io, only: &
        write_rt_vrf
  use c3_rt_latlon_latlon, only: &
        make_rt_latlon_latlon
  use c3_rt_latlon_raster, only: &
        make_rt_latlon_raster
  use c3_rt_latlon_polygon, only: &
        make_rt_latlon_polygon
  use c3_rt_raster_raster, only: &
        make_rt_raster_raster
  use c3_rt_raster_polygon, only: &
        make_rt_raster_polygon
  use c3_rt_polygon_polygon, only: &
        make_rt_polygon_polygon
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt'
  type(gs_), intent(inout) :: s
  type(gs_), intent(inout) :: t
  type(rt_), intent(inout) :: rt
  logical  , intent(in)    :: calc_coef
  logical  , intent(in)    :: make_vrf
  logical  , intent(in)    :: output

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  if( make_idxmap(s) /= 0 )then
    info = 1; call errret(); return
  endif
  if( make_grdidx(s) /= 0 )then
    info = 1; call errret(); return
  endif
  if( make_grdara(s) /= 0 )then
    info = 1; call errret(); return
  endif
  if( make_wgtmap(s) /= 0 )then
    info = 1; call errret(); return
  endif

  if( make_idxmap(t) /= 0 )then
    info = 1; call errret(); return
  endif
  if( make_grdidx(t) /= 0 )then
    info = 1; call errret(); return
  endif
  if( make_grdara(t) /= 0 )then
    info = 1; call errret(); return
  endif
  if( make_wgtmap(t) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  selectcase( trim(s%typ)//'_'//trim(t%typ) )
  !-------------------------------------------------------------
  ! Case: LatLon and LatLon
  case( trim(MESHTYPE__LATLON)//'_'//trim(MESHTYPE__LATLON) )

    if( make_rt_latlon_latlon(s, t, rt) /= 0 )then

      info = 1; call errret(); return

    endif
  !-------------------------------------------------------------
  ! Case: LatLon and Raster
  case( trim(MESHTYPE__LATLON)//'_'//trim(MESHTYPE__RASTER), &
        trim(MESHTYPE__RASTER)//'_'//trim(MESHTYPE__LATLON) )

    if( make_rt_latlon_raster(s, t, rt) /= 0 )then

      info = 1; call errret(); return

    endif
  !-------------------------------------------------------------
  ! Case: LatLon and Polygon
  case( trim(MESHTYPE__LATLON)//'_'//trim(MESHTYPE__POLYGON), &
        trim(MESHTYPE__POLYGON)//'_'//trim(MESHTYPE__LATLON) )

    if( make_rt_latlon_polygon(s, t, rt) /= 0 )then

      info = 1; call errret(); return

    endif
  !-------------------------------------------------------------
  ! Case: Raster and Raster
  case( trim(MESHTYPE__RASTER)//'_'//trim(MESHTYPE__RASTER) )
    if( make_rt_raster_raster(s, t, rt) /= 0 )then

      info = 1; call errret(); return

    endif
  !-------------------------------------------------------------
  ! Case: Raster and Polygon
  case( trim(MESHTYPE__RASTER)//'_'//trim(MESHTYPE__POLYGON), &
        trim(MESHTYPE__POLYGON)//'_'//trim(MESHTYPE__RASTER) )
    if( make_rt_raster_polygon(s, t, rt) /= 0 )then

      info = 1; call errret(); return

    endif
  !-------------------------------------------------------------
  ! Case: Polygon and Polygon
  case( trim(MESHTYPE__POLYGON)//'_'//trim(MESHTYPE__POLYGON) )
    if( make_rt_polygon_polygon(s, t, rt) /= 0 )then

      info = 1; call errret(); return

    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value()//&
              '\n  s%typ: '//str(s%typ)//&
              '\n  t%typ: '//str(t%typ))
    return
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call start_ctimer('rt_post')

  if( calc_coef )then
    if( finish_rt_main(rt, s, t) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  call stop_ctimer('rt_post')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call start_ctimer('rt_post')

  if( make_vrf )then
    if( make_rt_vrf(rt, s) /= 0 )then
      info = 1; call errret(); return
    endif
    if( make_rt_vrf(rt, t) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  call stop_ctimer('rt_post')
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call start_ctimer('io')

  if( output )then
    if( write_rt_main(rt%main) /= 0 )then
      info = 1; call errret(); return
    endif
    if( write_rt_vrf(rt, s) /= 0 )then
      info = 1; call errret(); return
    endif
    if( write_rt_vrf(rt, t) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  call stop_ctimer('io')
  !-------------------------------------------------------------
  ! Report
  !-------------------------------------------------------------
  if( .not. is_report_file_opened() )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nReport file is not opened.')
    return
  endif
  call report('------ Remapping table ------')
  call report('length: '//str(rt%main%nij))
  if( rt%main%nij > 0_8 )then
    call report('source_index min: '//str(rt%main%sidx_vmin)//' max: '//str(rt%main%sidx_vmax))
    call report('target_index min: '//str(rt%main%tidx_vmin)//' max: '//str(rt%main%tidx_vmax))
    call report('intersection_area total: '//str(sum(rt%main%area),'es12.5'))
    call report('interpolation_coefficient'//&
                ' min: '//str(rt%main%coef_vmin,'es12.5')//&
                ' max: '//str(rt%main%coef_vmax,'es12.5'))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function make_rt
!===============================================================
!
!===============================================================
end module c3_rt_driv
