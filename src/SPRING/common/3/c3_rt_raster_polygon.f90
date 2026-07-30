module c3_rt_raster_polygon
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_io
  use lib_math
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
  public :: make_rt_raster_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_rt_raster_polygon'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function make_rt_raster_polygon(&
    s, t, rt &
) result(info)
  use c2_area_raster_polygon, only: &
        c2arp_initialize      => initialize     , &
        c2arp_finalize        => finalize       , &
        c2arp_initialize_zone => initialize_zone, &
        c2arp_finalize_zone   => finalize_zone  , &
        calc_iarea                              , &
        get_dhv_polygon                         , &
        update_rt1d
  use c2_area_raster, only: &
        c2ar_initialize        => initialize       , &
        c2ar_finalize          => finalize         , &
        c2ar_initialize_zone   => initialize_zone  , &
        c2ar_finalize_zone     => finalize_zone    , &
        update_iarea_sum                           , &
        calc_iratio_sum
  use c2_rt1d, only: &
        alloc_rt1d  , &
        clear_rt1d  , &
        reshape_rt1d
  use c2_rt_main_util, only: &
        trap_rt_empty
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt_raster_polygon'
  type(gs_), intent(inout), target :: s, t
  type(rt_), intent(inout), target :: rt

  type(gs_)         , pointer :: a  ! raster
  type(gs_)         , pointer :: b  ! polygon
  type(gs_raster_)  , pointer :: ar
  type(gs_polygon_) , pointer :: bp
  type(raster_zone_), pointer :: arz
  type(polygon_)    , pointer :: bp0
  type(rt_main_)    , pointer :: rtm
  type(rt_vrf_)     , pointer :: rtv
  type(file_), pointer :: f

  type(rt1d_), pointer :: rt1d(:), rt1
  real(8), pointer :: iarea(:,:), iarea_sum(:,:)
  logical :: iarea_is_updated
  logical :: do_calc_iarea_sum, do_calc_iratio_sum
  integer    :: iaz
  integer(8) :: bij
  integer(8) :: bhi, bhf, bvi, bvf
  logical    :: fill_vrf
  integer(8), parameter :: IJSIZE_INIT = 4

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the pointers
  !-------------------------------------------------------------
  if( s%typ == MESHTYPE__RASTER .and. &
      t%typ == MESHTYPE__POLYGON )then
    a => s
    b => t
  elseif( s%typ == MESHTYPE__POLYGON .and. &
          t%typ == MESHTYPE__RASTER )then
    a => t
    b => s
  else
    info = 1
    call errret(str(msg_invalid_value())//&
              '\n  s%typ: '//str(s%typ)//&
              '\n  t%typ: '//str(t%typ))
    return
  endif

  ar => a%raster
  bp => b%polygon

  rtm => rt%main
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call start_ctimer('buffer')

  !
  !-------------------------------------------------------------
  if( alloc_rt1d(rt1d, bp%nij, IJSIZE_INIT) /= 0 )then
    info = 1; call errret(); return
  endif

  if( ar%is_source )then
    rtv => rt%vrf_src
  else
    rtv => rt%vrf_tgt
  endif

  do_calc_iarea_sum = .false.
  do_calc_iratio_sum = .false.
  if( rtv%f%out_iarea_sum%path  /= '' ) do_calc_iarea_sum  = .true.
  if( rtv%f%out_iratio_sum%path /= '' ) do_calc_iratio_sum = .true.
  if( do_calc_iratio_sum ) do_calc_iarea_sum = .true.

  ! Initialize modules
  !-------------------------------------------------------------
  if( c2arp_initialize(ar, bp, make_rt=.true.) /= 0 )then
    info = 1; call errret(); return
  endif

  if( do_calc_iarea_sum )then
    if( c2ar_initialize(ar) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  call stop_ctimer('buffer')
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call logent('Making a remapping table', PRCNAM, MODNAM)

  ! Raise flag
  fill_vrf = .true.

  rtm%nij = 0_8

  do iaz = 1, ar%nZone
    arz => ar%zone(iaz)
    if( .not. arz%is_valid ) cycle

    if( ar%nZone > 1 ) call logent('Zone '//str(iaz)//' / '//str(ar%nZone))
    !-------------------------------------------------------------
    ! Initialize module for zone
    !-------------------------------------------------------------
    allocate(iarea(arz%hi-1:arz%hf+1,arz%vi-1:arz%vf+1))
    if( c2arp_initialize_zone(arz) /= 0 )then
      info = 1; call errret(); return
    endif

    if( do_calc_iarea_sum )then
      if( c2ar_initialize_zone(arz) /= 0 )then
        info = 1; call errret(); return
      endif
      allocate(iarea_sum(arz%hi:arz%hf,arz%vi:arz%vf))
    endif
    !-------------------------------------------------------------
    ! Make a remapping table
    !-------------------------------------------------------------
    call start_ctimer('intersection')

    do bij = bp%ijs, bp%ije
      if( .not. bp%grid%msk(bij) ) cycle

      rt1 => rt1d(bij)

      bp0 => bp%polygon(bij)

      rt1%idx_self = bp0%idx

      if( calc_iarea(iarea, bp0, iarea_is_updated, arz%mskmap) /= 0 )then
        info = 1; call errret(); return
      endif

      if( .not. iarea_is_updated ) cycle

      if( update_rt1d(rt1, bij, iarea, arz%mskmap, arz%idxmap) /= 0 )then
        info = 1; call errret(); return
      endif

      call add(rtm%nij, rt1%mij)

      if( do_calc_iarea_sum )then
        call get_dhv_polygon(bhi, bhf, bvi, bvf)
        if( update_iarea_sum(iarea_sum, iarea, bhi, bhf, bvi, bvf) /= 0 )then
          info = 1; call errret(); return
        endif
      endif
    enddo  ! bij/

    call stop_ctimer('intersection')
    !-------------------------------------------------------------
    ! Finalize module for zone
    !-------------------------------------------------------------
    deallocate(iarea)
    if( c2arp_finalize_zone() /= 0 )then
      info = 1; call errret(); return
    endif
    !-------------------------------------------------------------
    ! Make raster verification data
    !-------------------------------------------------------------
    if( do_calc_iarea_sum .or. do_calc_iratio_sum )then
      call start_ctimer('rt_post')

      if( .not. ar%is_south_to_north ) call reverse(arz%mskmap,2)

      call stop_ctimer('rt_post')

      if( do_calc_iarea_sum )then
        call start_ctimer('io')

        f => rtv%f%out_iarea_sum
        if( f%path /= '' )then
          call logmsg('Writing iarea_sum  '//str(fileinfo(f))//&
                  '\n  ['//str((/arz%xi,arz%xf/),dgt(ar%nx),':')//&
                     ', '//str((/arz%yi,arz%yf/),dgt(ar%ny),':')//']')
          if( fill_vrf )then
            if( wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                     sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/), fill=rtv%dval_miss) /= 0 )then
              info = 1
              call errret('Case fill_vrf')
              return
            endif
          else
            if( wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                     sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/)) /= 0 )then
              info = 1
              call errret('Case not fill_vrf')
              return
            endif
          endif
        endif

        call stop_ctimer('io')
      endif

      if( do_calc_iratio_sum )then
        call start_ctimer('rt_post')

        if( calc_iratio_sum(iarea_sum, arz%mskmap) /= 0 )then
          info = 1; call errret(); return
        endif

        call stop_ctimer('rt_post')

        call start_ctimer('io')

        f => rtv%f%out_iratio_sum
        if( f%path /= '' )then
          call logmsg('Writing iratio_sum '//str(fileinfo(f))//&
                  '\n  ['//str((/arz%xi,arz%xf/),dgt(ar%nx),':')//&
                     ', '//str((/arz%yi,arz%yf/),dgt(ar%ny),':')//']')
          if( fill_vrf )then
            if( wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                     sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/), fill=rtv%dval_miss) /= 0 )then
              info = 1
              call errret('Case fill_vrf')
              return
            endif
          else
            if( wbin(iarea_sum, f%path, f%dtype, f%endian, f%rec, &
                     sz=(/ar%nx,ar%ny/), lb=(/arz%xi,arz%yi/)) /= 0 )then
              info = 1
              call errret('Case not fill_vrf')
              return
            endif
          endif
        endif

        call stop_ctimer('io')
      endif

      call start_ctimer('rt_post')

      if( .not. ar%is_south_to_north ) call reverse(arz%mskmap,2)

      call stop_ctimer('rt_post')
    endif

    ! Kill flag
    if( fill_vrf ) fill_vrf = .false.
    !-----------------------------------------------------------
    ! Finalize module for zone
    !-----------------------------------------------------------
    call start_ctimer('buffer')

    if( do_calc_iarea_sum )then
      deallocate(iarea_sum)
      if( c2ar_finalize_zone() /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    call stop_ctimer('buffer')
    !-----------------------------------------------------------
    if( ar%nZone > 1 ) call logext()
  enddo  ! iaz/

  ! Reshape 1d-remapping table
  !-------------------------------------------------------------
  call start_ctimer('post_rt')

  if( reshape_rt1d(rt1d, b%is_source, rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  if( trap_rt_empty(rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  call stop_ctimer('post_rt')

  call logext()
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  call start_ctimer('buffer')

  ! Finalize modules
  !-------------------------------------------------------------
  if( c2arp_finalize() /= 0 )then
    info = 1; call errret(); return
  endif

  if( do_calc_iarea_sum )then
    if( c2ar_finalize() /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  !
  !-------------------------------------------------------------
  nullify(rt1)
  if( clear_rt1d(rt1d) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(rtm)

  nullify(bp0)
  nullify(arz)
  nullify(ar, bp)
  nullify(a, b)

  call stop_ctimer('buffer')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function make_rt_raster_polygon
!===============================================================
!
!===============================================================
end module c3_rt_raster_polygon
