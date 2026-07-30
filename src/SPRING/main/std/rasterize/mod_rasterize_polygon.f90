module mod_rasterize_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rst
  use def_type
  implicit none
  character(CLEN_PROC), parameter :: MODNAM = 'mod_rasterize_polygon'
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: rasterize_polygon
  !-------------------------------------------------------------
  contains
!===============================================================
!
!===============================================================
subroutine rasterize_polygon(a, b, output)
  use c2_area_raster, only: &
        c2ar_initialize        => initialize       , &
        c2ar_finalize          => finalize         , &
        c2ar_initialize_thresh => initialize_thresh, &
        c2ar_finalize_thresh   => finalize_thresh  , &
        c2ar_initialize_zone   => initialize_zone  , &
        c2ar_finalize_zone     => finalize_zone    , &
        alloc_map       , &
        update_iarea_sum, &
        update_iarea_max
  use c2_area_raster_polygon, only: &
        c2arp_initialize      => initialize     , &
        c2arp_finalize        => finalize       , &
        c2arp_initialize_zone => initialize_zone, &
        c2arp_finalize_zone   => finalize_zone  , &
        calc_iarea     , &
        get_dhv_polygon
  use mod_data, only: &
        get_tasks, &
        make_data
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rasterize_polygon'
  type(gs_)    , intent(in) :: a
  type(gs_)    , intent(in) :: b
  type(output_), intent(in) :: output

  real(8)         , pointer :: iarea(:,:)     ! out
  real(8)         , pointer :: iarea_sum(:,:) ! inout
  type(iarea_max_), pointer :: iarea_max(:,:) ! inout
  logical :: do_make_iarea_max , &
             do_make_iarea_sum , &
             do_make_iratio_sum, &
             do_make_idx       , &
             do_make_mask
  type(gs_polygon_) , pointer :: ap
  type(gs_raster_)  , pointer :: br
  type(polygon_)    , pointer :: ap0
  type(raster_zone_), pointer :: brz
  integer(8) :: aij
  integer    :: ibz
  integer(8) :: dhi, dhf, dvi, dvf
  logical :: iarea_is_updated
  logical :: fill_miss

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ap => a%polygon
  br => b%raster

  call get_tasks(output, &
                 do_make_iarea_max , &
                 do_make_iarea_sum , &
                 do_make_iratio_sum, &
                 do_make_idx       , &
                 do_make_mask)

  nullify(iarea)
  nullify(iarea_max)
  nullify(iarea_sum)

  fill_miss = .true.

  call traperr( c2ar_initialize(br) )
  call traperr( c2ar_initialize_thresh(output%thresh) )
  call traperr( c2arp_initialize(br, ap, make_rt=.true.) )
  !-------------------------------------------------------------
  ! Rasterize
  !-------------------------------------------------------------
  do ibz = 1, br%nZone
    if( br%nZone>1 )&
      call logent('("'//str(br%nam)//'") zone '//str(ibz)//' / '//str(br%nZone), PRCNAM, MODNAM)

    brz => br%zone(ibz)
    if( .not. brz%is_valid )then
      if( br%nZone>1 ) call logext()
      cycle
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call traperr( c2ar_initialize_zone(brz) )
    call traperr( c2arp_initialize_zone(brz) )

    call traperr( alloc_map(iarea, buffer=1) )
    if( do_make_iarea_max ) call traperr( alloc_map(iarea_max) )
    if( do_make_iarea_sum ) call traperr( alloc_map(iarea_sum) )
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do aij = ap%ijs, ap%ije
      if( .not. ap%grid%msk(aij) ) cycle

      ap0 => ap%polygon(aij)
      !---------------------------------------------------------
      ! Calc. intersection area
      !---------------------------------------------------------
      call traperr( calc_iarea(iarea, ap0, iarea_is_updated) )

      if( .not. iarea_is_updated ) cycle
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      call get_dhv_polygon(dhi, dhf, dvi, dvf)

      if( do_make_iarea_max )then
        call traperr( update_iarea_max(&
               iarea_max,        &  ! inout
               iarea,            &  ! in
               ap0%idx,          &  ! in
               dhi, dhf, dvi, dvf) )! in
      endif

      if( do_make_iarea_sum )then
        call traperr( update_iarea_sum(&
               iarea_sum,        &  ! inout
               iarea,            &  ! in
               dhi, dhf, dvi, dvf) )! in
      endif
      !---------------------------------------------------------
    enddo  ! aij/
    !-----------------------------------------------------------
    ! Make the accompanying data
    !-----------------------------------------------------------
    call make_data(&
           iarea_max, iarea_sum,             & ! in
           brz%mskmap, br%is_south_to_north, & ! in
           output, fill_miss)                  ! in
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(iarea)
    if( do_make_iarea_max ) deallocate(iarea_max)
    if( do_make_iarea_sum ) deallocate(iarea_sum)

    call traperr( c2ar_finalize_zone() )
    call traperr( c2arp_finalize_zone() )
    !-----------------------------------------------------------
    ! Kill the flag
    !-----------------------------------------------------------
    fill_miss = .false.
    !-----------------------------------------------------------
    if( br%nZone>1 ) call logext()
  enddo  ! ibz/
  !-------------------------------------------------------------
  ! Postprocess
  !-------------------------------------------------------------
  call traperr( c2ar_finalize() )
  call traperr( c2ar_finalize_thresh() )
  call traperr( c2arp_finalize() )

  nullify(ap0)
  nullify(brz)
  nullify(ap, br)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine rasterize_polygon
!===============================================================
!
!===============================================================
end module mod_rasterize_polygon
