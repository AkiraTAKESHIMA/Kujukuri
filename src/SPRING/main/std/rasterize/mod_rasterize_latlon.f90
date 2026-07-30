module mod_rasterize_latlon
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rst
  use def_type
  implicit none
  character(CLEN_PROC), parameter :: MODNAM = 'mod_rasterize_latlon'
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: rasterize_latlon
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine rasterize_latlon(a, b, output)
  use c2_area_raster, only: &
        initialize       , &
        finalize         , &
        initialize_thresh, &
        finalize_thresh  , &
        initialize_zone  , &
        finalize_zone    , &
        alloc_map        , &
        update_iarea_sum , &
        update_iarea_max
  use c3_rt_llbnds, only: &
        calc_relations_llbnds
  use mod_data, only: &
        get_tasks, &
        make_data
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rasterize_latlon'
  type(gs_)       , intent(in) :: a  ! latlon
  type(gs_)       , intent(in) :: b  ! raster
  type(output_)   , intent(in) :: output

  real(8)         , pointer :: iarea(:,:)
  type(iarea_max_), pointer :: iarea_max(:,:)
  real(8)         , pointer :: iarea_sum(:,:)
  logical :: do_make_iarea_max , &
             do_make_iarea_sum , &
             do_make_iratio_sum, &
             do_make_idx       , &
             do_make_mask
  type(gs_latlon_), pointer :: al
  type(gs_raster_), pointer :: br
  type(hrel_), pointer :: ahr
  type(vrel_), pointer :: avr
  type(raster_zone_), pointer :: brz
  integer(8) :: iah, iav
  integer    :: ibz
  integer(8) :: bhi(2), bhf(2), bvi, bvf, ibh, ibv
  integer(8), allocatable :: iibh(:)
  integer    :: ibr
  integer(8) :: iibh_this
  integer(8) :: iibv
  logical :: fill_miss
  integer :: case_wgtmap, case_wgtmap_a, case_wgtmap_b

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Preprocess
  !-------------------------------------------------------------
  al => a%latlon
  br => b%raster

  allocate(iibh(br%hi-1_8:br%hf+1_8))

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

  selectcase( al%status_wgtmap )
  case( GRID_STATUS__PREPARED )
    case_wgtmap_a = 1
  case( GRID_STATUS__NOT_USED      , &
        GRID_STATUS__TO_BE_PREPARED )
    case_wgtmap_a = 0
  case( GRID_STATUS__UNDEF )
    call errend(msg_unexpected_condition()//&
              '\n  al%status_wgtmap == '//str(al%status_wgtmap))
  case default
    call errend(msg_invalid_value('al%status_wgtmap', al%status_wgtmap))
  endselect
  selectcase( br%status_wgtmap )
  case( GRID_STATUS__PREPARED )
    case_wgtmap_b = 1
  case( GRID_STATUS__NOT_USED      , &
        GRID_STATUS__TO_BE_PREPARED )
    case_wgtmap_b = 0
  case( GRID_STATUS__UNDEF )
    call errend(msg_unexpected_condition()//&
              '\n  br%status_wgtmap == '//str(br%status_wgtmap))
  case default
    call errend(msg_invalid_value('br%status_wgtmap', br%status_wgtmap))
  endselect

  case_wgtmap = case_wgtmap_a*10 + case_wgtmap_b

  call traperr( initialize(br) )
  call traperr( initialize_thresh(output%thresh) )
  !-------------------------------------------------------------
  ! Calc. relationships among grid lines and pixel lines
  !-------------------------------------------------------------
  call traperr( calc_relations_llbnds(al, br) )
  !-------------------------------------------------------------
  ! Rasterize
  !-------------------------------------------------------------
  call logent('Rasterizing', PRCNAM, MODNAM)

  do ibz = 1, br%nZone
    if( br%nZone>1 )&
      call logent('("'//str(br%nam)//'") zone '//str(ibz)//' / '//str(br%nZone), PRCNAM, MODNAM)

    brz => br%zone(ibz)
    if( .not. brz%is_valid )then
      if( br%nZone > 1 ) call logext()
      cycle
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call traperr( initialize_zone(brz) )

    if( do_make_iarea_max ) call traperr( alloc_map(iarea_max) )
    if( do_make_iarea_sum ) call traperr( alloc_map(iarea_sum) )
    !-----------------------------------------------------------
    ! Calc. intersection areas
    !-----------------------------------------------------------
    call traperr( alloc_map(iarea) )

    do iav = al%vi, al%vf
      avr => al%vrel(iav)
      if( avr%vi == 0_8 ) cycle

      do iah = al%hi, al%hf
        if( .not. al%mskmap(iah,iav) ) cycle

        ahr => al%hrel(iah)
        if( ahr%nr == 0 ) cycle
        if( all(ahr%hf(:ahr%nr) < brz%hi .or. brz%hf < ahr%hi(:ahr%nr)) ) cycle
        !-------------------------------------------------------
        ! Calc. ranges of $ibh and $ibv
        !-------------------------------------------------------
        iibh_this = 0_8
        do ibr = 1, ahr%nr
          do ibh = ahr%hi(ibr), ahr%hf(ibr)
            call add(iibh_this)
            iibh(ibh) = iibh_this
          enddo
          bhi(ibr) = max(brz%hi,ahr%hi(ibr))
          bhf(ibr) = min(brz%hf,ahr%hf(ibr))
        enddo

        bvi = max(brz%vi,avr%vi)
        bvf = min(brz%vf,avr%vf)
        !-------------------------------------------------------
        ! Loop for $b
        !-------------------------------------------------------
        selectcase( case_wgtmap )
        !-------------------------------------------------------
        ! Case: wgtmap of neither $a or $b has been prepared
        case( 0 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh))
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: wgtmap of only $b has been prepared
        case( 1 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                    * brz%wgtmap(ibh,ibv)
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: wgtmap of only $a has been prepared
        case( 10 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                    * al%wgtmap(iah,iav)
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: wgtmap of both $a and $b have been prepared
        case( 11 )
          do ibr = 1, ahr%nr
            do ibv = bvi, bvf
              iibv = ibv - avr%vi + 1_8
              do ibh = bhi(ibr), bhf(ibr)
                !if( .not. brz%mskmap(ibh,ibv) ) cycle
                iarea(ibh,ibv) &
                  = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                    * al%wgtmap(iah,iav) * brz%wgtmap(ibh,ibv)
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call errend(msg_invalid_value('case_wgtmap', case_wgtmap))
        endselect
        !-------------------------------------------------------
        !
        !-------------------------------------------------------
        if( do_make_iarea_max )then
          do ibr = 1, ahr%nr
            call traperr( update_iarea_max(&
                     iarea_max,                  &  ! inout
                     iarea,                      &  ! in
                     al%idxmap(iah,iav),         &  ! in
                     bhi(ibr), bhf(ibr), bvi, bvf) )! in
          enddo
        endif

        if( do_make_iarea_sum )then
          do ibr = 1, ahr%nr
            call traperr( update_iarea_sum(&
                     iarea_sum,                  &  ! inout
                     iarea,                      &  ! in
                     bhi(ibr), bhf(ibr), bvi, bvf) )! in
          enddo
        endif
        !-------------------------------------------------------
      enddo  ! iah/
    enddo  ! iav/

    deallocate(iarea)
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
    if( do_make_iarea_max ) deallocate(iarea_max)
    if( do_make_iarea_sum ) deallocate(iarea_sum)

    call traperr( finalize_zone() )
    !-----------------------------------------------------------
    ! Kill the flag
    !-----------------------------------------------------------
    fill_miss = .false.
    !-----------------------------------------------------------
    if( br%nZone>1 ) call logext()
  enddo  ! ibz/

  call logext()
  !-------------------------------------------------------------
  ! Postprocess
  !-------------------------------------------------------------
  call traperr( finalize() )
  call traperr( finalize_thresh() )

  deallocate(iibh)
  nullify(ahr, avr)
  deallocate(al%hrel, al%vrel)
  nullify(brz)
  nullify(al, br)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine rasterize_latlon
!===============================================================
!
!===============================================================
end module mod_rasterize_latlon
