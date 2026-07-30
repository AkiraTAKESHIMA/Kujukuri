module c3_rt_latlon_polygon
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
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
  public :: make_rt_latlon_polygon
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_rt_latlon_polygon'

  logical :: debug
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function make_rt_latlon_polygon(&
    s, t, rt &
) result(info)
  use c1_opt_ctrl, only: &
        get_opt_earth
  use c1_gs_util, only: &
        print_latlon , &
        print_polygon
  use c2_rt1d, only: &
        init_rt1d   , &
        clear_rt1d  , &
        reshape_rt1d
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt_latlon_polygon'
  type(gs_), intent(inout), target :: s
  type(gs_), intent(inout), target :: t
  type(rt_), intent(inout), target :: rt

  type(gs_)             , pointer :: a  ! latlon
  type(gs_)             , pointer :: b  ! polygon
  type(gs_latlon_)      , pointer :: al
  type(gs_polygon_)     , pointer :: bp
  type(file_latlon_in_) , pointer :: afl
  type(file_polygon_in_), pointer :: bfp
  type(grid_)           , pointer :: ag, bg
  type(polygon_)        , pointer :: bp0
  type(rt_main_), pointer :: rtm

  type(opt_earth_) :: earth
  type(rt1d_), pointer :: rt1d(:), rt1
  integer(8) :: ahi(2), ahf(2), iah
  integer(8) :: avi, avf, iav
  integer    :: nar, iar
  integer(8) :: aidx
  integer(8) :: bij
  real(8)    :: area
  real(8)    :: aarea
  integer(8) :: loc

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  if( s%typ == MESHTYPE__LATLON .and. &
      t%typ == MESHTYPE__POLYGON )then
    a => s
    b => t
  elseif( s%typ == MESHTYPE__POLYGON .and. &
          t%typ == MESHTYPE__LATLON )then
    a => t
    b => s
  else
    info = 1
    call errret(msg_invalid_value()//&
              '\n  s%typ: '//str(s%typ)//&
              '\n  t%typ: '//str(t%typ))
    return
  endif

  al => a%latlon
  bp => b%polygon

  afl => al%f_latlon_in
  bfp => bp%f_polygon_in

  ag => al%grid
  bg => bp%grid

  rtm => rt%main

  call get_opt_earth(earth)
  !-------------------------------------------------------------
  ! Print debugging grids
  !-------------------------------------------------------------
  debug = al%debug .or. bp%debug

  if( al%debug )then
    do iav = al%vi, al%vf
      do iah = al%hi, al%hf
        if( al%idxmap(iah,iav) /= al%idx_debug ) cycle
        call print_latlon('a_debug', al, iah, iav)
      enddo
    enddo
  endif

  if( bp%debug )then
    bp0 => bp%polygon(bg%ij_debug)
    call print_polygon(bp0, bp%coord_miss_s)
  endif
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call start_ctimer('buffer')

  allocate(rt1d(bp%nij))
  if( init_rt1d(rt1d) /= 0 )then
    info = 1; call errret(); return
  endif

  call stop_ctimer('buffer')
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call logent('Making remapping table')

  call start_ctimer('intersection')

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.true.)
  endif

  rtm%nij = 0_8

  do bij = 1_8, bp%nij
    if( .not. bg%msk(bij) ) cycle

    bp0 => bp%polygon(bij)

    if( get_range_lat(al, bp0, al%lat, avi, avf) /= 0 )then
      info = 1; call errret(); return
    endif
    if( get_range_lon(al, bp0, al%lon, ahi, ahf, nar) /= 0 )then
      info = 1; call errret(); return
    endif

    if( avi == 0_8 .or. nar == 0 ) cycle

    if( bp%debug )then
      if( bp0%idx == bp%idx_debug )then
        call logmsg('bij: '//str(bij))
        call logmsg('  bbox: '//str((/bp0%west,bp0%east,bp0%south,bp0%north/)*r2d,'f12.7',' '))
        call logmsg('  pos: '//str(bp0%pos)//' ('//str(str_polygon_pos_long(bp0%pos))//')')
        call logmsg('  nsr: '//str(nar))
        do iar = 1, nar
          call logmsg('  ah: '//str((/ahi(iar),ahf(iar)/), ' ~ '))
        enddo
        call logmsg('  av: '//str((/avi,avf/),' ~ '))
      endif
    endif
    !-----------------------------------------------------------
    ! Prep. $rt1d
    !-----------------------------------------------------------
    rt1 => rt1d(bij)
    rt1%idx_self = bp0%idx
    rt1%ijsize = (avf-avi+1_8) * sum(ahf(:nar)-ahi(:nar)+1_8)
    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%ara(rt1%ijsize))
    !-----------------------------------------------------------
    ! Calc. intersection area and update regridding table
    !-----------------------------------------------------------
    rt1%mij = 0_8

    do iar = 1, nar
      do iav = avi, avf
        do iah = ahi(iar), ahf(iar)
          if( .not. al%mskmap(iah,iav) ) cycle

          aidx = al%idxmap(iah,iav)

          call search(aidx, ag%idx, ag%idxarg, loc)
          aarea = ag%ara(ag%idxarg(loc)) / earth%r**2

          if( debug )then
            if( .not. al%debug )then
              call print_latlon('a', al, iah, iav)
            endif

            if( .not. bp%debug )then
              call print_polygon(bp0, bp%coord_miss_s)
            endif
          endif

          if( area_sphere_intersection_latlon_polygon(&
                al%lon(iah-1_8), al%lon(iah), &
                al%lat(iav-1_8), al%lat(iav), aarea, &
                bp0%pos, bp0%lon, bp0%lat, &
                bp0%arctyp, bp0%a, bp0%b, bp0%c, &
                bp0%n_pole, bp0%convex, bp0%lontop, bp0%lattop, &
                area) /= 0 )then
            info = 1
            call errret('@ iah, iav = '//str((/iah,iav/),',')//&
                        ', aidx = '//str(aidx)//&
                      '\n  bij = '//str(bij)//', bidx = '//str(bp0%idx))
            return
          endif

          if( debug )then
            call logmsg('area: '//str(area))
          endif

          if( area > 0.d0 )then
            call add(rt1%mij)
            rt1%idx(rt1%mij) = aidx
            rt1%ara(rt1%mij) = area
          endif
        enddo  ! iah/
      enddo  ! iav/
    enddo  ! iar/

    call add(rtm%nij, rt1%mij)
  enddo  ! tij/

  if( debug )then
    call set_modvar_lib_math_sphere(debug=.false.)
  endif

  call stop_ctimer('intersection')

  call logext()
  !-------------------------------------------------------------
  ! Reshape 1d-remapping table
  !-------------------------------------------------------------
  call logent('Reshaping remapping table')

  call start_ctimer('rt_post')

  if( reshape_rt1d(rt1d, b%is_source, rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  call stop_ctimer('rt_post')

  call logext()
  !-------------------------------------------------------------
  ! Deallocate
  !-------------------------------------------------------------
  call start_ctimer('buffer')

  nullify(rt1)
  if( clear_rt1d(rt1d) /= 0 )then
    info = 1; call errret(); return
  endif
  nullify(rtm)

  nullify(bp0)
  nullify(ag, bg)
  nullify(afl, bfp)
  nullify(al, bp)
  nullify(a, b)

  call stop_ctimer('buffer')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function make_rt_latlon_polygon
!===============================================================
!
!===============================================================
integer(4) function get_range_lat(al, p, lat, avi, avf) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_range_lat'
  type(gs_latlon_), intent(in)  :: al
  type(polygon_)  , intent(in)  :: p
  real(8)         , intent(in)  :: lat(0:)
  integer(8)      , intent(out) :: avi, avf

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( p%north <= al%south .or. al%north <= p%south )then
    avi = 0_8
    avf = 0_8
  else
    if( p%south <= al%south )then
      avi = al%vi
    else
      avi = al%vi
      do while( lat(avi) <= p%south )
        avi = avi + 1_8
      enddo
    endif

    if( al%north <= p%north )then
      avf = al%vf
    else
      avf = al%vi
      do while( lat(avf) < p%north )
        avf = avf + 1_8
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_range_lat
!===============================================================
!
!===============================================================
integer(4) function get_range_lon(al, p, lon, ahi, ahf, nar) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_range_lon'
  type(gs_latlon_), intent(in)  :: al
  type(polygon_)  , intent(in)  :: p
  real(8)         , pointer     :: lon(:)   ! in
  integer(8)      , intent(out) :: ahi(:), ahf(:)  !(2)
  integer         , intent(out) :: nar

  integer(8) :: ahi0, ahf0
  integer(8) :: iah

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nar = 0
  ahi(:) = 0_8
  ahf(:) = 0_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( al%region_type )
  !-------------------------------------------------------------
  ! Case: Zone type of the LatLon grid is "Cyclic" or "Global"
  case( REGION_TYPE_GLOBAL, &
        REGION_TYPE_CYCLIC )

    selectcase( p%pos )
    case( POLYGON_POSITION_NORMAL, &
          POLYGON_POSITION_LON0 )
      do iah = al%hi, al%hf
        if( lon(iah-1_8) < lon(iah) )then
          if( lon(iah-1_8) <= p%west .and. p%west < lon(iah) )then
            ahi0 = iah
            exit
          endif
        else
          if( lon(iah-1_8) <= p%west .or. p%west < lon(iah) )then
            ahi0 = iah
            exit
          endif
        endif
      enddo

      do iah = al%hi, al%hf
        if( lon(iah-1_8) < lon(iah) )then
          if( lon(iah-1_8) < p%east .and. p%east <= lon(iah) )then
            ahf0 = iah
            exit
          endif
        else
          if( lon(iah-1_8) < p%east .or. p%east <= lon(iah) )then
            ahf0 = iah
            exit
          endif
        endif
      enddo

      if( ahi0 == ahf0 )then
        if( lon(ahi0-1_8) < lon(ahi0) )then
          if( p%west < p%east )then
            nar = 1
            ahi(1) = ahi0
            ahf(1) = ahi0
          else
            nar = 1
            ahi(1) = al%hi
            ahf(1) = al%hf
          endif
        else
          if( p%west < p%east )then
            if( (lon(ahi0-1_8) <= p%west .and. p%east <= rad_360deg) .or. &
                (rad_0deg <= p%west .and. p%east <=  lon(ahi0)) )then
              nar = 1
              ahi(1) = ahi0
              ahf(1) = ahi0
            else
              nar = 1
              ahi(1) = al%hi
              ahf(1) = al%hf
            endif
          else
            if( lon(ahi0-1_8) <= p%west .and. p%east <= lon(ahi0) )then
              nar = 1
              ahi(1) = ahi0
              ahf(1) = ahi0
            else
              nar = 1
              ahi(1) = al%hi
              ahf(1) = al%hf
            endif
          endif
        endif
      elseif( ahi0 < ahf0 )then
        nar = 1
        ahi(1) = ahi0
        ahf(1) = ahf0
      else
        nar = 2
        ahi(1) = ahi0
        ahf(1) = al%hf
        ahi(2) = al%hi
        ahf(2) = ahf0
      endif
    case( POLYGON_POSITION_POLAR )
      nar = 1
      ahi(1) = al%hi
      ahf(1) = al%hf
    case default
      info = 1
      call errret(msg_invalid_value('p%pos', p%pos))
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Zone type of the LatLon grid is "Regional"
  case( REGION_TYPE_REGIONAL )

    selectcase( p%pos )
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Normal"
    case( POLYGON_POSITION_NORMAL )
      !---------------------------------------------------------
      ! Case: The latlon zone does not intersect with lon0
      if( al%west < al%east )then
        !-------------------------------------------------------
        ! Case: Not intersect
        if( al%east <= p%west .or. p%east <= al%west )then
          nar = 0
        !-------------------------------------------------------
        ! Case: Intersects
        else
          nar = 1

          if( p%west <= al%west )then
            ahi(1) = al%hi
          else
            iah = al%hi
            do while( lon(iah) <= p%west )
              iah = iah + 1_8
            enddo
            ahi(1) = iah
          endif

          if( al%east <= p%east )then
            ahf(1) = al%hf
          else
            iah = al%hf
            do while( p%east <= lon(iah-1_8) )
              iah = iah - 1_8
            enddo
            ahf(1) = iah
          endif
        endif
      !---------------------------------------------------------
      ! Case: The latlon zone intersects with lon0
      else
        !-------------------------------------------------------
        ! Case: Not intersect
        if( p%east <= al%west .and. al%east <= p%west )then
          nar = 0
        !-------------------------------------------------------
        ! Case: Eastern part of the latlon zone intersects with the polygon
        elseif( p%east <= al%west .and. p%west < al%east )then
          nar = 1

          ahf(1) = al%hf
          iah = al%hf
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(1) = iah
        !-------------------------------------------------------
        ! Case: Western part of the latlon zone intersects with the polygon
        elseif( al%west < p%east .and. al%east <= p%west )then
          nar = 1

          ahi(1) = al%hi
          iah = al%hi
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah
        !-------------------------------------------------------
        ! Case: Both side of the latlon zone intersect with the polygon
        elseif( al%west < p%east .and. p%west < al%east )then
          nar = 2

          ahi(1) = al%hi
          iah = al%hi
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah

          ahf(2) = al%hf
          iah = al%hf
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(2) = iah
        !-------------------------------------------------------
        ! Case: ERROR
        else
          info = 1
          call errret(msg_unexpected_condition())
          return
        endif
      endif
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Lon0"
    case( POLYGON_POSITION_LON0 )
      !---------------------------------------------------------
      ! Case: The latlon zone does not intersect with lon0
      if( al%west < al%east )then
        !-------------------------------------------------------
        ! Case: Not intersect
        if( al%east <= p%west .and. p%east <= al%west )then
          nar = 0
        !-------------------------------------------------------
        ! Case: Both side of the latlon zone intersect with the polygon
        elseif( al%west < p%east .and. p%west < al%east )then
          nar = 2

          ahi(1) = al%hi
          iah = al%hi
          do while( iah < al%hf .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah

          ahf(2) = al%hf
          iah = al%hf
          do while( iah > al%hi .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(2) = iah
        !-------------------------------------------------------
        ! Case: Western side of the latlon zone intersects with the polygon
        elseif( al%west < p%east .and. al%east <= p%west )then
          nar = 1

          ahi(1) = al%hi
          iah = al%hi
          do while( iah < al%hf .and. lon(iah) < p%east )
            iah = iah + 1_8
          enddo
          ahf(1) = iah
        !-------------------------------------------------------
        ! Case: Eastern side of the latlon zone intersects with the polygon
        elseif( p%east <= al%west .and. p%west < al%east )then
          nar = 1

          ahf(1) = al%hf
          iah = al%hf
          do while( iah > al%hi .and. lon(iah-1_8) > p%west )
            iah = iah - 1_8
          enddo
          ahi(1) = iah
        !-------------------------------------------------------
        ! Case: ERROR
        else
          info = 1
          call errret(msg_unexpected_condition())
          return
        endif
      !---------------------------------------------------------
      ! Case: The latlon zone intersects with lon0
      else
        nar = 1

        if( p%west <= al%west )then
          ahi(1) = al%hi
        else
          iah = al%hi
          do while( lon(iah-1_8) < lon(iah) .and. lon(iah) <= p%west )
            iah = iah + 1_8
          enddo
          ahi(1) = iah
        endif

        if( al%east <= p%east )then
          ahf(1) = al%hf
        else
          iah = al%hf
          do while( lon(iah-1_8) < lon(iah) .and. p%east <= lon(iah-1_8) )
            iah = iah - 1_8
          enddo
          ahf(1) = iah
        endif
      endif
    !-----------------------------------------------------------
    ! Case: The position of the polygon is "Polar"
    case( POLYGON_POSITION_POLAR )
      nar = 1
      ahi(1) = al%hi
      ahf(1) = al%hf
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      info = 1
      call errret(msg_invalid_value('p%pos', p%pos))
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('al%region_type', al%region_type))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_range_lon
!===============================================================
!
!===============================================================
end module c3_rt_latlon_polygon
