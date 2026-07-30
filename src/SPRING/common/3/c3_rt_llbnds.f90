module c3_rt_llbnds
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use c1_type_opt
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calc_relations_llbnds
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface calc_relations_llbnds
    module procedure calc_relations_llbnds__latlon_latlon
    module procedure calc_relations_llbnds__latlon_raster
    module procedure calc_relations_llbnds__raster_latlon
    module procedure calc_relations_llbnds__raster_raster
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_rt_llbnds'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function calc_relations_llbnds__latlon_latlon(&
    sl, tl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_relations_llbnds__latlon_latlon'
  type(gs_latlon_), intent(inout) :: sl
  type(gs_latlon_), intent(inout) :: tl

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( calc_relations_llbnds_core(&
        sl%hrel, sl%vrel, sl%nam     , tl%nam      ,         &
        sl%lon , sl%lat , sl%lonwidth,                       &
        tl%lon , tl%lat , tl%lonwidth, tl%is_cyclic, tl%lon0 &
      ) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_relations_llbnds__latlon_latlon
!===============================================================
!
!===============================================================
integer(4) function calc_relations_llbnds__latlon_raster(sl, tr) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_relations_llbnds__latlon_raster'
  type(gs_latlon_), intent(inout) :: sl
  type(gs_raster_), intent(inout) :: tr

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( calc_relations_llbnds_core(&
        sl%hrel, sl%vrel, sl%nam     , tr%nam      ,         &
        sl%lon , sl%lat , sl%lonwidth,                       &
        tr%lon , tr%lat , tr%lonwidth, tr%is_cyclic, tr%lon0 &
      ) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_relations_llbnds__latlon_raster
!===============================================================
!
!===============================================================
integer(4) function calc_relations_llbnds__raster_latlon(sr, tl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_relations_llbnds__raster_latlon'
  type(gs_raster_), intent(inout) :: sr
  type(gs_latlon_), intent(inout) :: tl

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( calc_relations_llbnds_core(&
        sr%hrel, sr%vrel, sr%nam     , tl%nam      ,         &
        sr%lon , sr%lat , sr%lonwidth,                       &
        tl%lon , tl%lat , tl%lonwidth, tl%is_cyclic, tl%lon0 &
      ) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_relations_llbnds__raster_latlon
!===============================================================
!
!===============================================================
integer(4) function calc_relations_llbnds__raster_raster(sr, tr) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_relations_llbnds__raster_raster'
  type(gs_raster_), intent(inout) :: sr
  type(gs_raster_), intent(inout) :: tr

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( calc_relations_llbnds_core(&
        sr%hrel, sr%vrel, sr%nam     , tr%nam      ,         &
        sr%lon , sr%lat , sr%lonwidth,                       &
        tr%lon , tr%lat , tr%lonwidth, tr%is_cyclic, tr%lon0 &
      ) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_relations_llbnds__raster_raster
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
integer(4) function calc_relations_llbnds_core(&
    shrel, svrel, snam     , tnam   ,       &
    slon , slat , slonwidth,                &
    tlon , tlat , tlonwidth, tcyclic, tlon0 &
) result(info)
  use c1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_relations_llbnds_core'
  type(hrel_)     , pointer    :: shrel(:) ! out
  type(vrel_)     , pointer    :: svrel(:) ! out
  character(*)    , intent(in) :: snam, tnam
  real(8)         , pointer    :: slon(:), slat(:) ! in
  real(8)         , pointer    :: slonwidth(:) ! in
  real(8)         , pointer    :: tlon(:), tlat(:) ! in
  real(8)         , pointer    :: tlonwidth(:) ! in
  logical         , intent(in) :: tcyclic
  logical         , pointer    :: tlon0(:) ! in

  type(opt_earth_) :: earth
  type(hrel_), pointer :: shr
  type(vrel_), pointer :: svr
  integer(8) :: ish, isv
  integer(8) :: mth, mtv
  integer(8) :: thwest, theast, tvsouth, tvnorth
  integer(8) :: ith, itv, iith
  integer :: ir
  integer :: stat1, stat2
  integer :: counter
  logical :: t_intersects_lon0
  logical :: is_out_of_range
  real(8) :: lonwidth_sum, latwidth_sum
  real(8) :: lapara_1rad_sum

  integer(8) :: smh, shi, shf, smv, svi, svf
  integer(8) :: tmh, thi, thf, tmv, tvi, tvf

  integer :: dgt_sh, dgt_sv, dgt_th, dgt_tv
  character(clen_wfmt), parameter :: wfmt = 'f12.7'
  logical, parameter :: print_check_info = .false.
  logical, parameter :: print_print_info = .false.

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('s: '//str(snam))
  call logmsg('t: '//str(tnam))

  shi = lbound(slon,1) + 1_8
  shf = ubound(slon,1)
  svi = lbound(slat,1) + 1_8
  svf = ubound(slat,1)
  smh = shf - shi + 1_8
  smv = svf - svi + 1_8

  thi = lbound(tlon,1) + 1_8
  thf = ubound(tlon,1)
  tvi = lbound(tlat,1) + 1_8
  tvf = ubound(tlat,1)
  tmh = thf - thi + 1_8
  tmv = tvf - tvi + 1_8

  allocate(shrel(shi:shf))
  allocate(svrel(svi:svf))

  dgt_sh = dgt(shf)
  dgt_sv = dgt(svf)
  dgt_th = dgt(thf)
  dgt_tv = dgt(tvf)

  call get_opt_earth(earth)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating relations of meridians', PRCNAM, MODNAM)

  do ish = shi, shf
    shrel(ish)%hi(:) = 0
    shrel(ish)%hf(:) = 0
    shrel(ish)%mh = 0
  enddo

  if( tcyclic )then
    t_intersects_lon0 = .true.
  else
    !t_intersects_lon0 = any(tlon0(:)) .or. any(tlon(:) == rad_0deg)
    t_intersects_lon0 = any(tlon0(:))
  endif

  call logmsg('s: '//str(slon(shi-1:shi)*r2d,'f12.7',', ')//&
        ', ..., '//str(slon(shf-1:shf)*r2d,'f12.7',', '))
  call logmsg('t: '//str(tlon(thi-1:thi)*r2d,'f12.7',', ')//&
        ', ..., '//str(tlon(thf-1:thf)*r2d,'f12.7',', '))
  call logmsg('t_intersects_lon0: '//str(t_intersects_lon0))

  ! West
  !-------------------------------------------------------------
  call logent('west', PRCNAM, MODNAM, '-p -x2')

  ish = shi
  ith = thi
  counter = 0
  do while( ish <= shf )
    if( .not. tcyclic )then
      if( t_intersects_lon0 )then
        is_out_of_range = ( tlon(thf) <= slon(ish-1) .and. slon(ish-1) < tlon(thi-1) )
      else
        is_out_of_range = ( slon(ish-1) < tlon(thi-1) .or. tlon(thf) <= slon(ish-1) )
      endif

      if( is_out_of_range )then
        ish = ish + 1
        cycle
      endif
    endif

    stat1 = which_is_western(slon(ish-1), tlon(ith-1))
    stat2 = which_is_western(slon(ish-1), tlon(ith))

    if( stat1 /= 1 .and. stat2 == 1 )then  ! tlon(ith-1) <= slon(ish-1) < tlon(ith)
      shrel(ish)%hi(1) = ith
      ish = ish + 1
    else
      !call logmsg('slon(ish-1) < tlon(ith-1) or tlon(ith) < slon(ish-1)'//&
      !        '\n  ish: '//str(ish)//', ith: '//str(ith)//&
      !        '\n  slon(ish-1): '//str(slon(ish-1)*r2d,'f8.3')//&
      !        '\n  tlon(ith-1): '//str(tlon(ith-1)*r2d,'f8.3')//&
      !        '\n  slon(ish)  : '//str(slon(ish  )*r2d,'f8.3'))
      ith = ith + 1
      if( ith > thf )then
        ith = thi
        counter = counter + 1
        if( counter > 1 )then
          info = 1
          call errret(msg_internal_error()//&
                    '\nRelations of the grid lines were not calculated correctly.'//&
                    '\n  ish: '//str(ish)//' ('//str(slon(ish-1:ish)*r2d,'f12.7',' ~ ')//')')
          return
        endif
      endif
    endif
  enddo  ! ish/

  call logext('west', PRCNAM, MODNAM)

  ! East
  !-------------------------------------------------------------
  call logent('east', PRCNAM, MODNAM, '-p -x2')

  ish = shi
  ith = thi
  counter = 0
  do while( ish <= shf )
    if( .not. tcyclic )then
      if( t_intersects_lon0 )then
        is_out_of_range = ( tlon(thf) < slon(ish) .and. slon(ish) <= tlon(thi-1) )
      else
        is_out_of_range = ( slon(ish) <= tlon(thi-1) .or. tlon(thf) < slon(ish) )
      endif

      if( is_out_of_range )then
        ish = ish + 1
        cycle
      endif
    endif

    stat1 = which_is_western(slon(ish), tlon(ith-1))
    stat2 = which_is_western(slon(ish), tlon(ith))

    if( stat1 == 2 .and. stat2 /= 2 )then  ! tlon(ith-1) < slon(ish) <= tlon(ith)
      shrel(ish)%hf(1) = ith
      ish = ish + 1
    else
      ith = ith + 1
      if( ith > thf )then
        ith = thi
        counter = counter + 1
        if( counter > 1 )then
          info = 1
          call errret(msg_internal_error()//&
                    '\nRelations of the grid lines were not calculated correctly.')
          return
        endif
      endif
    endif
  enddo  ! ish/

  call logext('east', PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( check_info('lon', print_check_info) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext('Calculating relations of meridians', PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Calc. relations of parallels
  !-------------------------------------------------------------
  call logent('Calculating relations of parallels', PRCNAM, MODNAM)

  svrel(:)%vi = 0
  svrel(:)%vf = 0
  svrel(:)%mv = 0

  call logmsg('s: '//str(slat(svi-1:svi)*r2d,'f12.7',', ')//&
        ', ..., '//str(slat(svf-1:svf)*r2d,'f12.7',', '))
  call logmsg('t: '//str(tlat(tvi-1:tvi)*r2d,'f12.7',', ')//&
        ', ..., '//str(tlat(tvf-1:tvf)*r2d,'f12.7',', '))

  ! South
  !-------------------------------------------------------------
  isv = svi
  itv = tvi
  do while( isv <= svf .and. itv <= tvf )
    if( slat(isv-1) < tlat(tvi-1) .or. tlat(tvf) <= slat(isv-1) )then
      isv = isv + 1
      cycle
    endif

    if( tlat(itv-1) <= slat(isv-1) .and. slat(isv-1) < tlat(itv) )then
      svrel(isv)%vi = itv
      isv = isv + 1
    else
      itv = itv + 1
    endif
  enddo

  ! North
  !-------------------------------------------------------------
  isv = svi
  itv = tvi
  do while( isv <= svf .and. itv <= tvf )
    if( slat(isv) <= tlat(tvi-1) .or. tlat(tvf) < slat(isv) )then
      isv = isv + 1
      cycle
    endif

    if( tlat(itv-1) < slat(isv) .and. slat(isv) <= tlat(itv) )then
      svrel(isv)%vf = itv
      isv = isv + 1
    else
      itv = itv + 1
    endif
  enddo

  if( check_info('lat', print_check_info) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext('Calculating relations of parallels', PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Adjust indices of the sides out of range
  !-------------------------------------------------------------
  call logent('Adjusting indices of the sides out of range', PRCNAM, MODNAM)

  do ish = shi, shf
    shr => shrel(ish)

    if( shr%hi(1) == 0 .and. shr%hf(1) > 0 )then
      shr%hi(1) = thi
    elseif( shr%hf(1) == 0 .and. shr%hi(1) > 0 )then
      shr%hf(1) = thf
    endif
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Calc. longitudes of boundaries of intersection
  !-------------------------------------------------------------
  call logent('Calculating longitudes of boundaries of intersection', PRCNAM, MODNAM)

  lonwidth_sum = 0.d0

  do ish = shi, shf
    shr => shrel(ish)
    !-----------------------------------------------------------
    ! Put indices of overlapping grids in
    !-----------------------------------------------------------
    thwest = shr%hi(1)
    theast = shr%hf(1)
    !-----------------------------------------------------------
    ! Case: Not intersect with grid t
    if( thwest == 0 .and. theast == 0 )then
      shr%nr = 0
      cycle
    !-----------------------------------------------------------
    ! Case: Not intersect with lon-line
    elseif( thwest <= theast )then
      shr%nr = 1
      mth = theast - thwest + 1
    !-----------------------------------------------------------
    ! Case: Intersect with lon0-line
    else
      mth = (thf-thwest+1_8) + theast
      shr%nr    = 2
      shr%hi(1) = thwest
      shr%hf(1) = thf
      shr%hi(2) = 1_8
      shr%hf(2) = theast

      call logmsg('nr = 2 @ h = '//str(ish)//' ('//str(slon(ish-1:ish)*r2d,wfmt,' ~ ')//')')
      call logmsg('  hi(1): '//str(thwest,dgt_th)//&
                ' ('//str(tlon(thwest-1:thwest)*r2d,wfmt,' ~ ')//&
                ') hf(1): '//str(thf,dgt_th)//&
                ' ('//str(tlon(thf-1:thf)*r2d,wfmt,' ~ ')//')')
      call logmsg('  hi(2): '//str(thi,dgt_th)//&
                ' ('//str(tlon(thi-1:thi)*r2d,wfmt,' ~ ')//&
                ') hf(2): '//str(theast,dgt_th)//&
                ' ('//str(tlon(theast-1:theast)*r2d,wfmt,' ~ ')//')')
    endif
    !-----------------------------------------------------------
    shr%mh = mth
    allocate(shr%west(mth))
    allocate(shr%east(mth))
    allocate(shr%lonwidth(mth))
    !-----------------------------------------------------------
    ! Put coords. of grid lines in
    !-----------------------------------------------------------
    ! Case: Not intersect with lon0-line.
    if( shr%nr == 1 )then
      shr%west(1)       = eastern(slon(ish-1), tlon(thwest-1))
      shr%west(2:mth)   = tlon(thwest:theast-1)

      shr%east(1:mth-1) = tlon(thwest:theast-1)
      shr%east(mth)     = western(slon(ish), tlon(theast))
    else
      iith = 0
      do ir = 1, shr%nr
        do ith = shr%hi(ir), shr%hf(ir)
          iith = iith + 1
          shr%west(iith) = tlon(ith-1)
          shr%east(iith) = tlon(ith)
        enddo
      enddo

      shr%west(1)   = eastern(slon(ish-1), tlon(thwest-1))
      shr%east(mth) = western(slon(ish), tlon(theast))
    endif
    !-----------------------------------------------------------
    ! Calc. width
    !-----------------------------------------------------------
    iith = 0
    do ir = 1, shr%nr
      do ith = shr%hi(ir), shr%hf(ir)
        iith = iith + 1
        if( shr%west(iith) == tlon(ith-1) .and. shr%east(iith) == tlon(ith) )then
          shr%lonwidth(iith) = tlonwidth(ith)
        elseif( shr%west(iith) == slon(ish-1) .and. shr%east(iith) == slon(ish) )then
          shr%lonwidth(iith) = slonwidth(ish)
        else
          shr%lonwidth(iith) = londiff_rad(shr%west(iith), shr%east(iith))
        endif
      enddo
    enddo

    lonwidth_sum = lonwidth_sum + sum(shr%lonwidth(:))
  enddo  ! ish/

  call logmsg('Sum: '//str(lonwidth_sum,wfmt)//' (rad) = '//&
            str(lonwidth_sum*r2d,wfmt)//' (deg)')
  call logext()
  !-------------------------------------------------------------
  ! Calc. latitudes of boundaries of intersection
  !-------------------------------------------------------------
  call logent('Calculating latitudes of boundaries of intersection', PRCNAM, MODNAM)

  latwidth_sum = 0.d0

  do isv = svi, svf
    svr => svrel(isv)

    ! Out of range
    if( svr%vi == 0 .and. svr%vf == 0 )then
      cycle
    elseif( svr%vi == 0 )then
      svr%vi = tvi
    elseif( svr%vf == 0 )then
      svr%vf = tvf
    endif

    svr%mv = svr%vf - svr%vi + 1

    tvsouth = svr%vi
    tvnorth = svr%vf
    mtv = svr%mv

    allocate(svr%south(mtv))
    allocate(svr%north(mtv))
    allocate(svr%latwidth(mtv))

    svr%south(1)     = max(slat(isv-1), tlat(tvsouth-1))
    svr%south(2:mtv) = tlat(tvsouth:tvnorth-1)

    svr%north(1:mtv-1) = tlat(tvsouth:tvnorth-1)
    svr%north(mtv)     = min(slat(isv), tlat(tvnorth))

    latwidth_sum = latwidth_sum + sum(svr%north(:) - svr%south(:))
  enddo

  call logmsg('Sum: '//str(latwidth_sum,wfmt)//' (rad) = '//&
            str(latwidth_sum*r2d,wfmt)//' (deg)')

  call print_info(print_print_info)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_consistency() /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Calc. intersection area per 1 radian
  !-------------------------------------------------------------
  call logent('Calculating intersection area per 1 radian', PRCNAM, MODNAM)

  lapara_1rad_sum = 0.d0

  selectcase( earth%shptyp )
  case( EARTH_SHPTYP__SPHERE )
    do isv = svi, svf
      svr => svrel(isv)
      if( svr%mv > 0_8 )then
        allocate(svr%lapara_1rad(svr%mv))
        svr%lapara_1rad(:) = area_sphere_rect(svr%south(:), svr%north(:))
        lapara_1rad_sum = lapara_1rad_sum + sum(svr%lapara_1rad)
      endif
    enddo
  case( EARTH_SHPTYP__ELLIPS )
    do isv = svi, svf
      svr => svrel(isv)
      if( svr%mv > 0_8 )then
        allocate(svr%lapara_1rad(svr%mv))
        svr%lapara_1rad(:) = area_ellips_rect(svr%south(:), svr%north(:), earth%e2)
        lapara_1rad_sum = lapara_1rad_sum + sum(svr%lapara_1rad)
      endif
    enddo
  case default
    info = 1
    call errret(msg_invalid_value('earth%shptyp', earth%shptyp))
    return
  endselect

  call logmsg('Total: '//str(lapara_1rad_sum)//' (m2)')
  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
integer(4) function check_consistency() result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_consistency'
  integer(8) :: ish
  type(hrel_), pointer :: shr, shr_prev
  real(8), parameter :: thresh_lonwidth_rerr = 1d-5

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ish = shi+1, shf
    shr => shrel(ish)
    shr_prev => shrel(ish-1)
    if( shr%mh == 0_8 .or. shr_prev%mh == 0_8 ) cycle
    !---------------------------------------------------------
    !
    !---------------------------------------------------------
    if( shr%west(1) /= shr_prev%east(shr_prev%mh) .and. &
        abs(shr%west(1) - shr_prev%east(shr_prev%mh)) /= rad_360deg )then
      info = 1
      call errret('Incontinuity of longitudes. \n'//&
                  'shrel('//str(ish-1)//')%east('//str(shr_prev%mh)//'): '//&
                  str(shr_prev%east(shr_prev%nr),'f7.3')//', \n'//&
                  'shrel('//str(ish)//')%west(1): '//str(shr%west(1),'f7.3'))
      return
    endif
    !---------------------------------------------------------
    ! Following condition must be fullfilled.
    !   abs(sum(shr%lonwidth(:))) == londiff_rad(slon(ish),slon(ish-1))
    !---------------------------------------------------------
    if( shr%west(1) == slon(ish-1) .and. shr%east(shr%mh) == slon(ish) )then
      if( abs(sum(shr%lonwidth(:))-londiff_rad(slon(ish),slon(ish-1))) &
             > londiff_rad(slon(ish),slon(ish-1)) * thresh_lonwidth_rerr )then
        info = 1
        call errret('Relative error of sum(shrel('//str(ish)//')%lonwidth(:)) '//&
                    'to the actual width of the cell exceeded the threshhold. \n'//&
                    'sum(%lonwidth(:)): '//str(sum(shr%lonwidth(:)),'f12.8')//&
                    ', cell width: '//str(londiff_rad(slon(ish),slon(ish-1)),'f12.8'))
        return
      endif
    endif
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_consistency
!---------------------------------------------------------------
integer(4) function check_info(nam, print_this) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_info'
  character(*), intent(in) :: nam
  logical     , intent(in) :: print_this
  integer(8) :: ish, isv
  integer(8) :: tvsouth, tvnorth
  character(128) :: c_west, c_east, c_south, c_north

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-----------------------------------------------------------
  selectcase( nam )
  !-----------------------------------------------------------
  ! Case: Lat
  case( 'lat' )
    do isv = svi, svf
      tvsouth = svrel(isv)%vi
      tvnorth = svrel(isv)%vf

      if( tvsouth == 0 .and. tvnorth == 0 ) cycle
      !-------------------------------------------------------
      if( print_this )then
        if( tvsouth == 0 )then
          c_south = '<  '//str(1      ,dgt_tv)//&
                    ' ('//str(tlat(        0:      1)*r2d,wfmt,' ~ ')//')'
        else
          c_south = 'in '//str(tvsouth,dgt_tv)//&
                    ' ('//str(tlat(tvsouth-1:tvsouth)*r2d,wfmt,' ~ ')//')'
        endif

        if( tvnorth == 0 )then
          c_north = '>  '//str(tvf ,dgt_tv)//&
                    ' ('//str(tlat(tvf-1   :tvf  )*r2d,wfmt,' ~ ')//')'
        else
          c_north = 'in '//str(tvnorth,dgt_tv)//&
                    ' ('//str(tlat(tvnorth-1:tvnorth)*r2d,wfmt,' ~ ')//')'
        endif

        call logmsg(str(isv,dgt_sv)//&
                 ' S ('//str(slat(isv-1)*r2d,wfmt)//') '//str(c_south)//&
                 ' N ('//str(slat(isv  )*r2d,wfmt)//') '//str(c_north))
      endif
      !-------------------------------------------------------
      if( tvsouth /= 0 )then
        if( slat(isv-1) < tlat(tvsouth-1) .or. slat(isv-1) > tlat(tvsouth) )then
          info = 1
          call errret('slat('//str(isv-1)//'): '//str(slat(isv-1)*r2d)//&
                      ', tv: '//str(tvsouth)//&
                      ' ('//str(tlat(tvsouth-1:tvsouth)*r2d,'',' ~ ')//')\n'//&
                      't_south - s: '//str((tlat(tvsouth-1)-slat(isv-1))*r2d)//'\n'//&
                      's - t_north: '//str((slat(isv-1)-tlat(tvsouth))*r2d))
          return
        endif
      endif
      if( tvnorth /= 0 )then
        if( slat(isv) < tlat(tvnorth-1) .or. slat(isv) > tlat(tvnorth) )then
          info = 1
          call errret('slat('//str(isv)//'): '//str(slat(isv)*r2d)//&
                      ', tv: '//str(tvnorth)//&
                      ' ('//str(tlat(tvnorth-1:tvnorth),'',' ~ ')//')\n'//&
                      't_south - s: '//str((tlat(tvnorth-1)-slat(isv))*r2d)//'\n'//&
                      's - t_north: '//str((slat(isv)-tlat(tvnorth))*r2d))
          return
        endif
      endif
      !-------------------------------------------------------
    enddo  ! isv/
  !-----------------------------------------------------------
  ! Case: Lon
  case( 'lon' )
    do ish = shi, shf
      thwest = shrel(ish)%hi(1)
      theast = shrel(ish)%hf(1)

      if( thwest == 0 .and. theast == 0 ) cycle
      !-------------------------------------------------------
      if( print_this )then
        if( thwest == 0 )then
          c_west = '<  '//str(thi   ,dgt_th)//' ('//str(tlon(thi-1   :thi   )*r2d,wfmt,' ~ ')//')'
        else
          c_west = 'in '//str(thwest,dgt_th)//' ('//str(tlon(thwest-1:thwest)*r2d,wfmt,' ~ ')//')'
        endif

        if( theast == 0 )then
          c_east = '>  '//str(thf   ,dgt_th)//' ('//str(tlon(thf-1   :thf   )*r2d,wfmt,' ~ ')//')'
        else
          c_east = 'in '//str(theast,dgt_th)//' ('//str(tlon(theast-1:theast)*r2d,wfmt,' ~ ')//')'
        endif

        call logmsg(str(ish,dgt_sh)//&
                   ' W ('//str(slon(ish-1)*r2d,wfmt)//') '//str(c_west)//&
                   ' E ('//str(slon(ish  )*r2d,wfmt)//') '//str(c_east))
      endif
      !-------------------------------------------------------
      if( thwest /= 0 )then
        if( which_is_western(slon(ish-1), tlon(thwest-1)) == 1 .or. &
            which_is_western(slon(ish-1), tlon(thwest  )) == 2 )then
        !if( formerIsWestern(slon(ish-1), tlon(thwest-1)) ==  1 .or. &
        !    formerIsWestern(slon(ish-1), tlon(thwest  )) == -1 )then
        !if( .not. tlon0(thwest) .and. &
        !    (slon(ish-1) < tlon(thwest-1) .or. slon(ish-1) > tlon(thwest)) .and. &
        !    abs(slon(ish-1)-tlon(thwest-1)) /= rad_360deg .and. &
        !    abs(slon(ish-1)-tlon(thwest)) /= rad_360deg  )then
          info = 1
          call errret('Contradiction in shrel(:)%xi.\n'//&
                      'Western grid line is out of the other cell.\n'//&
                      'slon('//str(ish-1)//'): '//str(slon(ish-1)*r2d)//&
                      ', tlon('//str(thwest-1)//'): '//str(tlon(thwest-1)*r2d)//&
                      ', tlon('//str(thwest)//'): '//str(tlon(thwest)*r2d))
          return
        endif
      endif
      if( theast /= 0 )then
        if( which_is_western(slon(ish), tlon(theast-1)) == 1 .or. &
            which_is_western(slon(ish), tlon(theast  )) == -1 )then
        !if( formerIsWestern(slon(ish), tlon(theast-1)) ==  1 .or. &
        !    formerIsWestern(slon(ish), tlon(theast  )) == -1 )then
        !if( tlon0(theast) .and. &
        !    (slon(ish) < tlon(theast-1) .and. slon(ish) > tlon(theast)) .and. &
        !    abs(slon(ish)-tlon(theast-1)) /= rad_360deg .and. &
        !    abs(slon(ish)-tlon(theast)) /= rad_360deg )then
          info = 1
          call errret('Contradiction in shrel(:)%xf.\n'//&
                      'Eastern grid line is out of the other cell.\n'//&
                      'slon('//str(ish)//'): '//str(slon(ish)*r2d)//&
                      ', tlon('//str(theast-1)//'): '//str(tlon(theast-1)*r2d)//&
                      ', tlon('//str(theast)//'): '//str(tlon(theast)*r2d))
          return
        endif
      endif
      !-------------------------------------------------------
    enddo  ! ish/
  !-----------------------------------------------------------
  endselect
  !-----------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_info
!---------------------------------------------------------------
subroutine print_info(print_this)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'print_info'
  logical, intent(in) :: print_this
  integer(8) :: tvsouth, tvnorth
  integer(8) :: thwest, theast
  integer(8) :: mth, mtv
  integer(8) :: ish, isv
  type(vrel_), pointer :: svr
  type(hrel_), pointer :: shr
  character(4)   :: sstat, nstat, wstat, estat
  character(256) :: wfmt
  character(256) :: msg

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. print_this )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  call logmsg(trim(snam))

  wfmt = "(i"//str(dgt_sv)//",1x,"//&
         " 2(a,f9.5,a,i"//str(dgt_tv)//",2(a,f9.5),a),"//&
         " 4(a,f9.5))"

  do isv = svi, svf
    svr => svrel(isv)
    tvsouth = svr%vi
    tvnorth = svr%vf
    mtv     = svr%mv

    sstat = ' in '
    nstat = ' in '
    if( tvsouth == 0 .and. tvnorth == 0 )then
      cycle
    elseif( tvsouth == 0 )then
      tvsouth = tvi
      sstat = ' <  '
    elseif( tvnorth == 0 )then
      tvnorth = tvf
      nstat = ' >  '
    endif

    write(msg,wfmt)&
         isv, &
         'south ',slat(isv-1)*r2d,sstat,tvsouth,&
         ' (',tlat(tvsouth-1)*r2d,' ~ ',tlat(tvsouth)*r2d,') ', &
         'north ',slat(isv  )*r2d,nstat,tvnorth,&
         ' (',tlat(tvnorth-1)*r2d,' ~ ',tlat(tvnorth)*r2d,') ', &
         'lap south ',svr%south(1  )*r2d,' ~ ',svr%north(1  )*r2d, &
            ' north ',svr%south(mtv)*r2d,' ~ ',svr%north(mtv)*r2d
    call logmsg(trim(msg))
  enddo

  wfmt = "(i"//str(dgt_sh)//",1x,"//&
         " 2(a,f9.5,a,i"//str(dgt_th)//",2(a,f9.5),a),"//&
         " 2(2(a,f9.5),a,f6.3,a))"

  do ish = shi, shf
    shr => shrel(ish)
    if( shr%nr == 0 ) cycle

    thwest = shr%hi(1)
    theast = shr%hf(shr%nr)
    mth    = shr%mh

    wstat = ' in '
    estat = ' in '

    if( thwest == 0 .and. theast == 0 )then
      cycle
    elseif( thwest == 0 )then
      thwest = thi
      wstat = ' < '
    elseif( theast == 0 )then
      theast = thf
      estat = ' > '
    endif

    write(msg,wfmt)&
          ish, &
          'west ',slon(ish-1)*r2d,wstat,thwest,&
          ' (',tlon(thwest-1)*r2d,' ~ ',tlon(thwest)*r2d,') ', &
          'east ',slon(ish  )*r2d,estat,theast,&
          ' (',tlon(theast-1)*r2d,' ~ ',tlon(theast)*r2d,') ', &
          'lap west ',shr%west(1  )*r2d,' ~ ',shr%east(1  )*r2d,' (',shr%lonwidth(1)  *r2d,') ', &
             ' east ',shr%west(mth)*r2d,' ~ ',shr%east(mth)*r2d,' (',shr%lonwidth(mth)*r2d,')'
    call logmsg(trim(msg))
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine print_info
!---------------------------------------------------------------
end function calc_relations_llbnds_core
!===============================================================
!
!===============================================================
end module c3_rt_llbnds
