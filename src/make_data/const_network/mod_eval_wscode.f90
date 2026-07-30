module mod_eval_wscode
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_math
  use lib_array
  use lib_io
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: evalWsCodeConsistency
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_eval_wscode'
  !-------------------------------------------------------------
  ! Interfaces for intrisic functions
  !-------------------------------------------------------------
  interface
    integer function access(f, mode)
      character(*), intent(in) :: f
      character(*), intent(in) :: mode
    end function access
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine evalWsCodeConsistency(regionName)
  use c1_grid, only: &
        apprx_isct_with_meridian, &
        apprx_isct_with_parallel
  use c2_nlni_const, &
        set_resolution => set_resolution
  use c2_nlni_grid, only: &
        gxs_of_lon , &
        gxe_of_lon , &
        gys_of_lat , &
        gye_of_lat , &
        west_of_gx , &
        east_of_gx , &
        south_of_gy, &
        north_of_gy
  use c2_nlni_io, only: &
        strWsCode          , &
        strRvCode          , &
        read_map_from_tile
  use c2_strnk_io, only: &
        strnk_get_f_stream_shp       => get_f_stream_shp, &
        strnk_get_f_stream_dbf       => get_f_stream_dbf, &
        strnk_get_f_isct_wsys        => get_f_isct_wsys , &
        strnk_get_f_incons_isct_wsys => get_f_incons_isct_wsys
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'evalWsCodeConsistency'
  character(*), intent(in) :: regionName

  type isct_
    integer :: sz
    integer :: n, is, ie
    integer, pointer :: wsCode(:)
    real(8), pointer :: leng(:)
    real(8), pointer :: ratio(:)
  end type

  type tbl_wsCode_shp_
    integer :: wsCode
    integer :: n_wsCode_map_isct
    integer, pointer :: wsCode_map_isct(:)
    logical :: is_consistent
  end type

  type(shp_) :: shp
  type(shp_entity_), pointer :: ent
  type(shp_part_)  , pointer :: part
  type(dbf_) :: dbf
  type(dbf_record_), pointer :: rec
  integer :: iEnt, iPart, iPoint

  integer :: nWsCode
  integer(4), pointer :: lst_wsCode(:)
  type(tbl_wsCode_shp_), pointer :: tbl_wsCode_shp(:), wsShp
  type(isct_) :: isct_part, isct_ent

  integer(4), pointer :: map_wsCode(:,:)

  real(8) :: rlon0, rlat0, rlon1, rlat1
  real(8) :: rwlon, rwlat, relon, relat
  real(8) :: rmwlon, rmwlat, rmelon, rmelat
  real(8) :: rmslon, rmslat, rmnlon, rmnlat
  real(8) :: rpslon, rpslat, rpnlon, rpnlat
  integer :: gxs, gxe, gys, gye, igx, igy
  logical :: is_same_wsys
  integer :: iWsys, jWsys
  real(8) :: leng_part, leng_ent
  real(8) :: leng
  integer(4) :: wsCode_in
  integer(8) :: rvCode_in
  integer(8), allocatable :: arg(:)
  integer :: is, ie, i, j

  logical :: is_consistent
  integer :: n_case11, n_case121, n_case122, n_case13, &
             n_case21, n_case22, n_case23

  character(CLEN_PATH) :: f
  integer :: un_isct, un_all
  character(CLEN_WFMT), parameter :: WFMT_LONLAT = 'f11.6'

  real(8), parameter :: RATIO_MISS_THRESH  = 0.8d0
  real(8), parameter :: RATIO_VALID_THRESH = 0.8d0

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_100M)
  !-------------------------------------------------------------
  ! Read stream data
  !-------------------------------------------------------------
  f = strnk_get_f_stream_shp(regionName)
  call logmsg('Reading '//str(f))
  call traperr( shp_open(f) )
  call traperr( shp_get_all(shp) )
  call traperr( shp_close() )

  f = strnk_get_f_stream_dbf(regionName)
  call logmsg('Reading '//str(f))
  call traperr( dbf_open(f) )
  call traperr( dbf_get_all(dbf) )
  call traperr( dbf_close() )
  !-------------------------------------------------------------
  ! Make a list of wsCodes of entities
  !-------------------------------------------------------------
  allocate(lst_wsCode(shp%nEntity))
  do iEnt = 1, shp%nEntity
    rec => dbf%record(iEnt)
    call traperr( c2v(rec%value(1)%s, wsCode_in) )
    lst_wsCode(iEnt) = wsCode_in
  enddo
  call sort(lst_wsCode)

  nWsCode = 0
  ie = 0
  do while( ie < shp%nEntity )
    is = ie + 1
    ie = is
    do while( ie < shp%nEntity )
      if( lst_wsCode(ie+1) /= lst_wsCode(is) ) exit
      ie = ie + 1
    enddo
    nWsCode = nWsCode + 1
    lst_wsCode(nWsCode) = lst_wsCode(is)
  enddo
  call realloc(lst_wsCode, nWsCode, clear=.false.)

  allocate(tbl_wsCode_shp(nWsCode))
  do i = 1, nWsCode
    wsShp => tbl_wsCode_shp(i)
    wsShp%wsCode = lst_wsCode(i)
    wsShp%is_consistent = .true.
    wsShp%n_wsCode_map_isct = 0
    allocate(wsShp%wsCode_map_isct(32))
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  isct_part%sz = 1
  allocate(isct_part%wsCode(0:isct_part%sz), &
           isct_part%leng(0:isct_part%sz))
  isct_ent%sz = 1
  allocate(isct_ent%wsCode(0:isct_ent%sz), &
           isct_ent%leng(0:isct_ent%sz), &
           isct_ent%ratio(0:isct_ent%sz))

  nullify(map_wsCode)

  n_case11 = 0
  n_case121 = 0
  n_case122 = 0
  n_case13 = 0
  n_case21 = 0
  n_case22 = 0
  n_case23 = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = strnk_get_f_isct_wsys(regionName)
  call logmsg('Writing '//str(f))
  open(newunit=un_isct, file=f, status='replace')

  do iEnt = 1, shp%nEntity
  !do iEnt = 186858, shp%nEntity
  !do iEnt = 56340, 56340
    ent => shp%entity(iEnt)
    gxs = gxs_of_lon(ent%xmin)
    gxe = gxe_of_lon(ent%xmax)
    gys = gys_of_lat(ent%ymin)
    gye = gye_of_lat(ent%ymax)

    rec => dbf%record(iEnt)
    call traperr( c2v(rec%value(1)%s, wsCode_in) )
    call traperr( c2v(rec%value(2)%s, rvCode_in) )

    call logent('Entity '//str(iEnt)//&
                ' wsCode '//strWsCode(wsCode_in)//&
                ' rvCode '//strRvCode(rvCode_in), opt='-p -x2')
    !call logmsg('lon: '//str((/ent%xmin,ent%xmax/),WFMT_LONLAT,' - ')//&
    !            ' (gx: '//str((/gxs,gxe/),DGT_GXY,' - ')//')'//&
    !            ' (lon: '//str((/west_of_gx(gxs),east_of_gx(gxe)/),WFMT_LONLAT,' - ')//')'//&
    !          '\nlat: '//str((/ent%ymin,ent%ymax/),WFMT_LONLAT,' - ')//&
    !            ' (gy: '//str((/gys,gye/),DGT_GXY,' - ')//')'//&
    !            ' (lat: '//str((/south_of_gy(gys),north_of_gy(gye)/),WFMT_LONLAT,' - ')//')')

    call realloc(map_wsCode, (/gxs,gys/), (/gxe,gye/))
    map_wsCode(:,:) = WSCODE_MISS_I
    call read_map_from_tile('wsCode', gxs, gys, map_wsCode)

    isct_ent%ie = 0
    isct_ent%wsCode(0) = 0
    isct_ent%leng(0) = 0.d0
    leng_ent = 0.d0

    do iPart = 1, ent%nPart
      part => ent%part(iPart)

      call logent('Entity '//str(iEnt)//' Part '//str(iPart)//&
                  ' nPoint='//str(part%nPoint), opt='-p -x2')
      !if( part%nPoint > 7 )then
      !  call logmsg('lon '//str(part%x(:3),WFMT_LONLAT,', ')//&
      !              ', ...        , '//str(part%x(part%nPoint-2:),WFMT_LONLAT,', '))
      !  call logmsg('lat '//str(part%y(:3),WFMT_LONLAT,', ')//&
      !              ', ...        , '//str(part%y(part%nPoint-2:),WFMT_LONLAT,', '))
      !else
      !  call logmsg('lon '//str(part%x,WFMT_LONLAT,', '))
      !  call logmsg('lat '//str(part%y,WFMT_LONLAT,', '))
      !endif

      isct_part%ie = 0
      isct_part%wsCode(0) = 0
      isct_part%leng(0) = 0.d0
      leng_part = 0.d0
      iWsys = 0
      !---------------------------------------------------------
      ! Calc. length in water systems
      !---------------------------------------------------------
      do iPoint = 2, part%nPoint
        !call logmsg('iPoint '//str(iPoint))

        rlon0 = part%x(iPoint-1)*d2r
        rlat0 = part%y(iPoint-1)*d2r
        rlon1 = part%x(iPoint)*d2r
        rlat1 = part%y(iPoint)*d2r
        if( rlon0 <= rlon1 )then
          rwlon = rlon0
          rwlat = rlat0
          relon = rlon1
          relat = rlat1
        else
          rwlon = rlon1
          rwlat = rlat1
          relon = rlon0
          relat = rlat0
        endif
        !-------------------------------------------------------
        ! Update length of part
        !-------------------------------------------------------
        call add(leng_part, dist_sphere(rlon0, rlat0, rlon1, rlat1))
        !-------------------------------------------------------
        ! Calc. length of outside the region
        !-------------------------------------------------------
        if( relon <= REGION_WEST*d2r .or. rwlon >= REGION_EAST*d2r )then
          leng = dist_sphere(rlon0, rlat0, rlon1, rlat1)
          call logmsg('Totally out of range. leng: '//str(leng))
          call update_isct(isct_part, WSCODE_MISS_I, leng)
          call logext()
          cycle
        endif

        if( rwlon < REGION_WEST*d2r )then
          rmwlon = REGION_WEST*d2r
          rmwlat = apprx_isct_with_meridian(&
                     rwlon, rwlat, relon, relat, rmwlon)
          leng = dist_sphere(rwlon, rwlat, rmwlon, rmwlat)
          call logmsg('Western side is out of range. leng: '//str(leng))
          call update_isct(isct_part, WSCODE_MISS_I, leng)
          rwlon = rmwlon
          rwlat = rmwlat
        endif

        if( relon > REGION_EAST*d2r )then
          rmelon = REGION_EAST*d2r
          rmelat = apprx_isct_with_meridian(&
                     rwlon, rwlat, relon, relat, rmelon)
          leng = dist_sphere(rmelon, rmelat, relon, relat)
          call logmsg('Eastern side is out of range. leng: '//str(leng))
          call update_isct(isct_part, WSCODE_MISS_I, leng)
          relon = rmelon
          relat = rmelat
        endif
        !-------------------------------------------------------
        ! Calc. length of inside the region
        !-------------------------------------------------------
        !call logmsg('('//str((/rwlon,rwlat/)*r2d,'f12.7',',')//')'//&
        !         ' - ('//str((/relon,relat/)*r2d,'f12.7',',')//')'//&
        !            ' leng: '//str(dist_sphere(rwlon,rwlat,relon,relat)))
        gxs = gxs_of_lon(rwlon*r2d)
        gxe = gxe_of_lon(relon*r2d)
        rmelon = rwlon
        rmelat = rwlat
        do igx = gxs, gxe
          !-----------------------------------------------------
          ! Calc. intersection with meridian
          !-----------------------------------------------------
          rmwlon = rmelon
          rmwlat = rmelat
          if( igx == gxe )then
            rmelon = relon
            rmelat = relat
          else
            rmelon = east_of_gx(igx)*d2r
            rmelat = apprx_isct_with_meridian(&
                       rwlon, rwlat, relon, relat, rmelon)
          endif

          if( rmwlat <= rmelat )then
            rmslon = rmwlon  ! south
            rmslat = rmwlat
            rmnlon = rmelon  ! north
            rmnlat = rmelat
          elseif( rmwlat > rmelat )then
            rmslon = rmelon  ! south
            rmslat = rmelat
            rmnlon = rmwlon  ! north
            rmnlat = rmwlat
          endif

          gys = gys_of_lat(rmslat*r2d)
          gye = gye_of_lat(rmnlat*r2d)
          if( gys == gye )then
            is_same_wsys = .true.
          elseif( all(map_wsCode(igx,gys:gye) == map_wsCode(igx,gys)) )then
            is_same_wsys = .true.
          else
            is_same_wsys = .false.
          endif

          !call logmsg('gx '//str((/gxs,gxe/),' - ')//' same_wsys: '//str(is_same_wsys))
          if( is_same_wsys )then
            leng = dist_sphere(rmslon, rmslat, rmnlon, rmnlat)
            !call logmsg('wsCode: '//str(map_wsCode(igx,gys))//' leng: '//str(leng))
            call update_isct(isct_part, map_wsCode(igx,gys), leng, iWsys)
          else
            rpnlon = rmslon
            rpnlat = rmslat
            do igy = gys, gye
              !-------------------------------------------------
              ! Calc. intersection with parallels
              !-------------------------------------------------
              rpslon = rpnlon
              rpslat = rpnlat
              if( igy == gye )then
                rpnlon = rmnlon
                rpnlat = rmnlat
              else
                rpnlat = north_of_gy(igy)*d2r
                rpnlon = apprx_isct_with_parallel(&
                           rmwlon, rmwlat, rmelon, rmelat, rpnlat)
              endif
              leng = dist_sphere(rpslon, rpslat, rpnlon, rpnlat)
              !call logmsg('igy: '//str(igy)//&
              !      ' ('//str((/rpslon,rpslat/)*r2d,'f12.7',',')//')'//&
              !      ' - ('//str((/rpnlon,rpnlat/)*r2d,'f12.7',',')//')'//&
              !      ' wsCode: '//str(map_wsCode(igx,gys))//' leng: '//str(leng))
              call update_isct(isct_part, map_wsCode(igx,gys), leng, iWsys)
            enddo  ! igy/
          endif
        enddo  ! igx/
      enddo  ! iPoint/
      !---------------------------------------------------------
      ! Update length of entity
      !---------------------------------------------------------
      call add(leng_ent, leng_part)
      !---------------------------------------------------------
      ! Update $isct_ent
      !---------------------------------------------------------
      jWsys = 0
      do iWsys = 0, isct_part%ie
        !if( .not. (iWsys == 0 .and. isct_part%wsCode(0) == 0) )then
        !  call logmsg('('//str(iWsys,dgt(isct_part%ie))//')'//&
        !              ' wsCode: '//strWsCode(isct_part%wsCode(iWsys))//&
        !              ' length: '//str(isct_part%leng(iWsys))//&
        !              ' ('//str(isct_part%leng(iWsys)/leng_part*1d2,'f6.2')//'%)')
        !endif

        if( isct_part%wsCode(iWsys) == WSCODE_MISS_I )then
          call update_isct(isct_ent, WSCODE_MISS_I, isct_part%leng(iWsys))
        else
          call update_isct(isct_ent, isct_part%wsCode(iWsys), isct_part%leng(iWsys), jWsys)
        endif
      enddo  ! iWsys/
      !---------------------------------------------------------
      call logext()
    enddo  ! iPart/
    !-------------------------------------------------------------
    ! Sort by ratio
    !-------------------------------------------------------------
    if( isct_ent%ie >= 1 )then
      allocate(arg(isct_ent%ie))
      call argsort(isct_ent%leng(1:isct_ent%ie), arg)
      call sort(isct_ent%wsCode(1:isct_ent%ie), arg)
      call sort(isct_ent%leng(1:isct_ent%ie), arg)
      deallocate(arg)
    endif

    if( isct_ent%wsCode(0) == 0 )then
      isct_ent%is = 1
    else
      isct_ent%is = 0
    endif
    isct_ent%n = isct_ent%ie - isct_ent%is + 1

    if( leng_ent == 0.d0 )then
      call logwrn('Total length of the entity is zero.')
      if( isct_ent%n > 1 )then
        call logwrn('Total length is zero but included in more than two water systems.')
      endif
      isct_ent%ratio(isct_ent%is:isct_ent%ie) = 1.d0
    else
      isct_ent%ratio(0:isct_ent%ie) = isct_ent%leng(0:isct_ent%ie) / leng_ent
    endif
    !-----------------------------------------------------------
    ! Check consistency with dbf
    !-----------------------------------------------------------
    is_consistent = .true.
    !-----------------------------------------------------------
    ! Case1: Watsys is KNOWN from dbf
    if( wsCode_in /= WSCODE_MISS_I )then
      !---------------------------------------------------------
      ! Case11: Inconsistent
      ! -- Watsys is UNKNOWN on the map
      if( isct_ent%ratio(0) > RATIO_MISS_THRESH )then
        is_consistent = .false.
        call add(n_case11)
      !---------------------------------------------------------
      ! Case12: 
      ! -- Stream intersects with a SINGLE watsys on the map
      elseif( isct_ent%ratio(1) > RATIO_VALID_THRESH )then
        !-------------------------------------------------------
        ! Case121: Consistent
        ! -- Dominant watsys is CONSISTENT between map and dbf
        if( isct_ent%wsCode(1) == wsCode_in )then
          call add(n_case121)
        !-------------------------------------------------------
        ! Case122: Inconsistent
        ! -- Dominant watsys is INCONSISTENT between map and dbf
        else
          is_consistent = .false.
          call add(n_case122)
        endif
      !---------------------------------------------------------
      ! Case13: Inconsistent
      ! -- Stream intersects with MULTIPLE watsys on the map
      else
        is_consistent = .false.
        call add(n_case13)
      endif
    !-----------------------------------------------------------
    ! Case2: Watsys is UNKNOWN from dbf
    else
      !---------------------------------------------------------
      ! Case21: Consistent
      ! -- Watsys is UNKNOWN on the map
      if( isct_ent%ratio(0) > RATIO_MISS_THRESH )then
        call add(n_case21)
      !---------------------------------------------------------
      ! Case22: Inconsistent
      ! -- A watsys is dominant on the map
      elseif( isct_ent%ratio(1) > RATIO_VALID_THRESH )then
        is_consistent = .false.
        call add(n_case22)
      !---------------------------------------------------------
      ! Case23: Inconsistent?
      ! -- No watsys is dominant on the map
      else
        is_consistent = .false.
        call add(n_case23)
      endif
    endif

    if( .not. is_consistent )then
      call logmsg('Entity '//str(iEnt)//&
                  ' wsCode '//strWsCode(wsCode_in)//&
                  ' rvCode '//strRvCode(rvCode_in))
      do iWsys = isct_ent%is, isct_ent%ie
        call logmsg('  '//str(iWsys,dgt(isct_ent%ie))//&
                    ' wsCode: '//strWsCode(isct_ent%wsCode(iWsys))//&
                    ' length: '//str(isct_ent%leng(iWsys))//&
                    ' ('//str(isct_ent%ratio(iWsys)*1d2,'f6.2')//'%)')
      enddo

      call search(wsCode_in, lst_wsCode, i)
      wsShp => tbl_wsCode_shp(i)
      wsShp%is_consistent = .false.

      ! Update the list of wsCode of map that the entity intersects
      do iWsys = isct_ent%is, isct_ent%ie
        do j = 1, wsShp%n_wsCode_map_isct
          if( isct_ent%wsCode(iWsys) == wsShp%wsCode_map_isct(j) ) exit
        enddo
        if( j == wsShp%n_wsCode_map_isct+1 )then
          if( wsShp%n_wsCode_map_isct == size(wsShp%wsCode_map_isct) )then
            call realloc(wsShp%wsCode_map_isct, wsShp%n_wsCode_map_isct*2, clear=.false.)
          endif
          call add(wsShp%n_wsCode_map_isct)
          wsShp%wsCode_map_isct(wsShp%n_wsCode_map_isct) = isct_ent%wsCode(iWsys)
        endif
      enddo
    endif
    !-------------------------------------------------------------
    ! Output
    !-------------------------------------------------------------
    write(un_isct,"(2(1x,i0))") iEnt, isct_ent%n
    do iWsys = isct_ent%is, isct_ent%ie
      write(un_isct,"(a)") &
        str(iWsys,dgt(isct_ent%ie))//&
        ' '//strWsCode(isct_ent%wsCode(iWsys))//&
        ' '//str(isct_ent%leng(iWsys),'es10.3')//&
        ' '//str(isct_ent%ratio(iWsys),'f6.3')
    enddo  ! iWsys/
    !-----------------------------------------------------------
    call logext()
  enddo  ! iEnt/
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  call logmsg('Case 11  '//str(n_case11,dgt(shp%nEntity))//&
            '\nCase 121 '//str(n_case121,dgt(shp%nEntity))//&
            '\nCase 122 '//str(n_case122,dgt(shp%nEntity))//&
            '\nCase 13  '//str(n_case13,dgt(shp%nEntity))//&
            '\nCase 21  '//str(n_case21,dgt(shp%nEntity))//&
            '\nCase 22  '//str(n_case22,dgt(shp%nEntity))//&
            '\nCase 23  '//str(n_case23,dgt(shp%nEntity)))

  f = strnk_get_f_incons_isct_wsys(regionName)
  call logmsg('Writing '//str(f))
  open(newunit=un_all, file=f, status='replace')
  write(un_all,"(1x,a,1x,i0)") 'n', nWsCode-count(tbl_wsCode_shp(:)%is_consistent)
  do i = 1, nWsCode
    wsShp => tbl_wsCode_shp(i)
    if( wsShp%is_consistent ) cycle
    write(un_all,"(1x,a,1x,i0)") &
          'shp '//strWsCode(wsShp%wsCode), wsShp%n_wsCode_map_isct
    do j = 1, wsShp%n_wsCode_map_isct
      write(un_all,"(3x,a,1x,i0)") 'map', wsShp%wsCode_map_isct(j)
    enddo
  enddo
  close(un_all)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  close(un_isct)

  deallocate(map_wsCode)
  deallocate(isct_part%wsCode, &
             isct_part%leng)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine update_isct(isct, wsCode, leng, idx)
  implicit none
  type(isct_), intent(inout) :: isct
  integer    , intent(in)    :: wsCode
  real(8)    , intent(in)    :: leng
  integer    , intent(inout), optional :: idx

  if( wsCode == WSCODE_MISS_I )then
    isct%wsCode(0) = WSCODE_MISS_I
    call add(isct%leng(0), leng)
    return
  endif

  if( idx <= isct%ie )then
    if( isct%wsCode(idx) == wsCode )then
      call add(isct%leng(idx), leng)
      return
    endif
  endif

  do idx = 1, isct%ie
    if( isct%wsCode(idx) == wsCode )then
      call add(isct%leng(idx), leng)
      return
    endif
  enddo

  if( isct%ie == isct%sz )then
    isct%sz = isct%sz * 2
    call realloc(isct%wsCode, 0, isct%sz, clear=.false.)
    call realloc(isct%leng  , 0, isct%sz, clear=.false.)
    call realloc(isct%ratio , 0, isct%sz, clear=.false.)
  endif
  isct%ie = idx
  isct%wsCode(idx) = wsCode
  isct%leng(idx) = leng
end subroutine update_isct
!---------------------------------------------------------------
end subroutine evalWsCodeConsistency
!===============================================================
!
!===============================================================
end module mod_eval_wscode
