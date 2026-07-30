module mod_eval_basin
   use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io 
  use c1_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calcNetworkBasinIntersections
  public :: evalNetworkBasinConsistencies
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_eval_basin'
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer :: dgt_irEnt, dgt_uid
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
subroutine calcNetworkBasinIntersections()
  use c2_jflw_const, &
        set_resolution => set_resolution
  use c2_strnk_io, only: &
        strnk_region_str2idx         => region_str2idx     , &
        strnk_region_idx2str         => region_idx2str     , &
        strnk_get_f_stream_shp       => get_f_stream_shp   , &
        strnk_get_f_tmp_networks_lst => get_f_tmp_networks_lst
  use mod_util, only: &
        get_fmt_network
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calcNetworkBasinIntersections'

  character(:), allocatable :: uid
  integer :: nNwk, iNwk

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call logent('Initializing')

  call set_resolution(RESOLUTION_1SEC)

  call get_fmt_network(dgt_irEnt, dgt_uid)
  allocate(character(dgt_uid) :: uid)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating intersections')

  f = strnk_get_f_tmp_networks_lst()
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) c_, nNwk
  read(un,*)
  do iNwk = 1, nNwk
    read(un,*) c_, uid
!if( uid /= 63665235 ) cycle
    call logmsg('Network '//str(iNwk)//' ID '//str(uid,dgt_uid))
    call calc_channel_intersections(uid)
!exit
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calcNetworkBasinIntersections
!===============================================================
!
!===============================================================
subroutine calc_channel_intersections(uid)
  use c1_grid, only: &
        apprx_isct_with_meridian, &
        apprx_isct_with_parallel
  use c2_strnk_io, only: &
        strnk_region_str2idx           => region_str2idx          , &
        strnk_region_idx2str           => region_idx2str          , &
        strnk_get_f_stream_shp         => get_f_stream_shp        , &
        strnk_get_f_tmp_networks_tmp   => get_f_tmp_networks_lst  , &
        strnk_get_f_tmp_network_entity => get_f_tmp_network_entity, &
        strnk_get_f_isct_basin         => get_f_isct_basin        
  use c2_jflw_const
  use c2_jflw_grid, only: &
        gxs_of_lon, &
        gxe_of_lon, &
        gys_of_lat, &
        gye_of_lat, &
        gx_to_x, &
        gy_to_y, &
        gx_of_x, &
        gy_of_y, &
        west_of_gx , &
        east_of_gx , &
        south_of_gy, &
        north_of_gy
  use c2_jflw_io, only: &
        tilename      , &
        get_f_map_tile
  use mod_util, only: &
        nwk_rgn_
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_channel_intersections'
  character(*), intent(in) :: uid

  type network_
    character(:), allocatable :: uid
    integer :: mRegion
    type(nwk_rgn_), pointer :: region(:)  !(mRegion)
    integer :: mEnt
    type(shp_entity_), pointer :: entity(:)  !(mEnt)
    real(8) :: west, east, south, north
    real(8) :: leng
  end type

  type dct_bsn_
    integer :: n
    integer :: sz
    integer, pointer :: id(:)
    real(8), pointer :: leng(:)
    real(8), pointer :: frac(:)
  end type

  type(network_)             :: nwk
  type(nwk_rgn_)   , pointer :: rgn
  type(shp_entity_), pointer :: ent
  type(shp_part_)  , pointer :: part
  type(dct_bsn_)             :: dct_bsn

  integer :: ikEnt  ! index in network
  integer :: ipPoint  ! index in part
  integer :: iiRegion
  integer :: iiEnt
  integer :: nDiv

  integer :: gxs, gxe, gys, gye, igx, igy
  integer :: txs, txe, tys, tye, itx, ity
  integer :: xs, xe, ys, ye
  integer :: xs_this, xe_this, ys_this, ye_this
  integer :: gxs_this, gxe_this, gys_this, gye_this
  integer :: gxs_out, gxe_out, gys_out, gye_out
  integer :: gx_margin, gy_margin
  character(32) :: c_gxy
  integer :: mTile
  integer(4), allocatable :: bsnmap(:,:)

  real(8) :: wlon, wlat, elon, elat
  real(8) :: dlon_west, dlat_west, dlon_east, dlat_east
  real(8) :: clon_west, clat_west, clon_east, clat_east
  real(8), allocatable :: leng_ent(:), & !(mEnt)
                          leng_ent_apprx(:), &
                          leng_ent_err(:)
  real(8) :: leng
  integer :: iBsn
  integer :: bsnId_prev
  integer, allocatable :: arg(:)
  integer :: i

  character(CLEN_PATH) :: f, f_shp
  integer :: un
  integer :: i_
  character :: c_

  integer :: dgt_bsnId

!logical :: debug
!real(8) :: leng_edge, leng_edge_tru

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(nwk%region)
  nullify(nwk%entity)
  nullify(dct_bsn%id)
  nullify(dct_bsn%leng)
  nullify(dct_bsn%frac)
  !-------------------------------------------------------------
  ! Read network data
  !-------------------------------------------------------------
  call logent('Reading network data')

  allocate(character(1) :: nwk%uid)
  nwk%uid = uid

  f = strnk_get_f_tmp_network_entity(nwk%uid)
  open(newunit=un, file=f, status='old')
  read(un,*) c_, nwk%mRegion
  allocate(nwk%region(nwk%mRegion))
  do iiRegion = 1, nwk%mRegion
    rgn => nwk%region(iiRegion)
    read(un,*) rgn%regionName, rgn%mEnt
    read(un,*)
    rgn%iRegion = strnk_region_str2idx(rgn%regionName)

    allocate(rgn%irEnt(rgn%mEnt))
    do iiEnt = 1, rgn%mEnt
      read(un,*) rgn%irEnt(iiEnt), i_, i_, i_, i_, nDiv
      if( nDiv > 0 )then
        read(un,*)
      endif
    enddo  ! iiEnt/
  enddo  ! iiRegion/
  close(un)

  nwk%mEnt = sum(nwk%region(:)%mEnt)
  allocate(nwk%entity(nwk%mEnt))
  !call logmsg('Entities '//str(nwk%mEnt))

  ikEnt = 0
  do iiRegion = 1, nwk%mRegion
    rgn => nwk%region(iiRegion)
    f_shp = strnk_get_f_stream_shp(rgn%regionName)
    call traperr( shp_open(f_shp) )
    do iiEnt = 1, rgn%mEnt
      call add(ikEnt)
      call traperr( shp_get_entity(rgn%irEnt(iiEnt), nwk%entity(ikEnt)) )
    enddo  ! iiEnt/
    call traperr( shp_close() )
  enddo  ! iiRegion/

  nwk%west = REGION_EAST
  nwk%east = REGION_WEST
  nwk%south = REGION_NORTH
  nwk%north = REGION_SOUTH
  do ikEnt = 1, nwk%mEnt
    ent => nwk%entity(ikEnt)
    nwk%west  = min(nwk%west, ent%xmin)
    nwk%east  = max(nwk%east, ent%xmax)
    nwk%south = min(nwk%south, ent%ymin)
    nwk%north = max(nwk%north, ent%ymax)
  enddo

  nullify(rgn)
  nullify(ent)

  call logext()
  !-------------------------------------------------------------
  ! Read basin id map
  !-------------------------------------------------------------
  call logent('Reading basin id map')

  gxs = gxs_of_lon(nwk%west)
  gxe = gxe_of_lon(nwk%east)
  gys = gys_of_lat(nwk%north)
  gye = gye_of_lat(nwk%south)

  call gx_to_x(gxs, txs, xs)
  call gx_to_x(gxe, txe, xe)
  call gy_to_y(gys, tys, ys)
  call gy_to_y(gye, tye, ye)

  allocate(bsnmap(gxs:gxe,gys:gye))
  call logmsg('gxy ['//str((/gxs,gxe/),DGT_GXY,':')//&
              ', '//str((/gys,gye/),DGT_GXY,':')//']')

  do ity = tys, tye
    ys_this = 1
    ye_this = NY
    if( ity == tys ) ys_this = ys
    if( ity == tye ) ye_this = ye

    do itx = txs, txe
      xs_this = 1
      xe_this = NX
      if( itx == txs ) xs_this = xs
      if( itx == txe ) xe_this = xe

      f = get_f_map_tile(RESOLUTION_1SEC, 'bsn', itx, ity)
      if( access(f,' ') /= 0 ) cycle

      gxs_this = gx_of_x(itx, xs_this)
      gxe_this = gx_of_x(itx, xe_this)
      gys_this = gy_of_y(ity, ys_this)
      gye_this = gy_of_y(ity, ye_this)

      call logmsg('Tile '//str(tilename(itx,ity))//&
                  ' ('//str((/itx,ity/),DGT_TXY,',')//')'//&
                  ' ['//str((/gxs_this,gxe_this/),DGT_GXY,':')//&
                  ', '//str((/gys_this,gye_this/),DGT_GXY,':')//']')
      
      call traperr( rbin(&
             bsnmap(gxs_this:gxe_this,gys_this:gye_this), &
             f, lb=int((/xs_this,ys_this/),8), sz=int((/NX,NY/),8)) )
    enddo  ! itx/
  enddo  ! ity/

  call logext()
  !-------------------------------------------------------------
  ! Calc. intersections
  !-------------------------------------------------------------
  call logent('Calculating intersections')

  ! Init.
  !-------------------------------------------------------------
  allocate(leng_ent(nwk%mEnt))
  allocate(leng_ent_apprx(nwk%mEnt))
  allocate(leng_ent_err(nwk%mEnt))

  dct_bsn%sz = 32
  allocate(dct_bsn%id(0:dct_bsn%sz))
  allocate(dct_bsn%leng(0:dct_bsn%sz))
  dct_bsn%n = 0
  dct_bsn%id(0) = BSN_MISS
  dct_bsn%leng(0) = 0.d0

  ! Calc. intersections
  !-------------------------------------------------------------
  bsnId_prev = BSN_MISS
  iBsn = 0

!print*, 'Entities', nwk%mEnt
  do ikEnt = 1, nwk%mEnt
    ent => nwk%entity(ikEnt)
    part => ent%part(1)

    leng_ent(ikEnt) = 0.d0
    do ipPoint = 1, part%nPoint-1
      leng = dist_sphere(&
        part%x(ipPoint)*d2r, part%y(ipPoint)*d2r, &
        part%x(ipPoint+1)*d2r, part%y(ipPoint+1)*d2r)
      call add(leng_ent(ikEnt), leng)
    enddo  ! ipPoint

    gxs = gxs_of_lon(ent%xmin)
    gxe = gxe_of_lon(ent%xmax)
    gys = gys_of_lat(ent%ymax)
    gye = gye_of_lat(ent%ymin)
!call logmsg(str((/ent%xmin,ent%xmax,ent%ymin,ent%ymax/),'f10.6',','))
!call logmsg(str((/gxs,gxe,gys,gye/),DGT_GXY,','))
    !-----------------------------------------------------------
    ! Case: BBox of entity is in the same basin
    if( all(bsnmap(gxs:gxe,gys:gye) == bsnmap(gxs,gys)) )then
      leng_ent_apprx(ikEnt) = 0.d0
      do ipPoint = 1, part%nPoint-1
        leng = dist_sphere(&
            part%x(ipPoint)*d2r, part%y(ipPoint)*d2r, &
            part%x(ipPoint+1)*d2r, part%y(ipPoint+1)*d2r)
        call add(leng_ent_apprx(ikEnt), leng)
      enddo  ! ipPoint

      call update_isct(bsnmap(gxs,gys), leng_ent_apprx(ikEnt))
    !-----------------------------------------------------------
    ! Case: BBox of entity intersects multiple basins
    else
      leng_ent_apprx(ikEnt) = 0.d0
      do ipPoint = 1, part%nPoint-1
        if( part%x(ipPoint) < part%x(ipPoint+1) )then
          wlon = part%x(ipPoint)
          elon = part%x(ipPoint+1)
          wlat = part%y(ipPoint)
          elat = part%y(ipPoint+1)
        else
          wlon = part%x(ipPoint+1)
          elon = part%x(ipPoint)
          wlat = part%y(ipPoint+1)
          elat = part%y(ipPoint)
        endif

        gxs = gxs_of_lon(wlon)
        gxe = gxe_of_lon(elon)
        gys = gys_of_lat(wlat)
        gye = gye_of_lat(elat)

        !-------------------------------------------------------
        ! Case: north to south
        if( wlat > elat )then
          gys = gys_of_lat(wlat)
          gye = gye_of_lat(elat)
          !-----------------------------------------------------
          ! Case: BBox of edge is in the same basin
          if( all(bsnmap(gxs:gxe,gys:gye) == bsnmap(gxs,gys)) )then
            leng = dist_sphere(&
                wlon*d2r, wlat*d2r, elon*d2r, elat*d2r)
            call add(leng_ent_apprx(ikEnt), leng)

!call logmsg('(1) '//str((/wlon,wlat/),'f10.6',',')//' - '//&
!            str((/elon,elat/),'f10.6',',')//' '//&
!            str(bsnmap(gxs,gys),8)//' '//str(leng))


            call update_isct(bsnmap(gxs,gys), leng)
          !-----------------------------------------------------
          ! Case: BBox of edge intersects multiple basins
          else
!call logmsg('(2) '//str((/wlon,wlat/),'f10.6',',')//' - '//&
!            str((/elon,elat/),'f10.6',','))

            do igy = gys, gye
              if( igy == gys )then
                clat_west = wlat
                clon_west = wlon
              else
                !clat_west = south_of_gy(igy)
                !clon_west = apprx_isct_with_parallel(&
                !    wlon, wlat, elon, elat, clat_west)
                clat_west = clat_east
                clon_west = clon_east
              endif
              if( igy == gye )then
                clat_east = elat
                clon_east = elon
              else
                clat_east = south_of_gy(igy)
                clon_east = apprx_isct_with_parallel(&
                    wlon, wlat, elon, elat, clat_east)
              endif

              gxs = gxs_of_lon(clon_west)
              gxe = gxe_of_lon(clon_east)
              do igx = gxs, gxe
                if( igx == gxs )then
                  dlon_west = clon_west
                  dlat_west = clat_west
                else
                  !dlon_west = west_of_gx(igx)
                  !dlat_west = apprx_isct_with_meridian(&
                  !    wlon, wlat, elon, elat, dlon_west)
                  !call traperr( intersection_sphere_normal_meridian(&
                  !       wlon*d2r, wlat*d2r, elon*d2r, elat*d2r, dlon_west*d2r, dlat_west) )
                  !dlat_west = dlat_west * r2d
                  dlon_west = dlon_east
                  dlat_west = dlat_east
                endif
                if( igx == gxe )then
                  dlon_east = clon_east
                  dlat_east = clat_east
                else
                  dlon_east = east_of_gx(igx)
                  !dlat_east = apprx_isct_with_meridian(&
                  !    wlon, wlat, elon, elat, dlon_east)
                  call traperr( intersection_sphere_normal_meridian(&
                         wlon*d2r, wlat*d2r, elon*d2r, elat*d2r, dlon_east*d2r, dlat_east) )
                  dlat_east = dlat_east * r2d
                endif

                leng = dist_sphere(&
                    dlon_west*d2r, dlat_west*d2r, &
                    dlon_east*d2r, dlat_east*d2r)
                call add(leng_ent_apprx(ikEnt), leng)

!call logmsg(str((/dlon_west,dlat_west/),'f10.6',',')//' - '//&
!            str((/dlon_east,dlat_east/),'f10.6',',')//' '//&
!            str(bsnmap(igx,igy),8)//' '//str(leng))

                call update_isct(bsnmap(igx,igy), leng)
              enddo  ! igx/
            enddo  ! igy/
          endif
        !-----------------------------------------------------
        ! Case: south to north
        else
          gys = gys_of_lat(elat)
          gye = gye_of_lat(wlat)
          !-----------------------------------------------------
          ! Case: BBox of edge is in the same basin
          if( all(bsnmap(gxs:gxe,gys:gye) == bsnmap(gxs,gys)) )then

            leng = dist_sphere(&
                wlon*d2r, wlat*d2r, elon*d2r, elat*d2r)
            call add(leng_ent_apprx(ikEnt), leng)

!call logmsg('(3) '//str((/wlon,wlat/),'f10.6',',')//' - '//&
!            str((/elon,elat/),'f10.6',',')//&
!            str(bsnmap(gxs,gys),8)//' '//str(leng))

            call update_isct(bsnmap(gxs,gys), leng)
          !-----------------------------------------------------
          ! Case: BBox of edge intersects multiple basins
          else
!call logmsg('(4) '//str((/wlon,wlat/),'f10.6',',')//' - '//&
!            str((/elon,elat/),'f10.6',','))

            do igy = gye, gys, -1
              if( igy == gye )then
                clat_west = wlat
                clon_west = wlon
              else
                !clat_west = north_of_gy(igy)
                !clon_west = apprx_isct_with_parallel(&
                !    wlon, wlat, elon, elat, clat_west)
                clat_west = clat_east
                clon_west = clon_east
              endif
              if( igy == gys )then
                clat_east = elat
                clon_east = elon
              else
                clat_east = north_of_gy(igy)
                clon_east = apprx_isct_with_parallel(&
                    wlon, wlat, elon, elat, clat_east)
              endif

              gxs = gxs_of_lon(clon_west)
              gxe = gxe_of_lon(clon_east)
              do igx = gxs, gxe
                if( igx == gxs )then
                  dlon_west = clon_west
                  dlat_west = clat_west
                else
                  !dlon_west = west_of_gx(igx)
                  !dlat_west = apprx_isct_with_meridian(&
                  !    wlon, wlat, elon, elat, dlon_west)
                  dlon_west = dlon_east
                  dlat_west = dlat_east
                endif
                if( igx == gxe )then
                  dlon_east = clon_east
                  dlat_east = clat_east
                else
                  dlon_east = east_of_gx(igx)
                  dlat_east = apprx_isct_with_meridian(&
                      wlon, wlat, elon, elat, dlon_east)
                endif

                leng = dist_sphere(&
                    dlon_west*d2r, dlat_west*d2r, &
                    dlon_east*d2r, dlat_east*d2r)
                call add(leng_ent_apprx(ikEnt), leng)

!call logmsg(str((/dlon_west,dlat_west/),'f10.6',',')//' - '//&
!            str((/dlon_east,dlat_east/),'f10.6',',')//' '//&
!            str(bsnmap(igx,igy),8)//' '//str(leng))

                call update_isct(bsnmap(igx,igy), leng)
              enddo  ! igx/
            enddo  ! igy/
          endif
        endif
      enddo  ! ipPoint/
    endif

    leng_ent_err(ikEnt) = get_leng_err(leng_ent_apprx(ikEnt), leng_ent(ikEnt))
  enddo  ! ikEnt/

  do ikEnt = 1, nwk%mEnt
    call mul(leng_ent(ikEnt), EARTH_R)
  enddo
  do iBsn = 0, dct_bsn%n
    call mul(dct_bsn%leng(iBsn), EARTH_R)
  enddo

  call logmsg('Errors in lengths of entities')
  do ikEnt = 1, nwk%mEnt
    ent => nwk%entity(ikEnt)
    if( abs(leng_ent_err(ikEnt)) > 1d-6 )then
      call logmsg('  Entity '//str(ikEnt,dgt(nwk%mEnt))//&
                  ' Length: '//str(leng_ent(ikEnt))//&
                  ' Relative error: '//str(leng_ent_err(ikEnt)))
    endif
  enddo

  ! Summarize results
  !-------------------------------------------------------------
  nwk%leng = 0.d0
  do ikEnt = 1, nwk%mEnt
    call add(nwk%leng, leng_ent(ikEnt))
  enddo

  call realloc(dct_bsn%id  , 0, dct_bsn%n, clear=.false.)
  call realloc(dct_bsn%leng, 0, dct_bsn%n, clear=.false.)
  allocate(dct_bsn%frac(0:dct_bsn%n))
  dct_bsn%frac(:) = dct_bsn%leng(:) / nwk%leng

  if( dct_bsn%n > 0 )then
    allocate(arg(dct_bsn%n))
    call argsort(dct_bsn%frac(1:), arg)
    call sort(dct_bsn%id(1:), arg)
    call sort(dct_bsn%leng(1:), arg)
    call sort(dct_bsn%frac(1:), arg)
    deallocate(arg)
  endif

  call logmsg('Basins '//str(dct_bsn%n))
  dgt_bsnId = dgt(dct_bsn%id(:), DGT_OPT_MAX)
  do iBsn = dct_bsn%n, 0, -1
    call logmsg('  Basin '//str(dct_bsn%id(iBsn),dgt_bsnId)//&
                ' Fraction '//str(dct_bsn%frac(iBsn)*1d2,'f10.6')//' %')
  enddo
  call logmsg('  Total '//str(sum(dct_bsn%frac*1d2),'f10.6')//' %')

  ! Output results
  !-------------------------------------------------------------
  f = strnk_get_f_isct_basin(nwk%uid)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')

  gxs = gxs_of_lon(nwk%west)
  gxe = gxe_of_lon(nwk%east)
  gys = gys_of_lat(nwk%north)
  gye = gye_of_lat(nwk%south)

  mTile = 0
  do ity = tys, tye
  do itx = txs, txe
    f = get_f_map_tile(RESOLUTION_1SEC, 'bsn', itx, ity)
    if( access(f,' ') /= 0 ) cycle
    call add(mTile)
  enddo
  enddo

  write(un,"(1x,a,4(1x,f12.7))") 'BBox', nwk%west, nwk%east, nwk%south, nwk%north

  do i = 1, 2
    if( i == 1 )then
      gx_margin = 0
      gy_margin = 0
      gxs_out = gxs
      gxe_out = gxe
      gys_out = gys
      gye_out = gye
      c_gxy = 'gxy'
    else
      gx_margin = int((gxe-gxs+1) * 0.05)
      gy_margin = int((gye-gys+1) * 0.05)
      gxs_out = max(gxs - gx_margin, 1)
      gxe_out = min(gxe + gx_margin, NGX)
      gys_out = max(gys - gy_margin, 1)
      gye_out = min(gye + gy_margin, NGY)
      c_gxy = 'gxyWithMagin'
    endif

    write(un,"(1x,a,4(1x,i"//str(DGT_GXY)//"))") &
          trim(c_gxy), gxs_out, gxe_out, gys_out, gye_out
    write(un,"(1x,a,4(1x,f12.7))") &
          'BBox_of_gxy', west_of_gx(gxs_out), east_of_gx(gxe_out), &
          south_of_gy(gye_out), north_of_gy(gys_out)
    write(un,"(1x,a,1x,i0)") 'Tiles', mTile
    write(un,"(3x,a)") 'tilename tx ty xs xe ys ye gxs gxe gys gye'

    call gx_to_x(gxs_out, txs, xs)
    call gx_to_x(gxe_out, txe, xe)
    call gy_to_y(gys_out, tys, ys)
    call gy_to_y(gye_out, tye, ye)

    do ity = tys, tye
      ys_this = 1
      ye_this = NY
      if( ity == tys ) ys_this = ys
      if( ity == tye ) ye_this = ye

      do itx = txs, txe
        xs_this = 1
        xe_this = NX
        if( itx == txs ) xs_this = xs
        if( itx == txe ) xe_this = xe

        f = get_f_map_tile(RESOLUTION_1SEC, 'bsn', itx, ity)
        if( access(f,' ') /= 0 ) cycle

        gxs_this = gx_of_x(itx, xs_this)
        gxe_this = gx_of_x(itx, xe_this)
        gys_this = gy_of_y(ity, ys_this)
        gye_this = gy_of_y(ity, ye_this)

        write(un,"(3x,a,"//&
                   "2(1x,i"//str(DGT_TXY)//"),"//&
                   "4(1x,i"//str(DGT_XY)//"),"//&
                   "4(1x,i"//str(DGT_GXY)//"))") &
              tilename(itx,ity), &
              itx, ity, xs_this, xe_this, ys_this, ye_this, &
              gxs_this, gxe_this, gys_this, gye_this
      enddo  ! itx/
    enddo  ! ity/
  enddo  ! i/

  write(un,"(1x,a,1x,i0)") 'Basins', dct_bsn%n
  write(un,"(3x,a)") 'ID fraction'
  do iBsn = dct_bsn%n, 0, -1
    write(un,"(3x,i"//str(dgt_bsnId)//",1x,es12.5)") &
          dct_bsn%id(iBsn), dct_bsn%frac(iBsn)
  enddo  ! iBsn/

  close(un)

  call logext()
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  deallocate(bsnmap)

  deallocate(nwk%region)
  deallocate(nwk%entity)
  call realloc(dct_bsn%id, 0)
  call realloc(dct_bsn%leng, 0)
  call realloc(dct_bsn%frac, 0)
  !-------------------------------------------------------------
  call logret()
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine update_isct(bsnId, leng)
  implicit none
  integer, intent(in) :: bsnId
  real(8), intent(in) :: leng

  if( leng == 0.d0 ) return

  if( bsnId /= bsnId_prev )then
    bsnId_prev = bsnId
    do iBsn = 0, dct_bsn%n
      if( dct_bsn%id(iBsn) == bsnId ) exit
    enddo
    if( iBsn == dct_bsn%n+1 )then
      if( dct_bsn%n == dct_bsn%sz )then
        dct_bsn%sz = dct_bsn%sz*2
        call realloc(dct_bsn%id  , 0, dct_bsn%sz, clear=.false.)
        call realloc(dct_bsn%leng, 0, dct_bsn%sz, clear=.false.)
      endif
      call add(dct_bsn%n)
      dct_bsn%id(dct_bsn%n) = bsnId
      dct_bsn%leng(dct_bsn%n) = 0.d0
    endif
  endif

  call add(dct_bsn%leng(iBsn), leng)
end subroutine update_isct
!---------------------------------------------------------------
real(8) function get_leng_err(val, tru) result(err)
  implicit none
  real(8), intent(in) :: val, tru

  if( tru < 1d-50 )then
    err = 0.d0
  else
    err = (val - tru) / tru
  endif
end function get_leng_err
!---------------------------------------------------------------
end subroutine calc_channel_intersections
!===============================================================
!
!===============================================================
subroutine evalNetworkBasinConsistencies()
  use c2_jflw_io, only: &
        jflw_get_f_lst_tile => get_f_lst_tile
  use c2_strnk_io, only: &
        strnk_get_f_tmp_networks_lst => get_f_tmp_networks_lst, &
        strnk_get_f_isct_basin       => get_f_isct_basin      , &
        strnk_get_f_eval_basin       => get_f_eval_basin      , &
        strnk_get_f_lst_eval_basin   => get_f_lst_eval_basin
  use mod_util, only: &
        get_fmt_network
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'evalNetworkBasinConsistencies'

  type nwk_bsn_
    integer :: id
    real(8) :: frac
  end type

  type nwk_
    character(:), allocatable :: uid
    integer :: mBasin
    type(nwk_bsn_), pointer :: basin(:)
    integer :: mEnt
    real(8) :: leng
    integer :: status
    real(8) :: score
  end type

  type dct_bsn_
    integer :: n
    integer, pointer :: id(:)  !(n)
    real(8), pointer :: area(:)  !(n)
  end type

  type basin_
    real(8) :: area
    integer :: n
    integer :: sz
    integer, pointer :: iNwk(:)  !(sz)->(n)
    real(8), pointer :: leng(:)
    real(8), pointer :: frac(:)
    integer, pointer :: arg_iNwk(:)
    real(8) :: leng_sum
    integer :: n_frac_signif
  end type

  type(nwk_)    , pointer :: lst_nwk(:), nwk
  type(nwk_bsn_), pointer :: nwbsn
  type(basin_)  , pointer :: lst_bsn(:), bsn
  integer :: nNwk_all, iNwk_all
  integer :: nNwk, iNwk, jNwk
  integer :: iiNwk
  integer :: nBasin, iBasin
  integer :: mBasin, iiBasin
  integer :: nTile, iTile
  real(8) :: leng
  real(8) :: frac
  integer, pointer :: arg(:)

  real(8), parameter :: THRESH_BSNARA = 1.d2  ![m2]
  real(8), parameter :: THRESH_FRACINGROUP = 0.02d0
  real(8), parameter :: THRESH_FRACINBASIN_SIGNIF   = 0.05d0
  real(8), parameter :: THRESH_FRACINBASIN_OCCUPIED = 0.95d0
  real(8), parameter :: THRESH_GROUPLENG = 1.d3  ![m]

  integer, parameter :: GRP_STAT__DO   = 0
  integer, parameter :: GRP_STAT__SKIP = 1

  integer, parameter :: GRP_CONSISTENCY__00 =  0
  integer, parameter :: GRP_CONSISTENCY__11 = 11
  integer, parameter :: GRP_CONSISTENCY__12 = 12
  integer, parameter :: GRP_CONSISTENCY__21 = 21
  integer, parameter :: GRP_CONSISTENCY__22 = 22

  character(CLEN_PATH) :: f
  integer :: un
  integer :: i_
  character :: c_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call get_fmt_network(dgt_irEnt, dgt_uid)
  allocate(character(dgt_uid) :: nwk%uid)
  !-------------------------------------------------------------
  ! Make basin list
  !-------------------------------------------------------------
  call logent('Making basin list')

  f = jflw_get_f_lst_tile(RESOLUTION_1SEC, 'id_area')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) c_, nBasin
  allocate(lst_bsn(nBasin))

  read(un,*)
  do iBasin = 1, nBasin
    bsn => lst_bsn(iBasin)
    read(un,*) i_, i_, bsn%area
  enddo
  close(un)

  call logext()
  !-------------------------------------------------------------
  ! Read channel networks
  !-------------------------------------------------------------
  call logent('Reading channel networks')

  f = strnk_get_f_tmp_networks_lst()
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) c_, nNwk_all

  read(un,*)
  nNwk = 0
  do iNwk_all = 1, nNwk_all
    read(un,*) c_, i_, i_, leng
    if( leng < THRESH_GROUPLENG ) cycle
    call add(nNwk)
  enddo

  call logmsg('Networks with length > '//str(THRESH_GROUPLENG*1d-3,'es9.3')//'km: '//str(nNwk))
  allocate(lst_nwk(nNwk))

  rewind(un)
  read(un,*)
  read(un,*)
  iNwk = 0
  do iNwk_all = 1, nNwk_all
    read(un,*) c_, i_, i_, leng
    if( leng < THRESH_GROUPLENG ) cycle
    call add(iNwk)
    nwk => lst_nwk(iNwk)
    backspace(un)
    read(un,*) c_, nwk%uid, nwk%mEnt, nwk%leng
  enddo
  close(un)

  call logext()
  !-------------------------------------------------------------
  ! Read intersecting basins
  !-------------------------------------------------------------
  call logent('Reading intersecting basins')

  do iBasin = 1, nBasin
    bsn => lst_bsn(iBasin)
    bsn%n = 0
    bsn%sz = 0
    nullify(bsn%iNwk)
    nullify(bsn%leng)
    nullify(bsn%frac)
    nullify(bsn%arg_iNwk)
    bsn%leng_sum = 0.d0
    bsn%n_frac_signif = 0
  enddo  ! iBasin/

  do iNwk = 1, nNwk
    nwk => lst_nwk(iNwk)

    f = strnk_get_f_isct_basin(nwk%uid)
    if( iNwk <= 2 .or. iNwk >= nNwk-1 )then
      call logmsg('Reading '//str(f))
    elseif( iNwk == 3 )then
      call logmsg('...')
    endif
    open(newunit=un, file=f, status='old')

    read(un,*) ! BBox
    read(un,*) ! gxy
    read(un,*) ! BBox of gxy

    read(un,*) c_, nTile
    read(un,*)
    do iTile = 1, nTile
      read(un,*)
    enddo

    read(un,*) c_, mBasin
    read(un,*)
    do iiBasin = 1, mBasin
      read(un,*) i_, frac
      if( frac < THRESH_FRACINGROUP ) exit
    enddo
    nwk%mBasin = iiBasin-1
!print*, 'length', nwk%leng
!print*, 'mBasin', nwk%mBasin
print*, nwk%uid, nwk%mBasin

    if( nwk%mBasin == 0 )then
      close(un)
      cycle
    endif

    allocate(nwk%basin(nwk%mBasin))

    if( nwk%mBasin == mBasin )then
      do iiBasin = 1, nwk%mBasin
        backspace(un)
      enddo
    else
      do iiBasin = 1, nwk%mBasin+1
        backspace(un)
      enddo
    endif

    do iiBasin = 1, nwk%mBasin
      nwbsn => nwk%basin(iiBasin)

      read(un,*) nwbsn%id, nwbsn%frac

      iBasin = nwbsn%id

      if( iBasin > nBasin )then
        call logwrn('Network '//nwk%uid//' ('//str(iNwk)//&
                    ') Basin ID: '//str(nwbsn%id)//' Frac: '//str(nwbsn%frac))
        cycle
      endif

      bsn => lst_bsn(iBasin)
      if( bsn%n == bsn%sz )then
        bsn%sz = max(bsn%sz*2, 2)
        call realloc(bsn%iNwk, bsn%sz, clear=.false.)
        call realloc(bsn%leng, bsn%sz, clear=.false.)
      endif
      call add(bsn%n)
      bsn%iNwk(bsn%n) = iNwk
      bsn%leng(bsn%n) = nwk%leng * nwbsn%frac

!if( nwk%uid == '22335236' )then
!  print*, nwbsn%id, nwbsn%frac
!endif
    enddo  ! iiBasin/

    close(un)
  enddo  ! iNwk/

  nullify(arg)
  do iBasin = 1, nBasin
    bsn => lst_bsn(iBasin)
    call realloc(bsn%iNwk, bsn%n, clear=.false.)
    call realloc(bsn%leng, bsn%n, clear=.false.)

    if( bsn%n == 0 ) cycle

    allocate(bsn%arg_iNwk(bsn%n))
    allocate(bsn%frac(bsn%n))
    bsn%leng_sum = sum(bsn%leng(:))
!print*, iBasin, bsn%leng_sum
    bsn%frac(:) = bsn%leng(:) / bsn%leng_sum

    if( size(arg) < bsn%n ) call realloc(arg, bsn%n)
    call argsort(bsn%frac, arg(:bsn%n))
    call sort(bsn%iNwk, arg(:bsn%n))
    call sort(bsn%leng, arg(:bsn%n))
    call sort(bsn%frac, arg(:bsn%n))
    call argsort(bsn%iNwk, bsn%arg_iNwk)

    bsn%n_frac_signif = 0
    do iiNwk = 1, bsn%n
      if( bsn%frac(iiNwk) < THRESH_FRACINBASIN_SIGNIF ) exit
      call add(bsn%n_frac_signif)
    enddo  ! iiNwk/
  enddo  ! iBasin/
  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  ! Eval.
  !-------------------------------------------------------------
  call logent('Evaluating consistency')

  do iNwk = 1, nNwk
    nwk => lst_nwk(iNwk)
    !-----------------------------------------------------------
    ! Classify situation
    !-----------------------------------------------------------
    selectcase( nwk%mBasin )
    !-----------------------------------------------------------
    ! Case: No intersection
    case( 0 )
      nwk%status = GRP_CONSISTENCY__00
    !-----------------------------------------------------------
    ! Case: Intersects one basin
    case( 1 )
      nwbsn => nwk%basin(1)
      iBasin = nwbsn%id

      ! Too small basin (area < thresh)
      if( iBasin > nBasin )then
        cycle
      endif

      bsn => lst_bsn(iBasin)

      nwk%status = 11

      selectcase( bsn%n )
      !---------------------------------------------------------
      ! Case: ERROR
      case( 0 )
        call errend(msg_unexpected_condition()//&
                  '\n  bsn%n == 0')
      !---------------------------------------------------------
      ! Case: The basin intersects only this network (one-on-one)
      case( 1 )
        if( iNwk /= bsn%iNwk(1) )then
          call errend(msg_unexpected_condition()//&
                    '\n  bsn%n == 1 and iNwk /= bsn%iNwk(1)')
        endif

        if( bsn%frac(1) < THRESH_FRACINBASIN_OCCUPIED )then
          nwk%status = 12
        endif
      !---------------------------------------------------------
      ! Case: The basin intersects multiple networks
      case( 2: )
        call search(iNwk, bsn%iNwk, bsn%arg_iNwk, iiNwk)
        if( iiNwk == 0 )then
          call errend(msg_unexpected_condition()//&
                    '\n  $iNwk was not found in $bsn%iNwk.')
        endif

        if( bsn%iNwk(bsn%arg_iNwk(iiNwk)) /= iNwk )then
          print*, 'ERROR'
          stop
        endif

        nwk%status = 13
      !---------------------------------------------------------
      ! Case: ERROR
      case default
        call errend(msg_invalid_value('bsn%n', bsn%n))
      endselect
    !-----------------------------------------------------------
    ! Case: Intersects multiple basins
    case( 2: )
      nwk%status = 21

      do iiBasin = 1, nwk%mBasin
        nwbsn => nwk%basin(iiBasin)
        iBasin = nwbsn%id

        ! Too small basin (area < thresh)
        if( iBasin > nBasin )then
          cycle
        endif

        bsn => lst_bsn(iBasin)
        selectcase( bsn%n )
        !-------------------------------------------------------
        ! Case: ERROR
        case( 0 )
          call errend(msg_unexpected_condition()//&
                    '\n  bsn%n == 0')
        !-------------------------------------------------------
        ! Case:
        case( 1 )
          if( iNwk /= bsn%iNwk(1) )then
            call errend(msg_unexpected_condition()//&
                      '\n  bsn%n == 1 and iNwk /= bsn%iNwk(1)')
          endif

          if( bsn%frac(1) < THRESH_FRACINBASIN_OCCUPIED )then
            nwk%status = 22
          endif
        !-------------------------------------------------------
        ! Case:
        case( 2: )
          call search(iNwk, bsn%iNwk, bsn%arg_iNwk, iiNwk)
          if( iiNwk == 0 )then
            call errend(msg_unexpected_condition()//&
                      '\n  $iNwk was not found in $bsn%iNwk.')
          endif

          if( bsn%iNwk(bsn%arg_iNwk(iiNwk)) /= iNwk )then
            print*, 'ERROR'
            stop
          endif

          nwk%status = 22

        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call errend(msg_invalid_value('bsn%n', bsn%n))
        endselect
      enddo  ! iiBasin/
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('nwk%mBasin', nwk%mBasin))
    endselect
    !-----------------------------------------------------------
    ! Calc. consistency score
    !-----------------------------------------------------------
    nwk%score = 0.d0
    do iiBasin = 1, nwk%mBasin
      nwbsn => nwk%basin(iiBasin)
      iBasin = nwbsn%id
      bsn => lst_bsn(iBasin)
      do iiNwk = 1, bsn%n
        jNwk = bsn%iNwk(iiNwk)
        if( jNwk == iNwk )then
          call add(nwk%score, (nwbsn%frac * bsn%frac(iinwk))**2)
          exit
        endif
      enddo
    enddo
    nwk%score = sqrt(nwk%score)
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    !call logmsg(str(iNwk,dgt(nNwk))//&
    !            'Netowrk '//str(nwk%uid,)//&
    !            ' Status '//str(nwk%status,-2))

    f = strnk_get_f_eval_basin(nwk%uid)
    if( inwk <= 2 .or. iNwk >= nNwk-1 )then
      call logmsg('Writing '//str(f))
    elseif( iNwk == 3 )then
      call logmsg('...')
    endif
    open(newunit=un, file=f, status='replace')
    write(un,"(a)") '{'
    write(un,"(2x,a)") '"status": '//str(nwk%status)//','
    write(un,"(2x,a)") '"score": '//str(nwk%score)//','
    write(un,"(2x,a)") '"leng":'//str(nwk%leng)//','
    write(un,"(2x,a)") '"nBasin":'//str(nwk%mBasin)//','
    write(un,"(2x,a)") '"basins": ['
    do iiBasin = 1, nwk%mBasin
      write(un,"(4x,a)") '{'
      nwbsn => nwk%basin(iiBasin)
      iBasin = nwbsn%id
      if( iBasin <= nBasin )then
        bsn => lst_bsn(iBasin)
        write(un,"(6x,a)") '"index": '//str(iBasin)//','
        write(un,"(6x,a)") '"area": '//str(bsn%area)//','
        write(un,"(6x,a)") '"frac_of_network": '//str(nwbsn%frac)//','
        write(un,"(6x,a)") '"nNetwork": '//str(bsn%n)//','
        write(un,"(6x,a)") '"networks": ['
        do iiNwk = 1, bsn%n
          jNwk = bsn%iNwk(iiNwk)
          write(un,"(8x,a)") '{'
          write(un,"(10x,a)") '"id": "'//str(lst_nwk(jNwk)%uid)//'",'
          write(un,"(10x,a)") '"leng": '//str(bsn%leng(iiNwk))//','
          write(un,"(10x,a)") '"frac_in_basin": '//str(bsn%frac(iiNwk))
          if( iiNwk < bsn%n )then
            write(un,"(8x,a)") '},'
          else
            write(un,"(8x,a)") '}'
          endif
        enddo
        write(un,"(6x,a)") ']'
      else
        write(un,"(6x,a)") '"id": '//str(iBasin)
      endif
      if( iiBasin < nwk%mBasin )then
        write(un,"(4x,a)") '},'
      else
        write(un,"(4x,a)") '}'
      endif
    enddo  ! iiBasin/
    write(un,"(2x,a)") ']'
    write(un,"(a)") '}'
    close(un)
  enddo  ! iNwk/

  allocate(arg(nNwk))
  call argsort(lst_nwk(:)%leng, arg)

  f = strnk_get_f_lst_eval_basin()
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(1x,a)") 'uid leng status score'
  do iiNwk = nNwk, 1, -1
    iNwk = arg(iiNwk)
    nwk => lst_nwk(iNwk)
    write(un,"(1x,a)") &
          nwk%uid//' '//str(nwk%leng)//' '//&
          str(nwk%status,2)//' '//str(nwk%score,'f4.2')
  enddo  ! iiNwk/
  close(un)

  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  nullify(nwk)
  deallocate(lst_nwk)

  nullify(nwbsn)
  nullify(bsn)
  deallocate(lst_bsn)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine evalNetworkBasinConsistencies
!===============================================================
!
!===============================================================
end module mod_eval_basin
