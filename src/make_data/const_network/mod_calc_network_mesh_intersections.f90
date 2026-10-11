module mod_calc_network_mesh_intersections
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_util
  use c2_strnk_const
  use c2_strnk_type
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calcNetworkMeshIntersections
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_calc_network_mesh_intersections'

  type node_
    real(8) :: lon, lat
  end type

  type isct_
    integer :: n
    integer, pointer :: x(:), y(:)
    real(8), pointer :: leng(:)
  end type

  type channel_
    integer :: nPt
    real(8), pointer :: lon(:), lat(:)
    type(node_) :: node(2)
    type(isct_) :: isct
  end type

  type network_
    integer :: nCh
    type(channel_), pointer :: channel(:)
  end type
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calcNetworkMeshIntersections(uid)
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c2_strnk_io, only: &
    get_f_lst_networks_channel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calcNetworkMeshIntersections'
  character(*), intent(in) :: uid

  character(DGT_NWKUID) :: uid_
  integer :: nNwk, iNwk
  character :: c_
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Setup J-FlwDir grid
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  ! Run
  !-------------------------------------------------------------
  if( uid == '' )then
    f = get_f_lst_networks_channel()
    call logmsg('Reading '//str(f))
    open(newunit=un, file=f, status='old')
    read(un,*) c_, nNwk
    read(un,*)
    do iNwk = 1, nNwk
      read(un,*) c_, uid_
      call calc_network_mesh_intersections(uid_)
    enddo
    close(un)
  else
    call calc_network_mesh_intersections(uid)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calcNetworkMeshIntersections
!===============================================================
!
!===============================================================
subroutine calc_network_mesh_intersections(uid)
  use c1_grid, only: &
    apprx_isct_with_meridian, &
    apprx_isct_with_parallel
  use c2_jflw_const
  use c2_jflw_grid, only: &
    gxs_of_lon , &
    gxe_of_lon , &
    gys_of_lat , &
    gye_of_lat , &
    west_of_gx , &
    east_of_gx , &
    south_of_gy, &
    north_of_gy
  use c2_strnk_io, only: &
    read_network                , &
    get_f_network_channel       , &
    get_f_network_isct_with_mesh
  use c2_strnk_util, only: &
    clear_cmn_network
  use c3_joint_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_network_mesh_intersections'
  character(*), intent(in) :: uid

  type(cmn_network_) :: cmnnwk
  type(network_) :: nwk
  type(channel_), pointer :: ch
  type(isct_), pointer :: ci
  integer :: jCh
  integer :: jPt
  integer :: xs, xe, ys, ye
  integer :: ix, iy
  integer :: xwest, xeast, ysouth, ynorth
  real(8) :: clon1, clat1, clon2, clat2
  real(8) :: dlon1, dlat1, dlon2, dlat2
  integer :: nMesh_tmp
  real(8) :: leng
  real(8) :: leng_nwk, leng_isct

  character(CLEN_PATH) :: f
  integer :: un

  integer :: jCh_debug = 0
  logical :: debug_this

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Read StrRank network data
  !-------------------------------------------------------------
  call logmsg('Network '//str(uid))

  allocate(character(len_trim(uid)) :: cmnnwk%uid)
  cmnnwk%uid = uid

  f = get_f_network_channel(uid, 'sbin')

  call read_network(f, cmnnwk)

  call copy_cmnnwk_to_nwk(cmnnwk, nwk)

  call clear_cmn_network(cmnnwk)
  !-------------------------------------------------------------
  ! Calc. intersection
  !-------------------------------------------------------------
  leng_nwk = 0.d0
  leng_isct = 0.d0

  do jCh = 1, nwk%nCh
    debug_this = jCh == jCh_debug

    ch => nwk%channel(jCh)
    ci => ch%isct

    xs = gxs_of_lon(minval(ch%node(:)%lon))
    xe = gxe_of_lon(maxval(ch%node(:)%lon))
    ys = gys_of_lat(maxval(ch%node(:)%lat))
    ye = gye_of_lat(minval(ch%node(:)%lat))
    nMesh_tmp = (xe - xs + 1) * (ye - ys + 1) * 4
    if( nMesh_tmp < 0 )then
      call errend('nMesh_tmp: '//str(nMesh_tmp))
    endif
    allocate(ci%x(nMesh_tmp))
    allocate(ci%y(nMesh_tmp))
    allocate(ci%leng(nMesh_tmp))

    ci%n = 0

    if( debug_this )then
      call logmsg('ch#'//str(jCh)//' nPt '//str(ch%nPt))
      call logmsg('  x '//str((/xs,xe/),' - ')//' y '//str((/ys,ye/),' - '))
    endif

    do jPt = 1, ch%nPt-1

      if( debug_this )then
        call logmsg('pt#'//str(jPt)//' '//&
          slonlat(ch%lon(jPt),ch%lat(jPt))//' - '//slonlat(ch%lon(jPt+1),ch%lat(jPt+1)))
      endif

      ! north to south
      if( ch%lat(jPt) > ch%lat(jPt+1) )then
        ynorth = gys_of_lat(ch%lat(jPt))
        ysouth = gye_of_lat(ch%lat(jPt+1))

        if( debug_this )then
          call logmsg('y n2s '//str((/ynorth,ysouth/),DGT_GXY))
        endif

        if( ynorth > ysouth )then
          if( ch%lat(jPt) /= ch%lat(jPt+1) )then
            call errend('ysouth < ynorth. ch#'//str(jCh))
          endif
          ynorth = ysouth
        endif

        do iy = ynorth, ysouth
          if( iy == ynorth )then
            clat1 = ch%lat(jPt)
            clon1 = ch%lon(jPt)
          else
            clat1 = clat2
            clon1 = clon2
          endif
          if( iy == ysouth )then
            clat2 = ch%lat(jPt+1)
            clon2 = ch%lon(jPt+1)
          else
            clat2 = south_of_gy(iy)
            clon2 = apprx_isct_with_parallel(&
              ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), clat2)
          endif

          call loop_for_x()
        enddo  ! iy/

      ! south to north
      else
        ysouth = gye_of_lat(ch%lat(jPt))
        ynorth = gys_of_lat(ch%lat(jPt+1))

        if( debug_this )then
          call logmsg('y s2n '//str((/ysouth,ynorth/),DGT_GXY))
        endif

        if( ysouth < ynorth )then
          if( ch%lat(jPt) /= ch%lat(jPt+1) )then
            call errend('ysouth < ynorth. ch#'//str(jCh))
          endif
          ynorth = ysouth
        endif      


        do iy = ysouth, ynorth, -1
          if( iy == ysouth )then
            clat1 = ch%lat(jPt)
            clon1 = ch%lon(jPt)
          else
            clat1 = clat2
            clon1 = clon2
          endif
          if( iy == ynorth )then
            clat2 = ch%lat(jPt+1)
            clon2 = ch%lon(jPt+1)
          else
            clat2 = north_of_gy(iy)
            clon2 = apprx_isct_with_parallel(&
              ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), clat2)
          endif

          call loop_for_x()
        enddo  ! iy/
      endif

    enddo  ! jPt/

    if( ci%n == 0 )then
      call logmsg('ch#'//str(jCh))
      call logmsg('  '//slonlat(ch%node(1)%lon,ch%node(1)%lat)//&
                  ' - '//slonlat(ch%node(2)%lon,ch%node(2)%lat))
      call logmsg('  x '//str((/xs,xe/),' - ')//' y '//str((/ys,ye/),' - '))
      call logmsg('  n '//str(ci%n))
    endif
    call realloc(ci%x, ci%n, clear=.false.)
    call realloc(ci%y, ci%n, clear=.false.)
    call realloc(ci%leng, ci%n, clear=.false.)

    call add(leng_isct, sum(ci%leng))

    do jPt = 1, ch%nPt-1
      call add(leng_nwk, &
        dist_sphere(ch%lon(jPt)*d2r, ch%lat(jPt)*d2r, &
          ch%lon(jPt+1)*d2r, ch%lat(jPt+1)*d2r) * EARTH_R &
      )
    enddo  ! jPt/
  enddo  ! jCh/

  call logmsg('Length nwk: '//str(leng_nwk)//' isct: '//str(leng_isct))
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f = get_f_network_isct_with_mesh(uid, MESHTYPE__JFLW)
  call logmsg('Writing '//str(f))

  open(newunit=un, file=f, form='unformatted', access='sequential', status='replace')

  write(un) nwk%nCh

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    ci => ch%isct

    write(un) ci%n
    write(un) ci%x(:)
    write(un) ci%y(:)
    write(un) ci%leng(:)
  enddo  ! jCh/

  close(un)
  !-------------------------------------------------------------
  ! Clear
  !-------------------------------------------------------------
  nullify(ch)
  deallocate(nwk%channel)
  nwk%nCh = 0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine loop_for_x()
  implicit none

  ! west to east
  if( ch%lon(jPt) < ch%lon(jPt+1) )then
    xwest = gxs_of_lon(clon1)
    xeast = gxe_of_lon(clon2)
    if( debug_this )then
      call logmsg('y '//str(iy,DGT_GXY)//' x w2e '//str((/xwest,xeast/),DGT_GXY))
    endif

    do ix = xwest, xeast
      if( ix == xwest )then
        dlon1 = clon1
        dlat1 = clat1
      else
        dlon1 = dlon2
        dlat1 = dlat2
      endif
      if( ix == xeast )then
        dlon2 = clon2
        dlat2 = clat2
      else
        dlon2 = east_of_gx(ix)
        dlat2 = apprx_isct_with_meridian(&
          ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), dlon2)
      endif

      call update()
    enddo  ! ix/

  ! east to west
  else
    xwest = gxs_of_lon(clon2)
    xeast = gxe_of_lon(clon1)
    if( debug_this )then
      call logmsg('y '//str(iy,DGT_GXY)//' x e2w '//str((/xeast,xwest/),DGT_GXY))
    endif

    do ix = xeast, xwest, -1
      if( ix == xeast )then
        dlon1 = clon1
        dlat1 = clat1
      else
        dlon1 = dlon2
        dlat1 = dlat2
      endif
      if( ix == xwest )then
        dlon2 = clon2
        dlat2 = clat2
      else
        dlon2 = west_of_gx(ix)
        dlat2 = apprx_isct_with_meridian(&
          ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1), dlon2)
      endif

      call update()
    enddo  ! ix/
  endif
end subroutine loop_for_x
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine update()
  implicit none

  leng = dist_sphere(dlon1*d2r, dlat1*d2r, dlon2*d2r, dlat2*d2r) * EARTH_R

  if( ci%n > 0 )then
    if( ci%x(ci%n) == ix .and. ci%y(ci%n) == iy )then
      call add(ci%leng(ci%n), leng)
      return
    endif

    if( abs(ci%x(ci%n) - ix) > 1 .or. &
        abs(ci%y(ci%n) - iy) > 1 )then
      call errend('Discontinuity was found in the intersection. ch#'//str(jCh)//&
      '\n  isct#'//str(ci%n)//' ('//str((/ci%x(ci%n),ci%y(ci%n)/),DGT_GXY,',')//')'//&
      '\n  isct#'//str(ci%n+1)//' ('//str((/ix,iy/),DGT_GXY,',')//')')
    endif
  endif

  if( size(ci%x) == ci%n )then
    call realloc(ci%x, ci%n*2, clear=.false.)
    call realloc(ci%y, ci%n*2, clear=.false.)
    call realloc(ci%leng, ci%n*2, clear=.false.)
  endif

  call add(ci%n)
  ci%x(ci%n) = ix
  ci%y(ci%n) = iy
  ci%leng(ci%n) = leng
end subroutine update
!---------------------------------------------------------------
end subroutine calc_network_mesh_intersections
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
subroutine copy_cmnnwk_to_nwk(cmnnwk, nwk)
  implicit none
  type(cmn_network_), intent(in) :: cmnnwk
  type(network_), intent(out) :: nwk

  type(cmn_channel_), pointer :: cmnch
  type(channel_), pointer :: ch
  integer :: jCh

  nwk%nCh = cmnnwk%nCh
  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    cmnch => cmnnwk%channel_(jCh)

    ch%nPt = cmnch%n
    allocate(ch%lon(ch%nPt))
    allocate(ch%lat(ch%nPt))
    ch%lon(:) = cmnch%lon(:)
    ch%lat(:) = cmnch%lat(:)

    ch%node(:)%lon = cmnch%node_(:)%lon
    ch%node(:)%lat = cmnch%node_(:)%lat
  enddo  ! jCh/
end subroutine copy_cmnnwk_to_nwk
!===============================================================
!
!===============================================================
end module mod_calc_network_mesh_intersections
