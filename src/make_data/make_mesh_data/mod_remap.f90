module mod_remap
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type
  use c1_util, only: &
    sBBox
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: makeRemappingTables
  public :: remap
  !-------------------------------------------------------------
  ! Private module variables (type)
  !-------------------------------------------------------------
  type rt_
    integer(8) :: nij
    integer(8), pointer :: sidx(:), tidx(:)
    real(8)   , pointer :: area(:), coef(:)
  end type

  type valwgt_
    integer :: i
    integer :: n
    integer(1), pointer :: vi1(:)
    real(8)   , pointer :: wgt(:)
    real(8) :: wgt_sum
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_remap'
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
subroutine makeRemappingTables(&
    resl, name_src, resl_src, overwrite)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeRemappingTables'
  character(*), intent(in) :: resl  ! Resolution of output data
  character(*), intent(in) :: name_src
  character(*), intent(in) :: resl_src
  logical, intent(in) :: overwrite

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( name_src )
  case( 'NLNI' )
    call make_rt_from_nlni(resl, resl_src, overwrite)
  case( 'J-FlwDir' )
    call make_rt_from_jflw(resl, resl_src, overwrite)
  case default
    call errend('Invalid value in `name_src`: '//str(name_src))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeRemappingTables
!===============================================================
!
!===============================================================
subroutine make_rt_from_jflw(resl, resl_src, overwrite)
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c2_jflw_grid, only: &
    west_of_tx, &
    east_of_tx, &
    south_of_ty, &
    north_of_ty
  use c2_jflw_io, only: &
    tilename, &
    get_dir_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt_from_jflw'
  character(*), intent(in) :: resl
  character(*), intent(in) :: resl_src
  logical, intent(in) :: overwrite

  integer :: itx, ity
  integer :: nx1, ny1
  real(8) :: west1, east1, south1, north1
  integer :: nx2, ny2
  real(8) :: west2, east2, south2, north2
  character(CLEN_PATH) :: dir
  character(CLEN_PATH) :: f_conf, f_log
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ity = 1, NTY
  do itx = 1, NTX
!if( tilename(itx,ity) /= 'n36e140' ) cycle
    call set_resolution(resl_src)

    nx1 = NX
    ny1 = NY
    west1 = west_of_tx(itx)
    east1 = east_of_tx(itx)
    south1 = south_of_ty(ity)
    north1 = north_of_ty(ity)

    call set_resolution(resl)

    nx2 = NX
    ny2 = NY
    west2 = west_of_tx(itx)
    east2 = east_of_tx(itx)
    south2 = south_of_ty(ity)
    north2 = north_of_ty(ity)

    dir = joined(get_dir_rt(resl_src, resl), tilename(itx,ity))
    call traperr( mkdir(dir) )

    if( access(joined(dir,'grid.bin'),' ') == 0 .and. .not. overwrite )then
      call logmsg('File already exists: '//str(joined(dir,'grid.bin')))
      cycle
    endif

    f_conf = joined(dir, 'conf')
    call logmsg('Writing '//str(f_conf))
    open(newunit=un, file=f_conf, status='replace')
    call write_conf()
    close(un)

    f_log = joined(dir, 'log')
    call logmsg('Log: '//str(f_log))
    call execute_command_line(&
        'srun '//PROG_SPRING_REMAP//' '//str(f_conf)//&
        ' >'//str(f_log)//' 2>&1')
  enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine write_conf()
  implicit none

  call w('#')
  call w('path_report: "'//str(joined(dir,'report.txt'))//'"')
  call w('')
  call w('[mesh_latlon]')
  call w('  nx: '//str(nx1))
  call w('  ny: '//str(ny1))
  call w('  west: '//str(west1))
  call w('  east: '//str(east1))
  call w('  south: '//str(south1))
  call w('  north: '//str(north1))
  call w('  is_south_to_north: .false.')
  call w('[end]')
  call w('')
  call w('[mesh_latlon]')
  call w('  nx: '//str(nx2))
  call w('  ny: '//str(ny2))
  call w('  west: '//str(west2))
  call w('  east: '//str(east2))
  call w('  south: '//str(south2))
  call w('  north: '//str(north2))
  call w('  is_south_to_north: .false.')
  call w('[end]')
  call w('')
  call w('[remapping]')
  call w('  dir: "'//str(dir)//'"')
  call w('  fout_rt_sidx: "grid.bin", int8, rec=1')
  call w('  fout_rt_tidx: "grid.bin", int8, rec=2')
  call w('  fout_rt_area: "area.bin", dble')
  call w('[end]')
  call w('')
  call w('[options]')
  call w('  old_files: remove')
  call w('  earth_geosys: WGS84')
  call w('  earth_rtyp: volmetric')
  call w('[end]')
end subroutine write_conf
!---------------------------------------------------------------
subroutine w(s)
  implicit none
  character(*), intent(in) :: s

  write(un,"(a)") s
end subroutine w
!---------------------------------------------------------------
end subroutine make_rt_from_jflw
!===============================================================
! NLNI to J-FlwDir
!===============================================================
subroutine make_rt_from_nlni(&
    resl, resl_nlni, overwrite)
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c3_nlni_const, &
    nlni_set_resolution => nlni_set_resolution
  use c2_jflw_grid, only: &
    gxs_of_tx, &
    gxe_of_tx, &
    gys_of_ty, &
    gye_of_ty, &
    west_of_tx, &
    east_of_tx, &
    south_of_ty, &
    north_of_ty
  use c2_jflw_io, only: &
    get_f_map_tile
  use c3_nlni_grid, only: &
    nlni_tx_of_gx, &
    nlni_ty_of_gy, &
    nlni_gxs_of_lon, &
    nlni_gxe_of_lon, &
    nlni_gys_of_lat, &
    nlni_gye_of_lat, &
    nlni_west_of_tx, &
    nlni_east_of_tx, &
    nlni_south_of_ty, &
    nlni_north_of_ty
  use c3_nlni_io, only: &
    nlni_get_f_map_tile
  use c3_joint_io, only: &
    joint_get_f_map_tile => get_f_map_tile, &
    joint_get_dir_rt_nlni2jflw => get_dir_rt_nlni2jflw
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt_from_nlni'
  character(*), intent(in) :: resl
  character(*), intent(in) :: resl_nlni
  logical, intent(in) :: overwrite

  integer :: gxs, gxe, gys, gye
  integer :: itx, ity
  real(8) :: west, east, south, north
  integer :: gxs2, gxe2, gys2, gye2
  integer :: txs2, txe2, tys2, tye2
  integer :: itx2, ity2
  real(8) :: west2, east2, south2, north2

  character(CLEN_PATH) :: dir
  character(CLEN_PATH) :: f_tile, f_conf, f_log
  integer :: un

  character(CLEN_WFMT), parameter :: WFMT_LON = 'es20.15'
  character(CLEN_WFMT), parameter :: WFMT_LAT = 'es20.15'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(resl)
  call nlni_set_resolution(resl_nlni)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ity = 1, NTY
  do itx = 1, NTX

    gys = gys_of_ty(ity)
    gye = gye_of_ty(ity)
    south = south_of_ty(ity)
    north = north_of_ty(ity)

    gxs = gxs_of_tx(itx)
    gxe = gxe_of_tx(itx)
    west = west_of_tx(itx)
    east = east_of_tx(itx)

    ! DEBUG
    !if( itx /= 18 .or. ity /= 11 ) cycle

    call logent('JFlw ('//str((/itx,ity/),DGT_TXY,',')//') '//&
        sBBox(west,east,south,north))

    gxs2 = nlni_gxs_of_lon(west)
    gxe2 = nlni_gxe_of_lon(east)
    gys2 = nlni_gys_of_lat(south)
    gye2 = nlni_gye_of_lat(north)

    txs2 = nlni_tx_of_gx(gxs2)
    txe2 = nlni_tx_of_gx(gxe2)
    tys2 = nlni_ty_of_gy(gys2)
    tye2 = nlni_ty_of_gy(gye2)

    do ity2 = tys2, tye2
    do itx2 = txs2, txe2
      f_tile = nlni_get_f_map_tile('landuse', itx2, ity2)
      if( access(f_tile, ' ') /= 0 ) cycle

      west2 = nlni_west_of_tx(itx2)
      east2 = nlni_east_of_tx(itx2)
      south2 = nlni_south_of_ty(ity2)
      north2 = nlni_north_of_ty(ity2)
      call logmsg('NLNI ('//str((/itx2,ity2/),NLNI_DGT_TXY,',')//') '//&
          sBBox(west2,east2,south2,north2))

      if( is_overlap_negligible(&
            west, east, south, north,&
            west2, east2, south2, north2) )then
        call logmsg('Overlap is negligible.')
        cycle
      endif

      dir = joint_get_dir_rt_nlni2jflw(&
          resl_nlni, resl, itx2, ity2, itx, ity)
      if( .not. overwrite )then
        if( access(joined(dir, 'grid.bin'), ' ') == 0 )then
          call logmsg('Output file already exists.')
          cycle
        endif
      endif

      f_conf = joined(dir, 'conf')
      call traperr( mkdir(dir) )
      open(newunit=un, file=f_conf, status='replace')
      call write_conf()
      close(un)

      f_log = joined(dir, 'log')
      call logmsg('Log: '//str(f_log))
      call execute_command_line(&
          'srun '//PROG_SPRING_REMAP//' '//str(f_conf)//&
          ' >'//str(f_log)//' 2>&1')
    enddo  ! itx2/
    enddo  ! ity2/

    call logext()
  enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine write_conf()
  implicit none

  call w('#')
  call w('path_report: "'//str(joined(dir,'report.txt'))//'"')
  call w('')
  call w('[mesh_latlon]')
  call w('  nx: '//str(NLNI_NX))
  call w('  ny: '//str(NLNI_NY))
  call w('  west: '//str(nlni_west_of_tx(itx2),WFMT_LON))
  call w('  east: '//str(nlni_east_of_tx(itx2),WFMT_LON))
  call w('  south: '//str(nlni_south_of_ty(ity2),WFMT_LAT))
  call w('  north: '//str(nlni_north_of_ty(ity2),WFMT_LAT))
  call w('  is_south_to_north: .true.')
  call w('[end]')
  call w('')
  call w('[mesh_latlon]')
  call w('  nx: '//str(NX))
  call w('  ny: '//str(NY))
  call w('  west: '//str(west,WFMT_LON))
  call w('  east: '//str(east,WFMT_LON))
  call w('  south: '//str(south,WFMT_LAT))
  call w('  north: '//str(north,WFMT_LAT))
  call w('  is_south_to_north: .false.')
  call w('[end]')
  call w('')
  call w('[remapping]')
  call w('  dir: "'//str(dir)//'"')
  call w('  fout_rt_sidx: "grid.bin", int8, rec=1')
  call w('  fout_rt_tidx: "grid.bin", int8, rec=2')
  call w('  fout_rt_area: "area.bin", dble')
  call w('[end]')
  call w('')
  call w('[options]')
  call w('  old_files: remove')
  call w('  earth_geosys: WGS84')
  call w('  earth_rtyp: volmetric')
  call w('[end]')
end subroutine write_conf
!---------------------------------------------------------------
subroutine w(s)
  implicit none
  character(*), intent(in) :: s

  write(un,"(a)") s
end subroutine w
!---------------------------------------------------------------
end subroutine make_rt_from_nlni
!===============================================================
!
!===============================================================
subroutine remap(&
    resl, name_src, resl_src, var, overwrite &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remap'
  character(*), intent(in) :: resl  ! Resolution of output (jflw) map
  character(*), intent(in) :: name_src
  character(*), intent(in) :: resl_src
  character(*), intent(in) :: var
  logical, intent(in) :: overwrite

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( name_src )

  case( 'NLNI' )
    call remap_nlni(resl, resl_src, var, overwrite)

  case( 'J-FlwDir' )
    call remap_jflw(resl, resl_src, var, overwrite)

  case default
    call errend(msg_invalid_value('name_src', name_src))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine remap
!===============================================================
!
!===============================================================
subroutine remap_nlni(&
    resl, resl_nlni, var, overwrite &
)
  use c2_jflw_const, &
    set_resolution => set_resolution
  use c3_nlni_const, &
    nlni_set_resolution => nlni_set_resolution
  use c2_jflw_grid, only: &
    gxs_of_tx, &
    gxe_of_tx, &
    gys_of_ty, &
    gye_of_ty, &
    west_of_tx, &
    east_of_tx, &
    south_of_ty, &
    north_of_ty
  use c2_jflw_io, only: &
    get_f_map_tile, &
    tilename
  use c3_nlni_grid, only: &
    nlni_tx_of_gx, &
    nlni_ty_of_gy, &
    nlni_gxs_of_lon, &
    nlni_gxe_of_lon, &
    nlni_gys_of_lat, &
    nlni_gye_of_lat, &
    nlni_west_of_tx, &
    nlni_east_of_tx, &
    nlni_south_of_ty, &
    nlni_north_of_ty
  use c2_nlni_io, only: &
    nlni_get_f_map_tile => get_f_map_tile
  use c3_joint_io, only: &
    joint_get_f_map_tile => get_f_map_tile, &
    joint_get_dir_rt_nlni2jflw => get_dir_rt_nlni2jflw
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remap_nlni'
  character(*), intent(in) :: resl  ! Resolution of output (jflw) map
  character(*), intent(in) :: resl_nlni
  character(*), intent(in) :: var
  logical, intent(in) :: overwrite

  type(rt_) :: rt
  type(valwgt_), pointer :: valwgtmap(:), vw
  integer(1), allocatable :: srcmapi1(:,:), tgtmapi1(:)
  integer(1) :: srcmissi1, tgtmissi1
  integer :: gxs, gxe, gys, gye
  integer :: itx, ity
  real(8) :: west, east, south, north
  integer :: gxs2, gxe2, gys2, gye2
  integer :: txs2, txe2, tys2, tye2
  integer :: itx2, ity2
  real(8) :: west2, east2, south2, north2
  integer :: k
  integer :: iw
  logical :: is_exist

  character(CLEN_PATH) :: dir
  character(CLEN_PATH) :: f_report, f_grid, f_area
  character(CLEN_PATH) :: f_src, f_tgt
  integer :: un
  character :: c_

  real(8), parameter :: THRESH_FRAC_VALID = 0.5d0

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(resl)
  call nlni_set_resolution(resl_nlni)

  selectcase( var )
  case( 'landuse' )
    srcmissi1 = NLNI_LANDUSE_MISS
    tgtmissi1 = LANDUSE_MISS
  case default
    call errend(msg_invalid_value('var', var))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(valwgtmap(NX*NY))
  do k = 1, NX*NY
    vw => valwgtmap(k)
    vw%n = 0
    allocate(vw%vi1(2))
    allocate(vw%wgt(2))
  enddo

  allocate(srcmapi1(NLNI_NX,NLNI_NY))
  allocate(tgtmapi1(NX*NY))

  do ity = 1, NTY
  do itx = 1, NTX
    ! DEBUG
    !if( itx /= 18 .or. ity /= 11 ) cycle
    !if( tilename(itx,ity) /= 'n34e133' ) cycle
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    gys = gys_of_ty(ity)
    gye = gye_of_ty(ity)
    gxs = gxs_of_tx(itx)
    gxe = gxe_of_tx(itx)

    south = south_of_ty(ity)
    north = north_of_ty(ity)
    west = west_of_tx(itx)
    east = east_of_tx(itx)

    call logent('J-FlwDir ('//str((/itx,ity/),DGT_TXY,',')//') '//&
        sBBox(west, east, south, north))

    f_tgt = get_f_map_tile(resl, var, itx, ity)
    if( .not. overwrite )then
      if( access(f_tgt, ' ') == 0 )then
        call logmsg('Output file already exists.')
        call logext()
        cycle
      endif
    endif

    gxs2 = nlni_gxs_of_lon(west)
    gxe2 = nlni_gxe_of_lon(east)
    gys2 = nlni_gys_of_lat(south)
    gye2 = nlni_gye_of_lat(north)

    txs2 = nlni_tx_of_gx(gxs2)
    txe2 = nlni_tx_of_gx(gxe2)
    tys2 = nlni_ty_of_gy(gys2)
    tye2 = nlni_ty_of_gy(gye2)

    !$omp parallel do
    do k = 1, NX*NY
      valwgtmap(k)%n = 0
      valwgtmap(k)%i = 0
      valwgtmap(k)%wgt_sum = 0.d0
    enddo
    !$omp end parallel do
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call logent('Calculating interpolation weights')

    is_exist = .false.

    do ity2 = tys2, tye2
    do itx2 = txs2, txe2

      f_src = nlni_get_f_map_tile(var, itx2, ity2)
      if( access(f_src,' ') /= 0 ) cycle

      is_exist = .true.

      west2 = nlni_west_of_tx(itx2)
      east2 = nlni_east_of_tx(itx2)
      south2 = nlni_south_of_ty(ity2)
      north2 = nlni_north_of_ty(ity2)
      call logmsg('NLNI ('//str((/itx2,ity2/),NLNI_DGT_TXY,',')//') '//&
          sBBox(west2,east2,south2,north2))

      if( is_overlap_negligible(&
            west, east, south, north,&
            west2, east2, south2, north2) )then
        call logmsg('Overlap is negligible.')
        cycle
      endif

      call traperr( rbin(srcmapi1, f_src) )

      dir = joint_get_dir_rt_nlni2jflw(&
          resl_nlni, resl, itx2, ity2, itx, ity)
      f_report = joined(dir, 'report.txt')
      f_grid = joined(dir, 'grid.bin')
      f_area = joined(dir, 'area.bin')

      open(newunit=un, file=f_report, status='old')
      read(un,*)
      read(un,*) c_, rt%nij

      allocate(rt%sidx(rt%nij))
      allocate(rt%tidx(rt%nij))
      allocate(rt%area(rt%nij))

      call traperr( rbin(rt%sidx, f_grid, rec=1, sz=rt%nij) )
      call traperr( rbin(rt%tidx, f_grid, rec=2, sz=rt%nij) )
      call traperr( rbin(rt%area, f_area) )
      call logmsg('  sidx min: '//str(minval(rt%sidx))//' max: '//str(maxval(rt%sidx)))
      call logmsg('  tidx min: '//str(minval(rt%tidx))//' max: '//str(maxval(rt%tidx)))

      call update_valwgt(valwgtmap, srcmapi1, srcmissi1, rt)
    enddo  ! itx2/
    enddo  ! ity2/

    call logext()
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( .not. is_exist )then
      call logext()
      cycle
    endif
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call logent('Calculating grid values')

    do k = 1, NX*NY
      vw => valwgtmap(k)
      if( vw%n == 0 )then
        tgtmapi1(k) = tgtmissi1
        cycle
      elseif( sum(vw%wgt(:vw%n)) / vw%wgt_sum < THRESH_FRAC_VALID )then
        tgtmapi1(k) = tgtmissi1
        cycle
      endif

      vw%i = 1
      do iw = 2, vw%n
        if( vw%wgt(iw) > vw%wgt(vw%i) )then
          vw%i = iw
        elseif( vw%wgt(iw) < vw%wgt(vw%i) )then
          cycle
        else
          ! smaller index is prioritized when they have same weight
          if( vw%vi1(iw) < vw%vi1(vw%i) )then  
            vw%i = iw
          endif
        endif
      enddo  ! iw/
      tgtmapi1(k) = vw%vi1(vw%i)
    enddo  ! k/

    call logext()
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call logmsg('Writing '//str(f_tgt))
    call traperr( wbin(tgtmapi1, f_tgt, replace=.true.) )
    !-----------------------------------------------------------
    call logext()
  enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(srcmapi1)
  deallocate(tgtmapi1)
  deallocate(valwgtmap)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine remap_nlni
!===============================================================
!
!===============================================================
subroutine remap_jflw(resl, resl_src, var, overwrite)
  use c2_jflw_const
  use c2_jflw_grid, only: &
    gxs_of_tx, &
    gys_of_ty
  use c2_jflw_io, only: &
    tilename, &
    read_map_from_tile, &
    get_f_map_tile, &
    get_dir_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remap_jflw'
  character(*), intent(in) :: resl
  character(*), intent(in) :: resl_src
  character(*), intent(in) :: var
  logical, intent(in) :: overwrite

  character(CLEN_KEY) :: dtype
  real(4) :: miss
  integer(8), allocatable :: sidx(:), tidx(:)
  real(8), allocatable :: area(:)
  real(4), allocatable :: srcmap(:,:), src1d(:)
  real(8), allocatable :: tgt1d(:)
  real(8), allocatable :: areasum(:)
  integer :: nij, ij
  integer :: nx1, ny1, nx2, ny2
  integer :: itx, ity

  character(CLEN_PATH) :: dir_rt
  character(CLEN_PATH) :: f_report
  character(CLEN_PATH) :: f_out
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(resl_src)
  nx1 = NX
  ny1 = NY

  call set_resolution(resl)
  nx2 = NX
  ny2 = NY

  allocate(srcmap(nx1,ny1))
  allocate(src1d(nx1*ny1))
  allocate(tgt1d(nx2*ny2))
  allocate(areasum(nx2*ny2))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( var )
  case( 'elv' )
    dtype = DTYPE_REAL
    miss = ELV_MISS
  case default
    call errend(msg_invalid_value('var', var))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(resl_src)

  do ity = 1, NTY
  do itx = 1, NTX

!if( tilename(itx,ity) /= 'n35e140' ) cycle

    f_out = get_f_map_tile(resl, var, itx, ity)
    if( .not. overwrite .and. access(f_out,' ') == 0 )then
      call logmsg('File already exists: '//str(f_out))
      cycle
    endif

    dir_rt = joined(get_dir_rt(resl_src, resl), tilename(itx,ity))
    f_report = joined(dir_rt, 'report.txt')
    open(newunit=un, file=f_report, status='old')
    read(un,*)
    read(un,*) c_, nij
    close(un)

    allocate(sidx(nij))
    allocate(tidx(nij))
    allocate(area(nij))
    call traperr( rbin(sidx, joined(dir_rt,'grid.bin'), rec=1) )
    call traperr( rbin(tidx, joined(dir_rt,'grid.bin'), rec=2) )
    call traperr( rbin(area, joined(dir_rt,'area.bin')) )

    call read_map_from_tile(&
        resl_src, var, dtype, miss, &
        gxs_of_tx(itx), gys_of_ty(ity), srcmap)
    src1d = reshape(srcmap, (/size(srcmap)/))

    tgt1d(:) = 0.d0
    areasum(:) = 0.d0
    do ij = 1_8, nij
      if( src1d(sidx(ij)) == miss ) cycle
      call add(tgt1d(tidx(ij)), src1d(sidx(ij)) * area(ij))
      call add(areasum(tidx(ij)), area(ij))
    enddo  ! ij/

    where( areasum > 0.d0 )
      tgt1d = tgt1d / areasum
    elsewhere
      tgt1d = miss
    endwhere

    call logmsg('Writing '//str(f_out))
    call traperr( wbin(tgt1d, f_out, dtype=dtype, replace=.true.) )

    deallocate(sidx)
    deallocate(tidx)
    deallocate(area)
  enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(srcmap)
  deallocate(src1d)
  deallocate(tgt1d)
  deallocate(areasum)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine remap_jflw
!===============================================================
!
!===============================================================
subroutine update_valwgt(valwgtmap, srcmap, smiss, rt)
  implicit none
  type(valwgt_), intent(inout), target :: valwgtmap(:)
  integer(1), intent(in) :: srcmap(:,:)
  integer(1), intent(in) :: smiss
  type(rt_), intent(in) :: rt

  type(valwgt_), pointer :: vw
  integer(4) :: s
  integer(8) :: ij
  integer :: i
  integer :: nsx, nsy, isx, isy

  nsx = size(srcmap,1)
  nsy = size(srcmap,2)

  !$omp parallel do private(isx, isy, vw, s, i)
  do ij = 1_8, rt%nij
    isy = (rt%sidx(ij)-1) / nsx + 1
    isx = rt%sidx(ij) - (isy-1)*nsx

    vw => valwgtmap(rt%tidx(ij))
    call add(vw%wgt_sum, rt%area(ij))

    s = srcmap(isx,isy)
    if( s == smiss ) cycle

    if( vw%i == 0 )then
      vw%i = 1
      vw%n = 1
      vw%vi1(1) = s
      vw%wgt(1) = rt%area(ij)
    else
      if( vw%vi1(vw%i) /= s )then
        do i = 1, vw%n
          if( vw%vi1(i) == s ) exit
        enddo
        if( i == vw%n+1 )then
          if( vw%n == size(vw%vi1) )then
            call realloc(vw%vi1, vw%n*2, clear=.false.)
            call realloc(vw%wgt, vw%n*2, clear=.false.)
          endif
          vw%vi1(i) = s
          vw%wgt(i) = 0.d0
          vw%n = i
        endif
        vw%i = i
      endif
      call add(vw%wgt(vw%i), rt%area(ij))
    endif
  enddo
  !$omp end parallel do
end subroutine update_valwgt
!===============================================================
!
!===============================================================
logical function is_overlap_negligible(&
    west, east, south, north, &
    west2, east2, south2, north2) result(res)
  use c2_jflw_const
  implicit none
  real(8), intent(in) :: west, east, south, north
  real(8), intent(in) :: west2, east2, south2, north2

  res = east2 <= west + GRIDSIZE_LON*1d-9 .or. &
        west2 >= east - GRIDSIZE_LON*1d-9 .or. &
        north2 <= south + GRIDSIZE_LAT*1d-9 .or. &
        south2 >= north - GRIDSIZE_LAT*1d-9
end function is_overlap_negligible
!===============================================================
!
!===============================================================
end module mod_remap
