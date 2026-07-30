module mod_calc_watsys_corresp
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calcWatsysAreas
  public :: makeWsCodeRemappingTables
  public :: mergeWsCodeRemappingTables
  public :: calcWatsysCorresp
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_calc_watsys_corresp'
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface 
    integer function access(path,mode)
      character(*), intent(in) :: path
      character(*), intent(in) :: mode
    end function
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calcWatsysAreas()
  use c2_nlni_const, &
        set_resolution => set_resolution
  use c2_nlni_grid, only: &
        west_of_tx , &
        east_of_tx , &
        south_of_ty, &
        north_of_ty
  use c2_nlni_io, only: &
        tilename           , &
        get_f_map_tile     , &
        get_dir_bsnara_tile
  implicit none
  integer :: itx, ity
  integer :: nij, nij_this, ij, ijs, ije
  integer, allocatable :: tmpgrdidx(:)
  real(8), allocatable :: tmpgrdara(:)
  integer, allocatable :: grdidx(:)
  real(8), allocatable :: grdara(:)
  integer, allocatable :: arg(:)
  character(CLEN_PATH) :: dir_out_this
  character(CLEN_PATH) :: f_conf, f_log
  character(CLEN_PATH) :: f_wsCode
  character(CLEN_PATH) :: f_grdidx, f_grdara
  character(CLEN_PATH) :: f_lst_bsnara_all
  integer :: un
  integer :: dgt_idx

  character(CLEN_PROC), parameter :: PRCNAM = 'calcWatsysAreas'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_100M)
  !-------------------------------------------------------------
  ! 
  !-------------------------------------------------------------
!if( .false. )then
  call logent('Calculating watsys area of each tile')

  do ity = TYMIN, TYMAX
  do itx = TXMIN, TXMAX
    f_wsCode = get_f_map_tile('wsCode', itx, ity)
    if( access(f_wsCode,' ') /= 0 ) cycle
    call logmsg(tilename(itx,ity))

    dir_out_this = get_dir_bsnara_tile(itx, ity)

    f_conf = joined(dir_out_this,'conf')
    f_log = joined(dir_out_this,'log')
    call write_conf()

    call execute_command_line(&
           trim(PROG_SPRING_MAKE_GRID_DATA)//&
           ' '//trim(f_conf)//' > '//trim(f_log))
  enddo  ! itx/
  enddo  ! ity/

  call logext()
!endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Merging tiled data')

  nij = 0
  do ity = TYMIN, TYMAX
  do itx = TXMIN, TXMAX
    if( access(get_f_map_tile('wsCode', itx, ity),' ') /= 0 ) cycle

    dir_out_this = get_dir_bsnara_tile(itx, ity)
    f_grdara = joined(dir_out_this, 'area.bin')
    if( mod(filesize(f_grdara),8) /= 0 )then
      call errend('File size is invalid.'//&
                '\n  path: '//str(f_grdara)//&
                '\n  size: '//str(filesize(f_grdara)))
    endif
    nij_this = int(filesize(f_grdara) / 8,4)
    call add(nij,nij_this)
  enddo  ! itx/
  enddo  ! ity/

  call logmsg('nij: '//str(nij))
  allocate(tmpgrdidx(nij), tmpgrdara(nij))

  nij = 0
  do ity = TYMIN, TYMAX
  do itx = TXMIN, TXMAX
    if( access(get_f_map_tile('wsCode', itx,ity),' ') /= 0 ) cycle

    dir_out_this = get_dir_bsnara_tile(itx, ity)
    f_grdara = joined(dir_out_this, 'area.bin')
    f_grdidx = joined(dir_out_this, 'index.bin')
    nij_this = int(filesize(f_grdara) / 8,4)
    if( nij_this == 0 ) cycle

    call traperr( rbin(tmpgrdara(nij+1:nij+nij_this), f_grdara) )
    call traperr( rbin(tmpgrdidx(nij+1:nij+nij_this), f_grdidx) )
    call add(nij,nij_this)
  enddo  ! itx/
  enddo  ! ity/

  allocate(arg(nij))
  call argsort(tmpgrdidx, arg)
  call sort(tmpgrdidx, arg)
  call sort(tmpgrdara, arg)
  deallocate(arg)

  allocate(grdidx(nij), grdara(nij))

  ij = 0
  ije = 0
  do while( ije < nij )
    ijs = ije + 1
    ije = ijs
    do while( ije < nij )
      if( tmpgrdidx(ije+1) /= tmpgrdidx(ijs) ) exit
      call add(ije)
    enddo
    call add(ij)
    grdidx(ij) = tmpgrdidx(ijs)
    grdara(ij) = sum(tmpgrdara(ijs:ije))
  enddo

  deallocate(tmpgrdidx, tmpgrdara)

  dgt_idx = dgt(grdidx(:nij), DGT_OPT_MAX)

  f_lst_bsnara_all = joined(DIR_PRD, 'bsnara/all.txt')
  call logmsg('Writing '//str(f_lst_bsnara_all))
  open(newunit=un, file=f_lst_bsnara_all, status='replace')
  write(un,"(1x,a,1x,i0)") 'nBsn', nij
  write(un,"(1x,a)") 'id area(m2)'
  do ij = 1, nij
    write(un,"(1x,a)") &
      str(grdidx(ij),dgt_idx)//' '//str(grdara(ij),'es12.5')
  enddo
  close(un)

  deallocate(grdidx, grdara)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine write_conf()
  implicit none

  open(newunit=un, file=f_conf, status='replace')
  call wl('')
  call wl('#')
  call wl('path_report: "'//str(dir_out_this)//'/report.txt"')
  call wl('')
  call wl('[mesh_raster]')
  call wl('  nx: '//str(NX))
  call wl('  ny: '//str(NY))
  call wl('  west: '//str(west_of_tx(itx),'f12.7'))
  call wl('  east: '//str(east_of_tx(itx),'f12.7'))
  call wl('  south: '//str(south_of_ty(ity),'f11.7'))
  call wl('  north: '//str(north_of_ty(ity),'f11.7'))
  call wl('  is_south_to_north: false')
  call wl('  fin_rstidx: "'//str(f_wsCode)//'"')
  call wl('  idx_miss: '//str(WSCODE_MISS_I))
  call wl('')
  call wl('  out_form: auto')
  call wl('  fout_grdidx: "'//str(dir_out_this)//'/index.bin"')
  call wl('  fout_grdara: "'//str(dir_out_this)//'/area.bin"')
  call wl('[end]')
  call wl('')
  call wl('[options]')
  call wl('  old_files: remove')
  call wl('  earth_r: '//str(EARTH_R))
  call wl('[end]')
  close(un)
end subroutine write_conf
!---------------------------------------------------------------
subroutine wl(s)
  implicit none
  character(*), intent(in) :: s

  write(un,"(a)") s
end subroutine wl
!---------------------------------------------------------------
end subroutine calcWatsysAreas
!===============================================================
!
!===============================================================
subroutine makeWsCodeRemappingTables()
  use c3_nlni_const
  use c2_nlni_const, only: &
        nlni_set_resolution => set_resolution
  use c2_nlni_grid, only: &
        nlni_west_of_tx  => west_of_tx , &
        nlni_east_of_tx  => east_of_tx , &
        nlni_south_of_ty => south_of_ty, &
        nlni_north_of_ty => north_of_ty, &
        nlni_txs_of_lon  => txs_of_lon , &
        nlni_txe_of_lon  => txe_of_lon , &
        nlni_tys_of_lat  => tys_of_lat , &
        nlni_tye_of_lat  => tye_of_lat 
  use c2_nlni_io, only: &
        nlni_tilename       => tilename      , &
        nlni_get_f_map_tile => get_f_map_tile
  use c2_jflw_const, &
        jflw_set_resolution => set_resolution
  use c2_jflw_grid, only: &
        west_of_tx , &
        east_of_tx , &
        south_of_ty, &
        north_of_ty 
  use c2_jflw_io, only: &
        tilename      , &
        get_f_map_tile
  use c3_joint_io, only: &
        get_dir_rt_nlni2jflw_wsCode
  implicit none
  integer :: jtx, jty
  integer :: itx, ity
  real(8) :: west, east, south, north
  integer :: txs_nlni, txe_nlni, tys_nlni, tye_nlni
  character(CLEN_PATH) :: f_jflw, f_nlni
  character(CLEN_PATH) :: f_conf, f_log
  character(CLEN_PATH) :: dir_rt
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'makeWsCodeRemappingTables'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call nlni_set_resolution(RESOLUTION_100M)
  call jflw_set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
!if( .false. )then

  do itx = 1, NTX
    west = west_of_tx(itx)
    east = east_of_tx(itx)
    txs_nlni = nlni_txs_of_lon(west)
    txe_nlni = nlni_txe_of_lon(east)
    !do while( nlni_east_of_tx(txs_nlni) <= west )
    !  txs_nlni = txs_nlni + 1
    !enddo
    !do while( nlni_west_of_tx(txe_nlni) >= east )
    !  txe_nlni = txe_nlni - 1
    !enddo
    if( txs_nlni > NLNI_TXMAX .or. txe_nlni < NLNI_TXMIN ) cycle
    call logmsg('x='//str(itx,dgt(NTX))//&
              '\n  J-FlwDir: '//str((/west,east/),'f12.7',' - ')//&
              '\n  NLNI    : '//str(&
                (/nlni_west_of_tx(txs_nlni),nlni_east_of_tx(txe_nlni)/),'f12.7',' - ')//&
                ' ('//str((/txs_nlni,txe_nlni/),dgt(NLNI_TXMAX),' - ')//')')
  enddo

  do ity = 1, NTY
    north = north_of_ty(ity)
    south = south_of_ty(ity)
    tys_nlni = nlni_tys_of_lat(south)
    tye_nlni = nlni_tye_of_lat(north)
    !do while( nlni_north_of_ty(tys_nlni) <= south )
    !  tys_nlni = tys_nlni + 1
    !enddo
    !do while( nlni_south_of_ty(tye_nlni) >= north )
    !  tye_nlni = tye_nlni - 1
    !enddo
    if( tys_nlni > NLNI_TYMAX .or. tye_nlni < NLNI_TYMIN ) cycle
    call logmsg('y='//str(ity,dgt(NTY))//&
              '\n  J-FlwDir: '//str((/south,north/),'f12.7',' - ')//&
              '\n  NLNI    : '//str(&
                (/nlni_south_of_ty(tys_nlni),nlni_north_of_ty(tye_nlni)/),'f12.7',' - ')//&
                ' ('//str((/tys_nlni,tye_nlni/),dgt(NLNI_TYMAX),' - ')//')')
  enddo

!endif
  !-------------------------------------------------------------
  ! Make remapping tables
  !-------------------------------------------------------------
!if( .false. )then
  call logent('Making remapping tables')

  do ity = 1, NTY
  do itx = 1, NTX
    f_jflw = get_f_map_tile(RESOLUTION_1SEC, 'bsn', itx, ity)
    if( access(f_jflw,' ') /= 0 ) cycle

    west = west_of_tx(itx)
    east = east_of_tx(itx)
    north = north_of_ty(ity)
    south = south_of_ty(ity)

    txs_nlni = nlni_txs_of_lon(west)
    txe_nlni = nlni_txe_of_lon(east)
    tys_nlni = nlni_tys_of_lat(south)
    tye_nlni = nlni_tye_of_lat(north)

    do jty = tys_nlni, tye_nlni
    do jtx = txs_nlni, txe_nlni
      f_nlni = nlni_get_f_map_tile('wsCode', jtx, jty)
      if( access(f_nlni,' ') /= 0 ) cycle

      dir_rt = get_dir_rt_nlni2jflw_wsCode(itx, ity, jtx, jty)
      f_conf = joined(dir_rt, 'conf')
      f_log = joined(dir_rt, 'log')
      call traperr( mkdir(dir_rt) )
      call write_conf_remap()

      call logmsg(trim(dir_rt))
      call execute_command_line(&
             trim(PROG_SPRING_REMAP)//&
             ' '//trim(f_conf)//' > '//trim(f_log))
    enddo  ! jty/
    enddo  ! jtx/
  enddo  ! itx/
  enddo  ! ity/

  call logext()
!endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine write_conf_remap()
  implicit none

  open(newunit=un, file=f_conf, status='replace')
  call wl('#')
  call wl('path_report: "'//str(dir_rt)//'/report.txt"')
  call wl('')
  call wl('[mesh_raster]')
  call wl('  nx: '//str(NLNI_NX))
  call wl('  ny: '//str(NLNI_NY))
  call wl('  west: '//str(nlni_west_of_tx(jtx),'f12.7'))
  call wl('  east: '//str(nlni_east_of_tx(jtx),'f12.7'))
  call wl('  south: '//str(nlni_south_of_ty(jty),'f12.7'))
  call wl('  north: '//str(nlni_north_of_ty(jty),'f12.7'))
  call wl('  fin_rstidx: "'//str(f_nlni)//'"')
  call wl('  idx_miss: '//str(NLNI_WSCODE_MISS_I))
  call wl('  is_south_to_north: true')
  call wl('[end]')
  call wl('')
  call wl('[mesh_raster]')
  call wl('  nx: '//str(NX))
  call wl('  ny: '//str(NY))
  call wl('  west: '//str(west,'f12.7'))
  call wl('  east: '//str(east,'f12.7'))
  call wl('  south: '//str(south,'f12.7'))
  call wl('  north: '//str(north,'f12.7'))
  call wl('  fin_rstidx: "'//str(f_jflw)//'"')
  call wl('  idx_miss: '//str(BSN_MISS))
  call wl('  is_south_to_north: false')
  call wl('[end]')
  call wl('')
  call wl('[remapping]')
  call wl('  dir: "'//str(dir_rt)//'"')
  call wl('  fout_rt_sidx: "grid.bin", int4, 1')
  call wl('  fout_rt_tidx: "grid.bin", int4, 2')
  call wl('  fout_rt_area: "area.bin", dble')
  call wl('  fout_rt_coef: "coef.bin", dble')
  call wl('')
  call wl('  allow_empty: .true.')
  call wl('')
  call wl('  mesh_vrf: source')
  call wl('  fout_vrf_grdidx     : "vrf/src_idx.bin", int4')
  call wl('  fout_vrf_grdara_true: "vrf/src_val.bin", dble, 1')
  call wl('  fout_vrf_grdara_rt  : "vrf/src_val.bin", dble, 2')
  call wl('  fout_vrf_rerr_grdara: "vrf/src_val.bin", dble, 3')
  call wl('')
  call wl('  mesh_vrf: target')
  call wl('  fout_vrf_grdidx     : "vrf/tgt_idx.bin", int4')
  call wl('  fout_vrf_grdara_true: "vrf/tgt_val.bin", dble, 1')
  call wl('  fout_vrf_grdara_rt  : "vrf/tgt_val.bin", dble, 2')
  call wl('  fout_vrf_rerr_grdara: "vrf/tgt_val.bin", dble, 3')
  call wl('[end]')
  call wl('')
  call wl('[options]')
  call wl('  old_files: remove')
  call wl('  earth_r: '//str(EARTH_R))
  call wl('[end]')
  close(un)
end subroutine write_conf_remap
!---------------------------------------------------------------
subroutine wl(s)
  implicit none
  character(*), intent(in) :: s

  write(un,"(a)") s
end subroutine wl
!---------------------------------------------------------------
end subroutine makeWsCodeRemappingTables
!===============================================================
!
!===============================================================
subroutine mergeWsCodeRemappingTables()
  use c3_nlni_const
  use c2_nlni_grid, only: &
        nlni_west_of_tx  => west_of_tx , &
        nlni_east_of_tx  => east_of_tx , &
        nlni_south_of_ty => south_of_ty, &
        nlni_north_of_ty => north_of_ty, &
        nlni_txs_of_lon  => txs_of_lon , &
        nlni_txe_of_lon  => txe_of_lon , &
        nlni_tys_of_lat  => tys_of_lat , &
        nlni_tye_of_lat  => tye_of_lat 
  use c2_nlni_io, only: &
        nlni_tilename       => tilename      , &
        nlni_get_f_map_tile => get_f_map_tile
  use c2_jflw_const
  use c2_jflw_grid, only: &
        west_of_tx , &
        east_of_tx , &
        south_of_ty, &
        north_of_ty
  use c2_jflw_io, only: &
        tilename      , &
        get_f_map_tile             
  use c3_joint_io, only: &
        get_dir_rt_nlni2jflw_wsCode       , &
        get_dir_rt_nlni2jflw_wsCode_merged
  implicit none
  integer :: jtx, jty
  integer :: itx, ity
  real(8) :: west, east, south, north
  integer :: txs_nlni, txe_nlni, tys_nlni, tye_nlni
  integer :: size_rt
  integer :: nij_rt
  character(:), allocatable :: f_jflw, f_nlni
  character(:), allocatable :: f_conf, f_log
  character(:), allocatable :: dir_rt, dir_rt_merged
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'mergeWsCodeRemappingTables'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dir_rt_merged = get_dir_rt_nlni2jflw_wsCode_merged()
  call traperr( mkdir(dir_rt_merged) )
  f_conf = joined(dir_rt_merged,'conf')
  f_log = joined(dir_rt_merged,'log')
  call write_conf_merge_head()

  do ity = 1, NTY
  do itx = 1, NTX
    f_jflw = joined(DIR_TILED, 'bsn/'//tilename(itx, ity)//'.bin')
    if( access(f_jflw,' ') /= 0 ) cycle

    west = west_of_tx(itx)
    east = east_of_tx(itx)
    north = north_of_ty(ity)
    south = south_of_ty(ity)

    txs_nlni = nlni_txs_of_lon(west)
    txe_nlni = nlni_txe_of_lon(east)
    tys_nlni = nlni_tys_of_lat(south)
    tye_nlni = nlni_tye_of_lat(north)

    do jty = tys_nlni, tye_nlni
    do jtx = txs_nlni, txe_nlni
      f_nlni = nlni_get_f_map_tile('wsCode', jtx, jty)
      if( access(f_nlni,' ') /= 0 ) cycle

      dir_rt = get_dir_rt_nlni2jflw_wsCode(itx, ity, jtx, jty)
      size_rt = int(filesize(str(dir_rt)//'/grid.bin'),4)
      if( mod(size_rt,8) /= 0 )then
        call errend('Invalid file size.'//&
                   '\n  path: '//str(str(dir_rt)//'/grid.bin')//&
                   '\n  size: '//str(size_rt))
      elseif( size_rt /= filesize(str(dir_rt)//'/area.bin') .or. &
              size_rt /= filesize(str(dir_rt)//'/coef.bin') )then
        call errend('Sizes of the files mismatch.'//&
                   '\n  directory: '//str(dir_rt)//&
                   '\n  grid.bin: '//str(size_rt)//&
                   '\n  area.bin: '//str(filesize(str(dir_rt)//'/area.bin'))//&
                   '\n  coef.bin: '//str(filesize(str(dir_rt)//'/coef.bin')))
      endif
      if( size_rt == 0 ) cycle

      call logmsg(trim(dir_rt))

      nij_rt = size_rt / 8
      call write_conf_merge_input_rt()
    enddo  ! jty/
    enddo  ! jtx/
  enddo  ! itx/
  enddo  ! ity/

  call write_conf_merge_tail()

  call logmsg('Executing SPRING program.')
  call logmsg('  Conf: '//str(f_conf))
  call logmsg('  Log : '//str(f_log))
  call execute_command_line(&
         trim(PROG_SPRING_MERGE)//&
         ' '//trim(f_conf)//' > '//trim(f_log))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine write_conf_merge_head()
  implicit none

  open(newunit=un, file=f_conf, status='replace')
  call wl('#')
  call wl('path_report: "'//str(dir_rt_merged)//'/report.txt"')
  call wl('')
  call wl('[input]')
end subroutine write_conf_merge_head
!---------------------------------------------------------------
subroutine write_conf_merge_input_rt()
  implicit none

  call wl('')
  call wl('  length_rt: '//str(nij_rt))
  call wl('  dir: "'//str(dir_rt)//'"')
  call wl('  f_rt_sidx: "grid.bin", rec=1')
  call wl('  f_rt_tidx: "grid.bin", rec=2')
  call wl('  f_rt_area: "area.bin"')
  call wl('  f_rt_coef: "coef.bin"')
end subroutine write_conf_merge_input_rt
!---------------------------------------------------------------
subroutine write_conf_merge_tail()
  implicit none

  call wl('  opt_idx_duplication: sum')
  call wl('[end]')
  call wl('')
  call wl('[output]')
  call wl('  mesh_sort: target')
  call wl('')
  call wl('  dir: "'//str(dir_rt_merged)//'"')
  call wl('  f_rt_sidx: "grid.bin", int4, 1')
  call wl('  f_rt_tidx: "grid.bin", int4, 2')
  call wl('  f_rt_area: "area.bin", dble')
  call wl('  f_rt_coef: "coef.bin", dble')  ! dummy
  call wl('')
  call wl('  opt_coef_sum_modify     : 1.d0')
  call wl('  opt_coef_error_excess    : 1.d-10')
  call wl('  opt_coef_sum_error_excess: 1.d-10')
  call wl('[end]')
  call wl('')
  call wl('[options]')
  call wl('  old_files: remove')
  call wl('[end]')
  close(un)
end subroutine write_conf_merge_tail
!---------------------------------------------------------------
subroutine wl(s)
  implicit none
  character(*), intent(in) :: s

  write(un,"(a)") s
end subroutine wl
!---------------------------------------------------------------
end subroutine mergeWsCodeRemappingTables
!===============================================================
!
!===============================================================
subroutine calcWatsysCorresp()
  use c3_nlni_const
  use c2_jflw_const, &
        set_resolution => set_resolution
  use c2_jflw_io, only: &
        tilename      , &
        get_f_map_tile, &
        get_f_lst_tile, &
        get_f_lst_all           
  use c3_joint_const
  use c3_joint_io, only: &
        get_dir_rt_nlni2jflw_wsCode_merged
  implicit none
  integer :: size_rt
  integer :: nij_rt, ij, ijs, ije
  integer :: nbsn, nbsn_nlni, ibsn, ibsn_nlni
  integer :: nbsn_max
  integer :: nbsn_ovrlap, ibsn_ovrlap
  integer, allocatable :: rt_sidx(:), rt_tidx(:)
  real(8), allocatable :: rt_area(:)
  integer, allocatable :: bsnid(:), bsnid_nlni(:)
  real(8), allocatable :: bsnara(:), bsnara_nlni(:)
  real(8), pointer :: Jaccard(:)
  real(8)          :: Jaccard_max
  integer, pointer :: arg(:)
  type lst_Jaccard_
    integer :: n
    integer :: bsnid
    real(8) :: bsnara
    real(8), pointer :: Jaccard(:)
    integer, pointer :: bsnid_nlni(:)
    real(8), pointer :: bsnara_nlni(:)
    real(8), pointer :: area_ovrlap(:)
    integer :: argmax
    real(8) :: area_ovrlap_tot
  end type
  type(lst_Jaccard_), pointer :: lst_Jaccard(:), Jcd
  integer :: ibsn_Jcd, iibsn_Jcd
  real(8), parameter :: BSNARA_THRESH = 1.d6  ! [m2]
  real(8), parameter :: JACCARD_THRESH = 0.2d0
  integer :: itx, ity
  integer :: ix, iy
  integer :: icx, icy, cx0, cy0
  integer, allocatable :: bsnmap(:,:)
  real(8), allocatable :: Jcdmap(:,:), Jcdmap_lres(:,:)
  integer :: bsnid_prev
  integer :: n_valid
  integer, parameter :: RES_RATIO = 36
  character(CLEN_PATH) :: dir_rt_merged
  character(CLEN_PATH) :: path_rt_grid, path_rt_area
  character(CLEN_PATH) :: f_lst_bsnara_jflw, f_lst_bsnara_nlni
  character(CLEN_PATH) :: f_lst_jaccard
  character(CLEN_PATH) :: f_bsnmap_jflw, f_Jcdmap, f_Jcdmap_lres
  integer :: un
  integer :: dgt_bsnid, dgt_bsnid_nlni
  character(1) :: c_
  integer :: i_

  character(CLEN_PROC), parameter :: PRCNAM = 'calcWatsysCorresp'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_resolution(RESOLUTION_1SEC)
  !-------------------------------------------------------------
  ! Read remapping table
  !-------------------------------------------------------------
  call logent('Reading remapping table')

  dir_rt_merged = get_dir_rt_nlni2jflw_wsCode_merged()
  path_rt_grid = joined(dir_rt_merged, 'grid.bin')
  path_rt_area = joined(dir_rt_merged, 'area.bin')
  size_rt = int(filesize(path_rt_grid),4)
  nij_rt = size_rt / 8
  allocate(rt_sidx(nij_rt), rt_tidx(nij_rt), rt_area(nij_rt))
  call traperr( rbin(rt_sidx, path_rt_grid, rec=1) )
  call traperr( rbin(rt_tidx, path_rt_grid, rec=2) )
  call traperr( rbin(rt_area, path_rt_area) )

  call logmsg('rt length: '//str(nij_rt))
  call logmsg('  sidx: '//str(rt_sidx(:3),', ')//&
              ', ..., '//str(rt_sidx(nij_rt-2:),', '))
  call logmsg('  tidx: '//str(rt_tidx(:3),', ')//&
              ', ..., '//str(rt_tidx(nij_rt-2:),', '))
  call logmsg('  area: '//str(rt_area(:3),'es9.2',', ')//&
              ', ..., '//str(rt_area(nij_rt-2:),'es9.2',', '))

  call logext()
  !-------------------------------------------------------------
  ! Read basin data
  !-------------------------------------------------------------
  call logent('Reading basin data')

  f_lst_bsnara_jflw = joined(DIR_TILED, 'id_area.txt')
  call logmsg('Reading '//str(f_lst_bsnara_jflw))
  open(newunit=un, file=f_lst_bsnara_jflw, status='old')
  read(un,*) c_, nbsn
  allocate(bsnid(nbsn), bsnara(nbsn))
  read(un,*)
  do ibsn = 1, nbsn
    read(un,*) i_, bsnid(ibsn), bsnara(ibsn)
  enddo
  close(un)

  f_lst_bsnara_nlni = joined(NLNI_DIR_PRD, 'bsnara/all.txt')
  open(newunit=un, file=f_lst_bsnara_nlni, status='old')
  read(un,*) c_, nbsn_nlni
  allocate(bsnid_nlni(nbsn_nlni), bsnara_nlni(nbsn_nlni))
  read(un,*)
  do ibsn = 1, nbsn_nlni
    read(un,*) bsnid_nlni(ibsn), bsnara_nlni(ibsn)
  enddo
  close(un)

  dgt_bsnid = dgt(bsnid(nbsn))
  dgt_bsnid_nlni = dgt(bsnid_nlni(:), DGT_OPT_MAX)
  allocate(arg(nbsn_nlni))
  call argsort(bsnid_nlni, arg)
  call sort(bsnid_nlni, arg)
  call sort(bsnara_nlni, arg)
  deallocate(arg)

  do ibsn = 1, nbsn
    if( bsnara(ibsn) < BSNARA_THRESH ) exit
  enddo
  nbsn_max = ibsn-1
  call logmsg('Num. of basins whose area is above the threshold: '//str(nbsn_max))
  allocate(lst_Jaccard(nbsn_max))

  call logext()
  !-------------------------------------------------------------
  ! Calc. Jaccard index
  !-------------------------------------------------------------
  call logent('Computing Jaccard indices of basins')

  f_lst_jaccard = get_f_lst_all(RESOLUTION_1SEC, 'Jaccard')
  call logmsg('Writing '//str(f_lst_jaccard))
  open(newunit=un, file=f_lst_jaccard, status='replace')
  write(un,"(1x,a,1x,i0,1x,a,1x,es9.2,1x,a,1x,f6.3)") &
        'nbsn', nbsn_max, 'bsnara_thresh(m2)', BSNARA_THRESH, &
        'Jaccard_thresh', JACCARD_THRESH
  write(un,"(a)") &
        'i_jflw id area Jaccard_max n area_ovrlap'
  write(un,"(a)") &
        '  id_nlni area_nlni area_ovrlap Jaccard'

  call logmsg('id_jflw ara_jflw id_nlni ara_nlni ara_ovrlap Jaccard')
  allocate(Jaccard(32))
  allocate(arg(32))
  do ibsn = 1, nbsn_max
    Jcd => lst_Jaccard(ibsn)
    Jcd%bsnid = bsnid(ibsn)
    Jcd%bsnara = bsnara(ibsn)

    call search(bsnid(ibsn), rt_tidx, ijs)
    !-------------------------------------------------------------
    ! Case: Basin was not found in the remapping table,
    ! meaning that it has no intersecting basin
    !-------------------------------------------------------------
    if( ijs == 0 )then
      call logmsg(str(bsnid(ibsn),dgt_bsnid)//' '//str(bsnara(ibsn),'es9.2')//&
                ' no intersection')
      Jcd%n = 0
      Jcd%area_ovrlap_tot = 0.d0
      cycle
    endif
    !-------------------------------------------------------------
    ! Case: Basin was found in the remapping table
    !-------------------------------------------------------------
    do while( ijs > 1 )
      if( rt_tidx(ijs-1) /= rt_tidx(ijs) ) exit
      ijs = ijs - 1
    enddo
    ije = ijs
    do while( ije < nij_rt )
      if( rt_tidx(ije+1) /= rt_tidx(ije) ) exit
      ije = ije + 1
    enddo
    nbsn_ovrlap = ije - ijs + 1
    if( nbsn_ovrlap > size(Jaccard) )then
      call realloc(Jaccard, size(Jaccard)*2, clear=.true.)
      call realloc(arg, size(Jaccard), clear=.true.)
    endif

    !call logmsg(str(bsnid(ibsn),dgt(bsnid(nbsn_max)))//&
    !            ' ij: '//str((/ijs,ije/),dgt(nij_rt),' - '))

    ibsn_ovrlap = 0
    Jcd%area_ovrlap_tot = 0.d0
    do ij = ijs, ije
      call add(ibsn_ovrlap)

      call add(Jcd%area_ovrlap_tot, rt_area(ij))

      call search(rt_sidx(ij), bsnid_nlni, ibsn_nlni)
      if( ibsn_nlni == 0 )then
        call errend('ibsn_nlni == 0'//&
                  '\n  ij: '//str(ij)//&
                  '\n  sidx: '//str(rt_sidx(ij)))
      endif
      Jaccard(ibsn_ovrlap) = rt_area(ij) / (bsnara(ibsn)+bsnara_nlni(ibsn_nlni)-rt_area(ij))
    enddo
    Jaccard_max = maxval(Jaccard(:nbsn_ovrlap))

    !call logmsg('Jaccard max: '//str(Jaccard_max,'f6.3'))

    Jcd%n = 0
    do ibsn_ovrlap = 1, nbsn_ovrlap
      if( Jaccard(ibsn_ovrlap) < Jaccard_max .and. &
          Jaccard(ibsn_ovrlap) < JACCARD_THRESH ) cycle
      call add(Jcd%n)
    enddo

    allocate(Jcd%Jaccard(Jcd%n))
    allocate(Jcd%bsnid_nlni(Jcd%n))
    allocate(Jcd%bsnara_nlni(Jcd%n))
    allocate(Jcd%area_ovrlap(Jcd%n))

    Jcd%n = 0
    Jcd%argmax = 0
    ibsn_ovrlap = 0
    do ij = ijs, ije
      call add(ibsn_ovrlap)

      if( Jaccard(ibsn_ovrlap) < Jaccard_max .and. &
          Jaccard(ibsn_ovrlap) < JACCARD_THRESH ) cycle

      call add(Jcd%n)

      call search(rt_sidx(ij), bsnid_nlni, ibsn_nlni)
      Jcd%Jaccard(Jcd%n) = Jaccard(ibsn_ovrlap)
      Jcd%bsnid_nlni(Jcd%n) = rt_sidx(ij)
      Jcd%bsnara_nlni(Jcd%n) = bsnara_nlni(ibsn_nlni)
      Jcd%area_ovrlap(Jcd%n) = rt_area(ij)
    enddo

    call argsort(Jcd%Jaccard(:Jcd%n), arg(:Jcd%n))
    call reverse(arg(:Jcd%n))
    Jcd%argmax = arg(1)
    !-----------------------------------------------------------
    ! Append to the list
    !-----------------------------------------------------------
    write(un,"(1x,i8,1x,i8,1x,es11.4,1x,f6.3,1x,i0,1x,es11.4)") &
          ibsn, bsnid(ibsn), bsnara(ibsn), &
          Jcd%Jaccard(Jcd%argmax), Jcd%n, Jcd%area_ovrlap_tot
    do iibsn_Jcd = 1, Jcd%n
      ibsn_Jcd = arg(iibsn_Jcd)
      call logmsg(str(bsnid(ibsn),dgt_bsnid)//' '//str(bsnara(ibsn),'es9.2')//&
                ' '//str(Jcd%bsnid_nlni(ibsn_Jcd),dgt_bsnid_nlni)//&
                ' '//str(Jcd%bsnara_nlni(ibsn_Jcd),'es9.2')//&
                ' '//str(Jcd%area_ovrlap(ibsn_Jcd),'es9.2')//&
                ' '//str(Jcd%Jaccard(ibsn_Jcd),'f6.3'))
      write(un,"(1x,i6,1x,es11.4,1x,es11.4,1x,f6.3)") &
            Jcd%bsnid_nlni(ibsn_Jcd), Jcd%bsnara_nlni(ibsn_Jcd), &
            Jcd%area_ovrlap(ibsn_Jcd), Jcd%Jaccard(ibsn_Jcd)
    enddo
  enddo  ! ibsn/

  close(un)
  call logmsg('Saved '//str(f_lst_jaccard))

  nullify(Jcd)
  deallocate(Jaccard, arg)

  deallocate(rt_sidx, rt_tidx, rt_area)

  call logext()
  !-------------------------------------------------------------
  ! Make maps of Jaccard index
  !-------------------------------------------------------------
!if( .false. )then
  call logent('Making maps of Jaccard index')

  allocate(bsnmap(NX,NY))
  allocate(Jcdmap(NX,NY))

  bsnid_prev = BSN_MISS
  do ity = 1, NTY
  do itx = 1, NTX
    f_bsnmap_jflw = get_f_map_tile(RESOLUTION_1SEC, 'bsn', itx, ity)
    if( access(f_bsnmap_jflw,' ') /= 0 ) cycle
    call logmsg(tilename(itx,ity))

    call traperr( rbin(bsnmap, f_bsnmap_jflw) )
    do iy = 1, NY
    do ix = 1, NX
      if( bsnmap(ix,iy) == BSN_MISS )then
        Jcdmap(ix,iy) = JACCARD_MISS
        cycle
      endif
      if( bsnmap(ix,iy) /= bsnid_prev )then
        bsnid_prev = bsnmap(ix,iy)
        call search(bsnmap(ix,iy), bsnid, ibsn)
      endif
      if( ibsn > nbsn_max )then
        Jcdmap(ix,iy) = JACCARD_MISS
      else
        Jcd => lst_Jaccard(ibsn)
        if( Jcd%n == 0 )then
          Jcdmap(ix,iy) = JACCARD_MISS
        else
          Jcdmap(ix,iy) = Jcd%Jaccard(Jcd%argmax)
        endif
      endif
    enddo  ! ix/
    enddo  ! iy/

    f_Jcdmap = get_f_map_tile(RESOLUTION_1SEC, 'Jaccard', itx, ity)
    call logmsg('Writing '//str(f_Jcdmap))
    call traperr( wbin(Jcdmap, f_Jcdmap) )
  enddo  ! itx/
  enddo  ! ity/

  deallocate(bsnmap, Jcdmap)
  nullify(Jcd)
  deallocate(lst_Jaccard)

  call logext()
!endif
  !-------------------------------------------------------------
  ! Make a low-res. map
  !-------------------------------------------------------------
!if( .false. )then
  call logent('Making a low-resolution map of Jaccard index')

  allocate(Jcdmap(NX,NY))
  allocate(Jcdmap_lres(NGX/RES_RATIO,NGY/RES_RATIO))
  Jcdmap_lres(:,:) = JACCARD_MISS

  do ity = 1, NTY
  do itx = 1, NTX
    f_bsnmap_jflw = get_f_map_tile(RESOLUTION_1SEC, 'bsn', itx, ity)
    if( access(f_bsnmap_jflw,' ') /= 0 ) cycle
    call logmsg(tilename(itx,ity))

    f_Jcdmap = get_f_map_tile(RESOLUTION_1SEC, 'Jaccard', itx, ity)
    call traperr( rbin(Jcdmap, f_Jcdmap) )

    cx0 = NX/RES_RATIO * (itx-1)
    cy0 = NY/RES_RATIO * (ity-1)

    do icy = 1, NY/RES_RATIO
    do icx = 1, NX/RES_RATIO
      Jcdmap_lres(cx0+icx,cy0+icy) = 0.d0
      n_valid = 0
      do iy = (icy-1)*RES_RATIO+1, icy*RES_RATIO
      do ix = (icx-1)*RES_RATIO+1, icx*RES_RATIO
        if( Jcdmap(ix,iy) == JACCARD_MISS ) cycle
        call add(n_valid)
        call add(Jcdmap_lres(cx0+icx,cy0+icy), Jcdmap(ix,iy))
      enddo  ! ix/
      enddo  ! iy/
      if( n_valid >= RES_RATIO**2/2 )then
        call div(Jcdmap_lres(cx0+icx,cy0+icy), n_valid)
      else
        Jcdmap_lres(cx0+icx,cy0+icy) = JACCARD_MISS
      endif
    enddo  ! icx/
    enddo  ! icy/
  enddo  ! itx/
  enddo  ! ity/

  call logmsg('Shape: ('//str(shape(Jcdmap_lres),',')//')')

  f_Jcdmap_lres = get_f_map_tile(RESOLUTION_1SEC, 'Jaccard_lres')
  call logmsg('Writing '//str(f_Jcdmap_lres))
  call traperr( wbin(Jcdmap_lres, f_Jcdmap_lres) )

  deallocate(Jcdmap, Jcdmap_lres)

  call logext()
!endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calcWatsysCorresp
!===============================================================
!
!===============================================================
end module mod_calc_watsys_corresp
