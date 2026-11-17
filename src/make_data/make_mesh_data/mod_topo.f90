module mod_topo
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_util
  use c2_strnk_const, only: &
    DGT_NWKID
  use c2_strnk_type, only: &
    cmn_node_, &
    cmn_channel_, &
    cmn_watsys_, &
    cmn_nwknode_, &
    cmn_network_
  use c3_joint_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calcUpperArea
  public :: removeRiverChannelEffect
  public :: fillDepressions
  public :: scaleUp
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_topo'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine calcUpperArea(&
  catmType, resl, modification, id, overwrite &
)
  use c2_jflw_const, &
    jflw_set_resolution => set_resolution
  use c2_jflw_grid, only: &
    south_of_gy
  use c3_jflw_util, only: &
    jflw_get_id_range
  use c3_jflw_io, only: &
    jflw_strid                      , &
    jflw_read_basin_domain_from_each, &
    jflw_get_f_map_basin            
  use c3_strnk_io, only: &
    strnk_read_network_mesh_domain, &
    strnk_get_f_lst_networks_mesh, &
    strnk_get_f_network_mesh
  use c3_joint_const
  use c3_joint_util, only: &
    get_varopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calcUpperArea'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl
  integer     , intent(in) :: modification
  character(*), intent(in) :: id
  logical     , intent(in) :: overwrite

  character(CLEN_PATH) :: opt_elv_in, opt_dir_in, opt_var_out
  character(32) :: id_
  integer :: id_int, idmin_int, idmax_int
  integer :: nCatm, jCatm
  integer(4), pointer :: bsn(:,:)
  integer(1), pointer :: fdr(:,:)
  real(8), pointer :: upa(:,:)
  integer, pointer :: upg(:,:)
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixara(:)
  integer :: nx, ny, gxs, gxe, gys, gye
  integer :: igy
  real(8) :: west, east, south, north

  logical :: is_done
  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call jflw_set_resolution(resl)

  call get_varopt(&
    catmType, modification, PRCNAM, &
    opt_elv_in, opt_dir_in, opt_var_out &
  )

  ! Calc. pixel area
  allocate(pixara(1:NGY))
  allocate(pixlat(0:NGY))

  pixlat(0) = REGION_NORTH * d2r
  do igy = 1, NGY
    pixlat(igy) = south_of_gy(igy) * d2r
  enddo
  !call logmsg('pixlat: '//str(pixlat(:2)*r2d,'f11.7',', ')//&
  !            ', ..., '//str(pixlat(NGY-2:)*r2d,'f11.7',', '))

  pixara(:) = area_sphere_rect(pixlat(0:NGY-1), pixlat(1:NGY)) &
    * (GRIDSIZE_LON*d2r) * (EARTH_R*1d-3)**2  ! [km2]

  deallocate(pixlat)

  nullify(bsn, fdr, upa, upg)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( catmType )
  !-------------------------------------------------------------
  ! Case: J-FlwDir basin
  case( CATMTYPE__BASIN )

    if( id == '' )then
      call errend(ERROR_CODE__VALUE, 'Id is missing.')

    elseif( index(id, '-') /= 0 )then
      call jflw_get_id_range(id, idmin_int, idmax_int)
      do id_int = idmin_int, idmax_int
        id_ = jflw_strid(id_int)
        call logmsg('Basin '//str(id_))
        call prep_basin(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_basin(id_)
      enddo

    else
      call logmsg('Basin '//str(id))
      call prep_basin(id, is_done)
      if( .not. is_done )then
        call core()
        call output_basin(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Network
  case( CATMTYPE__NETWORK )

    if( id == '' )then
      f = strnk_get_f_lst_networks_mesh(resl)
      call logmsg('Reading '//str(f))
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nCatm
      do jCatm = 1, nCatm
        read(un,*) c_, id_
        call logmsg('Network '//str(id_))
        call prep_network(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_network(id_)
      enddo
      close(un)

    else
      call logmsg('Network '//str(id))
      call prep_network(id, is_done)
      if( .not. is_done )then
        call core()
        call output_network(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(ERROR_CODE__VALUE, 'catmType == '//str(catmType))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(bsn, 0)
  call realloc(fdr, 0)
  call realloc(upa, 0)
  call realloc(upg, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine prep_basin(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_basin'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(jflw_get_f_map_basin(resl, 'upa', id, opt_var_out)) .and. &
    file_exists(jflw_get_f_map_basin(resl, 'upg', id, opt_var_out))
  if( is_done )then
    call logmsg('Output files already exist.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Loading input data')

  call jflw_read_basin_domain_from_each(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  call realloc(fdr, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(upa, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(upg, (/gxs,gys/), (/gxe,gye/), clear=.true.)

  f = jflw_get_f_map_basin(resl, 'dir', id, opt_dir_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(fdr, f) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_basin
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine output_basin(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_basin'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Saving data')

  f = jflw_get_f_map_basin(resl, 'upa', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(upa(gxs:gxe,gys:gye), f, dtype=DTYPE_REAL, replace=.true.) )

  f = jflw_get_f_map_basin(resl, 'upg', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(upg(gxs:gxe,gys:gye), f, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_basin
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine prep_network(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_network'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(strnk_get_f_network_mesh(resl, 'upa', id, opt_var_out)) .and. &
    file_exists(strnk_get_f_network_mesh(resl, 'upg', id, opt_var_out))
  if( is_done )then
    call logmsg('Output files already exist.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Loading input data')

  call strnk_read_network_mesh_domain(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north &
  )

  call realloc(fdr, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(upa, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(upg, (/gxs,gys/), (/gxe,gye/), clear=.true.)

  f = strnk_get_f_network_mesh(resl, 'dir', id, opt_dir_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(fdr, f) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_network
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine output_network(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_network'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Saving data')

  f = strnk_get_f_network_mesh(resl, 'upa', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(upa(gxs:gxe,gys:gye), f, dtype=DTYPE_REAL, replace=.true.) )

  f = strnk_get_f_network_mesh(resl, 'upg', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(upg(gxs:gxe,gys:gye), f, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_network
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine core()
  implicit none

  call calcUpperArea__core(gxs, gys, fdr, pixara, upa, upg)
end subroutine core
!---------------------------------------------------------------
end subroutine calcUpperArea
!===============================================================
!
!===============================================================
subroutine calcUpperArea__core(&
  gxs, gys, fdr, pixara, &
  upa, upg &
)
  use c2_jflw_const
!  use c2_jflw_grid, only: &
!    get_nextxy
  use c2_jflw_grid
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calcUpperArea__core'
  integer, intent(in) :: gxs, gys
  integer(1), intent(in) :: fdr(gxs:,gys:)
  real(8)   , intent(in) :: pixara(1:)
  real(8), intent(out) :: upa(gxs:,gys:)
  integer, intent(out) :: upg(gxs:,gys:)

  real(8), allocatable :: upaadd(:,:)
  integer, allocatable :: upgadd(:,:)
  integer, allocatable :: lst_gx(:), lst_gy(:)
  integer, allocatable :: passed(:,:)
  integer :: nx, ny, gxe, gye
  integer :: igx, igy, gx, gy, gxx, gyy
  integer :: n, n_next, i
  logical :: is_updated
  integer :: iter
  integer :: grdnum_true, grdnum
  real(8) :: bsnara_true, bsnara
  real(8) :: err_norm

  integer, parameter :: ITER_ULIM = 100000

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Set up
  !-------------------------------------------------------------
  nx = size(fdr,1)
  ny = size(fdr,2)
  gxe = gxs + nx - 1
  gye = gys + ny - 1

  allocate(upaadd(gxs:gxe,gys:gye))
  allocate(upgadd(gxs:gxe,gys:gye))
  allocate(passed(gxs:gxe,gys:gye))
  !-------------------------------------------------------------
  ! Calc. upper area
  !-------------------------------------------------------------
  call logent('Calculating upper area')

  n = 0
  do igy = gys, gye
  do igx = gxs, gxe
    if( is_fdr_land(fdr(igx,igy)) ) call add(n)
  enddo
  enddo
  allocate(lst_gx(n))
  allocate(lst_gy(n))

  n = 0
  upgadd(:,:) = 0
  upaadd(:,:) = 0.d0
  do igy = gys, gye
  do igx = gxs, gxe
    if( .not. is_fdr_land(fdr(igx,igy)) ) cycle
    upgadd(igx,igy) = 1
    upaadd(igx,igy) = pixara(igy)
    call add(n)
    lst_gx(n) = igx
    lst_gy(n) = igy
  enddo
  enddo
  grdnum_true = sum(upgadd)
  bsnara_true = sum(upaadd)

  upa(:,:) = 0.d0
  upg(:,:) = 0
  is_updated = .true.
  iter = 0
  passed(:,:) = 0
  do while( is_updated )
    is_updated = .false.
    call add(iter)
    if( iter > ITER_ULIM )then
      call errend(ERROR_CODE__CONDITION, &
        'Iteration reached the limit.')
    endif

    n_next = 0
    do i = 1, n
      gx = lst_gx(i)
      gy = lst_gy(i)

      if( upgadd(gx,gy) == 0 ) cycle

      is_updated = .true.

      call get_nextxy(gx, gy, fdr(gx,gy), gxx, gyy)

      call add(upg(gx,gy), upgadd(gx,gy))
      call add(upa(gx,gy), upaadd(gx,gy))

      if( gxx > 0 )then
        if( passed(gxx,gyy) /= iter )then
          passed(gxx,gyy) = iter
          call add(n_next)
          lst_gx(n_next) = gxx
          lst_gy(n_next) = gyy
        endif
        call add(upgadd(gxx,gyy), upgadd(gx,gy))
        call add(upaadd(gxx,gyy), upaadd(gx,gy))
      endif

      upgadd(gx,gy) = 0
      upaadd(gx,gy) = 0.d0
    enddo  ! i

    if( n_next > n )then
      call logwrn(ERROR_CODE__CONDITION, 'n_next > n'//&
      '\nn: '//str(n)//', n_next: '//str(n_next))
    endif

    n = n_next
  enddo  ! while is_updated

  call logext()
  !-------------------------------------------------------------
  ! Validate results
  !-------------------------------------------------------------
  call logent('Validating the results')

  grdnum = 0
  bsnara = 0.d0
  do igy = gys, gye
  do igx = gxs, gxe
    selectcase( fdr(igx,igy) )
    case( FDR__RIVERMOUTH, &
          FDR__INLAND )
      call add(grdnum, upg(igx,igy))
      call add(bsnara, upa(igx,igy))
    case( 1_8: )
      if( upg(igx,igy) == 0 .or. upa(igx,igy) == 0.d0 )then
        call errend(ERROR_CODE__CONDITION, &
          '(gx,gy)=('//str((/igx,igy/),',')//') dir='//&
          str(fdr(igx,igy))//' upg='//str(upg(igx,igy))//&
          ' upa='//str(upa(igx,igy)))
      endif
    case( FDR__MISS )
      upg(igx,igy) = UPG_MISS
      upa(igx,igy) = UPA_MISS
    case default
      call errend(ERROR_CODE__VALUE, 'fdr=='//str(fdr(igx,igy)))
    endselect
  enddo
  enddo

!  grdnum = 0
!  bsnara = 0.d0
!  do igy = gys, gye
!  do igx = gxs, gxe
!    if( .not. is_fdr_land(fdr(igx,igy)) ) cycle
!    if( fdr(igx,igy) <= 0_1 )then
!      call add(bsnara, upa(igx,igy))
!      call add(grdnum, upg(igx,igy))
!    else
!      call get_nextxy(igx, igy, fdr(igx,igy), gxx, gyy)
!      if( fdr(gxx,gyy) < 0_1 )then
!      call logwrn('('//str((/igx,igy/),',')//') fdr '//str(fdr(igx,igy))//&
!        ' ('//str((/center_of_gx(igx),center_of_gy(igy)/),'es20.13',',')//')')
!        call add(bsnara, upa(igx,igy))
!        call add(grdnum, upg(igx,igy))
!      endif
!    endif
!  enddo  ! igx
!  enddo  ! igy

  call logmsg('upg max: '//str(maxval(upg))//' grdnum: '//str(grdnum_true))
  call logmsg('upa max: '//str(maxval(upa))//' bsnara: '//str(bsnara_true))

  if( grdnum /= grdnum_true )then
    call errend(ERROR_CODE__CONDITION, &
      'The number of grids do not match.'//&
    '\n  truth     : '//str(grdnum_true)//&
    '\n  calculated: '//str(grdnum))
  endif

  err_norm = abs(bsnara - bsnara_true) / bsnara_true
  call logmsg('basin area truth: '//str(bsnara_true)//&
            ', upa: '//str(bsnara)//&
            ', normalized error: '//str(err_norm))
  if( err_norm > 1d-8 )then
    call errend(ERROR_CODE__CONDITION, 'Error exceeded the threshold.')
  endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(upgadd)
  deallocate(upaadd)
  deallocate(passed)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calcUpperArea__core
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
subroutine removeRiverChannelEffect(&
  catmType, resl, modification, id, &
  overwrite &
)
  use c2_jflw_const, &
    jflw_set_resolution => set_resolution
  use c3_jflw_util, only: &
    jflw_get_id_range
  use c3_jflw_io, only: &
    jflw_strid                      , &
    jflw_read_basin_domain_from_each, &
    jflw_get_f_map_basin            
  use c3_strnk_io, only: &
    strnk_read_network_mesh_domain, &
    strnk_get_f_lst_networks_mesh, &
    strnk_get_f_network_mesh
  use c3_joint_util, only: &
    get_varopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'removeRiverChannelEffect'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl
  integer     , intent(in) :: modification
  character(*), intent(in) :: id
  logical     , intent(in) :: overwrite

  character(CLEN_PATH) :: opt_elv_in, opt_dir_in, opt_var_out
  character(32) :: id_
  integer :: id_int, idmin_int, idmax_int
  real(8), pointer :: elv(:,:)
  real(8), pointer :: upa(:,:)
  real(8), pointer :: aelv(:,:)
  integer :: nCatm, iCatm
  integer :: nx, ny, gxs, gxe, gys, gye
  real(8) :: west, east, south, north

  logical :: is_done
  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_varopt(&
    catmType, modification, PRCNAM, &
    opt_elv_in, opt_dir_in, opt_var_out &
  )

  call jflw_set_resolution(resl)

  nullify(elv, upa, aelv)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( catmType )
  !-------------------------------------------------------------
  ! Case: J-FlwDir basin
  case( CATMTYPE__BASIN )

    if( id == '' )then
      call errend(ERROR_CODE__VALUE, 'Id is missing.')

    elseif( index(id, '-') /= 0 )then
      call jflw_get_id_range(id, idmin_int, idmax_int)
      do id_int = idmin_int, idmax_int
        id_ = jflw_strid(id_int)
        call logmsg('Basin '//str(id_))
        call prep_basin(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_basin(id_)
      enddo

    else
      call logmsg('Basin '//str(id))
      call prep_basin(id, is_done)
      if( .not. is_done )then
        call core()
        call output_basin(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: StrRank network
  case( CATMTYPE__NETWORK )

    if( id == '' )then
      f = strnk_get_f_lst_networks_mesh(resl)
      call logmsg('Reading '//str(f))
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nCatm
      read(un,*)
      do iCatm = 1, nCatm
        read(un,*) c_, id_
        call logmsg('Network '//str(id_))
        call prep_network(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_network(id_)
      enddo  ! iCatm
      close(un)

    else
      call logmsg('Network '//str(id))
      call prep_network(id, is_done)
      if( .not. is_done )then
        call core()
        call output_network(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: networkset
  case( CATMTYPE__NETWORKSET )
    call errend(ERROR_CODE__NOIMPLEMENTATION, 'catmType == '//str(catmType))

  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(ERROR_CODE__VALUE, 'catmType == '//str(catmType))

  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(elv, 0)
  call realloc(upa, 0)
  call realloc(aelv, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine prep_basin(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_basin'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(jflw_get_f_map_basin(resl, 'elv', id, opt_var_out))
  if( is_done )then
    call logmsg('Output file already exists.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  call logent('Loading input data')

  call jflw_read_basin_domain_from_each(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  call realloc(elv, (/1,1/), (/nx,ny/))
  call realloc(upa, (/1,1/), (/nx,ny/))
  call realloc(aelv, (/1,1/), (/nx,ny/))

  f = jflw_get_f_map_basin(resl, 'elv', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(elv, f, dtype=DTYPE_REAL) )

  f = jflw_get_f_map_basin(resl, 'upa', id)
  call logmsg('Reading '//str(f))
  call traperr( rbin(upa, f, dtype=DTYPE_REAL) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_basin
!---------------------------------------------------------------
subroutine output_basin(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_basin'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  call logent('Saving data')

  f = jflw_get_f_map_basin(resl, 'elv', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(aelv, f, dtype=DTYPE_REAL, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_basin
!---------------------------------------------------------------
subroutine prep_network(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_network'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(strnk_get_f_network_mesh(resl, 'elv', id, opt_var_out))
  if( is_done )then
    call logmsg('Output file already exists.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  call logent('Loading input data')

  call strnk_read_network_mesh_domain(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  call realloc(elv, (/1,1/), (/nx,ny/))
  call realloc(upa, (/1,1/), (/nx,ny/))
  call realloc(aelv, (/1,1/), (/nx,ny/))

  f = strnk_get_f_network_mesh(resl, 'elv', id, opt_elv_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(elv, f, dtype=DTYPE_REAL) )

  f = strnk_get_f_network_mesh(resl, 'upa', id, opt_dir_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(upa, f, dtype=DTYPE_REAL) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_network
!---------------------------------------------------------------
subroutine output_network(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_network'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  call logent('Saving data')

  f = strnk_get_f_network_mesh(resl, 'elv', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(aelv, f, dtype=DTYPE_REAL, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_network
!---------------------------------------------------------------
subroutine core()
  implicit none

  call removeRiverChannelEffect__core(elv, upa, aelv)
end subroutine core
!---------------------------------------------------------------
end subroutine removeRiverChannelEffect
!===============================================================
!
!===============================================================
subroutine removeRiverChannelEffect__core(&
  elv, upa, aelv &
)
  use c2_jflw_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'removeRiverChannelEffect__core'
  real(8), intent(in) :: elv(:,:)
  real(8), intent(in) :: upa(:,:)
  real(8), intent(out) :: aelv(:,:)

  real(8) :: elv_neighbor
  integer :: n_neighbor
  integer :: nx, ny, ix, iy, ibx, iby

  integer :: dgt_xy

  real(8), parameter :: UPA_RIV_THRESH = 1.d0  ! [km2]

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Modifying elevation')

  nx = size(elv,1)
  ny = size(elv,2)
  dgt_xy = dgt(max(nx,ny))

  aelv(:,:) = elv(:,:)

  do iy = 1, ny
  do ix = 1, nx
    if( upa(ix,iy) < UPA_RIV_THRESH ) cycle

    n_neighbor = 0
    elv_neighbor = 0.d0
    do iby = max(iy-1,1), min(iy+1,ny)
    do ibx = max(ix-1,1), min(ix+1,nx)
      if( upa(ibx,iby) >= UPA_RIV_THRESH ) cycle
      if( elv(ibx,iby) == ELV_MISS ) cycle

      call add(elv_neighbor, elv(ibx,iby))
      call add(n_neighbor)
    enddo  ! ibx/
    enddo  ! iby/

    if( n_neighbor == 0 )then
      call logmsg('Not updated; non-river grid not found around. '//&
        'elv('//str((/ix,iy/),dgt_xy,',')//')='//str(elv(ix,iy)))
    else
      aelv(ix,iy) = elv_neighbor / n_neighbor
    endif
  enddo  ! ix/
  enddo  ! iy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine removeRiverChannelEffect__core
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
subroutine fillDepressions(&
  catmType, resl, modification, id, &
  overwrite &
)
  use c2_jflw_const, &
    jflw_set_resolution => set_resolution
  use c3_jflw_util, only: &
    jflw_get_id_range
  use c3_jflw_io, only: &
    jflw_strid                      , &
    jflw_read_basin_domain_from_each, &
    jflw_get_f_map_basin
  use c3_strnk_io, only: &
    strnk_read_network_mesh_domain, &
    strnk_get_f_lst_networks_mesh, &
    strnk_get_f_network_mesh
  use c3_joint_const
  use c3_joint_util, only: &
    get_varopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fillDepressions'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl
  integer     , intent(in) :: modification
  character(*), intent(in) :: id
  logical     , intent(in) :: overwrite

  character(CLEN_PATH) :: opt_elv_in, opt_dir_in, opt_var_out
  real(8)   , pointer :: elv(:,:)
  integer(1), pointer :: fdr(:,:)
  real(8)   , pointer :: aelv(:,:)
  integer :: nNwk, iNwk
  character(32) :: id_
  integer :: id_int, idmin_int, idmax_int
  integer :: nx, ny, gxs, gxe, gys, gye
  real(8) :: west, east, south, north

  logical :: is_done
  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call jflw_set_resolution(resl)

  call get_varopt(&
    catmType, modification, PRCNAM, &
    opt_elv_in, opt_dir_in, opt_var_out &
  )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(elv, fdr, aelv)

  selectcase( catmType )
  !-------------------------------------------------------------
  ! Case: J-FlwDir basin
  case( CATMTYPE__BASIN )

    if( id == '' )then
      call errend(ERROR_CODE__VALUE, 'Id is missing.')

    elseif( index(id, '-') /= 0 )then

      call jflw_get_id_range(id, idmin_int, idmax_int)
      do id_int = idmin_int, idmax_int
        id_ = jflw_strid(id_int)
        call logmsg('Basin '//str(id_))
        call prep_basin(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_basin(id_)
      enddo

    else
      call logmsg('Basin '//str(id))
      call prep_basin(id, is_done)
      if( .not. is_done )then
        call core()
        call output_basin(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: StrRank network
  case( CATMTYPE__NETWORK )

    if( id == '' )then
      f = strnk_get_f_lst_networks_mesh(resl)
      call logmsg('Reading '//str(f))
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nNwk
      read(un,*)
      do iNwk = 1, nNwk
        read(un,*) c_, id_
        call logmsg('Network '//str(id_))
        call prep_network(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_network(id_)
      enddo
      close(un)

    else
      call logmsg('Network '//str(id))
      call prep_network(id, is_done)
      if( .not. is_done )then
        call core()
        call output_network(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: networkset
  case( CATMTYPE__NETWORKSET )
    call errend(ERROR_CODE__NOIMPLEMENTATION, 'catmType == '//str(catmType))

  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(ERROR_CODE__VALUE, 'catmType == '//str(catmType))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(elv, 0)
  call realloc(fdr, 0)
  call realloc(aelv, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
  !-------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine prep_basin(id, is_done)
  use c2_jflw_io
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_basin'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(jflw_get_f_map_basin(resl, 'elv', id, opt_var_out))
  if( is_done )then
    call logmsg('Output file already exists.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  call logent('Loading input data')

  call jflw_read_basin_domain_from_each(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  call realloc(elv, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(fdr, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(aelv, (/gxs,gys/), (/gxe,gye/), clear=.true.)

  f = jflw_get_f_map_basin(resl, 'elv', id, opt_elv_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(elv, f, dtype=DTYPE_REAL) )

  f = jflw_get_f_map_basin(resl, 'dir', id, opt_dir_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(fdr, f) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_basin
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine output_basin(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_basin'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = jflw_get_f_map_basin(resl, 'elv', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(elv, f, dtype=DTYPE_REAL, replace=.true.) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_basin
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine prep_network(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_network'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(strnk_get_f_network_mesh(resl, 'elv', id, opt_var_out))
  if( is_done )then
    call logmsg('Output file already exists.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  call logent('Loading input data')

  call strnk_read_network_mesh_domain(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  call realloc(elv, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(fdr, (/gxs,gys/), (/gxe,gye/), clear=.true.)
  call realloc(aelv, (/gxs,gys/), (/gxe,gye/), clear=.true.)

  f = strnk_get_f_network_mesh(resl, 'elv', id, opt_elv_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(elv, f, dtype=DTYPE_REAL) )

  f = strnk_get_f_network_mesh(resl, 'dir', id, opt_dir_in)
  call logmsg('Reading '//str(f))
  call traperr( rbin(fdr, f, dtype=DTYPE_INT1) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_network
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine output_network(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_network'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = strnk_get_f_network_mesh(resl, 'elv', id, opt_var_out)
  call logmsg('Writing '//str(f))
  call traperr( mkdir(dirname(f)) )
  call traperr( wbin(elv, f, dtype=DTYPE_REAL, replace=.true.) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_network
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine core()
  implicit none

  call fillDepressions__core(gxs, gys, elv, fdr, aelv)
end subroutine core
!---------------------------------------------------------------
end subroutine fillDepressions
!===============================================================
!
!===============================================================
subroutine fillDepressions__core(gxs, gys, elv, fdr, aelv)
  use c2_jflw_const
  use c2_jflw_grid, only: &
    center_of_gx    , &
    center_of_gy    , &
    west_of_gx      , &
    east_of_gx      , &
    south_of_gy     , &
    north_of_gy     , &
    get_nextxy      , &
    is_fdr_land     , &
    is_fdr_direction, &
    is_fdr_outlet   , &
    standard_dist_from_center
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fillDepressions__core'
  integer, intent(in) :: gxs, gys
  real(8), intent(in) :: elv(gxs:,gys:)
  integer(1), intent(in) :: fdr(gxs:,gys:)
  real(8), intent(out) :: aelv(gxs:,gys:)

  logical(1), allocatable :: source(:,:)
  logical(1), allocatable :: passed(:,:)
  real(8), allocatable :: dist(:,:)
  integer, allocatable :: lst_gx_source(:), lst_gy_source(:)
  real(8), allocatable :: lst_dist_source(:)
  integer, pointer :: lst_gx(:), lst_gy(:)
  integer, pointer :: lst_idx_updated(:)
  integer, allocatable :: arg(:)
  integer :: nSource, iSource
  integer :: nx, ny
  integer :: gxe, gye
  integer :: gx, gy, gxx, gyy, igx, igy
  integer :: gxup, gyup, gxdn, gydn
  integer :: gx_s1, gy_s1, gx_s2, gy_s2
  real(8) :: lon, lat, lon2, lat2
  integer :: n, i, i_start
  integer :: n_updated, n_updated_next, k
  integer :: switch
  logical :: is_updated
  real(8) :: dist_all, dist_up

  real(8), parameter :: ELV_LIFT = 500.d0  ! [m]
  real(8), parameter :: ELV_CARVE = 5.d0  ! [m]
  real(8), parameter :: ELV_INCREMENT = 0.01d0  ! [m]

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Set up
  !-------------------------------------------------------------
  nx = size(elv,1)
  ny = size(elv,2)
  gxe = gxs + nx - 1
  gye = gys + ny - 1
  !-------------------------------------------------------------
  ! Make a list of sources
  !-------------------------------------------------------------
  call logent('Making a list of sources')

  allocate(source(gxs:gxe,gys:gye))
  allocate(dist(gxs:gxe,gys:gye))

  source(:,:) = fdr(:,:) /= FDR__MISS
  dist(:,:) = DIST_MISS
  do igy = gys, gye
  do igx = gxs, gxe
    selectcase( fdr(igx,igy) )
    case( 1_8: )
      call get_nextxy(igx, igy, fdr(igx,igy), gxx, gyy)
      source(gxx,gyy) = .false.
    case( FDR__RIVERMOUTH, FDR__INLAND )
      dist(igx,igy) = standard_dist_from_center(igy)
    case( FDR__MISS )
      continue
    case default
      call errend(ERROR_CODE__VALUE, 'fdr == '//str(fdr(igx,igy)))
    endselect
  enddo  ! igx
  enddo  ! igy

  nSource = count(source)
  allocate(lst_gx_source(nSource))
  allocate(lst_gy_source(nSource))
  allocate(lst_dist_source(nSource))

  iSource = 0
  do igy = gys, gye
  do igx = gxs, gxe
    if( .not. source(igx,igy) ) cycle
    call add(iSource)

    ! Calc. distance from source to outlet
    gx = igx
    gy = igy
    lon = center_of_gx(gx)
    lat = center_of_gy(gy)

    dist_all = 0.d0
    do while( dist(gx,gy) == DIST_MISS )
      if( .not. is_fdr_land(fdr(gx,gy)) )then
        call errend(ERROR_CODE__VALUE, 'fdr == '//str(fdr(gx,gy))//&
          '\nsource: ('//str((/igx,igy/),',')//') fdr='//str(fdr(igx,igy))//&
            ' elv='//str(elv(igx,igy))//&
          '\nnow: ('//str((/gx,gy/),',')//')')
      endif
      call get_nextxy(gx, gy, fdr(gx,gy), gxx, gyy)
      lon2 = center_of_gx(gxx)
      lat2 = center_of_gy(gyy)
      call add(dist_all, dist_earth(lon, lat, lon2, lat2))
      gx = gxx
      gy = gyy
      lon = lon2
      lat = lat2
    enddo
    call add(dist_all, dist(gx,gy))

    ! Store data to the list
    lst_gx_source(iSource) = igx
    lst_gy_source(iSource) = igy
    lst_dist_source(iSource) = dist_all

    ! Fill distance to outlet
    gx = igx
    gy = igy
    lon = center_of_gx(gx)
    lat = center_of_gy(gy)

    dist_up = 0.d0
    do while( dist(gx,gy) == DIST_MISS )
      dist(gx,gy) = dist_all - dist_up
      call get_nextxy(gx, gy, fdr(gx,gy), gxx, gyy)
      lon2 = center_of_gx(gxx)
      lat2 = center_of_gy(gyy)
      call add(dist_up, dist_earth(lon, lat, lon2, lat2))
      gx = gxx
      gy = gyy
      lon = lon2
      lat = lat2
    enddo
  enddo  ! igx
  enddo  ! igy

  deallocate(source)

  allocate(arg(nSource))
  call argsort(lst_dist_source, arg)
  call reverse(arg)
  call sort(lst_gx_source, arg)
  call sort(lst_gy_source, arg)
  call sort(lst_dist_source, arg)
  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  ! Modify elevation
  !-------------------------------------------------------------
  call logent('Modifying elevation')

  allocate(lst_gx(nx+ny))
  allocate(lst_gy(nx+ny))
  allocate(lst_idx_updated(nx+ny))

  allocate(passed(gxs:gxe,gys:gye))

  ! Lifting
  !-------------------------------------------------------------
  call logent('Lifting')

  passed(:,:) = .false.

  do iSource = 1, nSource
    !call logmsg('source '//str(iSource)//' / '//str(nSource))

    call make_route_src2out()
    if( n <= 2 ) cycle

    do i = 2, n-1
      gxup = lst_gx(i-1)
      gyup = lst_gy(i-1)
      gx   = lst_gx(i)
      gy   = lst_gy(i)
      gxdn = lst_gx(i+1)
      gydn = lst_gy(i+1)
      if( aelv(gx,gy) < aelv(gxup,gyup) - ELV_LIFT .and. &
          aelv(gx,gy) < aelv(gxdn,gydn) - ELV_LIFT )then
        aelv(gx,gy) = (aelv(gxup,gyup) + aelv(gxdn,gydn)) * 0.5d0
      endif
    enddo

    do i = 1, n
      if( passed(lst_gx(i), lst_gy(i)) ) exit
      passed(lst_gx(i), lst_gy(i)) = .true.
    enddo
  enddo  ! iSource

  call logext()

  ! Carving
  !-------------------------------------------------------------
  call logent('Carving')

  passed(:,:) = .false.

  do iSource = 1, nSource
    !call logmsg('source '//str(iSource)//' / '//str(nSource))

    call make_route_src2out()
    if( n <= 1 ) cycle

    n_updated = n-1
    do i = 1, n-1
      lst_idx_updated(i) = i
    enddo

    do while( n_updated > 0 )
      n_updated_next = 0

      do k = 1, n_updated
        i = lst_idx_updated(k)
        if( i+1 > n )then
          !cycle
          call errend(ERROR_CODE__NOIMPLEMENTATION, &
            'i+1 > n; reached junction or outlet')
        endif
        gxup = lst_gx(i)  ! updated previous step
        gyup = lst_gy(i)
        gx = lst_gx(i+1)
        gy = lst_gy(i+1)

        if( aelv(gx,gy) > aelv(gxup,gyup) + ELV_CARVE )then
          aelv(gx,gy) = aelv(gxup,gyup)
          call add(n_updated_next)
          lst_idx_updated(n_updated_next) = i
        endif
      enddo

      n_updated = n_updated_next
    enddo

    do i = 1, n
      if( passed(lst_gx(i), lst_gy(i)) ) exit
      passed(lst_gx(i), lst_gy(i)) = .true.
    enddo
  enddo  ! iSource

  call logext()

  ! Lifting and carving
  !-------------------------------------------------------------
  call logent('Lifting and curving')

  do iSource = 1, nSource
    !call logmsg('source '//str(iSource)//' / '//str(nSource))

    call make_route_src2out()
    if( n <= 2 ) cycle

    i_start = 1

    is_updated = .true.
    do while( is_updated )
      is_updated = .false.
      switch = 0
      do i = i_start, n-1
        gx = lst_gx(i)
        gy = lst_gy(i)
        gxdn = lst_gx(i+1)
        gydn = lst_gy(i+1)

        if( switch == 0 .and. aelv(gx,gy) < aelv(gxdn,gydn) )then
          i_start = max(i-1, 1)
          gx_s1 = gx
          gy_s1 = gy
          switch = 1
        endif

        if( switch == 1 .and. aelv(gx,gy) > aelv(gxdn,gydn) )then
          gx_s2 = gx
          gy_s2 = gy
          switch = 2
        endif

        if( switch == 2 )then
          call logmsg('Source #'//str(iSource,dgt(nSource))//&
          '\n  elv('//str((/gx_s1,gy_s1/),DGT_GXY,',')//') '//&
            str(aelv(gx_s1,gy_s1))//' -> '//str(aelv(gx_s1,gy_s1)+ELV_INCREMENT)//&
          '\n  elv('//str((/gx_s2,gy_s2/),DGT_GXY,',')//') '//&
            str(aelv(gx_s2,gy_s2))//' -> '//str(aelv(gx_s2,gy_s2)-ELV_INCREMENT))
          call add(aelv(gx_s1,gy_s1), ELV_INCREMENT)
          call add(aelv(gx_s2,gy_s2), -ELV_INCREMENT)
          is_updated = .true.
          exit
        endif
      enddo  ! i
    enddo  ! while is_updated
     
  enddo  ! iSource

  call logext()
  !-------------------------------------------------------------

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_route_src2out()
  implicit none

  gx = lst_gx_source(iSource)
  gy = lst_gy_source(iSource)
  n = 1
  lst_gx(n) = gx
  lst_gy(n) = gy
  do while( is_fdr_direction(fdr(gx,gy)) )
    call get_nextxy(gx, gy, fdr(gx, gy), gxx, gyy)
    gx = gxx
    gy = gyy

    if( n == size(lst_gx) )then
      call realloc(lst_gx, n*2, clear=.false.)
      call realloc(lst_gy, n*2, clear=.false.)
      call realloc(lst_idx_updated, n*2, clear=.false.)
    endif
    call add(n)
    lst_gx(n) = gx
    lst_gy(n) = gy

    if( passed(gx,gy) ) exit
  enddo  ! while is_fdr_direction
end subroutine make_route_src2out
!---------------------------------------------------------------
end subroutine fillDepressions__core
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
subroutine scaleUp(&
  catmType, resl_in, resl_out, modification, id, &
  overwrite &
)
  use c1_grid, only: &
    get_cellsize_ratio
  use c2_jflw_const, &
    jflw_set_resolution => set_resolution
  use c3_jflw_util, only: &
    jflw_get_id_range
  use c3_jflw_io, only: &
    jflw_strid                      , &
    jflw_read_basin_domain_from_each, &
    jflw_write_basin_domain_for_each, &
    jflw_get_f_map_basin
  use c3_strnk_io, only: &
    strnk_read_network_mesh_domain , &
    strnk_write_network_mesh_domain, &
    strnk_get_f_lst_networks_mesh  , &
    strnk_get_f_network_mesh
  use c3_joint_util, only: &
    get_varopt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'scaleUp'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl_in
  character(*), intent(in) :: resl_out
  integer     , intent(in) :: modification
  character(*), intent(in) :: id
  logical     , intent(in) :: overwrite

  integer :: ratio
  integer :: nx, ny, gxs, gxe, gys, gye
  real(8) :: west, east, south, north
  integer :: nx2, ny2, gxs2, gxe2, gys2, gye2
  real(8) :: west2, east2, south2, north2
  real(4)   , pointer :: elv2(:,:)
  integer(1), pointer :: fdr2(:,:)
  integer(4), pointer :: upg2(:,:)

  integer :: nCatm, iCatm
  character(32) :: id_
  integer :: id_int, idmin_int, idmax_int
  logical :: is_done
  character(CLEN_PATH) :: opt_elv_in, opt_dir_in, opt_var_out
  character(CLEN_PATH) :: f
  character(CLEN_PATH) :: f_elv, f_fdr, f_upg
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( resl_in == resl_out )then
    call logmsg('Source and target resolution are same.')
    call logret(PRCNAM, MODNAM)
    return
  endif

  ratio = get_cellsize_ratio(resl_in, resl_out)

  call get_varopt(&
    catmType, modification, PRCNAM, &
    opt_elv_in, opt_dir_in, opt_var_out &
  )

  call jflw_set_resolution(resl_in)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( catmType )
  !-------------------------------------------------------------
  ! Case: J-FlwDir basin
  case( CATMTYPE__BASIN )

    if( id == '' )then
      call errend(ERROR_CODE__VALUE, 'Id is missing.')

    elseif( index(id, '-') /= 0 )then
      call jflw_get_id_range(id, idmin_int, idmax_int)
      do id_int = idmin_int, idmax_int
        id_ = jflw_strid(id_int)
        call logmsg('Basin '//str(id_))
        call prep_basin(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_basin(id_)
      enddo

    else
      call logmsg('Basin '//str(id))
      call prep_basin(id, is_done)
      if( .not. is_done )then
        call core()
        call output_basin(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Network mesh
  case( CATMTYPE__NETWORK )

    if( id == '' )then
      f = strnk_get_f_lst_networks_mesh(resl_in)
      call logmsg('Reading '//str(f))
      open(newunit=un, file=f, status='old')
      read(un,*) c_, nCatm
      read(un,*)
      do iCatm = 1, nCatm
        read(un,*) c_, id_
        call logmsg('Network '//str(id_))
        call prep_network(id_, is_done)
        if( is_done ) cycle
        call core()
        call output_network(id)
      enddo
      close(un)

    else
      call logmsg('Network '//str(id))
      call prep_network(id, is_done)
      if( .not. is_done )then
        call core()
        call output_network(id)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(ERROR_CODE__VALUE, 'catmType == '//str(catmType))

  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine prep_basin(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_basin'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(jflw_get_f_map_basin(resl_out, 'elv', id, opt_elv_in))
  if( is_done )then
    call logmsg('Output file already exists.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  call logent('Preparing inputs')

  call jflw_read_basin_domain_from_each(&
    resl_in, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  f_elv = jflw_get_f_map_basin(resl_in, 'elv', id, opt_elv_in)
  f_fdr = jflw_get_f_map_basin(resl_in, 'dir', id, opt_dir_in)
  f_upg = jflw_get_f_map_basin(resl_in, 'upg', id, opt_dir_in)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_basin
!---------------------------------------------------------------
subroutine output_basin(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_basin'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  call logent('Saving data')

  call jflw_write_basin_domain_for_each(&
    resl_out, id, &
    gxs2, gxe2, gys2, gye2, &
    west2, east2, south2, north2, &
    echo=.true. &
  )

  f_elv = jflw_get_f_map_basin(resl_out, 'elv', id, opt_var_out)
  call logmsg('Writing '//str(f_elv))
  call traperr( mkdir(dirname(f_elv)) )
  call traperr( wbin(elv2, f_elv, replace=.true.) )

  f_fdr = jflw_get_f_map_basin(resl_out, 'dir', id, opt_var_out)
  call logmsg('Writing '//str(f_fdr))
  call traperr( mkdir(dirname(f_fdr)) )
  call traperr( wbin(fdr2, f_fdr, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_basin
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine prep_network(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__prep_network'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  is_done = .not. overwrite .and. &
    file_exists(strnk_get_f_network_mesh(resl_out, 'elv', id, opt_elv_in))
  if( is_done )then
    call logmsg('Output file already exists.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  call logent('Preparing inputs')

  call strnk_read_network_mesh_domain(&
    resl_in, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  f_elv = strnk_get_f_network_mesh(resl_in, 'elv', id, opt_elv_in)
  f_fdr = strnk_get_f_network_mesh(resl_in, 'dir', id, opt_dir_in)
  f_upg = strnk_get_f_network_mesh(resl_in, 'upg', id, opt_dir_in)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_network
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine output_network(id)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__output_network'
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  call logent('Saving data')

  call strnk_write_network_mesh_domain(&
    resl_out, id, &
    nx2, ny2, gxs2, gxe2, gys2, gye2, &
    west2, east2, south2, north2, &
    echo=.true. &
  )

  f_elv = strnk_get_f_network_mesh(resl_out, 'elv', id, opt_var_out)
  call logmsg('Writing '//str(f_elv))
  call traperr( mkdir(dirname(f_elv)) )
  call traperr( wbin(elv2, f_elv, replace=.true.) )

  f_fdr = strnk_get_f_network_mesh(resl_out, 'dir', id, opt_var_out)
  call logmsg('Writing '//str(f_fdr))
  call traperr( mkdir(dirname(f_fdr)) )
  call traperr( wbin(fdr2, f_fdr, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine output_network
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine core()
  implicit none

  call scaleUp__core(&
    ratio, gxs, gxe, gys, gye, f_elv, f_fdr, f_upg, &
    nx2, ny2, gxs2, gxe2, gys2, gye2, &
    west2, east2, south2, north2, &
    elv2, fdr2, upg2 &
  )
end subroutine core
!---------------------------------------------------------------
end subroutine scaleUp
!===============================================================
!
!===============================================================
subroutine scaleUp__core(&
  ratio, gxs0, gxe0, gys0, gye0, &
  f_elv, f_fdr, f_upg, &
  nx2, ny2, gxs2, gxe2, gys2, gye2, &
  west2, east2, south2, north2, &
  elv2, fdr2, upg2 &
)
  use c2_jflw_const
  use c2_jflw_grid, only: &
    west_of_gx      , &
    east_of_gx      , &
    south_of_gy     , &
    north_of_gy     , &
    is_fdr_land     , &
    is_fdr_direction, &
    is_fdr_outlet   , &
    get_nextxy      , &
    get_fdr         , &
    all_sources_reach_outlet
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'scaleUp__core'
  integer, intent(in) :: ratio
  integer, intent(in) :: gxs0, gxe0, gys0, gye0
  character(*), intent(in) :: f_elv, f_fdr, f_upg
  integer, intent(out) :: nx2, ny2, gxs2, gxe2, gys2, gye2
  real(8), intent(out) :: west2, east2, south2, north2
  real(4)   , pointer :: elv2(:,:)  ! out
  integer(1), pointer :: fdr2(:,:)  ! out
  integer(4), pointer :: upg2(:,:)  ! out

  real(4)   , allocatable :: elv(:,:)
  integer(1), allocatable :: fdr(:,:)
  integer(4), allocatable :: upg(:,:)
  integer, allocatable :: top(:), bottom(:), left(:), right(:)
  integer, allocatable :: tile(:,:)
  integer, allocatable :: upgmax(:,:)
  integer, allocatable :: x_upgmax(:,:), y_upgmax(:,:)
  integer, allocatable :: upstream2(:,:)
  integer, allocatable :: upg_dif(:,:)
  integer, allocatable :: pass2(:,:)
  integer :: nx0, ny0
  real(8) :: west0, east0, south0, north0
  integer :: nx, ny, gxs, gxe, gys, gye
  integer :: xs, xe, ys, ye
  integer :: ix, iy, x, y, xx, yy
  real(8) :: cellsize
  real(8) :: west, east, south, north
  integer :: ix2, iy2, xx2, yy2
  real(8) :: cellsize2
  integer :: corner
  integer :: k

  integer :: dgt_xy2, dgt_gxy2

!  integer :: x2_debug = 2124
!  integer :: y2_debug = 1892
!  logical :: debug

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Modify the domain
  ! -- Make ($gxs-1) and $gxe are multiples of $ratio
  !-------------------------------------------------------------
  call logent('Modifying the domain')

  nx0 = gxe0 - gxs0 + 1
  ny0 = gye0 - gys0 + 1

  west0 = west_of_gx(gxs0)
  east0 = east_of_gx(gxe0)
  south0 = south_of_gy(gye0)
  north0 = north_of_gy(gys0)

  gxs = ((gxs0-1) / ratio) * ratio + 1
  gxe = ((gxe0-1) / ratio + 1) * ratio
  gys = ((gys0-1) / ratio) * ratio + 1
  gye = ((gye0-1) / ratio + 1) * ratio

  nx = gxe - gxs + 1
  ny = gye - gys + 1

  xs = gxs0 - gxs + 1
  xe = xs + (gxe0 - gxs0 + 1) - 1
  ys = gys0 - gys + 1
  ye = ys + (gye0 - gys0 + 1) - 1

  west = west_of_gx(gxs)
  east = east_of_gx(gxe)
  south = south_of_gy(gye)
  north = north_of_gy(gys)

  cellsize = GRIDSIZE_LON

  call logmsg('Original'//&
    '\n  nx: '//str(nx0)//' ('//str((/gxs0,gxe0/),DGT_GXY,' - ')//')'//&
    '\n  ny: '//str(ny0)//' ('//str((/gys0,gye0/),DGT_GXY,' - ')//')'//&
    '\n  BBox: '//sBBox(west0,east0,south0,north0))
  call logmsg('Modified'//&
    '\n  nx: '//str(nx)//' ('//str((/gxs,gxe/),DGT_GXY,' - ')//')'//&
      ' margin left: '//str(xs-1)//' right: '//str(nx-xe)//&
    '\n  ny: '//str(ny)//' ('//str((/gys,gye/),DGT_GXY,' - ')//')'//&
      ' margin top: '//str(ys-1)//' bottom: '//str(ny-ye)//&
    '\n  BBox: '//sBBox(west,east,south,north))

  call logext()
  !-------------------------------------------------------------
  ! Load topography data
  !-------------------------------------------------------------
  call logent('Loading topography data')

  allocate(elv(nx,ny))
  allocate(fdr(nx,ny))
  allocate(upg(nx,ny))

  elv(:,:) = ELV_MISS
  fdr(:,:) = FDR__MISS
  upg(:,:) = UPG_MISS
  call logmsg('Reading '//str(f_elv))
  call logmsg('Reading '//str(f_fdr))
  call logmsg('Reading '//str(f_upg))
  call traperr( rbin(elv(xs:xe,ys:ye), f_elv, dtype=DTYPE_REAL) )
  call traperr( rbin(fdr(xs:xe,ys:ye), f_fdr, dtype=DTYPE_INT1) )
  call traperr( rbin(upg(xs:xe,ys:ye), f_upg) )

  !k = all_sources_reach_outlet(fdr, STDERR) 
  !if( k /= 0 )then
  !  call errend(ERROR_CODE__CONDITION, 'Error('//str(k)//&
  !    ') There is a source that does not reach any outlet.')
  !endif

  call logext()
  !-------------------------------------------------------------
  ! Set the domain of the new resolution
  !-------------------------------------------------------------
  call logent('Setting the domain of the new resolution')

  if( ratio >= 9 )then
    corner = 3
  elseif( ratio >= 6 )then
    corner = 2
  elseif( ratio >= 3 )then
    corner = 1
  else
    corner = 0
  endif

  nx2 = nx / ratio
  ny2 = ny / ratio
  gxs2 = (gxs-1) / ratio + 1
  gxe2 = gxe / ratio
  gys2 = (gys-1) / ratio + 1
  gye2 = gye / ratio

  dgt_xy2 = dgt(max(nx2, ny2))
  dgt_gxy2 = dgt(max(ngx/ratio,ngy/ratio))

  cellsize2 = cellsize * ratio
  west2 = west
  east2 = east
  south2 = south
  north2 = north

  allocate(left(nx2), right(nx2), top(ny2), bottom(ny2))
  allocate(tile(nx,ny))
  allocate(upgmax(nx2,ny2))
  allocate(x_upgmax(nx2,ny2), y_upgmax(nx2,ny2))

  allocate(upstream2(nx2,ny2), upg_dif(nx,ny))
  allocate(pass2(nx2,ny2))

  allocate(elv2(nx2,ny2))
  allocate(fdr2(nx2,ny2))
  allocate(upg2(nx2,ny2))

  do ix2 = 1, nx2
    left(ix2) = (ix2-1) * ratio + 1
    right(ix2) = ix2 * ratio
  enddo

  do iy2 = 1, ny2
    top(iy2) = (iy2-1) * ratio + 1
    bottom(iy2) = iy2 * ratio
  enddo

  call logmsg('nx: '//str(nx2)//' ('//str((/gxs2,gxe2/),dgt_gxy2,' - ')//')')
  call logmsg('ny: '//str(ny2)//' ('//str((/gys2,gye2/),dgt_gxy2,' - ')//')')
  call logmsg('(BBox is unchanged)')

  call logext()
  !-------------------------------------------------------------
  ! Make tile mask
  !-------------------------------------------------------------
  call logent('Making a tile mask')

  tile(:,:) = 1

  ! Cutoff corners
  if( corner >= 1 )then
    do iy2 = 1, ny2
    do ix2 = 1, nx2
      ! top left
      tile(left(ix2):left(ix2)+corner-1, top(iy2):top(iy2)+corner-1) = 0

      ! top right
      tile(right(ix2)-corner+1:right(ix2), top(iy2):top(iy2)+corner-1) = 0

      ! bottom left
      tile(left(ix2):left(ix2)+corner-1, bottom(iy2)-corner+1:bottom(iy2)) = 0

      ! bottom right
      tile(right(ix2)-corner+1:right(ix2), bottom(iy2)-corner+1:bottom(iy2)) = 0
    enddo  ! ix2
    enddo  ! iy2
  endif

  call logext()
  !-------------------------------------------------------------
  ! Find the largest upg
  !-------------------------------------------------------------
  call logent('Finding the largest upg')

  upgmax(:,:) = 0
  x_upgmax(:,:) = 0
  y_upgmax(:,:) = 0
  do iy2 = 1, ny2
  do ix2 = 1, nx2
    k = 0
    do iy = top(iy2), bottom(iy2)
    do ix = left(ix2), right(ix2)
      if( elv(ix,iy) == ELV_MISS ) cycle
      if( tile(ix,iy) == 0 ) cycle

      if( upg(ix,iy) >= upgmax(ix2,iy2) )then
        upgmax(ix2,iy2) = upg(ix,iy)
        x_upgmax(ix2,iy2) = ix
        y_upgmax(ix2,iy2) = iy
      endif
    enddo  ! ix
    enddo  ! iy
  enddo  ! ix2
  enddo  ! iy2

  call logext()
  !-------------------------------------------------------------
  ! Find the neighbour
  !-------------------------------------------------------------
  call logent('Finding the neighbour')

  fdr2(:,:) = FDR__MISS
  do iy2 = 1, ny2
  do ix2 = 1, nx2
!debug = ix2 == x2_debug .and. iy2 == y2_debug
    !if( elv2(ix2,iy2) == ELV_MISS ) cycle
    ix = x_upgmax(ix2,iy2)
    iy = y_upgmax(ix2,iy2)
!if( debug )then
!  call logmsg('('//str((/ix2,iy2/),',')//&
!    ') upgmax ('//str((/ix,iy/),',')//') fdr '//str(fdr(ix,iy)))
!endif
    if( ix == 0 ) cycle

    if( is_fdr_outlet(fdr(ix,iy)) )then
      fdr2(ix2,iy2) = fdr(ix,iy)
      cycle
    endif

    x = ix
    y = iy
!if( debug )then
!  call logmsg('   xy('//str((/x,y/),',')//') xy2('//str((/ix2,iy2/),',')//') fdr '//str(fdr(x,y)))
!endif
    do while( is_fdr_direction(fdr(x,y)) )
      call get_nextxy(x, y, fdr(x,y), xx, yy)
      xx2 = (xx-1) / ratio + 1
      yy2 = (yy-1) / ratio + 1
!if( debug )then
!  call logmsg('-> xy('//str((/xx,yy/),',')//') xy2('//str((/xx2,yy2/),',')//') fdr '//str(fdr(xx,yy)))
!endif
      if( tile(xx,yy) == 1 )then
        ! Reached neighbour grid
        if( xx2 /= ix2 .or. yy2 /= iy2 )then
!if( debug )then
!  call logmsg('xy('//str((/xx,yy/),',')//') fdr '//str(fdr(xx,yy)))
!endif
          exit
        endif
      endif

      x = xx
      y = yy
    enddo
!if( debug )then
!  call logmsg('neighbour ('//str((/xx2,yy2/),',')//') fdr '//str(fdr2(xx2,yy2))//&
!    ' upgmax('//str((/x_upgmax(xx2,yy2),y_upgmax(xx2,yy2)/),',')//')')
!endif
    !-----------------------------------------------------------
    ! Update fdr
    !-----------------------------------------------------------
    if( tile(xx,yy) == 1 )then
      !---------------------------------------------------------
      ! Case: Reached the neighbour grid
      if( xx2 /= ix2 .or. yy2 /= iy2 )then
        fdr2(ix2,iy2) = get_fdr(ix2, iy2, xx2, yy2)
      !---------------------------------------------------------
      ! Case: Reached the end of the river
      elseif( .not. is_fdr_direction(fdr(x,y)) )then
        selectcase( fdr(x,y) )
        case( FDR__RIVERMOUTH, FDR__INLAND )
          fdr2(ix2,iy2) = fdr(x,y)
        case( FDR__MISS )
          call errend(ERROR_CODE__CONDITION, &
            'Unexpectedly reached the ocean grid. source: ('//str((/ix2,iy2/),',')//')')
        case default
          call errend(ERROR_CODE__VALUE, &
            'fdr == '//str(fdr(x,y)))
        endselect
      endif
    !-----------------------------------------------------------
    ! Case: tile == 0
    ! - tile = 0 の領域にある流出グリッドに到達
    else
      if( .not. is_fdr_outlet(fdr(x,y)) )then
        call errend(ERROR_CODE__CONDITION, &
          'Routing unexpectedly stopped. '//&
        '\nsrc xy2('//str((/ix2,iy2/),',')//')'//&
        '\ndst xy2('//str((/xx2,yy2/),',')//') fdr '//str(fdr(x,y)))
      endif

      fdr2(ix2,iy2) = fdr(x,y)
    !-----------------------------------------------------------
    endif
  enddo  ! ix2
  enddo  ! iy2

  call logext()
  !-------------------------------------------------------------
  ! Calc. elv
  !-------------------------------------------------------------
  call logent('Calculating elevtn')

  elv2(:,:) = 0.d0
  do iy2 = 1, ny2
  do ix2 = 1, nx2
    if( .not. is_fdr_land( fdr2(ix2,iy2) ) )then
      elv2(ix2,iy2) = ELV_MISS
      cycle
    endif

    k = 0
    do iy = top(iy2), bottom(iy2)
    do ix = left(ix2), right(ix2)
      if( elv(ix,iy) == ELV_MISS ) cycle

      call add(elv2(ix2,iy2), elv(ix,iy))
      call add(k)
      if( upg(ix,iy) >= upgmax(ix2,iy2) )then
        upgmax(ix2,iy2) = upg(ix,iy)
        x_upgmax(ix2,iy2) = ix
        y_upgmax(ix2,iy2) = iy
      endif
    enddo  ! ix
    enddo  ! iy

    if( k == 0 )then
      call errend(ERROR_CODE__CONDITION, 'k=='//str(k))
    endif

    call div(elv2(ix2,iy2), k)
    !if( k >= ratio**2 / 2 )then
    !  call div(elv2(ix2,iy2), k)
    !else
    !  elv2(ix2,iy2) = ELV_MISS
    !endif
  enddo  ! ix2
  enddo  ! iy2

  call logext()
  !-------------------------------------------------------------
  ! Validate the results
  !-------------------------------------------------------------
  call logent('Validating the results')

  do iy2 = 1, ny2
  do ix2 = 1, nx2
    if( elv2(ix2,iy2) == ELV_MISS .neqv. fdr2(ix2,iy2) == FDR__MISS )then
      call errend(ERROR_CODE__CONDITION, &
        'Inconsistency between missing values of `elv2` and `fdr2`.'//&
      '\n(x2,y2)=('//str((/ix2,iy2/),',')//'), elv2='//str(elv2(ix2,iy2))//&
        ', fdr2='//str(fdr2(ix2,iy2)))
    endif
  enddo  ! ix2
  enddo  ! iy2

  k = all_sources_reach_outlet(fdr2, STDERR) 
  if( k /= 0 )then
    call errend(ERROR_CODE__CONDITION, 'Error('//str(k)//&
      ') There is a source that does not reach any outlet.')
  endif

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
end subroutine scaleUp__core
!===============================================================
!
!===============================================================
end module mod_topo
