module mod_make_model_data
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_util
  use c2_nlni_const, only: &
    DGT_WSCODE
  use c2_strnk_const, only: &
    DGT_NWKID
  use c2_strnk_type, only: &
    cmn_node_   , &
    cmn_channel_, &
    cmn_watsys_ , &
    cmn_nwknode_, &
    cmn_network_
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: convBin2RRI
  public :: makeModelNetworkData
  public :: makeModelCrossSectionData
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_make_model_data'

  type, extends(cmn_node_) :: node_
    logical :: is_outlet
  end type

  type edge_
    real(8) :: pos
    logical :: is_outlet
    real(8) :: lon, lat
    real(8) :: downleng
  end type

  type section_
    integer :: n
    integer, pointer :: jPt(:)
    type(edge_) :: edge(2)
    real(8), pointer :: lon(:), lat(:)
    real(8) :: leng
  end type

  type, extends(cmn_channel_) ::  channel_
    logical :: is_valid
    type(node_), pointer :: node(:)  !(2)
    integer :: nSec
    type(section_), pointer :: section(:)  !(nSec)
  end type

  type, extends(cmn_watsys_) :: watsys_
  end type

  type, extends(cmn_nwknode_) :: nwknode_
    integer :: gx, gy
    integer :: iCh_down
    integer :: iNode_down
  end type

  type, extends(cmn_network_) :: network_
    type(watsys_), pointer :: wsys(:)
    type(channel_), pointer :: channel(:)
    type(nwknode_), pointer :: node(:)
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  real(8), parameter :: RATIO_CUT_TAIL = 0.1d0
  real(8), parameter :: LENG_CUT_MIN = 1.d2  ! [m]
  real(8), parameter :: THRESH_RATIO_CUT = 0.5d0
  real(8), parameter :: RATIO_EFFECTIVE_LENG = 0.5d0
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine convBin2RRI(&
  catmType, resl, id, var, overwrite &
)
  use c3_joint_const
  use c3_jflw_const, &
    jflw_set_resolution => set_resolution
  use c3_jflw_util, only: &
    jflw_get_id_range
  use c3_jflw_io, only: &
    jflw_strid                      , &
    jflw_read_basin_domain_from_each, &
    jflw_get_f_map_basin
  use c3_rri_const
  use c3_rri_io, only: &
    rri_get_f_data
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'convBin2RRI'
  character(*), intent(in) :: catmType
  character(*), intent(in) :: resl
  character(*), intent(in) :: id
  character(*), intent(in) :: var
  logical, intent(in) :: overwrite

  character(len_trim(var)) :: var_main, var_opt
  integer(1), pointer :: dati1(:,:)
  integer(4), pointer :: dati4(:,:)
  real(4)   , pointer :: datr4(:,:)
  integer(1) :: missi1
  integer(4) :: missi4
  real(4)    :: missr4
  integer :: nx, ny, gxs, gxe, gys, gye
  real(8) :: west, east, south, north
  logical :: is_done
  character(32) :: id_
  integer :: id_int, idmin_int, idmax_int
  character(CLEN_PATH) :: f, fout
  character(CLEN_KEY) :: dtype

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( index(var,'_') == 0 )then
    var_main = var
    var_opt = ''
  else
    var_main = var(:index(var,'_')-1)
    var_opt = var(index(var,'_')+1:)
  endif

  selectcase( var_main )
  case( 'elv' )
    dtype = DTYPE_REAL
    missr4 = JFLW_ELV_MISS
  case( 'dir' )
    dtype = DTYPE_INT1
    missi1 = JFLW_FDR__MISS
  case( 'upg' )
    dtype = DTYPE_INT4
    missi4 = JFLW_UPG_MISS
  case( 'upa' )
    dtype = DTYPE_REAL
    missr4 = JFLW_UPA_MISS
  case default
    call errend(ERROR_CODE__VALUE, 'var_main == '//str(var_main)//&
      '(var == '//str(var)//')')
  endselect

  call jflw_set_resolution(resl)

  nullify(dati1, dati4, datr4)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( catmType )
  !-------------------------------------------------------------
  ! Case: J-FlwDir Basin
  case( CATMTYPE__BASIN )
    if( id == '' )then
      call errend(ERROR_CODE__VALUE, 'Id is missing.')
      
    elseif( index(id, '-') /= 0 )then
      call jflw_get_id_range(id, idmin_int, idmax_int)
      do id_int = idmin_int, idmax_int
        id_ = jflw_strid(id_int)
        call prep_basin(id_, is_done)
        if( is_done ) cycle
        call core()
      enddo

    else
      call prep_basin(id, is_done)
      if( .not. is_done )then
        call core()
      endif
    endif

  !-------------------------------------------------------------
  ! Case: ERROR
  case default

  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(dati1, 0)
  call realloc(dati4, 0)
  call realloc(datr4, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine prep_basin(id, is_done)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_basin'
  character(*), intent(in) :: id
  logical, intent(out) :: is_done

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  fout = rri_get_f_data(catmType, resl, var, id)

  is_done = .not. overwrite .and. file_exists(fout)
  if( is_done )then
    call logmsg('Output file already exists: '//str(fout))
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  call jflw_read_basin_domain_from_each(&
    resl, id, &
    nx, ny, gxs, gxe, gys, gye, &
    west, east, south, north, &
    echo=.true. &
  )

  f = jflw_get_f_map_basin(resl, id, var)
  call logmsg('Reading '//str(f))

  selectcase( dtype )
  case( DTYPE_INT1 )
    call realloc(dati1, (/gxs,gys/), (/gxe,gye/))
    call traperr( rbin(dati1, f) )
  case( DTYPE_INT4 )
    call realloc(dati4, (/gxs,gys/), (/gxe,gye/))
    call traperr( rbin(dati4, f) )
  case( DTYPE_REAL )
    call realloc(datr4, (/gxs,gys/), (/gxe,gye/))
    call traperr( rbin(datr4, f) )
  case default
    call errend(ERROR_CODE__VALUE, 'dtype == '//str(dtype))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prep_basin
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine core()
  implicit none

  selectcase( dtype )
  !-------------------------------------------------------------
  ! Case: int1 (fdr)
  case( DTYPE_INT1 )
    call convBin2RRI__core__fdr(&
      fout, overwrite, &
      nx, ny, west, south, JFLW_GRIDSIZE_LON, &
      dati1, missi1 &
    )
  !-------------------------------------------------------------
  ! Case: int4 (upg)
  case( DTYPE_INT4 )
    call convBin2RRI__core__int4(&
      var, fout, overwrite, &
      nx, ny, west, south, JFLW_GRIDSIZE_LON, &
      dati4, missi4 &
    )
  !-------------------------------------------------------------
  ! Case: real (elv, upa)
  case( DTYPE_REAL )
    call convBin2RRI__core__real(&
      var, fout, overwrite, &
      nx, ny, west, south, JFLW_GRIDSIZE_LON, &
      datr4, missr4 &
    )
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(ERROR_CODE__VALUE, 'dtype == '//str(dtype))
  endselect
end subroutine core
!---------------------------------------------------------------
!
!---------------------------------------------------------------
end subroutine convBin2RRI
!===============================================================
!
!===============================================================
subroutine convBin2RRI__core__fdr(&
  f, overwrite, &
  nx, ny, west, south, cellsize, &
  dat, miss &
)
  use c3_joint_grid, only: &
    conv_fdr_jflw2rri
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'convBin2RRI__core__fdr'
  character(*), intent(in) :: f
  logical     , intent(in) :: overwrite
  integer     , intent(in) :: nx, ny
  real(8)     , intent(in) :: west, south
  real(8)     , intent(in) :: cellsize
  integer(1)  , intent(in) :: dat(:,:)
  integer(1)  , intent(in) :: miss

  integer(4) :: dat_rri(size(dat,1),size(dat,2))
  integer :: iy
  character(32) :: wfmt_val, wfmt
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( .not. overwrite .and. file_exists(f) )then
    call logmsg('Exists: '//str(f))
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  wfmt_val = RRI_FMT__FDR
  wfmt = '('//str(nx)//'(1x,'//str(wfmt_val)//'))'

  call conv_fdr_jflw2rri(dat, dat_rri)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  wfmt = '('//str(nx)//'(1x,'//str(RRI_FMT__FDR)//'))'

  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')

  write(un,"(a)") 'ncols '//str(nx)
  write(un,"(a)") 'nrows '//str(ny)
  write(un,"(a)") 'xllcorner '//str(west,RRI_FMT__LONLAT)
  write(un,"(a)") 'yllcorner '//str(south,RRI_FMT__LONLAT)
  write(un,"(a)") 'cellsize '//str(cellsize,RRI_FMT__CELLSIZE)
  write(un,"(a,1x,"//str(wfmt_val)//")") 'miss', miss
  do iy = 1, ny
    write(un,wfmt) dat(:,iy)
  enddo

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine convBin2RRI__core__fdr
!===============================================================
!
!===============================================================
subroutine convBin2RRI__core__int4(&
  var, f, overwrite, &
  nx, ny, west, south, cellsize, &
  dat, miss &
)
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'convBin2RRI__core__int4'
  character(*), intent(in) :: var
  character(*), intent(in) :: f
  logical     , intent(in) :: overwrite
  integer     , intent(in) :: nx, ny
  real(8)     , intent(in) :: west, south
  real(8)     , intent(in) :: cellsize
  integer(4)  , intent(in) :: dat(:,:)
  integer(4)  , intent(in) :: miss

  integer :: iy
  character(32) :: wfmt_val, wfmt
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( .not. overwrite .and. file_exists(f) )then
    call logmsg('Exists: '//str(f))
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( var )
  case( 'upg' )
    wfmt_val = RRI_FMT__UPG
  case default
    call errend(ERROR_CODE__VALUE, 'var == '//str(var))
  endselect

  wfmt = '('//str(nx)//'(1x,'//str(wfmt_val)//'))'
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')

  write(un,"(a)") 'ncols '//str(nx)
  write(un,"(a)") 'nrows '//str(ny)
  write(un,"(a)") 'xllcorner '//str(west,RRI_FMT__LONLAT)
  write(un,"(a)") 'yllcorner '//str(south,RRI_FMT__LONLAT)
  write(un,"(a)") 'cellsize '//str(cellsize,RRI_FMT__CELLSIZE)
  write(un,"(a,1x,"//str(wfmt_val)//")") 'miss', miss
  do iy = 1, ny
    write(un,wfmt) dat(:,iy)
  enddo

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine convBin2RRI__core__int4
!===============================================================
!
!===============================================================
subroutine convBin2RRI__core__real(&
  var, f, overwrite, &
  nx, ny, west, south, cellsize, &
  dat, miss &
)
  use c3_rri_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'convBin2RRI__core__real'
  character(*), intent(in) :: var
  character(*), intent(in) :: f
  logical     , intent(in) :: overwrite
  integer     , intent(in) :: nx, ny
  real(8)     , intent(in) :: west, south
  real(8)     , intent(in) :: cellsize
  real(4)     , intent(in) :: dat(:,:)
  real(4)     , intent(in) :: miss

  integer :: iy
  character(32) :: wfmt_val, wfmt
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( .not. overwrite .and. file_exists(f) )then
    call logmsg('Exists: '//str(f))
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( var )
  case( 'elv' )
    wfmt_val = RRI_FMT__ELV
  case( 'upa' )
    wfmt_val = RRI_FMT__UPA
  case default
    call errend(ERROR_CODE__VALUE, 'var == '//str(var))
  endselect

  wfmt = '('//str(nx)//'(1x,'//str(wfmt_val)//'))'
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')

  write(un,"(a)") 'ncols '//str(nx)
  write(un,"(a)") 'nrows '//str(ny)
  write(un,"(a)") 'xllcorner '//str(west,RRI_FMT__LONLAT)
  write(un,"(a)") 'yllcorner '//str(south,RRI_FMT__LONLAT)
  write(un,"(a)") 'cellsize '//str(cellsize,RRI_FMT__CELLSIZE)
  write(un,"(a,1x,"//str(wfmt_val)//")") 'miss', miss
  do iy = 1, ny
    write(un,wfmt) dat(:,iy)
  enddo

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine convBin2RRI__core__real
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
subroutine makeModelNetworkData(&
  productName, leng_standard, id &
)
  use c2_strnk_io, only: &
    get_f_lst_networks_channel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeModelNetworkData'
  character(*), intent(in) :: productName
  real(8)     , intent(in) :: leng_standard  ! [m]
  character(*), intent(in) :: id

  character(DGT_NWKID) :: id_
  integer :: nNwk, jNwk
  character(CLEN_PATH) :: f
  character :: c_
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( id == '' )then
    f = get_f_lst_networks_channel()
    call logmsg('Reading '//str(f))
    open(newunit=un, file=f, status='old')
    read(un,*) c_, nNwk
    read(un,*)
    do jNwk = 1, nNwk
      read(un,*) c_, id_
      call makeModelNetworkData__core(productName, leng_standard, id_)
    enddo  ! jNwk/
    close(un)
  else
    call makeModelNetworkData__core(productName, leng_standard, id)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeModelNetworkData
!===============================================================
!
!===============================================================
subroutine makeModelNetworkData__core(&
  productName, leng_standard, id &
)
  use c2_strnk_const
  use c2_strnk_io, only: &
    read_network         , &
    get_f_network_channel, &
    get_f_model_network
  use c2_strnk_util, only: &
    clear_cmn_network
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeModelNetworkData__core'
  character(*), intent(in) :: productName
  real(8), intent(in) :: leng_standard  ! [m]
  character(*), intent(in) :: id

  type(cmn_network_) :: cmnnwk
  type(network_) :: nwk
  type(channel_), pointer :: ch
  type(section_), pointer :: sec
  type(edge_), pointer :: edge
  integer :: iSec
  real(8) :: leng, leng_next, leng_prev, leng_acc, leng_section
  real(8) :: pos, pos_prev
  real(8) :: downleng
  integer :: jCh
  integer :: iCh
  integer :: jPt, jPt0, jjPt
  logical :: is_ok

  character(CLEN_PATH) :: f
  integer :: un

  real(8), parameter :: LENG_NEXT_LLIM = 1d-8
  real(8), parameter :: THRESH_DIST_BTW_POINTS = 1.d-3

logical :: debug_this

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logmsg('Network '//str(id))
  !-------------------------------------------------------------
  ! Load network data
  !-------------------------------------------------------------
  call logent('Loading network data')

  cmnnwk%id = id

  f = get_f_network_channel(cmnnwk%id, 'sbin')
  call logmsg('Reading '//str(f))
  call read_network(f, cmnnwk)

  call copy_cmn2nwk(cmnnwk, nwk)

  call clear_cmn_network(cmnnwk)

  ! Check if outlet exists
  !-------------------------------------------------------------
  is_ok = .false.
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    if( any(ch%node(:)%typ == NODETYPE_NEW__OUT) )then
      is_ok = .true.
      exit
    endif
  enddo  ! jCh/

  if( .not. is_ok )then
    call logmsg('Outlet was not found. Model network is not generated.')
    call logret(PRCNAM, MODNAM)
    return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Remove the points that are very close to the neighbour
  !-------------------------------------------------------------
  call logent('Removing points that are very close to the neighbour')

  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)
    ch%is_valid = .true.

    ch%leng = 0.d0
    do jPt = 1, ch%n-1
      call add(ch%leng, dist_earth(ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1)))
    enddo

    if( ch%leng < THRESH_DIST_BTW_POINTS )then
      !call erradd('ch%leng < THRESH (ch #'//str(jCh)//')')
      !do jPt = 1, ch%n-1
      !  leng = dist_earth(ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1))
      !  call errapd(str(jPt,dgt(ch%n))//' '//slonlat(ch%lon(jPt),ch%lat(jPt))//' - '//&
      !    slonlat(ch%lon(jPt+1),ch%lat(jPt+1))//' '//str(leng))
      !enddo
      !call errend('')

      ch%is_valid = .false.
      cycle
    endif

    jPt = 1
    do while( jPt < ch%n )
      leng = dist_earth(ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1))

      if( leng < THRESH_DIST_BTW_POINTS )then
        jPt0 = jPt  ! remove point #jPt
        if( jPt == 1 ) jPt0 = 2  ! remove point #2

        call logmsg('ch #'//str(jCh)//' leng='//str(leng)//&
          '. pt #'//str(jPt0)//' was removed')

        ch%lon(jPt0:ch%n-1) = ch%lon(jPt0+1:ch%n)
        ch%lat(jPt0:ch%n-1) = ch%lat(jPt0+1:ch%n)
        call add(ch%n, -1)
      else
        call add(jPt)
      endif
    enddo  ! jPt/

    if( ch%n < size(ch%lon) )then
      call realloc(ch%lon, ch%n, clear=.false.)
      call realloc(ch%lat, ch%n, clear=.false.)
    endif

    ch%leng = 0.d0
    do jPt = 1, ch%n-1
      call add(ch%leng, dist_earth(ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1)))
    enddo
  enddo  ! jCh/

  call logext()
  !-------------------------------------------------------------
  ! Divide channels
  !-------------------------------------------------------------
  call logent('Dividing channels into specified length')

  do jCh = 1, nwk%nCh
    debug_this = jCh == 0

    ch => nwk%channel(jCh)
    if( .not. ch%is_valid ) cycle

    if( debug_this )then
      call logmsg(ch%wsCode//' '//ch%rvCode//' '//ch%rvName)
    endif
    !-----------------------------------------------------------
    ! Calc. number of sections
    !-----------------------------------------------------------
    ch%nSec = int(ch%leng / leng_standard)

    if( ch%nSec == 0 )then
      ch%nSec = 1
    else
      if( abs(ch%leng / ch%nSec - leng_standard) > abs(ch%leng / (ch%nSec+1) - leng_standard) )then
        call add(ch%nSec)
      endif
    endif

    leng_section = ch%leng / ch%nSec

    if( debug_this )then
      leng_acc = 0.d0
      call logmsg('ch #'//str(jCh)//' nPt '//str(ch%n))
      do jPt = 1, ch%n-1
        leng = dist_earth(ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1))
        call add(leng_acc, leng)
        call logmsg(str(jPt,dgt(ch%n))//' '//slonlat(ch%lon(jPt),ch%lat(jPt))//' - '//&
          str(jPt+1,dgt(ch%n))//' '//slonlat(ch%lon(jPt+1),ch%lat(jPt+1))//' '//str(leng))
      enddo
      call logmsg('  leng '//str(ch%leng)//' leng_sum '//str(leng_acc)//&
        ' nSec '//str(ch%nSec)//' leng_section '//str(leng_section))

   
      call logmsg('lon '//str(ch%lon(:3),'f12.7',', ')//', ..., '//str(ch%lon(ch%n-2:),'f12.7',', '))
      call logmsg('lat '//str(ch%lat(:3),'f12.7',', ')//', ..., '//str(ch%lat(ch%n-2:),'f12.7',', '))
    endif

    allocate(ch%section(ch%nSec))
    do iSec = 1, ch%nSec
      sec => ch%section(iSec)
      sec%n = 0
      allocate(sec%jPt(ch%n))
    enddo  ! iSec/
    !-----------------------------------------------------------
    ! Determine points where the channel is divided
    !-----------------------------------------------------------
    iSec = 0
    jPt = 1
    pos = 0.d0
    leng_acc = 0.d0
    do while( jPt < ch%n )
      call add(iSec)
      sec => ch%section(iSec)
      sec%n = 1
      sec%jPt(1) = jPt
      sec%edge(1)%pos = pos
      sec%leng = 0.d0

      do while( jPt < ch%n )
        leng_next = dist_earth(ch%lon(jPt), ch%lat(jPt), ch%lon(jPt+1), ch%lat(jPt+1))

        if( debug_this )then
          call logmsg(str(jPt)//' pos '//str(pos,'f5.3')//&
            ' sum '//str(sec%leng)//' next '//str(leng_next)//&
            ' sum+next*(1-pos) '//str(sec%leng+leng_next*(1-pos)))
        endif

        if( sec%leng + leng_next*(1.d0-pos) > leng_section ) exit
        call add(sec%leng, leng_next*(1.d0-pos))
        call add(jPt)
        pos = 0.d0
        call add(sec%n)
        sec%jPt(sec%n) = jPt
      enddo  ! jPt/

      pos_prev = pos
      if( leng_next < LENG_NEXT_LLIM )then
        pos = 1.d0
      else
        pos = min(pos + min(max(leng_section - sec%leng, 0.d0) / leng_next, 1.d0), 1.d0)
      endif

      if( pos < 1d-8 )then
        pos = 0.d0
      elseif( 1.d0 - pos < 1d-8 )then
        pos = 1.d0
      endif

      if( jPt < ch%n )then
        call add(sec%n)
        sec%jPt(sec%n) = jPt
      endif
      sec%edge(2)%pos = pos

      leng_prev = sec%leng

      call add(sec%leng, leng_next*(pos-pos_prev))

      call add(leng_acc, sec%leng)

      if( debug_this )then
      !if( abs((sec%leng + leng_next*(pos-pos_prev)) - leng_section) > 1d-2 )then
        call logmsg('sec '//str(iSec)//' pt '//str(jPt)//&
          ' pos '//str(pos,'f5.3')//' pos_prev '//str(pos_prev,'f5.3')//&
        '\nsum '//str(leng_prev)//' + '//str(leng_next)//&
           ' x (pos-pos_prev) -> '//str(sec%leng)//&
        '\nerr '//str(sec%leng - leng_section)//' acc '//str(leng_acc))
      !endif
      endif

      if( pos == 1.d0 )then
        pos_prev = 0.d0
        pos = 0.d0
        call add(jPt)
      endif

      if( jPt < ch%n .and. iSec == ch%nSec )then
        call errend(ERROR_CODE__CONDITION, &
          'jPt < ch%n and iSec == ch%nSec'//&
        '\n  channel #'//str(jCh))
      endif
    enddo  ! jPt, iSec/
    !-----------------------------------------------------------
    ! Calc. coords. of edges
    !-----------------------------------------------------------
    if( debug_this )then
      call logmsg('ch '//slonlat(ch%lon(1),ch%lat(1))//' - '//slonlat(ch%lon(ch%n),ch%lat(ch%n)))
    endif
    do iSec = 1, ch%nSec
      sec => ch%section(iSec)
      call realloc(sec%jPt, sec%n, clear=.false.)

      jPt = sec%jPt(1)
      edge => sec%edge(1)
      edge%lon = ch%lon(jPt)*(1.d0-edge%pos) + ch%lon(jPt+1)*edge%pos
      edge%lat = ch%lat(jPt)*(1.d0-edge%pos) + ch%lat(jPt+1)*edge%pos

      jPt = sec%jPt(sec%n)
      edge => sec%edge(2)
      if( jPt == ch%n )then
        edge%lon = ch%lon(jPt)
        edge%lat = ch%lat(jPt)
      else
        edge%lon = ch%lon(jPt)*(1.d0-edge%pos) + ch%lon(jPt+1)*edge%pos
        edge%lat = ch%lat(jPt)*(1.d0-edge%pos) + ch%lat(jPt+1)*edge%pos
      endif

      if( debug_this )then
        call logmsg('sec('//str(iSec,dgt(ch%nSec))//') pt '//str(sec%n+2)//' '//&
          str(sec%edge(1)%pos,'f5.3')//' '//str(sec%jPt(1))//' '//&
          slonlat(sec%edge(1)%lon,sec%edge(1)%lat)//' - '//&
          str(sec%edge(2)%pos,'f5.3')//' '//str(sec%jPt(sec%n))//' '//&
          slonlat(sec%edge(2)%lon,sec%edge(2)%lat))
      endif
      !---------------------------------------------------------
      ! Put coords. of points
      !---------------------------------------------------------
      allocate(sec%lon(sec%n))
      allocate(sec%lat(sec%n))
      sec%lon(1) = sec%edge(1)%lon
      sec%lat(1) = sec%edge(1)%lat
      do jjPt = 2, sec%n-1
        sec%lon(jjPt) = ch%lon(sec%jPt(jjPt))
        sec%lat(jjPt) = ch%lat(sec%jPt(jjPt))
      enddo
      sec%lon(sec%n) = sec%edge(2)%lon
      sec%lat(sec%n) = sec%edge(2)%lat

      sec%leng = 0.d0
      do jjPt = 1, sec%n-1
        call add(sec%leng, dist_earth(&
          sec%lon(jjPt), sec%lat(jjPt), sec%lon(jjPt+1), sec%lat(jjPt+1) &
        ))
      enddo

      if( debug_this )then
        call logmsg('recalculated leng: '//str(sec%leng))
      endif
    enddo  ! iSec/
    !-----------------------------------------------------------
    ! Determine whether edges are outlet or not
    !-----------------------------------------------------------
    do iSec = 1, ch%nSec
      ch%section(iSec)%edge(:)%is_outlet = .false.
    enddo
    ch%section(1)%edge(1)%is_outlet = ch%node(1)%typ == NODETYPE_NEW__OUT
    ch%section(ch%nSec)%edge(2)%is_outlet = ch%node(2)%typ == NODETYPE_NEW__OUT
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( debug_this )then
      call logmsg('node%downleng: '//str(ch%node(:)%downleng)//&
         '\nsum(ch%section(:)%leng): '//str(sum(ch%section(:)%leng))//&
         '\nch%leng                : '//str(ch%leng))
    endif
    downleng = ch%node(2)%downleng

    do iSec = ch%nSec, 1, -1
      sec => ch%section(iSec)

      sec%edge(2)%downleng = downleng

      call add(downleng, sec%leng)

      sec%edge(1)%downleng = downleng
    enddo  ! iSec/

    ! Case: Nodes takes different shortest routes to the outlet
    ! e.g., network 0010002001 channel 79
    !sec => ch%section(1)
    !if( sec%edge(1)%downleng - ch%node(1)%downleng > 1d-2 )then
    !  call logmsg('ch%section(1)%edge(1)%downleng - ch%node(1)%downleng > 1d-2'//&
    !    '\n  channel #'//str(jCh))
    !endif
  enddo  ! jCh/

  call logmsg('Sections: '//str(sum(nwk%channel(:)%nSec)))
  call logmsg('Mean section length: '//str(sum(nwk%channel(:)%leng)/sum(nwk%channel(:)%nSec)))

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f = get_f_model_network(productName, nwk%id)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')

  write(un,"(a)") 'channel '//str(sum(nwk%channel(:)%nSec))

  iCh = 0
  do jCh = 1, nwk%nCh
    ch => nwk%channel(jCh)

    if( .not. ch%is_valid ) cycle

    do iSec = 1, ch%nSec
      sec => ch%section(iSec)
      call add(iCh)

      write(un,"(a)") '  channel '//str(iCh)
      write(un,"(a)") '  index_original '//str(jCh)
      write(un,"(a)") '  node_outlet '//str(sec%edge(:)%is_outlet)
      write(un,"(a)") '  distToMouth '//str(sec%edge(:)%downleng,'f10.2')
      write(un,"(a)") '  points '//str(sec%n)
      write(un,"(a)") '  lon '//str(sec%lon,'f12.7')
      write(un,"(a)") '  lat '//str(sec%lat,'f12.7')
    enddo  ! iSec/
  enddo  ! jCh/

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeModelNetworkData__core
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
subroutine makeModelCrossSectionData(&
  name_network, name_crosssection, id_in &
)
  use c2_strnk_io, only: &
    read_network_size         , &
    get_f_network_channel     , &
    get_f_network_crosssection, &
    get_f_model_network       , &
    get_f_model_crosssection
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeModelCrossSectionData'
  character(*), intent(in) :: name_network
  character(*), intent(in) :: name_crosssection
  character(*), intent(in) :: id_in

  integer :: nCh, jCh
  integer :: mCh, iCh
  real(8), allocatable :: width_org(:), hight_org(:), depth_org(:), levee_org(:)
  real(8), allocatable :: width(:), hight(:), depth(:), levee(:)

  character(CLEN_PATH) :: f
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( id_in == '' )then

    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! Load cross section data of original channels
  !-------------------------------------------------------------
  call logent('Loading cross section data of original channels')

  f = get_f_network_channel(id_in, 'sbin')
  call read_network_size(f, nCh=nCh)

  allocate(width_org(nCh))
  allocate(hight_org(nCh))
  allocate(depth_org(nCh))
  allocate(levee_org(nCh))

  f = get_f_network_crosssection(name_crosssection, 'width', id_in)
  call traperr( rbin(width_org, f) )

  f = get_f_network_crosssection(name_crosssection, 'height', id_in)
  call traperr( rbin(hight_org, f) )

  f = get_f_network_crosssection(name_crosssection, 'depth', id_in)
  call traperr( rbin(depth_org, f) )

  f = get_f_network_crosssection(name_crosssection, 'levee', id_in)
  call traperr( rbin(levee_org, f) )

  call logext()
  !-------------------------------------------------------------
  ! Make river cross section data of model channels
  !-------------------------------------------------------------
  call logent('Making river cross section data of model channels')

  f = get_f_model_network(name_network, id_in)
  open(newunit=un, file=f, status='old')
  read(un,*) c_, mCh

  allocate(width(mCh))
  allocate(hight(mCh))
  allocate(depth(mCh))
  allocate(levee(mCh))

  do iCh = 1, mCh
    read(un,*)   ! index
    read(un,*) c_, jCh
    read(un,*)   ! is_outlet
    read(un,*)   ! downleng
    read(un,*)   ! nPt
    read(un,*)   ! lon
    read(un,*)   ! lat

    width(iCh) = width_org(jCh)
    hight(iCh) = hight_org(jCh)
    depth(iCh) = depth_org(jCh)
    levee(iCh) = levee_org(jCh)
  enddo  ! jCh/

  close(un)

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Saving')

  f = get_f_model_crosssection(name_network, name_crosssection, 'width', id_in)
  call logmsg('Writing '//str(f))
  call traperr( wbin(width, f, replace=.true.) )

  f = get_f_model_crosssection(name_network, name_crosssection, 'height', id_in)
  call logmsg('Writing '//str(f))
  call traperr( wbin(hight, f, replace=.true.) )

  f = get_f_model_crosssection(name_network, name_crosssection, 'depth', id_in)
  call logmsg('Writing '//str(f))
  call traperr( wbin(depth, f, replace=.true.) )

  f = get_f_model_crosssection(name_network, name_crosssection, 'levee', id_in)
  call logmsg('Writing '//str(f))
  call traperr( wbin(levee, f, replace=.true.) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeModelCrossSectionData  
!===============================================================
!
!===============================================================
!
!
!
!
!
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine read_channels_model(f, nCh, channel)
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'read_channels_model'
  character(*), intent(in) :: f
  integer :: nCh
  type(channel_), pointer :: channel(:)  !(nCh)

  type(channel_), pointer :: ch
  integer :: jCh

  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=f, status='old')

  read(un,*) c_, nCh

  allocate(channel(nCh))

  do jCh = 1, nCh
    ch => channel(jCh)

    read(un,*) 

    read(un,*) ch%node(:)%is_outlet

    read(un,*) ch%n
    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))
    read(un,*) ch%lon
    read(un,*) ch%lat
  enddo  ! jCh/

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_channels_model
!===============================================================
!
!===============================================================
subroutine copy_cmn2nwk(cmn, nwk)
  implicit none
  type(cmn_network_), intent(in) :: cmn
  type(network_), intent(out) :: nwk

  type(cmn_watsys_), pointer :: cwsys
  type(cmn_channel_), pointer :: cch
  type(cmn_node_), pointer :: cnode
  type(watsys_), pointer :: wsys
  type(channel_), pointer :: ch
  type(node_), pointer :: node
  integer :: jWsys
  integer :: jCh
  integer :: jNode

  allocate(character(1) :: nwk%id)
  nwk%id = cmn%id

  nwk%nWsys = cmn%nWsys
  allocate(nwk%wsys(nwk%nWsys))
  do jWsys = 1, nwk%nWsys
    cwsys => cmn%wsys_(jWsys)
    wsys => nwk%wsys(jWsys)

    allocate(character(1) :: wsys%wsCode)
    wsys%wsCode = cwsys%wsCode

    wsys%leng = cwsys%leng
  enddo  ! jWsys/

  nwk%nCh = cmn%nCh
  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    cch => cmn%channel_(jCh)
    ch => nwk%channel(jCh)

    allocate(character(1) :: ch%wsCode)
    allocate(character(1) :: ch%rvCode)
    allocate(character(1) :: ch%rvName)
    ch%wsCode = cch%wsCode
    ch%rvCode = cch%rvCode
    ch%rvName = cch%rvName

    ch%n = cch%n
    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))
    ch%lon(:) = cch%lon(:)
    ch%lat(:) = cch%lat(:)

    allocate(ch%node(2))
    do jNode = 1, 2
      cnode => cch%node_(jNode)
      node => ch%node(jNode)
      node%lon = cnode%lon
      node%lat = cnode%lat
      node%typ = cnode%typ
      node%elv = cnode%elv
      node%downleng = cnode%downleng
      node%iNode = cnode%iNode
    enddo  ! jNode/
  enddo  ! jCh/
end subroutine copy_cmn2nwk
!===============================================================
!
!===============================================================
end module mod_make_model_data
