module mod_basin
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
  use c2_strnk_const, only: &
    DGT_NWKUID
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: trimBasin
  public :: modifyTopography
  !-------------------------------------------------------------
  ! Private module variables (type)
  !-------------------------------------------------------------
  type, extends(cmn_node_) :: node_
    logical :: is_outlet
  end type

  type, extends(cmn_channel_) :: channel_
    type(node_), pointer :: node(:)
    real(8) :: west, east, south, north
  end type

  type, extends(cmn_watsys_) :: watsys_
  end type

  type, extends(cmn_network_) :: network_
    type(watsys_), pointer :: wsys(:)
    type(channel_), pointer :: channel(:)
    real(8) :: west, east, south, north
    integer :: gxs, gxe, gys, gye
    integer, pointer :: jCh(:)
  end type

  type chpix_
    integer :: n
    integer, pointer :: gx(:), gy(:)
    real(8), pointer :: leng(:)
    integer :: gxs, gxe, gys, gye
  end type

  type nwkattr_
    character(DGT_NWKUID) :: uid
    integer :: nCh
    real(8) :: leng
    real(8) :: west, east, south, north
    type(chpix_) :: chpix
  end type
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_basin'
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
subroutine trimBasin(&
  basinType, resl, uid, varName, outfmt, overwrite &
)
  use c3_jflw_const, &
        jflw_set_resolution => jflw_set_resolution
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'trimBasin'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: uid
  character(*), intent(in) :: outfmt
  logical     , intent(in) :: overwrite

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call jflw_set_resolution(resl)

  call trim_basin(&
    basinType, resl, uid, varName, outfmt, overwrite &
  )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine trimBasin
!===============================================================
!
!===============================================================
subroutine trim_basin(&
  basinType, resl, uid, varName, outfmt, overwrite &
)
  use c3_jflw_const
  use c3_jflw_io, only: &
    jflw_intId                      , &
    jflw_get_f_map_basin            , &
    jflw_read_basin_domain_from_each, &
    jflw_read_map_from_tile         , &
    jflw_read_basin_map_from_tile
  use c3_strnk_io, only: &
    strnk_get_f_network_mesh      , &
    strnk_read_network_mesh_domain
  use c3_rri_io, only: &
    rri_get_f_data, &
    rri_read_map  , &
    rri_write_map
  use c3_joint_const
  use c3_joint_util, only: &
    joint_conv_fdr_jflw2rri => conv_fdr_jflw2rri, &
    joint_get_miss          => get_miss
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'trim_basin'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid
  character(*), intent(in) :: varName
  character(*), intent(in) :: outfmt
  logical     , intent(in) :: overwrite

  integer(1), allocatable :: i1map(:,:)
  integer(4), allocatable :: i4map(:,:)
  real(4)   , allocatable :: r4map(:,:)
  logical(1), allocatable :: mskmap(:,:)
  integer(1) :: i1miss
  integer(4) :: i4miss
  real(4)    :: r4miss
  integer :: gxs, gxe, gys, gye
  real(8) :: west, east, south, north
  integer :: igx, igy
  character(CLEN_VAR) :: inputDataName
  character(CLEN_KEY) :: dtype
  character(CLEN_PATH) :: f_msk, f_var

  character(CLEN_KEY), parameter :: OUTFMT__DEFAULT = 'default'
  character(CLEN_KEY), parameter :: OUTFMT__RRI     = 'rri'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Setup
  !-------------------------------------------------------------
  selectcase( outfmt )
  case( OUTFMT__DEFAULT )
    inputDataName = DATANAME__JFLW

    selectcase( basinType )
    case( BASINTYPE__FLWDIR )
      f_var = jflw_get_f_map_basin(resl, varName, uid)
    case( BASINTYPE__NETWORK )
      f_var = strnk_get_f_network_mesh(resl, varName, uid)
    case( BASINTYPE__NETWORKSET )
      call errend(msg_not_implemented()//&
        '\n  basinType: '//str(basinType))
    case default
      call errend(msg_invalid_value('basinType', basinType))
    endselect
  case( OUTFMT__RRI )
    inputDataName = DATANAME__RRI

    f_var = rri_get_f_data(basinType, resl, varName, uid)
  case default
    call errend(msg_invalid_value('outfmt', outfmt))
  endselect

  if( .not. overwrite .and. access(f_var,' ') == 0 )then
    call logmsg('File already exists: '//str(f_var))
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! Make a mask
  !-------------------------------------------------------------
  selectcase( basinType )
  !-------------------------------------------------------------
  ! Case: J-FlwDir basin map
  case( BASINTYPE__FLWDIR )
    call jflw_read_basin_domain_from_each(&
        resl, uid, &
        gxs, gxe, gys, gye, west, east, south, north)

    call logmsg('Basin '//str(uid))
    call logmsg('(x,y): ('//str((/gxe-gxs+1,gye-gys+1/),JFLW_DGT_GXY,',')//&
        ') ['//str((/gxs,gxe/),JFLW_DGT_GXY,':')//','//&
        str((/gys,gye/),JFLW_DGT_GXY,':')//']')
    call logmsg('BBox: '//sBBox(west,east,south,north))

    allocate(mskmap(gxs:gxe,gys:gye))

    if( resl == RESOLUTION_1SEC )then
      allocate(i4map(gxs:gxe,gys:gye))
      call jflw_read_map_from_tile(&
        resl, 'bsn', DTYPE_INT4, JFLW_BSN_MISS, gxs, gys, i4map)
      where( i4map == jflw_intId(uid) )
        mskmap = .true.
      elsewhere
        mskmap = .false.
      endwhere
      deallocate(i4map)
    else
      allocate(r4map(gxs:gxe,gys:gye))
      call rri_read_map(&
        basinType, resl, 'elv', uid, r4map, r4miss)
      where( r4map == r4miss )
        mskmap = .false.
      elsewhere
        mskmap = .true.
      endwhere
      deallocate(r4map)
    endif
  !-------------------------------------------------------------
  ! Case: Network mesh
  case( BASINTYPE__NETWORK )
    call strnk_read_network_mesh_domain(&
      resl, uid, &
      gxs, gxe, gys, gye, west, east, south, north &
    )

    allocate(mskmap(gxs:gxe,gys:gye))

    allocate(i1map(gxs:gxe,gys:gye))

    f_msk = strnk_get_f_network_mesh(resl, 'mask', uid)
    call traperr( rbin(i1map, f_msk) )

    where( i1map > 0_1 )
      mskmap = .true.
    elsewhere
      mskmap = .false.
    endwhere

    deallocate(i1map)
  !-------------------------------------------------------------
  ! Case: Network set mesh
  case( BASINTYPE__NETWORKSET )
    call errend(msg_not_implemented()//&
      '\n  basinType: '//str(basinType))
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('basinType', basinType))
  endselect
  !-------------------------------------------------------------
  ! Trim map and output
  !-------------------------------------------------------------
  selectcase( varName )
  !-------------------------------------------------------------
  ! Case: Int1 (dir, landuse)
  case( VARNAME__FDR, VARNAME__LANDUSE )
    dtype = DTYPE_INT1

    call joint_get_miss(inputDataName, varName, i1miss)
    i4miss = int(i1miss, 4)

    allocate(i4map(gxs:gxe,gys:gye))

    call jflw_read_map_from_tile(&
        resl, varName, DTYPE_INT1, i4miss, gxs, gys, i4map)

    where( .not. mskmap ) i4map = i4miss
  !-------------------------------------------------------------
  ! Case: Int4 (upg)
  case( VARNAME__UPG )
    dtype = DTYPE_INT4

    call joint_get_miss(inputDataName, varName, i4miss)

    allocate(i4map(gxs:gxe,gys:gye))

    call jflw_read_map_from_tile(&
        resl, varName, DTYPE_INT4, i4miss, gxs, gys, i4map)

    where( .not. mskmap ) i4map = i4miss
  !-------------------------------------------------------------
  ! Case: Real (elv, upa, wth)
  case( VARNAME__ELV, VARNAME__UPA, VARNAME__WTH )
    dtype = DTYPE_REAL

    call joint_get_miss(inputDataName, varName, r4miss)

    allocate(r4map(gxs:gxe,gys:gye))

    call jflw_read_map_from_tile(&
      resl, varName, DTYPE_REAL, r4miss, gxs, gys, r4map)

    where( .not. mskmap ) r4map = r4miss
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('varName', varName))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( varName == VARNAME__FDR .and. outfmt == OUTFMT__RRI )then
    do igy = gys, gye
    do igx = gxs, gxe
      call joint_conv_fdr_jflw2rri(int(i4map(igx,igy),1), i4map(igx,igy))
    enddo  ! igx/
    enddo  ! igy/
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  selectcase( outfmt )
  !-------------------------------------------------------------
  ! Case: Default (plain binary)
  case( OUTFMT__DEFAULT )
    call logmsg('Writing '//str(f_var))

    selectcase( dtype )
    case( DTYPE_INT1, DTYPE_INT4 )
      call traperr( wbin(i4map, f_var, replace=.true.) )
    case( DTYPE_REAL )
      call traperr( wbin(r4map, f_var, replace=.true.) )
    case default
      call errend(msg_invalid_value('dtype', dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: RRI
  case( OUTFMT__RRI )
    selectcase( dtype )
    case( DTYPE_INT1, DTYPE_INT4 )
      call rri_write_map(&
        basinType, resl, varName, uid, &
        i4map, i4miss, &
        west, south, JFLW_GRIDSIZE_LON &
      )
    case( DTYPE_REAL )
      call rri_write_map(&
        basinType, resl, varName, uid, &
        r4map, r4miss, &
        west, south, JFLW_GRIDSIZE_LON &
      )
    case default
      call errend(msg_invalid_value('dtype', dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('outfmt', outfmt))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine trim_basin
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
subroutine modifyTopography(basinType, resl, uid)
  use c3_joint_const
  use c2_strnk_io, only: &
    get_f_lst_networks_channel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modifyTopography'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid

  character(32) :: uid_
  integer :: n, i
  character :: c_
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( basinType )
  case( BASINTYPE__FLWDIR )
    call errend(msg_not_implemented()//'\nbasinType: '//str(basinType))

  case( BASINTYPE__NETWORK )
    if( uid == '' )then
      f = get_f_lst_networks_channel()
      open(newunit=un, file=f, status='old')
      read(un,*) c_, n
      read(un,*)
      do i = 1, n
        read(un,*) c_, uid_
        call modify_topography__network(resl, uid_)
      enddo
      close(un)
    else
      call modify_topography__network(resl, uid)
    endif

  case( BASINTYPE__NETWORKSET )

  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine modifyTopography
!===============================================================
!
!===============================================================
subroutine modify_topography__network(resl, uid)
  use c2_strnk_io, only: &
    get_f_network_channel
  use c1_io, only: &
    read_network
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'modify_topography__network'
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid

  type(cmn_network_) :: cmnnwk
  type(network_) :: nwk
  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_network_channel(uid, 'sbin')
  call read_network(f, cmnnwk)

  nwk%nNode = nwk%nCh*2
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine modify_topography__network
!===============================================================
!
!===============================================================
end module mod_basin
