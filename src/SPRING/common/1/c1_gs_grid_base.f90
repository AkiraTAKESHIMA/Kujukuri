module c1_gs_grid_base
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use c1_const
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_grid
  public :: free_grid
  public :: realloc_grid
  public :: free_grid_unused_comps

  public :: get_grid_calc_from_make
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_gs_grid_base'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function init_grid(g) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_grid'
  type(grid_), intent(inout) :: g

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g%id = ''

  g%nij = 0_8

  g%idxmin = 0_8
  g%idxmax = 0_8

  nullify(g%idx)
  nullify(g%idxarg)
  nullify(g%msk)
  nullify(g%uwa)
  nullify(g%ara)
  nullify(g%wgt)
  nullify(g%x)
  nullify(g%y)
  nullify(g%z)
  nullify(g%lon)
  nullify(g%lat)
  g%status_idx    = GRID_STATUS__UNDEF
  g%status_msk    = GRID_STATUS__UNDEF
  g%status_uwa    = GRID_STATUS__UNDEF
  g%status_ara    = GRID_STATUS__UNDEF
  g%status_wgt    = GRID_STATUS__UNDEF
  g%status_xyz    = GRID_STATUS__UNDEF
  g%status_lonlat = GRID_STATUS__UNDEF

  g%ij_debug = 0_8
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_grid
!===============================================================
!
!===============================================================
integer(4) function free_grid(grid) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_grid'
  type(grid_), intent(inout) :: grid

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid%nij = 0_8
  call realloc(grid%idx   , 0)
  call realloc(grid%idxarg, 0)
  call realloc(grid%msk   , 0)
  call realloc(grid%uwa   , 0)
  call realloc(grid%ara   , 0)
  call realloc(grid%wgt   , 0)
  call realloc(grid%x     , 0)
  call realloc(grid%y     , 0)
  call realloc(grid%z     , 0)
  call realloc(grid%lon   , 0)
  call realloc(grid%lat   , 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_grid
!===============================================================
!
!===============================================================
integer(4) function realloc_grid(&
    grid,                                   &
    idx, msk, uwa, ara, wgt, xyz, lonlat,   &
    clear,                                  &
    idx_miss, uwa_miss, ara_miss, wgt_miss, &
    xyz_miss, lonlat_miss                   &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_grid'
  type(grid_), intent(inout) :: grid
  logical    , intent(in)    :: clear
  logical    , intent(in)    :: idx, &
                                msk, &
                                uwa, &
                                ara, &
                                wgt, &
                                xyz, &
                                lonlat
  integer(8), intent(in), optional :: idx_miss
  real(8)   , intent(in), optional :: uwa_miss, ara_miss, wgt_miss, &
                                      xyz_miss, lonlat_miss

  integer(8) :: idx_miss_
  real(8)    :: uwa_miss_, ara_miss_, wgt_miss_, &
                xyz_miss_, lonlat_miss_

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  idx_miss_ = 0_8
  uwa_miss_ = 0.d0
  ara_miss_ = 0.d0
  wgt_miss_ = 0.d0
  xyz_miss_ = 0.d0
  lonlat_miss_ = 0.d0

  if( present(idx_miss)    ) idx_miss_    = idx_miss
  if( present(uwa_miss)    ) uwa_miss_    = uwa_miss
  if( present(ara_miss)    ) ara_miss_    = ara_miss
  if( present(wgt_miss)    ) wgt_miss_    = wgt_miss
  if( present(xyz_miss)    ) xyz_miss_    = xyz_miss
  if( present(lonlat_miss) ) lonlat_miss_ = lonlat_miss
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( idx )then
    call realloc(grid%idx   , grid%nij, clear=clear, fill=idx_miss_)
    call realloc(grid%idxarg, grid%nij, clear=clear, fill=0_8)

    if( clear )then
      grid%idxmin = idx_miss_
      grid%idxmax = idx_miss_
    endif
  endif

  if( msk )then
    call realloc(grid%msk, grid%nij, clear=clear, fill=logical(.false.,1))
  endif

  if( uwa )then
    call realloc(grid%uwa, grid%nij, clear=clear, fill=uwa_miss_)
  endif

  if( ara )then
    call realloc(grid%ara, grid%nij, clear=clear, fill=ara_miss_)
  endif

  if( wgt )then
    call realloc(grid%wgt, grid%nij, clear=clear, fill=wgt_miss_)
  endif

  if( xyz )then
    call realloc(grid%x, grid%nij, clear=clear, fill=xyz_miss_)
    call realloc(grid%y, grid%nij, clear=clear, fill=xyz_miss_)
    call realloc(grid%z, grid%nij, clear=clear, fill=xyz_miss_)
  endif

  if( lonlat )then
    call realloc(grid%lon, grid%nij, clear=clear, fill=lonlat_miss_)
    call realloc(grid%lat, grid%nij, clear=clear, fill=lonlat_miss_)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function realloc_grid
!===============================================================
!
!===============================================================
integer(4) function free_grid_unused_comps(&
    g, &
    idx, msk, uwa, ara, wgt, xyz, lonlat &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_grid_unused_comps'
  type(grid_), intent(inout) :: g
  logical    , intent(in)    :: idx, msk, uwa, ara, wgt, xyz, lonlat

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( .not. idx )then
    call realloc(g%idx, 0)
    call realloc(g%idxarg, 0)
  endif

  if( .not. msk )then
    call realloc(g%msk, 0)
  endif

  if( .not. uwa )then
    call realloc(g%uwa, 0)
  endif

  if( .not. ara )then
    call realloc(g%ara, 0)
  endif

  if( .not. wgt )then
    call realloc(g%wgt, 0)
  endif

  if( .not. xyz )then
    call realloc(g%x, 0)
    call realloc(g%y, 0)
    call realloc(g%z, 0)
  endif

  if( .not. lonlat )then
    call realloc(g%lon, 0)
    call realloc(g%lat, 0)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_grid_unused_comps
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
integer(4) function get_grid_calc_from_make(&
    calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat, &
    make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat  &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_grid_calc_from_make'
  logical, intent(out) :: calc_msk, calc_uwa, calc_ara, calc_wgt, calc_xyz, calc_lonlat
  logical, intent(in)  :: make_msk, make_uwa, make_ara, make_wgt, make_xyz, make_lonlat

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  calc_msk    = make_msk
  calc_uwa    = make_uwa
  calc_ara    = make_ara .or. make_xyz .or. make_lonlat
  calc_wgt    = make_ara .or. make_wgt .or. make_xyz .or. make_lonlat
  calc_xyz    = make_xyz .or. make_lonlat
  calc_lonlat = make_lonlat
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_grid_calc_from_make
!===============================================================
!
!===============================================================
end module c1_gs_grid_base
