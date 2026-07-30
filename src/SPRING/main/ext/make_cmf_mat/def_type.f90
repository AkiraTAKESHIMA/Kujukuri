module def_type
  use lib_const
  use lib_io
  use c1_type_opt, only: &
        opt_sys_, &
        opt_log_, &
        opt_earth_
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: cmn_
  public :: cmf_
  public :: mat_
  public :: opt_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type cmn_
    logical     :: is_tiled
    logical     :: is_raster_input
    integer     :: nTiles
    integer     :: ntx, nty, ntxy
    integer     :: size_lon, size_lat
    integer     :: tile_size_lon, tile_size_lat
    integer(8)  :: ncgx, ncgy          ! global
    integer(8)  :: ncx_1deg, ncy_1deg  ! 1deg
    integer(8)  :: nclx, ncly          ! tile
    integer(8)  :: nkgx, nkgy          ! global
    integer(8)  :: nkx_1deg, nky_1deg  ! 1deg
    integer(8)  :: nkx_grid, nky_grid  ! grid
    integer(8)  :: nklx, nkly          ! tile
    integer     :: west, east, south, north
    integer(8)  :: ijmax_river, ijmax_noriv
  end type

  type cmf_
    logical :: make_river, &
               make_river_end, &
               make_river_mouth, &
               make_river_inland, &
               make_noriv, &
               make_ocean

    logical :: make_rstbsn

    ! grid (in)
    type(file_) :: f_nextxy
    type(file_) :: f_basin

    ! grid (out)
    type(file_) :: f_grdidx_river, &
                   f_grdidx_river_end, &
                   f_grdidx_river_mouth, &
                   f_grdidx_river_inland, &
                   f_grdidx_noriv, &
                   f_grdidx_ocean

    ! raster (in, untiled)
    type(file_) :: f_catmxy

    ! raster (out, untiled)
    type(file_) :: f_rstidx_river, &
                   f_rstidx_river_end, &
                   f_rstidx_river_mouth, &
                   f_rstidx_river_inland, &
                   f_rstidx_noriv, &
                   f_rstidx_ocean
    type(file_) :: f_rstbsn

    ! raster (in, tiled)
    character(clen_path) :: path_list_catmxy
    character(clen_path), pointer :: list_path_catmxy(:) !(cmn%nTiles)
    character(clen_path) :: dir_catmxy
    character(clen_key)  :: dtype_catmxy
    character(clen_key)  :: endian_catmxy

    ! raster (out, tiled)
    character(clen_path), pointer :: list_path_rstidx_river(:), &
                                     list_path_rstidx_river_end(:), &
                                     list_path_rstidx_river_mouth(:), &
                                     list_path_rstidx_river_inland(:), &
                                     list_path_rstidx_noriv(:), &
                                     list_path_rstidx_ocean(:) !(cmn%nTiles)
    character(clen_path) :: dir_rstidx_river, &
                            dir_rstidx_river_end, &
                            dir_rstidx_river_mouth, &
                            dir_rstidx_river_inland, &
                            dir_rstidx_noriv, &
                            dir_rstidx_ocean
    character(clen_key)  :: dtype_rstidx
    character(clen_key)  :: endian_rstidx
    character(clen_path), pointer :: list_path_rstbsn(:) !(cmn%nTiles)
    character(clen_path) :: dir_rstbsn
    character(clen_key)  :: dtype_rstbsn
    character(clen_key)  :: endian_rstbsn

    ! index
    integer(8) :: catmxy_noriv_coastal
    integer(8) :: catmxy_noriv_inland
    integer(8) :: catmxy_ocean
    integer(8) :: nextxy_river_mouth
    integer(8) :: nextxy_river_inland
    integer(8) :: nextxy_ocean
    integer(8) :: idx_miss
    integer(8) :: bsn_miss

    ! option
    character(clen_key) :: idx_condition
  end type

  type mat_
    logical :: make_grdmsk_river, &
               make_grdmsk_river_end, &
               make_grdmsk_river_mouth, &
               make_grdmsk_river_inland, &
               make_grdmsk_noriv

    logical :: make_grdidx_river, &
               make_grdidx_river_end, &
               make_grdidx_river_mouth, &
               make_grdidx_river_inland, &
               make_grdidx_noriv, &
               make_grdidx_ocean

    logical :: make_grdidx_bnd_river, &
               make_grdidx_bnd_river_end, &
               make_grdidx_bnd_river_mouth, &
               make_grdidx_bnd_river_inland, &
               make_grdidx_bnd_noriv

    logical :: make_grdidx_mkbnd_river, &
               make_grdidx_mkbnd_noriv

    logical :: make_rstidx_river, &
               make_rstidx_river_end, &
               make_rstidx_river_mouth, &
               make_rstidx_river_inland, &
               make_rstidx_noriv, &
               make_rstidx_ocean

    logical :: make_rstidx_bnd_river, &
               make_rstidx_bnd_river_end, &
               make_rstidx_bnd_river_mouth, &
               make_rstidx_bnd_river_inland, &
               make_rstidx_bnd_noriv

    logical :: make_rstidx_mkbnd_river, &
               make_rstidx_mkbnd_noriv

    ! grdmsk
    type(file_) :: f_grdmsk_river, &
                   f_grdmsk_river_end, &
                   f_grdmsk_river_mouth, &
                   f_grdmsk_river_inland, &
                   f_grdmsk_noriv, &
                   f_grdmsk_ocean

    ! grdidx
    type(file_) :: f_grdidx_river, &
                   f_grdidx_river_end, &
                   f_grdidx_river_mouth, &
                   f_grdidx_river_inland, &
                   f_grdidx_noriv, &
                   f_grdidx_ocean

    ! grdidx_bnd
    type(file_) :: f_grdidx_bnd_river, &
                   f_grdidx_bnd_river_end, &
                   f_grdidx_bnd_river_mouth, &
                   f_grdidx_bnd_river_inland, &
                   f_grdidx_bnd_noriv

    ! grdidx_mkbnd
    type(file_) :: f_grdidx_mkbnd_river, &
                   f_grdidx_mkbnd_noriv

    ! rstidx (untiled)
    type(file_) :: f_rstidx_river, &
                   f_rstidx_river_end, &
                   f_rstidx_river_mouth, &
                   f_rstidx_river_inland, &
                   f_rstidx_noriv, &
                   f_rstidx_ocean

    ! rstidx_bnd (untiled)
    type(file_) :: f_rstidx_bnd_river, &
                   f_rstidx_bnd_river_end, &
                   f_rstidx_bnd_river_mouth, &
                   f_rstidx_bnd_river_inland, &
                   f_rstidx_bnd_noriv

    ! rstidx_bnd (untiled)
    type(file_) :: f_rstidx_mkbnd_river, &
                   f_rstidx_mkbnd_noriv

    ! rstidx (tiled)
    character(clen_path), pointer :: list_path_rstidx_river(:), &
                                     list_path_rstidx_river_end(:), &
                                     list_path_rstidx_river_mouth(:), &
                                     list_path_rstidx_river_inland(:), &
                                     list_path_rstidx_noriv(:), &
                                     list_path_rstidx_ocean(:) !(cmn%nTiles)
    character(clen_path) :: dir_rstidx_river, &
                            dir_rstidx_river_end, &
                            dir_rstidx_river_mouth, &
                            dir_rstidx_river_inland, &
                            dir_rstidx_noriv, &
                            dir_rstidx_ocean
    character(clen_key)  :: dtype_rstidx
    character(clen_key)  :: endian_rstidx

    ! rstidx_bnd (tiled)
    character(clen_path), pointer :: list_path_rstidx_bnd_river(:), &
                                     list_path_rstidx_bnd_river_end(:), &
                                     list_path_rstidx_bnd_river_mouth(:), &
                                     list_path_rstidx_bnd_river_inland(:), &
                                     list_path_rstidx_bnd_noriv(:) !(cmn%nTiles)
    character(clen_path) :: dir_rstidx_bnd_river, &
                            dir_rstidx_bnd_river_end, &
                            dir_rstidx_bnd_river_mouth, &
                            dir_rstidx_bnd_river_inland, &
                            dir_rstidx_bnd_noriv
    character(clen_key)  :: dtype_rstidx_bnd
    character(clen_key)  :: endian_rstidx_bnd

    ! rstidx_bnd (tiled)
    character(clen_path), pointer :: list_path_rstidx_mkbnd_river(:), &
                                     list_path_rstidx_mkbnd_noriv(:) !(cmn%nTiles)
    character(clen_path) :: dir_rstidx_mkbnd_river, &
                            dir_rstidx_mkbnd_noriv
    character(clen_key)  :: dtype_rstidx_mkbnd
    character(clen_key)  :: endian_rstidx_mkbnd

    ! index
    integer(8) :: idx_miss
  end type

  type opt_
    type(opt_sys_)   :: sys
    type(opt_log_)   :: log
    type(opt_earth_) :: earth
    logical :: save_memory
  end type
end module def_type
