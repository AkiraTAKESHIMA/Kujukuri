module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_util
  use lib_array
  use lib_math
  use c1_const
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: make_cmf_mat
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface realloc_dat
    module procedure realloc_dat_int1
    module procedure realloc_dat_int8
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_main'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_cmf_mat(cmn, cmf, mat, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_cmf_mat'
  type(cmn_), intent(inout) :: cmn
  type(cmf_), intent(in)    :: cmf
  type(mat_), intent(in)    :: mat
  type(opt_), intent(in)    :: opt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_cmf(cmn, cmf, opt)

  call make_mat(cmn, cmf, mat, opt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_cmf_mat
!===============================================================
!
!===============================================================
subroutine make_cmf(cmn, cmf, opt)
  use c1_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_cmf'
  type(cmn_), intent(in)         :: cmn
  type(cmf_), intent(in), target :: cmf
  type(opt_), intent(in)         :: opt

  integer(8), allocatable :: nextxx(:,:), nextyy(:,:)
  integer(8), allocatable :: catmxx(:,:), &
                             catmyy(:,:)
  integer(8), pointer     :: grdidx_river(:,:), &
                             grdidx_river_end(:,:), &
                             grdidx_river_mouth(:,:), &
                             grdidx_river_inland(:,:), &
                             grdidx_noriv(:,:), &
                             grdidx_ocean(:,:)
  integer(1), pointer     :: grdstat_river(:,:), &
                             grdstat_river_end(:,:), &
                             grdstat_river_mouth(:,:), &
                             grdstat_river_inland(:,:)
  integer(8), pointer     :: rstidx_river(:,:), &
                             rstidx_river_end(:,:), &
                             rstidx_river_mouth(:,:), &
                             rstidx_river_inland(:,:), &
                             rstidx_noriv(:,:), &
                             rstidx_ocean(:,:)
  integer(8), pointer :: rstbsn(:,:)
  integer(8), pointer :: grdbsn_1d(:)
  integer(8), pointer :: grdidx_river_1d(:)
  integer(8), pointer :: arg_grdidx_river_1d(:)
  integer(8) :: cgxi, cgxf, cgyi, cgyf
  integer(8) :: kgxi, kgxf, kgyi, kgyf
  integer(8) :: icgx, icgy
  integer(8) :: iklx, ikly
  integer(8) :: cgx
  integer(8) :: nkij_river, &
                nkij_river_end, &
                nkij_river_mouth, &
                nkij_river_inland, &
                nkij_noriv, &
                nkij_ocean
  integer :: west, east, south, north
  integer :: iTile
  logical :: make_anyriv
  character(clen_path), pointer :: path
  type(file_), pointer :: f

  integer :: dgt_cgxy, dgt_kgxy, dgt_idx, dgt_kij
  character(:), allocatable :: list_landTypes_river
  character(:), allocatable :: s

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_cgxy = dgt((/cmn%ncgx,cmn%ncgy/),dgt_opt_max)
  dgt_kgxy = dgt((/cmn%nkgx,cmn%nkgy/),dgt_opt_max)
  dgt_idx = dgt(cmn%ncgx*cmn%ncgy)
  dgt_kij = dgt(cmn%nkgx*cmn%nkgy)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nkij_river        = 0_8
  nkij_river_end    = 0_8
  nkij_river_mouth  = 0_8
  nkij_river_inland = 0_8
  nkij_noriv        = 0_8
  nkij_ocean        = 0_8

  allocate(grdidx_river(1,1))
  allocate(grdidx_river_end(1,1))
  allocate(grdidx_river_mouth(1,1))
  allocate(grdidx_river_inland(1,1))
  allocate(grdidx_noriv(1,1))
  allocate(grdidx_ocean(1,1))

  allocate(grdstat_river(1,1))
  allocate(grdstat_river_end(1,1))
  allocate(grdstat_river_mouth(1,1))
  allocate(grdstat_river_inland(1,1))

  allocate(rstidx_river(1,1))
  allocate(rstidx_river_end(1,1))
  allocate(rstidx_river_mouth(1,1))
  allocate(rstidx_river_inland(1,1))
  allocate(rstidx_noriv(1,1))
  allocate(rstidx_ocean(1,1))

  allocate(rstbsn(1,1))
  allocate(grdbsn_1d(1))
  allocate(grdidx_river_1d(1))
  allocate(arg_grdidx_river_1d(1))

  call logmsg('Make river       : '//str(cmf%make_river)//&
            '\n     river_end   : '//str(cmf%make_river_end)//&
            '\n     river_mouth : '//str(cmf%make_river_mouth)//&
            '\n     river_inland: '//str(cmf%make_river_inland)//&
            '\n     noriv       : '//str(cmf%make_noriv)//&
            '\n     ocean       : '//str(cmf%make_ocean))

  call realloc_dat(cmf%make_river, grdidx_river , cmn%ncgx, cmn%ncgy)
  call realloc_dat(cmf%make_river, grdstat_river, cmn%ncgx, cmn%ncgy)

  call realloc_dat(cmf%make_river_end, grdidx_river_end , cmn%ncgx, cmn%ncgy)
  call realloc_dat(cmf%make_river_end, grdstat_river_end, cmn%ncgx, cmn%ncgy)

  call realloc_dat(cmf%make_river_mouth, grdidx_river_mouth , cmn%ncgx, cmn%ncgy)
  call realloc_dat(cmf%make_river_mouth, grdstat_river_mouth, cmn%ncgx, cmn%ncgy)

  call realloc_dat(cmf%make_river_inland, grdidx_river_mouth , cmn%ncgx, cmn%ncgy)
  call realloc_dat(cmf%make_river_inland, grdstat_river_mouth, cmn%ncgx, cmn%ncgy)

  call realloc_dat(cmf%make_noriv, grdidx_noriv, cmn%ncgx, cmn%ncgy)

  call realloc_dat(cmf%make_ocean, grdidx_ocean, cmn%ncgx, cmn%ncgy)

  if( cmf%make_rstbsn )then
    call realloc(rstbsn, (/1_8,1_8/), (/cmn%nklx,cmn%nkly/), fill=cmf%bsn_miss)
    call realloc(grdbsn_1d, cmn%ncgx*cmn%ncgy)
    call realloc(grdidx_river_1d, cmn%ncgx*cmn%ncgy)
    call realloc(arg_grdidx_river_1d, cmn%ncgx*cmn%ncgy)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_anyriv = .false.
  allocate(character(1) :: list_landTypes_river)
  allocate(character(1) :: s)
  if( cmf%make_river_inland )then
    list_landTypes_river = 'and `river_inland`'
    make_anyriv = .true.
  endif
  if( cmf%make_river_mouth )then
    if( make_anyriv )then
      s = list_landTypes_river
      list_landTypes_river = ', `river_mouth` '//s
    else
      list_landTypes_river = 'and `river_mouth`'
    endif
    make_anyriv = .true.
  endif
  if( cmf%make_river_end )then
    if( make_anyriv )then
      s = list_landTypes_river
      list_landTypes_river = ', `river_end` '//s
    else
      list_landTypes_river = 'and `river_end`'
    endif
    make_anyriv = .true.
  endif
  if( cmf%make_river )then
    if( make_anyriv )then
      s = list_landTypes_river
      list_landTypes_river = '`river` '//s
    else
      list_landTypes_river = '`river`'
    endif
    make_anyriv = .true.
  endif
  deallocate(s)
  !-------------------------------------------------------------
  ! Read nextxy
  !-------------------------------------------------------------
  call logent('Reading nextxy', PRCNAM, MODNAM)

  allocate(nextxx(cmn%ncgx,cmn%ncgy))
  allocate(nextyy(cmn%ncgx,cmn%ncgy))

  f => cmf%f_nextxy
  call traperr( rbin(nextxx, f%path, f%dtype, f%endian, 1) )
  call traperr( rbin(nextyy, f%path, f%dtype, f%endian, 2) )

  do icgy = 1_8, cmn%ncgy
    do icgx = 1_8, cmn%ncgx
      cgx = nextxx(icgx,icgy)

      if( cgx > 0_8 )then
        continue
      elseif( cgx == cmf%nextxy_river_mouth .or. &
              cgx == cmf%nextxy_river_inland .or. &
              cgx == cmf%nextxy_ocean )then
        continue
      else
        call errend(msg_invalid_value('nextxx('//str((/icgx,icgy/),', ')//')', cgx))
      endif
    enddo
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Make grdidx of river, river_end, river_mouth, river_inland
  !-------------------------------------------------------------
  if( make_anyriv )then
    call logent('Making grid maps of grid indices of '//&
                list_landTypes_river, PRCNAM, MODNAM)

    call make_grdidx_river(grdidx_river)

    if( cmf%make_river_end )then
      call make_grdidx_river_end(grdidx_river_end)
    endif

    if( cmf%make_river_mouth )then
      call make_grdidx_river_mouth(grdidx_river_mouth)
    endif

    if( cmf%make_river_inland )then
      call make_grdidx_river_inland(grdidx_river_inland)
    endif

    if( cmf%make_river_end .or. cmf%make_river_mouth .or. cmf%make_river_inland )then
      call logent('Checking consistency', PRCNAM, MODNAM)

      call check_consistency_grdidx_river(&
             cmn, cmf%idx_miss, &
             grdidx_river      , grdidx_river_end, &
             grdidx_river_mouth, grdidx_river_inland)

      call logext()
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  ! Read basin ID
  !-------------------------------------------------------------
  if( cmf%make_rstbsn )then
    call logent('Reading basin ID', PRCNAM, MODNAM)

    f => cmf%f_basin
    call traperr( rbin(grdbsn_1d, f%path, f%dtype, f%endian, f%rec) )

    grdidx_river_1d(:) = reshape(grdidx_river,(/cmn%ncgx*cmn%ncgy/))
    call argsort(grdidx_river_1d, arg_grdidx_river_1d)

    call logext()
  endif
  !-------------------------------------------------------------
  ! Make raster data or grid data that requires raster data
  !-------------------------------------------------------------
  if( cmn%is_raster_input )then
    grdidx_noriv(:,:) = cmf%idx_miss
    if( cmf%make_river        ) grdstat_river(:,:)        = GRDSTAT_INVALID
    if( cmf%make_river_end    ) grdstat_river_end(:,:)    = GRDSTAT_INVALID
    if( cmf%make_river_mouth  ) grdstat_river_mouth(:,:)  = GRDSTAT_INVALID
    if( cmf%make_river_inland ) grdstat_river_inland(:,:) = GRDSTAT_INVALID

    allocate(catmxx(cmn%nklx,cmn%nkly))
    allocate(catmyy(cmn%nklx,cmn%nkly))

    do iTile = 1, cmn%nTiles
      if( cmn%is_tiled )&
      call logent('Tile '//str(iTile)//' / '//str(cmn%nTiles), PRCNAM, MODNAM)
      !-----------------------------------------------------------
      ! Read catmxy
      !-----------------------------------------------------------
      call logent('Reading catmxy', PRCNAM, MODNAM)

      if( .not. cmn%is_tiled )then
        cgxi = 1_8
        cgxf = cmn%ncgx
        cgyi = 1_8
        cgyf = cmn%ncgy

        f => cmf%f_catmxy
        call traperr( rbin(catmxx, f%path, f%dtype, f%endian, 1) )
        call traperr( rbin(catmyy, f%path, f%dtype, f%endian, 2) )
      else
        path => cmf%list_path_catmxy(iTile)

        call get_value_bounds_tile(&
               cmn, filename(path), west, east, south, north, &
               cgxi, cgxf, cgyi, cgyf, &
               kgxi, kgxf, kgyi, kgyf)
        call logmsg('  grid  ['//str((/cgxi,cgxf/),dgt_kgxy,':')//&
                           ', '//str((/cgyi,cgyf/),dgt_kgxy,':')//']')
        call logmsg('  raster['//str((/kgxi,kgxf/),dgt_kgxy,':')//&
                           ', '//str((/kgyi,kgyf/),dgt_kgxy,':')//']')

        call traperr( rbin(catmxx, path, cmf%dtype_catmxy, cmf%endian_catmxy, 1) )
        call traperr( rbin(catmyy, path, cmf%dtype_catmxy, cmf%endian_catmxy, 2) )
      endif

      do ikly = 1_8, cmn%nkly
        do iklx = 1_8, cmn%nklx
          cgx = catmxx(iklx,ikly)
          if( cgx > 0_8 )then
            continue
          elseif( cgx == cmf%catmxy_noriv_coastal .or. &
                  cgx == cmf%catmxy_noriv_inland )then
            continue
          elseif( cgx == cmf%catmxy_ocean )then
            continue
          else
            call errend(msg_invalid_value('catmxx('//str((/iklx,ikly/),', ')//')', cgx))
          endif
        enddo
      enddo

      call logext()
      !---------------------------------------------------------
      ! Make rstidx of river, river_end, river_mouth, river_inland
      !---------------------------------------------------------
      if( make_anyriv )then
        call logent('Making raster maps of grid indices of '//&
                    list_landTypes_river, PRCNAM, MODNAM)

        call realloc_dat(cmf%make_river       , rstidx_river       , cmn%nklx, cmn%nkly)
        call realloc_dat(cmf%make_river_end   , rstidx_river_end   , cmn%nklx, cmn%nkly)
        call realloc_dat(cmf%make_river_mouth , rstidx_river_mouth , cmn%nklx, cmn%nkly)
        call realloc_dat(cmf%make_river_inland, rstidx_river_inland, cmn%nklx, cmn%nkly)

        if( cmf%make_river )then
          call make_rstidx_river_from_grdidx(&
                 nkij_river, rstidx_river,     & ! out
                 cmn,                          & ! in
                 catmxx, catmyy, grdidx_river, &
                 cmf%idx_miss, .true., 'river')  ! in
        endif

        if( cmf%make_river_end )then
          call make_rstidx_river_from_grdidx(&
                 nkij_river_end, rstidx_river_end, & ! out
                 cmn,                              & ! in
                 catmxx, catmyy, grdidx_river_end, &
                 cmf%idx_miss, .false., 'river_end') ! in
        endif

        if( cmf%make_river_mouth )then
          call make_rstidx_river_from_grdidx(&
                 nkij_river_mouth, rstidx_river_mouth, & ! out
                 cmn,                                  & ! in
                 catmxx, catmyy, grdidx_river_mouth,   & ! in
                 cmf%idx_miss, .false., 'river_mouth')   ! in
        endif

        if( cmf%make_river_inland )then
          call make_rstidx_river_from_grdidx(&
                 nkij_river_inland, rstidx_river_inland, & ! out
                 cmn,                                    & ! in
                 catmxx, catmyy, grdidx_river_inland,    & ! in
                 cmf%idx_miss, .false., 'river_inland')    ! in
        endif

        call logext()
      endif
      !---------------------------------------------------------
      ! Check consistency among rstidx of
      ! river, river_end, river_mouth and river_inland
      !---------------------------------------------------------
      if( make_anyriv )then
        call logent('Checking consistency among raster '//&
                    'maps of grid indices of '//list_landTypes_river, &
                    PRCNAM, MODNAM)

        call check_consistency_rstidx_river(&
               cmn, cmf%idx_miss, &
               rstidx_river      , rstidx_river_end, &
               rstidx_river_mouth, rstidx_river_inland)

        call logext()
      endif
      !---------------------------------------------------------
      ! Check consistency of indices between raster and grid
      ! Update grid status
      !---------------------------------------------------------
      if( make_anyriv )then
        call logent('Checking consistency of grid '//&
                    'indices between raster map and grid map', &
                    PRCNAM, MODNAM)

        ! river
        !-------------------------------------------------------
        if( cmf%make_river )then
          call logent('`river`', PRCNAM, MODNAM)

          ! Check if the set of indices of raster are in that of grid
          call check_if_rstidx_in_grdidx(&
                 cmn, cmf%idx_miss, & ! in
                 grdidx_river, rstidx_river, & ! in
                 grdstat_river) ! inout

          ! Check if the set of indices of grid are in that of raster
          if( .not. cmn%is_tiled )then
            call cmf_check_if_grdidx_in_rstidx(&
                   'river', cmf%idx_miss, cmf%idx_condition, & ! in
                   grdidx_river, & ! in
                   grdstat_river) ! inout
          endif

          call logext()
        endif

        ! river_end
        !-------------------------------------------------------
        if( cmf%make_river_end )then
          call logent('`river_end`', PRCNAM, MODNAM)

          ! Check if the set of indices of raster are in that of grid
          call check_if_rstidx_in_grdidx(&
                 cmn, cmf%idx_miss, & ! in
                 grdidx_river_end, rstidx_river_end, & ! in
                 grdstat_river_end) ! inout

          ! Check if the set of indices of grid are in that of raster
          if( .not. cmn%is_tiled )then
            call cmf_check_if_grdidx_in_rstidx(&
                   'river_end', cmf%idx_miss, cmf%idx_condition, & ! in
                   grdidx_river_end, & ! in
                   grdstat_river_end) ! inout
          endif

          call logext()
        endif

        ! river_mouth
        !-------------------------------------------------------
        if( cmf%make_river_mouth )then
          call logent('`river_mouth`', PRCNAM, MODNAM)

          ! Check if the set of indices of raster are in that of grid
          call check_if_rstidx_in_grdidx(&
                 cmn, cmf%idx_miss, & ! in
                 grdidx_river_mouth, rstidx_river_mouth, & ! in
                 grdstat_river_mouth) ! inout

          ! Check if the set of indices of grid are in that of raster
          if( .not. cmn%is_tiled )then
            call cmf_check_if_grdidx_in_rstidx(&
                   'river_mouth', cmf%idx_miss, cmf%idx_condition, & ! in
                   grdidx_river_mouth, & ! in
                   grdstat_river_mouth) ! inout
          endif

          call logext()
        endif

        ! river_inland
        !-------------------------------------------------------
        if( cmf%make_river_inland )then
          call logent('`river_inland`', PRCNAM, MODNAM)

          ! Check if the set of indices of raster are in that of grid
          call check_if_rstidx_in_grdidx(&
                 cmn, cmf%idx_miss, & ! in
                 grdidx_river_inland, rstidx_river_inland, & ! in
                 grdstat_river_inland) ! inout

          ! Check if the set of indices of grid are in that of raster
          if( .not. cmn%is_tiled )then
            call cmf_check_if_grdidx_in_rstidx(&
                   'river_inland', cmf%idx_miss, cmf%idx_condition, & ! in
                   grdidx_river_inland, & ! in
                   grdstat_river_inland) ! inout
          endif

          call logext()
        endif

        call logext()
      endif
      !-----------------------------------------------------------
      ! Make rstbsn
      !-----------------------------------------------------------
      if( cmf%f_rstbsn%path /= '' .or. cmf%dir_rstbsn /= '' )then
        call logent('Making a raster map of basin ID', PRCNAM, MODNAM)

        call make_rstbsn(&
               rstbsn, & ! out
               grdbsn_1d, grdidx_river_1d, arg_grdidx_river_1d, rstidx_river) ! in

        call logext()
      endif
      !-----------------------------------------------------------
      ! Output rstidx of river_end, river_mouth, river_inland
      !-----------------------------------------------------------
      if( cmf%make_river_end .or. cmf%make_river_mouth .or. &
          cmf%make_river_inland )then
        call logent('Outputting raster index maps of '//&
                    '`river_end`, `river_mouth` and `river_inland`', &
                    PRCNAM, MODNAM)

        if( .not. cmn%is_tiled )then
          f => cmf%f_rstidx_river_end
          if( f%path /= '' )then
            call logmsg('Writing rstidx_river_end')
            call traperr( wbin(rstidx_river_end, f%path, f%dtype, f%endian, f%rec) )
          endif

          f => cmf%f_rstidx_river_mouth
          if( f%path /= '' )then
            call logmsg('Writing rstidx_river_mouth')
            call traperr( wbin(rstidx_river_mouth, f%path, f%dtype, f%endian, f%rec) )
          endif

          f => cmf%f_rstidx_river_inland
          if( f%path /= '' )then
            call logmsg('Writing rstidx_river_inland')
            call traperr( wbin(rstidx_river_inland, f%path, f%dtype, f%endian, f%rec) )
          endif
        else
          if( cmf%dir_rstidx_river_end /= '' )then
            path => cmf%list_path_rstidx_river_end(iTile)
            if( path /= '' )then
              call logmsg('Writing rstidx_river_end')
              call traperr( wbin(rstidx_river_end, path, cmf%dtype_rstidx, cmf%endian_rstidx, 1) )
            endif
          endif

          if( cmf%dir_rstidx_river_mouth /= '' )then
            path => cmf%list_path_rstidx_river_mouth(iTile)
            if( path /= '' )then
              call logmsg('Writing rstidx_river_mouth')
              call traperr( wbin(rstidx_river_mouth, path, cmf%dtype_rstidx, cmf%endian_rstidx, 1) )
            endif
          endif

          if( cmf%dir_rstidx_river_inland /= '' )then
            path => cmf%list_path_rstidx_river_inland(iTile)
            if( path /= '' )then
              call logmsg('Writing rstidx_river_inland')
              call traperr( wbin(rstidx_river_inland, path, cmf%dtype_rstidx, cmf%endian_rstidx, 1) )
            endif
          endif
        endif

        call logext()
      endif
      !---------------------------------------------------------
      if( opt%save_memory )then
        call realloc(rstidx_river_end   , 0)
        call realloc(rstidx_river_mouth , 0)
        call realloc(rstidx_river_inland, 0)
      endif
      !---------------------------------------------------------
      ! Make grdidx and rstidx of noriv
      !---------------------------------------------------------
      if( cmf%make_noriv )then
        call logent('Making grid and raster maps of '//&
                    'grid indices of `noriv`', PRCNAM, MODNAM)

        call realloc(rstidx_noriv, (/1_8,1_8/), (/cmn%nklx,cmn%nkly/))

        call make_grdidx_rstidx_noriv(&
               nkij_noriv, grdidx_noriv, & ! inout
               rstidx_noriv) ! in

        ! Check if the set of indices of raster are in that of grid
        call logent('Checking consistency with the grid data', PRCNAM, MODNAM)
        call check_consistency_grdidx_rstidx_rect(&
               cmn, cmf%idx_miss, cgxi, cgxf, cgyi, cgyf, &
               grdidx_noriv, rstidx_noriv)
        call logext()

        call logext()
      endif
      !---------------------------------------------------------
      ! Make grid and raster of ocean
      !---------------------------------------------------------
      if( cmf%make_ocean )then
        call logent('Making grid and raster maps of '//&
                    'grid indices of `ocean`', PRCNAM, MODNAM)

        call realloc(rstidx_ocean, (/1_8,1_8/), (/cmn%nklx,cmn%nkly/))

        call make_grdidx_rstidx_ocean(&
               nkij_ocean, & ! out
               grdidx_ocean, rstidx_ocean) ! out

        ! Check if the set of indices of raster are in that of grid
        call logent('Checking consistency with the grid data', PRCNAM, MODNAM)
        call check_consistency_grdidx_rstidx_rect(&
               cmn, cmf%idx_miss, cgxi, cgxf, cgyi, cgyf, &
               grdidx_ocean, rstidx_ocean)
        call logext()

        call logext()
      endif
      !---------------------------------------------------------
      ! Check consistency among rstidx of river, noriv, ocean
      !---------------------------------------------------------
      call logent('Checking consistency among raster maps '//&
                  'of grid indices of `river`, `noriv` and `ocean`', &
                  PRCNAM, MODNAM)

      call check_consistency_rstidx_validity(&
             cmn, cmf%idx_miss, &
             rstidx_river, rstidx_noriv, rstidx_ocean)

      call logext()
      !---------------------------------------------------------
      ! Output rstidx of river, noriv, ocean
      !---------------------------------------------------------
      call logent('Outputting raster index maps of '//&
                  '`river`, `noriv` and `ocean`', &
                  PRCNAM, MODNAM)

      if( .not. cmn%is_tiled )then
        f => cmf%f_rstidx_river
        if( f%path /= '' )then
          call logmsg('Writing rstidx_river')
          call traperr( wbin(rstidx_river, f%path, f%dtype, f%endian, f%rec) )
        endif

        f => cmf%f_rstidx_noriv
        if( f%path /= '' )then
          call logmsg('Writing rstidx_noriv')
          call traperr( wbin(rstidx_noriv, f%path, f%dtype, f%endian, f%rec) )
        endif

        f => cmf%f_rstidx_ocean
        if( f%path /= '' )then
          call logmsg('Writing rstidx_ocean')
          call traperr( wbin(rstidx_ocean, f%path, f%dtype, f%endian, f%rec) )
        endif
      else
        if( cmf%dir_rstidx_river /= '' )then
          path => cmf%list_path_rstidx_river(iTile)
          if( path /= '' )then
            call logmsg('Writing rstidx_river')
            call traperr( wbin(rstidx_river, path, cmf%dtype_rstidx, cmf%endian_rstidx, 1) )
          endif
        endif

        if( cmf%dir_rstidx_noriv /= '' )then
          path => cmf%list_path_rstidx_noriv(iTile)
          if( path /= '' )then
            call logmsg('Writing rstidx_noriv')
            call traperr( wbin(rstidx_noriv, path, cmf%dtype_rstidx, cmf%endian_rstidx, 1) )
          endif
        endif

        if( cmf%dir_rstidx_ocean /= '' )then
          path => cmf%list_path_rstidx_ocean(iTile)
          if( path /= '' )then
            call logmsg('Writing rstidx_ocean')
            call traperr( wbin(rstidx_ocean, path, cmf%dtype_rstidx, cmf%endian_rstidx, 1) )
          endif
        endif
      endif

      call logext()
      !---------------------------------------------------------
      ! Output rstbsn
      !---------------------------------------------------------
      if( cmf%make_rstbsn )then
        call logent('Outputting a raster map of basin ID', PRCNAM, MODNAM)

        if( .not. cmn%is_tiled )then
          f => cmf%f_rstbsn
          call logmsg('Writing rstbsn')
          call traperr( wbin(rstbsn, f%path, f%dtype, f%endian, f%rec) )
        else
          path => cmf%list_path_rstbsn(iTile)
          if( path /= '' )then
            call logmsg('Writing rstbsn')
            call traperr( wbin(rstbsn, path, cmf%dtype_rstbsn, cmf%endian_rstbsn, 1) )
          endif
        endif

        call logext()
      endif
      !---------------------------------------------------------
      if( opt%save_memory )then
        call realloc(rstidx_river, 0)
        call realloc(rstidx_noriv, 0)
        call realloc(rstidx_ocean, 0)
      endif
      !---------------------------------------------------------
      ! Summary
      !---------------------------------------------------------
      call logmsg('Raster')
      call logmsg('  river     : '//str(nkij_river       ,dgt_kij)//' '//str(cmf%make_river))
      call logmsg('    end     : '//str(nkij_river_end   ,dgt_kij)//' '//str(cmf%make_river_end   ))
      call logmsg('      mouth : '//str(nkij_river_mouth ,dgt_kij)//' '//str(cmf%make_river_mouth ))
      call logmsg('      inland: '//str(nkij_river_inland,dgt_kij)//' '//str(cmf%make_river_inland))
      call logmsg('  noriv     : '//str(nkij_noriv       ,dgt_kij)//' '//str(cmf%make_noriv       ))
      call logmsg('  ocean     : '//str(nkij_ocean       ,dgt_kij)//' '//str(cmf%make_ocean       ))
      call logmsg('  Total     : '//str(nkij_river+nkij_noriv+nkij_ocean,dgt_kij)//&
                  ' (river + noriv + ocean)')
      !---------------------------------------------------------
      if( cmn%is_tiled ) call logext()
    enddo  ! iTile/
  endif
  !-------------------------------------------------------------
  ! Check status of grid of river
  !-------------------------------------------------------------
  if( cmn%is_tiled )then
    call logent('Checking consistency of status and grid of river', PRCNAM, MODNAM)

    call cmf_check_if_grdidx_in_rstidx(&
           'river', cmf%idx_miss, cmf%idx_condition, & ! in
           grdidx_river, & ! in
           grdstat_river) ! inout

    call logext()
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  !if( cmf%make_river )then
  !  call cmf_check_consistency_grdstat_river(&
  !         cmf%idx_miss, grdidx_river, grdstat_river, &
  !         nextxx, nextyy, cmf%opt_invalid)
  !endif
  !-------------------------------------------------------------
  ! Output all grids
  !-------------------------------------------------------------
  call logent('Outputting all grids', PRCNAM, MODNAM)

  f => cmf%f_grdidx_river
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river')
    call traperr( wbin(grdidx_river, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => cmf%f_grdidx_river_end
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river_end')
    call traperr( wbin(grdidx_river_end, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => cmf%f_grdidx_river_mouth
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river_mouth')
    call traperr( wbin(grdidx_river_mouth, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => cmf%f_grdidx_river_inland
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river_inland')
    call traperr( wbin(grdidx_river_inland, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => cmf%f_grdidx_noriv
  if( f%path /= '' )then
    call logmsg('Writing grdidx_noriv')
    call traperr( wbin(grdidx_noriv, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => cmf%f_grdidx_ocean
  if( f%path /= '' )then
    call logmsg('Writing grdidx_ocean')
    call traperr( wbin(grdidx_ocean, f%path, f%dtype, f%endian, f%rec) )
  endif

  call logext()
  !-----------------------------------------------------------
  if( cmn%is_raster_input )then
    deallocate(catmxx)
    deallocate(catmyy)
  endif

  deallocate(nextxx)
  deallocate(nextyy)

  call realloc(grdidx_river       , 0)
  call realloc(grdidx_river_end   , 0)
  call realloc(grdidx_river_mouth , 0)
  call realloc(grdidx_river_inland, 0)
  call realloc(grdidx_noriv       , 0)
  call realloc(grdidx_ocean       , 0)

  call realloc(grdstat_river       , 0)
  call realloc(grdstat_river_end   , 0)
  call realloc(grdstat_river_mouth , 0)
  call realloc(grdstat_river_inland, 0)

  call realloc(rstidx_river       , 0)
  call realloc(rstidx_river_end   , 0)
  call realloc(rstidx_river_mouth , 0)
  call realloc(rstidx_river_inland, 0)
  call realloc(rstidx_noriv       , 0)
  call realloc(rstidx_ocean       , 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_grdidx_river(grdidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_river'
  integer(8), intent(out) :: grdidx(:,:)

  integer(8) :: icgx, icgy
  integer(8) :: iXX
  integer(8) :: nij_grid

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  grdidx(:,:) = cmf%idx_miss

  do icgy = 1_8, cmn%ncgy
    do icgx = 1_8, cmn%ncgx
      iXX = nextxx(icgx,icgy)

      if( iXX > 0_8 .or. &
          iXX == cmf%nextxy_river_mouth .or. &
          iXX == cmf%nextxy_river_inland )then
        grdidx(icgx,icgy) = (icgy-1_8)*cmn%ncgx + icgx
      endif
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij_grid = count(grdidx /= cmf%idx_miss)
  if( nij_grid == 0_8 )then
    call logmsg('Grid   nij: '//str(nij_grid))
  else
    call logmsg('Grid   nij: '//str(nij_grid)//&
              '\n       min: '//str(minval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_river
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_grdidx_river_end(grdidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_river_end'
  integer(8), intent(out) :: grdidx(:,:)

  integer(8) :: icgx, icgy
  integer(8) :: iXX
  integer(8) :: nij_grid

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  grdidx(:,:) = cmf%idx_miss

  do icgy = 1_8, cmn%ncgy
    do icgx = 1_8, cmn%ncgx
      iXX = nextxx(icgx,icgy)

      if( iXX == cmf%nextxy_river_mouth .or. &
          iXX == cmf%nextxy_river_inland )then
        grdidx(icgx,icgy) = (icgy-1_8)*cmn%ncgx + icgx
      endif
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij_grid = count(grdidx /= cmf%idx_miss)
  if( nij_grid == 0_8 )then
    call logmsg('Grid   nij: '//str(nij_grid))
  else
    call logmsg('Grid   nij: '//str(nij_grid)//&
              '\n       min: '//str(minval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_river_end
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_grdidx_river_mouth(grdidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_river_mouth'
  integer(8), intent(out) :: grdidx(:,:)

  integer(8) :: icgx, icgy
  integer(8) :: iXX
  integer(8) :: nij_grid

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  grdidx(:,:) = cmf%idx_miss

  do icgy = 1_8, cmn%ncgy
    do icgx = 1_8, cmn%ncgx
      iXX = nextxx(icgx,icgy)

      if( iXX == cmf%nextxy_river_mouth )then
        grdidx(icgx,icgy) = (icgy-1_8)*cmn%ncgx + icgx
      endif
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij_grid = count(grdidx /= cmf%idx_miss)
  if( nij_grid == 0_8 )then
    call logmsg('Grid   nij: '//str(nij_grid))
  else
    call logmsg('Grid   nij: '//str(nij_grid)//&
              '\n       min: '//str(minval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_river_mouth
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_grdidx_river_inland(grdidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_river_inland'
  integer(8), intent(out) :: grdidx(:,:)

  integer(8) :: icgx, icgy
  integer(8) :: iXX
  integer(8) :: nij_grid

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  grdidx(:,:) = cmf%idx_miss

  do icgy = 1_8, cmn%ncgy
    do icgx = 1_8, cmn%ncgx
      iXX = nextxx(icgx,icgy)

      if( iXX == cmf%nextxy_river_inland )then
        grdidx(icgx,icgy) = (icgy-1_8)*cmn%ncgx + icgx
      endif
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij_grid = count(grdidx /= cmf%idx_miss)
  if( nij_grid == 0_8 )then
    call logmsg('Grid   nij: '//str(nij_grid))
  else
    call logmsg('Grid   nij: '//str(nij_grid)//&
              '\n       min: '//str(minval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_river_inland
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_grdidx_rstidx_noriv(nkij, grdidx, rstidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_rstidx_noriv'
  integer(8), intent(out)   :: nkij
  integer(8), intent(inout) :: grdidx(:,:)
  integer(8), intent(out)   :: rstidx(:,:)

  integer(8) :: kxi, kxf, ikx, kyi, kyf, iky
  integer(8) :: icgx, icgy
  integer(8) :: cgx, cgy
  integer(8) :: idx
  integer(8) :: nij_grid, nij_raster

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  nkij = 0_8
  rstidx(:,:) = cmf%idx_miss

  kyf = 0_8
  do icgy = cgyi, cgyf
    kyi = kyf + 1_8
    kyf = kyf + cmn%nky_grid

    kxf = 0_8
    do icgx = cgxi, cgxf
      kxi = kxf + 1_8
      kxf = kxf + cmn%nkx_grid

      do iky = kyi, kyf
        do ikx = kxi, kxf
          cgx = catmxx(ikx,iky)
          cgy = catmyy(ikx,iky)

          if( cgx == cmf%catmxy_noriv_coastal .or. &
              cgx == cmf%catmxy_noriv_inland )then
            call add(nkij)
            idx = (icgy-1_8)*cmn%ncgx + icgx
            grdidx(icgx,icgy) = idx
            rstidx(ikx,iky) = idx
          endif
        enddo  ! ikx/
      enddo  ! iky/
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij_grid = count(grdidx /= cmf%idx_miss)
  if( nij_grid == 0_8 )then
    call logmsg('Grid   nij: '//str(nij_grid))
  else
    call logmsg('Grid   nij: '//str(nij_grid)//&
              '\n       min: '//str(minval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx))
  endif

  nij_raster = count(rstidx /= cmf%idx_miss)
  if( nij_raster == 0_8 )then
    call logmsg('Raster nij: '//str(nij_raster))
  else
    call logmsg('Raster nij: '//str(nij_raster)//&
              '\n       min: '//str(minval(rstidx,mask=rstidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(rstidx,mask=rstidx/=cmf%idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_rstidx_noriv
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine make_grdidx_rstidx_ocean(nkij, grdidx, rstidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_rstidx_ocean'
  integer(8), intent(out) :: nkij
  integer(8), intent(out) :: grdidx(:,:)
  integer(8), intent(out) :: rstidx(:,:)

  integer(8) :: kxi, kxf, ikx, kyi, kyf, iky
  integer(8) :: icgx, icgy
  integer(8) :: cgx, cgy
  integer(8) :: idx
  integer(8) :: nij_grid, nij_raster

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  nkij = 0_8
  rstidx(:,:) = cmf%idx_miss
  grdidx(:,:) = cmf%idx_miss

  kyf = 0_8
  do icgy = cgyi, cgyf
    kyi = kyf + 1_8
    kyf = kyf + cmn%nky_grid

    kxf = 0_8
    do icgx = cgxi, cgxf
      kxi = kxf + 1_8
      kxf = kxf + cmn%nkx_grid

      do iky = kyi, kyf
        do ikx = kxi, kxf
          cgx = catmxx(ikx,iky)
          cgy = catmyy(ikx,iky)

          if( cgx == cmf%catmxy_ocean )then
            call add(nkij)
            idx = (icgy-1_8)*cmn%ncgx + icgx
            rstidx(ikx,iky) = idx
            grdidx(icgx,icgy) = idx
          endif
        enddo  ! ikx/
      enddo  ! iky/
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij_grid = count(grdidx /= cmf%idx_miss)
  if( nij_grid == 0_8 )then
    call logmsg('Grid   nij: '//str(nij_grid))
  else
    call logmsg('Grid   nij: '//str(nij_grid)//&
              '\n       min: '//str(minval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(grdidx,mask=grdidx/=cmf%idx_miss),dgt_idx))
  endif

  nij_raster = count(rstidx /= cmf%idx_miss)
  if( nij_raster == 0_8 )then
    call logmsg('Raster nij: '//str(nij_raster))
  else
    call logmsg('Raster nij: '//str(nij_raster)//&
              '\n       min: '//str(minval(rstidx,mask=rstidx/=cmf%idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(rstidx,mask=rstidx/=cmf%idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_rstidx_ocean
!---------------------------------------------------------------
subroutine make_rstbsn(rstbsn, grdbsn_1d, grdidx_1d, arg_grdidx_1d, rstidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rstbsn'
  integer(8), intent(out) :: rstbsn(:,:)
  integer(8), intent(in)  :: grdbsn_1d(:)
  integer(8), intent(in)  :: grdidx_1d(:)
  integer(8), intent(in)  :: arg_grdidx_1d(:)
  integer(8), intent(in)  :: rstidx(:,:)

  integer(8) :: iklx, ikly
  integer(8) :: idx, idx_prev
  integer(8) :: loc
  integer(8) :: ij

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  idx_prev = cmf%idx_miss

  do ikly = 1_8, cmn%nkly
    do iklx = 1_8, cmn%nklx
      idx = rstidx(iklx,ikly)
      if( idx == cmf%idx_miss ) cycle

      if( idx /= idx_prev )then
        idx_prev = idx
        call search(idx, grdidx_1d, arg_grdidx_1d, loc)
        if( loc == 0_8 )then
        endif
        ij = arg_grdidx_1d(loc)
      endif

      rstbsn(iklx,ikly) = grdbsn_1d(ij)
    enddo  ! iklx/
  enddo  ! ikly/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_rstbsn
!---------------------------------------------------------------
end subroutine make_cmf
!===============================================================
!
!===============================================================
subroutine make_mat(cmn, cmf, mat, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_mat'
  type(cmn_), intent(in) :: cmn
  type(cmf_), intent(in), target :: cmf
  type(mat_), intent(in), target :: mat
  type(opt_), intent(in), target :: opt

  integer(1), pointer :: grdmsk_river(:,:), &
                         grdmsk_river_end(:,:), &
                         grdmsk_river_mouth(:,:), &
                         grdmsk_river_inland(:,:), &
                         grdmsk_noriv(:,:)
  integer(8), pointer :: grdidx_river(:,:), &
                         grdidx_river_end(:,:), &
                         grdidx_river_mouth(:,:), &
                         grdidx_river_inland(:,:), &
                         grdidx_noriv(:,:)
  integer(8), pointer :: grdidx_bnd_river(:,:), &
                         grdidx_bnd_river_end(:,:), &
                         grdidx_bnd_river_mouth(:,:), &
                         grdidx_bnd_river_inland(:,:), &
                         grdidx_bnd_noriv(:,:)
  integer(8), pointer :: grdidx_mkbnd_river(:,:), &
                         grdidx_mkbnd_noriv(:,:)

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdmsk_river)
  nullify(grdmsk_river_end)
  nullify(grdmsk_river_mouth)
  nullify(grdmsk_river_inland)
  nullify(grdmsk_noriv)

  nullify(grdidx_river)
  nullify(grdidx_river_end)
  nullify(grdidx_river_mouth)
  nullify(grdidx_river_inland)
  nullify(grdidx_noriv)

  nullify(grdidx_bnd_river)
  nullify(grdidx_bnd_river_end)
  nullify(grdidx_bnd_river_mouth)
  nullify(grdidx_bnd_river_inland)
  nullify(grdidx_bnd_noriv)

  nullify(grdidx_mkbnd_river)
  nullify(grdidx_mkbnd_noriv)

  if( .not. mat%make_grdmsk_river .and. &
      .not. mat%make_grdmsk_noriv )then
    call logmsg('Nothing to do')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc_dat(mat%make_grdmsk_river      , &
                            grdmsk_river      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_river      , &
                            grdidx_river      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_bnd_river  , &
                            grdidx_bnd_river  , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_mkbnd_river, &
                            grdidx_mkbnd_river, cmn%ncgx, cmn%ncgy)

  call realloc_dat(mat%make_grdmsk_river_end      , &
                            grdmsk_river_end      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_river_end      , &
                            grdidx_river_end      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_bnd_river_end  , &
                            grdidx_bnd_river_end  , cmn%ncgx, cmn%ncgy)

  call realloc_dat(mat%make_grdmsk_river_mouth      , &
                            grdmsk_river_mouth      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_river_mouth      , &
                            grdidx_river_mouth      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_bnd_river_mouth  , &
                            grdidx_bnd_river_mouth  , cmn%ncgx, cmn%ncgy)

  call realloc_dat(mat%make_grdmsk_river_inland      , &
                            grdmsk_river_inland      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_river_inland      , &
                            grdidx_river_inland      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_bnd_river_inland  , &
                            grdidx_bnd_river_inland  , cmn%ncgx, cmn%ncgy)

  call realloc_dat(mat%make_grdmsk_noriv      , &
                            grdmsk_noriv      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_noriv      , &
                            grdidx_noriv      , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_bnd_noriv  , &
                            grdidx_bnd_noriv  , cmn%ncgx, cmn%ncgy)
  call realloc_dat(mat%make_grdidx_mkbnd_noriv, &
                            grdidx_mkbnd_noriv, cmn%ncgx, cmn%ncgy)
  !-------------------------------------------------------------
  ! Make grid data
  !-------------------------------------------------------------
  call make_mat_grid(&
         mat, cmf, cmn          , &
         grdmsk_river           , &
         grdmsk_river_end       , &
         grdmsk_river_mouth     , &
         grdmsk_river_inland    , &
         grdmsk_noriv           , &
         grdidx_river           , &
         grdidx_river_end       , &
         grdidx_river_mouth     , &
         grdidx_river_inland    , &
         grdidx_noriv           , &
         grdidx_bnd_river       , &
         grdidx_bnd_river_end   , &
         grdidx_bnd_river_mouth , &
         grdidx_bnd_river_inland, &
         grdidx_bnd_noriv       , &
         grdidx_mkbnd_river     , &
         grdidx_mkbnd_noriv)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_mat_raster(&
         mat, cmf, cmn, opt, &
         grdidx_river, &
         grdidx_river_end, &
         grdidx_river_mouth, &
         grdidx_river_inland, &
         grdidx_noriv, &
         grdidx_bnd_river, &
         grdidx_bnd_river_end, &
         grdidx_bnd_river_mouth, &
         grdidx_bnd_river_inland, &
         grdidx_bnd_noriv, &
         grdidx_mkbnd_river, &
         grdidx_mkbnd_noriv)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call realloc(grdmsk_river       , 0)
  call realloc(grdmsk_river_end   , 0)
  call realloc(grdmsk_river_mouth , 0)
  call realloc(grdmsk_river_inland, 0)
  call realloc(grdmsk_noriv       , 0)

  call realloc(grdidx_river       , 0)
  call realloc(grdidx_river_end   , 0)
  call realloc(grdidx_river_mouth , 0)
  call realloc(grdidx_river_inland, 0)
  call realloc(grdidx_noriv       , 0)

  call realloc(grdidx_bnd_river       , 0)
  call realloc(grdidx_bnd_river_end   , 0)
  call realloc(grdidx_bnd_river_mouth , 0)
  call realloc(grdidx_bnd_river_inland, 0)
  call realloc(grdidx_bnd_noriv       , 0)

  call realloc(grdidx_mkbnd_river, 0)
  call realloc(grdidx_mkbnd_noriv, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_mat
!===============================================================
!
!===============================================================
subroutine make_mat_grid(&
    mat, cmf, cmn, &
    grdmsk_river, &
    grdmsk_river_end, &
    grdmsk_river_mouth, &
    grdmsk_river_inland, &
    grdmsk_noriv, &
    grdidx_river, &
    grdidx_river_end, &
    grdidx_river_mouth, &
    grdidx_river_inland, &
    grdidx_noriv, &
    grdidx_bnd_river, &
    grdidx_bnd_river_end, &
    grdidx_bnd_river_mouth, &
    grdidx_bnd_river_inland, &
    grdidx_bnd_noriv, &
    grdidx_mkbnd_river, &
    grdidx_mkbnd_noriv)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_mat_grid'
  type(mat_), intent(in), target :: mat
  type(cmf_), intent(in), target :: cmf
  type(cmn_), intent(in)         :: cmn
  integer(1), intent(out) :: grdmsk_river(:,:)       , &
                             grdmsk_river_end(:,:)   , &
                             grdmsk_river_mouth(:,:) , &
                             grdmsk_river_inland(:,:), &
                             grdmsk_noriv(:,:)           ! out
  integer(8), intent(out) :: grdidx_river(:,:)       , &
                             grdidx_river_end(:,:)   , &
                             grdidx_river_mouth(:,:) , &
                             grdidx_river_inland(:,:), &
                             grdidx_noriv(:,:)           ! out
  integer(8), intent(out) :: grdidx_bnd_river(:,:)       , &
                             grdidx_bnd_river_end(:,:)   , &
                             grdidx_bnd_river_mouth(:,:) , &
                             grdidx_bnd_river_inland(:,:), &
                             grdidx_bnd_noriv(:,:)
  integer(8), intent(out) :: grdidx_mkbnd_river(:,:), &
                             grdidx_mkbnd_noriv(:,:)    ! out

  type(file_), pointer :: f
  integer(8), allocatable :: nextxx(:,:)
  integer, parameter :: layer1 = 1
  integer, parameter :: layer2 = 2
  integer(8) :: ngij_river, ngij_noriv

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ngij_river = -1
  !-------------------------------------------------------------
  ! Make river
  !-------------------------------------------------------------
  if( mat%make_grdmsk_river .or. &
      mat%make_grdidx_river .or. &
      mat%make_grdidx_bnd_river .or. &
      mat%make_grdidx_mkbnd_river )then
    call logent('Making `river`', PRCNAM, MODNAM)

    if( mat%make_grdmsk_river )then
      call make_grdmsk(&
             grdmsk_river, & ! out
             ngij_river  , & ! out
             cmf%f_grdidx_river, 'cmf_grdidx_river', & ! in
             cmf%idx_miss) ! in
    endif

    if( mat%make_grdidx_river )then
      call make_grdidx_model(&
             grdidx_river, & ! out
             0_8, & ! in
             grdmsk_river, mat%idx_miss) ! in
    endif

    if( mat%make_grdidx_bnd_river )then
      call make_grdidx_bnd(&
             grdidx_bnd_river, & ! out
             grdmsk_river, layer1, mat%idx_miss) ! in
    endif

    if( mat%make_grdidx_mkbnd_river )then
      call make_grdidx_bnd(&
             grdidx_mkbnd_river, & ! out
             grdmsk_river, layer1, mat%idx_miss) ! in
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  ! Make noriv
  !-------------------------------------------------------------
  if( mat%make_grdmsk_noriv .or. &
      mat%make_grdidx_noriv .or. &
      mat%make_grdidx_bnd_noriv .or. &
      mat%make_grdidx_mkbnd_noriv )then
    call logent('Making `noriv`', PRCNAM, MODNAM)

    if( mat%make_grdmsk_noriv )then
      call make_grdmsk(&
             grdmsk_noriv, & ! out
             ngij_noriv, & ! out
             cmf%f_grdidx_noriv, 'cmf_grdidx_noriv', & ! in
             cmf%idx_miss) ! in
    endif

    if( mat%make_grdidx_noriv )then
      if( ngij_river < 0_8 )then
        call errend(msg_unexpected_condition()//&
                  '\n  $mat%make_grdidx_noriv is True but $ngij_river < 0')
      endif

      call make_grdidx_model(&
             grdidx_noriv, & ! out
             ngij_river, & ! in
             grdmsk_noriv, mat%idx_miss) ! in
    endif

    if( mat%make_grdidx_bnd_noriv )then
      call make_grdidx_bnd(&
             grdidx_bnd_noriv, & ! out
             grdmsk_noriv, layer2, mat%idx_miss) ! in
    endif

    if( mat%make_grdidx_mkbnd_noriv )then
      call make_grdidx_bnd(&
             grdidx_mkbnd_noriv, & ! out
             grdmsk_noriv, layer1, mat%idx_miss) ! in
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  ! Make river_end, river_mouth, river_inland
  !-------------------------------------------------------------
  allocate(nextxx(cmn%ncgx,cmn%ncgy))

  f => cmf%f_nextxy
  call logmsg('Reading nextxy')
  call traperr( rbin(nextxx, f%path, f%dtype, f%endian, 1) )

  ! river_end
  !-------------------------------------------------------------
  if( mat%make_grdidx_river_end .or. &
      mat%make_grdidx_bnd_river_end )then
    call logent('Making `river_end`', PRCNAM, MODNAM)

    call make_grdmsk_river_end(&
           grdmsk_river_end, & ! out
           nextxx, & ! in
           cmf%nextxy_ocean, cmf%nextxy_river_mouth, & ! in
           cmf%nextxy_river_inland) ! in

    if( mat%make_grdidx_river_end )then
      call mask_grdidx(&
             grdidx_river_end, & ! out
             grdmsk_river_end, grdidx_river, & ! in
             mat%idx_miss) ! in
    endif

    if( mat%make_grdidx_bnd_river_end )then
      call mask_grdidx(&
             grdidx_bnd_river_end, & ! out
             grdmsk_river_end, grdidx_bnd_river, & ! in
             mat%idx_miss) ! in
    endif

    call logext()
  endif

  ! river_mouth
  !-------------------------------------------------------------
  if( mat%make_grdidx_river_mouth .or. &
      mat%make_grdidx_bnd_river_mouth )then
    call logent('Making `river_mouth`', PRCNAM, MODNAM)

    call make_grdmsk_river_mouth(&
           grdmsk_river_mouth, & ! out
           nextxx, & ! in
           cmf%nextxy_ocean, cmf%nextxy_river_mouth, & ! in
           cmf%nextxy_river_inland) ! in

    if( mat%make_grdidx_river_mouth )then
      call mask_grdidx(&
             grdidx_river_mouth, & ! out
             grdmsk_river_mouth, grdidx_river, & ! in
             mat%idx_miss) ! in
    endif

    if( mat%make_grdidx_bnd_river_mouth )then
      call mask_grdidx(&
             grdidx_bnd_river_mouth, & ! out
             grdmsk_river_mouth, grdidx_bnd_river, & ! in
             mat%idx_miss) ! in
    endif

    call logext()
  endif

  ! river_inland
  !-------------------------------------------------------------
  if( mat%make_grdidx_river_inland .or. &
      mat%make_grdidx_bnd_river_inland )then
    call logent('Making `river_inland`', PRCNAM, MODNAM)

    call make_grdmsk_river_inland(&
           grdmsk_river_inland, & ! out
           nextxx, & ! in
           cmf%nextxy_ocean, cmf%nextxy_river_mouth, & ! in
           cmf%nextxy_river_inland) ! in

    if( mat%make_grdidx_river_inland )then
      call mask_grdidx(&
             grdidx_river_inland, & ! out
             grdmsk_river_inland, grdidx_river, & ! in
             mat%idx_miss) ! in
    endif

    if( mat%make_grdidx_bnd_river_inland )then
      call mask_grdidx(&
             grdidx_bnd_river_inland, & ! out
             grdmsk_river_inland, grdidx_bnd_river, & ! in
             mat%idx_miss) ! in
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(nextxx)
  !-------------------------------------------------------------
  ! Check consistency among grdidx of
  ! river, river_end, river_mouth and river_inland
  !-------------------------------------------------------------
  if( mat%make_grdidx_river_end .or. &
      mat%make_grdidx_river_mouth .or. &
      mat%make_grdidx_river_inland )then
    call logent('Checking consistency among grid maps of '//&
                'grid indices of `river`, `river_end`, '//&
                '`river_mouth` and `river_inland`', PRCNAM, MODNAM)

    call check_consistency_grdidx_river(&
           cmn, mat%idx_miss, &
           grdidx_river, grdidx_river_end, grdidx_river_mouth, grdidx_river_inland)

    call logext()
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting', PRCNAM, MODNAM)

  ! grdmsk
  !-------------------------------------------------------------
  f => mat%f_grdmsk_river
  if( f%path /= '' )then
    call logmsg('Writing grdmsk_river')
    call traperr( wbin(grdmsk_river, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdmsk_river_end
  if( f%path /= '' )then
    call logmsg('Writing grdmsk_river_end')
    call traperr( wbin(grdmsk_river_end, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdmsk_river_mouth
  if( f%path /= '' )then
    call logmsg('Writing grdmsk_river_mouth')
    call traperr( wbin(grdmsk_river_mouth, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdmsk_river_inland
  if( f%path /= '' )then
    call logmsg('Writing grdmsk_river_inland')
    call traperr( wbin(grdmsk_river_inland, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdmsk_noriv
  if( f%path /= '' )then
    call logmsg('Writing grdmsk_noriv')
    call traperr( wbin(grdmsk_noriv, f%path, f%dtype, f%endian, f%rec) )
  endif

  ! grdidx
  !-------------------------------------------------------------
  f => mat%f_grdidx_river
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river')
    call traperr( wbin(grdidx_river, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_river_end
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river_end')
    call traperr( wbin(grdidx_river_end, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_river_mouth
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river_mouth')
    call traperr( wbin(grdidx_river_mouth, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_river_inland
  if( f%path /= '' )then
    call logmsg('Writing grdidx_river_inland')
    call traperr( wbin(grdidx_river_inland, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_noriv
  if( f%path /= '' )then
    call logmsg('Writing grdidx_noriv')
    call traperr( wbin(grdidx_noriv, f%path, f%dtype, f%endian, f%rec) )
  endif

  ! grdidx_bnd
  !-------------------------------------------------------------
  f => mat%f_grdidx_bnd_river
  if( f%path /= '' )then
    call logmsg('Writing grdidx_bnd_river')
    call traperr( wbin(grdidx_bnd_river, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_bnd_river_end
  if( f%path /= '' )then
    call logmsg('Writing grdidx_bnd_river_end')
    call traperr( wbin(grdidx_bnd_river_end, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_bnd_river_mouth
  if( f%path /= '' )then
    call logmsg('Writing grdidx_bnd_river_mouth')
    call traperr( wbin(grdidx_bnd_river_mouth, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_bnd_river_inland
  if( f%path /= '' )then
    call logmsg('Writing grdidx_bnd_river_inland')
    call traperr( wbin(grdidx_bnd_river_inland, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_bnd_noriv
  if( f%path /= '' )then
    call logmsg('Writing grdidx_bnd_noriv')
    call traperr( wbin(grdidx_bnd_noriv, f%path, f%dtype, f%endian, f%rec) )
  endif

  ! grdidx_mkbnd
  !-------------------------------------------------------------
  f => mat%f_grdidx_mkbnd_river
  if( f%path /= '' )then
    call logmsg('Writing grdidx_mkbnd_river')
    call traperr( wbin(grdidx_mkbnd_river, f%path, f%dtype, f%endian, f%rec) )
  endif

  f => mat%f_grdidx_mkbnd_noriv
  if( f%path /= '' )then
    call logmsg('Writing grdidx_mkbnd_noriv')
    call traperr( wbin(grdidx_mkbnd_noriv, f%path, f%dtype, f%endian, f%rec) )
  endif

  call logext()
  !-----------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_mat_grid
!===============================================================
!
!===============================================================
subroutine make_mat_raster(&
    mat, cmf, cmn, opt, &
    grdidx_river, &
    grdidx_river_end, &
    grdidx_river_mouth, &
    grdidx_river_inland, &
    grdidx_noriv, &
    grdidx_bnd_river, &
    grdidx_bnd_river_end, &
    grdidx_bnd_river_mouth, &
    grdidx_bnd_river_inland, &
    grdidx_bnd_noriv, &
    grdidx_mkbnd_river, &
    grdidx_mkbnd_noriv)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_mat_raster'
  type(mat_), intent(in), target :: mat
  type(cmf_), intent(in), target :: cmf
  type(cmn_), intent(in)         :: cmn
  type(opt_), intent(in)         :: opt
  integer(8), intent(in) :: grdidx_river(:,:)       , &
                            grdidx_river_end(:,:)   , &
                            grdidx_river_mouth(:,:) , &
                            grdidx_river_inland(:,:), &
                            grdidx_noriv(:,:)
  integer(8), intent(in) :: grdidx_bnd_river(:,:)       , &
                            grdidx_bnd_river_end(:,:)   , &
                            grdidx_bnd_river_mouth(:,:) , &
                            grdidx_bnd_river_inland(:,:), &
                            grdidx_bnd_noriv(:,:)
  integer(8), intent(in) :: grdidx_mkbnd_river(:,:), &
                            grdidx_mkbnd_noriv(:,:)

  type(file_)         , pointer :: f
  character(clen_path), pointer :: path
  integer(8), allocatable :: catmxx(:,:), catmyy(:,:)
  integer :: iTile
  integer(8) :: cgxi, cgxf, cgyi, cgyf
  integer(8) :: kgxi, kgxf, kgyi, kgyf
  integer    :: west, east, south, north
  integer(8) :: nkij_river       , &
                nkij_river_end   , &
                nkij_river_mouth , &
                nkij_river_inland, &
                nkij_noriv       , &
                nkij_ocean
  integer :: dgt_kgxy, dgt_kij

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Make raster data
  !-------------------------------------------------------------
  dgt_kgxy = dgt((/cmn%nkgx,cmn%nkgy/),dgt_opt_max)
  dgt_kij = dgt(cmn%nkgx*cmn%nkgy)

  allocate(catmxx(cmn%nklx,cmn%nkly))
  allocate(catmyy(cmn%nklx,cmn%nkly))

  do iTile = 1, cmn%nTiles
    if( cmn%is_tiled )&
    call logent('Tile '//str(iTile)//' / '//str(cmn%nTiles), PRCNAM, MODNAM)
    !-----------------------------------------------------------
    ! Read catmxy
    !-----------------------------------------------------------
    call logent('Reading catmxy', PRCNAM, MODNAM)

    if( .not. cmn%is_tiled )then
      cgxi = 1_8
      cgxf = cmn%ncgx
      cgyi = 1_8
      cgyf = cmn%ncgy

      f => cmf%f_catmxy
      call traperr( rbin(catmxx, f%path, f%dtype, f%endian, 1) )
      call traperr( rbin(catmyy, f%path, f%dtype, f%endian, 2) )
    else
      path => cmf%list_path_catmxy(iTile)

      call get_value_bounds_tile(&
             cmn, filename(path), west, east, south, north, &
             cgxi, cgxf, cgyi, cgyf, &
             kgxi, kgxf, kgyi, kgyf)
      call logmsg('  grid  ['//str((/cgxi,cgxf/),dgt_kgxy,':')//&
                         ', '//str((/cgyi,cgyf/),dgt_kgxy,':')//']')
      call logmsg('  raster['//str((/kgxi,kgxf/),dgt_kgxy,':')//&
                         ', '//str((/kgyi,kgyf/),dgt_kgxy,':')//']')

      call traperr( rbin(catmxx, path, cmf%dtype_catmxy, cmf%endian_catmxy, 1) )
      call traperr( rbin(catmyy, path, cmf%dtype_catmxy, cmf%endian_catmxy, 2) )
    endif

    call logext()
    !-----------------------------------------------------------
    ! rstidx
    !-----------------------------------------------------------
    call make_mat_rstidx(&
           mat, cmf, cmn, opt, &
           catmxx, catmyy, &
           grdidx_river, grdidx_river_end, &
           grdidx_river_mouth, grdidx_river_inland, &
           grdidx_noriv, &
           iTile, cgxi, cgxf, cgyi, cgyf, &
           nkij_river, nkij_river_end, &
           nkij_river_mouth, nkij_river_inland, &
           nkij_noriv, nkij_ocean)
    !-----------------------------------------------------------
    ! rstidx_bnd
    !-----------------------------------------------------------
    call make_mat_rstidx_bnd(&
           mat, cmf, cmn, opt, &
           catmxx, catmyy, &
           grdidx_bnd_river, grdidx_bnd_river_end, &
           grdidx_bnd_river_mouth, grdidx_bnd_river_inland, &
           grdidx_bnd_noriv, &
           iTile, cgxi, cgxf, cgyi, cgyf)
    !-----------------------------------------------------------
    ! Make rstidx_mkbnd of river
    !-----------------------------------------------------------
    call make_mat_rstidx_mkbnd(&
           mat, cmf, cmn, opt, &
           catmxx, catmyy, &
           grdidx_mkbnd_river, grdidx_mkbnd_noriv, &
           iTile, cgxi, cgxf, cgyi, cgyf)
    !-----------------------------------------------------------
    ! Summary
    !-----------------------------------------------------------
    call logmsg('Raster')
    call logmsg('  river     : '//str(nkij_river       ,dgt_kij)//&
                             ' '//str(mat%make_rstidx_river))
    call logmsg('    end     : '//str(nkij_river_end   ,dgt_kij)//&
                             ' '//str(mat%make_rstidx_river_end   ))
    call logmsg('      mouth : '//str(nkij_river_mouth ,dgt_kij)//&
                             ' '//str(mat%make_rstidx_river_mouth ))
    call logmsg('      inland: '//str(nkij_river_inland,dgt_kij)//&
                             ' '//str(mat%make_rstidx_river_inland))
    call logmsg('  noriv     : '//str(nkij_noriv       ,dgt_kij)//&
                             ' '//str(mat%make_rstidx_noriv))
    call logmsg('  ocean     : '//str(nkij_ocean       ,dgt_kij)//&
                             ' '//str(mat%make_rstidx_ocean))
    call logmsg('  Total     : '//str(nkij_river+nkij_noriv+nkij_ocean,dgt_kij)//&
                ' (river + noriv + ocean)')
    !-----------------------------------------------------------
    if( cmn%is_tiled ) call logext()
    !-----------------------------------------------------------
  enddo  ! iTile/
  !-----------------------------------------------------------
  !
  !-----------------------------------------------------------
  deallocate(catmxx)
  deallocate(catmyy)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_mat_raster
!===============================================================
!
!===============================================================
subroutine make_mat_rstidx(&
    mat, cmf, cmn, opt, &
    catmxx, catmyy, &
    grdidx_river, grdidx_river_end, &
    grdidx_river_mouth, grdidx_river_inland, &
    grdidx_noriv, &
    iTile, cgxi, cgxf, cgyi, cgyf, &
    nkij_river, nkij_river_end, &
    nkij_river_mouth, nkij_river_inland, &
    nkij_noriv, nkij_ocean)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_mat_rstidx'
  type(mat_), intent(in), target :: mat
  type(cmf_), intent(in), target :: cmf
  type(cmn_), intent(in)         :: cmn
  type(opt_), intent(in)         :: opt
  integer(8), intent(in) :: catmxx(:,:), catmyy(:,:)
  integer(8), intent(in) :: grdidx_river(:,:)       , &
                            grdidx_river_end(:,:)   , &
                            grdidx_river_mouth(:,:) , &
                            grdidx_river_inland(:,:), &
                            grdidx_noriv(:,:)
  integer   , intent(in) :: iTile
  integer(8), intent(in) :: cgxi, cgxf, cgyi, cgyf
  integer(8), intent(out) :: nkij_river       , &
                             nkij_river_end   , &
                             nkij_river_mouth , &
                             nkij_river_inland, &
                             nkij_noriv       , &
                             nkij_ocean

  type(file_)         , pointer :: f
  character(clen_path), pointer :: path
  integer(8), pointer, save :: rstidx_river(:,:)       , &
                               rstidx_river_end(:,:)   , &
                               rstidx_river_mouth(:,:) , &
                               rstidx_river_inland(:,:), &
                               rstidx_noriv(:,:)       , &
                               rstidx_ocean(:,:)
  integer(1), pointer, save :: grdstat_river(:,:)      , &
                               grdstat_river_end(:,:)  , &
                               grdstat_river_mouth(:,:), &
                               grdstat_river_inland(:,:)
  integer :: iComp

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( iTile == 1 )then
    nullify(rstidx_river)
    nullify(rstidx_river_end)
    nullify(rstidx_river_mouth)
    nullify(rstidx_river_inland)
    nullify(rstidx_noriv)
    nullify(rstidx_ocean)

    nullify(grdstat_river)
    nullify(grdstat_river_end)
    nullify(grdstat_river_mouth)
    nullify(grdstat_river_inland)
  endif

  if( .not. opt%save_memory .and. iTile == 1 )then
    call realloc_dat(mat%make_rstidx_river       , &
                              rstidx_river       , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_river_end   , &
                              rstidx_river_end   , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_river_mouth , &
                              rstidx_river_mouth , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_river_inland, &
                              rstidx_river_inland, cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_noriv       , &
                              rstidx_noriv       , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_ocean       , &
                              rstidx_ocean       , cmn%nklx, cmn%nkly)
  endif

  if( iTile == 1 )then
    call realloc_dat(mat%make_rstidx_river       , &
                             grdstat_river       , cmn%ncgx, cmn%ncgx)
    call realloc_dat(mat%make_rstidx_river_end   , &
                             grdstat_river_end   , cmn%ncgx, cmn%ncgy)
    call realloc_dat(mat%make_rstidx_river_mouth , &
                             grdstat_river_mouth , cmn%ncgx, cmn%ncgy)
    call realloc_dat(mat%make_rstidx_river_inland, &
                             grdstat_river_inland, cmn%ncgx, cmn%ncgy)
  endif

  nkij_river        = 0_8
  nkij_river_end    = 0_8
  nkij_river_mouth  = 0_8
  nkij_river_inland = 0_8
  nkij_noriv        = 0_8
  nkij_ocean        = 0_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%save_memory )then
    call realloc_dat(mat%make_rstidx_river       , &
                              rstidx_river       , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_river_end   , &
                              rstidx_river_end   , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_river_mouth , &
                              rstidx_river_mouth , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_river_inland, &
                              rstidx_river_inland, cmn%nklx, cmn%nkly)
  endif
  !-------------------------------------------------------------
  ! Make river, river_end, river_mouth, river_inland
  !-------------------------------------------------------------
  call logent('Making `river`, `river_end`, '//&
              '`river_mouth` and `river_inland`', PRCNAM, MODNAM)

  if( mat%make_rstidx_river )then
    call make_rstidx_river_from_grdidx(&
           nkij_river, rstidx_river,     & ! out
           cmn,                          & ! in
           catmxx, catmyy, grdidx_river, & ! in
           mat%idx_miss, .true., 'river')  ! in
  endif

  if( mat%make_rstidx_river_end )then
    call make_rstidx_river_from_grdidx(&
           nkij_river_end, rstidx_river_end, & ! out
           cmn,                              & ! in
           catmxx, catmyy, grdidx_river_end, & ! in
           mat%idx_miss, .false., 'river_end') ! in
  endif

  if( mat%make_rstidx_river_mouth )then
    call make_rstidx_river_from_grdidx(&
           nkij_river_mouth, rstidx_river_mouth, & ! out
           cmn,                                  & ! in
           catmxx, catmyy, grdidx_river_mouth,   & ! in
           mat%idx_miss, .false., 'river_mouth')   ! in
  endif

  if( mat%make_rstidx_river_inland )then
    call make_rstidx_river_from_grdidx(&
           nkij_river_inland, rstidx_river_inland, & ! out
           cmn,                                    & ! in
           catmxx, catmyy, grdidx_river_inland,    & ! in
           mat%idx_miss, .false., 'river_inland')    ! in
  endif

  call logext()
  !-------------------------------------------------------------
  ! Update grdstat
  !-------------------------------------------------------------
  call logent('Updating grid status', PRCNAM, MODNAM)

  if( mat%make_rstidx_river )then
    call logent('river', PRCNAM, MODNAM)

    call check_if_rstidx_in_grdidx(&
           cmn, mat%idx_miss, & ! in
           grdidx_river, rstidx_river, & ! in
           grdstat_river) ! inout

    call logext()
  endif

  if( mat%make_rstidx_river_end )then
    call logent('river_end', PRCNAM, MODNAM)

    call check_if_rstidx_in_grdidx(&
           cmn, mat%idx_miss, & ! in
           grdidx_river_end, rstidx_river_end, & ! in
           grdstat_river_end) ! inout

    call logext()
  endif

  if( mat%make_rstidx_river_mouth )then
    call logent('river_mouth', PRCNAM, MODNAM)

    call check_if_rstidx_in_grdidx(&
           cmn, mat%idx_miss, & ! in
           grdidx_river_mouth, rstidx_river_mouth, & ! in
           grdstat_river_mouth) ! inout

    call logext()
  endif

  if( mat%make_rstidx_river_inland )then
    call logent('river_inland', PRCNAM, MODNAM)

    call check_if_rstidx_in_grdidx(&
           cmn, mat%idx_miss, & ! in
           grdidx_river_inland, rstidx_river_inland, & ! in
           grdstat_river_inland) ! inout

    call logext()
  endif

  call logext()
  !-------------------------------------------------------------
  ! Chek consistencies
  !-------------------------------------------------------------

  !   among river, river_end, river_mouth, river_inland
  !-------------------------------------------------------------
  call logent('Checking consistency among raster maps of '//&
              'grid indices of `river`, `river_end`, '//&
              '`river_mouth` and `river_inland`', PRCNAM, MODNAM)

  call check_consistency_rstidx_river(&
         cmn, mat%idx_miss, &
         rstidx_river, rstidx_river_end, rstidx_river_mouth, rstidx_river_inland)

  call logext()

  ! grdstat
  !-------------------------------------------------------------
  if( iTile == cmn%nTiles )then
    call logent('Checking consistency between '//&
                'grid indices and raster indices', PRCNAM, MODNAM)

    if( mat%make_rstidx_river )then
      call mat_check_if_grdidx_in_rstidx(&
             'river', &
             mat%idx_miss, grdidx_river, grdstat_river)
    endif

    if( mat%make_rstidx_river_end )then
      call mat_check_if_grdidx_in_rstidx(&
             'river_end', &
             mat%idx_miss, grdidx_river_end, grdstat_river_end)
    endif

    if( mat%make_rstidx_river_mouth )then
      call mat_check_if_grdidx_in_rstidx(&
             'river_mouth', &
             mat%idx_miss, grdidx_river_mouth, grdstat_river_mouth)
    endif

    if( mat%make_rstidx_river_inland )then
      call mat_check_if_grdidx_in_rstidx(&
             'river_inland', &
             mat%idx_miss, grdidx_river_inland, grdstat_river_inland)
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting', PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Case: Untiled
  if( .not. cmn%is_tiled )then
    f => mat%f_rstidx_river_end
    if( f%path /= '' )then
      call logmsg('Writing rstidx_river_end')
      call traperr( wbin(rstidx_river_end, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_river_mouth
    if( f%path /= '' )then
      call logmsg('Writing rstidx_river_mouth')
      call traperr( wbin(rstidx_river_mouth, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_river_inland
    if( f%path /= '' )then
      call logmsg('Writing rstidx_river_inland')
      call traperr( wbin(rstidx_river_inland, f%path, f%dtype, f%endian, f%rec) )
    endif
  !-------------------------------------------------------------
  ! Case: Tiled
  else
    if( mat%dir_rstidx_river_end /= '' )then
      path => mat%list_path_rstidx_river_end(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_river_end')
        call traperr( wbin(rstidx_river_end, path, mat%dtype_rstidx, mat%endian_rstidx, 1) )
      endif
    endif

    if( mat%dir_rstidx_river_mouth /= '' )then
      path => mat%list_path_rstidx_river_mouth(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_river_mouth')
        call traperr( wbin(rstidx_river_mouth, path, mat%dtype_rstidx, mat%endian_rstidx, 1) )
      endif
    endif

    if( mat%dir_rstidx_river_inland /= '' )then
      path => mat%list_path_rstidx_river_inland(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_river_inland')
        call traperr( wbin(rstidx_river_inland, path, mat%dtype_rstidx, mat%endian_rstidx, 1) )
      endif
    endif
  endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%save_memory )then
    call realloc(rstidx_river_end   , 0)
    call realloc(rstidx_river_mouth , 0)
    call realloc(rstidx_river_inland, 0)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%save_memory )then
    call realloc_dat(mat%make_rstidx_noriv, &
                              rstidx_noriv, cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_ocean, &
                              rstidx_ocean, cmn%nklx, cmn%nkly)
  endif
  !-------------------------------------------------------------
  ! Make noriv, ocean
  !-------------------------------------------------------------
  call logent('Making noriv and ocean', PRCNAM, MODNAM)

  if( mat%make_rstidx_noriv )then
    call make_rstidx_noriv_from_grdidx(&
           nkij_noriv, rstidx_noriv, & ! out
           cmn, cgxi, cgxf, cgyi, cgyf, & ! in
           catmxx, catmyy, grdidx_noriv, & ! in
           cmf%catmxy_noriv_coastal, cmf%catmxy_noriv_inland, & ! in
           mat%idx_miss) ! in
  endif

  ! TODO
  if( mat%make_rstidx_ocean )then

  endif

  call logext()
  !-------------------------------------------------------------
  ! Check consistency
  !-------------------------------------------------------------
  ! among river, noriv and ocean
  !-------------------------------------------------------------
  iComp = 0
  if( mat%make_rstidx_river ) call add(iComp)
  if( mat%make_rstidx_noriv ) call add(iComp)
  if( mat%make_rstidx_ocean ) call add(iComp)

  if( iComp >= 2 )then
    call logent('Checking consistency among river, noriv and ocean', &
                PRCNAM, MODNAM)

    call check_consistency_rstidx_validity(&
           cmn, mat%idx_miss, &
           rstidx_river, rstidx_noriv, rstidx_ocean)

    call logext()
  endif

  ! rstidx_noriv
  !-------------------------------------------------------------
  if( mat%make_rstidx_noriv )then
    call logent('Checking consistency of rstidx_noriv', PRCNAM, MODNAM)

    call check_consistency_grdidx_rstidx_rect(&
           cmn, mat%idx_miss, cgxi, cgxf, cgyi, cgyf, &
           grdidx_noriv, rstidx_noriv)

    call logext()
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting', PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Case: Untiled
  if( .not. cmn%is_tiled )then
    f => mat%f_rstidx_river
    if( f%path /= '' )then
      call logmsg('Writing rstidx_river')
      call traperr( wbin(rstidx_river, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_noriv
    if( f%path /= '' )then
      call logmsg('Writing rstidx_noriv')
      call traperr( wbin(rstidx_noriv, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_ocean
    if( f%path /= '' )then
      call logmsg('Writing rstidx_ocean')
      call traperr( wbin(rstidx_ocean, f%path, f%dtype, f%endian, f%rec) )
    endif
  !-------------------------------------------------------------
  ! Case: Tiled
  else
    if( mat%dir_rstidx_river /= '' )then
      path => mat%list_path_rstidx_river(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_river')
        call traperr( wbin(rstidx_river, path, mat%dtype_rstidx, mat%endian_rstidx, 1) )
      endif
    endif

    if( mat%dir_rstidx_noriv /= '' )then
      path => mat%list_path_rstidx_noriv(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_noriv')
        call traperr( wbin(rstidx_noriv, path, mat%dtype_rstidx, mat%endian_rstidx, 1) )
      endif
    endif

    if( mat%dir_rstidx_ocean /= '' )then
      path => mat%list_path_rstidx_ocean(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_ocean')
        call traperr( wbin(rstidx_ocean, path, mat%dtype_rstidx, mat%endian_rstidx, 1) )
      endif
    endif
  endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%save_memory )then
    call realloc(rstidx_river, 0)
    call realloc(rstidx_noriv, 0)
    call realloc(rstidx_ocean, 0)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. opt%save_memory .and. iTile == cmn%nTiles )then
    call realloc(rstidx_river       , 0)
    call realloc(rstidx_river_end   , 0)
    call realloc(rstidx_river_mouth , 0)
    call realloc(rstidx_river_inland, 0)
    call realloc(rstidx_noriv       , 0)
    call realloc(rstidx_ocean       , 0)
  endif

  if( iTile == cmn%nTiles )then
    call realloc(grdstat_river       , 0)
    call realloc(grdstat_river_end   , 0)
    call realloc(grdstat_river_mouth , 0)
    call realloc(grdstat_river_inland, 0)
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_mat_rstidx
!===============================================================
!
!===============================================================
subroutine make_mat_rstidx_bnd(&
    mat, cmf, cmn, opt, &
    catmxx, catmyy, &
    grdidx_bnd_river, grdidx_bnd_river_end, &
    grdidx_bnd_river_mouth, grdidx_bnd_river_inland, &
    grdidx_bnd_noriv, &
    iTile, cgxi, cgxf, cgyi, cgyf)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_mat_rstidx_bnd'
  type(mat_), intent(in), target :: mat
  type(cmf_), intent(in), target :: cmf
  type(cmn_), intent(in)         :: cmn
  type(opt_), intent(in)         :: opt
  integer(8), intent(in) :: catmxx(:,:), catmyy(:,:)
  integer(8), intent(in) :: grdidx_bnd_river(:,:)       , &
                            grdidx_bnd_river_end(:,:)   , &
                            grdidx_bnd_river_mouth(:,:) , &
                            grdidx_bnd_river_inland(:,:), &
                            grdidx_bnd_noriv(:,:)
  integer   , intent(in) :: iTile
  integer(8), intent(in) :: cgxi, cgxf, cgyi, cgyf

  type(file_)         , pointer :: f
  character(clen_path), pointer :: path
  integer(8), pointer, save :: rstidx_bnd_river(:,:)       , &
                               rstidx_bnd_river_end(:,:)   , &
                               rstidx_bnd_river_mouth(:,:) , &
                               rstidx_bnd_river_inland(:,:), &
                               rstidx_bnd_noriv(:,:)       , &
                               rstidx_bnd_ocean(:,:)
  integer(8) :: nkij_dummy
  integer :: iComp

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(f)
  nullify(path)

  if( opt%save_memory .or. iTile == 1 )then
    call realloc_dat(mat%make_rstidx_bnd_river       , &
                              rstidx_bnd_river       , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_bnd_river_end   , &
                              rstidx_bnd_river_end   , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_bnd_river_mouth , &
                              rstidx_bnd_river_mouth , cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_bnd_river_inland, &
                              rstidx_bnd_river_inland, cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_bnd_noriv       , &
                              rstidx_bnd_noriv       , cmn%nklx, cmn%nkly)
    call realloc_dat(.false.                         , &
                              rstidx_bnd_ocean       , cmn%nklx, cmn%nkly)
  endif
  !-------------------------------------------------------------
  ! Make data
  !-------------------------------------------------------------
  call logent('Making data', PRCNAM, MODNAM)

  if( mat%make_rstidx_bnd_river )then
    call make_rstidx_river_from_grdidx(&
           nkij_dummy, rstidx_bnd_river,     & ! out
           cmn,                              & ! in
           catmxx, catmyy, grdidx_bnd_river, & ! in
           mat%idx_miss, .true., 'bnd_river')  ! in
  endif

  if( mat%make_rstidx_bnd_river_end )then
    call make_rstidx_river_from_grdidx(&
           nkij_dummy, rstidx_bnd_river_end,     & ! out
           cmn,                                  & ! in
           catmxx, catmyy, grdidx_bnd_river_end, & ! in
           mat%idx_miss, .false., 'bnd_river_end') ! in
  endif

  if( mat%make_rstidx_bnd_river_mouth )then
    call make_rstidx_river_from_grdidx(&
           nkij_dummy, rstidx_bnd_river_mouth,     & ! out
           cmn,                                    & ! in
           catmxx, catmyy, grdidx_bnd_river_mouth, & ! in
           mat%idx_miss, .false., 'bnd_river_mouth') ! in
  endif

  if( mat%make_rstidx_bnd_river_inland )then
    call make_rstidx_river_from_grdidx(&
           nkij_dummy, rstidx_bnd_river_inland,     & ! out
           cmn,                                     & ! in
           catmxx, catmyy, grdidx_bnd_river_inland, & ! in
           mat%idx_miss, .false., 'bnd_river_inland') ! in
  endif

  if( mat%make_rstidx_bnd_noriv )then
    call make_rstidx_noriv_from_grdidx(&
           nkij_dummy, rstidx_bnd_noriv, & ! out
           cmn, cgxi, cgxf, cgyi, cgyf, & ! in
           catmxx, catmyy, grdidx_bnd_noriv, & ! in
           cmf%catmxy_noriv_coastal, cmf%catmxy_noriv_inland, & ! in
           mat%idx_miss) ! in
  endif

  call logext()
  !-------------------------------------------------------------
  ! Check consistency
  !-------------------------------------------------------------
  iComp = 0
  if( mat%make_rstidx_bnd_river ) call add(iComp)
  if( mat%make_rstidx_bnd_noriv ) call add(iComp)
  !if( mat%make_rstidx_bnd_ocean ) call add(iComp)

  if( iComp >= 2 )then
    call logent('Checking consistency', PRCNAM, MODNAM)

    call check_consistency_rstidx_validity(&
           cmn, mat%idx_miss, &
           rstidx_bnd_river, rstidx_bnd_noriv, rstidx_bnd_ocean)

    call logext()
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting', PRCNAM, MODNAM)
  !---------------------------------------------------------
  ! Case: Untiled
  if( .not. cmn%is_tiled )then
    f => mat%f_rstidx_bnd_river
    if( f%path /= '' )then
      call logmsg('Writing rstidx_bnd_river')
      call traperr( wbin(rstidx_bnd_river, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_bnd_river_end
    if( f%path /= '' )then
      call logmsg('Writing rstidx_bnd_river_end')
      call traperr( wbin(rstidx_bnd_river_end, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_bnd_river_mouth
    if( f%path /= '' )then
      call logmsg('Writing rstidx_bnd_river_mouth')
      call traperr( wbin(rstidx_bnd_river_mouth, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_bnd_river_inland
    if( f%path /= '' )then
      call logmsg('Writing rstidx_bnd_river_inland')
      call traperr( wbin(rstidx_bnd_river_inland, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_bnd_noriv
    if( f%path /= '' )then
      call logmsg('Writing rstidx_bnd_noriv')
      call traperr( wbin(rstidx_bnd_noriv, f%path, f%dtype, f%endian, f%rec) )
    endif
  !-------------------------------------------------------------
  ! Case: Tiled
  else
    if( mat%dir_rstidx_bnd_river /= '' )then
      path => mat%list_path_rstidx_bnd_river(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_bnd_river')
        call traperr( wbin(rstidx_bnd_river, & 
                  path, mat%dtype_rstidx_bnd, mat%endian_rstidx_bnd, 1) )
      endif
    endif

    if( mat%dir_rstidx_bnd_river_end /= '' )then
      path => mat%list_path_rstidx_bnd_river_end(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_bnd_river_end')
        call traperr( wbin(rstidx_bnd_river_end, &
                  path, mat%dtype_rstidx_bnd, mat%endian_rstidx_bnd, 1) )
      endif
    endif

    if( mat%dir_rstidx_bnd_river_mouth /= '' )then
      path => mat%list_path_rstidx_bnd_river_mouth(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_bnd_river_mouth')
        call traperr( wbin(rstidx_bnd_river_mouth, &
                  path, mat%dtype_rstidx_bnd, mat%endian_rstidx_bnd, 1) )
      endif
    endif

    if( mat%dir_rstidx_bnd_river_inland /= '' )then
      path => mat%list_path_rstidx_bnd_river_inland(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_bnd_river_inland')
        call traperr( wbin(rstidx_bnd_river_inland, &
                  path, mat%dtype_rstidx_bnd, mat%endian_rstidx_bnd, 1) )
      endif
    endif

    if( mat%dir_rstidx_bnd_noriv /= '' )then
      path => mat%list_path_rstidx_bnd_noriv(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_bnd_noriv')
        call traperr( wbin(rstidx_bnd_noriv, &
                  path, mat%dtype_rstidx_bnd, mat%endian_rstidx_bnd, 1) )
      endif
    endif
  endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%save_memory .or. iTile == cmn%nTiles )then
    call realloc(rstidx_bnd_river       , 0)
    call realloc(rstidx_bnd_river_end   , 0)
    call realloc(rstidx_bnd_river_mouth , 0)
    call realloc(rstidx_bnd_river_inland, 0)
    call realloc(rstidx_bnd_noriv       , 0)
    call realloc(rstidx_bnd_ocean       , 0)
  endif

  nullify(f)
  nullify(path)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_mat_rstidx_bnd
!===============================================================
!
!===============================================================
subroutine make_mat_rstidx_mkbnd(&
    mat, cmf, cmn, opt, &
    catmxx, catmyy, &
    grdidx_mkbnd_river, grdidx_mkbnd_noriv, &
    iTile, cgxi, cgxf, cgyi, cgyf)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_mat_rstidx_mkbnd'
  type(mat_), intent(in), target :: mat
  type(cmf_), intent(in), target :: cmf
  type(cmn_), intent(in)         :: cmn
  type(opt_), intent(in)         :: opt
  integer(8), intent(in) :: catmxx(:,:), catmyy(:,:)
  integer(8), intent(in) :: grdidx_mkbnd_river(:,:), &
                            grdidx_mkbnd_noriv(:,:)
  integer   , intent(in) :: iTile
  integer(8), intent(in) :: cgxi, cgxf, cgyi, cgyf

  type(file_)         , pointer :: f
  character(clen_path), pointer :: path
  integer(8), pointer, save :: rstidx_mkbnd_river(:,:), &
                               rstidx_mkbnd_noriv(:,:)
  integer(8) :: nkij_dummy

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(f)
  nullify(path)

  if( opt%save_memory .or. iTile == 1 )then
    call realloc_dat(mat%make_rstidx_mkbnd_river, &
                              rstidx_mkbnd_river, cmn%nklx, cmn%nkly)
    call realloc_dat(mat%make_rstidx_mkbnd_noriv, &
                              rstidx_mkbnd_noriv, cmn%nklx, cmn%nkly)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( mat%make_rstidx_mkbnd_river )then
    call make_rstidx_river_from_grdidx(&
           nkij_dummy, rstidx_mkbnd_river,     & ! out
           cmn,                                & ! in
           catmxx, catmyy, grdidx_mkbnd_river, & ! in
           mat%idx_miss, .true., 'mkbnd_river')  ! in
  endif

  if( mat%make_rstidx_mkbnd_noriv )then
    call make_rstidx_noriv_from_grdidx(&
           nkij_dummy, rstidx_mkbnd_noriv,     & ! out
           cmn, cgxi, cgxf, cgyi, cgyf,        & ! in
           catmxx, catmyy, grdidx_mkbnd_noriv, & ! in
           cmf%catmxy_noriv_coastal,           & ! in
           cmf%catmxy_noriv_inland,            & ! in
           mat%idx_miss)                         ! in
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting', PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Case: Untiled
  if( .not. cmn%is_tiled )then
    f => mat%f_rstidx_mkbnd_river
    if( f%path /= '' )then
      call logmsg('Writing rstidx_mkbnd_river')
      call traperr( wbin(rstidx_mkbnd_river, f%path, f%dtype, f%endian, f%rec) )
    endif

    f => mat%f_rstidx_mkbnd_noriv
    if( f%path /= '' )then
      call logmsg('Writing rstidx_mkbnd_noriv')
      call traperr( wbin(rstidx_mkbnd_noriv, f%path, f%dtype, f%endian, f%rec) )
    endif
  !-------------------------------------------------------------
  ! Case: Tiled
  else
    if( mat%dir_rstidx_mkbnd_river /= '' )then
      path => mat%list_path_rstidx_mkbnd_river(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_mkbnd_river')
        call traperr( wbin(rstidx_mkbnd_river, &
                  path, mat%dtype_rstidx_mkbnd, mat%endian_rstidx_mkbnd, 1) )
      endif
    endif

    if( mat%dir_rstidx_mkbnd_noriv /= '' )then
      path => mat%list_path_rstidx_mkbnd_noriv(iTile)
      if( path /= '' )then
        call logmsg('Writing rstidx_mkbnd_noriv')
        call traperr( wbin(rstidx_mkbnd_noriv, &
                  path, mat%dtype_rstidx_mkbnd, mat%endian_rstidx_mkbnd, 1) )
      endif
    endif
  endif

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%save_memory .or. iTile == cmn%nTiles )then
    call realloc(rstidx_mkbnd_river, 0)
    call realloc(rstidx_mkbnd_noriv, 0)
  endif

  nullify(f)
  nullify(path)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_mat_rstidx_mkbnd
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
subroutine make_grdmsk(&
    grdmsk, ngij, &
    f_cmf_grdidx, nam_f, cmf_idx_miss)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdmsk'
  integer(1)  , intent(out)        :: grdmsk(:,:)
  integer(8)  , intent(out)        :: ngij
  type(file_) , intent(in), target :: f_cmf_grdidx
  character(*), intent(in)         :: nam_f
  integer(8)  , intent(in)         :: cmf_idx_miss

  type(file_), pointer :: f
  integer(8), allocatable :: cmf_grdidx(:,:)
  integer(8) :: cmf_idx
  integer(8) :: ncgx, ncgy
  integer(8) :: icgx, icgy

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ncgx = size(grdmsk,1)
  ncgy = size(grdmsk,2)

  allocate(cmf_grdidx(ncgx,ncgy))

  f => f_cmf_grdidx
  call logmsg('Reading '//str(nam_f))
  call traperr( rbin(cmf_grdidx, f%path, f%dtype, f%endian, f%rec) )

  ngij = 0_8

  do icgy = 1, ncgy
    do icgx = 1, ncgx
      cmf_idx = cmf_grdidx(icgx,icgy)
      if( cmf_idx > 0_8 )then
        call add(ngij)
        grdmsk(icgx,icgy) = 1_1
      elseif( cmf_idx == cmf_idx_miss )then
        grdmsk(icgx,icgy) = 0_1
      else
        call errend(msg_invalid_value('cmf_grdidx('//str((/icgx,icgy/),',')//')', cmf_idx))
      endif
    enddo  ! icgx/
  enddo  ! icgy/

  call logmsg('ngij: '//str(ngij))

  deallocate(cmf_grdidx)
  nullify(f)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdmsk
!===============================================================
!
!===============================================================
subroutine make_grdidx_model(&
    grdidx, &
    idx0, &
    grdmsk, idx_miss)
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_model'
  integer(8), intent(out) :: grdidx(:,:)
  integer(8), intent(in)  :: idx0
  integer(1), intent(in)  :: grdmsk(:,:)
  integer(8), intent(in)  :: idx_miss

  integer(8) :: ncgx, ncgy
  integer(8) :: icgx, icgy
  integer(8) :: idx

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ncgx = size(grdidx,1)
  ncgy = size(grdidx,2)

  idx = idx0

  do icgy = 1_8, ncgy
    do icgx = 1_8, ncgx
      selectcase( grdmsk(icgx,icgy) )
      case( 0_1 )
        grdidx(icgx,icgy) = idx_miss
      case( 1_1 )
        call add(idx)
        grdidx(icgx,icgy) = idx
      case default
        call errend(msg_invalid_value('grdmsk('//str((/icgx,icgy/),',')//')', grdmsk(icgx,icgy)))
      endselect
    enddo  ! icgx/
  enddo  ! icgy/

  call logmsg('idx min: '//str(idx0+1_8)//', max: '//str(idx))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_model
!===============================================================
!
!===============================================================
subroutine make_grdidx_bnd(&
    grdidx, &
    grdmsk, layer, idx_miss)
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdidx_bnd'
  integer(8), intent(out) :: grdidx(:,:)
  integer(1), intent(in)  :: grdmsk(:,:)
  integer   , intent(in)  :: layer
  integer(8), intent(in)  :: idx_miss

  integer(8) :: ncgx, ncgy
  integer(8) :: icgx, icgy
  integer(8) :: idx0

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ncgx = size(grdidx,1)
  ncgy = size(grdidx,2)

  idx0 = (ncgx*ncgy) * (layer-1)

  do icgy = 1_8, ncgy
    do icgx = 1_8, ncgx
      selectcase( grdmsk(icgx,icgy) )
      case( 0_1 )
        grdidx(icgx,icgy) = idx_miss
      case( 1_1 )
        grdidx(icgx,icgy) = (icgy-1)*ncgx + icgx + idx0
      case default
        call errend(msg_invalid_value('grdmsk('//str((/icgx,icgy/),',')//')', grdmsk(icgx,icgy)))
      endselect
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdidx_bnd
!===============================================================
!
!===============================================================
subroutine make_grdmsk_river_end(&
    grdmsk, &
    nextxx, &
    nextxy_ocean, nextxy_river_mouth, nextxy_river_inland)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdmsk_river_end'
  integer(1), intent(out) :: grdmsk(:,:)
  integer(8), intent(in)  :: nextxx(:,:)
  integer(8), intent(in)  :: nextxy_ocean, nextxy_river_mouth, nextxy_river_inland

  integer(8) :: ncgx, ncgy
  integer(8) :: icgx, icgy
  integer(8) :: iXX

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ncgx = size(grdmsk,1)
  ncgy = size(grdmsk,2)

  do icgy = 1_8, ncgy
    do icgx = 1_8, ncgx
      iXX = nextxx(icgx,icgy)

      if( iXX == nextxy_river_mouth .or. &
          iXX == nextxy_river_inland )then
        grdmsk(icgx,icgy) = 1_1
      elseif( iXX == nextxy_ocean )then
        grdmsk(icgx,icgy) = 0_1
      elseif( iXX > 0_8 )then
        grdmsk(icgx,icgy) = 0_1
      else
        call errend(msg_invalid_value('nextxx('//str((/icgx,icgy/),',')//')', iXX))
      endif
    enddo  ! icgx/
  enddo ! icgy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdmsk_river_end
!===============================================================
!
!===============================================================
subroutine make_grdmsk_river_mouth(&
    grdmsk, &
    nextxx, &
    nextxy_ocean, nextxy_river_mouth, nextxy_river_inland)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdmsk_river_mouth'
  integer(1), intent(out) :: grdmsk(:,:)
  integer(8), intent(in)  :: nextxx(:,:)
  integer(8), intent(in)  :: nextxy_ocean, nextxy_river_mouth, nextxy_river_inland

  integer(8) :: ncgx, ncgy
  integer(8) :: icgx, icgy
  integer(8) :: iXX

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ncgx = size(grdmsk,1)
  ncgy = size(grdmsk,2)

  do icgy = 1_8, ncgy
    do icgx = 1_8, ncgx
      iXX = nextxx(icgx,icgy)

      if( iXX == nextxy_river_mouth )then
        grdmsk(icgx,icgy) = 1_1
      elseif( iXX == nextxy_ocean .or. &
              iXX == nextxy_river_inland )then
        grdmsk(icgx,icgy) = 0_1
      elseif( iXX > 0_8 )then
        grdmsk(icgx,icgy) = 0_1
      else
        call errend(msg_invalid_value('nextxx('//str((/icgx,icgy/),',')//')', iXX))
      endif
    enddo  ! icgx/
  enddo ! icgy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdmsk_river_mouth
!===============================================================
!
!===============================================================
subroutine make_grdmsk_river_inland(&
    grdmsk, &
    nextxx, &
    nextxy_ocean, nextxy_river_mouth, nextxy_river_inland)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_grdmsk_river_inland'
  integer(1), intent(out) :: grdmsk(:,:)
  integer(8), intent(in)  :: nextxx(:,:)
  integer(8), intent(in)  :: nextxy_ocean, nextxy_river_mouth, nextxy_river_inland

  integer(8) :: ncgx, ncgy
  integer(8) :: icgx, icgy
  integer(8) :: iXX

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ncgx = size(grdmsk,1)
  ncgy = size(grdmsk,2)

  do icgy = 1_8, ncgy
    do icgx = 1_8, ncgx
      iXX = nextxx(icgx,icgy)

      if( iXX == nextxy_river_inland )then
        grdmsk(icgx,icgy) = 1_1
      elseif( iXX == nextxy_ocean .or. &
              iXX == nextxy_river_mouth )then
        grdmsk(icgx,icgy) = 0_1
      elseif( iXX > 0_8 )then
        grdmsk(icgx,icgy) = 0_1
      else
        call errend(msg_invalid_value('cmf_nextxx('//str((/icgx,icgy/),',')//')', iXX))
      endif
    enddo  ! icgx/
  enddo ! icgy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_grdmsk_river_inland
!===============================================================
!
!===============================================================
subroutine mask_grdidx(&
    grdidx, &
    grdmsk, grdidx_in, idx_miss)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'mask_grdidx'
  integer(8), intent(out) :: grdidx(:,:)
  integer(1), intent(in)  :: grdmsk(:,:)
  integer(8), intent(in)  :: grdidx_in(:,:)
  integer(8), intent(in)  :: idx_miss

  integer(8) :: ncgx, ncgy
  integer(8) :: icgx, icgy

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ncgx = size(grdidx,1)
  ncgy = size(grdidx,2)

  do icgy = 1_8, ncgy
    do icgx = 1_8, ncgx
      selectcase( grdmsk(icgx,icgy) )
      case( 0_1 )
        grdidx(icgx,icgy) = idx_miss
      case( 1_1 )
        grdidx(icgx,icgy) = grdidx_in(icgx,icgy)
      case default
        call errend(msg_invalid_value('grdmsk('//str((/icgx,icgy/),',')//')', grdmsk(icgx,icgy)))
      endselect
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine mask_grdidx
!===============================================================
!
!===============================================================
subroutine make_rstidx_river_from_grdidx(&
    nkij, rstidx, &
    cmn, &
    catmxx, catmyy, grdidx, idx_miss, correspond_1on1, nam)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rstidx_river_from_grdidx'
  integer(8), intent(out) :: nkij
  integer(8), intent(out) :: rstidx(:,:)
  type(cmn_), intent(in)  :: cmn
  integer(8), intent(in)  :: catmxx(:,:), catmyy(:,:)
  integer(8), intent(in)  :: grdidx(:,:)
  integer(8), intent(in)  :: idx_miss
  logical   , intent(in)  :: correspond_1on1
  character(*), intent(in) :: nam

  integer(8) :: ikx, iky
  integer(8) :: cgx, cgy
  integer :: dgt_idx

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  call logmsg('data: '//str(nam))

  dgt_idx = dgt(grdidx,dgt_opt_max)

  nkij = 0_8
  rstidx(:,:) = idx_miss

!  call logmsg('nklx, nkly: '//str((/cmn%nklx,cmn%nkly/),', '))
!  call logmsg('shape(rstidx): ('//str((/lbound(rstidx,1),ubound(rstidx,1)/),':')//&
!              ', '//str((/lbound(rstidx,2),ubound(rstidx,2)/),':')//')')

  do iky = 1_8, cmn%nkly
    do ikx = 1_8, cmn%nklx
      cgx = catmxx(ikx,iky)
      cgy = catmyy(ikx,iky)
      if( cgx > 0_8 )then
        if( correspond_1on1 )then
          if( grdidx(cgx,cgy) <= 0_8 )then
            call errend(msg_unexpected_condition()//&
                     '\n  @ (ikx,iky) = ('//str((/ikx,iky/),', ')//')'//&
                     '\n  (cgx,cgy) = ('//str((/cgx,cgy/),', ')//')'//&
                     '\n  grdidx: '//str(grdidx(cgx,cgy)))
          endif
          call add(nkij)
          rstidx(ikx,iky) = grdidx(cgx,cgy)
        else
          if( grdidx(cgx,cgy) > 0_8 )then
            call add(nkij)
            rstidx(ikx,iky) = grdidx(cgx,cgy)
          endif
        endif
      endif
    enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( nkij == 0_8 )then
    call logmsg('Raster nij: '//str(nkij))
  else
    call logmsg('Raster nij: '//str(nkij)//&
              '\n       min: '//str(minval(rstidx,mask=rstidx/=idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(rstidx,mask=rstidx/=idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_rstidx_river_from_grdidx
!===============================================================
!
!===============================================================
subroutine make_rstidx_noriv_from_grdidx(&
    nkij, rstidx, &
    cmn, cgxi, cgxf, cgyi, cgyf, &
    catmxx, catmyy, grdidx, &
    catmxy_noriv_coastal, catmxy_noriv_inland, idx_miss)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rstidx_noriv_from_grdidx'
  integer(8), intent(out) :: nkij
  integer(8), intent(out) :: rstidx(:,:)
  type(cmn_), intent(in)  :: cmn
  integer(8), intent(in)  :: cgxi, cgxf, cgyi, cgyf
  integer(8), intent(in)  :: catmxx(:,:), catmyy(:,:)
  integer(8), intent(in)  :: grdidx(:,:)
  integer(8), intent(in)  :: catmxy_noriv_coastal, catmxy_noriv_inland
  integer(8), intent(in)  :: idx_miss

  integer(8) :: kxi, kxf, ikx, kyi, kyf, iky
  integer(8) :: icgx, icgy
  integer(8) :: cgx, cgy
  integer :: dgt_idx

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  dgt_idx = dgt(grdidx,dgt_opt_max)

  nkij = 0_8
  rstidx(:,:) = idx_miss

  kyf = 0_8
  do icgy = cgyi, cgyf
    kyi = kyf + 1_8
    kyf = kyf + cmn%nky_grid

    kxf = 0_8
    do icgx = cgxi, cgxf
      kxi = kxf + 1_8
      kxf = kxf + cmn%nkx_grid

      do iky = kyi, kyf
        do ikx = kxi, kxf
          cgx = catmxx(ikx,iky)
          cgy = catmyy(ikx,iky)

          if( cgx == catmxy_noriv_coastal .or. &
              cgx == catmxy_noriv_inland )then
            if( grdidx(icgx,icgy) == idx_miss )then
              call errend(msg_unexpected_condition()//&
                        '\n  (icgx,icgy): ('//str((/icgx,icgy/),', ')//')'//&
                        '\n  (ikx,iky)  : ('//str((/ikx,iky/),', ')//')'//&
                        '\n  catmxx: '//str(catmxx(ikx,iky))//&
                        '\n  grdidx: '//str(grdidx(icgx,icgy)))
            endif
            call add(nkij)
            rstidx(ikx,iky) = grdidx(icgx,icgy)
          endif
        enddo  ! ikx/
      enddo  ! iky/
    enddo  ! icgx/
  enddo  ! icgy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( nkij == 0_8 )then
    call logmsg('Raster nij: '//str(nkij))
  else
    call logmsg('Raster nij: '//str(nkij)//&
              '\n       min: '//str(minval(rstidx,mask=rstidx/=idx_miss),dgt_idx)//&
              '\n       max: '//str(maxval(rstidx,mask=rstidx/=idx_miss),dgt_idx))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_rstidx_noriv_from_grdidx
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
subroutine check_consistency_grdidx_river(&
    cmn, idx_miss, &
    grdidx_river, grdidx_river_end, grdidx_river_mouth, grdidx_river_inland)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_consistency_grdidx_river'
  type(cmn_), intent(in) :: cmn
  integer(8), intent(in) :: idx_miss
  integer(8), intent(in) :: grdidx_river(:,:), &
                            grdidx_river_end(:,:), &
                            grdidx_river_mouth(:,:), &
                            grdidx_river_inland(:,:)

  integer(8) :: icgx, icgy
  logical :: make_river, &
             make_river_end, &
             make_river_mouth, &
             make_river_inland

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  make_river        = size(grdidx_river       ) > 1
  make_river_end    = size(grdidx_river_end   ) > 1
  make_river_mouth  = size(grdidx_river_mouth ) > 1
  make_river_inland = size(grdidx_river_inland) > 1
  !-------------------------------------------------------------
  ! (A) Check inclusion relations between
  !   (1) river and river_end
  !   (2) river and river_mouth
  !   (3) river and river_inland
  !-------------------------------------------------------------
  ! (A-1) river and river_end
  !-------------------------------------------------------------
  if( make_river_end )then
    do icgy = 1_8, cmn%ncgy
      do icgx = 1_8, cmn%ncgx
        if( grdidx_river(icgx,icgy) == idx_miss .and. &
            grdidx_river_end(icgx,icgy) /= idx_miss )then
          call errend(msg_unexpected_condition()//&
                    '\n  grdidx_river == idx_miss .and. grdidx_river_end /= idx_miss'//&
                    '\n  (cgx,cgy): ('//str((/icgx,icgy/),',')//')'//&
                    '\n  grdidx_river    : '//str(grdidx_river(icgx,icgy))//&
                    '\n  grdidx_river_end: '//str(grdidx_river_end(icgx,icgy)), &
                      'A-1')
        endif
      enddo
    enddo
  endif
  !-------------------------------------------------------------
  ! (A-2) river and river_mouth
  !-------------------------------------------------------------
  if( make_river_mouth )then
    do icgy = 1_8, cmn%ncgy
      do icgx = 1_8, cmn%ncgx
        if( grdidx_river(icgx,icgy) == idx_miss .and. &
            grdidx_river_mouth(icgx,icgy) /= idx_miss )then
          call errend(msg_unexpected_condition()//&
                    '\n  grdidx_river == idx_miss .and. grdidx_river_mouth /= idx_miss'//&
                    '\n  (cgx,cgy): ('//str((/icgx,icgy/),',')//')'//&
                    '\n  grdidx_river      : '//str(grdidx_river(icgx,icgy))//&
                    '\n  grdidx_river_mouth: '//str(grdidx_river_mouth(icgx,icgy)), &
                      'A-2')
        endif
      enddo
    enddo
  endif
  !-------------------------------------------------------------
  ! (A-3) river and river_inland
  !-------------------------------------------------------------
  if( make_river_inland )then
    do icgy = 1_8, cmn%ncgy
      do icgx = 1_8, cmn%ncgx
        if( grdidx_river(icgx,icgy) == idx_miss .and. &
            grdidx_river_inland(icgx,icgy) /= idx_miss )then
          call errend(msg_unexpected_condition()//&
                    '\n  grdidx_river == idx_miss .and. grdidx_river_inland /= idx_miss'//&
                    '\n  (cgx,cgy): ('//str((/icgx,icgy/),',')//')'//&
                    '\n  grdidx_river       : '//str(grdidx_river(icgx,icgy))//&
                    '\n  grdidx_river_inland: '//str(grdidx_river_inland(icgx,icgy)), &
                      'A-3')
        endif
      enddo
    enddo
  endif
  !-------------------------------------------------------------
  ! (B) Check inclusion relations between
  !   (B-1) river_end and river_mouth
  !   (B-2) river_end and river_inland
  !-------------------------------------------------------------
  if( make_river_end )then
    !-----------------------------------------------------------
    ! (B-1) river_end and river_mouth
    !-----------------------------------------------------------
    if( make_river_mouth )then
      do icgy = 1_8, cmn%ncgy
        do icgx = 1_8, cmn%ncgx
          if( grdidx_river_end(icgx,icgy) == idx_miss .and. &
              grdidx_river_mouth(icgx,icgy) /= idx_miss )then
            call errend(msg_unexpected_condition()//&
                      '\n  grdidx_river_end == idx_miss .and. grdidx_river_mouth /= idx_miss'//&
                      '\n  (cgx,cgy): ('//str((/icgx,icgy/),',')//')'//&
                      '\n  grdidx_river_end  : '//str(grdidx_river_end(icgx,icgy))//&
                      '\n  grdidx_river_mouth: '//str(grdidx_river_mouth(icgx,icgy)), &
                        'B-1')
          endif
        enddo
      enddo
    endif
    !-----------------------------------------------------------
    ! (B-2) river_end and river_inland
    !-----------------------------------------------------------
    if( make_river_inland )then
      do icgy = 1_8, cmn%ncgy
        do icgx = 1_8, cmn%ncgx
          if( grdidx_river_end(icgx,icgy) == idx_miss .and. &
              grdidx_river_inland(icgx,icgy) /= idx_miss )then
            call errend(msg_unexpected_condition()//&
                      '\n  grdidx_river_end == idx_miss .and. grdidx_river_inland /= idx_miss'//&
                      '\n  (cgx,cgy): ('//str((/icgx,icgy/),',')//')'//&
                      '\n  grdidx_river_end   : '//str(grdidx_river_end(icgx,icgy))//&
                      '\n  grdidx_river_inland: '//str(grdidx_river_inland(icgx,icgy)), &
                        'B-2')
          endif
        enddo
      enddo
    endif
    !-----------------------------------------------------------
  endif
  !-------------------------------------------------------------
  ! (C) Check exclusion relations between river_mouth and river_inland
  !-------------------------------------------------------------
  if( make_river_mouth .and. make_river_inland )then
    do icgy = 1_8, cmn%ncgy
      do icgx = 1_8, cmn%ncgx
        if( grdidx_river_mouth(icgx,icgy) /= idx_miss .and. &
            grdidx_river_inland(icgx,icgy) /= idx_miss )then
          call errend(msg_unexpected_condition()//&
                    '\n  grdidx_river_end == idx_miss .and. grdidx_river_inland /= idx_miss'//&
                    '\n  (cgx,cgy): ('//str((/icgx,icgy/),',')//')'//&
                    '\n  grdidx_river_end   : '//str(grdidx_river_end(icgx,icgy))//&
                    '\n  grdidx_river_inland: '//str(grdidx_river_inland(icgx,icgy)), &
                      'C')
        endif
      enddo
    enddo
  endif
  !-------------------------------------------------------------
  call logmsg('...OK')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_consistency_grdidx_river
!===============================================================
!
!===============================================================
subroutine check_consistency_rstidx_river(&
    cmn, idx_miss, &
    rstidx_river, rstidx_river_end, rstidx_river_mouth, rstidx_river_inland)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_consistency_rstidx_river'
  type(cmn_), intent(in) :: cmn
  integer(8), intent(in) :: idx_miss
  integer(8), intent(in) :: rstidx_river(:,:), &
                            rstidx_river_end(:,:), &
                            rstidx_river_mouth(:,:), &
                            rstidx_river_inland(:,:)

  integer(8) :: ikx, iky
  logical :: make_river_end, &
             make_river_mouth, &
             make_river_inland

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  make_river_end    = size(rstidx_river_end   ) > 1
  make_river_mouth  = size(rstidx_river_mouth ) > 1
  make_river_inland = size(rstidx_river_inland) > 1
  !-------------------------------------------------------------
  ! Check inclusion relations between
  ! (1) river and river_end
  ! (2) river and river_mouth
  ! (3) river and river_inland
  !-------------------------------------------------------------
  ! (1) river and river_end
  !-----------------------------------------------------------
  if( make_river_end )then
    do iky = 1_8, cmn%nkly
      do ikx = 1_8, cmn%nklx
        if( rstidx_river(ikx,iky) == idx_miss .and. &
            rstidx_river_end(ikx,iky) /= idx_miss )then
          call errend(msg_unexpected_condition()//&
                    '\n  rstidx_river == idx_miss .and. rstidx_river_end /= idx_miss'//&
                    '\n  (kx,ky): ('//str((/ikx,iky/),', ')//')'//&
                    '\n  rstidx_river    : '//str(rstidx_river(ikx,iky))//&
                    '\n  rstidx_river_end: '//str(rstidx_river_end(ikx,iky)))
        endif
      enddo
    enddo
  else
    !-----------------------------------------------------------
    ! (2) river and river_mouth
    !-----------------------------------------------------------
    if( make_river_mouth )then
      do iky = 1_8, cmn%nkly
        do ikx = 1_8, cmn%nklx
          if( rstidx_river(ikx,iky) == idx_miss .and. &
              rstidx_river_mouth(ikx,iky) /= idx_miss )then
            call errend(msg_unexpected_condition()//&
                      '\n  rstidx_river == idx_miss .and. rstidx_river_mouth /= idx_miss'//&
                      '\n  (kx,ky): ('//str((/ikx,iky/),', ')//')'//&
                      '\n  rstidx_river      : '//str(rstidx_river(ikx,iky))//&
                      '\n  rstidx_river_mouth: '//str(rstidx_river_mouth(ikx,iky)))
          endif
        enddo
      enddo
    endif
    !-----------------------------------------------------------
    ! (3) river and river_inland
    !-----------------------------------------------------------
    if( make_river_inland )then
      do iky = 1_8, cmn%nkly
        do ikx = 1_8, cmn%nklx
          if( rstidx_river(ikx,iky) == idx_miss .and. &
              rstidx_river_inland(ikx,iky) /= idx_miss )then
            call errend(msg_unexpected_condition()//&
                      '\n  rstidx_river == idx_miss .and. rstidx_river_inland /= idx_miss'//&
                      '\n  (kx,ky): ('//str((/ikx,iky/),', ')//')'//&
                      '\n  rstidx_river       : '//str(rstidx_river(ikx,iky))//&
                      '\n  rstidx_river_inland: '//str(rstidx_river_inland(ikx,iky)))
          endif
        enddo
      enddo
    endif
  endif
  !-------------------------------------------------------------
  ! Check inclusion relations between
  ! (1) river_end and river_mouth
  ! (2) river_end and river_inland
  !-------------------------------------------------------------
  if( make_river_end )then
    !-----------------------------------------------------------
    ! (1) river_end and river_mouth
    !-----------------------------------------------------------
    if( make_river_mouth )then
      do iky = 1_8, cmn%nkly
        do ikx = 1_8, cmn%nklx
          if( rstidx_river_end(ikx,iky) == idx_miss .and. &
              rstidx_river_mouth(ikx,iky) /= idx_miss )then
            call errend(msg_unexpected_condition()//&
                      '\n  rstidx_river_end == idx_miss .and. rstidx_river_mouth /= idx_miss'//&
                      '\n  (kx,ky): ('//str((/ikx,iky/),', ')//')'//&
                      '\n  rstidx_river_end  : '//str(rstidx_river_end(ikx,iky))//&
                      '\n  rstidx_river_mouth: '//str(rstidx_river_mouth(ikx,iky)))
          endif
        enddo
      enddo
    endif
    !-----------------------------------------------------------
    ! (2) river_end and river_inland
    !-----------------------------------------------------------
    if( make_river_mouth )then
      do iky = 1_8, cmn%nkly
        do ikx = 1_8, cmn%nklx
          if( rstidx_river_end(ikx,iky) == idx_miss .and. &
              rstidx_river_inland(ikx,iky) /= idx_miss )then
            call errend(msg_unexpected_condition()//&
                      '\n  rstidx_river_end == idx_miss .and. rstidx_river_inland /= idx_miss'//&
                      '\n  (kx,ky): ('//str((/ikx,iky/),', ')//')'//&
                      '\n  rstidx_river_end   : '//str(rstidx_river_end(ikx,iky))//&
                      '\n  rstidx_river_inland: '//str(rstidx_river_inland(ikx,iky)))
          endif
        enddo
      enddo
    endif
    !-----------------------------------------------------------
  endif
  !-------------------------------------------------------------
  ! Check exclusion relatoins between river_mouth and river_inland
  !-------------------------------------------------------------
  if( make_river_mouth .and. make_river_inland )then
    do iky = 1_8, cmn%nkly
      do ikx = 1_8, cmn%nklx
        if( rstidx_river_mouth(ikx,iky) /= idx_miss .and. &
            rstidx_river_inland(ikx,iky) /= idx_miss )then
          call errend(msg_unexpected_condition()//&
                    '\n  rstidx_river_mouth /= idx_miss .and. rstidx_river_inland /= idx_miss'//&
                    '\n  (kx,ky): ('//str((/ikx,iky/),', ')//')'//&
                    '\n  rstidx_river_mouth : '//str(rstidx_river_mouth(ikx,iky))//&
                    '\n  rstidx_river_inland: '//str(rstidx_river_inland(ikx,iky)))
        endif
      enddo
    enddo
  endif
  !-------------------------------------------------------------
  call logmsg('...OK')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_consistency_rstidx_river
!===============================================================
!
!===============================================================
subroutine check_consistency_rstidx_validity(&
    cmn, idx_miss, &
    rstidx_river, rstidx_noriv, rstidx_ocean)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_consistency_rstidx_validity'
  type(cmn_), intent(in) :: cmn
  integer(8), intent(in) :: idx_miss
  integer(8), intent(in) :: rstidx_river(:,:), &
                            rstidx_noriv(:,:), &
                            rstidx_ocean(:,:)

  integer(8) :: ikx, iky
  integer :: n_valid
  logical :: make_river, &
             make_noriv, &
             make_ocean
  character(:), allocatable :: msg

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  make_river = size(rstidx_river) > 1
  make_noriv = size(rstidx_noriv) > 1
  make_ocean = size(rstidx_ocean) > 1

  do iky = 1_8, cmn%nkly
    do ikx = 1_8, cmn%nklx
      n_valid = 0

      if( make_river .and. make_noriv .and. make_ocean )then
        if( rstidx_river(ikx,iky) /= idx_miss ) call add(n_valid)
        if( rstidx_noriv(ikx,iky) /= idx_miss ) call add(n_valid)
        if( rstidx_ocean(ikx,iky) /= idx_miss ) call add(n_valid)

        if( n_valid /= 1 )then
          call errend(msg_unexpected_condition()//&
                    '\n  Status of raster @ ('//str((/ikx,iky/),', ')//') is invalid.'//&
                    '\n  rstidx_river: '//str(rstidx_river(ikx,iky))//&
                    '\n  rstidx_noriv: '//str(rstidx_noriv(ikx,iky))//&
                    '\n  rstidx_ocean: '//str(rstidx_ocean(ikx,iky)))
        endif
      else
        if( make_river )then
          if( rstidx_river(ikx,iky) /= idx_miss ) call add(n_valid)
        endif
        if( make_noriv )then
          if( rstidx_noriv(ikx,iky) /= idx_miss ) call add(n_valid)
        endif
        if( make_ocean )then
          if( rstidx_ocean(ikx,iky) /= idx_miss ) call add(n_valid)
        endif

        if( n_valid > 1 )then
          msg = msg_unexpected_condition()//&
              '\n  Status of raster @ ('//str((/ikx,iky/),', ')//') is invalid.'
          if( make_river ) msg = msg//'\n  rstidx_river: '//str(rstidx_river(ikx,iky))
          if( make_noriv ) msg = msg//'\n  rstidx_noriv: '//str(rstidx_noriv(ikx,iky))
          if( make_ocean ) msg = msg//'\n  rstidx_ocean: '//str(rstidx_ocean(ikx,iky))
          call errend(msg)
        endif
      endif
    enddo
  enddo

  call logmsg('...OK')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_consistency_rstidx_validity
!===============================================================
!
!===============================================================
subroutine check_consistency_grdidx_rstidx_rect(&
    cmn, idx_miss, cgxi, cgxf, cgyi, cgyf, &
    grdidx, rstidx)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_consistency_grdidx_rstidx_rect'
  type(cmn_), intent(in) :: cmn
  integer(8), intent(in) :: idx_miss
  integer(8), intent(in) :: cgxi, cgxf, cgyi, cgyf
  integer(8), intent(in) :: grdidx(:,:)
  integer(8), intent(in) :: rstidx(:,:)

  integer(8) :: icgx, icgy
  integer(8) :: kxi, kxf, kyi, kyf

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  kyf = 0_8
  do icgy = cgyi, cgyf
    kyi = kyf + 1_8
    kyf = kyf + cmn%nky_grid

    kxf = 0_8
    do icgx = cgxi, cgxf
      kxi = kxf + 1_8
      kxf = kxf + cmn%nkx_grid

      if( grdidx(icgx,icgy) == idx_miss )then
        if( any(rstidx(kxi:kxf,kyi:kyf) /= idx_miss) )then
          call errend(msg_unexpected_condition()//&
                    '\n  Valid raster exists in an invalid grid.'//&
                    '\n  @ grid ('//str((/icgx,icgy/),', ')//')'//&
                    '\n    raster('//str((/kxi,kxf/),':')//', '//str((/kyi,kyf/),':')//')')
        endif
      else
        if( all(rstidx(kxi:kxf,kyi:kyf) == idx_miss) )then
          call errend(msg_unexpected_condition()//&
                    '\n  Valid raster does not exist in an valid grid.'//&
                    '\n  @ grid ('//str((/icgx,icgy/),', ')//')'//&
                    '\n    raster('//str((/kxi,kxf/),':')//', '//str((/kyi,kyf/),':')//')')
        elseif( any(rstidx(kxi:kxf,kyi:kyf) /= idx_miss .and. &
                    rstidx(kxi:kxf,kyi:kyf) /= grdidx(icgx,icgy)) )then
          call errend(msg_unexpected_condition()//&
                    '\n  Raster has an index different from that of grid.'//&
                    '\n  @ grid ('//str((/icgx,icgy/),', ')//')'//&
                    '\n    raster('//str((/kxi,kxf/),':')//', '//str((/kyi,kyf/),':')//')')
        endif
      endif
    enddo
  enddo

  call logmsg('...OK')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_consistency_grdidx_rstidx_rect
!===============================================================
!
!===============================================================
subroutine check_if_rstidx_in_grdidx(&
    cmn, idx_miss, &
    grdidx, rstidx, grdstat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_if_rstidx_in_grdidx'
  type(cmn_), intent(in)    :: cmn
  integer(8), intent(in)    :: idx_miss
  integer(8), intent(in)    :: grdidx(:,:)
  integer(8), intent(in)    :: rstidx(:,:)
  integer(1), intent(inout) :: grdstat(:,:)

  integer(8), allocatable :: grdidx_1d(:)
  integer(8), allocatable :: arg(:)
  integer(8) :: ncgij, cgij, cgx, cgy
  integer(8) :: ikx, iky
  integer(8) :: idx, idx_prev
  integer(8) :: loc

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Check if the set of indices of rstidx is in that of grdidx
  !-------------------------------------------------------------
  call logent('Checking if the set of indices of rstidx is in that of grdidx', &
              PRCNAM, MODNAM)

  ncgij = cmn%ncgx * cmn%ncgy
  allocate(grdidx_1d(ncgij))
  allocate(arg(ncgij))
  grdidx_1d = reshape(grdidx,(/ncgij/))

  call argsort(grdidx_1d, arg)

  idx_prev = idx_miss

  do iky = 1_8, size(rstidx,2)
    do ikx = 1_8, size(rstidx,1)
      idx = rstidx(ikx,iky)
      if( idx == idx_miss ) cycle

      if( idx /= idx_prev )then
        idx_prev = idx
        call search(idx, grdidx_1d, arg, loc)
        if( loc == 0_8 )then
          call errend(msg_unexpected_condition()//&
                    '\n  Index '//str(idx)//' of raster was not found in grid.')
        endif
      endif

      cgij = arg(loc)
      cgy = (cgij-1_8) / cmn%ncgx + 1_8
      cgx = cgij - cmn%ncgx*(cgy-1_8)
      grdstat(cgx,cgy) = GRDSTAT_VALID
    enddo
  enddo

  deallocate(grdidx_1d)
  deallocate(arg)

  call logmsg('...OK')

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_if_rstidx_in_grdidx
!===============================================================
!
!===============================================================
subroutine cmf_check_if_grdidx_in_rstidx(&
    landType, idx_miss, idx_condition, grdidx, grdstat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'cmf_check_if_grdidx_in_rstidx'
  character(*), intent(in)    :: landType
  integer(8)  , intent(in)    :: idx_miss
  character(*), intent(in)    :: idx_condition
  integer(8)  , intent(in)    :: grdidx(:,:)
  integer(1)  , intent(inout) :: grdstat(:,:)

  integer :: icgx, icgy, cgx, cgy
  integer :: num_invalid

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  num_invalid = 0
  do icgy = 1, size(grdidx,2)
  do icgx = 1, size(grdidx,1)
    if( grdidx(icgx,icgy) /= idx_miss .and. &
        grdstat(icgx,icgy) == GRDSTAT_INVALID )then
      if( num_invalid == 0 )then
        cgx = icgx
        cgy = icgy
      endif
      call add(num_invalid)
    endif
  enddo  ! icgx/
  enddo  ! icgy/

  if( num_invalid == 0 )then
    call logmsg('...OK')
    call logret(PRCNAM, MODNAM)
    return
  endif

  selectcase( idx_condition )
  case( IDX_CONDITION__MATCH, IDX_CONDITION__GRD_IN_RST )
    call errend(msg_unexpected_condition()//&
              '\n  '//str(num_invalid)//' grids are defined but'//&
                ' not found in the raster map.'//&
              '\n  e.g. @ (icgx,icgy) = ('//str((/cgx,cgy/),',')//'), '//&
              '\n  grdidx /= idx_miss .and. grdstat == GRDSTAT_INVALID'//&
              '\n  (landType = '//str(landType)//')'//&
              '\nIt means that grid index '//str(grdidx(cgx,cgy))//&
                ' in `grdidx` does not exist in `rstidx`. This can'//&
                ' occur when `rstidx` is an upscaled one, not the'//&
                ' original one. For example, the case that you are using'//&
                ' `rstidx` of 1min resolution and CaMa-Flood map is'//&
                ' generated from 3sec map. You can ignore this error '//&
                ' by setting an option "idx_condition: raster_in_grid"'//&
                ' in the block "cama-flood".')
  case( IDX_CONDITION__RST_IN_GRD )
    call logmsg(str(num_invalid)//' grids are defined but'//&
                ' not found in the raster map.')
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine cmf_check_if_grdidx_in_rstidx
!===============================================================
!
!===============================================================
subroutine mat_check_if_grdidx_in_rstidx(&
    nam, idx_miss, grdidx, grdstat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'mat_check_if_grdidx_in_rstidx'
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: idx_miss
  integer(8)  , intent(in) :: grdidx(:,:)
  integer(1)  , intent(in) :: grdstat(:,:)

  integer(8) :: icgx, icgy
  integer(8) :: num_invalid
  integer :: dgt_cgxy

  call logbgn(PRCNAM, MODNAM)
  !---------------------------------------------------------------
  !
  !---------------------------------------------------------------
  call logmsg('data: '//str(nam))

  num_invalid = 0_8
  do icgy = 1_8, size(grdidx,2)
  do icgx = 1_8, size(grdidx,1)
    if( grdidx(icgx,icgy) /= idx_miss .and. &
        grdstat(icgx,icgy) == GRDSTAT_INVALID )then
      call add(num_invalid)
    endif
  enddo  ! icgx/
  enddo  ! icgy/

  if( num_invalid == 0 )then
    call logmsg('...OK')
    call logret(PRCNAM, MODNAM)
    return
  endif

  call erradd(msg_unexpected_condition()//&
              '  '//str(num_invalid)//' grid(s) is (are) defined but not found in catmxy.')

  dgt_cgxy = dgt(shape(grdidx),DGT_OPT_MAX)

  num_invalid = 0_8
  loop_mat:&
  do icgy = 1_8, size(grdidx,2)
  do icgx = 1_8, size(grdidx,1)
    if( grdidx(icgx,icgy) /= idx_miss .and. &
        grdstat(icgx,icgy) == GRDSTAT_INVALID )then
      call add(num_invalid)
      if( num_invalid > 10_8 )then
        call errapd('...')
        exit loop_mat
      endif

      call errapd('  @ ('//str((/icgx,icgy/),dgt_cgxy,', ')//') '//&
                  'grdidx: '//str(grdidx(icgx,icgy)))

    endif
  enddo  ! icgx/
  enddo& ! icgy/
  loop_mat
  call errend('')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine mat_check_if_grdidx_in_rstidx
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
subroutine get_value_bounds_tile(&
    cmn, path, west, east, south, north, &
    cgxi, cgxf, cgyi, cgyf, &
    kgxi, kgxf, kgyi, kgyf)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_value_bounds_tile'
  type(cmn_)  , intent(in)  :: cmn
  character(*), intent(in)  :: path
  integer     , intent(out) :: west, east, south, north
  integer(8)  , intent(out) :: cgxi, cgxf, cgyi, cgyf
  integer(8)  , intent(out) :: kgxi, kgxf, kgyi, kgyf

  character(len_trim(path)) :: fname
  character(1) :: sn, we

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fname = filename(path)

  sn = fname(1:1)
  south = int4_char(fname(2:3))
  we = fname(4:4)
  west = int4_char(fname(5:7))

  selectcase( sn )
  case( 's' )
    south = -south
  case( 'n' )
    continue
  case default
    call errend(msg_invalid_value('sn', sn)//&
              '\n  path: '//str(path))
  endselect

  selectcase( we )
  case( 'w' )
    west = -west
  case( 'e' )
    continue
  case default
    call errend(msg_invalid_value('we', we)//&
              '\n  path: '//str(path))
  endselect

  east = west + cmn%tile_size_lon
  north = south + cmn%tile_size_lat

  cgxi = int(west+180,8) * cmn%ncx_1deg + 1_8
  cgxf = cgxi + cmn%nclx - 1_8
  cgyi = int(90-north,8) * cmn%ncy_1deg + 1_8
  cgyf = cgyi + cmn%ncly - 1_8

  kgxi = int(west+180,8) * cmn%nkx_1deg + 1_8
  kgxf = kgxi + cmn%nklx - 1_8
  kgyi = int(90-north,8) * cmn%nky_1deg + 1_8
  kgyf = kgyi + cmn%nkly - 1_8
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_value_bounds_tile
!===============================================================
!
!===============================================================
subroutine realloc_dat_int1(alloc, dat, nx, ny)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_dat_int1'
  logical, intent(in) :: alloc
  integer(1), pointer :: dat(:,:)
  integer(8), intent(in) :: nx, ny

  if( alloc )then
    call realloc(dat, (/1_8,1_8/), (/nx,ny/))
  else
    call realloc(dat, (/1_8,1_8/), (/1_8,1_8/))
  endif
end subroutine realloc_dat_int1
!===============================================================
!
!===============================================================
subroutine realloc_dat_int8(alloc, dat, nx, ny)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_dat_int8'
  logical, intent(in) :: alloc
  integer(8), pointer :: dat(:,:)
  integer(8), intent(in) :: nx, ny

  if( alloc )then
    call realloc(dat, (/1_8,1_8/), (/nx,ny/))
  else
    call realloc(dat, (/1_8,1_8/), (/1_8,1_8/))
  endif
end subroutine realloc_dat_int8
!===============================================================
!
!===============================================================
end module mod_main
