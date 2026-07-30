module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_settings
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_set'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(s, t, dout)
  use c1_set, only: &
        open_setting_file      , &
        close_setting_file     , &
        line_number            , &
        read_path_report       , &
        get_path_report        , &
        find_block             , &
        check_num_of_key       , &
        bar                    , &
        raise_error_invalid_key, &
        msg_invalid_input
  use c1_file, only: &
        open_report_file
  use c1_opt_ctrl, only: &
        set_opt_sys  , &
        set_opt_log  , &
        set_opt_earth
  use c1_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log, &
        set_default_values_opt_earth
  use c1_gs_base, only: &
        init_mesh             , &
        set_miss_file_grid_in , &
        set_miss_file_grid_out, &
        set_save_file_grid_out
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings'
  type(gs_)    , intent(out), target :: s
  type(gs_)    , intent(out), target :: t
  type(output_), intent(out), target :: dout

  type counter_
    integer :: s
    integer :: t
    integer :: dout
    integer :: opt
  end type
  type(counter_) :: counter

  character(CLEN_VAR) :: blockName
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_LATLON  = 'mesh_latlon'
  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_RASTER  = 'mesh_raster'
  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_POLYGON = 'mesh_polygon'
  character(CLEN_VAR), parameter :: BLOCKNAME__RASTER       = 'raster'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUTPUT       = 'output'
  character(CLEN_VAR), parameter :: BLOCKNAME__OPT          = 'options'

  type(gs_common_), pointer :: sc, tc
  type(opt_) :: opt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call logent('Initilizing', PRCNAM, MODNAM)

  call traperr( init_mesh(s) )
  call traperr( init_mesh(t) )

  s%id = 's'
  s%nam = MESH__SOURCE
  s%is_source = .true.

  t%id = 't'
  t%nam = MESHTYPE__RASTER
  t%is_source = .false.

  call set_default_values_opt_sys(opt%sys)
  call set_default_values_opt_log(opt%log)
  call set_default_values_opt_earth(opt%earth)

  call logext()
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  call open_setting_file()

  ! Open report file
  !-------------------------------------------------------------
  call read_path_report()
  call traperr( open_report_file(get_path_report()) )

  ! Read settings
  !-------------------------------------------------------------
  call init_counter()

  do
    call find_block(blockName)

    selectcase( blockName )
    !-----------------------------------------------------------
    ! Case: No more block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Case: gs_latlon
    case( BLOCKNAME__MESH_LATLON )
      call update_counter(counter%s)
      call read_settings_gs_latlon(s)
    !-----------------------------------------------------------
    ! Case: gs_polygon
    case( BLOCKNAME__MESH_POLYGON )
      call update_counter(counter%s)
      call read_settings_gs_polygon(s)
    !-----------------------------------------------------------
    ! Case: gs_raster
    case( BLOCKNAME__MESH_RASTER )
      call errend(msg_not_implemented()// &
                '\nblockName: '//str(blockName))
    !-----------------------------------------------------------
    ! Case: raster
    case( BLOCKNAME__RASTER )
      call update_counter(counter%t)
      call read_settings_raster(t)
    !-----------------------------------------------------------
    ! Case: output
    case( BLOCKNAME__OUTPUT )
      call update_counter(counter%dout)
      call read_settings_output(dout)
    !-----------------------------------------------------------
    ! Case: opt
    case( BLOCKNAME__OPT )
      call update_counter(counter%opt)
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('blockName', blockName)//&
                '\nCheck the name of block.')
    endselect
  enddo

  call close_setting_file()

  call check_number_of_blocks()

  call logext()
  !-------------------------------------------------------------
  ! Check inputs
  !-------------------------------------------------------------
  call check_input(s, opt%earth)
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call logent('Setting some variables', PRCNAM, MODNAM)

  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(get_path_report())
    call logmsg('Directory of intermediates was not given.'//&
              '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

  sc => s%cmn
  tc => t%cmn

  ! Missing values of grid data
  !-------------------------------------------------------------
  call traperr( set_miss_file_grid_in(&
         sc%f_grid_in, &
         sc%idx_miss, sc%ara_miss, sc%wgt_miss, &
         sc%xyz_miss, sc%lonlat_miss, sc%val_miss) )

  call traperr( set_miss_file_grid_out(&
         sc%f_grid_out, &
         sc%idx_miss, sc%ara_miss, sc%wgt_miss, &
         sc%xyz_miss, sc%lonlat_miss, sc%val_miss) )

  call traperr( set_save_file_grid_out(sc%f_grid_out) )

  call traperr( set_miss_file_grid_in(&
         tc%f_grid_in, &
         tc%idx_miss, tc%ara_miss, tc%wgt_miss, &
         tc%xyz_miss, tc%lonlat_miss, tc%val_miss) )

  call traperr( set_miss_file_grid_out(&
         tc%f_grid_out, &
         tc%idx_miss, tc%ara_miss, tc%wgt_miss, &
         tc%xyz_miss, tc%lonlat_miss, tc%val_miss) )

  call traperr( set_save_file_grid_out(tc%f_grid_out) )

  ! Options
  !-------------------------------------------------------------
  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)
  call set_opt_earth(opt%earth)

  call logext()
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  selectcase( s%typ )
  case( MESHTYPE__LATLON )
    call echo_settings_gs_latlon(s%latlon)
  case( MESHTYPE__RASTER )
    call echo_settings_gs_raster(s%raster)
  case( MESHTYPE__POLYGON )
    call echo_settings_gs_polygon(s%polygon)
  case default
    call errend(msg_invalid_value('s%typ', s%typ))
  endselect

  call echo_settings_raster(t%raster)

  call echo_settings_output(dout)

  call echo_settings_opt(opt)

  call logmsg(bar(''))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(s, t, dout, opt%sys)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_counter'

  counter%s  = 0
  counter%t  = 0
  counter%dout = 0
  counter%opt  = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_counter'
  integer, intent(inout) :: n

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  if( counter%s > 1 )then
    call errend(msg_invalid_input(line_number())//&
              '\nBlocks of rasterized meshes appeared more than once:'//&
              '\n  "'//str(BLOCKNAME__MESH_LATLON)//&
                '", "'//str(BLOCKNAME__MESH_RASTER)//&
                '", "'//str(BLOCKNAME__MESH_POLYGON)//'"')
  endif

  call check_num_of_key(counter%t, BLOCKNAME__RASTER, 0, 1)

  call check_num_of_key(counter%dout, BLOCKNAME__OUTPUT, 0, 1)

  call check_num_of_key(counter%opt, BLOCKNAME__OPT, 0, 1)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_number_of_blocks'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( counter%s /= 1 )then
    call errend(msg_syntax_error()//&
              '\nThe number of blocks of grid system is incorrect:'//&
              '\n  "'//str(BLOCKNAME__MESH_LATLON)//&
               '", "'//str(BLOCKNAME__MESH_RASTER)//&
               '", "'//str(BLOCKNAME__MESH_POLYGON)//'"')
  endif

  if( counter%t /= 1 )then
    call errend(msg_syntax_error()//&
              '\nThe number of blocks of raster is incorrect:'//&
              '\n  "'//str(BLOCKNAME__RASTER)//'"')
  endif

  if( counter%dout /= 1 )then
    call errend(msg_syntax_error()//&
              '\nThe number of blocks of output is incorrect:'//&
              '\n  "'//str(BLOCKNAME__OUTPUT)//'"')
  endif

  if( counter%opt > 1 )then
    call errend(msg_syntax_error()//&
              '\nThe number of blocks of options is incorrect:'//&
              '\n  "'//str(BLOCKNAME__OPT)//'"')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_number_of_blocks
!---------------------------------------------------------------
end subroutine read_settings
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
subroutine read_settings_gs_latlon(u)
  use c1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use c1_gs_base, only: &
        init_mesh_latlon         , &
        set_mesh_common          , &
        alloc_file_grid_in_val   , &
        set_bounds_file_latlon_in, &
        set_bounds_file_grid_in  , &
        set_bounds_file_grid_out
  use c1_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  use c1_gs_util, only: &
        set_gs_debug
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_gs_latlon'
  type(gs_), intent(inout), target :: u

  type(gs_latlon_)     , pointer :: ul
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(clen_path) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call logent('Setting the limits. of the number of each keyword', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('name', 0, 1)

  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west' , 0, 1)
  call set_keynum('east' , 0, 1)
  call set_keynum('south', 0, 1)
  call set_keynum('north', 0, 1)
  call set_keynum('is_south_to_north', 0, 1)

  call set_keynum('dir', 0, -1)
  call set_keynum('f_lon_bound', 0, 1)
  call set_keynum('f_lat_bound', 0, 1)
  call set_keynum('coord_unit', 0, 1)

  call set_keynum('idx_bgn', 0, 1)

  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('fin_grdval', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)

  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('val_miss', 0, 1)

  call set_keynum('idx_debug', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  call traperr( init_mesh_latlon(u) )
  call traperr( set_mesh_common(u) )

  ul => u%latlon
  fl     => ul%f_latlon_in
  fg_in  => ul%f_grid_in
  fg_out => ul%f_grid_out

  call logext()
  !-------------------------------------------------------------
  ! Count the number of each key
  !-------------------------------------------------------------
  call logent('Counting the number of each keyword', PRCNAM, MODNAM)

  fg_in%nFiles_val = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
    !-----------------------------------------------------------
    !
    case default
      continue
    endselect
  enddo

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( alloc_file_grid_in_val(fg_in) )
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  call back_to_block_head()
  call reset_keynum()

  dir = ''
  fg_in%nFiles_val = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Name
    case( 'name' )
      call read_value(u%nam)
      call traperr( remove_quotes(u%nam, QUOTE_BOTH) )
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ul%nx)
    case( 'ny' )
      call read_value(ul%ny)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(ul%west)
    case( 'east' )
      call read_value(ul%east)
    case( 'south' )
      call read_value(ul%south)
    case( 'north' )
      call read_value(ul%north)
    !-----------------------------------------------------------
    ! Y-axis
    case( 'is_south_to_north' )
      call read_value(ul%is_south_to_north)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! LonLat bounds.
    case( 'f_lon_bound' )
      call read_value(fl%lon, dir)
    case( 'f_lat_bound' )
      call read_value(fl%lat, dir)

    case( 'coord_unit' )
      call read_value(ul%coord_unit, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data
    case( 'idx_bgn' )
      call read_value(fg_in%idx_bgn)

    case( 'fin_grdidx' )
      call read_value(fg_in%idx, dir)
    case( 'fin_grdara' )
      call read_value(fg_in%ara, dir)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt, dir)
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
      call read_value(fg_in%val(fg_in%nFiles_val), dir)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      call read_value(fg_in%ub(2), pos=2)

    case( 'in_unit_ara' )
      call read_value(fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(ul%idx_miss)
    case( 'ara_miss' )
      call read_value(ul%ara_miss)
    case( 'wgt_miss' )
      call read_value(ul%wgt_miss)
    case( 'val_miss' )
      call read_value(ul%val_miss)
    !-----------------------------------------------------------
    ! For debugging
    case( 'idx_debug' )
      call read_value(ul%idx_debug)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call logent('Checking the values', PRCNAM, MODNAM)

  if( keynum('west' ) == 1 ) call traperr( check_bounds_lon(ul%west , ul%east ) )
  if( keynum('south') == 1 ) call traperr( check_bounds_lat(ul%south, ul%north) )

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  call traperr( set_bounds_file_latlon_in(&
         fl, ul%nx, ul%ny,                       &  ! in
         ul%nh, ul%hi, ul%hf, ul%nv, ul%vi, ul%vf) )! out
  call traperr( set_bounds_file_grid_in(fg_in, ul%nx, ul%ny) )
  call traperr( set_bounds_file_grid_out(fg_out, fg_in%sz(1), fg_in%sz(2)) )

  ! For debugging
  !-------------------------------------------------------------
  call traperr( set_gs_debug(&
         ul%debug, ul%idx_debug, ul%idx_miss, keynum('idx_debug')==1) )

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_keynum_relations'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Coords.
  !-------------------------------------------------------------
  if( keynum('west') == 0 .and. keynum('east') == 0 )then
    if( keynum('f_lon_bound') == 0 )then
      call errend(msg_invalid_input()//&
                '\nInformation of longitude is missing.'//&
                  ' Give "west" and "east", or give "f_lon_bound".')
    endif
  elseif( keynum('west') == 1 .and. keynum('east') == 1 )then
    if( keynum('f_lon_bound') == 1 )then
      call errend(msg_invalid_input()//&
                '\nInformation of longitude is duplicated.'//&
                  ' Give "west" and "east", or give "f_lon_bound".')
    endif
  elseif( keynum('west') == 1 .neqv. keynum('east') == 1 )then
    call errend(msg_invalid_input()//&
              '\nIf either "west" or "east" is given, both must be given.')
  endif

  if( keynum('south') == 0 .and. keynum('north') == 0 )then
    if( keynum('f_lon_bound') == 0 )then
      call errend(msg_invalid_input()//&
                '\nInformation of longitude is missing.'//&
                  ' Give "south" and "north", or give "f_lon_bound".')
    endif
  elseif( keynum('south') == 1 .and. keynum('north') == 1 )then
    if( keynum('f_lon_bound') == 1 )then
      call errend(msg_invalid_input()//&
                '\nInformation of longitude is duplicated.'//&
                  ' Give "south" and "north", or give "f_lon_bound".')
    endif
  elseif( keynum('south') == 1 .neqv. keynum('north') == 1 )then
    call errend(msg_invalid_input()//&
              '\nIf either "south" or "north" is given both must be given.')
  endif

  if( keynum('f_lon_bound') == 0 .and. keynum('f_lat_bound') == 0 .and. &
      keynum('coord_unit') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"coord_unit" is given but '//&
                'neither "f_lon_bound" or "f_lat_bound" is given.'//&
                ' The input given by "coord_unit" is ignored.')
  endif
  !-------------------------------------------------------------
  ! Grid data
  !-------------------------------------------------------------
  if( keynum('idx_bgn') == 1 .and. keynum('fin_grdidx') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"idx_bgn" is given although "fin_grdidx" is given.'//&
                ' The input for "idx_bgn" is ignored.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call errend(msg_unexpected_condition()//&
              '\n"fin_grdara" and "fin_grdwgt" must not be given at the same time.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      keynum('fin_grdara') == 0 .and. &
      keynum('fin_grdwgt') == 0 .and. &
      keynum('fin_grdval') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call logwrn(msg_undesirable_input()//&
              '\nAny value is given by the following keywords:'//&
              '\n  "'//str('in_grid_sz')//'"'//&
              '\n  "'//str('in_grid_lb')//'"'//&
              '\n  "'//str('in_grid_ub')//'"'//&
              '\nbut any value is not given by the following keywords:'//&
              '\n  "'//str('fin_grdidx')//'"'//&
              '\n  "'//str('fin_grdara')//'"'//&
              '\n  "'//str('fin_grdwgt')//'"'//&
              '\n  "'//str('fin_grdval')//'"'//&
              '\nThe inputs given by the former keywords are ignored.')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine read_settings_gs_polygon(u)
  use c1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use c1_gs_base, only: &
        init_mesh_polygon         , &
        set_mesh_common           , &
        alloc_file_grid_in_val    , &
        set_bounds_file_polygon_in, &
        set_bounds_file_grid_in   , &
        set_bounds_file_grid_out
  use c1_gs_util, only: &
        set_gs_debug
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_gs_polygon'
  type(gs_), intent(inout), target :: u

  type(gs_polygon_)     , pointer :: up
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out

  character(CLEN_PATH) :: dir
  real(8) :: coord_miss

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call logent('Setting the limits. of the number of each keyword', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('name', 0, 1)

  call set_keynum('np', 1, 1)
  call set_keynum('nij', 1, 1)

  call set_keynum('dir', 0, -1)

  call set_keynum('f_lon_vertex', 0, 1)
  call set_keynum('f_lat_vertex', 0, 1)
  call set_keynum('f_x_vertex'  , 0, 1)
  call set_keynum('f_y_vertex'  , 0, 1)
  call set_keynum('f_z_vertex'  , 0, 1)
  call set_keynum('coord_unit'  , 0, 1)
  call set_keynum('coord_miss'  , 0, 1)

  call set_keynum('f_arctyp'    , 0, 1)
  call set_keynum('arc_parallel', 0, 1)

  call set_keynum('idx_bgn', 0, 1)

  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)

  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('val_miss', 0, 1)

  call set_keynum('idx_debug', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  call traperr( init_mesh_polygon(u) )
  call traperr( set_mesh_common(u) )

  up => u%polygon
  fp     => up%f_polygon_in
  fg_in  => up%f_grid_in
  fg_out => up%f_grid_out

  call logext()
  !-------------------------------------------------------------
  ! Count the number of times each keyword was used
  !-------------------------------------------------------------
  call logent('Counting the number of times each keyword was used', PRCNAM, MODNAM)

  fg_in%nFiles_val = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
    !-----------------------------------------------------------
    !
    case default
      continue
    endselect
  enddo

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( alloc_file_grid_in_val(fg_in) )
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  call back_to_block_head()
  call reset_keynum()

  dir = ''
  fg_in%nFiles_val = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Name
    case( 'name' )
      call read_value(u%nam)
      call traperr( remove_quotes(u%nam, QUOTE_BOTH) )
    !-----------------------------------------------------------
    ! Shape
    case( 'np' )
      call read_value(up%np)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nij' )
      call read_value(up%nij)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Vertex
    case( 'f_lon_vertex' )
      call read_value(fp%lon, dir)
    case( 'f_lat_vertex' )
      call read_value(fp%lat, dir)
    case( 'f_x_vertex' )
      call read_value(fp%x, dir)
    case( 'f_y_vertex' )
      call read_value(fp%y, dir)
    case( 'f_z_vertex' )
      call read_value(fp%z, dir)

    case( 'coord_unit' )
      call read_value(up%coord_unit, is_keyword=.true.)

    case( 'coord_miss' )
      call read_value(coord_miss)
    !-----------------------------------------------------------
    ! Arc type
    case( 'f_arctyp' )
      call read_value(fp%arctyp, dir)

    case( 'arc_parallel' )
      call read_value(up%arc_parallel)
    !-----------------------------------------------------------
    ! Grid data
    case( 'idx_bgn' )
      call read_value(fg_in%idx_bgn)

    case( 'fin_grdidx' )
      call read_value(fg_in%idx, dir)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt, dir)
    case( 'fin_grdval' )
      call add(fg_in%nFiles_val)
      call read_value(fg_in%val(fg_in%nFiles_val), dir)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      !call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      !call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      !call read_value(fg_in%ub(2), pos=2)

    case( 'in_unit_ara' )
      call read_value(fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(up%idx_miss)
    case( 'ara_miss' )
      call read_value(up%ara_miss)
    case( 'wgt_miss' )
      call read_value(up%wgt_miss)
    case( 'val_miss' )
      call read_value(up%val_miss)
    !-----------------------------------------------------------
    ! For debugging
    case( 'idx_debug' )
      call read_value(up%idx_debug)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  call traperr( set_bounds_file_polygon_in(fp, up%ijs, up%ije, up%np, up%nij) )
  call traperr( set_bounds_file_grid_in(fg_in, up%nij, 1_8) )
  call traperr( set_bounds_file_grid_out(fg_out, up%nij, 1_8) )

  ! Coordinate system
  !-------------------------------------------------------------
  if( fp%lon%path /= '' )then
    up%coord_sys = COORD_SYS_SPHERICAL

    if( keynum('coord_unit') == 0 )then
      up%coord_unit = UNIT_DEGREE
    else
      if( up%coord_unit /= UNIT_DEGREE .and. &
          up%coord_unit /= UNIT_RADIAN )then
        call errend(msg_invalid_value('up%coord_unit', up%coord_unit)//&
                  '\nThis is invalid when "f_lon_vertex" is given.')
      endif
    endif

    if( keynum('coord_miss') == 1 ) up%coord_miss_s = coord_miss
  else
    up%coord_sys = COORD_SYS_CARTESIAN

    if( keynum('coord_unit') == 0 )then
      up%coord_unit = UNIT_METER
    else
      if( up%coord_unit /= UNIT_METER .and. &
          up%coord_unit /= UNIT_KILOMETER )then
        call errend(msg_invalid_value('up%coord_unit', up%coord_unit)//&
                  '\nThis is invalid when "f_x_vertex" is given.')
      endif
    endif

    if( keynum('coord_miss') == 1 ) up%coord_miss_c = coord_miss
  endif

  ! For debugging
  !-------------------------------------------------------------
  call traperr( set_gs_debug(up%debug, up%idx_debug, up%idx_miss, keynum('idx_debug')==1) )

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_keynum_relations'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Coords.
  !-------------------------------------------------------------
  if( .not. (keynum('f_lon_vertex') == 1 .and. &
             keynum('f_lat_vertex') == 1) &
      .and. &
      .not. (keynum('f_lon_vertex') == 0 .and. &
             keynum('f_lat_vertex') == 0) )then
    call errend(msg_invalid_input()//&
              '\nIf either "f_lon_vertex" or "f_lat_vertex" is given'//&
                ' both must be given.')
  endif

  if( .not. (keynum('f_x_vertex') == 1 .and. &
             keynum('f_y_vertex') == 1 .and. &
             keynum('f_z_vertex') == 1) &
      .and. &
      .not. (keynum('f_x_vertex') == 0 .and. &
             keynum('f_y_vertex') == 0 .and. &
             keynum('f_z_vertex') == 0) )then
    call errend(msg_invalid_input()//&
              '\nIf any of "f_x_vertex", "f_y_vertex" or "f_z_vertex" is given'//&
                ' all of them must be given.')
  endif

  if( keynum('f_lon_vertex') == 1 .and. keynum('f_x_vertex') == 1 )then
    call errend(msg_invalid_input()//&
              '\n"f_lon_vertex" and "f_x_vertex" cannot be given at the same time.')
  elseif( keynum('f_lon_vertex') == 0 .and. keynum('f_x_vertex') == 0 )then
    call errend(msg_invalid_input()//&
              '\nEither "f_lon_vertex" or "f_x_vertex" must be given.')
  endif

  if( keynum('arc_parallel') == 1 .and. keynum('f_arctyp') == 1 )then
    call errend(msg_invalid_input()//&
              '\nBoth "arc_parallel" and "f_arctyp" are given.')
  endif
  !--------------------------------------------------------------
  ! Grid data
  !--------------------------------------------------------------
  if( keynum('idx_bgn') == 1 .and. keynum('fin_grdidx') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"idx_bgn" is given but "fin_grdidx" is also given.'//&
                ' The input for "idx_bgn" is ignored.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      keynum('fin_grdara') == 0 .and. &
      keynum('fin_grdwgt') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call logwrn(msg_undesirable_input()//&
              '\nAny value is given by the following keywords:'//&
              '\n  "'//str('in_grid_sz')//'"'//&
              '\n  "'//str('in_grid_lb')//'"'//&
              '\n  "'//str('in_grid_ub')//'"'//&
              '\nbut any value is not given by the following keywords:'//&
              '\n  "'//str('fin_grdidx')//'"'//&
              '\n  "'//str('fin_grdara')//'"'//&
              '\n  "'//str('fin_grdwgt')//'"'//&
              '\nThe inputs given by the former keywords are ignored.')
  endif
  !-------------------------------------------------------------
  ! Missing value
  !-------------------------------------------------------------
  if( keynum('fin_grdara') == 0 .and. keynum('ara_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"ara_miss" is given although "fin_grdara" is not given.'//&
                ' The input for "ara_miss" is ignored.')
  endif

  if( keynum('fin_grdwgt') == 0 .and. keynum('wgt_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"wgt_miss" is given although "fin_grdwgt" is not given.'//&
                ' The input for "wgt_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_raster(u)
  use c1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use c1_gs_base, only: &
        init_mesh_raster         , &
        set_mesh_common          , &
        set_bounds_file_raster_in, &
        set_bounds_file_grid_in  , &
        set_bounds_file_grid_out
  use c1_gs_define, only: &
        check_bounds_lon, &
        check_bounds_lat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_raster'
  type(gs_), intent(inout), target :: u

  type(gs_raster_)     , pointer :: ur
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(clen_path) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west' , 1, 1)
  call set_keynum('east' , 1, 1)
  call set_keynum('south', 1, 1)
  call set_keynum('north', 1, 1)
  call set_keynum('is_south_to_north', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  call traperr( init_mesh_raster(u) )
  call traperr( set_mesh_common(u) )

  ur => u%raster
  fr     => ur%f_raster_in
  fg_in  => ur%f_grid_in
  fg_out => ur%f_grid_out

  call logext()
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  dir = ''

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ur%nx)
    case( 'ny' )
      call read_value(ur%ny)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(ur%west)
    case( 'east' )
      call read_value(ur%east)
    case( 'south' )
      call read_value(ur%south)
    case( 'north' )
      call read_value(ur%north)

    case( 'is_south_to_north' )
      call read_value(ur%is_south_to_north)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  !call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call logent('Checking the values', PRCNAM, MODNAM)

  call traperr( check_bounds_lon(ur%west , ur%east ) )
  call traperr( check_bounds_lat(ur%south, ur%north) )

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  call traperr( set_bounds_file_raster_in(&
         fr,                                     &  ! inout
         ur%nx, ur%ny, ur%is_south_to_north,     &  ! in
         ur%xi, ur%xf, ur%yi, ur%yf,             &  ! out
         ur%nh, ur%hi, ur%hf, ur%nv, ur%vi, ur%vf) )! out
  call traperr( set_bounds_file_grid_in(fg_in) )
  call traperr( set_bounds_file_grid_out(fg_out, fg_in%sz(1), fg_in%sz(2)) )

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_raster
!===============================================================
!
!===============================================================
subroutine read_settings_output(dout)
  use c1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_output'
  type(output_), intent(out) :: dout

  character(clen_path) :: dir
  logical :: include_min, include_max

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('ratio_min', 0, 1)
  call set_keynum('ratio_max', 0, 1)
  call set_keynum('include_min', 0, 1)
  call set_keynum('include_max', 0, 1)
  call set_keynum('ratio_ignored', 0, 1)
  call set_keynum('ratio_min_idx', 0, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_mask'     , 0, 1)
  call set_keynum('f_ratio_sum', 0, 1)
  call set_keynum('f_area_sum' , 0, 1)
  call set_keynum('f_idx'      , 0, 1)
  call set_keynum('val_miss', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default variables
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  ! [0.5, 1.0] in default
  dout%thresh%iratio_min = 0.5d0
  dout%thresh%iratio_max = 1.d0
  include_min = .true.
  include_max = .true.

  dout%thresh%iratio_ignored = 0.d0
  dout%thresh%iratio_min_idx = 0.d0

  call set_file_default(action=ACTION_WRITE)
  dout%f_iarea_sum  = file('', DTYPE_DBLE, id='output%f_iarea_sum')
  dout%f_iratio_sum = file('', DTYPE_DBLE, id='output%f_iratio_sum')
  dout%f_mask       = file('', DTYPE_INT4, id='output%f_mask')
  dout%f_idx        = file('', DTYPE_INT4, id='output%f_idx')
  call reset_file_default()

  dout%val_miss = -1d20

  call logext()
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  dir = ''

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Range for the mask
    case( 'ratio_min' )
      call read_value(dout%thresh%iratio_min)
    case( 'ratio_max' )
      call read_value(dout%thresh%iratio_max)

    case( 'include_min' )
      call read_value(include_min)
    case( 'include_max' )
      call read_value(include_max)
    !-----------------------------------------------------------
    !
    case( 'ratio_min_idx' )
      call read_value(dout%thresh%iratio_min_idx)
    !-----------------------------------------------------------
    ! Range for the index data
    case( 'ratio_ignored' )
      call read_value(dout%thresh%iratio_ignored)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Output files
    case( 'f_mask' )
      call read_value(dout%f_mask, dir)
    case( 'f_ratio_sum' )
      call read_value(dout%f_iratio_sum, dir)
    case( 'f_area_sum' )
      call read_value(dout%f_iarea_sum, dir)
    case( 'f_idx' )
      call read_value(dout%f_idx, dir)
    !-----------------------------------------------------------
    ! Missing values
    case( 'val_miss' )
      call read_value(dout%val_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  !call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  if( keynum('ratio_min') == 0 )then
    dout%thresh%ineq_iratio_min = INEQ_NONE
  else
    if( include_min )then
      dout%thresh%ineq_iratio_min = INEQ_GE
    else
      dout%thresh%ineq_iratio_min = INEQ_GT
    endif
  endif

  if( keynum('ratio_max') == 0 )then
    dout%thresh%ineq_iratio_max = INEQ_NONE
  else
    if( include_max )then
      dout%thresh%ineq_iratio_max = INEQ_LE
    else
      dout%thresh%ineq_iratio_max = INEQ_LT
    endif
  endif

  call logext()
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call logent('Checking the values', PRCNAM, MODNAM)

  call check_values_iratio_minmax(&
         dout%thresh%ineq_iratio_min, dout%thresh%ineq_iratio_max, &
         dout%thresh%iratio_min     , dout%thresh%iratio_max)

  call logext()
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine check_values_iratio_minmax(ineq_min, ineq_max, vmin, vmax)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_values_iratio_minmax'
  character(*), intent(in) :: ineq_min, ineq_max
  real(8)     , intent(in) :: vmin, vmax

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( ineq_min == INEQ_NONE .or. ineq_max == INEQ_NONE )then
    continue
  elseif( ineq_min == INEQ_GE .and. ineq_max == INEQ_LE )then
    if( vmin > vmax )then
      call errend(msg_unexpected_condition()//&
                '\nRange of the ratio is invalid.'//&
                ' Check the values of "ratio_min", "ratio_max", "include_min" and "include_max".')
    endif
  else
    if( vmin >= vmax )then
      call errend(msg_unexpected_condition()//&
                '\nRange of the ratio is invalid.'//&
                ' Check the values of "ratio_min", "ratio_max", "include_min" and "include_max".')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_values_iratio_minmax
!---------------------------------------------------------------
end subroutine read_settings_output
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  use c1_const_util, only: &
        checkval_opt_old_files
  use c1_set, only: &
        line_number            , &
        back_to_block_head     , &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
  use c1_opt_set, only: &
        KEY_OLD_FILES           , &
        KEY_DIR_INTERMEDIATES   , &
        KEY_REMOVE_INTERMEDIATES, &
        KEY_MEMORY_ULIM         , &
        KEY_EARTH_GEOSYS        , &
        KEY_EARTH_RTYP          , &
        KEY_EARTH_R             , &
        KEY_EARTH_FINV          , &
        KEY_EARTH_F             , &
        KEY_EARTH_E2            
  use c1_opt_set, only: &
        set_values_opt_earth
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_opt'
  type(opt_), intent(inout) :: opt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum(KEY_OLD_FILES           , 0, 1)
  call set_keynum(KEY_DIR_INTERMEDIATES   , 0, 1)
  call set_keynum(KEY_REMOVE_INTERMEDIATES, 0, 1)
  call set_keynum(KEY_MEMORY_ULIM         , 0, 1)

  call set_keynum(KEY_EARTH_GEOSYS, 0, 1)
  call set_keynum(KEY_EARTH_RTYP  , 0, 1)
  call set_keynum(KEY_EARTH_R     , 0, 1)
  call set_keynum(KEY_EARTH_FINV  , 0, 1)
  call set_keynum(KEY_EARTH_F     , 0, 1)
  call set_keynum(KEY_EARTH_E2    , 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of the block
    case( '' )
      exit
    !-----------------------------------------------------------
    !
    case( KEY_OLD_FILES )
      call read_value(opt%sys%old_files, is_keyword=.true.)

    case( KEY_DIR_INTERMEDIATES )
      call read_value(opt%sys%dir_im, is_path=.true.)

    case( KEY_REMOVE_INTERMEDIATES )
      call read_value(opt%sys%remove_im)

    case( KEY_MEMORY_ULIM )
      call read_value(opt%sys%memory_ulim)
    !-----------------------------------------------------------
    ! Earth's shape
    case( KEY_EARTH_GEOSYS )
      call read_value(opt%earth%geosys, is_keyword=.true.)
    case( KEY_EARTH_RTYP )
      call read_value(opt%earth%rtyp, is_keyword=.true.)
    case( KEY_EARTH_R )
      call read_value(opt%earth%r)
    case( KEY_EARTH_FINV )
      call read_value(opt%earth%finv)
    case( KEY_EARTH_F )
      call read_value(opt%earth%f)
    case( KEY_EARTH_E2 )
      call read_value(opt%earth%e2)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()
  !call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call logent('Checking the values', PRCNAM, MODNAM)

  call traperr( checkval_opt_old_files(opt%sys%old_files, 'opt%sys%old_files') )

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  call traperr( set_values_opt_earth(&
         opt%earth, &
         keynum(KEY_EARTH_R), keynum(KEY_EARTH_FINV), &
         keynum(KEY_EARTH_F), keynum(KEY_EARTH_E2)) )

  call logext()
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_opt
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
subroutine check_input(&
    a, opt_earth)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input'
  type(gs_), intent(in) :: a
  type(opt_earth_), intent(in) :: opt_earth

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt_earth%shptyp == EARTH_SHPTYP__ELLIPS )then
    if( a%typ == MESHTYPE__POLYGON )then
      call errend(msg_unexpected_condition()//&
                '\n  opt%earth%shptyp == '//str(opt_earth%shptyp)//&
                  ' and '//str(a%id)//'%typ == '//str(a%typ)//&
                '\nEarth shape "'//str(opt_earth%shptyp)//'" is '//&
                  'not implemented yet for '//str(a%typ)//' meshes.')
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_input
!===============================================================
!
!===============================================================
subroutine check_paths(s, t, dout, opt_sys)
  use c1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_paths'
  type(gs_)     , intent(in), target :: s
  type(gs_)     , intent(in), target :: t
  type(output_) , intent(in), target :: dout
  type(opt_sys_), intent(in)         :: opt_sys

  type(file_latlon_in_), pointer :: sfl
  type(file_raster_in_) , pointer :: sfr
  type(file_polygon_in_), pointer :: sfp
  type(file_grid_in_), pointer :: sfg_in

  integer :: iFile

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking input files', PRCNAM, MODNAM)

  selectcase( s%typ )
  !-------------------------------------------------------------
  ! Case: Latlon
  case( MESHTYPE__LATLON )
    sfl => s%latlon%f_latlon_in
    sfg_in => s%latlon%f_grid_in

    call traperr( check_permission(sfl%lon, allow_empty=.true.) )
    call traperr( check_permission(sfl%lat, allow_empty=.true.) )

    do iFile = 1, sfg_in%nFiles_val
      call traperr( check_permission(sfg_in%val(iFile), allow_empty=.false.) )
    enddo
  !-------------------------------------------------------------
  ! Case: Raster
  case( MESHTYPE__RASTER )
    sfr => s%raster%f_raster_in
    sfg_in => s%raster%f_grid_in

    call errend(msg_not_implemented()//&
              '\n  s%typ: '//str(s%typ))
  !-------------------------------------------------------------
  ! Case: Polygon
  case( MESHTYPE__POLYGON )
    sfp => s%polygon%f_polygon_in
    sfg_in => s%polygon%f_grid_in

    call traperr( check_permission(sfp%lon   , allow_empty=.true.) )
    call traperr( check_permission(sfp%lat   , allow_empty=.true.) )
    call traperr( check_permission(sfp%x     , allow_empty=.true.) )
    call traperr( check_permission(sfp%y     , allow_empty=.true.) )
    call traperr( check_permission(sfp%z     , allow_empty=.true.) )
    call traperr( check_permission(sfp%arctyp, allow_empty=.true.) )

    do iFile = 1, sfg_in%nFiles_val
      call traperr( check_permission(sfg_in%val(iFile), allow_empty=.true.) )
    enddo
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('s%typ', s%typ))
  endselect

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking old output files', PRCNAM, MODNAM)

  call traperr( set_opt_old_files(opt_sys%old_files) )

  call traperr( handle_old_file(dout%f_mask) )
  call traperr( handle_old_file(dout%f_iratio_sum) )
  call traperr( handle_old_file(dout%f_iarea_sum) )
  call traperr( handle_old_file(dout%f_idx) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Preparing output directories', PRCNAM, MODNAM)

  call traperr( set_opt_mkdir(output=.true., hut=hut_command) )

  call traperr( mkdir(dirname(dout%f_mask%path)) )
  call traperr( mkdir(dirname(dout%f_iratio_sum%path)) )
  call traperr( mkdir(dirname(dout%f_iarea_sum%path)) )
  call traperr( mkdir(dirname(dout%f_idx%path)) )

  call traperr( check_permission(dout%f_mask      , allow_empty=.true.) )
  call traperr( check_permission(dout%f_iratio_sum, allow_empty=.true.) )
  call traperr( check_permission(dout%f_iarea_sum , allow_empty=.true.) )
  call traperr( check_permission(dout%f_idx       , allow_empty=.true.) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_paths
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
subroutine echo_settings_gs_latlon(ul)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_gs_latlon'
  type(gs_latlon_), intent(in), target :: ul

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_), pointer :: fg_in
  integer :: dgt_nxy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(bar('Grid System (LatLon)'))

  fl => ul%f_latlon_in
  fg_in => ul%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(ul%nx, ul%ny, maxval(fg_in%sz(:2))))

  call logmsg('Name: '//str(ul%nam))

  call logmsg('Mesh type: '//str(MESHTYPE__LATLON))

  call logmsg('nx: '//str(ul%nx))
  call logmsg('ny: '//str(ul%ny))

  if( fl%lon%path == '' )then
    call logmsg('West : '//str(ul%west,'f12.5'))
    call logmsg('East : '//str(ul%east,'f12.5'))
  else
    call logmsg('Bounds of longit.: '//str(fl%lon%path))
  endif

  if( fl%lat%path == '' )then
    call logmsg('South: '//str(ul%south,'f12.5'))
    call logmsg('North: '//str(ul%north,'f12.5'))
  else
    call logmsg('Bounds of latit. : '//str(fl%lat%path))
  endif

  call logmsg('Is south to north: '//str(ul%is_south_to_north))

  call logmsg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call logmsg('  Index : '//str(fileinfo(fg_in%idx)))
    call logmsg('  Area  : '//str(fileinfo(fg_in%ara)))
    call logmsg('  Weight: '//str(fileinfo(fg_in%wgt)))
    call logmsg('  Size: ('//str(fg_in%sz(:2),dgt_nxy,', ')//')')
    call logmsg('  Use : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_nxy,':')//&
                       ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_nxy,':')//')')
    if( fg_in%ara%path /= '' )then
      call logmsg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  else
    call logmsg('  (No input)')
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(ul%idx_miss))
  call logmsg('  Area  : '//str(ul%ara_miss))
  call logmsg('  Weight: '//str(ul%wgt_miss))
  call logmsg('  XYZ   : '//str(ul%xyz_miss))
  call logmsg('  LatLon: '//str(ul%lonlat_miss))
  call logmsg('  Value : '//str(ul%val_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_raster(ur)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_gs_raster'
  type(gs_raster_), intent(in), target :: ur

  integer :: dgt_nxy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(bar('Grid system (Raster)'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(ur%nx,ur%ny))

  call logmsg('Name: '//str(ur%nam))

  call logmsg('nx: '//str(ur%nx,dgt_nxy))
  call logmsg('ny: '//str(ur%ny,dgt_nxy))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_polygon(up)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_gs_polygon'
  type(gs_polygon_), target :: up

  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  integer :: dgt_ij

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(bar('Grid System (Polygon)'))

  fp => up%f_polygon_in
  fg_in => up%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_ij = dgt(maxval(fp%sz(:2)))

  call logmsg('Name: '//str(up%nam))

  call logmsg('Mesh type: '//str(MESHTYPE__POLYGON))

  call logmsg('Grid data')
  call logmsg('  Size : '//str(fp%sz(2),dgt_ij))
  call logmsg('  Input: ('//str((/fp%lb(2),fp%ub(2)/),dgt_ij,':')//')')

  call logmsg('Max. num. of vertices of a grid: '//str(up%np))

  call logmsg('Coordinates')
  call logmsg('  Coordinate system: '//str(up%coord_sys))
  call logmsg('  Files of coords. of vertices')
  selectcase( up%coord_sys )
  case( coord_sys_spherical )
    call logmsg('    Lon: '//str(fileinfo(fp%lon)))
    call logmsg('    Lat: '//str(fileinfo(fp%lat)))
  case( coord_sys_cartesian )
    call logmsg('    X: '//str(fileinfo(fp%x)))
    call logmsg('    Y: '//str(fileinfo(fp%y)))
    call logmsg('    Z: '//str(fileinfo(fp%z)))
  case default
    call errend(msg_invalid_value('up%coord_sys', up%coord_sys))
  endselect

  call logmsg('  Unit: '//str(up%coord_unit))

  call logmsg('  Missing value of coords.')
  call logmsg('    Spherical: '//str(up%coord_miss_s))
  call logmsg('    Cartesian: '//str(up%coord_miss_c))

  if( fp%arctyp%path == '' )then
    call logmsg('Treat the arcs whose edges have same lattitude as small arcs: '//&
                str(up%arc_parallel))
  else
    call logmsg('Types of the arcs: '//str(fileinfo(fp%arctyp)))
  endif

  call logmsg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call logmsg('  Index : '//str(fileinfo(fg_in%idx)))
    call logmsg('  Area  : '//str(fileinfo(fg_in%ara)))
    call logmsg('  Weight: '//str(fileinfo(fg_in%wgt)))
    if( fg_in%ara%path /= '' )then
      call logmsg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  else
    call logmsg('  (No input)')
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(up%idx_miss))
  call logmsg('  Area  : '//str(up%ara_miss))
  call logmsg('  Weight: '//str(up%wgt_miss))
  call logmsg('  XYZ   : '//str(up%xyz_miss))
  call logmsg('  LatLon: '//str(up%lonlat_miss))
  call logmsg('  Value : '//str(up%val_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine echo_settings_raster(ur)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_raster'
  type(gs_raster_), intent(in), target :: ur

  integer :: dgt_nxy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(bar('Raster'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(ur%nx,ur%ny))

  call logmsg('Name: '//str(ur%nam))

  call logmsg('nx: '//str(ur%nx,dgt_nxy))
  call logmsg('ny: '//str(ur%ny,dgt_nxy))

  call logmsg('West : '//str(ur%west,'f12.5'))
  call logmsg('East : '//str(ur%east,'f12.5'))
  call logmsg('South: '//str(ur%south,'f12.5'))
  call logmsg('North: '//str(ur%north,'f12.5'))
  call logmsg('Is south to north: '//str(ur%is_south_to_north))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_raster
!===============================================================
!
!===============================================================
subroutine echo_settings_output(dout)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_output'
  type(output_), intent(in), target :: dout

  character(clen_wfmt*2) :: range_left, range_right

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(bar('Output'))
  !-------------------------------------------------------------
  ! Make a string of range of the ratio
  !-------------------------------------------------------------
  selectcase( dout%thresh%ineq_iratio_min )
  case( INEQ_NONE )
    range_left = '(-inf'
  case( INEQ_GT )
    range_left = '('//str(dout%thresh%iratio_min)
  case( INEQ_GE )
    range_left = '['//str(dout%thresh%iratio_min)
  case( INEQ_LT, &
        INEQ_LE )
    call errend(msg_unexpected_condition()//&
              '\n  dout%ineq_iratio_min: '//str(dout%thresh%ineq_iratio_min))
  case default
    call errend(msg_invalid_value('dout%ineq_iratio_min', dout%thresh%ineq_iratio_min))
  endselect

  selectcase( dout%thresh%ineq_iratio_max )
  case( INEQ_NONE )
    range_right = '+inf)'
  case( INEQ_LT )
    range_right = str(dout%thresh%iratio_min)//')'
  case( INEQ_LE )
    range_right = str(dout%thresh%iratio_min)//']'
  case( INEQ_GT, &
        INEQ_GE )
    call errend(msg_unexpected_condition()//&
              '\n  dout%thresh%ineq_iratio_min: '//str(dout%thresh%ineq_iratio_min))
  case default
    call errend(msg_invalid_value('dout%thresh%ineq_iratio_min', dout%thresh%ineq_iratio_min))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Files')
  call logmsg('  area_sum : '//str(fileinfo(dout%f_iarea_sum)))
  call logmsg('  ratio_sum: '//str(fileinfo(dout%f_iratio_sum)))
  call logmsg('  mask     : '//str(fileinfo(dout%f_mask)))
  call logmsg('  idx      : '//str(fileinfo(dout%f_idx)))
  call logmsg('Missing value: '//str(dout%val_miss))
  call logmsg('Range of intersection ratio: '//str(range_left)//', '//str(range_right))
  call logmsg('Missing index is applied when sum. of ratio <= '//&
              str(dout%thresh%iratio_min_idx))
  call logmsg('Intersection is ignored when its ratio <= '//&
              str(dout%thresh%iratio_ignored))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_output
!===============================================================
!
!===============================================================
subroutine echo_settings_opt(opt)
  use c1_set, only: &
        bar
  use c1_opt_set, only: &
        echo_settings_opt_sys, &
        echo_settings_opt_log, &
        echo_settings_opt_earth
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt'
  type(opt_), intent(in) :: opt

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(bar('Options'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)

  call echo_settings_opt_log(opt%log)

  call echo_settings_opt_earth(opt%earth)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
end module mod_set
