module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
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
subroutine read_settings(s, t, rt)
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
        set_opt_sys, &
        set_opt_log, &
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
  use c2_rt_base, only: &
        init_rt                    , &
        apply_oldfiles_rt_main_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings'
  type(gs_) , intent(out), target :: s, t
  type(rt_) , intent(out), target :: rt

  type counter_
    integer :: gs
    integer :: rmp
    integer :: opt
    integer :: fig
  end type
  type(counter_) :: counter

  character(CLEN_VAR) :: block_name

  type(gs_)         , pointer :: a
  type(gs_common_)  , pointer :: ac
  type(rt_main_)    , pointer :: rtm
  type(opt_) :: opt
  integer :: iGs

  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_LATLON  = 'mesh_latlon'
  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_RASTER  = 'mesh_raster'
  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_POLYGON = 'mesh_polygon'
  character(CLEN_VAR), parameter :: BLOCKNAME__REMAPPING    = 'remapping'
  character(CLEN_VAR), parameter :: BLOCKNAME__OPT          = 'options'
  character(CLEN_VAR), parameter :: BLOCKNAME__FIG          = 'figures'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call logent('Initializing', PRCNAM, MODNAM)

  call traperr( init_mesh(s) )
  call traperr( init_mesh(t) )
  call traperr( init_rt(rt) )

  s%id = 'mesh_src'
  s%nam = MESH__SOURCE
  s%is_source = .true.

  t%id = 'mesh_tgt'
  t%nam = MESH__TARGET
  t%is_source = .false.

  rt%id = 'rt'

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

  ! Read the settings
  !-------------------------------------------------------------
  call init_counter()

  do
    call find_block(block_name)

    selectcase( block_name )
    !-----------------------------------------------------------
    ! Case: No more block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Case: gs_latlon
    case( BLOCKNAME__MESH_LATLON )
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, s, t, a)
      call read_settings_gs_latlon(a)
    !-----------------------------------------------------------
    ! Case: gs_raster
    case( BLOCKNAME__MESH_RASTER )
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, s, t, a)
      call read_settings_gs_raster(a)
    !-----------------------------------------------------------
    ! Case: gs_polygon
    case( BLOCKNAME__MESH_POLYGON )
      call update_counter(counter%gs, block_name)
      call select_gs(counter%gs, s, t, a)
      call read_settings_gs_polygon(a)
    !-----------------------------------------------------------
    ! Case: rt
    case( BLOCKNAME__REMAPPING )
      call update_counter(counter%rmp, block_name)
      call read_settings_remapping(rt, s, t)
    !-----------------------------------------------------------
    ! Case: opt
    case( BLOCKNAME__OPT )
      call update_counter(counter%opt, block_name)
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: fig
    case( BLOCKNAME__FIG )
      call update_counter(counter%fig, block_name)
      call skip_unused_block()
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('block_name', block_name)//&
                '\nCheck names of the blocks.')
    endselect
  enddo

  call close_setting_file()

  call check_number_of_blocks()

  call logext()
  !-------------------------------------------------------------
  ! Check inputs
  !-------------------------------------------------------------
  call check_input(s, t, rt, opt%earth)
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call logent('Setting some variables', PRCNAM, MODNAM)

  rtm => rt%main

  ! Remapping table
  !-------------------------------------------------------------
  call traperr( apply_oldfiles_rt_main_file(rt, opt%sys%old_files) )

  ! Directory of interemdiates
  !-------------------------------------------------------------
  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(get_path_report())
    call logmsg('Directory of intermediates was not given.'//&
              '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

  ! Missing values
  ! Path of intermediates of grid
  !-------------------------------------------------------------
  do iGs = 1, 2
    call select_gs(iGs, s, t, a)
    ac => a%cmn

    call traperr( set_miss_file_grid_in(&
           ac%f_grid_in, &
           ac%idx_miss, ac%ara_miss, ac%wgt_miss, &
           ac%xyz_miss, ac%lonlat_miss, ac%val_miss) )

    call traperr( set_miss_file_grid_out(&
           ac%f_grid_out, &
           ac%idx_miss, ac%ara_miss, ac%wgt_miss, &
           ac%xyz_miss, ac%lonlat_miss, ac%val_miss) )

    call traperr( set_save_file_grid_out(ac%f_grid_out) )
  enddo

  ! Copy options to the module variables
  !-------------------------------------------------------------
  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)
  call set_opt_earth(opt%earth)

  call logext()
  !-------------------------------------------------------------
  ! Print the settings
  !-------------------------------------------------------------
  call logent('Printing the settings', PRCNAM, MODNAM, '-p -x2')

  do iGs = 1, 2
    call select_gs(iGs, s, t, a)

    selectcase( a%typ )
    case( MESHTYPE__LATLON )
      call echo_settings_gs_latlon(a%latlon)

    case( MESHTYPE__RASTER )
      call echo_settings_gs_raster(a%raster)

    case( MESHTYPE__POLYGON )
      call echo_settings_gs_polygon(a%polygon)

    case default
      call errend(msg_invalid_value(str(a%id)//'%typ', a%typ))
    endselect
  enddo

  call echo_settings_remapping(rt, s, t)

  call echo_settings_opt(opt)

  call logmsg(bar(''))

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(s, t, rt, opt%sys)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__init_counter'

  counter%gs = 0
  counter%rmp = 0
  counter%opt = 0
  counter%fig = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n, block_name)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__update_counter'
  integer, intent(inout) :: n
  character(*), intent(in) :: block_name

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  selectcase( block_name )
  case( BLOCKNAME__MESH_LATLON, &
        BLOCKNAME__MESH_RASTER, &
        BLOCKNAME__MESH_POLYGON )
    if( n > 2 )then
      call errend(msg_invalid_input(line_number())//&
                '\nBlocks of mesh appeared more than twice:'//&
                '\n  "'//str(BLOCKNAME__MESH_LATLON)//'"'//&
                '\n  "'//str(BLOCKNAME__MESH_RASTER)//'"'//&
                '\n  "'//str(BLOCKNAME__MESH_POLYGON)//'"')
    endif
  case( BLOCKNAME__REMAPPING )
    call check_num_of_key(n, block_name, 0, 2)
  case( BLOCKNAME__OPT )
    call check_num_of_key(n, block_name, 0, 1)
  case( BLOCKNAME__FIG )
    call check_num_of_key(n, block_name, 0, 1)
  case default
    call errend(msg_invalid_value('block_name', block_name))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__check_number_of_blocks'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( counter%gs /= 2 )then
    call errend(msg_invalid_input()//&
              '\nThe number of blocks of mesh is invalid:'//&
              '\n  "'//str(BLOCKNAME__MESH_LATLON)//'"'//&
              '\n  "'//str(BLOCKNAME__MESH_RASTER)//'"'//&
              '\n  "'//str(BLOCKNAME__MESH_POLYGON)//'"')
  endif

  call check_num_of_key(counter%rmp, BLOCKNAME__REMAPPING, 1, 1)

  call check_num_of_key(counter%opt, BLOCKNAME__OPT, 0, 1)

  call check_num_of_key(counter%fig, BLOCKNAME__FIG, 0, 1)
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
subroutine read_settings_gs_latlon(a)
  use c1_set, only: &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
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
  type(gs_), intent(inout), target :: a

  type(gs_latlon_)     , pointer :: al
  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

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
  call set_keynum('coord_unit' , 0, 1)

  call set_keynum('idx_bgn'   , 0, 1)

  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)
  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('val_miss'   , 0, 1)

  call set_keynum('idx_debug', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  call traperr( init_mesh_latlon(a) )
  call traperr( set_mesh_common(a) )

  al => a%latlon
  fl     => al%f_latlon_in
  fg_in  => al%f_grid_in
  fg_out => al%f_grid_out

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
    ! Name
    case( 'name' )
      call read_value(a%nam)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(al%nx)
    case( 'ny' )
      call read_value(al%ny)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(al%west)
    case( 'east' )
      call read_value(al%east)
    case( 'south' )
      call read_value(al%south)
    case( 'north' )
      call read_value(al%north)
    !-----------------------------------------------------------
    ! Y-axis
    case( 'is_south_to_north' )
      call read_value(al%is_south_to_north)
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
      call read_value(al%coord_unit, is_keyword=.true.)
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

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      call read_value(fg_in%ub(2), pos=2)

    case( 'unit_ara' )
      call read_value(fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(al%idx_miss)
    case( 'ara_miss' )
      call read_value(al%ara_miss)
    case( 'wgt_miss' )
      call read_value(al%wgt_miss)
    case( 'val_miss' )
      call read_value(al%val_miss)
    !-----------------------------------------------------------
    ! Debugging
    case( 'idx_debug' )
      call read_value(al%idx_debug)
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

  if( keynum('west') == 1 )then
    call traperr( check_bounds_lon(al%west , al%east ) )
  endif
  if( keynum('south') == 1 )then
    call traperr( check_bounds_lat(al%south, al%north) )
  endif

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  call traperr( set_bounds_file_latlon_in(&
         fl, al%nx, al%ny,                       &  ! in
         al%nh, al%hi, al%hf, al%nv, al%vi, al%vf) )! out
  call traperr( set_bounds_file_grid_in(&
         fg_in, al%nx, al%ny) )
  call traperr( set_bounds_file_grid_out(&
         fg_out, al%nx, al%ny) )

  call traperr( set_gs_debug(&
         al%debug, al%idx_debug, al%idx_miss, &
         keynum('idx_debug')==1) )

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

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Coords.
  !-------------------------------------------------------------
  if( keynum('west') == 0 .and. keynum('east') == 0 )then
!    if( keynum('f_lon_bound') == 0 )then
!      call errend(msg_invalid_input()//&
!                '\nInformation of longitude is missing.'//&
!                  ' Give "west" and "east", or give "f_lon_bound".')
!    endif
  elseif( keynum('west') == 1 .and. keynum('east') == 1 )then
!    if( keynum('f_lon_bound') == 1 )then
!      call errend(msg_invalid_input()//&
!                '\nInformation of longitude is duplicated.'//&
!                  ' Give "west" and "east", or give "f_lon_bound".')
!    endif
  elseif( keynum('west') == 1 .neqv. keynum('east') == 1 )then
    call errend(msg_invalid_input()//&
              '\nIf either "west" or "east" is given, both must be given.')
  endif

  if( keynum('south') == 0 .and. keynum('north') == 0 )then
!    if( keynum('f_lon_bound') == 0 )then
!      call errend(msg_invalid_input()//&
!                '\nInformation of longitude is missing.'//&
!                  ' Give "south" and "north", or give "f_lon_bound".')
!    endif
  elseif( keynum('south') == 1 .and. keynum('north') == 1 )then
!    if( keynum('f_lon_bound') == 1 )then
!      call errend(msg_invalid_input()//&
!                '\nInformation of longitude is duplicated.'//&
!                  ' Give "south" and "north", or give "f_lon_bound".')
!    endif
  elseif( keynum('south') == 1 .neqv. keynum('north') == 1 )then
    call errend(msg_invalid_input()//&
              '\nIf either "south" or "north" is given both must be given.')
  endif

  if( keynum('f_lon_bound') == 0 .and. keynum('f_lat_bound') == 0 .and. &
      keynum('coord_unit') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"coord_unit" is given although '//&
                'neither "f_lon_bound" or "f_lat_bound" is given.'//&
                ' The input for "coord_unit" is ignored.')
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
    call errend('"fin_grdara" and "fin_grdwgt" must not be given at the same time.')
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
  !! idx_miss might be refered in block "figures"
  !if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
  !  call logwrn(msg_undesirable_input()//&
  !            '\n"idx_miss" is given but "fin_grdidx" is given.'//&
  !              ' The input given by "idx_miss" is ignored.')
  !endif

  if( keynum('fin_grdara') == 0 .and. keynum('ara_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"ara_miss" is given although "fin_grdara" is not given.'//&
                ' The input given by "ara_miss" is ignored.')
  endif

  if( keynum('fin_grdwgt') == 0 .and. keynum('wgt_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"wgt_miss" is given although "fin_grdwgt" is not given.'//&
                ' The input given by "wgt_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine read_settings_gs_raster(a)
  use c1_const_util, only: &
        checkval_idx_condition
  use c1_set, only: &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
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
  use c1_gs_util, only: &
        set_gs_debug
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_gs_raster'
  type(gs_), intent(inout), target :: a

  type(gs_raster_)     , pointer :: ar
  type(file_raster_in_), pointer :: fr
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('name', 0, 1)

  call set_keynum('nx', 0, 1)
  call set_keynum('ny', 0, 1)
  call set_keynum('xi', 0, 1)
  call set_keynum('xf', 0, 1)
  call set_keynum('yi', 0, 1)
  call set_keynum('yf', 0, 1)
  call set_keynum('west' , 0, 1)
  call set_keynum('east' , 0, 1)
  call set_keynum('south', 0, 1)
  call set_keynum('north', 0, 1)
  call set_keynum('is_south_to_north', 0, 1)

  call set_keynum('dir', 0, -1)

  call set_keynum('fin_rstidx', 0, 1)
  call set_keynum('fin_rstara', 0, 1)
  call set_keynum('fin_rstwgt', 0, 1)
  call set_keynum('in_raster_sz', 0, 1)
  call set_keynum('in_raster_lb', 0, 1)
  call set_keynum('in_raster_ub', 0, 1)

  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)

  call set_keynum('idx_condition', 0, 1)
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

  call traperr( init_mesh_raster(a) )
  call traperr( set_mesh_common(a) )

  ar => a%raster
  fr     => ar%f_raster_in
  fg_in  => ar%f_grid_in
  fg_out => ar%f_grid_out

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
    ! Name
    case( 'name' )
      call read_value(a%nam)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ar%nx)
    case( 'ny' )
      call read_value(ar%ny)
    case( 'xi' )
      call read_value(ar%xi)
    case( 'xf' )
      call read_value(ar%xf)
    case( 'yi' )
      call read_value(ar%yi)
    case( 'yf' )
      call read_value(ar%yf)
    !-----------------------------------------------------------
    ! Region
    case( 'west' )
      call read_value(ar%west)
    case( 'east' )
      call read_value(ar%east)
    case( 'south' )
      call read_value(ar%south)
    case( 'north' )
      call read_value(ar%north)
    !-----------------------------------------------------------
    ! Y-axis
    case( 'is_south_to_north' )
      call read_value(ar%is_south_to_north)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Raster data
    case( 'fin_rstidx' )
      call read_value(fr%idx, dir)
    case( 'fin_rstara' )
      call read_value(fr%ara, dir)
    case( 'fin_rstwgt' )
      call read_value(fr%wgt, dir)

    case( 'in_raster_sz' )
      call read_value(fr%sz(1), pos=1)
      call read_value(fr%sz(2), pos=2)
    case( 'in_raster_lb' )
      call read_value(fr%lb(1), pos=1)
      call read_value(fr%lb(2), pos=2)
    case( 'in_raster_ub' )
      call read_value(fr%ub(1), pos=1)
      call read_value(fr%ub(2), pos=2)
    !-----------------------------------------------------------
    ! Grid data
    case( 'fin_grdidx' )
      call read_value(fg_in%idx, dir)
    case( 'fin_grdara' )
      call read_value(fg_in%ara, dir)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt, dir)

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      call read_value(fg_in%ub(2), pos=2)
    !-----------------------------------------------------------
    ! Raster data and grid data
    case( 'idx_condition' )
      call read_value(ar%idx_condition, is_keyword=.true.)

    case( 'in_unit_ara' )
      call read_value(fg_in%unit_ara, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(ar%idx_miss)
    case( 'ara_miss' )
      call read_value(ar%ara_miss)
    case( 'wgt_miss' )
      call read_value(ar%wgt_miss)
    case( 'val_miss' )
      call read_value(ar%val_miss)
    !-----------------------------------------------------------
    ! Debugging
    case( 'idx_debug' )
      call read_value(ar%idx_debug)
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

  call traperr( check_bounds_lon(ar%west , ar%east ) )
  call traperr( check_bounds_lat(ar%south, ar%north) )

  call traperr( checkval_idx_condition(ar%idx_condition, 'ar%idx_condition') )

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  call traperr( set_bounds_file_raster_in(&
         fr,                                     &  ! inout
         ar%nx, ar%ny, ar%is_south_to_north,     &  ! in
         ar%xi, ar%xf, ar%yi, ar%yf,             &  ! out
         ar%nh, ar%hi, ar%hf, ar%nv, ar%vi, ar%vf) )! out
  call traperr( set_bounds_file_grid_in(&
         fg_in) )
  call traperr( set_bounds_file_grid_out(&
         fg_out, fg_in%nx, fg_in%ny) )

  call traperr( set_gs_debug(&
         ar%debug, ar%idx_debug, ar%idx_miss, &
         keynum('idx_debug')==1) )

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
  ! Grid data
  !-------------------------------------------------------------
  if( keynum('fin_grdidx') == 0 .and. &
      (keynum('fin_grdara') == 1 .or. &
       keynum('fin_grdwgt') == 1) )then
    call errend(msg_invalid_input()//&
              '\n"fin_grdara" or "fin_grdwgt"'//&
                ' although "fin_grdidx" is not given.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call errend(msg_undesirable_input()//&
              '\nBoth "fin_grdara" and "fin_grdwgt" must not be given'//&
                ' at the same time.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call logwrn(msg_undesirable_input()//&
              '\n"in_grid_sz", "in_grid_lb" or "in_grid_ub" is given'//&
                ' although "fin_grdidx" is not given.'//&
                ' The inputs for "in_grid_*" are ignored.')
  endif
  !-------------------------------------------------------------
  ! Missing value
  !-------------------------------------------------------------
  if( (keynum('fin_grdara') == 0 .and. keynum('fin_rstara') == 0) .and. &
      keynum('ara_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"ara_miss" is given although "fin_grdara" is not given.'//&
                ' The input for "ara_miss" is ignored.')
  endif

  if( (keynum('fin_grdwgt') == 0 .and. keynum('fin_rstwgt') == 0) .and. &
      keynum('wgt_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"wgt_miss" is given although "fin_grdwgt" is not given.'//&
                ' The input for "wgt_miss" is ignored.')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!---------------------------------------------------------------
end subroutine read_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine read_settings_gs_polygon(a)
  use c1_set, only: &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
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
        set_bounds_file_polygon_in, &
        set_bounds_file_grid_in   , &
        set_bounds_file_grid_out
  use c1_gs_util, only: &
        set_gs_debug
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_gs_polygon'
  type(gs_), intent(inout), target :: a

  type(gs_polygon_)     , pointer :: ap
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out

  character(CLEN_PATH) :: dir
  real(8) :: coord_miss

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('name', 0, 1)

  call set_keynum('np', 0, 1)
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

  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('val_miss'   , 0, 1)

  call set_keynum('idx_debug', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  call traperr( init_mesh_polygon(a) )
  call traperr( set_mesh_common(a) )

  ap => a%polygon
  fp     => ap%f_polygon_in
  fg_in  => ap%f_grid_in
  fg_out => ap%f_grid_out

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
    ! Name
    case( 'name' )
      call read_value(a%nam)
    !-----------------------------------------------------------
    ! Shape
    case( 'np' )
      call read_value(ap%np)
    !-----------------------------------------------------------
    ! Resolution
    case( 'nij' )
      call read_value(ap%nij)
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
      call read_value(ap%coord_unit, is_keyword=.true.)

    case( 'coord_miss' )
      call read_value(coord_miss)
    !-----------------------------------------------------------
    ! Arc type
    case( 'f_arctyp' )
      call read_value(fp%arctyp, dir)

    case( 'arc_parallel' )
      call read_value(ap%arc_parallel)
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

    case( 'in_grid_sz' )
      call read_value(fg_in%sz(1), pos=1)
      !call read_value(fg_in%sz(2), pos=2)
    case( 'in_grid_lb' )
      call read_value(fg_in%lb(1), pos=1)
      !call read_value(fg_in%lb(2), pos=2)
    case( 'in_grid_ub' )
      call read_value(fg_in%ub(1), pos=1)
      !call read_value(fg_in%ub(2), pos=2)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(ap%idx_miss)
    case( 'ara_miss' )
      call read_value(ap%ara_miss)
    case( 'wgt_miss' )
      call read_value(ap%wgt_miss)
    case( 'val_miss' )
      call read_value(ap%val_miss)
    !-----------------------------------------------------------
    ! Debugging
    case( 'idx_debug' )
      call read_value(ap%idx_debug)
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

  call traperr( set_bounds_file_polygon_in(&
         fp, ap%ijs, ap%ije, ap%np, ap%nij) )
  call traperr( set_bounds_file_grid_in(&
         fg_in, ap%nij, 1_8) )
  call traperr( set_bounds_file_grid_out(&
         fg_out, ap%nij, 1_8) )

  ! Coordinate system
  !-------------------------------------------------------------
  if( fp%lon%path /= '' )then
    ap%coord_sys = COORD_SYS_SPHERICAL

    if( keynum('coord_unit') == 0 )then
      ap%coord_unit = UNIT_DEGREE
    else
      if( ap%coord_unit /= UNIT_DEGREE .and. &
          ap%coord_unit /= UNIT_RADIAN )then
        call errend('Invalid value in $ap%coord_unit: '//str(ap%coord_unit)//&
                  '\nThis is invalid when "f_lon_vertex" is given.')
      endif
    endif

    if( keynum('coord_miss') == 1 ) ap%coord_miss_s = coord_miss
  else
    ap%coord_sys = COORD_SYS_CARTESIAN

    if( keynum('coord_unit') == 0 )then
      ap%coord_unit = UNIT_METER
    else
      if( ap%coord_unit /= UNIT_METER .and. &
          ap%coord_unit /= UNIT_KILOMETER )then
        call errend('Invalid value in $ap%coord_unit: '//str(ap%coord_unit)//&
                  '\nThis is invalid when "f_x_vertex" is given.')
      endif
    endif

    if( keynum('coord_miss') == 1 ) ap%coord_miss_c = coord_miss
  endif

  ! About debugging
  !-------------------------------------------------------------
  call traperr( set_gs_debug(&
         ap%debug, ap%idx_debug, ap%idx_miss, &
         keynum('idx_debug')==1) )

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
              '\n"f_lon_vertex" and "f_x_vertex" must not be given at the same time.')
  elseif( keynum('f_lon_vertex') == 0 .and. keynum('f_x_vertex') == 0 )then
!    call errend(msg_invalid_input()//&
!              '\nEither "f_lon_vertex" or "f_x_vertex" must be given.')
  endif

  if( keynum('arc_parallel') == 1 .and. keynum('f_arctyp') == 1 )then
    call errend(msg_invalid_input()//&
              '\n"arc_parallel" and "f_arctyp" must not be given at the same time.')
  endif
  !--------------------------------------------------------------
  ! Grid data
  !--------------------------------------------------------------
  if( keynum('idx_bgn') == 1 .and. keynum('fin_grdidx') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\n"idx_bgn" is given although "fin_grdidx" is given.'//&
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
              '\n  "in_grid_sz"'//&
              '\n  "in_grid_lb"'//&
              '\n  "in_grid_ub"'//&
              '\nbut any value is not given by the following keywords:'//&
              '\n  "fin_grdidx"'//&
              '\n  "fin_grdara"'//&
              '\n  "fin_grdwgt"'//&
              '\nThe inputs given by the former keywords are ignored.')
  endif
  !-------------------------------------------------------------
  ! Missing value
  !-------------------------------------------------------------
  !! idx_miss might be refered in block "figures"
  !if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
  !  call logwrn(msg_undesirable_input()//&
  !            '\n"idx_miss" is given but "fin_grdidx" is given.'//&
  !              ' The input for "idx_miss" is ignored.')
  !endif

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
subroutine read_settings_remapping(rt, s, t)
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
        alloc_file_grid_in_val, &
        alloc_file_grid_out_val
  use c2_rt_base, only: &
        set_default_values_rt
  use c2_rt_set, only: &
        KEY_OPT_COEF_SUM_MODIFY      , &
        KEY_OPT_COEF_SUM_MODIFY_ULIM , &
        KEY_OPT_COEF_ZERO_POSITIVE   , &
        KEY_OPT_COEF_ZERO_NEGATIVE   , &
        KEY_OPT_COEF_ERROR_EXCESS    , &
        KEY_OPT_COEF_SUM_ERROR_EXCESS
  use c2_rt_set, only: &
        check_values_opt_rt_coef
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_remapping'
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(inout), target :: s, t

  type(rt_main_), pointer :: rtm
  type(rt_vrf_), pointer :: rtv
  type(gs_common_)  , pointer :: sc, tc

  character(CLEN_PATH) :: dir
  character(CLEN_KEY) :: mesh_vrf

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('rt_status', 0, 1)
  call set_keynum('mode', 0, 1)

  call set_keynum('dir', 0, -1)

  call set_keynum('fin_grdval' , 0, -1)
  call set_keynum('fout_grdval', 0, -1)

  call set_keynum('length_rt', 0, 1)
  call set_keynum('fin_rt_sidx', 0, 1)
  call set_keynum('fin_rt_tidx', 0, 1)
  call set_keynum('fin_rt_area', 0, 1)
  call set_keynum('fin_rt_coef', 0, 1)

  call set_keynum('fout_rt_sidx', 0, 1)
  call set_keynum('fout_rt_tidx', 0, 1)
  call set_keynum('fout_rt_area', 0, 1)
  call set_keynum('fout_rt_coef', 0, 1)

  call set_keynum('mesh_coef', 0, 1)
  call set_keynum('mesh_sort', 0, 1)
  call set_keynum('allow_empty', 0, 1)
  call set_keynum('opt_coef_sum_modify'      , 0, 1)
  call set_keynum('opt_coef_sum_modify_ulim' , 0, 1)
  call set_keynum('opt_coef_zero_positive'   , 0, 1)
  call set_keynum('opt_coef_zero_negative'   , 0, 1)
  call set_keynum('opt_coef_error_excess'    , 0, 1)
  call set_keynum('opt_coef_sum_error_excess', 0, 1)

  call set_keynum('mesh_vrf'            , 0, 2)
  call set_keynum('fout_vrf_grdidx'     , 0, 2)
  call set_keynum('fout_vrf_grdara_true', 0, 2)
  call set_keynum('fout_vrf_grdara_rt'  , 0, 2)
  call set_keynum('fout_vrf_rerr_grdara', 0, 2)
  call set_keynum('fout_vrf_grdnum'     , 0, 2)
  call set_keynum('fout_vrf_iarea_sum'  , 0, 2)
  call set_keynum('fout_vrf_iratio_sum' , 0, 2)
  call set_keynum('vrf_val_miss'        , 0, 2)

  call logext()
  !-------------------------------------------------------------
  ! Count the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Counting the number of times each keyword is used', PRCNAM, MODNAM)

  sc => s%cmn
  tc => t%cmn
  sc%f_grid_in%nFiles_val = 0
  tc%f_grid_out%nFiles_val = 0

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
      call add(sc%f_grid_in%nFiles_val)
    case( 'fout_grdval' )
      call add(tc%f_grid_out%nFiles_val)
    !-----------------------------------------------------------
    !
    case default
      continue
    endselect
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  call traperr( set_default_values_rt(rt) )

  rtm => rt%main

  call traperr( alloc_file_grid_in_val(sc%f_grid_in) )
  call traperr( alloc_file_grid_out_val(tc%f_grid_out) )

  call logext()
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  call back_to_block_head()
  call reset_keynum()

  dir = ''
  sc%f_grid_in%nFiles_val = 0
  tc%f_grid_out%nFiles_val = 0

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
    case( 'mode' )
      call read_value(rtm%mode, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Grid value
    case( 'fin_grdval' )
      call add(sc%f_grid_in%nFiles_val)
      call read_value(sc%f_grid_in%val(sc%f_grid_in%nFiles_val), dir)
    case( 'fout_grdval' )
      call add(tc%f_grid_out%nfiles_val)
      call read_value(tc%f_grid_out%val(tc%f_grid_out%nFiles_val), dir)
    !-----------------------------------------------------------
    ! Input remapping table
    case( 'length_rt' )
      call read_value(rtm%nij)
    case( 'fin_rt_sidx' )
      call read_value(rtm%f%sidx, dir)
    case( 'fin_rt_tidx' )
      call read_value(rtm%f%tidx, dir)
    case( 'fin_rt_area' )
      call read_value(rtm%f%area, dir)
    case( 'fin_rt_coef' )
      call read_value(rtm%f%coef, dir)
    !-----------------------------------------------------------
    ! Output remapping table
    case( 'fout_rt_sidx' )
      call read_value(rtm%f%sidx, dir)
    case( 'fout_rt_tidx' )
      call read_value(rtm%f%tidx, dir)
    case( 'fout_rt_area' )
      call read_value(rtm%f%area, dir)
    case( 'fout_rt_coef' )
      call read_value(rtm%f%coef, dir)
    !-----------------------------------------------------------
    ! Options for remapping table
    case( 'mesh_coef' )
      call read_value(rtm%mesh_coef, is_keyword=.true.)

    case( 'mesh_sort' )
      call read_value(rtm%mesh_sort, is_keyword=.true.)

    case( 'allow_empty' )
      call read_value(rtm%allow_empty)

    case( KEY_OPT_COEF_SUM_MODIFY )
      call read_value(rtm%opt_coef%sum_modify)
      rtm%opt_coef%is_sum_modify_enabled = .true.
    case( KEY_OPT_COEF_SUM_MODIFY_ULIM )
      call read_value(rtm%opt_coef%sum_modify_ulim)
      rtm%opt_coef%is_sum_modify_ulim_enabled = .true.
    case( KEY_OPT_COEF_ZERO_POSITIVE )
      call read_value(rtm%opt_coef%zero_positive)
      rtm%opt_coef%is_zero_positive_enabled = .true.
    case( KEY_OPT_COEF_ZERO_NEGATIVE )
      call read_value(rtm%opt_coef%zero_negative)
      rtm%opt_coef%is_zero_negative_enabled = .true.
    case( KEY_OPT_COEF_ERROR_EXCESS )
      call read_value(rtm%opt_coef%error_excess)
      rtm%opt_coef%is_error_excess_enabled = .true.
    case( KEY_OPT_COEF_SUM_ERROR_EXCESS )
      call read_value(rtm%opt_coef%sum_error_excess)
      rtm%opt_coef%is_sum_error_excess_enabled = .true.
    !-----------------------------------------------------------
    ! Verification data
    case( 'mesh_vrf' )
      call read_value(mesh_vrf, is_keyword=.true.)
      selectcase( mesh_vrf )
      case( MESH__SOURCE )
        rtv => rt%vrf_src
      case( MESH__TARGET )
        rtv => rt%vrf_tgt
      case default
        call errend(msg_invalid_value('mesh_vrf', mesh_vrf))
      endselect

    case( 'fout_vrf_grdidx' )
      call read_value_fvrf_file(rtv%f%out_grdidx)
    case( 'fout_vrf_grdara_true' )
      call read_value_fvrf_file(rtv%f%out_grdara_true)
    case( 'fout_vrf_grdara_rt' )
      call read_value_fvrf_file(rtv%f%out_grdara_rt)
    case( 'fout_vrf_rerr_grdara' )
      call read_value_fvrf_file(rtv%f%out_rerr_grdara)
    case( 'fout_vrf_grdnum' )
      call read_value_fvrf_file(rtv%f%out_grdnum)
    case( 'fout_vrf_iarea_sum' )
      call read_value_fvrf_file(rtv%f%out_iarea_sum)
    case( 'fout_vrf_iratio_sum' )
      call read_value_fvrf_file(rtv%f%out_iratio_sum)

    case( 'vrf_val_miss' )
      call read_value(rtv%dval_miss)
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
  ! Check and modify the values
  !-------------------------------------------------------------
  call logent('Checking and modifying the values', PRCNAM, MODNAM)

  call check_values_opt_rt_coef(rtm%opt_coef)

  if( keynum('fout_rt_sidx') == 1 )then
    rt%status = RT_STATUS__MAKE
  elseif( keynum('fin_rt_sidx') == 1 )then
    rt%status = RT_STATUS__READ
  else
    rt%status = RT_STATUS__NONE
  endif

  call logext()
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !--------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine read_value_fvrf_file(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value_fvrf_file'
  type(file_), intent(inout) :: f

  type(gs_), pointer :: a

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( mesh_vrf )
  case( MESH__SOURCE )
    a => s
  case( MESH__TARGET )
    a => t
  case default
    call errend(msg_invalid_value('mesh_vrf', mesh_vrf))
  endselect

  selectcase( a%typ )
  case( MESHTYPE__LATLON, MESHTYPE__POLYGON )
    if( key() /= 'fout_vrf_grdidx'      .and. &
        key() /= 'fout_vrf_grdara_true' .and. &
        key() /= 'fout_vrf_grdara_rt'   .and. &
        key() /= 'fout_vrf_rerr_grdara' .and. &
        key() /= 'fout_vrf_grdnum'      )then
      call errend(msg_invalid_input(line_number())//&
                '\nOnly the following keys can be used for the group of'//&
                  ' verification data of '//str(a%typ)//' meshes:'//&
                '\n  "fout_vrf_grdidx"'//&
                '\n  "fout_vrf_grdara_true"'//&
                '\n  "fout_vrf_grdara_rt"'//&
                '\n  "fout_vrf_rerr_grdara"'//&
                '\n  "fout_vrf_grdnum"')
    endif
  case( MESHTYPE__RASTER )
    if( key() /= 'fout_vrf_grdidx'      .and. &
        key() /= 'fout_vrf_grdara_true' .and. &
        key() /= 'fout_vrf_grdara_rt'   .and. &
        key() /= 'fout_vrf_rerr_grdara' .and. &
        key() /= 'fout_vrf_grdnum'      .and. &
        key() /= 'fout_vrf_iarea_sum'   .and. &
        key() /= 'fout_vrf_iratio_sum'  )then
      call errend(msg_invalid_input(line_number())//&
                '\nOnly the following keys can be used for the group of'//&
                  ' verification data of '//str(a%typ)//' meshes:'//&
                '\n  "fout_vrf_grdidx"'//&
                '\n  "fout_vrf_grdara_true"'//&
                '\n  "fout_vrf_grdara_rt"'//&
                '\n  "fout_vrf_rerr_grdara"'//&
                '\n  "fout_vrf_grdnum"'//&
                '\n  "fout_vrf_iarea_sum"'//&
                '\n  "fout_vrf_iratio_sum"')
    endif
  case default
    call errend(msg_invalid_value('a%typ', a%typ))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path /= '' )then
    call errend(msg_invalid_input(line_number())//&
              '\nDuplicated input of "'//str(key())//'".')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_value(f, dir=dir)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_value_fvrf_file
!----------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_keynum_relations'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !--------------------------------------------------------------
  ! Remapping table
  !--------------------------------------------------------------
  if( keynum('fin_rt_sidx') == 1 .and. &
      keynum('fin_rt_tidx') == 1 .and. &
      keynum('fin_rt_area') == 1 .and. &
      keynum('fin_rt_coef') == 1 )then
    if( keynum('length_rt') == 0 )then
      call errend(msg_invalid_input()//&
                '\nPre-existing remapping table was specified '//&
                  'without specifying its length. '//&
                  'Specify the length by "length_rt".')
    endif
    if( keynum('fout_rt_sidx') == 1 .or. &
        keynum('fout_rt_tidx') == 1 .or. &
        keynum('fout_rt_area') == 1 .or. &
        keynum('fout_rt_coef') == 1 )then
      call errend(msg_invalid_input()//&
                '\nOutput remapping table cannot be specified '//&
                 'when pre-existing remapping table was specified.')
    endif
  elseif( keynum('fout_rt_sidx') == 1 .and. &
          keynum('fout_rt_tidx') == 1 .and. &
          keynum('fout_rt_area') == 1 .and. &
          keynum('fout_rt_coef') == 1 )then
    if( keynum('fin_rt_sidx') == 1 .or. &
        keynum('fin_rt_tidx') == 1 .or. &
        keynum('fin_rt_area') == 1 .or. &
        keynum('fin_rt_coef') == 1 )then
      call errend(msg_invalid_input()//&
                '\nPre-existing remapping table cannot be specified '//&
                 'when output remapping table was specified.')
    endif
  endif
  !--------------------------------------------------------------
  ! Remapping data
  !--------------------------------------------------------------
  if( keynum('fin_grdval') /= keynum('fout_grdval') )then
    call errend(msg_invalid_input()//&
              '\nThe number of input and output data does not match.'//&
              '\n  "fin_grdval"  (in) : '//str(keynum('fin_grdval'))//&
              '\n  "fout_grdval" (out): '//str(keynum('fout_grdval')))
  endif
  !--------------------------------------------------------------
  ! Remapping data and remapping table
  !-------------------------------------------------------------
!  if( keynum('fin_grdval') > 0 )then
!    if( keynum('fout_rt_sidx') == 0 .or. &
!        keynum('fout_rt_tidx') == 0 .or. &
!        keynum('fout_rt_coef') == 0 )then
!      call errend(msg_invalid_input()//&
!                '\n  Output file of sidx, tidx, coef of the remapping table '//&
!                  'cannnot be omitted when the remapping data was specified.'//&
!                '\n  Please specify "'//str(key_fout_rt_sidx)//&
!                  '", "'//str(key_fout_rt_tidx)//'", "'//str(key_fout_rt_coef)//'".')
!    endif
!  endif
  !--------------------------------------------------------------
  ! Options for coef
  !--------------------------------------------------------------
  if( keynum('fout_rt_coef') == 0 )then
    if( keynum('mesh_coef') == 1 )then
      call logwrn(msg_undesirable_input()//&
                 '\n"mesh_coef is given although "fout_rt_coef" is not given.'//&
                    ' The input given by "mesh_coef" is ignored.')
    endif

    if( keynum(KEY_OPT_COEF_SUM_MODIFY)       == 1 .or. &
        keynum(KEY_OPT_COEF_SUM_MODIFY_ULIM)  == 1 .or. &
        keynum(KEY_OPT_COEF_ZERO_POSITIVE)    == 1 .or. &
        keynum(KEY_OPT_COEF_ZERO_NEGATIVE)    == 1 .or. &
        keynum(KEY_OPT_COEF_ERROR_EXCESS)     == 1 .or. &
        keynum(KEY_OPT_COEF_SUM_ERROR_EXCESS) == 1 )then
      call logwrn(msg_undesirable_input()//&
                '\nAny of the following keywords is given:'//&
                '\n  "'//str(KEY_OPT_COEF_SUM_ERROR_EXCESS)//'"'//&
                '\n  "'//str(KEY_OPT_COEF_SUM_MODIFY_ULIM)//'"'//&
                '\n  "'//str(KEY_OPT_COEF_ZERO_POSITIVE)//'"'//&
                '\n  "'//str(KEY_OPT_COEF_ZERO_NEGATIVE)//'"'//&
                '\n  "'//str(KEY_OPT_COEF_ERROR_EXCESS)//'"'//&
                '\n  "'//str(KEY_OPT_COEF_SUM_ERROR_EXCESS)//'"'//&
                '\nalthough "fout_rt_coef" is not given.'//&
                '\nThe inputs for these keywords are ignored.')
    endif
  endif

  if( keynum(KEY_OPT_COEF_SUM_MODIFY) == 1 .and. &
      keynum(KEY_OPT_COEF_SUM_MODIFY_ULIM) == 1 )then
    call errend(msg_invalid_input()//&
              '\n"'//str(KEY_OPT_COEF_SUM_MODIFY)//'" and "'//&
                str(KEY_OPT_COEF_SUM_MODIFY_ULIM)//&
                '" must not be given at the same time.')
  endif
  !--------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_remapping
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  use c1_const_util, only: &
        checkval_opt_old_files
  use c1_set, only: &
        key                    , &
        keynum                 , &
        alloc_keynum           , &
        free_keynum            , &
        set_keynum             , &
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
subroutine skip_unused_block()
  use c1_set, only: &
        key       , &
        read_input
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'skip_unused_block'

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  do
    call read_input()

    selectcase( key() )
    case( '' )
      exit
    case default
      continue
    endselect
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine skip_unused_block
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
    s, t, rt, opt_earth)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input'
  type(gs_), intent(in), target :: s, t
  type(rt_), intent(in) :: rt
  type(opt_earth_), intent(in) :: opt_earth

  type(gs_), pointer :: a
  integer :: iMesh

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rt%status )
  case( RT_STATUS__MAKE )
    do iMesh = 1, 2
      call select_gs(iMesh, s, t, a)

      selectcase( a%typ )
      case( MESHTYPE__LATLON )
        call check_input__mesh_latlon__rt_out(a%latlon)
        
      case( MESHTYPE__RASTER )
        call check_input__mesh_raster__rt_out(a%raster)

      case( MESHTYPE__POLYGON )
        call check_input__mesh_polygon__rt_out(a%polygon)

      case( MESHTYPE__UNDEF )
        call errend(msg_unexpected_condition()//&
                  '\n  a%typ == MESHTYPE__UNDEF'//&
                  '\n  Mesh: '//str(a%nam))

      case default
        call errend(msg_invalid_value('a%typ', a%typ)//&
                  '\n  Mesh: '//str(a%nam))
      endselect
    enddo  ! iMesh/
  case( RT_STATUS__READ )
    do iMesh = 1, 2
      call select_gs(iMesh, s, t, a)

      selectcase( a%typ )
      case( MESHTYPE__LATLON )
        call check_input__mesh_latlon__rt_in(a%latlon)
        
      case( MESHTYPE__RASTER )
        call check_input__mesh_raster__rt_in(a%raster)

      case( MESHTYPE__POLYGON )
        call check_input__mesh_polygon__rt_in(a%polygon)

      case( MESHTYPE__UNDEF )
        call errend(msg_unexpected_condition()//&
                  '\n  a%typ == MESHTYPE__UNDEF'//&
                  '\n  Mesh: '//str(a%nam))

      case default
        call errend(msg_invalid_value('a%typ', a%typ)//&
                  '\n  Mesh: '//str(a%nam))
      endselect
    enddo  ! iMesh/

  case( RT_STATUS__NONE )
    call errend(msg_unexpected_condition()//&
              '\n  rt%status == RT_STATUS__NONE')

  case( RT_STATUS__UNDEF )
    call errend(msg_unexpected_condition()//&
              '\n  rt%status == RT_STATUS__UNDEF')

  case default
    call errend(msg_invalid_value('rt%status', rt%status))

  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt_earth%shptyp == EARTH_SHPTYP__ELLIPS )then
    do iMesh = 1, 2
      call select_gs(iMesh, s, t, a)

      if( a%typ == MESHTYPE__POLYGON )then
        call errend(msg_unexpected_condition()//&
                  '\n  opt%earth%shptyp == '//str(opt_earth%shptyp)//&
                    ' and '//str(a%id)//'%typ == '//str(a%typ)//&
                  '\nEarth shape "'//str(opt_earth%shptyp)//'" is '//&
                    'not implemented yet for '//str(a%typ)//' meshes.')
      endif
    enddo
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(a)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_input
!===============================================================
!
!===============================================================
subroutine check_input__mesh_latlon__rt_out(al)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input__mesh_latlon__rt_out'
  type(gs_latlon_), intent(in) :: al

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( al%nx <= 0_8 )then
    call errend(msg_invalid_value('al%nx', al%nx)//&
              '\n  mesh: '//str(al%nam))
  endif
  if( al%ny <= 0_8 )then
    call errend(msg_invalid_value('al%ny', al%ny)//&
              '\n  mesh: '//str(al%nam))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_input__mesh_latlon__rt_out
!===============================================================
!
!===============================================================
subroutine check_input__mesh_raster__rt_out(ar)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input__mesh_raster__rt_out'
  type(gs_raster_), intent(in) :: ar

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ar%nx <= 0_8 )then
    call errend(msg_invalid_value('ar%nx', ar%nx)//&
              '\n  mesh: '//str(ar%nam))
  endif
  if( ar%ny <= 0_8 )then
    call errend(msg_invalid_value('ar%ny', ar%ny)//&
              '\n  mesh: '//str(ar%nam))
  endif
  if( ar%f_raster_in%idx%path == '' )then
    call errend(msg_invalid_value('ar%f_raster_in%idx%path', ar%f_raster_in%idx%path)//&
              '\n  mesh: '//str(ar%nam))
  endif
  !-------------------------------------------------------------
   call logret(PRCNAM, MODNAM)
end subroutine check_input__mesh_raster__rt_out
!===============================================================
!
!===============================================================
subroutine check_input__mesh_polygon__rt_out(ap)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input__mesh_polygon__rt_out'
  type(gs_polygon_), intent(in) :: ap

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ap%nij <= 0_8 )then
    call errend(msg_invalid_value('ap%nij', ap%nij)//&
              '\n  mesh: '//str(ap%nam))
  endif
  if( ap%np <= 0_8 )then
    call errend(msg_invalid_value('ap%np', ap%np)//&
              '\n  mesh: '//str(ap%nam))
  endif
  !-------------------------------------------------------------
   call logret(PRCNAM, MODNAM)
end subroutine check_input__mesh_polygon__rt_out
!===============================================================
!
!===============================================================
subroutine check_input__mesh_latlon__rt_in(al)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input__mesh_latlon__rt_in'
  type(gs_latlon_), intent(in) :: al

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( al%nx <= 0_8 )then
    call errend(msg_invalid_value('al%nx', al%nx)//&
              '\n  mesh: '//str(al%nam))
  endif
  if( al%ny <= 0_8 )then
    call errend(msg_invalid_value('al%ny', al%ny)//&
              '\n  mesh: '//str(al%nam))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_input__mesh_latlon__rt_in
!===============================================================
!
!===============================================================
subroutine check_input__mesh_raster__rt_in(ar)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input__mesh_raster__rt_in'
  type(gs_raster_), intent(in) :: ar

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ar%f_grid_in%sz(1) <= 0_8 )then
    call errend(msg_invalid_value('ar%f_grid_in%sz(1)', ar%f_grid_in%sz(1))//&
              '\n  mesh: '//str(ar%nam))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_input__mesh_raster__rt_in
!===============================================================
!
!===============================================================
subroutine check_input__mesh_polygon__rt_in(ap)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input__mesh_polygon__rt_in'
  type(gs_polygon_), intent(in) :: ap

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ap%nij <= 0_8 )then
    call errend(msg_invalid_value('ap%nij', ap%nij)//&
              '\n  mesh: '//str(ap%nam))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_input__mesh_polygon__rt_in
!===============================================================
!
!===============================================================
subroutine check_paths(s, t, rt, opt_sys)
  use c1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_paths'
  type(gs_)     , intent(in), target :: s, t
  type(rt_)     , intent(in), target :: rt
  type(opt_sys_), intent(in)         :: opt_sys

  type(gs_)             , pointer :: a
  type(file_latlon_in_) , pointer :: fl
  type(file_raster_in_) , pointer :: fr
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  type(file_grid_out_)  , pointer :: fg_out
  type(file_)           , pointer :: f
  type(rt_main_)     , pointer :: rtm
  type(rt_vrf_)      , pointer :: rtv

  integer :: iGs
  integer :: iFile

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking input files', PRCNAM, MODNAM)

  !-------------------------------------------------------------
  call logent('Meshes', PRCNAM, MODNAM)

  do iGs = 1, 2
    call select_gs(iGs, s, t, a)

    selectcase( a%typ )
    !-----------------------------------------------------------
    ! Case: Latlon
    case( MESHTYPE__LATLON )
      fl => a%latlon%f_latlon_in
      call traperr( check_permission(fl%lon, allow_empty=.true.) )
      call traperr( check_permission(fl%lat, allow_empty=.true.) )
    !-----------------------------------------------------------
    ! Case: Raster
    case( MESHTYPE__RASTER )
      fr => a%raster%f_raster_in
      call traperr( check_permission(fr%idx, allow_empty=.true.) )
      call traperr( check_permission(fr%ara, allow_empty=.true.) )
      call traperr( check_permission(fr%wgt, allow_empty=.true.) )
    !-----------------------------------------------------------
    ! Case: Polygon
    case( MESHTYPE__POLYGON )
      fp => a%polygon%f_polygon_in
      call traperr( check_permission(fp%lon   , allow_empty=.true.) )
      call traperr( check_permission(fp%lat   , allow_empty=.true.) )
      call traperr( check_permission(fp%x     , allow_empty=.true.) )
      call traperr( check_permission(fp%y     , allow_empty=.true.) )
      call traperr( check_permission(fp%z     , allow_empty=.true.) )
      call traperr( check_permission(fp%arctyp, allow_empty=.true.) )
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value(str(a%id)//'%typ', a%typ))
    endselect
    !-----------------------------------------------------------
    ! Fundamental grid data
    !-----------------------------------------------------------
    fg_in => a%cmn%f_grid_in
    call traperr( check_permission(fg_in%idx, allow_empty=.true.) )
    call traperr( check_permission(fg_in%ara, allow_empty=.true.) )
    call traperr( check_permission(fg_in%wgt, allow_empty=.true.) )
  enddo  ! iGs/

  call logext()
  !-------------------------------------------------------------
  call logext()
  !-------------------------------------------------------------
  ! Check old output files
  !-------------------------------------------------------------
  selectcase( rt%status )
  case( RT_STATUS__MAKE )
    call logent('Checking old output files', PRCNAM, MODNAM)

    call traperr( set_opt_old_files(opt_sys%old_files) )

    rtm => rt%main

    call traperr( handle_old_file(rtm%f%sidx) )
    call traperr( handle_old_file(rtm%f%tidx) )
    call traperr( handle_old_file(rtm%f%area) )
    call traperr( handle_old_file(rtm%f%coef) )

    do iGs = 1, 2
      call select_rt_vrf(iGs, rt, rtv)

      call traperr( handle_old_file(rtv%f%out_grdidx) )
      call traperr( handle_old_file(rtv%f%out_grdara_true) )
      call traperr( handle_old_file(rtv%f%out_grdara_rt) )
      call traperr( handle_old_file(rtv%f%out_rerr_grdara) )
      call traperr( handle_old_file(rtv%f%out_grdnum) )
      call traperr( handle_old_file(rtv%f%out_iarea_sum) )
      call traperr( handle_old_file(rtv%f%out_iratio_sum) )
    enddo  ! iGs/

    fg_out => t%cmn%f_grid_out
    do iFile = 1, fg_out%nFiles_val
      call traperr( handle_old_file(fg_out%val(iFile)) )
    enddo

    call logext()
  case( RT_STATUS__READ )
    continue
  case( RT_STATUS__UNDEF, &
        RT_STATUS__NONE )
    call errend(msg_unexpected_condition()//&
              '\n  rt%status == RT_STATUS__UNDEF .or. rt%status == RT_STATUS__NONE')
  endselect
  !-------------------------------------------------------------
  ! Prep. output directories and files
  !-------------------------------------------------------------
  call logent('Preparing output directories and files', PRCNAM, MODNAM)

  call traperr( set_opt_mkdir(output=.true., hut=hut_command) )

  call traperr( mkdir(opt_sys%dir_im) )
  call traperr( try_make_empty_file(opt_sys%dir_im) )

  rtm => rt%main

  call traperr( mkdir(dirname(rtm%f%sidx%path)) )
  call traperr( mkdir(dirname(rtm%f%tidx%path)) )
  call traperr( mkdir(dirname(rtm%f%area%path)) )
  call traperr( mkdir(dirname(rtm%f%coef%path)) )

  call traperr( check_permission(rtm%f%sidx, allow_empty=.true.) )
  call traperr( check_permission(rtm%f%tidx, allow_empty=.true.) )
  call traperr( check_permission(rtm%f%area, allow_empty=.true.) )
  call traperr( check_permission(rtm%f%coef, allow_empty=.true.) )

  do iGs = 1, 2
    call select_rt_vrf(iGs, rt, rtv)

    call traperr( mkdir(dirname(rtv%f%out_grdidx%path)) )
    call traperr( mkdir(dirname(rtv%f%out_grdara_true%path)) )
    call traperr( mkdir(dirname(rtv%f%out_grdara_rt%path)) )
    call traperr( mkdir(dirname(rtv%f%out_rerr_grdara%path)) )
    call traperr( mkdir(dirname(rtv%f%out_grdnum%path)) )
    call traperr( mkdir(dirname(rtv%f%out_iarea_sum%path)) )
    call traperr( mkdir(dirname(rtv%f%out_iratio_sum%path)) )

    call traperr( check_permission(rtv%f%out_grdidx     , allow_empty=.true.) )
    call traperr( check_permission(rtv%f%out_grdara_true, allow_empty=.true.) )
    call traperr( check_permission(rtv%f%out_grdara_rt  , allow_empty=.true.) )
    call traperr( check_permission(rtv%f%out_rerr_grdara, allow_empty=.true.) )
    call traperr( check_permission(rtv%f%out_grdnum     , allow_empty=.true.) )
    call traperr( check_permission(rtv%f%out_iarea_sum  , allow_empty=.true.) )
    call traperr( check_permission(rtv%f%out_iratio_sum , allow_empty=.true.) )
  enddo  ! iGs/

  ! Grid values for remapping
  !-------------------------------------------------------------
  fg_in => s%cmn%f_grid_in
  do iFile = 1, fg_in%nFiles_val
    f => fg_in%val(iFile)
    call traperr( check_permission(f, allow_empty=.true.) )
  enddo

  fg_out => t%cmn%f_grid_out
  do iFile = 1, fg_out%nFiles_val
    f => fg_out%val(iFile)
    call traperr( mkdir(dirname(f%path)) )
    call traperr( check_permission(f, allow_empty=.true.) )
  enddo

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
subroutine echo_settings_gs_latlon(al)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_gs_latlon'
  type(gs_latlon_), intent(in), target :: al

  type(file_latlon_in_), pointer :: fl
  type(file_grid_in_), pointer :: fg_in
  integer :: dgt_nxy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( al%is_source )then
    call logmsg(bar(str(str_bgn_sentence(MESH__SOURCE))//' mesh'))
  else
    call logmsg(bar(str(str_bgn_sentence(MESH__TARGET))//' mesh'))
  endif

  fl => al%f_latlon_in
  fg_in => al%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(max(al%nx, al%ny, maxval(fg_in%sz(:2))))

  call logmsg('Name: '//str(al%nam))

  call logmsg('Mesh type: '//str(MESHTYPE__LATLON))

  call logmsg('The number of grids')
  call logmsg('  X: '//str(al%nx,dgt_nxy))
  call logmsg('  Y: '//str(al%ny,dgt_nxy))

  if( fl%lon%path == '' )then
    call logmsg('West : '//str(al%west,'f12.5'))
    call logmsg('East : '//str(al%east,'f12.5'))
  else
    call logmsg('Bounds of longit.: '//str(fl%lon%path))
  endif

  if( fl%lat%path == '' )then
    call logmsg('South: '//str(al%south,'f12.5'))
    call logmsg('North: '//str(al%north,'f12.5'))
  else
    call logmsg('Bounds of latit. : '//str(fl%lat%path))
  endif

  call logmsg('Is south to north: '//str(al%is_south_to_north))

  call logmsg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call logmsg('  Index : '//str(fileinfo(fg_in%idx)))
    if( fg_in%idx%path == '' )then
      call logmsg('    Index starts from '//str(fg_in%idx_bgn))
    endif
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
    if( fg_in%idx%path == '' )then
      call logmsg('    Index starts from '//str(fg_in%idx_bgn))
    endif
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(al%idx_miss))
  call logmsg('  Area  : '//str(al%ara_miss))
  call logmsg('  Weight: '//str(al%wgt_miss))
  call logmsg('  XYZ   : '//str(al%xyz_miss))
  call logmsg('  LatLon: '//str(al%lonlat_miss))
  call logmsg('  Value : '//str(al%val_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_gs_latlon
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_raster(ar)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_gs_raster'
  type(gs_raster_), intent(in), target :: ar

  type(file_raster_in_), pointer :: fr
  type(file_grid_in_), pointer :: fg_in
  integer :: dgt_nxy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ar%is_source )then
    call logmsg(bar(str(str_bgn_sentence(MESH__SOURCE))//' mesh'))
  else
    call logmsg(bar(str(str_bgn_sentence(MESH__TARGET))//' mesh'))
  endif

  fr => ar%f_raster_in
  fg_in => ar%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_nxy = dgt(maxval(fr%sz(:2)))

  call logmsg('Name: '//str(ar%nam))

  call logmsg('Mesh type: '//str(MESHTYPE__RASTER))

  call logmsg('The number of grids')
  call logmsg('  x: '//str(ar%nx,dgt_nxy)//' ('//str((/ar%xi,ar%xf/),dgt_nxy,' - ')//')')
  call logmsg('  y: '//str(ar%ny,dgt_nxy)//' ('//str((/ar%yi,ar%yf/),dgt_nxy,' - ')//')')

  call logmsg('West : '//str(ar%west,'f12.5'))
  call logmsg('East : '//str(ar%east,'f12.5'))
  call logmsg('South: '//str(ar%south,'f12.5'))
  call logmsg('North: '//str(ar%north,'f12.5'))

  call logmsg('Is south to north: '//str(ar%is_south_to_north))

  call logmsg('Raster data (in)')
  call logmsg('  Index : '//str(fileinfo(fr%idx)))
  call logmsg('  Area  : '//str(fileinfo(fr%ara)))
  call logmsg('  Weight: '//str(fileinfo(fr%wgt)))
  call logmsg('  Size : ('//str(fr%sz(:2),dgt_nxy,', ')//')')
  call logmsg('  Input: ('//str((/fr%lb(1),fr%ub(1)/),dgt_nxy,':')//&
                    ', '//str((/fr%lb(2),fr%ub(2)/),dgt_nxy,':')//')')

  call logmsg('Cell data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call logmsg('  Index : '//str(fileinfo(fg_in%idx)))
    call logmsg('  Area  : '//str(fileinfo(fg_in%ara)))
    call logmsg('  Weight: '//str(fileinfo(fg_in%wgt)))
    call logmsg('  Size : ('//str(fg_in%sz(:2),dgt_nxy,', ')//')')
    call logmsg('  Input: ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_nxy,':')//&
                      ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_nxy,':')//')')
    if( fg_in%idx%path /= '' )then
      call logmsg('  Condition for index: '//str(ar%idx_condition))
    endif
    if( fg_in%ara%path /= '' )then
      call logmsg('  Unit of Area: '//str(fg_in%unit_ara))
    endif
  else
    call logmsg('  (No input)')
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(ar%idx_miss))
  call logmsg('  Area  : '//str(ar%ara_miss))
  call logmsg('  Weight: '//str(ar%wgt_miss))
  call logmsg('  xyz   : '//str(ar%xyz_miss))
  call logmsg('  LatLon: '//str(ar%lonlat_miss))
  call logmsg('  Field : '//str(ar%val_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_gs_raster
!===============================================================
!
!===============================================================
subroutine echo_settings_gs_polygon(ap)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_gs_polygon'
  type(gs_polygon_), intent(in), target :: ap

  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_)   , pointer :: fg_in
  integer :: dgt_ij

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( ap%is_source )then
    call logmsg(bar(str(str_bgn_sentence(MESH__SOURCE))//' mesh'))
  else
    call logmsg(bar(str(str_bgn_sentence(MESH__TARGET))//' mesh'))
  endif

  fp => ap%f_polygon_in
  fg_in => ap%f_grid_in
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_ij = dgt(maxval(fp%sz(:2)))

  call logmsg('Name: '//str(ap%nam))

  call logmsg('Mesh type: '//str(MESHTYPE__POLYGON))

  call logmsg('Grid data')
  call logmsg('  Size : '//str(fp%sz(2),dgt_ij))
  call logmsg('  Input: ('//str((/fp%lb(2),fp%ub(2)/),dgt_ij,':')//')')

  call logmsg('Max. num. of vertices of a grid: '//str(ap%np))

  call logmsg('Coordinates')
  call logmsg('  Coordinate system: '//str(ap%coord_sys))
  call logmsg('  Files of coords. of vertices')
  selectcase( ap%coord_sys )
  case( COORD_SYS_SPHERICAL )
    call logmsg('    Longit.: '//str(fileinfo(fp%lon)))
    call logmsg('    Latit. : '//str(fileinfo(fp%lat)))
  case( COORD_SYS_CARTESIAN )
    call logmsg('    X: '//str(fileinfo(fp%x)))
    call logmsg('    Y: '//str(fileinfo(fp%y)))
    call logmsg('    Z: '//str(fileinfo(fp%z)))
  case default
    call errend(msg_invalid_value('ap%coord_sys', ap%coord_sys))
  endselect

  call logmsg('  Unit: '//str(ap%coord_unit))

  call logmsg('  Missing value of coords.')
  call logmsg('    Spherical: '//str(ap%coord_miss_s))
  call logmsg('    Cartesian: '//str(ap%coord_miss_c))


  if( fp%arctyp%path == '' )then
    call logmsg('Treat the arcs whose edges have same lattitude as small arcs: '//&
              str(ap%arc_parallel))
  else
    call logmsg('Types of the arcs: '//str(fileinfo(fp%arctyp)))
  endif

  call logmsg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call logmsg('  Index : '//str(fileinfo(fg_in%idx)))
    if( fg_in%idx%path == '' )then
      call logmsg('    Index starts from '//str(fg_in%idx_bgn))
    endif
    call logmsg('  Area  : '//str(fileinfo(fg_in%ara)))
    call logmsg('  Weight: '//str(fileinfo(fg_in%wgt)))
    if( fg_in%ara%path /= '' )then
      call logmsg('  Unit of area: '//str(fg_in%unit_ara))
    endif
  else
    call logmsg('  (No input)')
    if( fg_in%idx%path == '' )then
      call logmsg('    Index starts from '//str(fg_in%idx_bgn))
    endif
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(ap%idx_miss))
  call logmsg('  Area  : '//str(ap%ara_miss))
  call logmsg('  Weight: '//str(ap%wgt_miss))
  call logmsg('  XYZ   : '//str(ap%xyz_miss))
  call logmsg('  LatLon: '//str(ap%lonlat_miss))
  call logmsg('  Field : '//str(ap%val_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine echo_settings_remapping(rt, s, t)
  use c1_set, only: &
        bar
  use c2_rt_set, only: &
        KEY_OPT_COEF_SUM_MODIFY      , &
        KEY_OPT_COEF_SUM_MODIFY_ULIM , &
        KEY_OPT_COEF_ZERO_POSITIVE   , &
        KEY_OPT_COEF_ZERO_NEGATIVE   , &
        KEY_OPT_COEF_ERROR_EXCESS    , &
        KEY_OPT_COEF_SUM_ERROR_EXCESS
  use c2_rt_set, only: &
        echo_settings_opt_rt_coef
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_remapping'
  type(rt_), intent(in), target :: rt
  type(gs_), intent(in), target :: s, t

  type(rt_main_), pointer :: rtm
  type(rt_vrf_) , pointer :: rtv
  type(file_rt_main_) , pointer :: f_main
  type(file_grid_in_) , pointer :: sfg
  type(file_grid_out_), pointer :: tfg
  type(file_)         , pointer :: sf, tf

  integer :: iGs
  type(gs_), pointer :: a
  character(CLEN_KEY) :: mesh
  integer :: iFile

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(bar('Remapping Table'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => rt%main

  call logmsg('Mode: '//str(rtm%mode))
  !-------------------------------------------------------------
  ! Main product
  !-------------------------------------------------------------
  f_main => rtm%f

  call logent('Main product', PRCNAM, MODNAM)

  selectcase( rt%status )
  case( RT_STATUS__READ )
    call logmsg('sidx: '//str(fileinfo(f_main%sidx)))
    call logmsg('tidx: '//str(fileinfo(f_main%tidx)))
    call logmsg('area: '//str(fileinfo(f_main%area)))
    call logmsg('coef: '//str(fileinfo(f_main%coef)))
  case( RT_STATUS__MAKE )
    call logmsg('sidx: '//str(fileinfo(f_main%sidx)))
    call logmsg('tidx: '//str(fileinfo(f_main%tidx)))
    call logmsg('area: '//str(fileinfo(f_main%area)))
    call logmsg('coef: '//str(fileinfo(f_main%coef)))

    call logmsg('Allow empty: '//str(rtm%allow_empty))

    call logmsg('Mesh to calc coef.: '//str(rtm%mesh_coef))
    call logmsg('Mesh to sort by index: '//str(rtm%mesh_sort))

    call echo_settings_opt_rt_coef(rtm%opt_coef, 0)
  case default
    call errend(msg_invalid_value('rt%status', rt%status))
  endselect

  call logext()
  !-------------------------------------------------------------
  ! Verification data
  !-------------------------------------------------------------
  call logent('Verification data', PRCNAM, MODNAM)

  do iGs = 1, 2
    call select_gs(iGs, s, t, a, mesh)
    call select_rt_vrf(iGs, rt, rtv)

    call logent('For '//str(a%nam)//' ('//str(mesh)//')', PRCNAM, MODNAM)

    selectcase( a%typ )
    case( MESHTYPE__LATLON, &
          MESHTYPE__POLYGON )
      call logmsg('(out) grdidx     : '//str(fileinfo(rtv%f%out_grdidx)))
      call logmsg('(out) grdara_true: '//str(fileinfo(rtv%f%out_grdara_true)))
      call logmsg('(out) grdara_rt  : '//str(fileinfo(rtv%f%out_grdara_rt)))
      call logmsg('(out) rerr_grdara: '//str(fileinfo(rtv%f%out_rerr_grdara)))
      call logmsg('(out) grdnum     : '//str(fileinfo(rtv%f%out_grdnum)))
    case( MESHTYPE__RASTER )
      call logmsg('(out) grdidx     : '//str(fileinfo(rtv%f%out_grdidx)))
      call logmsg('(out) grdara_true: '//str(fileinfo(rtv%f%out_grdara_true)))
      call logmsg('(out) grdara_rt  : '//str(fileinfo(rtv%f%out_grdara_rt)))
      call logmsg('(out) rerr_grdara: '//str(fileinfo(rtv%f%out_rerr_grdara)))
      call logmsg('(out) grdnum     : '//str(fileinfo(rtv%f%out_grdnum)))
      call logmsg('(out) iarea_sum  : '//str(fileinfo(rtv%f%out_iarea_sum)))
      call logmsg('(out) iratio_sum : '//str(fileinfo(rtv%f%out_iratio_sum)))
    endselect

    call logmsg('Missing values')
    call logmsg('  idx        : '//str(rtv%idx_miss))
    call logmsg('  val (float): '//str(rtv%dval_miss))
    call logmsg('  val (int)  : '//str(rtv%ival_miss))

    call logext()
  enddo  ! iGs/

  call logext()
  !-------------------------------------------------------------
  ! Remapping data
  !-------------------------------------------------------------
  call logent('Remapping Data', PRCNAM, MODNAM)

  sfg => s%cmn%f_grid_in
  tfg => t%cmn%f_grid_out

  do iFile = 1, sfg%nFiles_val
    sf => sfg%val(iFile)
    tf => tfg%val(iFile)

    call logmsg('File '//str(iFile,dgt(sfg%nFiles_val))//' / '//str(sfg%nFiles_val))
    call logmsg('  In : '//str(fileinfo(sf)))
    call logmsg('  Out: '//str(fileinfo(tf)))
  enddo  ! iFile/

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_remapping
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
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine select_gs(iGs, s, t, a, mesh)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'select_gs'
  integer, intent(in) :: iGs
  type(gs_)   , intent(in), target :: s, t
  type(gs_)   , pointer            :: a
  character(*), intent(out), optional :: mesh

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( iGs )
  case( 1 )
    a => s
    if( present(mesh) ) mesh = MESH__SOURCE
  case( 2 )
    a => t
    if( present(mesh) ) mesh = MESH__TARGET
  case default
    call errend(msg_invalid_value('iGs', iGs))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine select_gs
!===============================================================
!
!===============================================================
subroutine select_rt_vrf(iGs, rt, rtv)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'select_rt_vrf'
  integer      , intent(in)         :: iGs
  type(rt_)    , intent(in), target :: rt
  type(rt_vrf_), pointer            :: rtv

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( iGs )
  case( 1 )
    rtv => rt%vrf_src
  case( 2 )
    rtv => rt%vrf_tgt
  case default
    call errend(msg_invalid_value('iGs', iGs))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine select_rt_vrf
!===============================================================
!
!===============================================================
end module mod_set
