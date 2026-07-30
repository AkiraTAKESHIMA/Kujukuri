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
subroutine read_settings(a)
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
  use c1_opt_ctrl, only: &
        set_opt_sys  , &
        set_opt_log  , &
        set_opt_earth
  use c1_opt_set, only: &
        set_default_values_opt_sys  , &
        set_default_values_opt_log  , &
        set_default_values_opt_earth
  use c1_file, only: &
        open_report_file
  use c1_gs_base, only: &
        init_mesh             , &
        set_miss_file_grid_in , &
        set_miss_file_grid_out, &
        set_save_file_grid_out
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings'
  type(gs_) , intent(out), target :: a

  type counter_
    integer :: gs
    integer :: opt
  end type
  type(counter_) :: counter

  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_LATLON  = 'mesh_latlon'
  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_RASTER  = 'mesh_raster'
  character(CLEN_VAR), parameter :: BLOCKNAME__MESH_POLYGON = 'mesh_polygon'
  character(CLEN_VAR), parameter :: BLOCKNAME__OPT          = 'options'

  character(CLEN_VAR) :: blockName
  !-------------------------------------------------------------
  type(gs_common_), pointer :: ac
  type(opt_) :: opt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call logent('Initializing', PRCNAM, MODNAM)

  call traperr( init_mesh(a) )
  a%id = 'a'
  a%nam = 'mesh'

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
    !-------------------------------------------------------------
    ! Case: No more block
    case( '' )
      exit
    !-------------------------------------------------------------
    ! Case: gs_latlon
    case( BLOCKNAME__MESH_LATLON )
      call update_counter(counter%gs, blockName)
      call read_settings_gs_latlon(a)
    !-------------------------------------------------------------
    ! Case: gs_raster
    case( BLOCKNAME__MESH_RASTER )
      call update_counter(counter%gs, blockName)
      call read_settings_gs_raster(a)
    !-------------------------------------------------------------
    ! Case: gs_polygon
    case( BLOCKNAME__MESH_POLYGON )
      call update_counter(counter%gs, blockName)
      call read_settings_gs_polygon(a)
    !-------------------------------------------------------------
    ! Case: opt
    case( BLOCKNAME__OPT )
      call update_counter(counter%opt, blockName)
      call read_settings_opt(opt)
    !-------------------------------------------------------------
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
  call check_input(a, opt%earth)
  !-------------------------------------------------------------
  ! Set the values
  !-------------------------------------------------------------
  call logent('Setting the values', PRCNAM, MODNAM)

  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(get_path_report())
    call logmsg('Directory of intermediates was not given.'//&
            '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

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

  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)
  call set_opt_earth(opt%earth)

  call logext()
  !-------------------------------------------------------------
  ! Print the settings
  !-------------------------------------------------------------
  call logent('Printing the settings', PRCNAM, MODNAM, '-p -x2')

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

  call echo_settings_opt(opt)

  call logmsg(str(bar('')))

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(a, opt%sys)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_counter'

  counter%gs = 0
  counter%opt = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n, blockName)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_counter'
  integer, intent(inout) :: n
  character(*), intent(in) :: blockName

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  selectcase( blockName )
  case( BLOCKNAME__MESH_LATLON, &
        BLOCKNAME__MESH_RASTER, &
        BLOCKNAME__MESH_POLYGON )
    if( n > 1 )then
      call errend(msg_invalid_input(line_number())//&
                '\nBlocks of mesh appeared more than once.')
    endif
  case( BLOCKNAME__OPT )
    call check_num_of_key(n, blockName, 0, 1)
  case default
    call errend(msg_invalid_value('blockName', blockName))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_number_of_blocks'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( counter%gs /= 1 )then
    call errend(msg_invalid_input()//&
              '\nBlocks of mesh appeared more than once.')
  endif

  call check_num_of_key(counter%opt, BLOCKNAME__OPT, 0, 1)
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
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call logent('Setting the limits. of the number of each keyword', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('name', 0, 1)
  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west', 0, 1)
  call set_keynum('east', 0, 1)
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
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)
  call set_keynum('out_form', 0, 1)
  call set_keynum('fout_grdmsk', 0, 1)
  call set_keynum('fout_grdidx', 0, 1)
  call set_keynum('fout_grdara', 0, 1)
  call set_keynum('fout_grdwgt', 0, 1)
  call set_keynum('fout_grdx', 0, 1)
  call set_keynum('fout_grdy', 0, 1)
  call set_keynum('fout_grdz', 0, 1)
  call set_keynum('fout_grdlon', 0, 1)
  call set_keynum('fout_grdlat', 0, 1)
  call set_keynum('out_grid_sz', 0, 1)
  call set_keynum('out_grid_lb', 0, 1)
  call set_keynum('out_grid_ub', 0, 1)
  call set_keynum('out_unit_ara', 0, 1)
  call set_keynum('out_unit_xyz', 0, 1)
  call set_keynum('out_unit_lonlat', 0, 1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)
  call set_keynum('xyz_miss', 0, 1)
  call set_keynum('lonlat_miss', 0, 1)

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
      call traperr( remove_quotes(a%nam, QUOTE_BOTH) )
    !-------------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(al%nx)
    case( 'ny' )
      call read_value(al%ny)
    !-------------------------------------------------------------
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
      call read_value(fl%lon)
      fl%lon%path = joined(dir, fl%lon%path)
    case( 'f_lat_bound' )
      call read_value(fl%lat)
      fl%lat%path = joined(dir, fl%lat%path)

    case( 'coord_unit' )
      call read_value(al%coord_unit, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data (in)
    case( 'idx_bgn' )
      call read_value(fg_in%idx_bgn)
    case( 'fin_grdidx' )
      call read_value(fg_in%idx)
      fg_in%idx%path = joined(dir, fg_in%idx%path)
    case( 'fin_grdara' )
      call read_value(fg_in%ara)
      fg_in%ara%path = joined(dir, fg_in%ara%path)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

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
    ! Grid data (out)
    !-----------------------------------------------------------
    case( 'out_form' )
      call read_value(fg_out%form, is_keyword=.true.)

    case( 'fout_grdmsk' )
      call read_value(fg_out%msk)
      fg_out%msk%path = joined(dir, fg_out%msk%path)
    case( 'fout_grdidx' )
      call read_value(fg_out%idx)
      fg_out%idx%path = joined(dir, fg_out%idx%path)
    case( 'fout_grdara' )
      call read_value(fg_out%ara)
      fg_out%ara%path = joined(dir, fg_out%ara%path)
    case( 'fout_grdwgt' )
      call read_value(fg_out%wgt)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)
    case( 'fout_grdx' )
      call read_value(fg_out%x)
      fg_out%x%path = joined(dir, fg_out%x%path)
    case( 'fout_grdy' )
      call read_value(fg_out%y)
      fg_out%y%path = joined(dir, fg_out%y%path)
    case( 'fout_grdz' )
      call read_value(fg_out%z)
      fg_out%z%path = joined(dir, fg_out%z%path)
    case( 'fout_grdlon' )
      call read_value(fg_out%lon)
      fg_out%lon%path = joined(dir, fg_out%lon%path)
    case( 'fout_grdlat' )
      call read_value(fg_out%lat)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( 'out_grid_sz' )
      call read_value(fg_out%sz(1), pos=1)
      call read_value(fg_out%sz(2), pos=2)
    case( 'out_grid_lb' )
      call read_value(fg_out%lb(1), pos=1)
      call read_value(fg_out%lb(2), pos=2)
    case( 'out_grid_ub' )
      call read_value(fg_out%ub(1), pos=1)
      call read_value(fg_out%ub(2), pos=2)

    case( 'out_unit_ara' )
      call read_value(fg_out%unit_ara, is_keyword=.true.)
    case( 'out_unit_xyz' )
      call read_value(fg_out%unit_xyz, is_keyword=.true.)
    case( 'out_unit_lonlat' )
      call read_value(fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(al%idx_miss)
    case( 'ara_miss' )
      call read_value(al%ara_miss)
    case( 'wgt_miss' )
      call read_value(al%wgt_miss)
    case( 'xyz_miss' )
      call read_value(al%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(al%lonlat_miss)
    !-----------------------------------------------------------
    ! Error
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Check the number of inputs for each keyword
  !-------------------------------------------------------------
  call logent('Checking the number of inputs for each keyword', PRCNAM, MODNAM)

  call check_keynum()
  call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Check the values
  !-------------------------------------------------------------
  call logent('Check the values', PRCNAM, MODNAM)

  if( keynum('west' ) == 1 )then
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
         fg_out, fg_in%sz(1), fg_in%sz(2)) )

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
              '\n"idx_bgn" is given but "fin_grdidx" is also given.'//&
                ' The input given by "idx_bgn" is ignored.')
  endif

  if( keynum('fin_grdidx') == 0 .and. &
      keynum('fin_grdara') == 0 .and. &
      keynum('fin_grdwgt') == 0 .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call errend(msg_invalid_input()//&
              '\nThere are inputs for the following keys:'//&
              '\n  "in_grid_sz", "in_grid_lb", "in_grid_ub"'//&
              '\nbut no input for:'//&
              '\n  "in_grdidx", "in_grdara", "in_grdwgt".'//&
              '\nThe former inputs are ignored.')
  endif

  selectcase( fg_out%form )
  case( GRID_FORM_AUTO )
    continue
  case( GRID_FORM_INDEX )
    if( keynum('fin_grdidx') == 0 )then
      call errend(msg_invalid_input()//&
                '\n  "fin_grdidx" must be given when '//&
                  'the value of "out_form" is "'//str(GRID_FORM_INDEX)//'".')
    endif
  case default
    call errend(msg_invalid_value('fg_out%form', fg_out%form)//&
              '\nCheck the value of "out_form".')
  endselect

  if( keynum('fout_grdidx') == 0 .and. &
      keynum('fout_grdara') == 0 .and. &
      keynum('fout_grdwgt') == 0 .and. &
      keynum('fout_grdx') == 0 .and. &
      keynum('fout_grdy') == 0 .and. &
      keynum('fout_grdz') == 0 .and. &
      keynum('fout_grdlon') == 0 .and. &
      keynum('fout_grdlat') == 0 .and. &
      (keynum('out_grid_sz') == 1 .or. &
       keynum('out_grid_lb') == 1 .or. &
       keynum('out_grid_ub') == 1) )then
    call errend(msg_invalid_input()//&
              '\nThere are inputs for any of the following keys:'//&
              '\n  "in_grid_sz", "in_grid_lb", "in_grid_ub"'//&
              '\nbut no input for:'//&
              '\n  "in_grdidx", "in_grdara", "in_grdwgt".'//&
              '\nThe former inputs are ignored.')
  endif

  if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\nThe value for "idx_miss" is given although'//&
                ' that for "fin_grdidx" is not given.'//&
                ' The input for "idx_miss" is ignored.')
  endif

  if( keynum('fout_grdara') == 0 .and. keynum('ara_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\nThe value for "ara_miss" is given although'//&
                ' that for "fout_grdara" is not given.'//&
                ' The input for "ara_miss" is ignored.')
  endif

  if( keynum('fout_grdwgt') == 0 .and. keynum('wgt_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\nThe value for "wgt_miss" is given although'//&
                ' that for "fout_grdwgt" is not given.'//&
                ' The input for "wgt_miss" is ignored.')
  endif

  if( (keynum('fout_grdx') == 0 .and. keynum('fout_grdy') == 0 .and. &
       keynum('fout_grdz') == 0) .and. &
      keynum('xyz_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\nThe value for "xyz_miss" is given although that for "fout_grdx", '//&
                '"fout_grdy" or "fout_grdz" is not given.'//&
                ' The input for "xyz_miss" is ignored.')
  endif

  if( (keynum('fout_grdlon') == 0 .and. keynum('fout_grdlat') == 0) .and. &
      keynum('lonlat_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\nThe value for "lonlat_miss" is given although that for "fout_grdlon" or '//&
                '"fout_grdlat" is not given.'//&
                ' The input for "lonlat_miss" is ignored.')
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
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call logent('Setting the limits. of the number of each keyword', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('name',0,1)
  call set_keynum('nx', 1, 1)
  call set_keynum('ny', 1, 1)
  call set_keynum('west' , 1, 1)
  call set_keynum('east' , 1, 1)
  call set_keynum('south', 1, 1)
  call set_keynum('north', 1, 1)
  call set_keynum('is_south_to_north', 0, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('fin_rstidx', 1, 1)
  call set_keynum('fin_rstara', 0, 1)
  call set_keynum('in_raster_sz', 0, 1)
  call set_keynum('in_raster_lb', 0, 1)
  call set_keynum('in_raster_ub', 0, 1)
  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)
  call set_keynum('idx_condition', 0, 1)
  call set_keynum('out_form', 1, 1)
  call set_keynum('fout_grdmsk', 0, 1)
  call set_keynum('fout_grdidx', 0, 1)
  call set_keynum('fout_grdara', 0, 1)
  call set_keynum('fout_grdwgt', 0, 1)
  call set_keynum('fout_grdx'  , 0, 1)
  call set_keynum('fout_grdy'  , 0, 1)
  call set_keynum('fout_grdz'  , 0, 1)
  call set_keynum('fout_grdlon', 0, 1)
  call set_keynum('fout_grdlat', 0, 1)
  call set_keynum('out_grid_sz', 0, 1)
  call set_keynum('out_grid_lb', 0, 1)
  call set_keynum('out_grid_ub', 0, 1)
  call set_keynum('out_unit_ara'   , 0, 1)
  call set_keynum('out_unit_xyz'   , 0, 1)
  call set_keynum('out_unit_lonlat', 0, 1)
  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('xyz_miss'   , 0, 1)
  call set_keynum('lonlat_miss', 0, 1)

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
      call traperr( remove_quotes(a%nam, QUOTE_BOTH) )
    !-----------------------------------------------------------
    ! Resolution
    case( 'nx' )
      call read_value(ar%nx)
    case( 'ny' )
      call read_value(ar%ny)
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
      call read_value(fr%idx)
      fr%idx%path = joined(dir, fr%idx%path)
    case( 'fin_rstara' )
      call read_value(fr%ara)
      fr%ara%path = joined(dir, fr%ara%path)

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
    ! Grid data (in)
    case( 'fin_grdidx' )
      call read_value(fg_in%idx)
      fg_in%idx%path = joined(dir, fg_in%idx%path)
    case( 'fin_grdara' )
      call read_value(fg_in%ara)
      fg_in%ara%path = joined(dir, fg_in%ara%path)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

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

    case( 'idx_condition' )
      call read_value(ar%idx_condition, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Grid data (out)
    case( 'out_form' )
      call read_value(fg_out%form, is_keyword=.true.)

    case( 'fout_grdmsk' )
      call read_value(fg_out%msk)
      fg_out%msk%path = joined(dir, fg_out%msk%path)
    case( 'fout_grdidx' )
      call read_value(fg_out%idx)
      fg_out%idx%path = joined(dir, fg_out%idx%path)
    case( 'fout_grdara' )
      call read_value(fg_out%ara)
      fg_out%ara%path = joined(dir, fg_out%ara%path)
    case( 'fout_grdwgt' )
      call read_value(fg_out%wgt)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)
    case( 'fout_grdx' )
      call read_value(fg_out%x)
      fg_out%x%path = joined(dir, fg_out%x%path)
    case( 'fout_grdy' )
      call read_value(fg_out%y)
      fg_out%y%path = joined(dir, fg_out%y%path)
    case( 'fout_grdz' )
      call read_value(fg_out%z)
      fg_out%z%path = joined(dir, fg_out%z%path)
    case( 'fout_grdlon' )
      call read_value(fg_out%lon)
      fg_out%lon%path = joined(dir, fg_out%lon%path)
    case( 'fout_grdlat' )
      call read_value(fg_out%lat)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( 'out_grid_sz' )
      call read_value(fg_out%sz(1), pos=1)
      call read_value(fg_out%sz(2), pos=2)
    case( 'out_grid_lb' )
      call read_value(fg_out%lb(1), pos=1)
      call read_value(fg_out%lb(2), pos=2)
    case( 'out_grid_ub' )
      call read_value(fg_out%ub(1), pos=1)
      call read_value(fg_out%ub(2), pos=2)

    case( 'out_unit_ara' )
      call read_value(fg_out%unit_ara, is_keyword=.true.)
    case( 'out_unit_xyz' )
      call read_value(fg_out%unit_xyz, is_keyword=.true.)
    case( 'out_unit_lonlat' )
      call read_value(fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(ar%idx_miss)
    case( 'ara_miss' )
      call read_value(ar%ara_miss)
    case( 'wgt_miss' )
      call read_value(ar%wgt_miss)
    case( 'xyz_miss' )
      call read_value(ar%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(ar%lonlat_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Check the number of each keyword
  !-------------------------------------------------------------
  call logent('Checking the number of each keyword', PRCNAM, MODNAM)

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
         fg_out, fg_in%sz(1), fg_in%sz(2)) )

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
  !
  !-------------------------------------------------------------
  if( keynum('fin_grdidx') == 0 )then
    if( keynum('fin_grdara') == 1 .or. keynum('fin_grdwgt') == 1 )then
      call errend(msg_invalid_input()//&
                '\n"fin_grdara" or "fin_grdwgt"'//&
                  ' cannot be given when "fin_grdidx" is not given.')
    endif

    if( keynum('in_grid_sz') == 1 .or. &
        keynum('in_grid_lb') == 1 .or. &
        keynum('in_grid_ub') == 1 )then
      call errend(msg_invalid_input()//&
                '\n"in_grid_sz", "in_grid_lb" or "in_grid_ub"'//&
                  '" cannot be given when "fin_grdidx" is not given.')
    endif
  endif

  if( keynum('fin_grdidx') == 1 .and. keynum('in_grid_sz') == 0 )then
    call errend(msg_invalid_input()//&
              '\n"in_grid_sz" must be given when "fin_grdidx" is given.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call errend(msg_invalid_input()//&
              '\n"fin_grdara" and "fin_grdwgt" cannot be given at the same time.')
  endif

  selectcase( fg_out%form )
  case( GRID_FORM_AUTO )
    continue
  case( GRID_FORM_INDEX )
    if( keynum('fin_grdidx') == 0 )then
      call errend(msg_invalid_input()//&
                '\n"fin_grdidx" must be given when '//&
                  'the value of "out_form" is "'//str(GRID_FORM_INDEX)//'".')
    endif
  case default
    call errend(msg_invalid_value('fg_out%form', fg_out%form))
  endselect
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
        keynum                 , &
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
  call set_keynum('f_x_vertex', 0, 1)
  call set_keynum('f_y_vertex', 0, 1)
  call set_keynum('f_z_vertex', 0, 1)
  call set_keynum('coord_unit', 0, 1)
  call set_keynum('coord_miss', 0, 1)
  call set_keynum('f_arctyp', 0, 1)
  call set_keynum('arc_parallel', 0, 1)
  call set_keynum('fin_grdidx', 0, 1)
  call set_keynum('fin_grdara', 0, 1)
  call set_keynum('fin_grdwgt', 0, 1)
  call set_keynum('in_grid_sz', 0, 1)
  call set_keynum('in_grid_lb', 0, 1)
  call set_keynum('in_grid_ub', 0, 1)
  call set_keynum('in_unit_ara', 0, 1)
  call set_keynum('out_form', 1, 1)
  call set_keynum('fout_grdidx', 0, 1)
  call set_keynum('fout_grdmsk', 0, 1)
  call set_keynum('fout_grdara', 0, 1)
  call set_keynum('fout_grdwgt', 0, 1)
  call set_keynum('fout_grdx'  , 0, 1)
  call set_keynum('fout_grdy'  , 0, 1)
  call set_keynum('fout_grdz'  , 0, 1)
  call set_keynum('fout_grdlon', 0, 1)
  call set_keynum('fout_grdlat', 0, 1)
  call set_keynum('out_grid_sz', 0, 1)
  call set_keynum('out_grid_lb', 0, 1)
  call set_keynum('out_grid_ub', 0, 1)
  call set_keynum('out_unit_ara'   , 0, 1)
  call set_keynum('out_unit_xyz'   , 0, 1)
  call set_keynum('out_unit_lonlat', 0, 1)
  call set_keynum('idx_miss'   , 0, 1)
  call set_keynum('ara_miss'   , 0, 1)
  call set_keynum('wgt_miss'   , 0, 1)
  call set_keynum('xyz_miss'   , 0, 1)
  call set_keynum('lonlat_miss', 0, 1)

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
      call traperr( remove_quotes(a%nam, QUOTE_BOTH) )
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
      call read_value(fp%lon)
      fp%lon%path = joined(dir, fp%lon%path)
    case( 'f_lat_vertex' )
      call read_value(fp%lat)
      fp%lat%path = joined(dir, fp%lat%path)
    case( 'f_x_vertex' )
      call read_value(fp%x)
      fp%x%path = joined(dir, fp%x%path)
    case( 'f_y_vertex' )
      call read_value(fp%y)
      fp%y%path = joined(dir, fp%y%path)
    case( 'f_z_vertex' )
      call read_value(fp%z)
      fp%z%path = joined(dir, fp%z%path)

    case( 'coord_unit' )
      call read_value(ap%coord_unit, is_keyword=.true.)
    case( 'coord_miss' )
      call read_value(coord_miss)
    !-----------------------------------------------------------
    ! Arc type
    case( 'f_arctyp' )
      call read_value(fp%arctyp)
      fp%arctyp%path = joined(dir, fp%arctyp%path)
    case( 'arc_parallel' )
      call read_value(ap%arc_parallel)
    !-----------------------------------------------------------
    ! Grid data (in)
    case( 'fin_grdidx' )
      call read_value(fg_in%idx)
      fg_in%idx%path = joined(dir, fg_in%idx%path)
    case( 'fin_grdara' )
      call read_value(fg_in%ara)
      fg_in%ara%path = joined(dir, fg_in%ara%path)
    case( 'fin_grdwgt' )
      call read_value(fg_in%wgt)
      fg_in%wgt%path = joined(dir, fg_in%wgt%path)

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
    ! Grid data (out)
    case( 'out_form' )
      call read_value(fg_out%form, is_keyword=.true.)

    case( 'fout_grdmsk' )
      call read_value(fg_out%msk)
      fg_out%msk%path = joined(dir, fg_out%msk%path)
    case( 'fout_grdidx' )
      call read_value(fg_out%idx)
      fg_out%idx%path = joined(dir, fg_out%idx%path)
    case( 'fout_grdara' )
      call read_value(fg_out%ara)
      fg_out%ara%path = joined(dir, fg_out%ara%path)
    case( 'fout_grdwgt' )
      call read_value(fg_out%wgt)
      fg_out%wgt%path = joined(dir, fg_out%wgt%path)
    case( 'fout_grdx' )
      call read_value(fg_out%x)
      fg_out%x%path = joined(dir, fg_out%x%path)
    case( 'fout_grdy' )
      call read_value(fg_out%y)
      fg_out%y%path = joined(dir, fg_out%y%path)
    case( 'fout_grdz' )
      call read_value(fg_out%z)
      fg_out%z%path = joined(dir, fg_out%z%path)
    case( 'fout_grdlon' )
      call read_value(fg_out%lon)
      fg_out%lon%path = joined(dir, fg_out%lon%path)
    case( 'fout_grdlat' )
      call read_value(fg_out%lat)
      fg_out%lat%path = joined(dir, fg_out%lat%path)

    case( 'out_grid_sz' )
      call read_value(fg_out%sz(1), pos=1)
      !call read_value(fg_out%sz(2), pos=2)
    case( 'out_grid_lb' )
      call read_value(fg_out%lb(1), pos=1)
      !call read_value(fg_out%lb(2), pos=2)
    case( 'out_grid_ub' )
      call read_value(fg_out%ub(1), pos=1)
      !call read_value(fg_out%ub(2), pos=2)

    case( 'out_unit_ara' )
      call read_value(fg_out%unit_ara, is_keyword=.true.)
    case( 'out_unit_xyz' )
      call read_value(fg_out%unit_xyz, is_keyword=.true.)
    case( 'out_unit_lonlat' )
      call read_value(fg_out%unit_lonlat, is_keyword=.true.)
    !-----------------------------------------------------------
    ! Missing value
    case( 'idx_miss' )
      call read_value(ap%idx_miss)
    case( 'ara_miss' )
      call read_value(ap%ara_miss)
    case( 'wgt_miss' )
      call read_value(ap%wgt_miss)
    case( 'xyz_miss' )
      call read_value(ap%xyz_miss)
    case( 'lonlat_miss' )
      call read_value(ap%lonlat_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Check the number of each keyword
  !-------------------------------------------------------------
  call logent('Checking the number of each keyword', PRCNAM, MODNAM)

  call check_keynum()
  call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  call traperr( set_bounds_file_polygon_in(fp, ap%ijs, ap%ije, ap%np, ap%nij) )
  call traperr( set_bounds_file_grid_in(fg_in, ap%nij, 1_8) )
  call traperr( set_bounds_file_grid_out(fg_out, ap%nij, 1_8) )

  ! Coords.
  !-------------------------------------------------------------
  if( fp%lon%path /= '' )then
    ap%coord_sys = COORD_SYS_SPHERICAL

    if( keynum('coord_unit') == 0 )then
      ap%coord_unit = UNIT_DEGREE
    else
      if( ap%coord_unit /= UNIT_DEGREE .and. &
          ap%coord_unit /= UNIT_RADIAN )then
        call errend(msg_invalid_input()//&
                  '\n  ap%coord_unit: '//str(ap%coord_unit)//&
                  '\nThis value is invalid when "f_lon_vertex"'//&
                    ' is given. Check the value of "coord_unit".')
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
        call errend(msg_invalid_input()//&
                  '\n  ap%coord_unit: '//str(ap%coord_unit)//&
                  '\nThis value is invalid when "f_x_vertex"'//&
                    ' is given. Check the value of "coord_unit".')
      endif
    endif

    if( keynum('coord_miss') == 1 ) ap%coord_miss_c = coord_miss
  endif

  call logext()
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
  ! Relations
  !-------------------------------------------------------------
  if( keynum('f_lon_vertex') == 1 .neqv. keynum('f_lat_vertex') == 1 )then
    call errend(msg_invalid_input()//&
              '\nBoth "f_lon_vertex" and "f_lat_vertex" must be given'//&
                ' when any of them is given.')
  endif

  if( (keynum('f_x_vertex') == 1 .neqv. keynum('f_y_vertex') == 1) .or. &
      (keynum('f_x_vertex') == 1 .neqv. keynum('f_z_vertex') == 1) )then
    call errend(msg_invalid_input()//&
              '\nAll of "f_x_vertex", "f_y_vertex" and "f_z_vertex" must be given'//&
                ' when any of them is given.')
  endif

  if( keynum('f_lon_vertex') == 1 .and. keynum('f_x_vertex') == 1 )then
    call errend(msg_invalid_input()//&
              '\n"f_lon_vertex" and "f_x_vertex" must not be given at the same time.')
  elseif( keynum('f_lon_vertex') == 0 .and. keynum('f_x_vertex') == 0 )then
    call errend(msg_invalid_input()//&
              '\nNeither "f_lon_vertex" nor "f_x_vertex" is given.')
  endif

  if( keynum('arc_parallel') == 1 .and. keynum('f_arctyp') == 1 )then
    call errend(msg_invalid_input()//&
              '\n"arc_parallel" and "f_arctyp" must not be given at the same time.')
  endif

  if( (keynum('fin_grdidx') == 0 .and. &
       keynum('fin_grdara') == 0 .and. &
       keynum('fin_grdwgt') == 0) .and. &
      (keynum('in_grid_sz') == 1 .or. &
       keynum('in_grid_lb') == 1 .or. &
       keynum('in_grid_ub') == 1) )then
    call errend(msg_invalid_input()//&
              '\nAny of "in_grid_sz", "in_grid_lb" or "in_grid_ub" must not be given'//&
              '\n when none of "fin_grdidx", "fin_grdara" or "fin_grdwgt" is given.')
  endif

  if( keynum('fin_grdidx') == 0 .and. keynum('idx_miss') == 1 )then
    call logwrn(msg_undesirable_input()//&
              '\nThe value for "idx_miss" is given although that for "fin_grdidx" is not given.'//&
                ' The input for "idx_miss" is ignored.')
  endif

  if( keynum('fin_grdara') == 1 .and. keynum('fin_grdwgt') == 1 )then
    call errend(msg_invalid_input()//&
              '\n"fin_grdara" and "fin_grdwgt" must not be given at the same time.')
  endif

  selectcase( fg_out%form )
  case( GRID_FORM_AUTO )
    continue
  case( GRID_FORM_INDEX )
    if( keynum('fin_grdidx') == 0 )then
      call errend(msg_invalid_input()//&
                '\n"fin_grdidx" must be given when '//&
                  'the value of "out_form" is "'//str(GRID_FORM_INDEX)//'".')
    endif
  case default
    call errend(msg_invalid_value('fg_out%form', fg_out%form))
  endselect
  !--------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
  use c1_const_util, only: &
        checkval_opt_old_files
  use c1_set, only: &
        line_number            , &
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
        KEY_EARTH_E2            , &
        set_values_opt_earth
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_opt'
  type(opt_), intent(inout) :: opt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the limits. of the number of each keyword
  !-------------------------------------------------------------
  call logent('Setting the limits. of the number of each keyword', PRCNAM, MODNAM)

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
  ! Set the default values
  !-------------------------------------------------------------
  ! Default values are set in advance because this procedure
  ! is not necessarily called.
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Files
    case( KEY_OLD_FILES )
      call read_value(opt%sys%old_files, is_keyword=.true.)

    case( KEY_DIR_INTERMEDIATES )
      call read_value(opt%sys%dir_im, is_keyword=.false.)

    case( KEY_REMOVE_INTERMEDIATES )
      call read_value(opt%sys%remove_im)
    !-----------------------------------------------------------
    ! System
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

  call traperr( checkval_opt_old_files(&
         opt%sys%old_files, 'opt%sys%old_files') )

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
  ! Free the external module variables
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
subroutine check_input(a, opt_earth)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input'
  type(gs_), intent(in), target :: a
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
subroutine check_paths(a, opt_sys)
  use c1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_paths'
  type(gs_)     , intent(inout) :: a
  type(opt_sys_), intent(in)    :: opt_sys

  type(file_latlon_in_), pointer :: fl
  type(file_raster_in_) , pointer :: fr
  type(file_polygon_in_), pointer :: fp
  type(file_grid_in_) , pointer :: fg_in
  type(file_grid_out_), pointer :: fg_out
  logical :: allow_empty

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking input files', PRCNAM, MODNAM)

  selectcase( a%typ )
  case( MESHTYPE__LATLON )
    fl => a%latlon%f_latlon_in
    call traperr( check_permission(fl%lon, allow_empty=.true.) )
    call traperr( check_permission(fl%lat, allow_empty=.true.) )
  case( MESHTYPE__RASTER )
    fr => a%raster%f_raster_in
    call traperr( check_permission(fr%idx, allow_empty=.false.) )
    call traperr( check_permission(fr%ara, allow_empty=.true.) )
    call traperr( check_permission(fr%wgt, allow_empty=.true.) )
  case( MESHTYPE__POLYGON )
    fp => a%polygon%f_polygon_in
    call traperr( check_permission(fp%x, allow_empty=.true.) )
    call traperr( check_permission(fp%y, allow_empty=.true.) )
    call traperr( check_permission(fp%z, allow_empty=.true.) )
    call traperr( check_permission(fp%lon, allow_empty=.true.) )
    call traperr( check_permission(fp%lat, allow_empty=.true.) )
    call traperr( check_permission(fp%arctyp, allow_empty=.true.) )
  case default
    call errend(msg_invalid_value('a%typ', a%typ))
  endselect

  fg_in => a%cmn%f_grid_in
  fg_out => a%cmn%f_grid_out

  selectcase( fg_out%form )
  case( GRID_FORM_AUTO )
    allow_empty = .true.
  case( GRID_FORM_INDEX )
    allow_empty = .false.
  case default
    call errend(msg_invalid_value('fg_out%form', fg_out%form))
  endselect

  call traperr( check_permission(fg_in%idx, allow_empty=allow_empty) )
  call traperr( check_permission(fg_in%ara, allow_empty=.true.) )
  call traperr( check_permission(fg_in%wgt, allow_empty=.true.) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking old output files', PRCNAM, MODNAM)

  call traperr( set_opt_old_files(opt_sys%old_files) )

  call traperr( handle_old_file(fg_out%idx) )
  call traperr( handle_old_file(fg_out%ara) )
  call traperr( handle_old_file(fg_out%wgt) )
  call traperr( handle_old_file(fg_out%x) )
  call traperr( handle_old_file(fg_out%y) )
  call traperr( handle_old_file(fg_out%z) )
  call traperr( handle_old_file(fg_out%lon) )
  call traperr( handle_old_file(fg_out%lat) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Preparing output directories', PRCNAM, MODNAM)

  call traperr( set_opt_mkdir(output=.true., hut=hut_command) )

  call traperr( mkdir(opt_sys%dir_im) )
  call traperr( try_make_empty_file(opt_sys%dir_im) )

  call traperr( mkdir(dirname(fg_out%idx%path)) )
  call traperr( mkdir(dirname(fg_out%ara%path)) )
  call traperr( mkdir(dirname(fg_out%wgt%path)) )
  call traperr( mkdir(dirname(fg_out%x%path)) )
  call traperr( mkdir(dirname(fg_out%y%path)) )
  call traperr( mkdir(dirname(fg_out%z%path)) )
  call traperr( mkdir(dirname(fg_out%lon%path)) )
  call traperr( mkdir(dirname(fg_out%lat%path)) )

  call traperr( check_permission(fg_out%idx, allow_empty=.true.) )
  call traperr( check_permission(fg_out%ara, allow_empty=.true.) )
  call traperr( check_permission(fg_out%wgt, allow_empty=.true.) )
  call traperr( check_permission(fg_out%x  , allow_empty=.true.) )
  call traperr( check_permission(fg_out%y  , allow_empty=.true.) )
  call traperr( check_permission(fg_out%z  , allow_empty=.true.) )
  call traperr( check_permission(fg_out%lon, allow_empty=.true.) )
  call traperr( check_permission(fg_out%lat, allow_empty=.true.) )

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
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  integer :: dgt_xy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Grid System (Lattice)')))

  fl     => al%f_latlon_in
  fg_in  => al%f_grid_in
  fg_out => al%f_grid_out
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_xy = dgt(max(al%nx, al%ny, maxval(fg_in%sz(:2))))

  call logmsg('ID: '//str(al%id))

  call logmsg('Mesh type: '//str(MESHTYPE__LATLON))

  call logmsg('nx: '//str(al%nx))
  call logmsg('ny: '//str(al%ny))

  if( fl%lon%path == '' )then
    call logmsg('West : '//str(al%west,'f12.5'))
    call logmsg('East : '//str(al%east,'f12.5'))
  else
    call logmsg('File of bounds of longit.: '//str(fl%lon%path))
  endif

  if( fl%lat%path == '' )then
    call logmsg('South: '//str(al%south,'f12.5'))
    call logmsg('North: '//str(al%north,'f12.5'))
  else
    call logmsg('File of bounds of latit. : '//str(fl%lat%path))
  endif

  call logmsg('Is south to north: '//str(al%is_south_to_north))

  call logmsg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call logmsg('  File of index : '//str(fileinfo(fg_in%idx)))
    call logmsg('          area  : '//str(fileinfo(fg_in%ara)))
    call logmsg('          weight: '//str(fileinfo(fg_in%wgt)))
    call logmsg('  Size: ('//str(fg_in%sz(:2),dgt_xy,', ')//')')
    call logmsg('  Use : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_xy,':')//')')
    if( fg_in%ara%path /= '' )then
      call logmsg('  Unit of area: '//str(fg_in%unit_ara))
    endif
  endif

  call logmsg('Grid data (out)')
  if( fg_out%save_idx .or. fg_out%save_ara .or. &
      fg_out%save_wgt .or. &
      fg_out%save_xyz .or. fg_out%save_lonlat )then
    call logmsg('  Form: '//str(fg_out%form))
    call logmsg('  File of index  : '//str(fileinfo(fg_out%idx)))
    call logmsg('          mask   : '//str(fileinfo(fg_out%msk)))
    call logmsg('          area   : '//str(fileinfo(fg_out%ara)))
    call logmsg('          weight : '//str(fileinfo(fg_out%wgt)))
    call logmsg('          x      : '//str(fileinfo(fg_out%x)))
    call logmsg('          y      : '//str(fileinfo(fg_out%y)))
    call logmsg('          z      : '//str(fileinfo(fg_out%z)))
    call logmsg('          longit.: '//str(fileinfo(fg_out%lon)))
    call logmsg('          latit. : '//str(fileinfo(fg_out%lat)))
    if( fg_out%save_ara )then
      call logmsg('  Unit of area  : '//str(fg_out%unit_ara))
    endif
    if( fg_out%save_xyz )then
      call logmsg('  Unit of xyz   : '//str(fg_out%unit_xyz))
    endif
    if( fg_out%save_lonlat )then
      call logmsg('  Unit of lonlat: '//str(fg_out%unit_lonlat))
    endif
  else
    call logmsg('  (No input)')
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(al%idx_miss))
  call logmsg('  Area  : '//str(al%ara_miss))
  call logmsg('  Weight: '//str(al%wgt_miss))
  call logmsg('  xyz   : '//str(al%xyz_miss))
  call logmsg('  lonlat: '//str(al%lonlat_miss))
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
  type(file_grid_in_)  , pointer :: fg_in
  type(file_grid_out_) , pointer :: fg_out
  integer :: dgt_xy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Grid System (Raster)')))

  fr     => ar%f_raster_in
  fg_in  => ar%f_grid_in
  fg_out => ar%f_grid_out
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_xy = dgt(maxval(fr%sz(:2)))

  call logmsg('ID: '//str(ar%id))

  call logmsg('Mesh type: '//str(MESHTYPE__RASTER))

  call logmsg('nx: '//str(ar%nx,dgt_xy))
  call logmsg('ny: '//str(ar%ny,dgt_xy))

  call logmsg('West : '//str(ar%west,'f12.5'))
  call logmsg('East : '//str(ar%east,'f12.5'))
  call logmsg('South: '//str(ar%south,'f12.5'))
  call logmsg('North: '//str(ar%north,'f12.5'))

  call logmsg('Is south to north: '//str(ar%is_south_to_north))

  call logmsg('Raster data')
  call logmsg('  File of index : '//str(fileinfo(fr%idx)))
  call logmsg('          area  : '//str(fileinfo(fr%ara)))
  call logmsg('          weight: '//str(fileinfo(fr%wgt)))
  call logmsg('  Size: ('//str(fr%sz(:2),dgt_xy,', ')//')')
  call logmsg('  Use : ('//str((/fr%lb(1),fr%ub(1)/),dgt_xy,':')//&
                   ', '//str((/fr%lb(2),fr%ub(2)/),dgt_xy,':')//')')

  call logmsg('Grid data (in)')
  if( fg_in%idx%path /= '' .or. fg_in%ara%path /= '' .or. fg_in%wgt%path /= '' )then
    call logmsg('  File of index : '//str(fileinfo(fg_in%idx)))
    call logmsg('          area  : '//str(fileinfo(fg_in%ara)))
    call logmsg('          weight: '//str(fileinfo(fg_in%wgt)))
    call logmsg('  Size  : ('//str(fg_in%sz(:2),dgt_xy,', ')//')')
    call logmsg('  Use   : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_xy,':')//')')
    call logmsg('  Length: '//str(fg_in%nij))
    if( fg_in%idx%path /= '' )then
      call logmsg('  Condition for index: '//str(ar%idx_condition))
    endif
    if( fg_in%ara%path /= '' )then
      call logmsg('  Unit of area: '//str(fg_in%unit_ara))
    endif
  endif

  call logmsg('Grid data (out)')
  if( fg_out%save_idx .or. fg_out%save_ara .or. &
      fg_out%save_wgt .or. &
      fg_out%save_xyz .or. fg_out%save_lonlat )then
    call logmsg('  Form: '//str(fg_out%form))
    call logmsg('  File of index  : '//str(fileinfo(fg_out%idx)))
    call logmsg('          mask   : '//str(fileinfo(fg_out%msk)))
    call logmsg('          area   : '//str(fileinfo(fg_out%ara)))
    call logmsg('          weight : '//str(fileinfo(fg_out%wgt)))
    call logmsg('          x      : '//str(fileinfo(fg_out%x)))
    call logmsg('          y      : '//str(fileinfo(fg_out%y)))
    call logmsg('          z      : '//str(fileinfo(fg_out%z)))
    call logmsg('          longit.: '//str(fileinfo(fg_out%lon)))
    call logmsg('          latit. : '//str(fileinfo(fg_out%lat)))
    call logmsg('  Size: ('//str(fg_out%sz(:2),dgt_xy,', ')//')')
    call logmsg('  Use : ('//str((/fg_out%lb(1),fg_out%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_out%lb(2),fg_out%ub(2)/),dgt_xy,':')//')')
    if( fg_out%save_ara )then
      call logmsg('  Unit of area  : '//str(fg_out%unit_ara))
    endif
    if( fg_out%save_xyz )then
      call logmsg('  Unit of xyz   : '//str(fg_out%unit_xyz))
    endif
    if( fg_out%save_lonlat )then
      call logmsg('  Unit of lonlat: '//str(fg_out%unit_lonlat))
    endif
  else
    call logmsg('  (No input)')
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(ar%idx_miss))
  call logmsg('  Area  : '//str(ar%ara_miss))
  call logmsg('  Weight: '//str(ar%wgt_miss))
  call logmsg('  xyz   : '//str(ar%xyz_miss))
  call logmsg('  lonlat: '//str(ar%lonlat_miss))
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
  type(file_grid_out_)  , pointer :: fg_out
  integer :: dgt_xy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Grid System (Polygon)')))

  fp     => ap%f_polygon_in
  fg_in  => ap%f_grid_in
  fg_out => ap%f_grid_out
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_xy = dgt(maxval(fp%sz(:2)))

  call logmsg('ID: '//str(ap%id))

  call logmsg('Mesh type: '//str(MESHTYPE__POLYGON))

  call logmsg('Polygon data')
  call logmsg('  Size : '//str(fp%sz(2),dgt_xy))
  call logmsg('  Input: ('//str((/fp%lb(2),fp%ub(2)/),dgt_xy,':')//')')

  call logmsg('Max. num. of vertices of a grid: '//str(ap%np))

  call logmsg('Coordinates')
  call logmsg('  Coordinate system: '//str(ap%coord_sys))
  call logmsg('  Files of coords. of vertices')
  selectcase( ap%coord_sys )
  case( COORD_SYS_SPHERICAL )
    call logmsg('    Longit.: '//str(fileinfo(fp%lon)))
    call logmsg('    Latit. : '//str(fileinfo(fp%lat)))
  case( COORD_SYS_CARTESIAN )
    call logmsg('    x: '//str(fileinfo(fp%x)))
    call logmsg('    y: '//str(fileinfo(fp%y)))
    call logmsg('    z: '//str(fileinfo(fp%z)))
  case default
    call errend(msg_invalid_value('ap%coord_sys', ap%coord_sys))
  endselect
  call logmsg('  Unit: '//str(ap%coord_unit))

  call logmsg('  Missing values of coords.')
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
    call logmsg('  File of index : '//str(fileinfo(fg_in%idx)))
    call logmsg('          area  : '//str(fileinfo(fg_in%ara)))
    call logmsg('          weight: '//str(fileinfo(fg_in%wgt)))
    call logmsg('  Size: ('//str(fg_in%sz(:2),dgt_xy,', ')//')')
    call logmsg('  Use : ('//str((/fg_in%lb(1),fg_in%ub(1)/),dgt_xy,':')//&
                     ', '//str((/fg_in%lb(2),fg_in%ub(2)/),dgt_xy,':')//')')
    if( fg_in%ara%path /= '' )then
      call logmsg('  Unit of area: '//str(fg_in%unit_ara))
    endif
  else
    call logmsg('  (No input)')
  endif

  call logmsg('Grid data (out)')
  if( fg_out%save_idx .or. fg_out%save_ara .or. &
      fg_out%save_wgt .or. &
      fg_out%save_xyz .or. fg_out%save_lonlat )then
    call logmsg('  Form: '//str(fg_out%form))
    call logmsg('  File of index  : '//str(fileinfo(fg_out%idx)))
    call logmsg('          mask   : '//str(fileinfo(fg_out%msk)))
    call logmsg('          area   : '//str(fileinfo(fg_out%ara)))
    call logmsg('          weight : '//str(fileinfo(fg_out%wgt)))
    call logmsg('          x      : '//str(fileinfo(fg_out%x)))
    call logmsg('          y      : '//str(fileinfo(fg_out%y)))
    call logmsg('          z      : '//str(fileinfo(fg_out%z)))
    call logmsg('          longit.: '//str(fileinfo(fg_out%lon)))
    call logmsg('          latit. : '//str(fileinfo(fg_out%lat)))
    if( fg_out%save_ara )then
      call logmsg('  Unit of area  : '//str(fg_out%unit_ara))
    endif
    if( fg_out%save_xyz )then
      call logmsg('  Unit of xyz   : '//str(fg_out%unit_xyz))
    endif
    if( fg_out%save_lonlat )then
      call logmsg('  Unit of lonlat: '//str(fg_out%unit_lonlat))
    endif
  else
    call logmsg('  (No input)')
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(ap%idx_miss))
  call logmsg('  Area  : '//str(ap%ara_miss))
  call logmsg('  Weight: '//str(ap%wgt_miss))
  call logmsg('  xyz   : '//str(ap%xyz_miss))
  call logmsg('  lonlat: '//str(ap%lonlat_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_gs_polygon
!===============================================================
!
!===============================================================
subroutine echo_settings_opt(opt)
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
  call logmsg(str('Options'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call setlog('+x2')

  call echo_settings_opt_sys(opt%sys)

  call echo_settings_opt_earth(opt%earth)

  call setlog('-x2')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
end module mod_set
