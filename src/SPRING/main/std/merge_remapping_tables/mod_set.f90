module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_opt_set, only: &
        KEY_OLD_FILES           , &
        KEY_DIR_INTERMEDIATES   , &
        KEY_REMOVE_INTERMEDIATES, &
        KEY_MEMORY_ULIM
  use c2_type_rt
  use c2_rt_set, only: &
        KEY_OPT_COEF_SUM_MODIFY      , &
        KEY_OPT_COEF_SUM_MODIFY_ULIM , &
        KEY_OPT_COEF_ZERO_POSITIVE   , &
        KEY_OPT_COEF_ZERO_NEGATIVE   , &
        KEY_OPT_COEF_ERROR_EXCESS    , &
        KEY_OPT_COEF_SUM_ERROR_EXCESS
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

  character(CLEN_VAR), parameter :: BLOCKNAME__INPUT  = 'input'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUTPUT = 'output'
  character(CLEN_VAR), parameter :: BLOCKNAME__OPT    = 'options'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_settings(input, output, opt)
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
  use c1_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log
  use c2_rt_base, only: &
        init_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings'
  type(input_) , intent(out) :: input
  type(output_), intent(out) :: output
  type(opt_)   , intent(out) :: opt

  type counter_
    integer :: input
    integer :: output
    integer :: opt
  end type

  type(counter_) :: counter

  character(clen_var) :: blockName

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call logent('Initializing', PRCNAM, MODNAM)

  call traperr( init_rt(output%rt) )
  output%rt%id = 'output%rt'

  call set_default_values_opt_sys(opt%sys)
  call set_default_values_opt_log(opt%log)

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
    call find_block(blockName)

    selectcase( blockName )
    !-----------------------------------------------------------
    ! Case: No more block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Case: input
    case( BLOCKNAME__INPUT )
      call update_counter(counter%input, blockName)
      call read_settings_input(input)
    !-----------------------------------------------------------
    ! Case: output
    case( BLOCKNAME__OUTPUT )
      call update_counter(counter%output, blockName)
      call read_settings_output(output)
    !-----------------------------------------------------------
    ! Case: options
    case( BLOCKNAME__OPT )
      call update_counter(counter%opt, blockName )
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('blockName', blockName)//&
                '\nCheck the names of the blocks.')
    endselect
  enddo

  call close_setting_file()

  call check_number_of_blocks()

  call logext()
  !-------------------------------------------------------------
  ! Check inputs
  !-------------------------------------------------------------
  call check_input(input, output)
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call logent('Setting some variables', PRCNAM, MODNAM)

  if( opt%sys%dir_im == '' )then
    opt%sys%dir_im = dirname(get_path_report())
    call logmsg('Directory of intermediates was not given.'//&
              '\nAutomatically set to "'//str(opt%sys%dir_im)//'".')
  endif

  output%path_grid_im = joined(opt%sys%dir_im, 'spring.grid.im')

  call logext()
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo_settings_input(input)

  call echo_settings_output(output)

  call echo_settings_opt(opt)

  call logmsg(str(bar('')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_paths(input, output, opt%sys)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_counter'

  counter%input  = 0
  counter%output = 0
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
  case( BLOCKNAME__INPUT , &
        BLOCKNAME__OUTPUT, &
        BLOCKNAME__OPT    )
    if( n > 2 )then
      call errend(msg_invalid_input(line_number())//&
                '\nBlock "'//str(blockName)//'" appeared more than once.')
    endif
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
  call check_num_of_key(counter%input , BLOCKNAME__INPUT , 1, 1)
  call check_num_of_key(counter%output, BLOCKNAME__OUTPUT, 1, 1)
  call check_num_of_key(counter%opt   , BLOCKNAME__OPT   , 0, 1)
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
subroutine read_settings_input(input)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_input'
  type(input_), intent(inout) :: input

  type(f_rt_)  , pointer :: f_rt
  type(f_grid_), pointer :: f_grid
  integer :: iFile_rt
  integer :: iFile_grid
  integer :: num_f_rt_sidx, num_f_rt_tidx, num_f_rt_area, num_f_rt_coef
  integer :: num_f_grdidx, num_f_grdara
  integer :: line_number_prev_length_rt
  character(clen_var) :: id

  character(clen_path) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('dir', 0, -1)
  call set_keynum('length_rt', 1, -1)
  call set_keynum('f_rt_sidx', 1, -1)
  call set_keynum('f_rt_tidx', 1, -1)
  call set_keynum('f_rt_area', 1, -1)
  call set_keynum('f_rt_coef', 1, -1)
  call set_keynum('length_grid', 0, -1)
  call set_keynum('f_grdidx', 0, -1)
  call set_keynum('f_grdara', 0, -1)
  call set_keynum('idx_miss', 0, 1)
  call set_keynum('opt_idx_duplication', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Count the number of times each keyword was used
  !-------------------------------------------------------------
  call logent('Counting the number of times each keyword was used', PRCNAM, MODNAM)

  input%nFiles_rt = 0
  input%nFiles_grid = 0
  line_number_prev_length_rt = 0
  num_f_rt_sidx = 0
  num_f_rt_tidx = 0
  num_f_rt_area = 0
  num_f_rt_coef = 0
  num_f_grdidx = 0
  num_f_grdara = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Remapping table
    case( 'length_rt' )
      call update_num_rt(input%nFiles_rt)

    case( 'f_rt_sidx' )
      call update_num_f_rt(num_f_rt_sidx, input%nFiles_rt)
    case( 'f_rt_tidx' )
      call update_num_f_rt(num_f_rt_tidx, input%nFiles_rt)
    case( 'f_rt_area' )
      call update_num_f_rt(num_f_rt_area, input%nFiles_rt)
    case( 'f_rt_coef' )
      call update_num_f_rt(num_f_rt_coef, input%nFiles_rt)
    !-----------------------------------------------------------
    ! Grid data
    case( 'length_grid' )
      call add(input%nFiles_grid)

    case( 'f_grdidx' )
      call add(num_f_grdidx)
    case( 'f_grdara' )
      call add(num_f_grdara)
    !-----------------------------------------------------------
    endselect
  enddo

  call check_keynum()
  call check_keynum_relations()

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  ! Remapping table
  !-------------------------------------------------------------
  allocate(input%list_f_rt(input%nFiles_rt))
  do iFile_rt = 1, input%nFiles_rt
    f_rt => input%list_f_rt(iFile_rt)
    id = 'input%list_f_rt('//str(iFile_rt)//')'
    call set_file_default(action=ACTION_READ)
    f_rt%f_sidx = file(trim(id)//'%f_sidx', dtype=DTYPE_INT4)
    f_rt%f_tidx = file(trim(id)//'%f_tidx', dtype=DTYPE_INT4)
    f_rt%f_area = file(trim(id)//'%f_area', dtype=DTYPE_DBLE)
    f_rt%f_coef = file(trim(id)//'%f_coef', dtype=DTYPE_DBLE)
    call reset_file_default()
  enddo

  ! Grid data
  !-------------------------------------------------------------
  if( input%nFiles_grid > 0 )then
    allocate(input%list_f_grid(input%nFiles_grid))
    do iFile_grid = 1, input%nFiles_grid
      f_grid => input%list_f_grid(iFile_grid)
      id = 'input%list_f_grid('//str(iFile_grid)//')'
      call set_file_default(action=ACTION_READ)
      f_grid%f_idx = file(trim(id)//'%f_idx', dtype=DTYPE_INT4)
      f_grid%f_ara = file(trim(id)//'%f_ara', dtype=DTYPE_DBLE)
      call reset_file_default()
    enddo
  endif

  input%idx_miss = IDX_MISS_DEFAULT
  input%opt_idx_dup = INPUT_OPT_IDX_DUP_STOP

  call logext()
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  call back_to_block_head()
  call reset_keynum()

  dir = ''
  input%nFiles_rt = 0
  input%nFiles_grid = 0

  do
    call read_input()
    call update_keynum()

    selectcase( key() )
    !-----------------------------------------------------------
    ! End of block
    case( '' )
      exit
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Remapping table
    case( 'length_rt' )
      call add(input%nFiles_rt)
      f_rt => input%list_f_rt(input%nFiles_rt)
      call read_value(f_rt%nij)

    case( 'f_rt_sidx' )
      call read_value(f_rt%f_sidx, dir)
    case( 'f_rt_tidx' )
      call read_value(f_rt%f_tidx, dir)
    case( 'f_rt_area' )
      call read_value(f_rt%f_area, dir)
    case( 'f_rt_coef' )
      call read_value(f_rt%f_coef, dir)
    !-----------------------------------------------------------
    ! Grid data
    case( 'length_grid' )
      call add(input%nFiles_grid)
      f_grid => input%list_f_grid(input%nFiles_grid)
      call read_value(f_grid%nmax)

    case( 'f_grdidx' )
      call read_value(f_grid%f_idx, dir)
    case( 'f_grdara' )
      call read_value(f_grid%f_ara, dir)

    case( 'idx_miss' )
      call read_value(input%idx_miss)

    case( 'opt_idx_duplication' )
      call read_value(input%opt_idx_dup, is_keyword=.true.)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  ! Remapping table
  !-------------------------------------------------------------
  do iFile_rt = 1, input%nFiles_rt
    f_rt => input%list_f_rt(iFile_rt)

    f_rt%f_sidx%length = f_rt%nij
    f_rt%f_tidx%length = f_rt%nij
    f_rt%f_area%length = f_rt%nij
    f_rt%f_coef%length = f_rt%nij
  enddo  ! iFile_rt/

  ! Grid data
  !-------------------------------------------------------------
  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    f_grid%f_idx%length = f_grid%nmax
    f_grid%f_ara%length = f_grid%nmax
  enddo  ! iFile_grid/

  selectcase( input%opt_idx_dup )
  case( input_opt_idx_dup_sum, &
        input_opt_idx_dup_stop )
    continue
  case default
    call errend(str(msg_invalid_value())//&
            '\n  input%opt_idx_dup: '//str(input%opt_idx_dup)//&
            '\nCheck the value of "opt_idx_duplication".')
  endselect

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
subroutine update_num_rt(n)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_num_rt'
  integer, intent(inout) :: n

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  if( n /= num_f_rt_sidx .or. &
      n /= num_f_rt_tidx .or. &
      n /= num_f_rt_area .or. &
      n /= num_f_rt_coef )then
    call errend(str(msg_invalid_input())//&
            '\n  Any of "f_rt_sidx", "f_rt_tidx", "f_rt_area" or "f_rt_coef" is missing'//&
              ' in the group of regridding tables starts with "length_rt"'//&
              ' @ line '//str(line_number_prev_length_rt)//'.')
  endif

  line_number_prev_length_rt = line_number()
  n = n + 1
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_num_rt
!---------------------------------------------------------------
subroutine update_num_f_rt(n, n_rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_num_f_rt'
  integer     , intent(inout) :: n
  integer     , intent(in)    :: n_rt

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  if( n /= n_rt )then
    call errend(str(msg_invalid_input())//&
            '\n  @ line '//str(line_number())//&
            '\n  "'//str(key())//'" appeared too many times in the group of remapping tables,'//&
              ' which start with "length_rt".')
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_num_f_rt
!---------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_keynum_relations'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !--------------------------------------------------------------
  ! Relations
  !--------------------------------------------------------------
  if( input%nFiles_rt /= num_f_rt_sidx .or. &
      input%nFiles_rt /= num_f_rt_tidx .or. &
      input%nFiles_rt /= num_f_rt_area .or. &
      input%nFiles_rt /= num_f_rt_coef )then
    call errend(msg_invalid_input()//&
              '\n  The number of inputs of "f_rt_sidx", "f_rt_tidx", '//&
                '"f_rt_area" or "f_rt_coef" mismatch with that of "length_rt".'//&
              '\n"length_rt": '//str(input%nFiles_rt)//&
              '\n"f_rt_sidx": '//str(num_f_rt_sidx)//&
              '\n"f_rt_tidx": '//str(num_f_rt_tidx)//&
              '\n"f_rt_area": '//str(num_f_rt_area)//&
              '\n"f_rt_coef": '//str(num_f_rt_coef))
  endif

  if( input%nFiles_grid /= num_f_grdidx .or. &
      input%nFiles_grid /= num_f_grdara )then
    call errend(msg_invalid_input()//&
              '\n  The number of inputs of "f_grdidx" or "f_grdara"'//&
                ' mismatch with that of the groups of grid data,'//&
                ' which start with "length_grid".'//&
              '\n"length_grid": '//str(input%nFiles_grid)//&
              '\n"f_grdidx"   : '//str(num_f_grdidx)//&
              '\n"f_grdara"   : '//str(num_f_grdara))
  endif
  !--------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_input
!===============================================================
!
!===============================================================
subroutine read_settings_output(output)
  use c1_set, only: &
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
  use c2_rt_base, only: &
        set_default_values_rt
  use c2_rt_set, only: &
        check_values_opt_rt_coef
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_output'
  type(output_), intent(inout), target :: output

  type(rt_), pointer :: rt
  type(rt_main_), pointer :: rtm

  character(clen_path) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('dir', 0, -1)
  call set_keynum('mesh_coef', 0, 1)
  call set_keynum('mesh_sort', 0, 1)
  call set_keynum('f_rt_sidx', 1, 1)
  call set_keynum('f_rt_tidx', 1, 1)
  call set_keynum('f_rt_area', 1, 1)
  call set_keynum('f_rt_coef', 1, 1)
  call set_keynum(KEY_OPT_COEF_SUM_MODIFY      , 0, 1)
  call set_keynum(KEY_OPT_COEF_SUM_MODIFY_ULIM , 0, 1)
  call set_keynum(KEY_OPT_COEF_ZERO_POSITIVE   , 0, 1)
  call set_keynum(KEY_OPT_COEF_ZERO_NEGATIVE   , 0, 1)
  call set_keynum(KEY_OPT_COEF_ERROR_EXCESS    , 0, 1)
  call set_keynum(KEY_OPT_COEF_SUM_ERROR_EXCESS, 0, 1)
  call set_keynum('f_grdidx', 0, 1)
  call set_keynum('f_grdara', 0, 1)
  !call set_keynum('f_grdcoef', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  rt => output%rt
  rtm => rt%main

  call traperr( set_default_values_rt(rt) )

  output%f_grid%f_idx = file(dtype=DTYPE_INT4, action=ACTION_WRITE, id='output%f_grid%f_idx')
  output%f_grid%f_ara = file(dtype=DTYPE_DBLE, action=ACTION_WRITE, id='output%f_grid%f_ara')

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
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Remapping table
    case( 'mesh_coef' )
      call read_value(rtm%mesh_coef, is_keyword=.true.)

    case( 'mesh_sort' )
      call read_value(rtm%mesh_sort, is_keyword=.true.)

    case( 'f_rt_sidx' )
      call read_value(rtm%f%sidx, dir)
    case( 'f_rt_tidx' )
      call read_value(rtm%f%tidx, dir)
    case( 'f_rt_area' )
      call read_value(rtm%f%area, dir)
    case( 'f_rt_coef' )
      call read_value(rtm%f%coef, dir)
    !-----------------------------------------------------------
    ! Options of coef.
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
    ! Grid data (output)
    case( 'f_grdidx' )
      call read_value(output%f_grid%f_idx, dir)
    case( 'f_grdara' )
      call read_value(output%f_grid%f_ara, dir)
    !-----------------------------------------------------------
    ! ERORR
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

  call check_values_opt_rt_coef(rtm%opt_coef)

  selectcase( rtm%mesh_coef )
  case( MESH__SOURCE, &
        MESH__TARGET, &
        MESH__NONE )
    continue
  case default
    call errend(msg_invalid_value('rtm%mesh_coef', rtm%mesh_coef)//&
              '\nCheck the value of "mesh_coef".')
  endselect

  selectcase( rtm%mesh_sort )
  case( MESH__SOURCE, &
        MESH__TARGET, &
        MESH__NONE )
    continue
  case default
    call errend(str(msg_invalid_value())//&
            '\n  rtm%mesh_sort: '//str(rtm%mesh_sort)//&
            '\nCheck the value of "mesh_sort".')
  endselect

  call logext()
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!----------------------------------------------------------------
contains
!----------------------------------------------------------------
subroutine check_keynum_relations()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__check_keynum_relations'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !--------------------------------------------------------------
  !
  !--------------------------------------------------------------
  if( keynum(KEY_OPT_COEF_SUM_MODIFY) == 1 .and. &
      keynum(KEY_OPT_COEF_SUM_MODIFY_ULIM) == 1 )then
    call errend(str(msg_invalid_input())//&
            '\n"'//str(KEY_OPT_COEF_SUM_MODIFY)//'" and "'//&
              str(KEY_OPT_COEF_SUM_MODIFY_ULIM)//&
              '" must not be given at the same time.')
  endif
  !--------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!----------------------------------------------------------------
end subroutine read_settings_output
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
        reset_keynum           , &
        update_keynum          , &
        check_keynum           , &
        read_input             , &
        read_value             , &
        raise_error_invalid_key, &
        msg_invalid_input      , &
        msg_undesirable_input
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
    ! System
    case( KEY_OLD_FILES )
      call read_value(opt%sys%old_files, is_keyword=.true.)

    case( KEY_DIR_INTERMEDIATES )
      call read_value(opt%sys%dir_im, is_path=.true.)

    case( KEY_REMOVE_INTERMEDIATES )
      call read_value(opt%sys%remove_im)

    case( KEY_MEMORY_ULIM )
      call read_value(opt%sys%memory_ulim)
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

  call traperr( checkval_opt_old_files(opt%sys%old_files, 'opt%sys%old_files') )

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
subroutine check_keynum_relations()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_keynum_relations'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !--------------------------------------------------------------
  !
  !--------------------------------------------------------------

  !--------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_keynum_relations
!---------------------------------------------------------------
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
subroutine check_input(input, output)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input'
  type(input_) , intent(in) :: input
  type(output_), intent(in) :: output

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( output%rt%main%mesh_coef )
  case( MESH__SOURCE, &
        MESH__TARGET )
    if( output%rt%main%opt_coef%is_sum_modify_enabled )then
      if( input%nFiles_grid > 0 )then
        call logwrn(msg_unexpected_condition()//&
                  '\n  "'//str(KEY_OPT_COEF_SUM_MODIFY)//'" is active and grid data are input.'//&
                  '\nInterpolation coefficients are computed using intersection area data '//&
                    'of the remapping tables and modified so that the summuation of the '//&
                    'coefficients of each grid (specified by the key "mesh_coef" in the block "'//&
                    str(BLOCKNAME__OUTPUT)//'" and is "target" by default) is equal to the '//&
                    'value specified by the key "'//str(KEY_OPT_COEF_SUM_MODIFY)//'". '//&
                    'Inputs of grid area data and index data are ignored because they '//&
                    'are not used with the current settings.')
      endif
    else
      if( input%nFiles_grid == 0 )then
        call errend(msg_unexpected_condition()//&
                  '\n  "'//str(KEY_OPT_COEF_SUM_MODIFY)//&
                    '" is inactive and grid data are not input.'//&
                  '\nInterpolation coefficients are computed using intersection area '//&
                    'data of the remapping tables and grid area data, but now grid area data '//&
                    'are missing. Specify the files of grid area data and correspondant '//&
                    'grid index data by the keys "fin_grdara" and "fin_grdidx", respectively, '//&
                    'in the block "'//str(BLOCKNAME__INPUT)//'".')
      endif
    endif
  case( MESH__NONE )
    continue
  case default
    call errend(msg_invalid_value('output%rt%main%mesh_coef', output%rt%main%mesh_coef))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_input
!===============================================================
!
!===============================================================
subroutine check_paths(input, output, opt_sys)
  use c1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_paths'
  type(input_)  , intent(in)         :: input
  type(output_) , intent(in), target :: output
  type(opt_sys_), intent(in)         :: opt_sys

  type(f_rt_)  , pointer :: f_rt
  type(f_grid_), pointer :: f_grid
  type(rt_main_), pointer :: rtm

  integer :: iFile_rt
  integer :: iFile_grid

  character(2), parameter :: hut_command = '+ '

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => output%rt%main
  !-------------------------------------------------------------
  ! Check input files
  !-------------------------------------------------------------
  call logent('Checking input files', PRCNAM, MODNAM)

  do iFile_rt = 1, input%nFiles_rt
    f_rt => input%list_f_rt(iFile_rt)

    call traperr( check_permission(f_rt%f_sidx) )
    call traperr( check_permission(f_rt%f_tidx) )
    call traperr( check_permission(f_rt%f_area) )
    call traperr( check_permission(f_rt%f_coef) )

    call traperr( check_file_size(f_rt%f_sidx) )
    call traperr( check_file_size(f_rt%f_tidx) )
    call traperr( check_file_size(f_rt%f_area) )
    call traperr( check_file_size(f_rt%f_coef) )
  enddo

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    call traperr( check_permission(f_grid%f_idx) )
    call traperr( check_permission(f_grid%f_ara) )

    call traperr( check_file_size(f_grid%f_idx) )
    call traperr( check_file_size(f_grid%f_ara) )
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Check old files of output
  !-------------------------------------------------------------
  call logent('Checking old files of output', PRCNAM, MODNAM)

  call traperr( set_opt_old_files(opt_sys%old_files) )

  call traperr( handle_old_file(output%f_grid%f_idx%path, 'output%f_grid%f_idx%path') )
  call traperr( handle_old_file(output%f_grid%f_ara%path, 'output%f_grid%f_ara%path') )

  call traperr( handle_old_file(rtm%f%sidx%path, 'output%f_rt%f_sidx%path') )
  call traperr( handle_old_file(rtm%f%tidx%path, 'output%f_rt%f_tidx%path') )
  call traperr( handle_old_file(rtm%f%area%path, 'output%f_rt%f_area%path') )
  call traperr( handle_old_file(rtm%f%coef%path, 'output%f_rt%f_coef%path') )

  call logext()
  !-------------------------------------------------------------
  ! Prepare output directories
  !-------------------------------------------------------------
  call logent('Preparing output directories', PRCNAM, MODNAM)

  call traperr( set_opt_mkdir(.true., hut_command) )

  call traperr( mkdir(dirname(output%f_grid%f_idx%path)) )
  call traperr( mkdir(dirname(output%f_grid%f_ara%path)) )

  call traperr( try_make_empty_file(dirname(output%f_grid%f_idx%path)) )
  call traperr( try_make_empty_file(dirname(output%f_grid%f_ara%path)) )

  call traperr( mkdir(dirname(rtm%f%sidx%path)) )
  call traperr( mkdir(dirname(rtm%f%tidx%path)) )
  call traperr( mkdir(dirname(rtm%f%area%path)) )
  call traperr( mkdir(dirname(rtm%f%coef%path)) )

  call traperr( try_make_empty_file(dirname(rtm%f%sidx%path)) )
  call traperr( try_make_empty_file(dirname(rtm%f%tidx%path)) )
  call traperr( try_make_empty_file(dirname(rtm%f%area%path)) )
  call traperr( try_make_empty_file(dirname(rtm%f%coef%path)) )

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
subroutine echo_settings_input(input)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_input'
  type(input_), intent(in) :: input

  type(f_rt_)  , pointer :: f_rt
  type(f_grid_), pointer :: f_grid

  integer :: iFile_rt
  integer :: iFile_grid
  character(clen_line) :: msg

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Input')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Remapping tables')
  call logmsg('  Number of remapping tables: '//str(input%nFiles_rt))
  do iFile_rt = 1, input%nFiles_rt
    f_rt => input%list_f_rt(iFile_rt)
    call logmsg('    ('//str(iFile_rt,dgt(input%nFiles_rt))//')')
    call logmsg('    Src idx: '//str(fileinfo(f_rt%f_sidx)))
    call logmsg('    Tgt idx: '//str(fileinfo(f_rt%f_tidx)))
    call logmsg('    Area   : '//str(fileinfo(f_rt%f_area)))
    call logmsg('    Coef   : '//str(fileinfo(f_rt%f_coef)))
  enddo

  call logmsg('Grid data')
  call logmsg('  Number of grid data: '//str(input%nFiles_grid))
  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)
    call logmsg('    ('//str(iFile_grid,dgt(input%nFiles_grid))//')')
    call logmsg('    Index: '//str(fileinfo(f_grid%f_idx)))
    call logmsg('    Area : '//str(fileinfo(f_grid%f_ara)))
  enddo

  call logmsg('  Missing index: '//str(input%idx_miss))

  selectcase( input%opt_idx_dup )
  case( input_opt_idx_dup_stop )
    msg = 'stop'
  case( input_opt_idx_dup_sum )
    msg = 'calc. sum. of area'
  case default
    call errend(str(msg_invalid_value())//&
            '\n  input%opt_idx_dup: '//str(input%opt_idx_dup))
  endselect

  call logmsg('Option for duplicated index in grid data: '//str(msg))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_input
!===============================================================
!
!===============================================================
subroutine echo_settings_output(output)
  use c1_set, only: &
        bar
  use c2_rt_set, only: &
        echo_settings_opt_rt_coef
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_output'
  type(output_), intent(in), target :: output

  type(rt_main_), pointer :: rtm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Output')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtm => output%rt%main

  call logmsg('Remapping table')
  call logmsg('  Mesh to calc. coef.: '//str(rtm%mesh_coef))
  call logmsg('  Mesh to sort by    : '//str(rtm%mesh_sort))

  call logmsg('  Src idx: '//str(fileinfo(rtm%f%sidx)))
  call logmsg('  Tgt idx: '//str(fileinfo(rtm%f%tidx)))
  call logmsg('  Area   : '//str(fileinfo(rtm%f%area)))
  call logmsg('  Coef   : '//str(fileinfo(rtm%f%coef)))

  call echo_settings_opt_rt_coef(rtm%opt_coef, 2)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Grid data')
  call logmsg('  Index: '//str(fileinfo(output%f_grid%f_idx)))
  call logmsg('  Area : '//str(fileinfo(output%f_grid%f_ara)))
  call logmsg('  Intermediate file: '//str(output%path_grid_im))
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
        echo_settings_opt_log
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt'
  type(opt_), intent(in) :: opt

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Options')))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)
  call echo_settings_opt_log(opt%log)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt
!===============================================================
!
!===============================================================
end module mod_set
