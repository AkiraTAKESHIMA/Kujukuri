module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_opt
  use c2_type_rt
  use def_const
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
subroutine read_settings(&
    rt_in_agcm_to_ogcm, rt_out_lsm_to_agcm, agcm, lsm, opt_ext)
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
  use c2_rt_base, only: &
        init_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings'
  type(rt_)  , intent(out), target :: rt_in_agcm_to_ogcm
  type(rt_)  , intent(out), target :: rt_out_lsm_to_agcm
  type(agcm_), intent(out), target :: agcm
  type(lsm_) , intent(out), target :: lsm
  type(opt_ext_), intent(out) :: opt_ext

  type counter_
    integer :: input_rt_agcm_to_ogcm
    integer :: output_rt_lsm_to_agcm
    integer :: input_agcm
    integer :: input_lsm
    integer :: options
  end type
  type(counter_) :: counter

  character(CLEN_VAR), parameter :: BLOCKNAME__IN_RT_A2O  = 'input_rt_agcm_to_ogcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_AGCM    = 'input_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_LSM     = 'input_lsm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_RT_L2A = 'output_rt_lsm_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OPT        = 'options'

  character(CLEN_VAR) :: blockName
  type(opt_) :: opt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Initializing variables', PRCNAM, MODNAM)

  call traperr( init_rt(rt_in_agcm_to_ogcm) )
  call traperr( init_rt(rt_out_lsm_to_agcm) )

  rt_in_agcm_to_ogcm%id = 'rt_in_agcm_to_ogcm'
  rt_out_lsm_to_agcm%id = 'rt_out_lsm_to_agcm'
  agcm%id = 'agcm'
  lsm%id  = 'lsm'

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
    ! No more block
    case( '' )
     exit
    !-----------------------------------------------------------
    !
    case( BLOCKNAME__IN_RT_A2O )
      call update_counter(counter%input_rt_agcm_to_ogcm)
      call read_settings_input_rt(rt_in_agcm_to_ogcm)

    case( BLOCKNAME__IN_AGCM )
      call update_counter(counter%input_agcm)
      call read_settings_input_agcm(agcm)

    case( BLOCKNAME__IN_LSM )
      call update_counter(counter%input_lsm)
      call read_settings_input_lsm(lsm)

    case( BLOCKNAME__OUT_RT_L2A )
      call update_counter(counter%output_rt_lsm_to_agcm)
      call read_settings_output_rt(rt_out_lsm_to_agcm)

    case( BLOCKNAME__OPT )
      call update_counter(counter%options)
      call read_settings_opt(opt, opt_ext)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call errend(msg_invalid_value('blockName', blockName)//&
                '\nCheck the name of the block.')
    endselect
  enddo

  call close_setting_file()

  call check_number_of_blocks()

  call logext()
  !-------------------------------------------------------------
  ! Check inputs
  !-------------------------------------------------------------
  !-------------------------------------------------------------
  ! Set some variables
  !-------------------------------------------------------------
  call logent('Setting some variables', PRCNAM, MODNAM)

  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)
  call set_opt_earth(opt%earth)

  call logext()
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo_settings_input_rt(rt_in_agcm_to_ogcm)
  call echo_settings_input_agcm(agcm)
  call echo_settings_input_lsm(lsm)
  call echo_settings_output_rt(rt_out_lsm_to_agcm)
  call echo_settings_opt(opt, opt_ext)
  call logmsg(str(bar('')))
  !-------------------------------------------------------------
  ! Check paths
  !-------------------------------------------------------------
  call check_paths(rt_in_agcm_to_ogcm, agcm, lsm, rt_out_lsm_to_agcm, opt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_counter'

  counter%input_rt_agcm_to_ogcm = 0
  counter%input_agcm = 0
  counter%input_lsm = 0
  counter%output_rt_lsm_to_agcm = 0
  counter%options = 0
end subroutine init_counter
!---------------------------------------------------------------
subroutine update_counter(n)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_counter'
  integer, intent(inout) :: n

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  n = n + 1

  call check_num_of_key(&
         counter%input_rt_agcm_to_ogcm, &
         BLOCKNAME__IN_RT_A2O, 0, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         BLOCKNAME__IN_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_lsm, &
         BLOCKNAME__IN_LSM, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_to_agcm, &
         BLOCKNAME__OUT_RT_L2A, 0, 1)

  call check_num_of_key(&
         counter%options, &
         BLOCKNAME__OPT, 0, 1)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine update_counter
!---------------------------------------------------------------
subroutine check_number_of_blocks()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_number_of_blocks'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call check_num_of_key(&
         counter%input_rt_agcm_to_ogcm, &
         BLOCKNAME__IN_RT_A2O, 1, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         BLOCKNAME__IN_AGCM, 1, 1)

  call check_num_of_key(&
         counter%input_lsm, &
         BLOCKNAME__IN_LSM, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_to_agcm, &
         BLOCKNAME__OUT_RT_L2A, 1, 1)

  call check_num_of_key(&
         counter%options, &
         BLOCKNAME__OPT, 0, 1)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_number_of_blocks
!---------------------------------------------------------------
end subroutine read_settings
!===============================================================
!
!===============================================================
subroutine read_settings_input_rt(rt)
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
  use c2_rt_base, only: &
        set_default_values_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_input_rt'
  type(rt_), intent(inout), target :: rt

  type(rt_main_), pointer :: rtm

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('length', 1, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_sidx', 1, 1)
  call set_keynum('f_tidx', 1, 1)
  call set_keynum('f_area', 1, 1)
  call set_keynum('f_coef', 1, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  rtm => rt%main

  call traperr( set_default_values_rt(rt) )
  rtm%mesh_sort = MESH__NONE

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
    !
    case( 'length' )
      call read_value(rtm%nij)
    !-----------------------------------------------------------
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)

    case( 'f_sidx' )
      call read_value(rtm%f%sidx, dir)
    case( 'f_tidx' )
      call read_value(rtm%f%tidx, dir)
    case( 'f_area' )
      call read_value(rtm%f%area, dir)
    case( 'f_coef' )
      call read_value(rtm%f%coef, dir)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  rtm%f%sidx%length = rtm%nij
  rtm%f%tidx%length = rtm%nij
  rtm%f%area%length = rtm%nij
  rtm%f%coef%length = rtm%nij

  call logext()
  !-------------------------------------------------------------
  ! Free module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_input_rt
!===============================================================
!
!===============================================================
subroutine read_settings_input_agcm(agcm)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_input_agcm'
  type(agcm_), intent(inout) :: agcm

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('nij', 1, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_grdidx', 0, 1)
  call set_keynum('f_grdara', 1, 1)
  call set_keynum('f_grdlon', 1, 1)
  call set_keynum('f_grdlat', 1, 1)
  call set_keynum('idx_miss', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  agcm%nij = 0_8

  call set_file_default(action=ACTION_READ)
  agcm%f_grdidx = file(dtype=DTYPE_INT4, id=trim(agcm%id)//'%f_grdidx')
  agcm%f_grdara = file(dtype=DTYPE_DBLE, id=trim(agcm%id)//'%f_grdara')
  agcm%f_grdlon = file(dtype=DTYPE_DBLE, id=trim(agcm%id)//'%f_grdlon')
  agcm%f_grdlat = file(dtype=DTYPE_DBLE, id=trim(agcm%id)//'%f_grdlat')
  call reset_file_default()

  agcm%idx_miss = IDX_MISS_DEFAULT

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
    !
    case( 'nij' )
      call read_value(agcm%nij)
    !-----------------------------------------------------------
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    case( 'f_grdidx' )
      call read_value(agcm%f_grdidx, dir)
    case( 'f_grdara' )
      call read_value(agcm%f_grdara, dir)
    case( 'f_grdlon' )
      call read_value(agcm%f_grdlon, dir)
    case( 'f_grdlat' )
      call read_value(agcm%f_grdlat, dir)
    !-----------------------------------------------------------
    !
    case( 'idx_miss' )
      call read_value(agcm%idx_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  agcm%f_grdidx%length = agcm%nij
  agcm%f_grdara%length = agcm%nij
  agcm%f_grdlon%length = agcm%nij
  agcm%f_grdlat%length = agcm%nij

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_input_lsm(lsm)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_input_lsm'
  type(lsm_), intent(inout) :: lsm

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('nij', 1, 1)
  call set_keynum('dir', 0, -1)
  call set_keynum('f_grdidx', 0, 1)
  call set_keynum('f_grdara', 1, 1)
  call set_keynum('f_grdlon', 1, 1)
  call set_keynum('f_grdlat', 1, 1)
  call set_keynum('idx_miss', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  lsm%nij = 0_8

  call set_file_default(action=ACTION_READ)
  lsm%f_grdidx = file(dtype=DTYPE_INT4, id=trim(lsm%id)//'%f_grdidx')
  lsm%f_grdara = file(dtype=DTYPE_DBLE, id=trim(lsm%id)//'%f_grdara')
  lsm%f_grdlon = file(dtype=DTYPE_DBLE, id=trim(lsm%id)//'%f_grdlon')
  lsm%f_grdlat = file(dtype=DTYPE_DBLE, id=trim(lsm%id)//'%f_grdlat')
  call reset_file_default()

  lsm%idx_miss = IDX_MISS_DEFAULT

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
    !
    case( 'nij' )
      call read_value(lsm%nij)
    !-----------------------------------------------------------
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)

    case( 'f_grdidx' )
      call read_value(lsm%f_grdidx, dir)
    case( 'f_grdara' )
      call read_value(lsm%f_grdara, dir)
    case( 'f_grdlon' )
      call read_value(lsm%f_grdlon, dir)
    case( 'f_grdlat' )
      call read_value(lsm%f_grdlat, dir)
    !-----------------------------------------------------------
    !
    case( 'idx_miss' )
      call read_value(lsm%idx_miss)

    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call logext()
  !-------------------------------------------------------------
  ! Set the related values
  !-------------------------------------------------------------
  call logent('Setting the related values', PRCNAM, MODNAM)

  lsm%f_grdidx%length = lsm%nij
  lsm%f_grdara%length = lsm%nij
  lsm%f_grdlon%length = lsm%nij
  lsm%f_grdlat%length = lsm%nij

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_input_lsm
!===============================================================
!
!===============================================================
subroutine read_settings_output_rt(rt)
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
  use c2_rt_base, only: &
        set_default_values_rt
  use c2_rt_set, only: &
        KEY_OPT_COEF_SUM_MODIFY      , &
        KEY_OPT_COEF_SUM_MODIFY_ULIM , &
        KEY_OPT_COEF_ZERO_POSITIVE   , &
        KEY_OPT_COEF_ZERO_NEGATIVE   , &
        KEY_OPT_COEF_ERROR_EXCESS    , &
        KEY_OPT_COEF_SUM_ERROR_EXCESS
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_output_rt'
  type(rt_), intent(inout), target :: rt

  type(rt_main_), pointer :: rtm
  type(rt_vrf_), pointer :: rtv

  character(CLEN_PATH) :: dir
  character(CLEN_KEY) :: mesh_vrf

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('mesh_coef', 0, 1)
  call set_keynum('mesh_sort', 0, 1)

  call set_keynum('dir', 0, -1)

  call set_keynum('fout_rt_sidx', 1, 1)
  call set_keynum('fout_rt_tidx', 1, 1)
  call set_keynum('fout_rt_area', 1, 1)
  call set_keynum('fout_rt_coef', 1, 1)
  call set_keynum(KEY_OPT_COEF_SUM_MODIFY      , 0, 1)
  call set_keynum(KEY_OPT_COEF_SUM_MODIFY_ULIM , 0, 1)
  call set_keynum(KEY_OPT_COEF_ZERO_POSITIVE   , 0, 1)
  call set_keynum(KEY_OPT_COEF_ZERO_NEGATIVE   , 0, 1)
  call set_keynum(KEY_OPT_COEF_ERROR_EXCESS    , 0, 1)
  call set_keynum(KEY_OPT_COEF_SUM_ERROR_EXCESS, 0, 1)

  call set_keynum('mesh_vrf'     , 0, -1)
  call set_keynum('fout_vrf_grdidx'     , 0, -1)
  call set_keynum('fout_vrf_grdara_true', 0, -1)
  call set_keynum('fout_vrf_grdara_rt'  , 0, -1)
  call set_keynum('fout_vrf_rerr_grdara', 0, -1)
  call set_keynum('fout_vrf_grdnum'     , 0, -1)
  call set_keynum('vrf_val_miss'        , 0, -1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  rtm => rt%main

  call traperr( set_default_values_rt(rt) )

  call logext()
  !-------------------------------------------------------------
  ! Read the settings
  !-------------------------------------------------------------
  call logent('Reading the settings', PRCNAM, MODNAM)

  call back_to_block_head()
  call reset_keynum()

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
    !
    case( 'mesh_coef' )
      call read_value(rtm%mesh_coef, is_keyword=.true.)

    case( 'mesh_sort' )
      call read_value(rtm%mesh_sort, is_keyword=.true.)
    !-----------------------------------------------------------
    !
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    case( 'fout_rt_sidx' )
      call read_value(rtm%f%sidx, dir)
    case( 'fout_rt_tidx' )
      call read_value(rtm%f%tidx, dir)
    case( 'fout_rt_area' )
      call read_value(rtm%f%area, dir)
    case( 'fout_rt_coef' )
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

    case( 'vrf_val_miss' )
      call read_value(rtv%dval_miss)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

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
subroutine read_value_fvrf_file(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_value_fvrf_file'
  type(file_), intent(inout) :: f

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( key() /= 'fout_vrf_grdidx'      .and. &
      key() /= 'fout_vrf_grdara_true' .and. &
      key() /= 'fout_vrf_grdara_rt'   .and. &
      key() /= 'fout_vrf_rerr_grdara' .and. &
      key() /= 'fout_vrf_grdnum'      )then
    call errend(msg_invalid_input(line_number())//&
              '\nOnly the following keys can be used for the group of'//&
                ' verification data:'//&
              '\n  "fout_vrf_grdidx"'//&
              '\n  "fout_vrf_grdara_true"'//&
              '\n  "fout_vrf_grdara_rt"'//&
              '\n  "fout_vrf_rerr_grdara"'//&
              '\n  "fout_vrf_grdnum"')
  endif
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
!---------------------------------------------------------------
end subroutine read_settings_output_rt
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt, opt_ext)
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
  use mod_const_util, only: &
        checkval_method_rivwat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_opt'
  type(opt_)    , intent(inout) :: opt
  type(opt_ext_), intent(inout) :: opt_ext

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

  call set_keynum('method_rivwat', 0, 1)

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
    ! Option
    case( 'method_rivwat' )
      call read_value(opt_ext%method_rivwat)
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
  call checkval_method_rivwat(opt_ext%method_rivwat, 'opt_ext%method_rivwat')

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
subroutine echo_settings_input_rt(rt)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_input_rt'
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call logmsg(str(bar('Input of Regridding Table (AGCM - OGCM_ocean)')))
  !-------------------------------------------------------------
  rtm => rt%main

  call logmsg('length: '//str(rtm%nij))
  call logmsg('sidx: '//str(fileinfo(rtm%f%sidx)))
  call logmsg('tidx: '//str(fileinfo(rtm%f%tidx)))
  call logmsg('coef: '//str(fileinfo(rtm%f%coef)))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_input_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_input_agcm(agcm)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_input_agcm'
  type(agcm_), intent(in) :: agcm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call logmsg(str(bar('Input of OGCM')))
  !-------------------------------------------------------------
  call logmsg('nij: '//str(agcm%nij))
  call logmsg('grdidx: '//str(fileinfo(agcm%f_grdidx)))
  call logmsg('grdara: '//str(fileinfo(agcm%f_grdara)))
  call logmsg('grdlon: '//str(fileinfo(agcm%f_grdlon)))
  call logmsg('grdlat: '//str(fileinfo(agcm%f_grdlat)))

  call logmsg('Missing index: '//str(agcm%idx_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine echo_settings_input_lsm(lsm)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_input_lsm'
  type(lsm_), intent(in) :: lsm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call logmsg(str(bar('Input of LSM')))
  !-------------------------------------------------------------
  call logmsg('nij: '//str(lsm%nij))
  call logmsg('grdidx: '//str(fileinfo(lsm%f_grdidx)))
  call logmsg('grdara: '//str(fileinfo(lsm%f_grdara)))
  call logmsg('grdlon: '//str(fileinfo(lsm%f_grdlon)))
  call logmsg('grdlat: '//str(fileinfo(lsm%f_grdlat)))

  call logmsg('Missing index: '//str(lsm%idx_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_input_lsm
!===============================================================
!
!===============================================================
subroutine echo_settings_output_rt(rt)
  use c1_set, only: &
        bar
  use c2_rt_set, only: &
        echo_settings_opt_rt_coef
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_output_rt'
  type(rt_), intent(in), target :: rt

  type(rt_main_), pointer :: rtm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call logmsg(str(bar('Output of Regridding Table (LSM - AGCM)')))
  !-------------------------------------------------------------
  rtm => rt%main

  call logmsg('mesh_coef: '//str(rtm%mesh_coef))
  call logmsg('mesh_sort: '//str(rtm%mesh_sort))

  call logmsg('Files')
  call logmsg('  sidx: '//str(fileinfo(rtm%f%sidx)))
  call logmsg('  tidx: '//str(fileinfo(rtm%f%tidx)))
  call logmsg('  area: '//str(fileinfo(rtm%f%area)))
  call logmsg('  coef: '//str(fileinfo(rtm%f%coef)))

  call echo_settings_opt_rt_coef(rtm%opt_coef, 0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_output_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_opt(opt, opt_ext)
  use c1_set, only: &
        bar
  use c1_opt_set, only: &
        echo_settings_opt_sys, &
        echo_settings_opt_log, &
        echo_settings_opt_earth
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt'
  type(opt_)    , intent(in) :: opt
  type(opt_ext_), intent(in) :: opt_ext

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  call echo_settings_opt_sys(opt%sys)
  call echo_settings_opt_log(opt%log)
  call echo_settings_opt_earth(opt%earth)

  call logmsg('Method for river water: '//str(opt_ext%method_rivwat))
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
subroutine check_paths(&
    rt_in_agcm_to_ogcm, agcm, lsm, &
    rt_out_lsm_to_agcm, opt)
  use c1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_paths'
  type(rt_)   , intent(in), target :: rt_in_agcm_to_ogcm
  type(agcm_) , intent(in)         :: agcm
  type(lsm_)  , intent(in)         :: lsm
  type(rt_)   , intent(in), target :: rt_out_lsm_to_agcm
  type(opt_)  , intent(in)         :: opt

  type(rt_main_), pointer :: rtmi
  type(rt_main_), pointer :: rtmo

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtmi => rt_in_agcm_to_ogcm%main
  rtmo => rt_out_lsm_to_agcm%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking permissions of input files', PRCNAM, MODNAM)

  call traperr( check_permission(rtmi%f%sidx) )
  call traperr( check_permission(rtmi%f%tidx) )
  call traperr( check_permission(rtmi%f%area) )
  call traperr( check_permission(rtmi%f%coef) )

  call traperr( check_permission(agcm%f_grdidx, allow_empty=.true.) )
  call traperr( check_permission(agcm%f_grdara) )
  call traperr( check_permission(agcm%f_grdlon) )
  call traperr( check_permission(agcm%f_grdlat) )

  call traperr( check_permission(lsm%f_grdidx, allow_empty=.true.) )
  call traperr( check_permission(lsm%f_grdara) )
  call traperr( check_permission(lsm%f_grdlon) )
  call traperr( check_permission(lsm%f_grdlat) )

  call traperr( check_file_size(rtmi%f%sidx) )
  call traperr( check_file_size(rtmi%f%tidx) )
  call traperr( check_file_size(rtmi%f%area) )
  call traperr( check_file_size(rtmi%f%coef) )

  call traperr( check_file_size(agcm%f_grdidx, allow_empty=.true.) )
  call traperr( check_file_size(agcm%f_grdara) )
  call traperr( check_file_size(agcm%f_grdlon) )
  call traperr( check_file_size(agcm%f_grdlat) )

  call traperr( check_file_size(lsm%f_grdidx, allow_empty=.true.) )
  call traperr( check_file_size(lsm%f_grdara) )
  call traperr( check_file_size(lsm%f_grdlon) )
  call traperr( check_file_size(lsm%f_grdlat) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking old files of output', PRCNAM, MODNAM)

  call traperr( set_opt_old_files(opt%sys%old_files) )

  call traperr( handle_old_file(rtmo%f%sidx) )
  call traperr( handle_old_file(rtmo%f%tidx) )
  call traperr( handle_old_file(rtmo%f%area) )
  call traperr( handle_old_file(rtmo%f%coef) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Preparing output directories', PRCNAM, MODNAM)

  call traperr( set_opt_mkdir(output=.true., hut=hut_command) )

  call traperr( mkdir(dirname(rtmo%f%sidx%path)) )
  call traperr( mkdir(dirname(rtmo%f%tidx%path)) )
  call traperr( mkdir(dirname(rtmo%f%area%path)) )
  call traperr( mkdir(dirname(rtmo%f%coef%path)) )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_paths
!===============================================================
!
!===============================================================
end module mod_set
