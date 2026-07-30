module mod_set
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
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
subroutine read_settings(rt_in, rt_out, agcm, rm, lsm)
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
        set_opt_log
  use c1_opt_set, only: &
        set_default_values_opt_sys, &
        set_default_values_opt_log
  use c2_rt_base, only: &
        init_rt                    , &
        set_default_values_rt      , &
        set_status_rt_main_file    , &
        set_action_rt_main_file    , &
        apply_oldfiles_rt_main_file
  use c2_rt_set, only: &
        init_opt_rt_coef
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings'
  type(rt_in_) , intent(out) :: rt_in
  type(rt_out_), intent(out) :: rt_out
  type(agcm_)  , intent(out) :: agcm
  type(rm_)    , intent(out) :: rm
  type(lsm_)   , intent(out) :: lsm

  type counter_
    integer :: input_rt_ogcm_ocean_to_agcm
    integer :: input_rt_ogcm_land_to_agcm
    integer :: input_rt_rm_river_to_agcm
    integer :: input_rt_rm_noriv_to_agcm
    integer :: input_rt_rm_ocean_to_agcm
    integer :: input_agcm
    integer :: input_rm
    integer :: output_opt_rt_coef
    integer :: output_rt_lsm_river_to_agcm
    integer :: output_rt_lsm_noriv_to_agcm
    integer :: output_rt_lsm_ocean_to_agcm
    integer :: output_rt_agcm_to_lsm_river
    integer :: output_rt_agcm_to_lsm_noriv
    integer :: output_rt_agcm_to_lsm_ocean
    integer :: output_agcm
    integer :: output_lsm
    integer :: options
  end type
  type(counter_) :: counter

  character(CLEN_VAR), parameter :: BLOCKNAME__IN_RT_OOCN2A    = 'input_rt_ogcm_ocean_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_RT_OLND2A    = 'input_rt_ogcm_land_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_RT_RRIV2A    = 'input_rt_rm_river_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_RT_RNRV2A    = 'input_rt_rm_noriv_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_RT_ROCN2A    = 'input_rt_rm_ocean_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_AGCM         = 'input_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__IN_RM           = 'input_rm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_OPT_RT_COEF = 'output_opt_rt_coef'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_RT_LRIV2A   = 'output_rt_lsm_river_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_RT_LNRV2A   = 'output_rt_lsm_noriv_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_RT_LOCN2A   = 'output_rt_lsm_ocean_to_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_RT_A2LRIV   = 'output_rt_agcm_to_lsm_river'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_RT_A2LNRV   = 'output_rt_agcm_to_lsm_noriv'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_RT_A2LOCN   = 'output_rt_agcm_to_lsm_ocean'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_AGCM        = 'output_agcm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OUT_LSM         = 'output_lsm'
  character(CLEN_VAR), parameter :: BLOCKNAME__OPT             = 'options'

  character(CLEN_VAR) :: blockName
  type(opt_rt_coef_) :: opt_rt_coef
  type(opt_) :: opt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Init.
  !-------------------------------------------------------------
  call logent('Initializing', PRCNAM, MODNAM)

  call set_default_values_opt_sys(opt%sys)
  call set_default_values_opt_log(opt%log)

  call traperr( init_rt(rt_in%ogcm_ocean_to_agcm) )
  call traperr( init_rt(rt_in%ogcm_land_to_agcm) )
  call traperr( init_rt(rt_in%rm_river_to_agcm) )
  call traperr( init_rt(rt_in%rm_noriv_to_agcm) )
  call traperr( init_rt(rt_in%rm_ocean_to_agcm) )

  call traperr( init_rt(rt_out%lsm_river_to_agcm) )
  call traperr( init_rt(rt_out%lsm_noriv_to_agcm) )
  call traperr( init_rt(rt_out%lsm_noriv_virt_to_agcm) )
  call traperr( init_rt(rt_out%lsm_ocean_to_agcm) )
  call traperr( init_rt(rt_out%agcm_to_lsm_river) )
  call traperr( init_rt(rt_out%agcm_to_lsm_noriv) )
  call traperr( init_rt(rt_out%agcm_to_lsm_ocean) )

  rt_in%ogcm_ocean_to_agcm%id = 'rt_in%ogcm_ocean_to_agcm'
  rt_in%ogcm_land_to_agcm%id  = 'rt_in%ogcm_land_to_agcm'
  rt_in%rm_river_to_agcm%id   = 'rt_in%rm_river_to_agcm'
  rt_in%rm_noriv_to_agcm%id   = 'rt_in%rm_noriv_to_agcm'
  rt_in%rm_ocean_to_agcm%id   = 'rt_in%rm_ocean_to_agcm'
  call traperr( set_default_values_rt(rt_in%ogcm_ocean_to_agcm, status=RT_STATUS__READ) )
  call traperr( set_default_values_rt(rt_in%ogcm_land_to_agcm , status=RT_STATUS__READ) )
  call traperr( set_default_values_rt(rt_in%rm_river_to_agcm  , status=RT_STATUS__READ) )
  call traperr( set_default_values_rt(rt_in%rm_noriv_to_agcm  , status=RT_STATUS__READ) )
  call traperr( set_default_values_rt(rt_in%rm_ocean_to_agcm  , status=RT_STATUS__READ) )

  rt_out%lsm_river_to_agcm%id      = 'rt_out%lsm_river_to_agcm'
  rt_out%lsm_noriv_to_agcm%id      = 'rt_out%lsm_noriv_to_agcm'
  rt_out%lsm_noriv_virt_to_agcm%id = 'rt_out%lsm_noriv_virt_to_agcm'
  rt_out%lsm_ocean_to_agcm%id      = 'rt_out%lsm_ocean_to_agcm'
  rt_out%agcm_to_lsm_river%id      = 'rt_out%agcm_to_lsm_river'
  rt_out%agcm_to_lsm_noriv%id      = 'rt_out%agcm_to_lsm_noriv'
  rt_out%agcm_to_lsm_ocean%id      = 'rt_out%agcm_to_lsm_ocean'
  call traperr( set_default_values_rt(rt_out%lsm_river_to_agcm     , status=RT_STATUS__MAKE) )
  call traperr( set_default_values_rt(rt_out%lsm_noriv_to_agcm     , status=RT_STATUS__MAKE) )
  call traperr( set_default_values_rt(rt_out%lsm_noriv_virt_to_agcm, status=RT_STATUS__MAKE) )
  call traperr( set_default_values_rt(rt_out%lsm_ocean_to_agcm     , status=RT_STATUS__MAKE) )
  call traperr( set_default_values_rt(rt_out%agcm_to_lsm_river     , status=RT_STATUS__MAKE) )
  call traperr( set_default_values_rt(rt_out%agcm_to_lsm_noriv     , status=RT_STATUS__MAKE) )
  call traperr( set_default_values_rt(rt_out%agcm_to_lsm_ocean     , status=RT_STATUS__MAKE) )

  call init_opt_rt_coef(opt_rt_coef)  ! modified on v2.4.3

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
    ! Case: input rt
    case( BLOCKNAME__IN_RT_OOCN2A )
      call update_counter(counter%input_rt_ogcm_ocean_to_agcm)
      call read_settings_input_rt(rt_in%ogcm_ocean_to_agcm)

    case( BLOCKNAME__IN_RT_OLND2A )
      call update_counter(counter%input_rt_ogcm_land_to_agcm)
      call read_settings_input_rt(rt_in%ogcm_land_to_agcm)

    case( BLOCKNAME__IN_RT_RRIV2A )
      call update_counter(counter%input_rt_rm_river_to_agcm)
      call read_settings_input_rt(rt_in%rm_river_to_agcm)

    case( BLOCKNAME__IN_RT_RNRV2A )
      call update_counter(counter%input_rt_rm_noriv_to_agcm)
      call read_settings_input_rt(rt_in%rm_noriv_to_agcm)

    case( BLOCKNAME__IN_RT_ROCN2A )
      call update_counter(counter%input_rt_rm_ocean_to_agcm)
      call read_settings_input_rt(rt_in%rm_ocean_to_agcm)

    case( BLOCKNAME__IN_AGCM )
      call update_counter(counter%input_agcm)
      call read_settings_input_agcm(agcm)

    case( BLOCKNAME__IN_RM )
      call update_counter(counter%input_rm)
      call read_settings_input_rm(rm)
    !-----------------------------------------------------------
    ! Case: opt_rt_coef
    case( BLOCKNAME__OUT_OPT_RT_COEF )
      call update_counter(counter%output_opt_rt_coef)
      call read_settings_rt_coef_options(opt_rt_coef)
    !-----------------------------------------------------------
    ! Case: output rt
    case( BLOCKNAME__OUT_RT_LRIV2A )
      call update_counter(counter%output_rt_lsm_river_to_agcm)
      call read_settings_output_rt(rt_out%lsm_river_to_agcm)

    case( BLOCKNAME__OUT_RT_LNRV2A )
      call update_counter(counter%output_rt_lsm_noriv_to_agcm)
      call read_settings_output_rt(rt_out%lsm_noriv_to_agcm)

    case( BLOCKNAME__OUT_RT_LOCN2A )
      call update_counter(counter%output_rt_lsm_ocean_to_agcm)
      call read_settings_output_rt(rt_out%lsm_ocean_to_agcm)

    case( BLOCKNAME__OUT_RT_A2LRIV )
      call update_counter(counter%output_rt_agcm_to_lsm_river)
      call read_settings_output_rt(rt_out%agcm_to_lsm_river)

    case( BLOCKNAME__OUT_RT_A2LNRV )
      call update_counter(counter%output_rt_agcm_to_lsm_noriv)
      call read_settings_output_rt(rt_out%agcm_to_lsm_noriv)

    case( BLOCKNAME__OUT_RT_A2LOCN )
      call update_counter(counter%output_rt_agcm_to_lsm_ocean)
      call read_settings_output_rt(rt_out%agcm_to_lsm_ocean)

    case( BLOCKNAME__OUT_AGCM )
      call update_counter(counter%output_agcm)
      call read_settings_output_agcm(agcm)

    case( BLOCKNAME__OUT_LSM )
      call update_counter(counter%output_lsm)
      call read_settings_output_lsm(lsm)

    case( BLOCKNAME__OPT )
      call update_counter(counter%options)
      call read_settings_opt(opt)
    !-----------------------------------------------------------
    ! Case: ERROR
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

  lsm%nij = rm%nij
  lsm%ncx = rm%ncx
  lsm%ncy = rm%ncy
  lsm%nkx = rm%nkx
  lsm%nky = rm%nky

  call traperr( apply_oldfiles_rt_main_file(rt_in%ogcm_ocean_to_agcm) )
  call traperr( apply_oldfiles_rt_main_file(rt_in%rm_ocean_to_agcm  ) )
  call traperr( apply_oldfiles_rt_main_file(rt_in%rm_river_to_agcm  ) )
  call traperr( apply_oldfiles_rt_main_file(rt_in%rm_noriv_to_agcm  ) )
  call traperr( apply_oldfiles_rt_main_file(rt_in%rm_ocean_to_agcm  ) )

  call traperr( apply_oldfiles_rt_main_file(rt_out%lsm_river_to_agcm     , opt%sys%old_files) )
  call traperr( apply_oldfiles_rt_main_file(rt_out%lsm_noriv_to_agcm     , opt%sys%old_files) )
  call traperr( apply_oldfiles_rt_main_file(rt_out%lsm_noriv_virt_to_agcm, opt%sys%old_files) )
  call traperr( apply_oldfiles_rt_main_file(rt_out%lsm_ocean_to_agcm     , opt%sys%old_files) )
  call traperr( apply_oldfiles_rt_main_file(rt_out%agcm_to_lsm_river     , opt%sys%old_files) )
  call traperr( apply_oldfiles_rt_main_file(rt_out%agcm_to_lsm_noriv     , opt%sys%old_files) )
  call traperr( apply_oldfiles_rt_main_file(rt_out%agcm_to_lsm_ocean     , opt%sys%old_files) )

  if( opt_rt_coef%is_sum_modify_enabled )then
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_modify(opt_rt_coef%sum_modify, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_sum_modify_ulim_enabled )then
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_modify_ulim(opt_rt_coef%sum_modify_ulim, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_zero_positive_enabled )then
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%lsm_river_to_agcm)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%agcm_to_lsm_river)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_zero_positive(opt_rt_coef%zero_positive, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_zero_negative_enabled )then
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%lsm_river_to_agcm)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%agcm_to_lsm_river)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_zero_negative(opt_rt_coef%zero_negative, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_error_excess_enabled )then
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%lsm_river_to_agcm)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%agcm_to_lsm_river)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_error_excess(opt_rt_coef%error_excess, rt_out%agcm_to_lsm_ocean)
  endif

  if( opt_rt_coef%is_sum_error_excess_enabled )then
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%lsm_river_to_agcm)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%lsm_noriv_to_agcm)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%lsm_ocean_to_agcm)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%agcm_to_lsm_river)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%agcm_to_lsm_noriv)
    call put_opt_coef_sum_error_excess(opt_rt_coef%sum_error_excess, rt_out%agcm_to_lsm_ocean)
  endif

  call set_opt_sys(opt%sys)
  call set_opt_log(opt%log)

  call logext()
  !-------------------------------------------------------------
  ! Print settings
  !-------------------------------------------------------------
  call echo_settings_input_rt(rt_in%ogcm_ocean_to_agcm)
  call echo_settings_input_rt(rt_in%ogcm_land_to_agcm)
  call echo_settings_input_rt(rt_in%rm_river_to_agcm)
  call echo_settings_input_rt(rt_in%rm_noriv_to_agcm)
  call echo_settings_input_rt(rt_in%rm_ocean_to_agcm)

  call echo_settings_input_agcm(agcm)
  call echo_settings_input_rm(rm)

  call echo_settings_output_rt(rt_out%lsm_river_to_agcm)
  call echo_settings_output_rt(rt_out%lsm_noriv_to_agcm)
  call echo_settings_output_rt(rt_out%lsm_ocean_to_agcm)
  call echo_settings_output_rt(rt_out%agcm_to_lsm_river)
  call echo_settings_output_rt(rt_out%agcm_to_lsm_noriv)
  call echo_settings_output_rt(rt_out%agcm_to_lsm_ocean)

  call echo_settings_output_agcm(agcm)
  call echo_settings_output_lsm(lsm)

  call logmsg(str(bar('')))
  !-------------------------------------------------------------
  ! Check paths
  !-------------------------------------------------------------
  call check_paths(rt_in, rt_out, agcm, rm, lsm, opt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine init_counter()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_counter'

  counter%input_rt_ogcm_ocean_to_agcm = 0
  counter%input_rt_ogcm_land_to_agcm = 0
  counter%input_rt_rm_river_to_agcm = 0
  counter%input_rt_rm_noriv_to_agcm = 0
  counter%input_rt_rm_ocean_to_agcm = 0
  counter%input_agcm = 0
  counter%input_rm   = 0
  counter%output_opt_rt_coef = 0
  counter%output_rt_lsm_river_to_agcm = 0
  counter%output_rt_lsm_noriv_to_agcm = 0
  counter%output_rt_lsm_ocean_to_agcm = 0
  counter%output_rt_agcm_to_lsm_river = 0
  counter%output_rt_agcm_to_lsm_noriv = 0
  counter%output_rt_agcm_to_lsm_ocean = 0
  counter%output_agcm = 0
  counter%output_lsm  = 0
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
         counter%input_rt_ogcm_ocean_to_agcm, &
         BLOCKNAME__IN_RT_OOCN2A, 0, 1)

  call check_num_of_key(&
         counter%input_rt_ogcm_land_to_agcm, &
         BLOCKNAME__IN_RT_OLND2A, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_river_to_agcm, &
         BLOCKNAME__IN_RT_RRIV2A, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_noriv_to_agcm, &
         BLOCKNAME__IN_RT_RNRV2A, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_ocean_to_agcm, &
         BLOCKNAME__IN_RT_ROCN2A, 0, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         BLOCKNAME__IN_AGCM, 0, 1)

  call check_num_of_key(&
         counter%input_rm, &
         BLOCKNAME__IN_RM, 0, 1)

  call check_num_of_key(&
         counter%output_opt_rt_coef, &
         BLOCKNAME__OUT_OPT_RT_COEF, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_river_to_agcm, &
         BLOCKNAME__OUT_RT_LRIV2A, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_noriv_to_agcm, &
         BLOCKNAME__OUT_RT_LNRV2A, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         BLOCKNAME__OUT_RT_LOCN2A, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         BLOCKNAME__OUT_RT_LOCN2A, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_river, &
         BLOCKNAME__OUT_RT_A2LRIV, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_noriv, &
         BLOCKNAME__OUT_RT_A2LNRV, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_ocean, &
         BLOCKNAME__OUT_RT_A2LOCN, 0, 1)

  call check_num_of_key(&
         counter%output_agcm, &
         BLOCKNAME__OUT_AGCM, 0, 1)

  call check_num_of_key(&
         counter%output_lsm, &
         BLOCKNAME__OUT_LSM, 0, 1)

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
         counter%input_rt_ogcm_ocean_to_agcm, &
         BLOCKNAME__IN_RT_OOCN2A, 1, 1)

  call check_num_of_key(&
         counter%input_rt_ogcm_land_to_agcm, &
         BLOCKNAME__IN_RT_OLND2A, 0, 1)

  call check_num_of_key(&
         counter%input_rt_rm_river_to_agcm, &
         BLOCKNAME__IN_RT_RRIV2A, 1, 1)

  call check_num_of_key(&
         counter%input_rt_rm_noriv_to_agcm, &
         BLOCKNAME__IN_RT_RNRV2A, 1, 1)

  call check_num_of_key(&
         counter%input_rt_rm_ocean_to_agcm, &
         BLOCKNAME__IN_RT_ROCN2A, 1, 1)

  call check_num_of_key(&
         counter%input_agcm, &
         BLOCKNAME__IN_AGCM, 1, 1)

  call check_num_of_key(&
         counter%input_rm, &
         BLOCKNAME__IN_RM, 1, 1)

  call check_num_of_key(&
         counter%output_opt_rt_coef, &
         BLOCKNAME__OUT_OPT_RT_COEF, 0, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_river_to_agcm, &
         BLOCKNAME__OUT_RT_LRIV2A, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_noriv_to_agcm, &
         BLOCKNAME__OUT_RT_LNRV2A, 1, 1)

  call check_num_of_key(&
         counter%output_rt_lsm_ocean_to_agcm, &
         BLOCKNAME__OUT_RT_LOCN2A, 0, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_river, &
         BLOCKNAME__OUT_RT_A2LRIV, 1, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_noriv, &
         BLOCKNAME__OUT_RT_A2LNRV, 1, 1)

  call check_num_of_key(&
         counter%output_rt_agcm_to_lsm_ocean, &
         BLOCKNAME__OUT_RT_A2LOCN, 0, 1)

  call check_num_of_key(&
         counter%output_agcm, &
         BLOCKNAME__OUT_AGCM, 1, 1)

  call check_num_of_key(&
         counter%output_lsm, &
         BLOCKNAME__OUT_LSM, 1, 1)

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
        set_default_values_rt_main
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

  call set_keynum('dir', 0, -1)
  call set_keynum('length', 1, 1)
  call set_keynum('f_sidx', 1, 1)
  call set_keynum('f_tidx', 1, 1)
  call set_keynum('f_area', 1, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  !call logent('Setting the default values', PRCNAM, MODNAM)

  rtm => rt%main

  !call logext()
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
    !
    case( 'length' )
      call read_value(rtm%nij)

    case( 'f_sidx' )
      call read_value(rtm%f%sidx, dir)

    case( 'f_tidx' )
      call read_value(rtm%f%tidx, dir)

    case( 'f_area' )
      call read_value(rtm%f%area, dir)
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

  rtm%f%sidx%length = rtm%nij
  rtm%f%tidx%length = rtm%nij
  rtm%f%area%length = rtm%nij

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variables
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

  call set_keynum('dir', 0, -1)
  call set_keynum('nij', 1, 1)
  call set_keynum('f_grdidx', 0, 1)
  call set_keynum('f_grdara', 1, 1)
  call set_keynum('idx_miss', 0, 1)

  call set_keynum('opt_thresh_lndfrc_noriv_virt_min'   , 0, 1)
  call set_keynum('opt_thresh_lndfrc_excess'           , 0, 1)
  call set_keynum('opt_thresh_lndfrc_noriv_virt_excess', 0, 1)
  call set_keynum('opt_thresh_lndfrc_zero'             , 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  agcm%nij = 0_8
  call set_file_default(dtype=DTYPE_INT4, action=ACTION_READ)
  agcm%fin_grdidx = file(id='agcm%fin_grdidx', dtype=DTYPE_INT4)
  agcm%fin_grdara = file(id='agcm%fin_grdara', dtype=DTYPE_DBLE)
  call reset_file_default()

  agcm%idx_miss = IDX_MISS_DEFAULT

  agcm%opt_thresh_lndfrc_noriv_virt_min    = AGCM_OPT_THRESH_LNDFRC_NORIV_VIRT_MIN_DEFAULT
  agcm%opt_thresh_lndfrc_excess            = AGCM_OPT_THRESH_LNDFRC_EXCESS_DEFAULT
  agcm%opt_thresh_lndfrc_noriv_virt_excess = AGCM_OPT_THRESH_LNDFRC_NORIV_VIRT_EXCESS_DEFAULT
  agcm%opt_thresh_lndfrc_zero              = AGCM_OPT_THRESH_LNDFRC_ZERO_DEFAULT

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
    case( 'dir' )
      call read_value(dir, is_path=.true.)

    case( 'nij' )
      call read_value(agcm%nij)

    case( 'f_grdidx' )
      call read_value(agcm%fin_grdidx, dir)

    case( 'f_grdara' )
      call read_value(agcm%fin_grdara, dir)

    case( 'idx_miss' )
      call read_value(agcm%idx_miss)
    !-----------------------------------------------------------
    ! Thresholds
    case( 'opt_thresh_lndfrc_noriv_virt_min' )
      call read_value(agcm%opt_thresh_lndfrc_noriv_virt_min)

    case( 'opt_thresh_lndfrc_excess' )
      call read_value(agcm%opt_thresh_lndfrc_excess)

    case( 'opt_thresh_lndfrc_noriv_virt_excess' )
      call read_value(agcm%opt_thresh_lndfrc_noriv_virt_excess)

    case( 'opt_thresh_lndfrc_zero' )
      call read_value(agcm%opt_thresh_lndfrc_zero)
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

  agcm%fin_grdidx%length = agcm%nij
  agcm%fin_grdara%length = agcm%nij

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_input_rm(rm)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_input_rm'
  type(rm_), intent(inout) :: rm

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('dir', 0, -1)

  call set_keynum('nx_grid', 1, 1)
  call set_keynum('ny_grid', 1, 1)
  call set_keynum('nx_raster', 1, 1)
  call set_keynum('ny_raster', 1, 1)

  call set_keynum('f_grdidx_river', 1, 1)
  call set_keynum('f_grdidx_noriv', 1, 1)
  call set_keynum('f_grdidx_ocean', 1, 1)
  call set_keynum('f_grdara_river', 1, 1)
  call set_keynum('f_grdara_noriv', 1, 1)
  call set_keynum('f_grdara_ocean', 1, 1)
  call set_keynum('f_rstidx_river', 1, 1)
  call set_keynum('f_rstidx_noriv', 1, 1)
  call set_keynum('f_rstidx_ocean', 1, 1)

  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  rm%ncx = 0_8
  rm%ncy = 0_8
  rm%nkx = 0_8
  rm%nky = 0_8

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_READ)
  rm%fin_grdidx_river = file(id='rm%fin_grdidx_river', dtype=DTYPE_INT4)
  rm%fin_grdidx_noriv = file(id='rm%fin_grdidx_noriv', dtype=DTYPE_INT4)
  rm%fin_grdidx_ocean = file(id='rm%fin_grdidx_ocean', dtype=DTYPE_INT4)

  rm%fin_grdara_river = file(id='rm%fin_grdara_river', dtype=DTYPE_DBLE)
  rm%fin_grdara_noriv = file(id='rm%fin_grdara_noriv', dtype=DTYPE_DBLE)
  rm%fin_grdara_ocean = file(id='rm%fin_grdara_ocean', dtype=DTYPE_DBLE)

  rm%fin_rstidx_river = file(id='rm%fin_rstidx_river', dtype=DTYPE_INT4)
  rm%fin_rstidx_noriv = file(id='rm%fin_rstidx_noriv', dtype=DTYPE_INT4)
  rm%fin_rstidx_ocean = file(id='rm%fin_rstidx_ocean', dtype=DTYPE_INT4)
  call reset_file_default()

  rm%idx_miss = IDX_MISS_DEFAULT
  rm%ara_miss = ARA_MISS_DEFAULT

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
    case( 'nx_grid' )
      call read_value(rm%ncx)
    case( 'ny_grid' )
      call read_value(rm%ncy)

    case( 'nx_raster' )
      call read_value(rm%nkx)
    case( 'ny_raster' )
      call read_value(rm%nky)
    !-----------------------------------------------------------
    ! Parent directory
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    ! Grid index data
    case( 'f_grdidx_river' )
      call read_value(rm%fin_grdidx_river, dir)

    case( 'f_grdidx_noriv' )
      call read_value(rm%fin_grdidx_noriv, dir)

    case( 'f_grdidx_ocean' )
      call read_value(rm%fin_grdidx_ocean, dir)
    !-----------------------------------------------------------
    ! Grid area data
    case( 'f_grdara_river' )
      call read_value(rm%fin_grdara_river, dir)

    case( 'f_grdara_noriv' )
      call read_value(rm%fin_grdara_noriv, dir)

    case( 'f_grdara_ocean' )
      call read_value(rm%fin_grdara_ocean, dir)
    !-----------------------------------------------------------
    ! Raster index data
    case( 'f_rstidx_river' )
      call read_value(rm%fin_rstidx_river, dir)

    case( 'f_rstidx_noriv' )
      call read_value(rm%fin_rstidx_noriv, dir)

    case( 'f_rstidx_ocean' )
      call read_value(rm%fin_rstidx_ocean, dir)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(rm%idx_miss)

    case( 'ara_miss' )
      call read_value(rm%ara_miss)
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

  rm%nij = rm%ncx * rm%ncy

  rm%fin_grdidx_river%length = rm%nij
  rm%fin_grdidx_noriv%length = rm%nij
  rm%fin_grdidx_ocean%length = rm%nij

  rm%fin_grdara_river%length = rm%nij
  rm%fin_grdara_noriv%length = rm%nij
  rm%fin_grdara_ocean%length = rm%nij

  rm%fin_rstidx_river%length = rm%nkx * rm%nky
  rm%fin_rstidx_noriv%length = rm%nkx * rm%nky
  rm%fin_rstidx_ocean%length = rm%nkx * rm%nky

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variables
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_input_rm
!===============================================================
!
!===============================================================
subroutine read_settings_rt_coef_options(opt_coef)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_rt_coef_options'
  type(opt_rt_coef_), intent(inout) :: opt_coef

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('opt_coef_sum_modify'      , 0, 1)
  call set_keynum('opt_coef_sum_modify_ulim' , 0, 1)
  call set_keynum('opt_coef_zero_positive'   , 0, 1)
  call set_keynum('opt_coef_zero_negative'   , 0, 1)
  call set_keynum('opt_coef_error_excess'    , 0, 1)
  call set_keynum('opt_coef_sum_error_excess', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------

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
    !
    case( 'opt_coef_sum_modify' )
      call read_value(opt_coef%sum_modify)
      opt_coef%is_sum_modify_enabled = .true.

    case( 'opt_coef_sum_modify_ulim' )
      call read_value(opt_coef%sum_modify_ulim)
      opt_coef%is_sum_modify_ulim_enabled = .true.

    case( 'opt_coef_zero_positive' )
      call read_value(opt_coef%zero_positive)
      opt_coef%is_zero_positive_enabled = .true.

    case( 'opt_coef_zero_negative' )
      call read_value(opt_coef%zero_negative)
      opt_coef%is_zero_negative_enabled = .true.

    case( 'opt_coef_error_excess' )
      call read_value(opt_coef%error_excess)
      opt_coef%is_error_excess_enabled = .true.

    case( 'opt_coef_sum_error_excess' )
      call read_value(opt_coef%sum_error_excess)
      opt_coef%is_sum_error_excess_enabled = .true.
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_rt_coef_options
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
        set_default_values_rt_main
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_output_rt'
  type(rt_), intent(inout), target :: rt

  type(rt_main_), pointer :: rtm

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('dir', 0, -1)
  call set_keynum('f_sidx', 1, 1)
  call set_keynum('f_tidx', 1, 1)
  call set_keynum('f_area', 1, 1)
  call set_keynum('f_coef', 1, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  !call logent('Setting the default values', PRCNAM, MODNAM)

  rtm => rt%main

  !call logext()
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
    case( 'dir' )
      call read_value(dir, is_path=.true.)
    !-----------------------------------------------------------
    !
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
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_output_rt
!===============================================================
!
!===============================================================
subroutine read_settings_output_agcm(agcm)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_output_agcm'
  type(agcm_), intent(inout), target :: agcm

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()
  call set_keynum('dir', 0, -1)
  call set_keynum('f_lndara_ogcm'      , 0, 1)
  call set_keynum('f_lndara_river'     , 0, 1)
  call set_keynum('f_lndara_noriv_real', 0, 1)
  call set_keynum('f_lndara_noriv_virt', 0, 1)
  call set_keynum('f_lndara_noriv'     , 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting the default values', PRCNAM, MODNAM)

  call set_file_default(dtype=DTYPE_DBLE, action=ACTION_WRITE)
  agcm%fout_lndara_ogcm       = file(id='agcm%fout_lndara_ogcm')
  agcm%fout_lndara_river      = file(id='agcm%fout_lndara_river')
  agcm%fout_lndara_noriv_real = file(id='agcm%fout_lndara_noriv_real')
  agcm%fout_lndara_noriv_virt = file(id='agcm%fout_lndara_noriv_virt')
  agcm%fout_lndara_noriv      = file(id='agcm%fout_lndara_noriv')
  call reset_file_default()

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
    !
    case( 'f_lndara_ogcm' )
      call read_value(agcm%fout_lndara_ogcm, dir)
    case( 'f_lndara_river' )
      call read_value(agcm%fout_lndara_river, dir)
    case( 'f_lndara_noriv_real' )
      call read_value(agcm%fout_lndara_noriv_real, dir)
    case( 'f_lndara_noriv_virt' )
      call read_value(agcm%fout_lndara_noriv_virt, dir)
    case( 'f_lndara_noriv' )
      call read_value(agcm%fout_lndara_noriv, dir)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_output_agcm
!===============================================================
!
!===============================================================
subroutine read_settings_output_lsm(lsm)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'read_settings_output_lsm'
  type(lsm_), intent(inout), target :: lsm

  character(CLEN_PATH) :: dir

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set the lim. of the number of times each keyword is used
  !-------------------------------------------------------------
  call logent('Setting the lim. of the number of times each keyword is used', PRCNAM, MODNAM)

  call alloc_keynum()

  call set_keynum('dir', 0, -1)

  call set_keynum('f_grdmsk_river'     , 0, 1)
  call set_keynum('f_grdmsk_noriv'     , 0, 1)
  call set_keynum('f_grdmsk_noriv_real', 0, 1)
  call set_keynum('f_grdmsk_noriv_virt', 0, 1)
  call set_keynum('f_grdmsk_ocean'     , 0, 1)
  call set_keynum('f_grdidx_river'     , 0, 1)
  call set_keynum('f_grdidx_noriv'     , 0, 1)
  call set_keynum('f_grdidx_noriv_real', 0, 1)
  call set_keynum('f_grdidx_noriv_virt', 0, 1)
  call set_keynum('f_grdidx_ocean'         , 0, 1)
  call set_keynum('f_grdidx_bnd_river'     , 0, 1)
  call set_keynum('f_grdidx_bnd_noriv'     , 0, 1)
  call set_keynum('f_grdidx_bnd_noriv_real', 0, 1)
  call set_keynum('f_grdidx_bnd_noriv_virt', 0, 1)
  call set_keynum('f_grdara_river'     , 0, 1)
  call set_keynum('f_grdara_noriv'     , 0, 1)
  call set_keynum('f_grdara_noriv_real', 0, 1)
  call set_keynum('f_grdara_noriv_virt', 0, 1)
  call set_keynum('f_grdara_ocean'     , 0, 1)
  call set_keynum('f_grdwgt_river'     , 0, 1)
  call set_keynum('f_grdwgt_noriv'     , 0, 1)
  call set_keynum('f_grdwgt_noriv_real', 0, 1)
  call set_keynum('f_grdwgt_noriv_virt', 0, 1)
  call set_keynum('f_grdwgt_ocean'     , 0, 1)

  call set_keynum('f_rstidx_river'     , 0, 1)
  call set_keynum('f_rstidx_noriv'     , 0, 1)
  call set_keynum('f_rstidx_noriv_real', 0, 1)
  call set_keynum('f_rstidx_noriv_virt', 0, 1)
  call set_keynum('f_rstidx_ocean'     , 0, 1)
  call set_keynum('f_rstidx_bnd_river'     , 0, 1)
  call set_keynum('f_rstidx_bnd_noriv'     , 0, 1)
  call set_keynum('f_rstidx_bnd_noriv_real', 0, 1)
  call set_keynum('f_rstidx_bnd_noriv_virt', 0, 1)

  call set_keynum('idx_miss', 0, 1)
  call set_keynum('ara_miss', 0, 1)
  call set_keynum('wgt_miss', 0, 1)

  call set_keynum('opt_thresh_grdwgt_noriv_virt_excess', 0, 1)

  call logext()
  !-------------------------------------------------------------
  ! Set the default values
  !-------------------------------------------------------------
  call logent('Setting default values', PRCNAM, MODNAM)

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_WRITE)
  lsm%fout_grdmsk_river      = file(id='lsm%fout_grdmsk_river')
  lsm%fout_grdmsk_noriv      = file(id='lsm%fout_grdmsk_noriv')
  lsm%fout_grdmsk_noriv_real = file(id='lsm%fout_grdmsk_noriv_real')
  lsm%fout_grdmsk_noriv_virt = file(id='lsm%fout_grdmsk_noriv_virt')
  lsm%fout_grdmsk_ocean      = file(id='lsm%fout_grdmsk_ocean')

  lsm%fout_grdidx_river      = file(id='lsm%fout_grdidx_river')
  lsm%fout_grdidx_noriv      = file(id='lsm%fout_grdidx_noriv')
  lsm%fout_grdidx_noriv_real = file(id='lsm%fout_grdidx_noriv_real')
  lsm%fout_grdidx_noriv_virt = file(id='lsm%fout_grdidx_noriv_virt')
  lsm%fout_grdidx_ocean      = file(id='lsm%fout_grdidx_ocean')

  lsm%fout_grdidx_bnd_river      = file(id='lsm%fout_grdidx_river')
  lsm%fout_grdidx_bnd_noriv      = file(id='lsm%fout_grdidx_noriv')
  lsm%fout_grdidx_bnd_noriv_real = file(id='lsm%fout_grdidx_noriv_real')
  lsm%fout_grdidx_bnd_noriv_virt = file(id='lsm%fout_grdidx_noriv_virt')

  call set_file_default(dtype=DTYPE_DBLE, action=ACTION_WRITE)
  lsm%fout_grdara_river      = file(id='lsm%fout_grdara_river')
  lsm%fout_grdara_noriv      = file(id='lsm%fout_grdara_noriv')
  lsm%fout_grdara_noriv_real = file(id='lsm%fout_grdara_noriv_real')
  lsm%fout_grdara_noriv_virt = file(id='lsm%fout_grdara_noriv_virt')
  lsm%fout_grdara_ocean      = file(id='lsm%fout_grdara_ocean')

  lsm%fout_grdwgt_river      = file(id='lsm%fout_grdwgt_river')
  lsm%fout_grdwgt_noriv      = file(id='lsm%fout_grdwgt_noriv')
  lsm%fout_grdwgt_noriv_real = file(id='lsm%fout_grdwgt_noriv_real')
  lsm%fout_grdwgt_noriv_virt = file(id='lsm%fout_grdwgt_noriv_virt')
  lsm%fout_grdwgt_ocean      = file(id='lsm%fout_grdwgt_ocean')

  call set_file_default(dtype=DTYPE_INT4, action=ACTION_WRITE)
  lsm%fout_rstidx_river      = file(id='lsm%fout_rstidx_river')
  lsm%fout_rstidx_noriv      = file(id='lsm%fout_rstidx_noriv')
  lsm%fout_rstidx_noriv_real = file(id='lsm%fout_rstidx_noriv_real')
  lsm%fout_rstidx_noriv_virt = file(id='lsm%fout_rstidx_noriv_virt')
  lsm%fout_rstidx_ocean      = file(id='lsm%fout_rstidx_ocean')

  lsm%fout_rstidx_bnd_river      = file(id='lsm%fout_rstidx_bnd_river')
  lsm%fout_rstidx_bnd_noriv      = file(id='lsm%fout_rstidx_bnd_noriv')
  lsm%fout_rstidx_bnd_noriv_real = file(id='lsm%fout_rstidx_bnd_noriv_real')
  lsm%fout_rstidx_bnd_noriv_virt = file(id='lsm%fout_rstidx_bnd_noriv_virt')
  call reset_file_default()

  lsm%idx_miss = IDX_MISS_DEFAULT
  lsm%ara_miss = ARA_MISS_DEFAULT
  lsm%wgt_miss = WGT_MISS_DEFAULT

  lsm%opt_thresh_grdwgt_noriv_virt_excess = LSM_OPT_THRESH_GRDWGT_NORIV_VIRT_EXCESS_DEFAULT

  call reset_file_default()

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
    ! Grid mask
    !-----------------------------------------------------------
    case( 'f_grdmsk_river' )
      call read_value(lsm%fout_grdmsk_river, dir)
    case( 'f_grdmsk_noriv' )
      call read_value(lsm%fout_grdmsk_noriv, dir)
    case( 'f_grdmsk_noriv_real' )
      call read_value(lsm%fout_grdmsk_noriv_real, dir)
    case( 'f_grdmsk_noriv_virt' )
      call read_value(lsm%fout_grdmsk_noriv_virt, dir)
    case( 'f_grdmsk_ocean' )
      call read_value(lsm%fout_grdmsk_ocean, dir)
    !-----------------------------------------------------------
    ! Grid index
    !-----------------------------------------------------------
    case( 'f_grdidx_river' )
      call read_value(lsm%fout_grdidx_river, dir)
    case( 'f_grdidx_noriv' )
      call read_value(lsm%fout_grdidx_noriv, dir)
    case( 'f_grdidx_noriv_real' )
      call read_value(lsm%fout_grdidx_noriv_real, dir)
    case( 'f_grdidx_noriv_virt' )
      call read_value(lsm%fout_grdidx_noriv_virt, dir)
    case( 'f_grdidx_ocean' )
      call read_value(lsm%fout_grdidx_ocean, dir)
    !-----------------------------------------------------------
    ! Grid index (bnd)
    !-----------------------------------------------------------
    case( 'f_grdidx_bnd_river' )
      call read_value(lsm%fout_grdidx_bnd_river, dir)
    case( 'f_grdidx_bnd_noriv' )
      call read_value(lsm%fout_grdidx_bnd_noriv, dir)
    case( 'f_grdidx_bnd_noriv_real' )
      call read_value(lsm%fout_grdidx_bnd_noriv_real, dir)
    case( 'f_grdidx_bnd_noriv_virt' )
      call read_value(lsm%fout_grdidx_bnd_noriv_virt, dir)
    !-----------------------------------------------------------
    ! Grid area
    !-----------------------------------------------------------
    case( 'f_grdara_river' )
      call read_value(lsm%fout_grdara_river, dir)
    case( 'f_grdara_noriv' )
      call read_value(lsm%fout_grdara_noriv, dir)
    case( 'f_grdara_noriv_real' )
      call read_value(lsm%fout_grdara_noriv_real, dir)
    case( 'f_grdara_noriv_virt' )
      call read_value(lsm%fout_grdara_noriv_virt, dir)
    case( 'f_grdara_ocean' )
      call read_value(lsm%fout_grdara_ocean, dir)
    !-----------------------------------------------------------
    ! Grid weight
    !-----------------------------------------------------------
    case( 'f_grdwgt_river' )
      call read_value(lsm%fout_grdwgt_river, dir)
    case( 'f_grdwgt_noriv' )
      call read_value(lsm%fout_grdwgt_noriv, dir)
    case( 'f_grdwgt_noriv_real' )
      call read_value(lsm%fout_grdwgt_noriv_real, dir)
    case( 'f_grdwgt_noriv_virt' )
      call read_value(lsm%fout_grdwgt_noriv_virt, dir)
    case( 'f_grdwgt_ocean' )
      call read_value(lsm%fout_grdwgt_ocean, dir)
    !-----------------------------------------------------------
    ! Raster index
    !-----------------------------------------------------------
    case( 'f_rstidx_river' )
      call read_value(lsm%fout_rstidx_river, dir)
    case( 'f_rstidx_noriv' )
      call read_value(lsm%fout_rstidx_noriv, dir)
    case( 'f_rstidx_noriv_real' )
      call read_value(lsm%fout_rstidx_noriv_real, dir)
    case( 'f_rstidx_noriv_virt' )
      call read_value(lsm%fout_rstidx_noriv_virt, dir)
    case( 'f_rstidx_ocean' )
      call read_value(lsm%fout_rstidx_ocean, dir)
    !-----------------------------------------------------------
    ! Raster index (bnd)
    !-----------------------------------------------------------
    case( 'f_rstidx_bnd_river' )
      call read_value(lsm%fout_rstidx_bnd_river, dir)
    case( 'f_rstidx_bnd_noriv' )
      call read_value(lsm%fout_rstidx_bnd_noriv, dir)
    case( 'f_rstidx_bnd_noriv_real' )
      call read_value(lsm%fout_rstidx_bnd_noriv_real, dir)
    case( 'f_rstidx_bnd_noriv_virt' )
      call read_value(lsm%fout_rstidx_bnd_noriv_virt, dir)
    !-----------------------------------------------------------
    ! Missing values
    case( 'idx_miss' )
      call read_value(lsm%idx_miss)
    case( 'ara_miss' )
      call read_value(lsm%ara_miss)
    case( 'wgt_miss' )
      call read_value(lsm%wgt_miss)
    !-----------------------------------------------------------
    ! Option
    case( 'opt_thresh_grdwgt_noriv_virt_excess' )
      call read_value(lsm%opt_thresh_grdwgt_noriv_virt_excess)
    !-----------------------------------------------------------
    ! ERROR
    case default
      call raise_error_invalid_key()
    endselect
  enddo

  call check_keynum()

  call logext()
  !-------------------------------------------------------------
  ! Free the external module variable
  !-------------------------------------------------------------
  call free_keynum()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_settings_output_lsm
!===============================================================
!
!===============================================================
subroutine read_settings_opt(opt)
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
        KEY_MEMORY_ULIM
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

  selectcase( opt%sys%old_files )
  case( OPT_OLD_FILES_STOP, &
        OPT_OLD_FILES_REMOVE, &
        OPT_OLD_FILES_OVERWRITE )
    continue
  case default
    call errend('Invalid value in opt%sys%old_files: '//str(opt%sys%old_files)//&
            '\nCheck the value of "old_files".')
  endselect

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
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Input of remapping tables')))
  !-------------------------------------------------------------
  rtm => rt%main

  call logmsg('id: '//str(rt%id))
  call logmsg('length: '//str(rtm%nij))
  call logmsg('sidx: '//str(fileinfo(rtm%f%sidx)))
  call logmsg('tidx: '//str(fileinfo(rtm%f%tidx)))
  call logmsg('area: '//str(fileinfo(rtm%f%area)))
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
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Input of AGCM')))
  !-------------------------------------------------------------
  call logmsg('Mesh')
  call logmsg('  nij: '//str(agcm%nij))

  call logmsg('Grid data')
  call logmsg('  Index: '//str(fileinfo(agcm%fin_grdidx)))
  call logmsg('  Area : '//str(fileinfo(agcm%fin_grdara)))

  call logmsg('Index for missing grid: '//str(agcm%idx_miss))

  call logmsg('Options of land fraction')
  call logmsg('  Min. of land fraction (noriv-virt): '//&
            str(agcm%opt_thresh_lndfrc_noriv_virt_min))
  call logmsg('  Stop if land fraction exceeded 1.0 + '//&
            str(agcm%opt_thresh_lndfrc_excess))
  call logmsg('  Stop if land fraction (noriv-virt) exceeded 1.0 + '//&
            str(agcm%opt_thresh_lndfrc_noriv_virt_excess))
  call logmsg('  Land fraction less than this value is regarded as zero:'//&
            str(agcm%opt_thresh_lndfrc_zero))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_input_agcm
!===============================================================
!
!===============================================================
subroutine echo_settings_input_rm(rm)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_input_rm'
  type(rm_), intent(in) :: rm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Input of river model')))
  !-------------------------------------------------------------
  call logmsg('Mesh')
  call logmsg('  nx_grid: '//str(rm%ncx))
  call logmsg('  ny_grid: '//str(rm%ncy))
  call logmsg('  nx_raster: '//str(rm%nkx))
  call logmsg('  ny_raster: '//str(rm%nky))

  call logmsg('Grid data')
  call logmsg('  Area')
  call logmsg('    river: '//str(fileinfo(rm%fin_grdara_river)))
  call logmsg('    noriv: '//str(fileinfo(rm%fin_grdara_noriv)))
  call logmsg('    ocean: '//str(fileinfo(rm%fin_grdara_ocean)))

  call logmsg('Raster data')
  call logmsg('  Index')
  call logmsg('    river: '//str(fileinfo(rm%fin_rstidx_river)))
  call logmsg('    noriv: '//str(fileinfo(rm%fin_rstidx_noriv)))
  call logmsg('    ocean: '//str(fileinfo(rm%fin_rstidx_ocean)))

  call logmsg('Missing values')
  call logmsg('  Index : '//str(rm%idx_miss))
  call logmsg('  Area  : '//str(rm%ara_miss))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_input_rm
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
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Output of remapping tables')))
  !-------------------------------------------------------------
  rtm => rt%main

  call logmsg('id: '//str(rt%id))
  call logmsg('mesh_coef: '//str(rtm%mesh_coef))
  call logmsg('mesh_sort: '//str(rtm%mesh_sort))
  call logmsg('sidx: '//str(fileinfo(rtm%f%sidx)))
  call logmsg('tidx: '//str(fileinfo(rtm%f%tidx)))
  call logmsg('area: '//str(fileinfo(rtm%f%area)))
  call logmsg('coef: '//str(fileinfo(rtm%f%coef)))
  call echo_settings_opt_rt_coef(rtm%opt_coef,0)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_output_rt
!===============================================================
!
!===============================================================
subroutine echo_settings_output_agcm(agcm)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_output_agcm'
  type(agcm_), intent(in) :: agcm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Output of AGCM')))
  !-------------------------------------------------------------
  call logmsg('Land area')
  call logmsg('  ogcm      : '//str(fileinfo(agcm%fout_lndara_ogcm)))
  call logmsg('  river     : '//str(fileinfo(agcm%fout_lndara_river)))
  call logmsg('  noriv_real: '//str(fileinfo(agcm%fout_lndara_noriv_real)))
  call logmsg('  noriv_virt: '//str(fileinfo(agcm%fout_lndara_noriv_virt)))
  call logmsg('  noriv     : '//str(fileinfo(agcm%fout_lndara_noriv)))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_output_agcm
!===============================================================
!
!===============================================================
subroutine echo_settings_output_lsm(lsm)
  use c1_set, only: &
        bar
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_output_lsm'
  type(lsm_), intent(in) :: lsm

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(str(bar('Output of LSM')))
  !-------------------------------------------------------------
  call logmsg('Grid data')

  call logmsg('  Land mask')
  if( lsm%fout_grdmsk_river%path == '' .and. &
      lsm%fout_grdmsk_noriv%path == '' .and. &
      lsm%fout_grdmsk_noriv_real%path == '' .and. &
      lsm%fout_grdmsk_noriv_virt%path == '' .and. &
      lsm%fout_grdmsk_ocean%path == '' )then
    call logmsg('    (Not specified)')
  else
    call logmsg('    river     : '//str(fileinfo(lsm%fout_grdmsk_river)))
    call logmsg('    noriv     : '//str(fileinfo(lsm%fout_grdmsk_noriv)))
    call logmsg('    noriv_real: '//str(fileinfo(lsm%fout_grdmsk_noriv_real)))
    call logmsg('    noriv_virt: '//str(fileinfo(lsm%fout_grdmsk_noriv_virt)))
    call logmsg('    ocean     : '//str(fileinfo(lsm%fout_grdmsk_ocean)))
  endif

  call logmsg('  Index')
  if( lsm%fout_grdidx_river%path == '' .and. &
      lsm%fout_grdidx_noriv%path == '' .and. &
      lsm%fout_grdidx_noriv_real%path == '' .and. &
      lsm%fout_grdidx_noriv_virt%path == '' .and. &
      lsm%fout_grdidx_ocean%path == '' )then
    call logmsg('    (Not specified)')
  else
    call logmsg('    river     : '//str(fileinfo(lsm%fout_grdidx_river)))
    call logmsg('    noriv     : '//str(fileinfo(lsm%fout_grdidx_noriv)))
    call logmsg('    noriv_real: '//str(fileinfo(lsm%fout_grdidx_noriv_real)))
    call logmsg('    noriv_virt: '//str(fileinfo(lsm%fout_grdidx_noriv_virt)))
    call logmsg('    ocean     : '//str(fileinfo(lsm%fout_grdidx_ocean)))
  endif

  call logmsg('  Index for bnd.')
  if( lsm%fout_grdidx_bnd_river%path == '' .and. &
      lsm%fout_grdidx_bnd_noriv%path == '' .and. &
      lsm%fout_grdidx_bnd_noriv_real%path == '' .and. &
      lsm%fout_grdidx_bnd_noriv_virt%path == '' )then
    call logmsg('    (Not specified)')
  else
    call logmsg('    river     : '//str(fileinfo(lsm%fout_grdidx_bnd_river)))
    call logmsg('    noriv     : '//str(fileinfo(lsm%fout_grdidx_bnd_noriv)))
    call logmsg('    noriv_real: '//str(fileinfo(lsm%fout_grdidx_bnd_noriv_real)))
    call logmsg('    noriv_virt: '//str(fileinfo(lsm%fout_grdidx_bnd_noriv_virt)))
  endif

  call logmsg('  Area')
  if( lsm%fout_grdara_river%path == '' .and. &
      lsm%fout_grdara_noriv%path == '' .and. &
      lsm%fout_grdara_noriv_real%path == '' .and. &
      lsm%fout_grdara_noriv_virt%path == '' .and. &
      lsm%fout_grdara_ocean%path == '' )then
    call logmsg('    (Not specified)')
  else
    call logmsg('    river     : '//str(fileinfo(lsm%fout_grdara_river)))
    call logmsg('    noriv     : '//str(fileinfo(lsm%fout_grdara_noriv)))
    call logmsg('    noriv_real: '//str(fileinfo(lsm%fout_grdara_noriv_real)))
    call logmsg('    noriv_virt: '//str(fileinfo(lsm%fout_grdara_noriv_virt)))
    call logmsg('    ocean     : '//str(fileinfo(lsm%fout_grdara_ocean)))
  endif

  call logmsg('  Weight')
  if( lsm%fout_grdwgt_river%path == '' .and. &
      lsm%fout_grdwgt_noriv%path == '' .and. &
      lsm%fout_grdwgt_noriv_real%path == '' .and. &
      lsm%fout_grdwgt_noriv_virt%path == '' .and. &
      lsm%fout_grdwgt_ocean%path == '' )then
    call logmsg('    (Not specified)')
  else
    call logmsg('    river     : '//str(fileinfo(lsm%fout_grdwgt_river)))
    call logmsg('    noriv     : '//str(fileinfo(lsm%fout_grdwgt_noriv)))
    call logmsg('    noriv_real: '//str(fileinfo(lsm%fout_grdwgt_noriv_real)))
    call logmsg('    noriv_virt: '//str(fileinfo(lsm%fout_grdwgt_noriv_virt)))
    call logmsg('    ocean     : '//str(fileinfo(lsm%fout_grdwgt_ocean)))
  endif

  call logmsg('Raster data')

  call logmsg('  Index')
  if( lsm%fout_rstidx_river%path == '' .and. &
      lsm%fout_rstidx_noriv%path == '' .and. &
      lsm%fout_rstidx_noriv_real%path == '' .and. &
      lsm%fout_rstidx_noriv_virt%path == '' .and. &
      lsm%fout_rstidx_ocean%path == '' )then
    call logmsg('    (Not specified)')
  else
    call logmsg('    river     : '//str(fileinfo(lsm%fout_rstidx_river)))
    call logmsg('    noriv     : '//str(fileinfo(lsm%fout_rstidx_noriv)))
    call logmsg('    noriv_real: '//str(fileinfo(lsm%fout_rstidx_noriv_real)))
    call logmsg('    noriv_virt: '//str(fileinfo(lsm%fout_rstidx_noriv_virt)))
    call logmsg('    ocean     : '//str(fileinfo(lsm%fout_rstidx_ocean)))
  endif

  call logmsg('  Index for bnd.')
  if( lsm%fout_rstidx_bnd_river%path == '' .and. &
      lsm%fout_rstidx_bnd_noriv%path == '' .and. &
      lsm%fout_rstidx_bnd_noriv_real%path == '' .and. &
      lsm%fout_rstidx_bnd_noriv_virt%path == '' )then
    call logmsg('    (Not specified)')
  else
    call logmsg('    river     : '//str(fileinfo(lsm%fout_rstidx_bnd_river)))
    call logmsg('    noriv     : '//str(fileinfo(lsm%fout_rstidx_bnd_noriv)))
    call logmsg('    noriv_real: '//str(fileinfo(lsm%fout_rstidx_bnd_noriv_real)))
    call logmsg('    noriv_virt: '//str(fileinfo(lsm%fout_rstidx_bnd_noriv_virt)))
  endif

  call logmsg('Missing values')
  call logmsg('  Index : '//str(lsm%idx_miss))
  call logmsg('  Area  : '//str(lsm%ara_miss))
  call logmsg('  Weight: '//str(lsm%wgt_miss))

  call logmsg('Options')
  call logmsg('  Stop if grid area fraction (noriv-virt) exceeded 1.d0 + '//&
            str(lsm%opt_thresh_grdwgt_noriv_virt_excess))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_output_lsm
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
subroutine check_paths(rt_in, rt_out, agcm, rm, lsm, opt)
  use c1_file, only: &
        set_opt_old_files, &
        handle_old_file
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_paths'
  type(rt_in_) , intent(in), target :: rt_in
  type(rt_out_), intent(in), target :: rt_out
  type(agcm_)  , intent(in), target :: agcm
  type(rm_)    , intent(in), target :: rm
  type(lsm_)   , intent(in), target :: lsm
  type(opt_)   , intent(in)         :: opt

  type(rt_main_), pointer :: rtmi_oo_a  ! ogcm_ocean_to_agcm
  type(rt_main_), pointer :: rtmi_ol_a  ! ogcm_land_to_agcm
  type(rt_main_), pointer :: rtmi_rr_a ! rm_river_to_agcm
  type(rt_main_), pointer :: rtmi_rn_a ! rm_noriv_to_agcm
  type(rt_main_), pointer :: rtmi_ro_a ! rm_ocean_to_agcm
  type(rt_main_), pointer :: rtmo_lr_a ! lsm_river_to_agcm
  type(rt_main_), pointer :: rtmo_ln_a ! lsm_noriv_to_agcm
  type(rt_main_), pointer :: rtmo_lo_a ! lsm_ocean_to_agcm
  type(rt_main_), pointer :: rtmo_a_lr ! agcm_to_lsm_river
  type(rt_main_), pointer :: rtmo_a_ln ! agcm_to_lsm_noriv
  type(rt_main_), pointer :: rtmo_a_lo ! agcm_to_lsm_ocean

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rtmi_oo_a  => rt_in%ogcm_ocean_to_agcm%main
  rtmi_ol_a  => rt_in%ogcm_land_to_agcm%main
  rtmi_rr_a => rt_in%rm_river_to_agcm%main
  rtmi_rn_a => rt_in%rm_noriv_to_agcm%main
  rtmi_ro_a => rt_in%rm_ocean_to_agcm%main

  rtmo_lr_a => rt_out%lsm_river_to_agcm%main
  rtmo_ln_a => rt_out%lsm_noriv_to_agcm%main
  rtmo_lo_a => rt_out%lsm_ocean_to_agcm%main
  rtmo_a_lr => rt_out%agcm_to_lsm_river%main
  rtmo_a_ln => rt_out%agcm_to_lsm_noriv%main
  rtmo_a_lo => rt_out%agcm_to_lsm_ocean%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking permissions of input files', PRCNAM, MODNAM)

  !call traperr( check_permission(rtmi_o_a%f%sidx) )
  !call traperr( check_permission(rtmi_o_a%f%tidx) )
  !call traperr( check_permission(rtmi_o_a%f%area) )

  call traperr( check_permission(rtmi_oo_a%f%sidx) )
  call traperr( check_permission(rtmi_oo_a%f%tidx) )
  call traperr( check_permission(rtmi_oo_a%f%area) )

  call traperr( check_permission(rtmi_ol_a%f%sidx, allow_empty=.true.) )
  call traperr( check_permission(rtmi_ol_a%f%tidx, allow_empty=.true.) )
  call traperr( check_permission(rtmi_ol_a%f%area, allow_empty=.true.) )

  call traperr( check_permission(rtmi_rr_a%f%sidx) )
  call traperr( check_permission(rtmi_rr_a%f%tidx) )
  call traperr( check_permission(rtmi_rr_a%f%area) )

  call traperr( check_permission(rtmi_rn_a%f%sidx) )
  call traperr( check_permission(rtmi_rn_a%f%tidx) )
  call traperr( check_permission(rtmi_rn_a%f%area) )

  call traperr( check_permission(rtmi_ro_a%f%sidx) )
  call traperr( check_permission(rtmi_ro_a%f%tidx) )
  call traperr( check_permission(rtmi_ro_a%f%area) )

  call traperr( check_permission(agcm%fin_grdidx, allow_empty=.true.) )
  call traperr( check_permission(agcm%fin_grdara) )

  call traperr( check_permission(rm%fin_grdidx_river) )
  call traperr( check_permission(rm%fin_grdidx_noriv) )
  call traperr( check_permission(rm%fin_grdidx_ocean) )

  call traperr( check_permission(rm%fin_grdara_river) )
  call traperr( check_permission(rm%fin_grdara_noriv) )
  call traperr( check_permission(rm%fin_grdara_ocean) )

  call traperr( check_permission(rm%fin_rstidx_river) )
  call traperr( check_permission(rm%fin_rstidx_noriv) )
  call traperr( check_permission(rm%fin_rstidx_ocean) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking sizes of input files', PRCNAM, MODNAM)

  !call traperr( check_file_size(rtmi_o_a%f%sidx) )
  !call traperr( check_file_size(rtmi_o_a%f%tidx) )
  !call traperr( check_file_size(rtmi_o_a%f%area) )

  call traperr( check_file_size(rtmi_oo_a%f%sidx) )
  call traperr( check_file_size(rtmi_oo_a%f%tidx) )
  call traperr( check_file_size(rtmi_oo_a%f%area) )

  call traperr( check_file_size(rtmi_ol_a%f%sidx, allow_empty=.true.) )
  call traperr( check_file_size(rtmi_ol_a%f%tidx, allow_empty=.true.) )
  call traperr( check_file_size(rtmi_ol_a%f%area, allow_empty=.true.) )

  call traperr( check_file_size(rtmi_rr_a%f%sidx) )
  call traperr( check_file_size(rtmi_rr_a%f%tidx) )
  call traperr( check_file_size(rtmi_rr_a%f%area) )

  call traperr( check_file_size(rtmi_rn_a%f%sidx) )
  call traperr( check_file_size(rtmi_rn_a%f%tidx) )
  call traperr( check_file_size(rtmi_rn_a%f%area) )

  call traperr( check_file_size(rtmi_ro_a%f%sidx) )
  call traperr( check_file_size(rtmi_ro_a%f%tidx) )
  call traperr( check_file_size(rtmi_ro_a%f%area) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking old files of output', PRCNAM, MODNAM)

  call traperr( set_opt_old_files(opt%sys%old_files) )

  call traperr( handle_old_file(rtmo_lr_a%f%sidx) )
  call traperr( handle_old_file(rtmo_lr_a%f%tidx) )
  call traperr( handle_old_file(rtmo_lr_a%f%area) )
  call traperr( handle_old_file(rtmo_lr_a%f%coef) )

  call traperr( handle_old_file(rtmo_ln_a%f%sidx) )
  call traperr( handle_old_file(rtmo_ln_a%f%tidx) )
  call traperr( handle_old_file(rtmo_ln_a%f%area) )
  call traperr( handle_old_file(rtmo_ln_a%f%coef) )

  call traperr( handle_old_file(rtmo_lo_a%f%sidx) )
  call traperr( handle_old_file(rtmo_lo_a%f%tidx) )
  call traperr( handle_old_file(rtmo_lo_a%f%area) )
  call traperr( handle_old_file(rtmo_lo_a%f%coef) )

  call traperr( handle_old_file(rtmo_a_lr%f%sidx) )
  call traperr( handle_old_file(rtmo_a_lr%f%tidx) )
  call traperr( handle_old_file(rtmo_a_lr%f%area) )
  call traperr( handle_old_file(rtmo_a_lr%f%coef) )

  call traperr( handle_old_file(rtmo_a_ln%f%sidx) )
  call traperr( handle_old_file(rtmo_a_ln%f%tidx) )
  call traperr( handle_old_file(rtmo_a_ln%f%area) )
  call traperr( handle_old_file(rtmo_a_ln%f%coef) )

  call traperr( handle_old_file(rtmo_a_lo%f%sidx) )
  call traperr( handle_old_file(rtmo_a_lo%f%tidx) )
  call traperr( handle_old_file(rtmo_a_lo%f%area) )
  call traperr( handle_old_file(rtmo_a_lo%f%coef) )

  call traperr( handle_old_file(agcm%fout_lndara_ogcm) )
  call traperr( handle_old_file(agcm%fout_lndara_river) )
  call traperr( handle_old_file(agcm%fout_lndara_noriv_real) )
  call traperr( handle_old_file(agcm%fout_lndara_noriv_virt) )
  call traperr( handle_old_file(agcm%fout_lndara_noriv) )

  call traperr( handle_old_file(lsm%fout_grdmsk_river) )
  call traperr( handle_old_file(lsm%fout_grdmsk_noriv) )
  call traperr( handle_old_file(lsm%fout_grdmsk_noriv_real) )
  call traperr( handle_old_file(lsm%fout_grdmsk_noriv_virt) )
  call traperr( handle_old_file(lsm%fout_grdmsk_ocean) )

  call traperr( handle_old_file(lsm%fout_grdidx_river) )
  call traperr( handle_old_file(lsm%fout_grdidx_noriv) )
  call traperr( handle_old_file(lsm%fout_grdidx_noriv_real) )
  call traperr( handle_old_file(lsm%fout_grdidx_noriv_virt) )
  call traperr( handle_old_file(lsm%fout_grdidx_ocean) )

  call traperr( handle_old_file(lsm%fout_grdidx_bnd_river) )
  call traperr( handle_old_file(lsm%fout_grdidx_bnd_noriv) )
  call traperr( handle_old_file(lsm%fout_grdidx_bnd_noriv_real) )
  call traperr( handle_old_file(lsm%fout_grdidx_bnd_noriv_virt) )

  call traperr( handle_old_file(lsm%fout_grdara_river) )
  call traperr( handle_old_file(lsm%fout_grdara_noriv) )
  call traperr( handle_old_file(lsm%fout_grdara_noriv_real) )
  call traperr( handle_old_file(lsm%fout_grdara_noriv_virt) )
  call traperr( handle_old_file(lsm%fout_grdara_ocean) )

  call traperr( handle_old_file(lsm%fout_grdwgt_river) )
  call traperr( handle_old_file(lsm%fout_grdwgt_noriv) )
  call traperr( handle_old_file(lsm%fout_grdwgt_noriv_real) )
  call traperr( handle_old_file(lsm%fout_grdwgt_noriv_virt) )
  call traperr( handle_old_file(lsm%fout_grdwgt_ocean) )

  call traperr( handle_old_file(lsm%fout_rstidx_river) )
  call traperr( handle_old_file(lsm%fout_rstidx_noriv) )
  call traperr( handle_old_file(lsm%fout_rstidx_noriv_real) )
  call traperr( handle_old_file(lsm%fout_rstidx_noriv_virt) )
  call traperr( handle_old_file(lsm%fout_rstidx_ocean) )

  call traperr( handle_old_file(lsm%fout_rstidx_bnd_river) )
  call traperr( handle_old_file(lsm%fout_rstidx_bnd_noriv) )
  call traperr( handle_old_file(lsm%fout_rstidx_bnd_noriv_real) )
  call traperr( handle_old_file(lsm%fout_rstidx_bnd_noriv_virt) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Preparing output directories', PRCNAM, MODNAM)

  call traperr( set_opt_mkdir(output=.true., hut=hut_command) )

  ! rt
  !-------------------------------------------------------------
  call traperr( mkdir(dirname(rtmo_lr_a%f%sidx%path)) )
  call traperr( mkdir(dirname(rtmo_lr_a%f%tidx%path)) )
  call traperr( mkdir(dirname(rtmo_lr_a%f%area%path)) )
  call traperr( mkdir(dirname(rtmo_lr_a%f%coef%path)) )

  call traperr( check_permission(rtmo_lr_a%f%sidx) )
  call traperr( check_permission(rtmo_lr_a%f%tidx) )
  call traperr( check_permission(rtmo_lr_a%f%area) )
  call traperr( check_permission(rtmo_lr_a%f%coef) )

  call traperr( mkdir(dirname(rtmo_ln_a%f%sidx%path)) )
  call traperr( mkdir(dirname(rtmo_ln_a%f%tidx%path)) )
  call traperr( mkdir(dirname(rtmo_ln_a%f%area%path)) )
  call traperr( mkdir(dirname(rtmo_ln_a%f%coef%path)) )

  call traperr( check_permission(rtmo_ln_a%f%sidx) )
  call traperr( check_permission(rtmo_ln_a%f%tidx) )
  call traperr( check_permission(rtmo_ln_a%f%area) )
  call traperr( check_permission(rtmo_ln_a%f%coef) )

  call traperr( mkdir(dirname(rtmo_lo_a%f%sidx%path)) )
  call traperr( mkdir(dirname(rtmo_lo_a%f%tidx%path)) )
  call traperr( mkdir(dirname(rtmo_lo_a%f%area%path)) )
  call traperr( mkdir(dirname(rtmo_lo_a%f%coef%path)) )

  call traperr( check_permission(rtmo_lo_a%f%sidx, allow_empty=.true.) )
  call traperr( check_permission(rtmo_lo_a%f%tidx, allow_empty=.true.) )
  call traperr( check_permission(rtmo_lo_a%f%area, allow_empty=.true.) )
  call traperr( check_permission(rtmo_lo_a%f%coef, allow_empty=.true.) )

  call traperr( mkdir(dirname(rtmo_a_lr%f%sidx%path)) )
  call traperr( mkdir(dirname(rtmo_a_lr%f%tidx%path)) )
  call traperr( mkdir(dirname(rtmo_a_lr%f%area%path)) )
  call traperr( mkdir(dirname(rtmo_a_lr%f%coef%path)) )

  call traperr( check_permission(rtmo_a_lr%f%sidx) )
  call traperr( check_permission(rtmo_a_lr%f%tidx) )
  call traperr( check_permission(rtmo_a_lr%f%area) )
  call traperr( check_permission(rtmo_a_lr%f%coef) )

  call traperr( mkdir(dirname(rtmo_a_ln%f%sidx%path)) )
  call traperr( mkdir(dirname(rtmo_a_ln%f%tidx%path)) )
  call traperr( mkdir(dirname(rtmo_a_ln%f%area%path)) )
  call traperr( mkdir(dirname(rtmo_a_ln%f%coef%path)) )

  call traperr( check_permission(rtmo_a_ln%f%sidx) )
  call traperr( check_permission(rtmo_a_ln%f%tidx) )
  call traperr( check_permission(rtmo_a_ln%f%area) )
  call traperr( check_permission(rtmo_a_ln%f%coef) )

  call traperr( mkdir(dirname(rtmo_a_lo%f%sidx%path)) )
  call traperr( mkdir(dirname(rtmo_a_lo%f%tidx%path)) )
  call traperr( mkdir(dirname(rtmo_a_lo%f%area%path)) )
  call traperr( mkdir(dirname(rtmo_a_lo%f%coef%path)) )

  call traperr( check_permission(rtmo_a_lo%f%sidx, allow_empty=.true.) )
  call traperr( check_permission(rtmo_a_lo%f%tidx, allow_empty=.true.) )
  call traperr( check_permission(rtmo_a_lo%f%area, allow_empty=.true.) )
  call traperr( check_permission(rtmo_a_lo%f%coef, allow_empty=.true.) )

  ! agcm
  !-------------------------------------------------------------
  call traperr( set_opt_check_permission(allow_empty=.true.) )

  call traperr( mkdir(dirname(agcm%fout_lndara_ogcm%path)) )
  call traperr( mkdir(dirname(agcm%fout_lndara_river%path)) )
  call traperr( mkdir(dirname(agcm%fout_lndara_noriv_real%path)) )
  call traperr( mkdir(dirname(agcm%fout_lndara_noriv_virt%path)) )
  call traperr( mkdir(dirname(agcm%fout_lndara_noriv%path)) )

  call traperr( check_permission(agcm%fout_lndara_ogcm) )
  call traperr( check_permission(agcm%fout_lndara_river) )
  call traperr( check_permission(agcm%fout_lndara_noriv) )
  call traperr( check_permission(agcm%fout_lndara_noriv_real) )
  call traperr( check_permission(agcm%fout_lndara_noriv_virt) )

  call traperr( init_opt_check_permission('allow_empty') )

  ! lsm
  !-------------------------------------------------------------
  call traperr( set_opt_check_permission(allow_empty=.true.) )

  call traperr( mkdir(dirname(lsm%fout_grdmsk_river%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdmsk_noriv%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdmsk_noriv_real%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdmsk_noriv_virt%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdmsk_ocean%path)) )

  call traperr( check_permission(lsm%fout_grdmsk_river) )
  call traperr( check_permission(lsm%fout_grdmsk_noriv) )
  call traperr( check_permission(lsm%fout_grdmsk_noriv_real) )
  call traperr( check_permission(lsm%fout_grdmsk_noriv_virt) )
  call traperr( check_permission(lsm%fout_grdmsk_ocean) )

  call traperr( mkdir(dirname(lsm%fout_grdidx_river%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdidx_noriv%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdidx_noriv_real%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdidx_noriv_virt%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdidx_ocean%path)) )

  call traperr( check_permission(lsm%fout_grdidx_river) )
  call traperr( check_permission(lsm%fout_grdidx_noriv) )
  call traperr( check_permission(lsm%fout_grdidx_noriv_real) )
  call traperr( check_permission(lsm%fout_grdidx_noriv_virt) )
  call traperr( check_permission(lsm%fout_grdidx_ocean) )

  call traperr( mkdir(dirname(lsm%fout_grdidx_bnd_river%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdidx_bnd_noriv%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdidx_bnd_noriv_real%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdidx_bnd_noriv_virt%path)) )

  call traperr( check_permission(lsm%fout_grdidx_bnd_river) )
  call traperr( check_permission(lsm%fout_grdidx_bnd_noriv) )
  call traperr( check_permission(lsm%fout_grdidx_bnd_noriv_real) )
  call traperr( check_permission(lsm%fout_grdidx_bnd_noriv_virt) )

  call traperr( mkdir(dirname(lsm%fout_grdara_river%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdara_noriv%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdara_noriv_real%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdara_noriv_virt%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdara_ocean%path)) )

  call traperr( check_permission(lsm%fout_grdara_river) )
  call traperr( check_permission(lsm%fout_grdara_noriv) )
  call traperr( check_permission(lsm%fout_grdara_noriv_real) )
  call traperr( check_permission(lsm%fout_grdara_noriv_virt) )
  call traperr( check_permission(lsm%fout_grdara_ocean) )

  call traperr( mkdir(dirname(lsm%fout_grdwgt_river%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdwgt_noriv%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdwgt_noriv_real%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdwgt_noriv_virt%path)) )
  call traperr( mkdir(dirname(lsm%fout_grdwgt_ocean%path)) )

  call traperr( check_permission(lsm%fout_grdwgt_river) )
  call traperr( check_permission(lsm%fout_grdwgt_noriv) )
  call traperr( check_permission(lsm%fout_grdwgt_noriv_real) )
  call traperr( check_permission(lsm%fout_grdwgt_noriv_virt) )
  call traperr( check_permission(lsm%fout_grdwgt_ocean) )

  call traperr( mkdir(dirname(lsm%fout_rstidx_river%path)) )
  call traperr( mkdir(dirname(lsm%fout_rstidx_noriv%path)) )
  call traperr( mkdir(dirname(lsm%fout_rstidx_noriv_real%path)) )
  call traperr( mkdir(dirname(lsm%fout_rstidx_noriv_virt%path)) )
  call traperr( mkdir(dirname(lsm%fout_rstidx_ocean%path)) )

  call traperr( check_permission(lsm%fout_rstidx_river) )
  call traperr( check_permission(lsm%fout_rstidx_noriv) )
  call traperr( check_permission(lsm%fout_rstidx_noriv_real) )
  call traperr( check_permission(lsm%fout_rstidx_noriv_virt) )
  call traperr( check_permission(lsm%fout_rstidx_ocean) )

  call traperr( mkdir(dirname(lsm%fout_rstidx_bnd_river%path)) )
  call traperr( mkdir(dirname(lsm%fout_rstidx_bnd_noriv%path)) )
  call traperr( mkdir(dirname(lsm%fout_rstidx_bnd_noriv_real%path)) )
  call traperr( mkdir(dirname(lsm%fout_rstidx_bnd_noriv_virt%path)) )

  call traperr( check_permission(lsm%fout_rstidx_bnd_river) )
  call traperr( check_permission(lsm%fout_rstidx_bnd_noriv) )
  call traperr( check_permission(lsm%fout_rstidx_bnd_noriv_real) )
  call traperr( check_permission(lsm%fout_rstidx_bnd_noriv_virt) )

  call traperr( init_opt_check_permission('allow_empty') )
  !-------------------------------------------------------------
  call traperr( init_opt_mkdir('output') )
  call traperr( init_opt_mkdir('hut') )

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_paths
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_modify(fill, rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'put_opt_coef_sum_modify'
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_sum_modify_enabled )then
    opt%is_sum_modify_enabled = .true.
    opt%sum_modify = fill
  endif
end subroutine put_opt_coef_sum_modify
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_modify_ulim(fill, rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'put_opt_coef_sum_modify_ulim'
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_sum_modify_ulim_enabled )then
    opt%is_sum_modify_ulim_enabled = .true.
    opt%sum_modify_ulim = fill
  endif
end subroutine put_opt_coef_sum_modify_ulim
!===============================================================
!
!===============================================================
subroutine put_opt_coef_zero_positive(fill, rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'put_opt_coef_zero_positive'
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_zero_positive_enabled )then
    opt%is_zero_positive_enabled = .true.
    opt%zero_positive = fill
  endif
end subroutine put_opt_coef_zero_positive
!===============================================================
!
!===============================================================
subroutine put_opt_coef_zero_negative(fill, rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'put_opt_coef_zero_negative'
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_zero_negative_enabled )then
    opt%is_zero_negative_enabled = .true.
    opt%zero_negative = fill
  endif
end subroutine put_opt_coef_zero_negative
!===============================================================
!
!===============================================================
subroutine put_opt_coef_error_excess(fill, rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'put_opt_coef_error_excess'
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_error_excess_enabled )then
    opt%is_error_excess_enabled = .true.
    opt%error_excess = fill
  endif
end subroutine put_opt_coef_error_excess
!===============================================================
!
!===============================================================
subroutine put_opt_coef_sum_error_excess(fill, rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'put_opt_coef_sum_error_excess'
  real(8)  , intent(in)            :: fill
  type(rt_), intent(inout), target :: rt

  type(opt_rt_coef_), pointer :: opt

  opt => rt%main%opt_coef
  if( .not. opt%is_sum_error_excess_enabled )then
    opt%is_sum_error_excess_enabled = .true.
    opt%sum_error_excess = fill
  endif
end subroutine put_opt_coef_sum_error_excess
!===============================================================
!
!===============================================================
end module mod_set
