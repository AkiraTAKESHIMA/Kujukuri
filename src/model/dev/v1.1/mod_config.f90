module mod_config
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use def_const
  use def_type
  use mod_global
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: prepare_static_data
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_config'

  type config_output_
    character(CLEN_PATH) :: dir
    logical :: overwrite
    integer :: interval  ! sec
  end type

  type config_simulation_
    character(32) :: stime_start
    character(32) :: stime_end
    integer :: version_slope_calc_discharge
    integer :: version_slope_advance_slope
  end type

  type config_timestep_
    integer :: model  ! sec
    integer :: slope  ! sec
    integer :: river  ! sec
  end type

  type config_solver_adaptive_rk_
    real(8) :: error_tolerance
    real(8) :: dt_min_slope
    real(8) :: dt_min_river
  end type

  type config_solver_test_
    real(8) :: error_tolerance
    real(8) :: dt_min_slope
    real(8) :: dt_min_river
  end type

  type config_solver_
    character(CLEN_KEY) :: method
    type(config_solver_adaptive_rk_) :: adaptive_rk
    type(config_solver_test_) :: test
  end type

  type config_domain_
    real(8) :: west
    real(8) :: east
    real(8) :: south
    real(8) :: north
    integer :: nx
    integer :: ny
    real(8) :: earth_r
  end type

  type config_input_topography_
    character(CLEN_PATH) :: file_elv       ! elevation
    character(CLEN_PATH) :: file_flwdir    ! flow direction
  end type

  type config_input_landcover_
    character(CLEN_PATH) :: file_landcover   ! land cover
  end type

  type config_input_river_network_
    character(CLEN_PATH) :: file_network  ! river network
    logical :: allow_channels_outside_domain
  end type

  type config_input_river_crosssection_
    character(CLEN_KEY) :: method
    character(CLEN_PATH) :: file_width, file_depth, file_levee
    character(CLEN_PATH) :: dir_explicit
  end type

  type config_input_river_
    type(config_input_river_network_) :: network
    type(config_input_river_crosssection_) :: crssct
  end type

  type config_input_forcing_
    integer :: interval
    character(CLEN_PATH) :: file_prcp  ! precipitation
    real(4) :: prcp_miss
  end type

  type config_input_
    type(config_input_topography_) :: topography
    type(config_input_landcover_)  :: landcover
    type(config_input_river_)      :: river
    type(config_input_forcing_)    :: forcing
  end type

  type config_param_landcover_
    integer :: diffusion
    real(8) :: ns, soildepth, gammaa, ksv, faif, infilt_limit, &
               ka, km, gammam, beta, da, dm, &
               ksg, gammag, kg0, fpg, rgl
  end type

  type config_param_slope_
    integer :: num_landcover
    type(config_param_landcover_), pointer :: landcover(:)
  end type

  type config_param_river_
    real(8) :: ns
  end type

  type config_param_
    type(config_param_slope_) :: slope
    type(config_param_river_) :: river
  end type

  type config_
    type(config_output_) :: output
    type(config_simulation_) :: simulation
    type(config_timestep_) :: timestep
    type(config_solver_) :: solver
    type(config_domain_) :: domain
    type(config_input_) :: input
    type(config_param_) :: param
  end type

  type(config_), target :: config
  integer :: un
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prepare_static_data()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prepare_static_data'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Preparing static data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_config()

  call setup_output(output)

  call setup_time(time)

  call setup_grid(grid, slope)

  call setup_solver(solver)

  call setup_scheme(slope, river)

  call setup_forcing(time, forcing)

  call load_input_data(grid, slope, river)

  call build_static_data(grid, slope, river)
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine prepare_static_data
!===============================================================
!
!===============================================================
subroutine read_config()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_config'

  character(CLEN_PATH) :: f_conf

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_conf = argument(1)
  open(newunit=un, file=f_conf, status='old')

  call read_config_output()

  call read_config_simulation()

  call read_config_timestep()

  call read_config_solver()

  call read_config_domain()

  call read_config_topography()

  call read_config_landcover()

  call read_config_river_network()

  call read_config_river_crosssection()

  call read_config_param_slope()

  call read_config_param_river()

  call read_config_forcing()

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_config
!===============================================================
!
!===============================================================
subroutine read_config_output()
  implicit none
  type(config_output_), pointer :: c

  character(CLEN_PATH) :: dir
  real(8) :: interval
  logical :: overwrite

  namelist/output/ dir, interval, overwrite

  c => config%output

  dir = './output'
  interval = 600.d0
  overwrite = .false.

  rewind(un)
  read(un,nml=output)

  c%dir = dir
  c%interval = interval
  c%overwrite = overwrite
end subroutine read_config_output
!===============================================================
!
!===============================================================
subroutine read_config_simulation()
  implicit none
  type(config_simulation_), pointer :: c

  character(32) :: datetime_start
  character(32) :: datetime_end
  integer :: version_slope_advance_slope
  integer :: version_slope_calc_discharge

  namelist/simulation/ &
    datetime_start, datetime_end, &
    version_slope_advance_slope, version_slope_calc_discharge

  c => config%simulation

  datetime_start = ''
  datetime_end = ''
  version_slope_advance_slope = 1
  version_slope_calc_discharge = 1

  rewind(un)
  read(un,nml=simulation)

  c%stime_start = datetime_start
  c%stime_end   = datetime_end
  c%version_slope_advance_slope = version_slope_advance_slope
  c%version_slope_calc_discharge = version_slope_calc_discharge
end subroutine read_config_simulation
!===============================================================
!
!===============================================================
subroutine read_config_timestep()
  implicit none
  type(config_timestep_), pointer :: c

  real(8) :: model, slope, river

  namelist/timestep/ model, slope, river

  c => config%timestep

  model = 3600.d0
  slope = 60.d0
  river = 60.d0

  rewind(un)
  read(un,nml=timestep)

  c%model = model
  c%slope = slope
  c%river = river
end subroutine read_config_timestep
!===============================================================
!
!===============================================================
subroutine read_config_solver()
  implicit none
  type(config_solver_), pointer :: c

  character(CLEN_KEY) :: method
  real(8) :: error_tolerance
  real(8) :: dt_min_slope, dt_min_river

  namelist/solver/ method

  namelist/solver_adaptive_rk45/ &
    error_tolerance, dt_min_slope, dt_min_river

  namelist/solver_test/ &
    error_tolerance, dt_min_slope, dt_min_river

  c => config%solver

  rewind(un)
  read(un,nml=solver)

  c%method = method

  selectcase( method )

  case( 'adaptive_rk45' )

    error_tolerance = 0.01d0
    dt_min_slope = 0.1d0
    dt_min_river = 0.1d0

    rewind(un)
    read(un,nml=solver_adaptive_rk45)

    c%adaptive_rk%error_tolerance = error_tolerance
    c%adaptive_rk%dt_min_slope = dt_min_slope
    c%adaptive_rk%dt_min_river = dt_min_river

  case( 'test' )

    error_tolerance = 0.01d0
    dt_min_slope = 0.1d0
    dt_min_river = 0.1d0

    rewind(un)
    read(un,nml=solver_test)

    c%test%error_tolerance = error_tolerance
    c%test%dt_min_slope = dt_min_slope
    c%test%dt_min_river = dt_min_river

  case default
    call errend(msg_invalid_value('method', method)//&
      'namelist: solver')
  endselect
end subroutine read_config_solver
!===============================================================
!
!===============================================================
subroutine read_config_domain()
  implicit none
  type(config_domain_), pointer :: c

  real(8) :: west, east, south, north
  integer :: nx, ny
  real(8) :: earth_radius

  namelist/domain/ west, east, south, north, nx, ny, earth_radius

  c => config%domain

  west = 0.d0
  east = 0.d0
  south = 0.d0
  north = 0.d0
  nx = 0
  ny = 0
  earth_radius = EARTH_CONST__WGS84_R_VOLMETRIC

  rewind(un)
  read(un,nml=domain)

  c%west = west
  c%east = east
  c%south = south
  c%north = north
  c%nx = nx
  c%ny = ny
  c%earth_r = earth_radius
end subroutine read_config_domain
!===============================================================
!
!===============================================================
subroutine read_config_topography()
  implicit none
  type(config_input_topography_), pointer :: c

  character(CLEN_PATH) :: file_elv
  character(CLEN_PATH) :: file_flwdir

  namelist/topography/ &
    file_elv, file_flwdir

  c => config%input%topography

  file_elv = ''
  file_flwdir = ''

  rewind(un)
  read(un,nml=topography)

  c%file_elv = file_elv
  c%file_flwdir = file_flwdir
end subroutine read_config_topography
!===============================================================
!
!===============================================================
subroutine read_config_landcover()
  implicit none
  type(config_input_landcover_), pointer :: c

  integer :: num_category
  character(CLEN_PATH) :: file_landcover

  namelist/landcover/ &
    num_category, file_landcover

  c => config%input%landcover

  num_category = 1
  file_landcover = ''

  rewind(un)
  read(un,nml=landcover)

  config%param%slope%num_landcover = num_category
  c%file_landcover = file_landcover
end subroutine read_config_landcover
!===============================================================
!
!===============================================================
subroutine read_config_river_network()
  implicit none
  type(config_input_river_network_), pointer :: c

  character(CLEN_PATH) :: file_river_network
  logical :: allow_channels_outside_domain

  namelist/river_network/ &
    file_river_network, allow_channels_outside_domain

  c => config%input%river%network

  file_river_network = ''
  allow_channels_outside_domain = .false.

  rewind(un)
  read(un,nml=river_network)

  c%file_network = file_river_network
  c%allow_channels_outside_domain = allow_channels_outside_domain
end subroutine read_config_river_network
!===============================================================
!
!===============================================================
subroutine read_config_river_crosssection()
  implicit none
  type(config_input_river_crosssection_), pointer :: c
  character(CLEN_KEY) :: method
  character(CLEN_PATH) :: file_width, file_depth, file_levee
  character(CLEN_PATH) :: dir_explicit

  namelist/river_crosssection/ &
    method, &
    file_width, file_depth, file_levee, &
    dir_explicit

  c => config%input%river%crssct

  method = ''
  file_width = ''
  file_depth = ''
  file_levee = ''
  dir_explicit = ''

  rewind(un)
  read(un,nml=river_crosssection)

  c%method = method
  c%file_width = file_width
  c%file_depth = file_depth
  c%file_levee = file_levee
  c%dir_explicit = dir_explicit
end subroutine read_config_river_crosssection
!===============================================================
!
!===============================================================
subroutine read_config_param_slope()
  implicit none
  type(config_param_slope_), pointer :: c

  integer :: n
  integer, allocatable :: diffusion(:)
  real(8), allocatable :: ns(:), soildepth(:)
  real(8), allocatable :: gammaa(:), ksv(:), faif(:), infilt_limit(:)
  real(8), allocatable :: ka(:), gammam(:), beta(:)
  real(8), allocatable :: ksg(:), gammag(:), kg0(:), fpg(:), rgl(:)

  namelist/param_landcover/ &
    diffusion, ns, soildepth, gammaa, ksv, faif, &
    infilt_limit, ka, gammam, beta, &
    ksg, gammag, kg0, fpg, rgl

  c => config%param%slope

  n = config%param%slope%num_landcover

  allocate(diffusion(n))
  allocate(ns(n))
  allocate(soildepth(n))
  allocate(gammaa(n))
  allocate(ksv(n))
  allocate(faif(n))
  allocate(infilt_limit(n))
  allocate(ka(n))
  allocate(gammam(n))
  allocate(beta(n))
  allocate(ksg(n))
  allocate(gammag(n))
  allocate(kg0(n))
  allocate(fpg(n))
  allocate(rgl(n))
  diffusion(:) = 0
  ns(:) = 0.d0
  soildepth(:) = 0.d0
  gammaa(:) = 0.d0
  ksv(:) = 0.d0
  faif(:) = 0.d0
  infilt_limit(:) = 0.d0
  ka(:) = 0.d0
  gammam(:) = 0.d0
  beta(:) = 0.d0
  ksg(:) = 0.d0
  gammag(:) = 0.d0
  kg0(:) = 0.d0
  gammag(:) = 0.d0
  kg0(:) = 0.d0
  fpg(:) = 0.d0
  rgl(:) = 0.d0

  rewind(un)
  read(un,nml=param_landcover)

  allocate(c%landcover(n))
  c%landcover(:)%diffusion = diffusion(:)
  c%landcover(:)%ns = ns(:)
  c%landcover(:)%soildepth = soildepth(:)
  c%landcover(:)%gammaa = gammaa(:)
  c%landcover(:)%ksv = ksv(:)
  c%landcover(:)%faif = faif(:)
  c%landcover(:)%infilt_limit = infilt_limit(:)
  c%landcover(:)%ka = ka(:)
  c%landcover(:)%gammam = gammam(:)
  c%landcover(:)%beta = beta(:)
  c%landcover(:)%ksg = ksg(:)
  c%landcover(:)%gammag = gammag(:)
  c%landcover(:)%kg0 = kg0(:)
  c%landcover(:)%fpg = fpg(:)
  c%landcover(:)%rgl = rgl(:)
end subroutine read_config_param_slope
!===============================================================
!
!===============================================================
subroutine read_config_param_river()
  implicit none
  type(config_param_river_), pointer :: c

  real(8) :: ns

  namelist/param_river/ ns

  c => config%param%river

  read(un,nml=param_river)

  c%ns = ns
end subroutine read_config_param_river
!===============================================================
!
!===============================================================
subroutine read_config_forcing()
  implicit none
  type(config_input_forcing_), pointer :: c

  real(8) :: interval
  character(CLEN_PATH) :: file_prcp
  real(4) :: prcp_miss

  namelist/forcing/ interval, file_prcp, prcp_miss

  c => config%input%forcing

  interval = 0.d0
  file_prcp = ''
  prcp_miss = 0.0

  read(un,nml=forcing)

  c%interval = interval
  c%file_prcp = file_prcp
  c%prcp_miss = prcp_miss
end subroutine read_config_forcing
!===============================================================
!
!===============================================================
subroutine setup_output(output)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_output'
  type(output_), intent(inout) :: output

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  output%dir = config%output%dir
  output%overwrite = config%output%overwrite

  call traperr( mkdir(output%dir) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine setup_output
!===============================================================
!
!===============================================================
subroutine setup_time(time)
  use mod_time, only: &
    strftime
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_time'
  type(time_), intent(inout) :: time

  integer :: year_start

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(time%datetime_start(7))
  allocate(time%datetime_end(7))
  allocate(time%datetime_base(7))
  allocate(time%datetime_now(7))

  call stime2time(config%simulation%stime_start, time%datetime_start)
  call stime2time(config%simulation%stime_end, time%datetime_end)

  year_start = time%datetime_start(1)

  time%datetime_base(:) = 0
  time%datetime_base(1) = year_start

  time%datetime_now(:) = 0

  time%t_start = second(time%datetime_start, year_start)
  time%t_end = second(time%datetime_end, year_start)

  time%dt_model = config%timestep%model
  time%dt_river = config%timestep%river
  time%dt_slope = config%timestep%slope

  time%dt_forcing = config%input%forcing%interval

  call logmsg('datetime start: '//strftime(time%datetime_start)//&
    ' ('//str(time%t_start)//' sec from '//str(year_start)//'-01-01T00:00:00)')
  call logmsg('         end  : '//strftime(time%datetime_end)//&
    ' ('//str(time%t_end)//' sec from '//str(year_start)//'-01-01T00:00:00)')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine stime2time(stime, time)
  implicit none
  character(*), intent(in) :: stime
  integer, intent(out) :: time(:)

  read(stime(:4),*) time(1)  ! year
  read(stime(6:7),*) time(2)  ! month
  read(stime(9:10),*) time(3)  ! day
  !read(stime(11:11),*)
  time(4) = 0  ! time difference from UTC
  read(stime(12:13),*) time(5)  ! hour
  read(stime(15:16),*) time(6)  ! minutes
  read(stime(18:19),*) time(7)  ! minutes
end subroutine stime2time
!---------------------------------------------------------------
end subroutine setup_time
!===============================================================
!
!===============================================================
subroutine setup_grid(grid, slope)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_grid'
  type(grid_), intent(inout) :: grid
  type(static_slope_), intent(inout) :: slope

  type(config_domain_), pointer :: c

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  c => config%domain

  grid%nx = c%nx
  grid%ny = c%ny
  grid%west = c%west
  grid%east = c%east
  grid%south = c%south
  grid%north = c%north
  grid%earth_r = c%earth_r

  grid%cellsize_lon = (grid%east - grid%west) / grid%nx
  grid%cellsize_lat = (grid%north - grid%south) / grid%ny

  slope%nx = grid%nx
  slope%ny = grid%ny
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine setup_grid
!===============================================================
!
!===============================================================
subroutine setup_solver(solver)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_solver'
  type(solver_), intent(inout) :: solver

  type(config_solver_), pointer :: c

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  c => config%solver

  selectcase( c%method )
  case( 'adaptive_rk45' )
    solver%method = SOLVER_METHOD__ADAPTIVE_RK45
    solver%adaptive_rk45%dt_min_slope = c%adaptive_rk%dt_min_slope
    solver%adaptive_rk45%dt_min_river = c%adaptive_rk%dt_min_river
    solver%adaptive_rk45%error_tolerance = c%adaptive_rk%error_tolerance
  case( 'test' )
    solver%method = SOLVER_METHOD__TEST
    solver%test%dt_min_slope = c%test%dt_min_slope
    solver%test%dt_min_river = c%test%dt_min_river
    solver%test%error_tolerance = c%test%error_tolerance
  case default
    call errend(msg_invalid_value('config%solver%method', config%solver%method))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine setup_solver
!===============================================================
!
!===============================================================
subroutine setup_scheme(slope, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_scheme'
  type(static_slope_), intent(inout) :: slope
  type(static_river_), intent(inout) :: river

  type(config_simulation_), pointer :: c

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  c => config%simulation

  slope%version%advance_slope = c%version_slope_advance_slope
  slope%version%calc_discharge = c%version_slope_calc_discharge
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine setup_scheme
!===============================================================
!
!===============================================================
subroutine setup_forcing(time, forcing)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_forcing'
  type(time_), intent(inout) :: time
  type(forcing_), intent(inout) :: forcing

  type(config_input_forcing_), pointer :: c

  c => config%input%forcing

  time%dt_forcing = c%interval

  forcing%file_prcp = c%file_prcp
  forcing%prcp_miss = c%prcp_miss
end subroutine setup_forcing
!===============================================================
!
!===============================================================
subroutine load_input_data(grid, slope, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_input_data'
  type(grid_), intent(inout) :: grid
  type(static_slope_), intent(inout) :: slope
  type(static_river_), intent(inout) :: river

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call load_topography(grid, slope)

  call load_landcover(slope)

  call load_river_data(river)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_input_data
!===============================================================
!
!===============================================================
subroutine load_topography(grid, slope)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_topography'
  type(grid_), intent(inout) :: grid
  type(static_slope_), intent(inout) :: slope

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grid%domain_mask(slope%nx,slope%ny))
  allocate(slope%zs(slope%nx,slope%ny))

  slope%domain_mask => grid%domain_mask

  call traperr( rbin(slope%zs, config%input%topography%file_elv, dtype=DTYPE_REAL) )

  where( slope%zs <= ZS_MISS_THRESH )
    slope%domain_mask = DOMAIN__OUTSIDE
  elsewhere
    slope%domain_mask = DOMAIN__INSIDE
  endwhere
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_topography
!===============================================================
!
!===============================================================
subroutine load_landcover(slope)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_landcover'
  type(static_slope_), intent(inout) :: slope

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(slope%landcover(slope%nx,slope%ny))

  if( config%input%landcover%file_landcover == '' )then
    slope%landcover(:,:) = 1
  else
    call traperr( rbin(slope%landcover, config%input%landcover%file_landcover) )
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_landcover
!===============================================================
!
!===============================================================
subroutine load_river_data(river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_river_data'
  type(static_river_), intent(inout) :: river

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call load_river_network(river)

  call load_river_crosssection(river)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_river_data
!===============================================================
!
!===============================================================
subroutine load_river_network(river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_river_network'
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  type(node_), pointer :: nd1, nd2
  integer :: iCh
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  river%allow_channels_outside_domain = config%input%river%network%allow_channels_outside_domain

  open(newunit=un, file=config%input%river%network%file_network, status='old')

  read(un,*) c_, river%nCh
  allocate(river%channel(river%nCh))

  do iCh = 1, river%nCh
    ch => river%channel(iCh)

    read(un,*)  ! index
    read(un,*)  ! index of original channel

    allocate(ch%node(2))
    nd1 => ch%node(1)
    nd2 => ch%node(2)
    read(un,*) c_, nd1%is_outlet, nd2%is_outlet
    read(un,*) c_, nd1%downleng, nd2%downleng

    read(un,*) c_, ch%nPt
    allocate(ch%pt(ch%nPt))

    read(un,*) c_, ch%pt(:)%lon
    read(un,*) c_, ch%pt(:)%lat
  enddo  ! iCh/

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_river_network
!===============================================================
!
!===============================================================
subroutine load_river_crosssection(river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_river_crosssection'
  type(static_river_), intent(inout) :: river

  type(config_input_river_crosssection_), pointer :: c

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Loading river cross section data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  c => config%input%river%crssct

  selectcase( c%method )
  case( 'rectangular' )
    river%crssct_method = RIVER_CRSSCT_METHOD__RECTANGULAR
    call traperr( rbin(river%channel(:)%width, c%file_width) )
    call traperr( rbin(river%channel(:)%depth, c%file_depth) )
    call traperr( rbin(river%channel(:)%levee, c%file_levee) )
    call logmsg('width min: '//str(minval(river%channel(:)%width))//&
                     ' max: '//str(maxval(river%channel(:)%width)))
    call logmsg('depth min: '//str(minval(river%channel(:)%depth))//&
                     ' max: '//str(maxval(river%channel(:)%depth)))
    call logmsg('levee min: '//str(minval(river%channel(:)%levee))//&
                     ' max: '//str(maxval(river%channel(:)%levee)))

  case( 'explicit' )
    river%crssct_method = RIVER_CRSSCT_METHOD__EXPLICIT

    !TMP

  case default
    call errend(msg_invalid_value('config%input%river%crssct%method', c%method))
  endselect
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine load_river_crosssection
!===============================================================
!
!===============================================================
subroutine build_static_data(grid, slope, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_static_data'
  type(grid_), intent(inout) :: grid
  type(static_slope_), intent(inout) :: slope
  type(static_river_), intent(inout) :: river

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building static data')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call build_grid(grid)

  call build_slope_properties(grid, slope)

  call build_river_properties(river)

  call build_river_network(grid, river)

  call build_river_crosssection(river)

  call build_slope_river_mapping(grid, river)

  call build_river_topography(grid, slope, river)
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine build_static_data
!===============================================================
!
!===============================================================
subroutine build_grid(grid)
  use mod_mesh, only: &
    west_of_x, &
    east_of_x, &
    south_of_y, &
    north_of_y
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_grid'
  type(grid_), intent(inout) :: grid

  integer :: ix, iy

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building grid')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grid%area(grid%nx, grid%ny))

  do iy = 1, grid%ny
  do ix = 1, grid%nx
    if( grid%domain_mask(ix,iy) == DOMAIN__OUTSIDE ) cycle

    grid%area(ix,iy) = area_sphere_rect( &
      south_of_y(grid,iy)*d2r, north_of_y(grid,iy)*d2r &
    ) * (east_of_x(grid,ix)-west_of_x(grid,ix)) * d2r * grid%earth_r**2
  enddo  ! ix/
  enddo  ! iy/

  call logmsg('grid area mean: '//str(sum(grid%area,mask=grid%domain_mask/=DOMAIN__OUTSIDE)/&
      count(grid%domain_mask/=DOMAIN__OUTSIDE))//' [m2]')
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine build_grid
!===============================================================
!
!===============================================================
subroutine build_slope_properties(grid, slope)
  use mod_mesh, only: &
    west_of_x, &
    east_of_x, &
    south_of_y, &
    north_of_y, &
    lon_center_of_x, &
    lat_center_of_y
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_slope_properties'
  type(grid_), intent(in) :: grid
  type(static_slope_), intent(inout) :: slope

  type(config_param_landcover_), pointer :: lc
  integer :: ix, iy, xx, yy, k, l
  real(8) :: len

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building slope properties')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  slope%nGrid = count(slope%domain_mask /= DOMAIN__OUTSIDE)
  slope%nDir = 4

  allocate(slope%idx2i(slope%nGrid))
  allocate(slope%idx2j(slope%nGrid))
  allocate(slope%ij2idx(slope%nx,slope%ny))

  allocate(slope%area(slope%nx,slope%ny))

  allocate(slope%domain_mask_idx(slope%nGrid))

  allocate(slope%down_idx(slope%nDir,slope%nGrid))
  allocate(slope%up_idx(slope%nDir,slope%nGrid))
  allocate(slope%area_idx(slope%nGrid))
  allocate(slope%dis_idx(slope%nDir,slope%nGrid))
  allocate(slope%len_idx(slope%nDir,slope%nGrid))

  allocate(slope%landcover_idx(slope%nGrid))
  allocate(slope%zb_idx(slope%nGrid))

  allocate(slope%diffusion_idx(slope%nGrid))
  allocate(slope%ns_idx(slope%nGrid))
  allocate(slope%soildepth_idx(slope%nGrid))
  allocate(slope%gammaa_idx(slope%nGrid))

  allocate(slope%ksv_idx(slope%nGrid))
  allocate(slope%faif_idx(slope%nGrid))
  allocate(slope%infilt_limit_idx(slope%nGrid))
  allocate(slope%ka_idx(slope%nGrid))
  allocate(slope%km_idx(slope%nGrid))
  allocate(slope%gammam_idx(slope%nGrid))
  allocate(slope%beta_idx(slope%nGrid))
  allocate(slope%da_idx(slope%nGrid))
  allocate(slope%dm_idx(slope%nGrid))
  allocate(slope%ksg_idx(slope%nGrid))

  allocate(slope%gammag_idx(slope%nGrid))
  allocate(slope%kg0_idx(slope%nGrid))
  allocate(slope%fpg_idx(slope%nGrid))
  allocate(slope%rgl_idx(slope%nGrid))

  k = 0
  do iy = 1, slope%ny
  do ix = 1, slope%nx
    if( slope%domain_mask(ix,iy) == DOMAIN__OUTSIDE ) cycle

    lc => config%param%slope%landcover(slope%landcover(ix,iy))

    k = k + 1
    slope%idx2i(k) = ix
    slope%idx2j(k) = iy
    slope%ij2idx(ix,iy) = k

    ! TMP
    ! intersection with river must be considered inthe future
    slope%area(ix,iy) = grid%area(ix,iy)

    slope%domain_mask_idx(k) = slope%domain_mask(ix,iy)

!    slope%acc_idx(k) = slope%acc(ix,iy)
    slope%area_idx(k) = slope%area(ix,iy)
    slope%landcover_idx(k) = slope%landcover(ix,iy)

    slope%diffusion_idx(k) = lc%diffusion
    slope%ns_idx(k) = lc%ns
    slope%soildepth_idx(k) = lc%soildepth
    slope%gammaa_idx(k) = lc%gammaa

    slope%zb_idx(k) = slope%zs(ix,iy) - slope%soildepth_idx(k)

    slope%ksv_idx(k) = lc%ksv
    slope%faif_idx(k) = lc%faif
    slope%infilt_limit_idx(k) = lc%infilt_limit

    slope%ka_idx(k) = lc%ka
    slope%km_idx(k) = lc%km

    slope%gammam_idx(k) = lc%gammam
    slope%beta_idx(k) = lc%beta
    slope%da_idx(k) = lc%da
    slope%dm_idx(k) = lc%dm

    slope%ksg_idx(k) = lc%ksg
    slope%gammag_idx(k) = lc%gammag
    slope%kg0_idx(k) = lc%kg0
    slope%fpg_idx(k) = lc%fpg
    slope%rgl_idx(k) = lc%rgl
  enddo  ! ix/
  enddo  ! iy/

  ! Search for downstream cell
  k = 0
  slope%down_idx(:,:) = -1
  slope%up_idx(:,:) = -1
  do iy = 1, slope%ny
  do ix = 1, slope%nx
    if( slope%domain_mask(ix,iy) == DOMAIN__OUTSIDE ) cycle

    k = k + 1

    ! 8-direction: nDir = 4, 4-direction: nDir = 2
    do l = 1, slope%nDir ! (1: right，2: down, 3: right down, 4: left down)
      selectcase( l )
      case( 1 )
        xx = ix + 1
        yy = iy
        ! len = dy / 2
        len = dist_sphere(&
            east_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, north_of_y(grid, iy)*d2r) &
            / 2.d0 * grid%earth_r
      case( 2 )
        xx = ix
        yy = iy + 1
        ! len = dx / 2
        len = dist_sphere(&
            west_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r) &
            / 2.d0 * grid%earth_r
      case( 3 )
        xx = ix + 1
        yy = iy + 1
        ! len = sqrt(dx**2 + dy**2) / 4
        len = dist_sphere(&
            west_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, north_of_y(grid, iy)*d2r) &
            / 4.d0 * grid%earth_r
      case( 4 )
        xx = ix - 1
        yy = iy + 1
        len = dist_sphere(&
            west_of_x(grid, ix)*d2r, north_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r) &
            / 4.d0 * grid%earth_r
      endselect

      if( xx < 1 .or. xx > slope%nx .or. &
          yy < 1 .or. yy > slope%ny ) cycle
      if( slope%domain_mask(xx,yy) == DOMAIN__OUTSIDE ) cycle

      slope%down_idx(l,k) = slope%ij2idx(xx,yy)
      slope%up_idx(l,slope%down_idx(l,k)) = k
      slope%dis_idx(l,k) = dist_sphere(&
        lon_center_of_x(grid, ix)*d2r, lat_center_of_y(grid, iy)*d2r, &
        lon_center_of_x(grid, xx)*d2r, lat_center_of_y(grid, yy)*d2r &
      ) * grid%earth_r
      slope%len_idx(l,k) = len
    enddo  ! l/
  enddo  ! ix/
  enddo  ! iy/

  call logmsg('Grid boundary length mean: '//&
    str(sum(slope%len_idx,mask=slope%len_idx>0.d0)/count(slope%len_idx>0.d0))//&
    ' min: '//str(minval(slope%len_idx,mask=slope%len_idx>0.d0))//&
    ' max: '//str(maxval(slope%len_idx)))
  call logmsg('Distance between grids mean: '//&
    str(sum(slope%dis_idx,mask=slope%dis_idx>0.d0)/count(slope%dis_idx>0.d0))//&
    ' min: '//str(minval(slope%dis_idx,mask=slope%dis_idx>0.d0))//&
    ' max: '//str(maxval(slope%dis_idx)))
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine build_slope_properties
!===============================================================
!
!===============================================================
subroutine build_river_properties(river)
  implicit none
  type(static_river_), intent(inout) :: river
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_properties'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building river properties')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  river%ns = config%param%river%ns
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine build_river_properties
!===============================================================
!
!===============================================================
subroutine build_river_network(grid, river)
  use mod_mesh, only: &
       xs_of_lon, &
       ys_of_lat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_network'

  type(grid_), intent(in) :: grid
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch, ch_down
  type(point_)  , pointer :: pt
  type(node_)   , pointer :: nd, nd1, nd2
  type(ch_conn_), pointer :: ch_conn
  type(nd_conn_), pointer :: nd_conn
  type(outlet_) , pointer :: outlet
  type(source_) , pointer :: src
  real(8), allocatable :: nd_lon(:), nd_lat(:)
  integer, allocatable :: nd_iCh(:), nd_jNode(:)
  integer :: k
  integer :: iPt
  integer :: jNode
  integer :: nnNode
  integer :: kNode, ksNode, keNode, k0Node, kksNode, kkeNode
  integer :: iNode_conn, iNode_conn_down
  integer :: iSource
  real(8) :: downleng_min
  integer, allocatable :: arg(:)

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building river network')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating length of sections')

  do k = 1, river%nCh
    ch => river%channel(k)

    ch%leng = 0.d0
    do iPt = 1, ch%nPt-1
      call add(ch%leng, dist_sphere(&
        ch%pt(iPt)%lon*d2r, ch%pt(iPt)%lat*d2r, &
        ch%pt(iPt+1)%lon*d2r, ch%pt(iPt+1)%lat*d2r &
      ) * grid%earth_r)
    enddo  ! iPt/
  enddo  ! k/

  call logmsg('leng mean: '//str(sum(river%channel(:)%leng)/river%nCh)//&
    ' min: '//str(minval(river%channel(:)%leng))//&
    ' max: '//str(maxval(river%channel(:)%leng)))

  call logext()
  !-------------------------------------------------------------
  ! Add data of points and nodes
  !-------------------------------------------------------------
  call logent('Adding data of points and nodes')

  do k = 1, river%nCh
    ch => river%channel(k)

    do iPt = 1, ch%nPt
      pt => ch%pt(iPt)
      pt%x = xs_of_lon(grid, pt%lon)
      pt%y = ys_of_lat(grid, pt%lat)
      if( pt%lon == grid%east ) pt%x = grid%nx
      if( pt%lat == grid%south ) pt%y = grid%ny
      if( pt%x < 1 .or. pt%x > grid%nx .or. pt%y < 1 .or. pt%y > grid%ny )then
        if( river%allow_channels_outside_domain )then
          call logmsg('Point is outside the domain.'//&
                     '\n  channel '//str(k)//' point '//str(iPt)//&
                     ' (x,y): ('//str((/pt%x,pt%y/),',')//')')
        else
          call errend('Point is outside the domain.'//&
                     '\n  channel '//str(k)//' point '//str(iPt)//&
                     ' (x,y): ('//str((/pt%x,pt%y/),',')//')')
        endif
      endif
    enddo  ! iPt/

    nd1 => ch%node(1)
    nd2 => ch%node(2)
    nd1%lon = ch%pt(1)%lon
    nd1%lat = ch%pt(1)%lat
    nd2%lon = ch%pt(ch%nPt)%lon
    nd2%lat = ch%pt(ch%nPt)%lat
  enddo  ! k/

  call logext()
  !-------------------------------------------------------------
  ! Count nodes and store index in $river%channel%node
  !-------------------------------------------------------------
  call logent('Counting nodes and storing index')

  nnNode = river%nCh * 2
  allocate(nd_lon(nnNode))
  allocate(nd_lat(nnNode))
  allocate(nd_iCh(nnNode))
  allocate(nd_jNode(nnNode))

  kNode = 0
  do k = 1, river%nCh
    ch => river%channel(k)

    call add(kNode)
    nd_lon(kNode) = ch%node(1)%lon
    nd_lat(kNode) = ch%node(1)%lat
    nd_iCh(kNode) = k
    nd_jNode(kNode) = 1

    call add(kNode)
    nd_lon(kNode) = ch%node(2)%lon
    nd_lat(kNode) = ch%node(2)%lat
    nd_iCh(kNode) = k
    nd_jNode(kNode) = 2
  enddo  ! k/

  allocate(arg(nnNode))
  call argsort(nd_lon, arg)
  call sort(nd_lon, arg)
  call sort(nd_lat, arg)
  call sort(nd_iCh, arg)
  call sort(nd_jNode, arg)

  river%nNode = 0
  keNode = 0
  do while( keNode < nnNode )
    ksNode = keNode + 1
    keNode = ksNode
    do while( keNode < nnNode )
      if( nd_lon(keNode+1) /= nd_lon(ksNode) ) exit
      keNode = keNode + 1
    enddo

    if( ksNode == keNode )then
      call add(river%nNode)
      cycle
    endif

    call argsort(nd_lat(ksNode:keNode), arg(ksNode:keNode))
    call sort(nd_lat(ksNode:keNode), arg(ksNode:keNode))
    call sort(nd_iCh(ksNode:keNode), arg(ksNode:keNode))
    call sort(nd_jNode(ksNode:keNode), arg(ksNode:keNode))

    kkeNode = ksNode - 1
    do while( kkeNode < keNode )
      kksNode = kkeNode + 1
      kkeNode = kksNode
      do while( kkeNode < keNode )
        if( nd_lat(kkeNode+1) /= nd_lat(kksNode) ) exit
        kkeNode = kkeNode + 1
      enddo
      call add(river%nNode)

      do kNode = kksNode, kkeNode
        river%channel(nd_iCh(kNode))%node(nd_jNode(kNode))%iNode = river%nNode
      enddo

    enddo  ! kkeNode/
  enddo  ! keNode/

  call logmsg('Nodes: '//str(river%nNode))

  call logext()
  !-------------------------------------------------------------
  ! Store indices of outlet nodes
  !-------------------------------------------------------------
  call logent('Storing indices of outlet nodes')

  river%nOutlet = 0
  do k = 1, river%nCh
    ch => river%channel(k)
    do jNode = 1, 2
      nd => ch%node(jNode)
      if( nd%is_outlet ) call add(river%nOutlet)
    enddo  ! jNode/
  enddo  ! k/

  call logmsg('Outlets: '//str(river%nOutlet))
  allocate(river%outlet(river%nOutlet))

  river%nOutlet = 0
  do k = 1, river%nCh
    ch => river%channel(k)
    do jNode = 1, 2
      nd => ch%node(jNode)
      if( .not. nd%is_outlet ) cycle
      call add(river%nOutlet)
      outlet => river%outlet(river%nOutlet)
      outlet%iCh = k
      outlet%jNode = jNode
      outlet%iNode = nd%iNode
    enddo  ! jNode/
  enddo  ! k/

  call logext()
  !-------------------------------------------------------------
  ! Calc. connections
  !-------------------------------------------------------------
  call logent('Calculating connections')

  do k = 1, river%nCh
    ch => river%channel(k)

    do jNode = 1, 2
      nd => ch%node(jNode)

      call search_nearest(nd%lon, nd_lon, ksNode, keNode)
      if( ksNode == keNode )then
        if( nd_iCh(ksNode) /= k )then
          call errend(msg_unexpected_condition()//&
                    '\n  ksNode == keNode .and. nd_iCh(ksNode) /= k'//&
                    '\nLongitude was not found.')
        endif
        nd%nNode_conn = 0
        cycle
      elseif( ksNode < 1 .or. keNode > nnNode )then
        call errend(msg_unexpected_condition()//&
                  '\n  ksNode < 1 .or. keNode > nnNode'//&
                  '\nLongitude is out of range.')
      endif
      k0Node = ksNode - 1
      call search_nearest(nd%lat, nd_lat(ksNode:keNode), ksNode, keNode)
      ksNode = ksNode + k0Node
      keNode = keNode + k0Node
      if( ksNode == keNode )then
        if( nd_iCh(ksNode) /= k )then
          call errend(msg_unexpected_condition()//&
                    '\n  nd_iCh(ksNode) /= k'//&
                    '\nLatitude was not found.')
        endif
        nd%nNode_conn = 0
        cycle
      elseif( ksNode < 1 .or. keNode > nnNode )then
        call errend(msg_unexpected_condition()//&
                  '\n  ksNode < 1 .or. keNode > nnNode'//&
                  '\nLatitude is out of range.')
      endif

      nd%nNode_conn = keNode - ksNode
      allocate(nd%node_conn(nd%nNode_conn))

      nd%nNode_conn = 0
      do kNode = ksNode, keNode
        if( nd_iCh(kNode) == k ) cycle
        call add(nd%nNode_conn)
        nd%node_conn(nd%nNode_conn)%iCh = nd_iCh(kNode)
        nd%node_conn(nd%nNode_conn)%jNode = nd_jNode(kNode)
      enddo
      if( nd%nNode_conn /= size(nd%node_conn) )then
        call errend(msg_unexpected_condition()//&
          '\n  nd%nNode_conn /= size(nd%node_conn)'//&
          '\nchannel #'//str(k)//' node #'//str(jNode)//&
          '\nnd%nNode_conn: '//str(nd%nNode_conn)//&
          '\nsize(nd%node_conn): '//str(size(nd%node_conn)))
      endif
    enddo  ! jNode/

    ch%nCh_conn = sum(ch%node(:)%nNode_conn)
    allocate(ch%ch_conn(ch%nCh_conn))
    ch%nCh_conn = 0
    do jNode = 1, 2
      nd => ch%node(jNode)
      do iNode_conn = 1, nd%nNode_conn
        nd_conn => nd%node_conn(iNode_conn)
        call add(ch%nCh_conn)
        ch_conn => ch%ch_conn(ch%nCh_conn)
        ch_conn%jNode_self = jNode
        ch_conn%iCh   = nd_conn%iCh
        ch_conn%jNode = nd_conn%jNode
      enddo
    enddo
  enddo  ! k/

  call logext()
  !-------------------------------------------------------------
  ! Make lists of channels from source to outlet
  !-------------------------------------------------------------
  call logent('Searching for routes from source to outlet')

  river%nSource = 0
  do k = 1, river%nCh
    if( river%channel(k)%node(1)%nNode_conn == 0 ) call add(river%nSource)
  enddo

  call logmsg('Sources: '//str(river%nSource))

  allocate(river%source(river%nSource))

  iSource = 0
  do k = 1, river%nCh
    ch => river%channel(k)

    nd => ch%node(1)
    if( nd%nNode_conn > 0 ) cycle
    call add(iSource)
    nd2 => ch%node(2)

    src => river%source(iSource)
    allocate(src%iCh(river%nCh))
    src%nCh = 1
    src%iCh(1) = k

    do while( .not. nd2%is_outlet )
      downleng_min = nd%downleng
      iNode_conn_down = 0
      do iNode_conn = 1, nd2%nNode_conn
        nd_conn => nd2%node_conn(iNode_conn)
        if( nd_conn%jNode /= 1 ) cycle
        ch_down => river%channel(nd_conn%iCh)
        if( ch_down%node(2)%downleng < downleng_min )then
          iNode_conn_down = iNode_conn
        endif
      enddo

      if( iNode_conn_down == 0 )then
        call errend(msg_unexpected_condition()//&
          '\n  iNode_conn_down == 0')
      endif

      nd_conn => nd2%node_conn(iNode_conn_down)
      ch => river%channel(nd_conn%iCh)
      nd => ch%node(1)
      nd2 => ch%node(2)

      call add(src%nCh)
      src%iCh(src%nCh) = nd_conn%iCh
    enddo  ! while .not. nd2%is_outlet

    call realloc(src%iCh, src%nCh, clear=.false.)
    call logmsg('source#'//str(iSource,dgt(river%nSource))//&
      ' dist: '//str(river%channel(src%iCh(1))%node(1)%downleng*1d-3,'f8.3')//&
      ' channels: '//str(src%nCh))
  enddo  ! k/

  iSource = 0
  do k = 1, river%nCh
    ch => river%channel(k)

    nd => ch%node(1)
    if( nd%nNode_conn > 0 ) cycle
    call add(iSource)

    src => river%source(iSource)
  enddo  ! k/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(nd_lon)
  deallocate(nd_lat)
  deallocate(nd_iCh)
  deallocate(nd_jNode)
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine build_river_network
!===============================================================
!
!===============================================================
subroutine build_river_crosssection(river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_crosssection'

  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  integer :: iCh

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building river cross section')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( river%crssct_method )

  case( RIVER_CRSSCT_METHOD__RECTANGULAR )
    allocate(river%area_idx(river%nCh))
    do iCh = 1, river%nCh
      ch => river%channel(iCh)

      ch%area = ch%width * ch%leng
      ch%volume = ch%area * (ch%levee + ch%depth)

      river%area_idx(iCh) = ch%area
    enddo

    call logmsg('river area min: '//str(minval(river%area_idx))//&
      ' max: '//str(maxval(river%area_idx)))

  case( RIVER_CRSSCT_METHOD__EXPLICIT )
    ! TMP

  case default
    call errend(msg_invalid_value('river%crssct_method', river%crssct_method))
  endselect
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine build_river_crosssection
!===============================================================
!
!===============================================================
subroutine build_slope_river_mapping(grid, river)
  use mod_mesh, only: &
        west_of_x, &
        east_of_x, &
        south_of_y, &
        north_of_y, &
        xs_of_lon, &
        xe_of_lon, &
        ys_of_lat, &
        ye_of_lat, &
        apprx_isct_with_meridian, &
        apprx_isct_with_parallel
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_slope_river_mapping'

  type(grid_), intent(in) :: grid
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  type(point_)  , pointer :: pt1, pt2
  type(ch_mesh_), pointer :: mesh
  real(8) :: wlon, wlat, elon, elat
  real(8) :: clon_west, clat_west, clon_east, clat_east
  real(8) :: dlon_west, dlat_west, dlon_east, dlat_east
  integer :: xs, xe, ix, ys, ye, iy
  real(8) :: leng, leng_sum
  integer :: k
  integer :: iPt
  integer :: iMesh
  integer :: szIsct_ch, nIsct_ch
  integer :: szIsct_pt, nIsct_pt
  integer :: is, ie, iis, iie
  integer, pointer :: isct_pt_x(:), isct_pt_y(:)
  real(8), pointer :: isct_pt_leng(:)
  integer, pointer :: isct_ch_x(:), isct_ch_y(:)
  real(8), pointer :: isct_ch_leng(:)
  integer, pointer :: arg(:)

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building slope and river mapping')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  szIsct_pt = river%nCh * 100
  allocate(isct_pt_x(szIsct_pt))
  allocate(isct_pt_y(szIsct_pt))
  allocate(isct_pt_leng(szIsct_pt))
  nullify(arg)

  szIsct_ch = river%nCh * 10
  allocate(isct_ch_x(szIsct_ch))
  allocate(isct_ch_y(szIsct_ch))
  allocate(isct_ch_leng(szIsct_ch))

  do k = 1, river%nCh
    ch => river%channel(k)
    !-----------------------------------------------------------
    ! Calc. intersections for each section
    !-----------------------------------------------------------
    nIsct_pt = 0
    do iPt = 1, ch%nPt-1
      pt1 => ch%pt(iPt)
      pt2 => ch%pt(iPt+1)

      if( pt1%lon < pt2%lon )then
        wlon = pt1%lon
        wlat = pt1%lat
        elon = pt2%lon
        elat = pt2%lat
      else
        wlon = pt2%lon
        wlat = pt2%lat
        elon = pt1%lon
        elat = pt1%lat
      endif
      !---------------------------------------------------------
      ! Case: north to south
      if( wlat > elat )then
        ys = ys_of_lat(grid, wlat)
        ye = ye_of_lat(grid, elat)
        do iy = ys, ye
          if( iy == ys )then
            clat_west = wlat
            clon_west = wlon
          else
            clat_west = clat_east
            clon_west = clon_east
          endif
          if( iy == ye )then
            clat_east = elat
            clon_east = elon
          else
            clat_east = south_of_y(grid, iy)
            clon_east = apprx_isct_with_parallel(&
                wlon, wlat, elon, elat, clat_east)
          endif

          call func_x()
        enddo  ! iy/
      !---------------------------------------------------------
      ! Case: south to north
      else
        ys = ys_of_lat(grid, elat)
        ye = ye_of_lat(grid, wlat)
        do iy = ye, ys, -1
          if( iy == ye )then
            clat_west = wlat
            clon_west = wlon
          else
            clat_west = clat_east
            clon_west = clon_east
          endif
          if( iy == ys )then
            clat_east = elat
            clon_east = elon
          else
            clat_east = north_of_y(grid, iy)
            clon_east = apprx_isct_with_parallel(&
              wlon, wlat, elon, elat, clat_east)
          endif

          call func_x()
        enddo  ! iy/
      endif
    enddo  ! iPt/
    !-----------------------------------------------------------
    ! Sum. leng. for each pair of (x,y)
    !-----------------------------------------------------------
    call realloc(arg, szIsct_pt)
    call argsort(isct_pt_x(:nIsct_pt), arg(:nIsct_pt))
    call sort(isct_pt_x(:nIsct_pt), arg(:nIsct_pt))
    call sort(isct_pt_y(:nIsct_pt), arg(:nIsct_pt))
    call sort(isct_pt_leng(:nIsct_pt), arg(:nIsct_pt))

    nIsct_ch = 0
    ie = 0
    do while( ie < nIsct_pt )
      is = ie + 1
      ie = is
      do while( ie < nIsct_pt )
        if( isct_pt_x(ie+1) /= isct_pt_x(is) ) exit
        ie = ie + 1
      enddo
      call argsort(isct_pt_y(is:ie), arg(is:ie))
      call sort(isct_pt_y(is:ie), arg(is:ie))
      call sort(isct_pt_leng(is:ie), arg(is:ie))
      iie = is - 1
      do while( iie < ie )
        iis = iie + 1
        iie = iis
        do while( iie < ie )
          if( isct_pt_y(iie+1) /= isct_pt_y(iis) ) exit
          iie = iie + 1
        enddo
        if( nIsct_ch == szIsct_ch )then
          call mul(szIsct_ch, 2)
          call realloc(isct_ch_x, szIsct_ch, clear=.false.)
          call realloc(isct_ch_y, szIsct_ch, clear=.false.)
          call realloc(isct_ch_leng, szIsct_ch, clear=.false.)
        endif
        call add(nIsct_ch)
        isct_ch_x(nIsct_ch) = isct_pt_x(iis)
        isct_ch_y(nIsct_ch) = isct_pt_y(iis)
        isct_ch_leng(nIsct_ch) = sum(isct_pt_leng(iis:iie))
      enddo  ! iie/
    enddo  ! ie/

    ch%nMesh = nIsct_ch
    allocate(ch%mesh(ch%nMesh))
    leng_sum = sum(isct_ch_leng(:ch%nMesh))
    do iMesh = 1, ch%nMesh
      mesh => ch%mesh(iMesh)
      mesh%x = isct_ch_x(iMesh)
      mesh%y = isct_ch_y(iMesh)
      mesh%leng = ch%leng * isct_ch_leng(iMesh) / leng_sum
      mesh%area = mesh%leng * ch%width

      mesh%is_outside_domain = .false.
      if( mesh%x < 1 .or. mesh%x > grid%nx .or. &
          mesh%y < 1 .or. mesh%y > grid%ny )then
        mesh%is_outside_domain = .true.
      elseif( grid%domain_mask(mesh%x,mesh%y) == DOMAIN__OUTSIDE )then
        mesh%is_outside_domain = .true.
      endif
    enddo
  enddo  ! k/

  deallocate(arg)
  deallocate(isct_pt_x)
  deallocate(isct_pt_y)
  deallocate(isct_pt_leng)
  deallocate(isct_ch_x)
  deallocate(isct_ch_y)
  deallocate(isct_ch_leng)
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine func_x()
  implicit none

  xs = xs_of_lon(grid, clon_west)
  xe = xe_of_lon(grid, clon_east)
  do ix = xs, xe
    if( ix == xs )then
      dlon_west = clon_west
      dlat_west = clat_west
    else
      dlon_west = dlon_east
      dlat_west = clat_east
    endif
    if( ix == xe )then
      dlon_east = clon_east
      dlat_east = clat_east
    else
      dlon_east = east_of_x(grid, ix)
      dlat_east = apprx_isct_with_meridian(&
        wlon, wlat, elon, elat, dlon_east)
    endif

    leng = dist_sphere(&
        dlon_west*d2r, dlat_west*d2r, &
        dlon_east*d2r, dlat_east*d2r &
    ) * grid%earth_r
    if( leng < 1d-12 ) cycle

    if( nIsct_pt == szIsct_pt )then
      call mul(szIsct_pt, 2)
      call realloc(isct_pt_x, szIsct_pt, clear=.false.)
      call realloc(isct_pt_y, szIsct_pt, clear=.false.)
      call realloc(isct_pt_leng, szIsct_pt, clear=.false.)
    endif
    call add(nIsct_pt)
    isct_pt_x(nIsct_pt) = ix
    isct_pt_y(nIsct_pt) = iy
    isct_pt_leng(nIsct_pt) = leng

    if( .not. river%allow_channels_outside_domain )then
      if( grid%domain_mask(ix,iy) == DOMAIN__OUTSIDE )then
        call errend('Channel intersects the mesh outside the domain.'//&
                  '\nchannel '//str(k)//&
                  '\npoint '//str(iPt)//' ('//str(pt1%lon,'f12.7')//','//str(pt1%lat,'f11.7')//')'//&
                  '\n      '//str(iPt+1)//' ('//str(pt2%lon,'f12.7')//','//str(pt2%lat,'f11.7')//')'//&
                  '\(x,y): '//str((/ix,iy/),' - '))
      endif
    endif
  enddo  ! ix/
end subroutine func_x
!---------------------------------------------------------------
end subroutine build_slope_river_mapping
!===============================================================
!
!===============================================================
subroutine build_river_topography(grid, slope, river)
  use mod_mesh, only: &
    xs_of_lon, &
    ys_of_lat
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_topography'
  type(grid_), intent(in) :: grid
  type(static_slope_), intent(in) :: slope
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  type(ch_mesh_), pointer :: chmesh
  type(outlet_), pointer :: outlet
  type(node_), pointer :: nd
  real(8) :: leng_sum
  integer :: iCh
  integer :: iMesh
  integer :: iOutlet
  integer :: x, y

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  call logent('Building river topography')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(river%zb_idx(river%nCh))

  do iCh = 1, river%nCh
    ch => river%channel(iCh)

    ch%zb = 0.d0
    leng_sum = 0.d0
    do iMesh = 1, ch%nMesh
      chmesh => ch%mesh(iMesh)
      if( chmesh%is_outside_domain ) cycle
      call add(ch%zb, (slope%zs(chmesh%x,chmesh%y) - ch%depth) * chmesh%leng)
      call add(leng_sum, chmesh%leng)
    enddo

    ! TMP
    if( leng_sum < 1d-12 )then
      !call logwrn('ch('//str(iCh)//') nMesh: '//str(ch%nMesh)//' leng: '//str(leng_sum))
      ch%zb = -ch%depth
    else
      call mul(ch%zb, 1.d0/leng_sum)
    endif

    river%zb_idx(iCh) = ch%zb
  enddo  ! iCh/

  do iOutlet = 1, river%nOutlet
    outlet => river%outlet(iOutlet)
    ch => river%channel(outlet%iCh)

    nd => ch%node(outlet%jNode)
    x = xs_of_lon(grid, nd%lon)
    y = ys_of_lat(grid, nd%lat)

    if( slope%domain_mask(x,y) == DOMAIN__OUTSIDE )then
      outlet%zb = 0.d0 - ch%depth
    else
      outlet%zb = slope%zs(x,y) - ch%depth
    endif

    outlet%v_sealevel = max(-outlet%zb, 0.d0) * ch%area

    call logmsg('outlet#'//str(iOutlet)//' ch#'//str(outlet%iCh)//&
      ' depth '//str(ch%depth)//' zb '//str(outlet%zb)//&
      ' area '//str(ch%area))
  enddo
  !-------------------------------------------------------------
  call logext()
  call logret(PRCNAM, MODNAM)
end subroutine build_river_topography
!===============================================================
!
!===============================================================
end module mod_config
