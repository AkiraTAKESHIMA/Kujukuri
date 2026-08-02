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
  implicit none
  private

  public :: prepare_static_data
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_config'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prepare_static_data(config, time, grid, static)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prepare_static_data'
  type(config_), intent(out) :: config
  type(time_), intent(out) :: time
  type(grid_), intent(out) :: grid
  type(static_), intent(out) :: static

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_config(config)

  call setup_time(config, time)

  call setup_grid(config, grid, static%slope)

  call load_input_data(config, grid, static)

  call build_static_data(config, grid, static)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine prepare_static_data
!===============================================================
!
!===============================================================
subroutine read_config(config)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_config'
  type(config_), intent(out), target :: config

  type(param_slope_), pointer :: slope
  type(input_river_section_), pointer :: section

  integer :: i

  character(CLEN_PATH) :: file_conf
  integer :: un
  character(1) :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  file_conf = argument(1)
  open(newunit=un, file=file_conf, status='old')
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  read(un,*) c_, config%output%dir
  read(un,*) c_, config%output%overwrite
  read(un,*) c_, config%output%interval
  !-------------------------------------------------------------
  ! Simulation
  !-------------------------------------------------------------
  read(un,*)
  read(un,*) c_, config%simulation%sdatetime_start
  read(un,*) c_, config%simulation%sdatetime_end
  !-------------------------------------------------------------
  ! Timestep
  !-------------------------------------------------------------
  read(un,*)
  read(un,*) c_, config%timestep%model
  read(un,*) c_, config%timestep%slope
  read(un,*) c_, config%timestep%river
  !-------------------------------------------------------------
  ! Adaptive Runge-Kutta
  !-------------------------------------------------------------
  read(un,*)
  read(un,*) c_, config%adaptive_rk%error_tolerance
  read(un,*) c_, config%adaptive_rk%dt_min_river
  read(un,*) c_, config%adaptive_rk%dt_min_slope
  !-------------------------------------------------------------
  ! Domain
  !-------------------------------------------------------------
  read(un,*)
  read(un,*) c_, config%domain%west
  read(un,*) c_, config%domain%east
  read(un,*) c_, config%domain%south
  read(un,*) c_, config%domain%north
  read(un,*) c_, config%domain%nx
  read(un,*) c_, config%domain%ny
  !-------------------------------------------------------------
  ! Input
  !-------------------------------------------------------------
  read(un,*)
  read(un,*) c_, config%input%topography%file_elv
  read(un,*) c_, config%input%topography%file_flwdir
  read(un,*) c_, config%input%landcover%file_landuse
  read(un,*)
  read(un,*) c_, config%input%river%network%file_network
  read(un,*) c_, config%input%river%network%allow_channels_outside_domain
  read(un,*)
  read(un,*) c_, config%input%forcing%file_prcp
  !-------------------------------------------------------------
  ! River parameters
  !-------------------------------------------------------------
  read(un,*)
  read(un,*) c_, config%param%river%ns
  !-------------------------------------------------------------
  ! Slope parameters
  !-------------------------------------------------------------
  slope => config%param%slope

  read(un,*)
  read(un,*) c_, slope%num_landuse

  allocate(slope%diffusion(slope%num_landuse))
  allocate(slope%ns(slope%num_landuse))
  allocate(slope%soildepth(slope%num_landuse))
  allocate(slope%gammaa(slope%num_landuse))
  allocate(slope%ksv(slope%num_landuse))
  allocate(slope%faif(slope%num_landuse))
  allocate(slope%infilt_limit(slope%num_landuse))
  allocate(slope%ka(slope%num_landuse))
  allocate(slope%gammam(slope%num_landuse))
  allocate(slope%beta(slope%num_landuse))
  allocate(slope%da(slope%num_landuse))
  allocate(slope%dm(slope%num_landuse))
  allocate(slope%km(slope%num_landuse))
  allocate(slope%ksg(slope%num_landuse))
  allocate(slope%gammag(slope%num_landuse))
  allocate(slope%kg0(slope%num_landuse))
  allocate(slope%fpg(slope%num_landuse))
  allocate(slope%rgl(slope%num_landuse))

  read(un,*)
  read(un,*) c_, slope%diffusion(:)
  read(un,*) c_, slope%ns(:)
  read(un,*) c_, slope%soildepth(:)
  read(un,*) c_, slope%gammaa(:)

  read(un,*)
  read(un,*) c_, slope%ksv(:)
  read(un,*) c_, slope%faif(:)

  read(un,*)
  read(un,*) c_, slope%ka(:)
  read(un,*) c_, slope%gammam(:)
  read(un,*) c_, slope%beta(:)

  read(un,*)
  read(un,*) c_, slope%ksg(:)
  read(un,*) c_, slope%gammag(:)
  read(un,*) c_, slope%kg0(:)
  read(un,*) c_, slope%fpg(:)
  read(un,*) c_, slope%rgl(:)

  nullify(slope)
  !-------------------------------------------------------------
  ! River section
  !-------------------------------------------------------------
  section => config%input%river%section

  read(un,*)
  read(un,*) c_, section%method

  if( section%method == RIVER_SECTION_METHOD__AREA_ESTIMATION )then
    read(un,*) c_, section%area_estimation%width_c
    read(un,*) c_, section%area_estimation%width_s
    read(un,*) c_, section%area_estimation%depth_c
    read(un,*) c_, section%area_estimation%depth_s
    read(un,*) c_, section%area_estimation%levee_height
    read(un,*) c_, section%area_estimation%levee_upa_thresh
  else
    do i = 1, 6
      read(un,*)
    enddo
  endif

  if( section%method == RIVER_SECTION_METHOD__RECTANGULAR )then
    read(un,*)
    read(un,*) c_, section%rectangular%file_width
    read(un,*) c_, section%rectangular%file_depth
    read(un,*) c_, section%rectangular%file_levee
  else
    do i = 1, 4
      read(un,*)
    enddo
  endif

  if( section%method == RIVER_SECTION_METHOD__EXPLICIT )then
    read(un,*)
    read(un,*) c_, section%explicit%dir
  else
    do i = 1, 2
      read(un,*)
    enddo
  endif

  nullify(section)
  !-------------------------------------------------------------
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_config
!===============================================================
!
!===============================================================
subroutine setup_time(config, time)
  use mod_util, only: &
    stime2time
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_time'
  type(config_), intent(in) :: config
  type(time_), intent(inout) :: time

  integer :: year_start

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(time%datetime_start(7))
  allocate(time%datetime_end(7))
  allocate(time%datetime_base(7))
  call stime2time(config%simulation%sdatetime_start, time%datetime_start)
  call stime2time(config%simulation%sdatetime_end, time%datetime_end)

  year_start = time%datetime_start(1)

  time%datetime_base(:) = 0
  time%datetime_base(1) = year_start

  time%t_start = second(time%datetime_start, year_start)
  time%t_end = second(time%datetime_end, year_start)

  time%dt_model = config%timestep%model
  time%dt_river = config%timestep%river
  time%dt_slope = config%timestep%slope

  call logmsg('datetime start: '//str(time%datetime_start(1),-4)//'-'//&
    str(time%datetime_start(2:3),-2,'-')//'T'//str(time%datetime_start(5:7),-2,':')//&
    ' ('//str(time%t_start)//' sec from '//str(year_start)//'-01-01T00:00:00)')
  call logmsg('         end  : '//str(time%datetime_end(1),-4)//'-'//&
    str(time%datetime_end(2:3),-2,'-')//'T'//str(time%datetime_end(5:7),-2,':')//&
    ' ('//str(time%t_end)//' sec from '//str(year_start)//'-01-01T00:00:00)')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine setup_time
!===============================================================
!
!===============================================================
subroutine setup_grid(config, grid, slope)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setup_grid'
  type(config_), intent(in) :: config
  type(grid_), intent(inout) :: grid
  type(static_slope_), intent(inout) :: slope

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grid%nx = config%domain%nx
  grid%ny = config%domain%ny
  grid%west = config%domain%west
  grid%east = config%domain%east
  grid%south = config%domain%south
  grid%north = config%domain%north

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
subroutine load_input_data(config, grid, static)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_input_data'
  type(config_), intent(in)    :: config
  type(grid_), intent(inout) :: grid
  type(static_) , intent(inout) :: static

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call load_topography(config, grid, static%slope)

  call load_landcover(config, static%slope)

  call load_river_data(config, static%river)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_input_data
!===============================================================
!
!===============================================================
subroutine load_topography(config, grid, slope)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_topography'
  type(config_), intent(in) :: config
  type(grid_), intent(inout) :: grid
  type(static_slope_), intent(inout) :: slope

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grid%domain_mask(grid%nx, grid%ny))
  allocate(slope%zs(grid%nx, grid%ny))

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
subroutine load_landcover(config, slope)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_landcover'
  type(config_), intent(in) :: config
  type(static_slope_), intent(inout) :: slope

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(slope%landuse(slope%nx, slope%ny))

  if( config%input%landcover%file_landuse == '' )then
    slope%landuse(:,:) = 1
  else
    call traperr( rbin(slope%landuse, config%input%landcover%file_landuse) )
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_landcover
!===============================================================
!
!===============================================================
subroutine load_river_data(config, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_river_data'
  type(config_), intent(in) :: config
  type(static_river_), intent(out) :: river

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call load_river_network(config, river)

  call load_river_section(config, river)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_river_data
!===============================================================
!
!===============================================================
subroutine load_river_network(config, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_river_network'
  type(config_), intent(in) :: config
  type(static_river_), intent(out) :: river

  type(channel_), pointer :: ch
  type(nd_), pointer :: nd1, nd2

  integer :: iCh
  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  river%allow_channels_outside_domain = config%input%river%network%allow_channels_outside_domain
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=config%input%river%network%file_network, status='old')

  read(un,*) c_, river%nCh
  allocate(river%channel(river%nCh))

  do iCh = 1, river%nCh
    ch => river%channel(iCh)

    read(un,*)

    allocate(ch%node(2))
    nd1 => ch%node(1)
    nd2 => ch%node(2)
    read(un,*) c_, nd1%is_outlet, nd2%is_outlet

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
subroutine load_river_section(config, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_river_section'
  type(config_), intent(in) :: config
  type(static_river_), intent(inout) :: river

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  river%section_method = config%input%river%section%method

  selectcase( river%section_method )
  case( RIVER_SECTION_METHOD__AREA_ESTIMATION )
    continue
  case( RIVER_SECTION_METHOD__RECTANGULAR )
    call load_river_section_rectangular(config, river)
  case( RIVER_SECTION_METHOD__EXPLICIT )
    !call load_river_section_explicit(config, river)
  case default
    call errend(msg_invalid_value(&
      'river%section_method', river%section_method &
    ))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_river_section
!===============================================================
!
!===============================================================
subroutine load_river_section_rectangular(config, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'load_river_section_rectangular'
  type(config_), intent(in) :: config
  type(static_river_), intent(inout) :: river

  integer :: iCh

  integer :: un
  character :: c_

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iCh = 1, river%nCh
    river%channel(iCh)%pt(:)%width = 5.d0
    river%channel(iCh)%pt(:)%depth = 1.d0
    river%channel(iCh)%pt(:)%levee = 0.d0
  enddo

!  open(newunit=un, file=config%input%river%section%rectangular%file_width, status='old')
!  do iCh = 1, river%nCh
!    read(un,*) c_, river%channel(iCh)%pt(:)%width
!  enddo  ! iCh/
!  close(un)

!  open(newunit=un, file=config%input%river%section%rectangular%file_depth, status='old')
!  do iCh = 1, river%nCh
!    read(un,*) c_, river%channel(iCh)%pt(:)%depth
!  enddo  ! iCh/
!  close(un)

!  open(newunit=un, file=config%input%river%section%rectangular%file_levee, status='old')
!  do iCh = 1, river%nCh
!    read(un,*) c_, river%channel(iCh)%pt(:)%levee
!  enddo  ! iCh/
!  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine load_river_section_rectangular
!===============================================================
!
!===============================================================
!===============================================================
!
!===============================================================
subroutine build_static_data(config, grid, static)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_static_data'
  type(config_), intent(in) :: config
  type(grid_), intent(inout) :: grid
  type(static_), intent(inout) :: static

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call build_grid(grid)

  call build_slope_properties(config, grid, static%slope)

  call build_river_properties(config, static%river)

  call build_river_network(grid, static%river)

  call build_river_section(static%river)

  call build_slope_river_mapping(grid, static%river)

  call build_river_topography(static%slope, static%river)
  !-------------------------------------------------------------
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

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grid%area(grid%nx, grid%ny))

  do iy = 1, grid%ny
  do ix = 1, grid%nx
    if( grid%domain_mask(ix,iy) == DOMAIN__OUTSIDE ) cycle

    grid%area(ix,iy) = area_sphere_rect( &
      south_of_y(grid,iy)*d2r, north_of_y(grid,iy)*d2r &
    ) * (east_of_x(grid,ix)-west_of_x(grid,ix)) * d2r * EARTH_R**2
  enddo  ! ix/
  enddo  ! iy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_grid
!===============================================================
!
!===============================================================
subroutine build_slope_properties(config, grid, slope)
  use mod_mesh, only: &
    west_of_x, &
    east_of_x, &
    south_of_y, &
    north_of_y, &
    lon_center_of_x, &
    lat_center_of_y
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_slope_properties'
  type(config_), intent(in), target :: config
  type(grid_), intent(in) :: grid
  type(static_slope_), intent(inout) :: slope

  type(param_slope_), pointer :: param
  integer :: k
  integer :: l
  integer :: ix, iy, xx, yy
  integer :: lnd
  real(8) :: len

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  param => config%param%slope

  slope%nSlo = count(slope%domain_mask /= DOMAIN__OUTSIDE)

  allocate(slope%idx2i(slope%nSlo))
  allocate(slope%idx2j(slope%nSlo))
  allocate(slope%ij2idx(slope%nx,slope%ny))

  allocate(slope%domain_mask_idx(slope%nSlo))

  allocate(slope%down_idx(slope%lmax,slope%nSlo))
  allocate(slope%area_idx(slope%nSlo))
  allocate(slope%dis_idx(slope%lmax,slope%nSlo))
  allocate(slope%len_idx(slope%lmax,slope%nSlo))
!  allocate(acc_slo_idx(nSlo))
!  allocate(down_slo_1d_idx(nSlo))
!  allocate(dis_slo_1d_idx(nSlo))
!  allocate(len_slo_1d_idx(nSlo))

  allocate(slope%landuse_idx(slope%nSlo))
  allocate(slope%zb_idx(slope%nSlo))

  allocate(slope%diffusion_idx(slope%nSlo))
  allocate(slope%ns_idx(slope%nSlo))
  allocate(slope%soildepth_idx(slope%nSlo))
  allocate(slope%gammaa_idx(slope%nSlo))

  allocate(slope%ksv_idx(slope%nSlo))
  allocate(slope%faif_idx(slope%nSlo))
  allocate(slope%infilt_limit_idx(slope%nSlo))
  allocate(slope%ka_idx(slope%nSlo))
  allocate(slope%km_idx(slope%nSlo))
  allocate(slope%gammam_idx(slope%nSlo))
  allocate(slope%beta_idx(slope%nSlo))
  allocate(slope%da_idx(slope%nSlo))
  allocate(slope%dm_idx(slope%nSlo))
  allocate(slope%ksg_idx(slope%nSlo))
  allocate(slope%gammag_idx(slope%nSlo))
  allocate(slope%kg0_idx(slope%nSlo))
  allocate(slope%fpg_idx(slope%nSlo))
  allocate(slope%rgl_idx(slope%nSlo))

  k = 0
  slope%down_idx(:,:) = -1
  do iy = 1, slope%ny
  do ix = 1, slope%nx
    if( slope%domain_mask(ix,iy) == DOMAIN__OUTSIDE ) cycle

    call add(k)
    slope%idx2i(k) = ix
    slope%idx2j(k) = iy
    slope%ij2idx(ix,iy) = k

    slope%domain_mask_idx(k) = slope%domain_mask(ix,iy)

!    slope%acc_idx(k) = slope%acc(ix,iy)
    slope%area_idx(k) = grid%area(ix,iy)
    slope%landuse_idx(k) = slope%landuse(ix,iy)

    lnd = slope%landuse_idx(k)

    slope%diffusion_idx(k) = param%diffusion(lnd)
    slope%ns_idx(k) = param%ns(lnd)
    slope%soildepth_idx(k) = param%soildepth(lnd)
    slope%gammaa_idx(k) = param%gammaa(lnd)

    slope%zb_idx(k) = slope%zs(ix,iy) - slope%soildepth_idx(k)

    slope%ksv_idx(k) = param%ksv(lnd)
    slope%faif_idx(k) = param%faif(lnd)
    slope%infilt_limit_idx(k) = param%infilt_limit(lnd)

    slope%ka_idx(k) = param%ka(lnd)
    slope%km_idx(k) = param%km(lnd)
    slope%gammam_idx(k) = param%gammam(lnd)
    slope%beta_idx(k) = param%beta(lnd)
    slope%da_idx(k) = param%da(lnd)
    slope%dm_idx(k) = param%dm(lnd)

    slope%ksg_idx(k) = param%ksg(lnd)
    slope%gammag_idx(k) = param%gammag(lnd)
    slope%kg0_idx(k) = param%kg0(lnd)
    slope%fpg_idx(k) = param%fpg(lnd)
    slope%rgl_idx(k) = param%rgl(lnd)
  enddo  ! ix/
  enddo  ! iy/

  ! Search for downstream cell
  k = 0
  slope%down_idx(:,:) = -1
  do iy = 1, slope%ny
  do ix = 1, slope%nx
    if( slope%domain_mask(ix,iy) == DOMAIN__OUTSIDE ) cycle

    ! 8-direction: lmax = 4, 4-direction: lmax = 2
    do l = 1, slope%lmax ! (1: right，2: down, 3: right down, 4: left down)
      selectcase( l )
      case( 1 )
        xx = ix + 1
        yy = iy
        ! len = dy / 2
        len = dist_sphere(&
            east_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, north_of_y(grid, iy)*d2r) &
            / 2.d0 * EARTH_R
      case( 2 )
        xx = ix
        yy = iy + 1
        ! len = dx / 2
        len = dist_sphere(&
            west_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r) &
            / 2.d0 * EARTH_R
      case( 3 )
        xx = ix + 1
        yy = iy + 1
        ! len = sqrt(dx**2 + dy**2) / 4
        len = dist_sphere(&
            west_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, north_of_y(grid, iy)*d2r) &
            / 4.d0 * EARTH_R
      case( 4 )
        xx = ix - 1
        yy = iy + 1
        len = dist_sphere(&
            west_of_x(grid, ix)*d2r, north_of_y(grid, iy)*d2r, &
            east_of_x(grid, ix)*d2r, south_of_y(grid, iy)*d2r) &
            / 4.d0 * EARTH_R
      endselect

      if( xx < 1 .or. xx > slope%nx .or. &
          yy < 1 .or. yy > slope%ny ) cycle
      if( slope%domain_mask(xx,yy) == DOMAIN__OUTSIDE ) cycle

      slope%down_idx(l,k) = slope%ij2idx(xx,yy)
      slope%dis_idx(l,k) = dist_sphere(&
        lon_center_of_x(grid, ix)*d2r, lat_center_of_y(grid, iy)*d2r, &
        lon_center_of_x(grid, xx)*d2r, lat_center_of_y(grid, yy)*d2r &
      ) * EARTH_R
      slope%len_idx(l,k) = len
    enddo  ! l/
  enddo  ! ix/
  enddo  ! iy/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(param)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_slope_properties
!===============================================================
!
!===============================================================
subroutine build_river_properties(config, river)
  implicit none
  type(config_), intent(in) :: config
  type(static_river_), intent(inout) :: river

  river%ns = config%param%river%ns
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

  type(channel_), pointer :: ch
  type(pt_), pointer :: pt
  type(nd_)     , pointer :: nd
  type(ch_conn_), pointer :: ch_conn
  type(nd_conn_), pointer :: nd_conn
  type(outlet_) , pointer :: outlet
  real(8), allocatable :: nd_lon(:), nd_lat(:)
  integer, allocatable :: nd_iCh(:), nd_jNode(:)
  integer :: iCh
  integer :: iPt
  integer :: jNode
  integer :: nnNode
  integer :: kNode, ksNode, keNode, k0Node, kksNode, kkeNode
  integer :: iNode_conn
  integer, allocatable :: arg(:)
  logical :: is_outside
  character(:), allocatable :: msg

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Get grid index of points
  !-------------------------------------------------------------
  call logent('Getting grid index of points')

  do iCh = 1, river%nCh
    ch => river%channel(iCh)

    do iPt = 1, ch%nPt
      pt => ch%pt(iPt)

      pt%x = xs_of_lon(grid, pt%lon)
      pt%y = ys_of_lat(grid, pt%lat)
      if( pt%lon == grid%east ) pt%x = grid%nx
      if( pt%lat == grid%south ) pt%y = grid%ny

      is_outside = .false.
      if( pt%x < 1 .or. pt%x > grid%nx .or. &
          pt%y < 1 .or. pt%y > grid%ny )then
        is_outside = .true.
      elseif( grid%domain_mask(pt%x, pt%y) == DOMAIN__OUTSIDE )then
        is_outside = .true.
      endif

      if( is_outside .and. &
          .not. river%allow_channels_outside_domain )then
        msg = 'Point is outside the domain.'//&
            '\n  channel '//str(iCh)//' point '//str(iPt)//&
              ' (x,y): ('//str((/pt%x,pt%y/),',')//')'
        call errend(trim(msg))
      endif
    enddo  ! iPt/
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  ! Get coords. of nodes
  !-------------------------------------------------------------
  call logent('Getting coords. of nodes')

  do iCh = 1, river%nCh
    ch => river%channel(iCh)
    ch%node(1)%lon = ch%pt(1)%lon
    ch%node(1)%lat = ch%pt(1)%lat
    ch%node(2)%lon = ch%pt(ch%nPt)%lon
    ch%node(2)%lat = ch%pt(ch%nPt)%lat
  enddo  ! iCh/

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
  do iCh = 1, river%nCh
    ch => river%channel(iCh)

    call add(kNode)
    nd_lon(kNode) = ch%node(1)%lon
    nd_lat(kNode) = ch%node(1)%lat
    nd_iCh(kNode) = iCh
    nd_jNode(kNode) = 1

    call add(kNode)
    nd_lon(kNode) = ch%node(2)%lon
    nd_lat(kNode) = ch%node(2)%lat
    nd_iCh(kNode) = iCh
    nd_jNode(kNode) = 2
  enddo  ! iCh/

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
      if( nd_lat(keNode+1) /= nd_lat(ksNode) ) exit
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

  deallocate(arg)

  call logext()
  !-------------------------------------------------------------
  ! Store indices of outlet nodes
  !-------------------------------------------------------------
  call logent('Storing indices of outlet nodes')

  river%nOutlet = 0
  do iCh = 1, river%nCh
    ch => river%channel(iCh)
    do jNode = 1, 2
      nd => ch%node(jNode)
      if( nd%is_outlet ) call add(river%nOutlet)
    enddo  ! jNode/
  enddo  ! iCh/

  call logmsg('Outlets: '//str(river%nOutlet))
  allocate(river%outlet(river%nOutlet))

  river%nOutlet = 0
  do iCh = 1, river%nCh
    ch => river%channel(iCh)
    do jNode = 1, 2
      nd => ch%node(jNode)
      if( .not. nd%is_outlet ) cycle
      call add(river%nOutlet)
      outlet => river%outlet(river%nOutlet)
      outlet%iCh = iCh
      outlet%jNode = jNode
      outlet%x = xs_of_lon(grid, nd%lon)
      outlet%y = ys_of_lat(grid, nd%lat)
    enddo  ! jNode/
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  ! Getting connections
  !-------------------------------------------------------------
  call logent('Getting connections')

  do iCh = 1, river%nCh
    ch => river%channel(iCh)

    do jNode = 1, 2
      nd => ch%node(jNode)

      call search_nearest(nd%lon, nd_lon, ksNode, keNode)
      if( ksNode == keNode )then
        if( nd_iCh(ksNode) /= iCh )then
          call errend(msg_unexpected_condition()//&
                    '\n  ksNode == keNode .and. nd_iCh(ksNode) /= iCh'//&
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
        if( nd_iCh(ksNode) /= iCh )then
          call errend(msg_unexpected_condition()//&
                    '\n  nd_iCh(ksNode) /= iCh'//&
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
        if( nd_iCh(kNode) == iCh ) cycle
        call add(nd%nNode_conn)
        nd%node_conn(nd%nNode_conn)%iCh = nd_iCh(kNode)
        nd%node_conn(nd%nNode_conn)%jNode = nd_jNode(kNode)
      enddo
      if( nd%nNode_conn /= size(nd%node_conn) )then
        call errend(msg_unexpected_condition()//&
                  '\n  nd%nNode_conn /= size(nd%node_conn)')
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
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(nd_lon)
  deallocate(nd_lat)
  deallocate(nd_iCh)
  deallocate(nd_jNode)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_river_network
!===============================================================
!
!===============================================================
subroutine build_river_section(river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_section'
  type(static_river_), intent(inout) :: river

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( river%section_method )
  case( RIVER_SECTION_METHOD__AREA_ESTIMATION )
    call errend(msg_not_implemented()//&
      '\n river%section_method: '//str(river%section_method))

  case( RIVER_SECTION_METHOD__RECTANGULAR )
    call build_river_section_rectangular(river)

  case( RIVER_SECTION_METHOD__EXPLICIT )
    call errend(msg_not_implemented()//&
      '\n river%section_method: '//str(river%section_method))

  case default
    call errend(msg_invalid_value(&
      'river%section_method', river%section_method &
    ))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_river_section
!===============================================================
!
!===============================================================
subroutine build_river_section_rectangular(river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_section_rectangular'
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  type(pt_), pointer :: pt1, pt2
  integer :: iCh
  integer :: iPt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(river%area_idx(river%nCh))

  do iCh = 1, river%nCh
    ch => river%channel(iCh)

    ! Length between points and length of channel
    ch%leng = 0.d0
    ch%width = 0.d0
    do iPt = 1, ch%nPt-1
      pt1 => ch%pt(iPt)
      pt2 => ch%pt(iPt+1)

      pt1%leng = dist_sphere(&
          pt1%lon*d2r, pt1%lat*d2r, pt2%lon*d2r, pt2%lat*d2r) &
          * EARTH_R

      call add(ch%leng, pt1%leng)

      call add(ch%width, (pt1%width + pt2%width)*0.5d0*pt1%leng)
      call add(ch%depth, (pt1%depth + pt2%depth)*0.5d0*pt1%leng)
      call add(ch%levee, (pt1%levee + pt2%levee)*0.5d0*pt1%leng)
    enddo  ! ip/
    ch%pt(ch%nPt)%leng = 0.d0

    call mul(ch%width, 1.d0/ch%leng)
    call mul(ch%depth, 1.d0/ch%leng)
    call mul(ch%levee, 1.d0/ch%leng)

    ch%area = ch%width * ch%leng

!    width_idx(iCh) = ch%width
!    depth_idx(iCh) = ch%depth
!    levee_idx(iCh) = ch%levee
    river%area_idx(iCh) = ch%area

    !call logmsg(str(iCh,dgt(river%nCh))//' width '//str(ch%width)//&
    !            ' depth '//str(ch%depth)//' levee '//str(ch%levee)//&
    !            ' leng '//str(ch%leng)//' area '//str(ch%area))
    if( ch%leng < 1d-12 )then
      call logwrn(str(iCh,dgt(river%nCh))//' leng '//str(ch%leng))
    endif
  enddo  ! iCh/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_river_section_rectangular
!===============================================================
!
!===============================================================
subroutine build_river_topography(slope, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_river_topography'
  type(static_slope_), intent(in) :: slope
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  type(ch_mesh_), pointer :: chmesh
  real(8) :: leng_sum
  integer :: iCh
  integer :: iMesh

  call logbgn(PRCNAM, MODNAM)
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
      ch%zb = 1d-3
    else
      call mul(ch%zb, 1.d0/leng_sum)
    endif

    river%zb_idx(iCh) = ch%zb
  enddo  ! iCh/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_river_topography
!===============================================================
!
!===============================================================
subroutine build_slope_river_mapping(grid, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'build_slope_river_mapping'
  type(grid_), intent(inout) :: grid
  type(static_river_), intent(inout) :: river

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_outlet_grid(grid, river)

  call build_slope_river_intersections(grid, river)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine build_slope_river_mapping
!===============================================================
!
!===============================================================
subroutine set_outlet_grid(grid, river)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_outlet_grid'
  type(grid_), intent(inout) :: grid
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  type(pt_), pointer :: pt
  type(nd_), pointer :: nd
  integer :: iCh
  integer :: iPt

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iCh = 1, river%nCh
    ch => river%channel(iCh)
    do iPt = 1, ch%nPt
      pt => ch%pt(iPt)

      if( river%allow_channels_outside_domain )then
        if( pt%x < 1 .or. pt%x > grid%nx .or. &
            pt%y < 1 .or. pt%y > grid%ny )then
          cycle
        elseif( grid%domain_mask(pt%x,pt%y) == DOMAIN__OUTSIDE )then
          cycle
        endif
      else
        if( pt%x < 1 .or. pt%x > grid%nx .or. &
            pt%y < 1 .or. pt%y > grid%ny )then
          call errend('Point is outside the domain.'//&
                     '\n  channel: '//str(iCh)//&
                     '\n  point: '//str(iPt)//&
                     '\n  (lon,lat): ('//str(pt%lon,'f12.7')//','//str(pt%lat,'f11.7')//')'//&
                     '\n  (x,y): ('//str((/pt%x,pt%y/),',')//')')
        elseif( grid%domain_mask(pt%x,pt%y) == DOMAIN__OUTSIDE )then
          call errend('Point is outside the domain.'//&
                     '\n  channel: '//str(iCh)//&
                     '\n  point: '//str(iPt)//&
                     '\n  (lon,lat): ('//str(pt%lon,'f12.7')//','//str(pt%lat,'f11.7')//')'//&
                     '\n  (x,y): ('//str((/pt%x,pt%y/),',')//')')
        endif
      endif

      if( iPt == 1 )then
        nd => ch%node(1)
      elseif( iPt == ch%nPt )then
        nd => ch%node(2)
      else
        cycle
      endif
      if( nd%is_outlet )then
        grid%domain_mask(pt%x,pt%y) = DOMAIN__OUTLET
      endif
    enddo  ! iPt/
  enddo  ! iCh/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_outlet_grid
!===============================================================
!
!===============================================================
subroutine build_slope_river_intersections(grid, river)
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
  character(CLEN_PROC), parameter :: PRCNAM = 'build_slope_river_intersections'
  type(grid_), intent(in) :: grid
  type(static_river_), intent(inout) :: river

  type(channel_), pointer :: ch
  type(pt_), pointer :: pt1, pt2
  type(ch_mesh_), pointer :: chmesh
  real(8) :: wlon, wlat, elon, elat
  real(8) :: clon_west, clat_west, clon_east, clat_east
  real(8) :: dlon_west, dlat_west, dlon_east, dlat_east
  integer :: xs, xe, ix, ys, ye, iy
  real(8) :: leng, leng_sum
  integer :: iCh
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

  call logbgn(PRCNAM, MODNAM)
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

  do iCh = 1, river%nCh
    ch => river%channel(iCh)
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
      chmesh => ch%mesh(iMesh)
      chmesh%x = isct_ch_x(iMesh)
      chmesh%y = isct_ch_y(iMesh)
      chmesh%leng = ch%leng * isct_ch_leng(iMesh) / leng_sum
      chmesh%area = chmesh%leng * ch%width

      chmesh%is_outside_domain = .false.
      if( chmesh%x < 1 .or. chmesh%x > grid%nx .or. &
          chmesh%y < 1 .or. chmesh%y > grid%ny )then
        chmesh%is_outside_domain = .true.
      elseif( grid%domain_mask(chmesh%x,chmesh%y) == DOMAIN__OUTSIDE )then
        chmesh%is_outside_domain = .true.
      endif
    enddo
  enddo  ! iCh/

  deallocate(arg)
  deallocate(isct_pt_x)
  deallocate(isct_pt_y)
  deallocate(isct_pt_leng)
  deallocate(isct_ch_x)
  deallocate(isct_ch_y)
  deallocate(isct_ch_leng)
  !-------------------------------------------------------------
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
        dlon_east*d2r, dlat_east*d2r) * EARTH_R
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
  enddo  ! ix/

  if( .not. river%allow_channels_outside_domain )then
    if( xs < 1 .or. xe > grid%nx .or. ys < 1 .or. ye > grid%ny )then
      call errend('Channel intersects the mesh outside the domain.'//&
                '\nchannel '//str(iCh)//&
                '\npoint '//str(iPt)//' ('//str(pt1%lon,'f12.7')//','//str(pt1%lat,'f11.7')//')'//&
                '\n      '//str(iPt+1)//' ('//str(pt2%lon,'f12.7')//','//str(pt2%lat,'f11.7')//')'//&
                '\nx: '//str((/xs,xe/),' - ')//&
                '\ny: '//str((/ys,ye/),' - '))
    endif
  endif
end subroutine func_x
!---------------------------------------------------------------
end subroutine build_slope_river_intersections
!===============================================================
!
!===============================================================
end module mod_config
