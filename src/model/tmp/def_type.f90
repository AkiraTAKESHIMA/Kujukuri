module def_type
  use lib_const
  implicit none
  !-------------------------------------------------------------
  ! Config
  !-------------------------------------------------------------
  type output_
    character(CLEN_PATH) :: dir
    logical :: overwrite
    integer :: interval  ! sec
  end type

  type simulation_
    character(20) :: sdatetime_start
    character(20) :: sdatetime_end
  end type

  type timestep_
    integer :: model  ! sec
    integer :: slope  ! sec
    integer :: river  ! sec
  end type

  type adaptive_rk_
    real(8) :: dt_min_slope
    real(8) :: dt_min_river
    real(8) :: error_tolerance
  end type

  type domain_
    real(8) :: west
    real(8) :: east
    real(8) :: south
    real(8) :: north
    integer :: nx
    integer :: ny
  end type

  type input_topography_
    character(CLEN_PATH) :: file_elv       ! elevation
    character(CLEN_PATH) :: file_flwdir    ! flow direction
  end type

  type input_landcover_
    character(CLEN_PATH) :: file_landuse   ! land use
  end type

  type input_river_network_
    character(CLEN_PATH) :: file_network  ! river network
    logical :: allow_channels_outside_domain
  end type

  type rivsec_area_estimation_
    real(8) :: width_c, width_s
    real(8) :: depth_c, depth_s
    real(8) :: levee_height
    real(8) :: levee_upa_thresh
  end type

  type rivsec_rectangular_
    character(CLEN_PATH) :: file_width
    character(CLEN_PATH) :: file_depth
    character(CLEN_PATH) :: file_levee
  end type

  type rivsec_explicit_
    character(CLEN_PATH) :: dir
  end type

  type input_river_section_
    integer :: method
    type(rivsec_area_estimation_) :: area_estimation
    type(rivsec_rectangular_)     :: rectangular
    type(rivsec_explicit_)        :: explicit
  end type

  type input_river_
    type(input_river_network_) :: network
    type(input_river_section_) :: section
  end type

  type input_forcing_
    integer :: interval
    character(CLEN_PATH) :: file_prcp  ! precipitation
    real(4) :: prcp_miss
  end type

  type input_
    type(input_topography_) :: topography
    type(input_landcover_)  :: landcover
    type(input_river_)      :: river
    type(input_forcing_)    :: forcing
  end type

  type param_river_
    real(8) :: ns
  end type

  type param_slope_
    integer :: num_landuse
    integer, pointer :: diffusion(:)     ! (num_landuse)
    real(8), pointer :: ns(:)            ! (num_landuse)
    real(8), pointer :: soildepth(:)     ! (num_landuse)
    real(8), pointer :: gammaa(:)        ! (num_landuse)
    real(8), pointer :: ksv(:)           ! (num_landuse)
    real(8), pointer :: faif(:)          ! (num_landuse)
    real(8), pointer :: infilt_limit(:)  ! (num_landuse)
    real(8), pointer :: ka(:)            ! (num_landuse)
    real(8), pointer :: km(:)            ! (num_landuse)
    real(8), pointer :: gammam(:)        ! (num_landuse)
    real(8), pointer :: beta(:)          ! (num_landuse)
    real(8), pointer :: da(:)            ! (num_landuse)
    real(8), pointer :: dm(:)            ! (num_landuse)
    real(8), pointer :: ksg(:)           ! (num_landuse)
    real(8), pointer :: gammag(:)        ! (num_landuse)
    real(8), pointer :: kg0(:)           ! (num_landuse)
    real(8), pointer :: fpg(:)           ! (num_landuse)
    real(8), pointer :: rgl(:)           ! (num_landuse)
  end type

  type param_
    type(param_river_) :: river
    type(param_slope_) :: slope
  end type

  type config_
    type(output_) :: output
    type(simulation_) :: simulation
    type(timestep_) :: timestep
    type(adaptive_rk_) :: adaptive_rk
    type(domain_) :: domain
    type(input_) :: input
    type(param_) :: param
  end type
  !-------------------------------------------------------------
  ! Static
  !-------------------------------------------------------------
  type grid_
    integer :: nx
    integer :: ny
    real(8) :: west
    real(8) :: east
    real(8) :: south
    real(8) :: north
    real(8) :: cellsize_lon
    real(8) :: cellsize_lat
    integer(1), pointer :: domain_mask(:,:)
    real(8), pointer :: area(:,:)
  end type

  type static_slope_
    integer :: nx, ny
    integer :: nGrid
    integer :: nDir
    integer, pointer :: idx2i(:)
    integer, pointer :: idx2j(:)
    integer, pointer :: ij2idx(:,:)
    integer(1), pointer :: domain_mask(:,:)
    real(8), pointer :: area(:,:)
    integer, pointer :: landuse(:,:)
    real(8), pointer :: zs(:,:)  ! surface elevation
    integer(1), pointer :: domain_mask_idx(:)
    integer, pointer :: down_idx(:,:)
    !integer, pointer :: acc_idx(:,:)
    real(8), pointer :: area_idx(:)
    real(8), pointer :: dis_idx(:,:)
    real(8), pointer :: len_idx(:,:)
    integer, pointer :: landuse_idx(:)
    real(8), pointer :: zb_idx(:)  ! bedrock elevation
    real(8), pointer :: diffusion_idx(:)
    real(8), pointer :: ns_idx(:)
    real(8), pointer :: soildepth_idx(:)
    real(8), pointer :: gammaa_idx(:)
    real(8), pointer :: ksv_idx(:)
    real(8), pointer :: faif_idx(:)
    real(8), pointer :: infilt_limit_idx(:)
    real(8), pointer :: ka_idx(:)
    real(8), pointer :: km_idx(:)
    real(8), pointer :: gammam_idx(:)
    real(8), pointer :: beta_idx(:)
    real(8), pointer :: da_idx(:)
    real(8), pointer :: dm_idx(:)
    real(8), pointer :: ksg_idx(:)
    real(8), pointer :: gammag_idx(:)
    real(8), pointer :: kg0_idx(:)
    real(8), pointer :: fpg_idx(:)
    real(8), pointer :: rgl_idx(:)
  end type

  type pt_
    real(8) :: lon, lat
    integer :: x, y
    real(8) :: leng  ! distance to the next point
    real(8) :: width
    real(8) :: depth
    real(8) :: levee
    real(8) :: zs
    real(8) :: zb
  end type

  type nd_conn_
    integer :: iCh  ! serial number
    integer :: jNode  ! 1 or 2
  end type

  type nd_
    real(8) :: lon, lat
    integer :: nNode_conn
    type(nd_conn_), pointer :: node_conn(:)  !(nNode_conn)
    integer :: iNode  ! serial number
    logical :: is_outlet
  end type

  type ch_conn_
    integer :: jNode_self  ! 1 or 2
    integer :: iCh  ! serial number
    integer :: jNode  ! 1 or 2
  end type

  type ch_mesh_
    integer :: x, y
    real(8) :: leng
    real(8) :: area
    logical :: is_outside_domain
  end type

  type channel_
    integer :: nPt
    type(pt_), pointer :: pt(:)  !(nPt)
    type(nd_), pointer :: node(:)  !(2)
    integer :: nCh_conn
    type(ch_conn_), pointer :: ch_conn(:)  !(nCh_conn)
    real(8) :: leng
    real(8) :: width
    real(8) :: depth
    real(8) :: levee
    real(8) :: zb
    real(8) :: area
    integer :: nMesh
    type(ch_mesh_), pointer :: mesh(:)  !(nMesh)
  end type

  type outlet_
    integer :: iCh
    integer :: jNode
    integer :: x, y
  end type

  type static_river_
    ! Network
    logical :: allow_channels_outside_domain
    integer :: nCh
    type(channel_), pointer :: channel(:)
    integer :: nNode
    integer :: nOutlet
    type(outlet_), pointer :: outlet(:)
    real(8), pointer :: area_idx(:)

    ! Cross section
    integer :: section_method

    ! Properties
    real(8) :: ns

    ! Topography
    real(8), pointer :: zb_idx(:)
  end type

  type static_
    type(static_slope_) :: slope
    type(static_river_) :: river
  end type
  !-------------------------------------------------------------
  ! Time
  !-------------------------------------------------------------
  type time_
    integer, pointer :: datetime_start(:)
    integer, pointer :: datetime_end(:)
    integer, pointer :: datetime_base(:)
    real(8) :: t_start
    real(8) :: t_end
    real(8) :: t_now
    real(8) :: t_next
    real(8) :: t_forcing_next
    real(8) :: dt_model
    real(8) :: dt_river
    real(8) :: dt_slope
    real(8) :: dt_forcing
    integer :: count_model
    integer :: count_forcing
  end type
  !-------------------------------------------------------------
  ! State
  !-------------------------------------------------------------
  type state_slope_
    real(8), pointer :: hs_idx(:)
    real(8), pointer :: hs(:,:)
  end type

  type state_river_
    real(8), pointer :: hr_idx(:)
    real(8), pointer :: vr_idx(:)
  end type

  type state_
    type(state_slope_) :: slope
    type(state_river_) :: river
  end type
  !-------------------------------------------------------------
  ! Tendency
  !-------------------------------------------------------------
  type tendency_slope_
    real(8), pointer :: dwlv_dt(:,:)
  end type

  type tendency_river_
    real(8), pointer :: dwlv_dt(:,:)
    real(8), pointer :: dstorage_dt(:)
  end type

  type tendency_
    type(tendency_slope_) :: slope
    type(tendency_river_) :: river
  end type
  !-------------------------------------------------------------
  ! Forcing
  !-------------------------------------------------------------
  type forcing_
    real(8), pointer :: prcp(:,:)
    real(8), pointer :: prcp_idx(:)
  end type
  !-------------------------------------------------------------
  ! Solver
  !-------------------------------------------------------------
  type solver_adaptive_rk45_
    real(8) :: dt_min_slope
    real(8) :: dt_min_river
    real(8) :: error_tolerance
  end type

  type solver_
    type(solver_adaptive_rk45_) :: adaptive_rk45
  end type
  !-------------------------------------------------------------
  ! Workspace
  !-------------------------------------------------------------
  type workspace_slope_
    real(8), pointer :: hs_idx(:)
    real(8), pointer :: qs_idx(:,:)
  end type

  type workspace_river_
    real(8), pointer :: hr_idx(:)
    real(8), pointer :: qr_idx(:,:)
  end type

  type workspace_rk45_submodel_
    integer :: nGrid
    integer :: nDir
    real(8), pointer :: y5(:)
    real(8), pointer :: y4(:)
    real(8), pointer :: yt(:)
    real(8), pointer :: yerr(:)
    real(8), pointer :: k1(:), k2(:), k3(:), k4(:), k5(:), k6(:)
    real(8), pointer :: q1(:,:), q2(:,:), q3(:,:), q4(:,:), q5(:,:), q6(:,:)
  end type

  type workspace_rk45_
    type(workspace_rk45_submodel_) :: river
    type(workspace_rk45_submodel_) :: slope
  end type

  type workspace_
    type(workspace_river_) :: river
    type(workspace_slope_) :: slope
    type(workspace_rk45_) :: rk45
  end type
end module def_type
