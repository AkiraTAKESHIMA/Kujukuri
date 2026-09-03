module def_type
  use lib_const
  implicit none
  !-------------------------------------------------------------
  ! Time
  !-------------------------------------------------------------
  type time_
    integer, pointer :: datetime_start(:)
    integer, pointer :: datetime_end(:)
    integer, pointer :: datetime_base(:)
    integer, pointer :: datetime_now(:)
    real(8) :: t_start
    real(8) :: t_end
    real(8) :: t_now
    real(8) :: t_model_next
    real(8) :: t_forcing_next
    real(8) :: dt_model
    real(8) :: dt_river
    real(8) :: dt_slope
    real(8) :: dt_forcing
    integer :: count_model
    integer :: count_forcing
  end type
  !-------------------------------------------------------------
  ! Grid
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
    real(8) :: earth_r
    integer(1), pointer :: domain_mask(:,:)
    real(8), pointer :: area(:,:)
  end type
  !-------------------------------------------------------------
  ! Solver
  !-------------------------------------------------------------
  type solver_adaptive_rk45_
    real(8) :: error_tolerance
    real(8) :: dt_min_slope, dt_min_river
  end type

  type solver_test_
    real(8) :: error_tolerance
    real(8) :: dt_min_slope, dt_min_river
  end type

  type solver_
    integer :: method
    type(solver_adaptive_rk45_) :: adaptive_rk45
    type(solver_test_) :: test
  end type
  !-------------------------------------------------------------
  ! Static: Slope
  !-------------------------------------------------------------
  type slope_version_
    integer :: advance_slope
    integer :: calc_discharge
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
    integer, pointer :: landcover(:,:)
    real(8), pointer :: zs(:,:)  ! surface elevation
    integer(1), pointer :: domain_mask_idx(:)
    integer, pointer :: down_idx(:,:)
    integer, pointer :: up_idx(:,:)
    real(8), pointer :: area_idx(:)
    real(8), pointer :: dis_idx(:,:)
    real(8), pointer :: len_idx(:,:)
    integer, pointer :: landcover_idx(:)
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
    type(slope_version_) :: version
  end type
  !-------------------------------------------------------------
  ! Static: River
  !-------------------------------------------------------------
  type point_
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

  type node_
    real(8) :: lon, lat
    integer :: nNode_conn
    type(nd_conn_), pointer :: node_conn(:)  !(nNode_conn)
    integer :: iNode  ! serial number
    logical :: is_outlet
    real(8) :: downleng
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
    type(point_), pointer :: pt(:)  !(nPt)
    type(node_), pointer :: node(:)  !(2)
    integer :: nCh_conn
    type(ch_conn_), pointer :: ch_conn(:)  !(nCh_conn)
    real(8) :: leng
    real(8) :: width
    real(8) :: depth
    real(8) :: levee
    real(8) :: zb
    real(8) :: area
    real(8) :: volume
    integer :: nMesh
    type(ch_mesh_), pointer :: mesh(:)  !(nMesh)
  end type

  type outlet_
    integer :: iCh
    integer :: jNode
    integer :: iNode
    real(8) :: zb  ! river bed elevation of outlet node
    real(8) :: v_sealevel  ! volume of h = -zb (sea level)
  end type

  type source_
    integer :: nCh
    integer, pointer :: iCh(:)  !(nCh)
  end type

  type static_river_
    ! Network
    logical :: allow_channels_outside_domain
    integer :: nCh
    type(channel_), pointer :: channel(:)
    integer :: nNode
    type(node_), pointer :: node(:)
    integer :: nOutlet
    type(outlet_), pointer :: outlet(:)
    integer :: nSource
    type(source_), pointer :: source(:)
    real(8), pointer :: area_idx(:)

    ! Cross section
    integer :: crssct_method

    ! Properties
    real(8) :: ns

    ! Topography
    real(8), pointer :: zb_idx(:)
  end type
  !-------------------------------------------------------------
  ! Forcing data
  !-------------------------------------------------------------
  type forcing_
    character(CLEN_PATH) :: file_prcp
    real(4) :: prcp_miss
    integer :: un_prcp
    real(4), pointer :: prcp_real(:,:)  ! workspace
  end type
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type output_
    character(CLEN_PATH) :: dir
    logical :: overwrite
  end type
end module def_type
