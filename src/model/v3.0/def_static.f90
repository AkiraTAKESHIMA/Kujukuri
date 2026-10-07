module def_static
  implicit none

  ! Input files and procedure controls
  ! -- Forcing (precip.)
  character(256) :: rainfile

  ! -- Topography
  character(256) :: demfile
  character(256) :: dirfile
  character(256) :: upafile

  ! -- River network
  character(256) :: file_riv_network

  ! -- River cross section
  integer :: switch_riv_crssct

  ! ---- rectangular
  character(256) :: widthfile
  character(256) :: depthfile
  character(256) :: heightfile

  ! ---- arbitrary
  character(256) :: sec_map_file
  character(256) :: sec_file

  ! -- Initial conditions
  integer :: init_slo_switch, init_riv_switch, init_gw_switch, init_gampt_ff_switch
  character(256) :: initfile_slo
  character(256) :: initfile_riv
  character(256) :: initfile_gw
  character(256) :: initfile_gampt_ff

  ! -- Boundary conditions
  integer :: bound_slo_wlev_switch, bound_riv_wlev_switch
  character(256) :: boundfile_slo_wlev
  character(256) :: boundfile_riv_wlev

  integer :: bound_slo_disc_switch, bound_riv_disc_switch
  character(256) :: boundfile_slo_disc
  character(256) :: boundfile_riv_disc

  ! -- Ground water
  integer :: gw_switch

  ! -- Land cover
  integer :: land_switch
  character(256) :: landfile

  ! -- Forcing (evaporation)
  integer :: evp_switch
  character(256) :: evpfile

  ! Simulation span
  ! TMP
  !integer, allocatable :: datetime_start(:), datetime_end(:)
  integer :: time_hours
  real(8) :: time_start, time_end

  ! Time step
  real(8) :: dt_model
  real(8) :: dt_slo
  real(8) :: dt_riv
  integer :: nt_model

  ! Output
  character(256) :: dir_out
  real(8) :: dt_out

  ! Calculation domain
  integer :: nx, ny, num_of_cell
  real(8) :: west, east, south, north
  real(8) :: cellsize
  real(8) :: dx, dy
  real(8) :: length
  real(8) :: area
  integer :: eight_dir
  integer, allocatable, save :: domain(:,:)

  ! Forcing data
  integer :: nt_rain
  !real(8), allocatable :: time_rain(:)
  !real(8), allocatable :: rain_all(:,:,:)

  ! Slope
  real(8), allocatable, save :: zs(:,:)
  real(8), allocatable, save :: zb(:,:)

  integer, save :: lmax, slo_count
  integer, allocatable, save :: slo_idx2i(:)
  integer, allocatable, save :: slo_idx2j(:)
  integer, allocatable, save :: slo_ij2idx(:,:)
  integer, allocatable, save :: down_slo_idx(:,:)
  integer, allocatable, save :: domain_slo_idx(:)
  integer, allocatable, save :: land_idx(:)
  integer, allocatable, save :: down_slo_1d_idx(:)
  real(8), allocatable, save :: ns_slo_idx(:)
  real(8), allocatable, save :: soildepth_idx(:)
  real(8), allocatable, save :: gammaa_idx(:)
  real(8), allocatable, save :: ksv_idx(:)
  real(8), allocatable, save :: faif_idx(:)
  real(8), allocatable, save :: infilt_limit_idx(:)
  real(8), allocatable, save :: ka_idx(:)
  real(8), allocatable, save :: gammam_idx(:)
  real(8), allocatable, save :: beta_idx(:)
  real(8), allocatable, save :: da_idx(:)
  real(8), allocatable, save :: dm_idx(:)
  real(8), allocatable, save :: ksg_idx(:)
  real(8), allocatable, save :: gammag_idx(:)
  real(8), allocatable, save :: kg0_idx(:)
  real(8), allocatable, save :: fpg_idx(:)
  real(8), allocatable, save :: rgl_idx(:)
  real(8), allocatable, save :: zb_slo_idx(:)
  real(8), allocatable, save :: dis_slo_idx(:,:)
  real(8), allocatable, save :: len_slo_idx(:,:)
  real(8), allocatable, save :: dis_slo_1d_idx(:)
  real(8), allocatable, save :: len_slo_1d_idx(:)
  integer, allocatable, save :: flow_slo_idx(:)
  real(8), allocatable, save :: acc_slo_idx(:)

  ! -- intersection with rivers
  type slo_riv_isct_
    integer :: nCh
    integer, pointer :: iCh(:)
    integer, pointer :: jSlo(:)  ! index in ch%isct
  end type

  type(slo_riv_isct_), pointer :: slo_riv_isct(:)  !(slo_count)

  ! Land cover
  integer, allocatable, save :: land(:,:)

  ! River
  integer, allocatable, save :: dir(:,:)
  real(8), allocatable, save :: upa(:,:)

  integer :: riv_count

  ! -- River network
  type ch_point_
    real(8) :: lon, lat
  end type

  type ch_node_
    real(8) :: lon, lat
    logical :: is_outlet
    real(8) :: dist_to_mouth
    integer :: stat_updown
    integer :: iNode  ! index in nwk%node
  end type

  ! intersection with slope grids
  type ch_isct_
    integer :: nSlo
    integer, pointer :: x(:)  !(nSlo)
    integer, pointer :: y(:)  !(nSlo)
    integer, pointer :: iSlo(:)  !(nSlo)
    integer, pointer :: jCh(:)  !(nSlo) index in slo_riv_isct
    real(8), pointer :: leng(:)  !(nSlo)
    real(8), pointer :: area(:)  !(nSlo)
    integer, pointer :: domain(:)  !(nSlo)
    real(8) :: leng_domain
  end type

  ! node
  type nwk_node_
    real(8) :: lon, lat
    integer :: nCh
    integer, pointer :: iCh(:)  !(nCh)
  end type

  type channel_
    real(8) :: zs
    real(8) :: zb
    real(8) :: leng
    real(8) :: upa
    real(8) :: width
    real(8) :: depth
    real(8) :: height
    real(8) :: area
    integer :: flow
    logical :: is_outlet
    real(8) :: dist_to_mouth
    type(ch_node_), pointer :: node(:)  !(2)
    integer :: nPoint
    type(ch_point_), pointer :: point(:)  !(nPoint)
    type(ch_isct_) :: isct
    integer :: nCh_up, nCh_down
    integer, pointer :: iCh_up(:)  !(nCh_up)
    integer, pointer :: iCh_down(:)  !(nCh_down)
    real(8), pointer :: dist_down(:)
  end type

  type network_
    integer :: nNode
    type(nwk_node_), pointer :: node(:)
  end type

  type(channel_), pointer :: channel(:)  !(riv_count)
  type(network_) :: nwk

  ! -- River section
  real(8) :: width_param_c
  real(8) :: width_param_s
  real(8) :: depth_param_c
  real(8) :: depth_param_s
  real(8) :: height_param
  integer :: height_limit_param
  real(8) :: width_llim
  real(8) :: depth_llim

  ! Dam
  integer :: dam_switch
  character(256) :: damfile  ! dam control file

  ! Parameters

  ! -- River
  real(8), save :: ns_river

  ! -- Land cover
  integer, save :: num_of_landuse
  integer, allocatable, save :: flow(:)
  real(8), allocatable, save :: ns_slope(:)
  real(8), allocatable, save :: soildepth(:)
  real(8), allocatable, save :: gammaa(:)

  real(8), allocatable, save :: ksv(:)
  real(8), allocatable, save :: faif(:)
  real(8), allocatable, save :: infilt_limit(:)

  real(8), allocatable, save :: ka(:)
  real(8), allocatable, save :: gammam(:)
  real(8), allocatable, save :: beta(:)
  real(8), allocatable, save :: da(:), dm(:)

  real(8), allocatable, save :: ksg(:)
  real(8), allocatable, save :: gammag(:)
  real(8), allocatable, save :: kg0(:)
  real(8), allocatable, save :: fpg(:)
  real(8), allocatable, save :: rgl(:)

  ! Boundary conditions
  real(8), allocatable, save :: bound_slo_wlev(:,:), bound_riv_wlev(:,:)
  real(8), allocatable, save :: bound_slo_disc(:,:), bound_riv_disc(:,:)

  integer, save :: tt_max_bound_slo_wlev, tt_max_bound_riv_wlev
  integer, save :: tt_max_bound_slo_disc, tt_max_bound_riv_disc
  integer, allocatable :: t_bound_slo_wlev(:), t_bound_riv_wlev(:)
  integer, allocatable :: t_bound_slo_disc(:), t_bound_riv_disc(:)
  real(8), allocatable, save :: bound_slo_wlev_idx(:,:), bound_riv_wlev_idx(:,:)
  real(8), allocatable, save :: bound_slo_disc_idx(:,:), bound_riv_disc_idx(:,:)

  ! River cross sections
  integer, save :: sec_id_max
  integer, allocatable, save :: sec_map(:, :)
  integer, allocatable, save :: sec_map_idx(:)
  integer, allocatable, save :: sec_div(:)
  real(8), allocatable, save :: sec_length(:,:), sec_depth(:), sec_height(:)
  real(8), allocatable, save :: sec_hr(:,:), sec_area(:,:), sec_peri(:,:)
  real(8), allocatable, save :: sec_b(:,:), sec_ns_river(:,:), sec_length_idx(:)

end module def_static
