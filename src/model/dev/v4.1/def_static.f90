module def_static
  implicit none

  ! Input files and procedure controls
  ! -- Forcing (precip.)
  character(256) :: prcpfile

  ! -- Topography
  character(256) :: demfile
  character(256) :: accfile
  character(256) :: dirfile

  ! -- River section (rectangular)
  integer :: rivfile_switch
  character(256) :: widthfile
  character(256) :: depthfile
  character(256) :: heightfile

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

  ! -- River division
  integer :: div_switch
  character(256) :: divfile

  ! -- Forcing (evaporation)
  integer :: evp_switch
  character(256) :: evpfile

  ! -- River length
  integer :: sec_length_switch
  character(256) :: sec_length_file

  ! -- River section (arbitrary)
  integer :: sec_switch
  character(256) :: sec_map_file
  character(256) :: sec_file

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
  real(8) :: xllcorner, yllcorner, xurcorner, yurcorner
  real(8) :: cellsize
  real(8) :: dx, dy
  real(8) :: length
  real(8) :: area
  integer :: utm
  integer :: eight_dir
  integer, allocatable, save :: domain(:,:)

  ! Forcing data
  integer :: nt_prcp
  real(8), allocatable :: time_prcp(:)
  real(8), allocatable :: prcp_all(:,:,:)

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
  integer, allocatable, save :: dif_slo_idx(:)
  real(8), allocatable, save :: acc_slo_idx(:)

  ! Land cover
  integer, allocatable, save :: land(:,:)

  ! River
  integer, allocatable, save :: riv(:,:)
  integer, allocatable, save :: acc(:,:)
  integer, allocatable, save :: dir(:,:)
  real(8), allocatable, save :: zb_riv(:,:)
  real(8), allocatable, save :: width(:,:)
  real(8), allocatable, save :: depth(:,:)
  real(8), allocatable, save :: height(:,:)
  real(8), allocatable, save :: len_riv(:,:) ! v1.4 add
  real(8), allocatable, save :: area_ratio(:,:)

  integer :: riv_count
  integer, allocatable :: riv_idx2i(:), riv_idx2j(:)
  integer, allocatable :: riv_ij2idx(:,:)
  integer, allocatable :: down_riv_idx(:)
  integer, allocatable :: domain_riv_idx(:)
  real(8), allocatable :: width_idx(:)
  real(8), allocatable :: depth_idx(:)
  real(8), allocatable :: height_idx(:)
  real(8), allocatable :: area_ratio_idx(:)
  real(8), allocatable :: zb_riv_idx(:)
  real(8), allocatable :: dis_riv_idx(:)
  real(8), allocatable :: len_riv_idx(:)
  integer, allocatable :: dif_riv_idx(:)

  integer :: riv_thresh
  real(8) :: width_param_c
  real(8) :: width_param_s
  real(8) :: depth_param_c
  real(8) :: depth_param_s
  real(8) :: height_param
  integer :: height_limit_param

  ! River division
  integer, save :: div_id_max
  integer, allocatable, save :: div_org_idx(:), div_dest_idx(:)
  real(8), allocatable, save :: div_rate(:)

  ! Dam
  integer :: dam_switch
  character(256) :: damfile  ! dam control file

  ! Parameters

  ! -- River
  real(8), save :: ns_river

  ! -- Land cover
  integer, save :: num_of_landuse
  integer, allocatable, save :: dif(:)
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
