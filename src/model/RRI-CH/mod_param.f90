module mod_param
  use lib_const
  use def_type
  implicit none
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  ! Output directory
  character(CLEN_PATH) :: dir_out

  ! Time
  integer, allocatable :: time_bgn(:)
  integer, allocatable :: time_end(:)
  real(8) :: dt
  real(8) :: dt_riv
  real(8) :: dt_slo
  integer :: nt
  integer :: dt_out  ![sec]
  integer :: dt_prcp = 3600

  real(8) :: eps
  real(8) :: ddt_min_riv
  real(8) :: ddt_min_slo

  real(8), parameter :: eps_default = 0.01d0
  real(8), parameter :: ddt_min_riv_default = 0.1d0
  real(8), parameter :: ddt_min_slo_default = 1.d0

  ! Mesh
  real(8) :: west, east, south, north
  integer :: nx, ny
  real(8) :: cellsize_lon, cellsize_lat

  ! Topography
  character(CLEN_PATH) :: file_elvtn  ! elevation
  character(CLEN_PATH) :: file_flwdir  ! flow direction
  character(CLEN_PATH) :: file_landuse   ! land use
  character(CLEN_PATH) :: file_rivshp  ! river channels

  integer :: lmax  ! ! 8-direction: 4, 4-direction: 2

  integer, allocatable :: domain(:,:)
  real(8), allocatable :: zs(:,:)
  real(8), allocatable :: zb(:,:)
  integer, allocatable :: fdr(:,:)
!  integer, allocatable :: acc(:,:)
  integer, allocatable :: land(:,:)
  real(8), allocatable :: area_slo(:,:)

  integer :: nLu
  integer, allocatable, save :: dif(:)  !(nLu)
  real(8), allocatable, save :: ns_slope(:)  !(nLu)
  real(8), allocatable, save :: soildepth(:)  !(nLu)
  real(8), allocatable, save :: gammaa(:)  !(nLu)

  real(8), allocatable, save :: ksv(:)  !(nLu)
  real(8), allocatable, save :: faif(:)  !(nLu)
  real(8), allocatable, save :: infilt_limit(:)  !(nLu)

  real(8), allocatable, save :: ka(:)  !(nLu)
  real(8), allocatable, save :: km(:)  !(nLu)
  real(8), allocatable, save :: gammam(:)  !(nLu)
  real(8), allocatable, save :: beta(:)  !(nLu)
  real(8), allocatable, save :: da(:)  !(nLu)
  real(8), allocatable, save :: dm(:)  !(nLu)

  real(8), allocatable, save :: ksg(:)  !(nLu)
  real(8), allocatable, save :: gammag(:)  !(nLu)
  real(8), allocatable, save :: kg0(:)  !(nLu)
  real(8), allocatable, save :: fpg(:)  !(nLu)
  real(8), allocatable, save :: rgl(:)  !(nLu)

  integer :: nSlo
  integer, allocatable :: slo_idx2i(:)  !(nSlo)
  integer, allocatable :: slo_idx2j(:)  !(nSlo)
  integer, allocatable :: slo_ij2idx(:,:)  !(nx,ny)
  integer, allocatable :: domain_slo_idx(:)  !(nSlo)
  integer, allocatable :: down_slo_idx(:,:)  !(lmax,nSlo)
  real(8), allocatable :: area_slo_idx(:)  !(nSlo)
  real(8), allocatable :: dis_slo_idx(:,:)  !(lmax,nSlo)
  real(8), allocatable :: len_slo_idx(:,:)  !(lmax,nSlo)
!  integer, allocatable :: acc_slo_idx(:)  !(nSlo)

  real(8), allocatable :: land_idx(:)  !(nSlo)

  real(8), allocatable :: zb_slo_idx(:)  !(nSlo)
  integer, allocatable :: dif_slo_idx(:)  !(nSlo)
  real(8), allocatable :: ns_slo_idx(:)  !(nSlo)
  real(8), allocatable :: soildepth_idx(:)  !(nSlo)
  real(8), allocatable :: gammaa_idx(:)  !(nSlo)

  real(8), allocatable :: ksv_idx(:)  !(nSlo)
  real(8), allocatable :: faif_idx(:)  !(nSlo)
  real(8), allocatable :: infilt_limit_idx(:)  !(nSlo)
  real(8), allocatable :: ka_idx(:)  !(nSlo)
  real(8), allocatable :: km_idx(:)  !(nSlo)
  real(8), allocatable :: gammam_idx(:)  !(nSlo)
  real(8), allocatable :: beta_idx(:)  !(nSlo)
  real(8), allocatable :: da_idx(:)  !(nSlo)
  real(8), allocatable :: dm_idx(:)  !(nSlo)
  real(8), allocatable :: ksg_idx(:)  !(nSlo)
  real(8), allocatable :: gammag_idx(:)  !(nSlo)
  real(8), allocatable :: kg0_idx(:)  !(nSlo)
  real(8), allocatable :: fpg_idx(:)  !(nSlo)
  real(8), allocatable :: rgl_idx(:)  !(nSlo)

  ! Channels
  integer :: nCh
  type(ch_), pointer :: lst_ch(:)  !(nCh)

  integer :: nNode

  integer :: nOutlet
  type(outlet_), pointer :: lst_outlet(:)  !(nOutlet)

!  real(8), allocatable :: width_idx(:)  !(nCh)
!  real(8), allocatable :: depth_idx(:)  !(nCh)
!  real(8), allocatable :: levee_idx(:)  !(nCh)
  real(8), allocatable :: area_riv_idx(:)  !(nCh)
  real(8), allocatable :: zb_riv_idx(:)  !(nCh)

  integer :: width_mode
  integer :: depth_mode
  integer :: levee_mode
  real(8) :: width_param_c, width_param_s
  real(8) :: depth_param_c, depth_param_s
  real(8) :: levee_param
  real(8) :: levee_upa_thresh

  logical :: allow_channel_outside_domain

  ! Earth's constants
  real(8) :: earth_r

  ! Physics
  real(8) :: ns_river

  ! Forcing
  character(CLEN_PATH) :: file_prcp

  logical :: debug = .false.
  integer :: it_debug = 0
  integer :: k_debug = 697
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
end module mod_param
