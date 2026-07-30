module def_type
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_opt
  use c2_type_rt
  implicit none

  type rt_in_
    type(rt_) :: ogcm_ocean_to_agcm
    type(rt_) :: ogcm_land_to_agcm
    type(rt_) :: rm_river_to_agcm
    type(rt_) :: rm_noriv_to_agcm
    type(rt_) :: rm_ocean_to_agcm
  end type

  type rt_out_
    type(rt_) :: lsm_river_to_agcm
    type(rt_) :: lsm_noriv_to_agcm
    type(rt_) :: lsm_noriv_virt_to_agcm  ! no output
    type(rt_) :: lsm_ocean_to_agcm
    type(rt_) :: agcm_to_lsm_river
    type(rt_) :: agcm_to_lsm_noriv
    type(rt_) :: agcm_to_lsm_ocean
  end type

  type agcm_
    integer(8) :: nij

    type(file_) :: fin_grdidx
    type(file_) :: fin_grdara
    type(file_) :: fout_lndara_ogcm
    type(file_) :: fout_lndara_river
    type(file_) :: fout_lndara_noriv
    type(file_) :: fout_lndara_noriv_real
    type(file_) :: fout_lndara_noriv_virt

    integer(8), pointer :: grdidx(:)
    integer(8), pointer :: grdidxarg(:)
    real(8)   , pointer :: grdara(:)
    real(8)   , pointer :: lndara_ogcm(:)
    real(8)   , pointer :: lndara_river(:)
    real(8)   , pointer :: lndara_noriv(:)
    real(8)   , pointer :: lndara_noriv_real(:)
    real(8)   , pointer :: lndara_noriv_virt(:)

    real(8) :: sum_grdara
    real(8) :: sum_lndara_ogcm
    real(8) :: sum_lndara_river
    real(8) :: sum_lndara_noriv
    real(8) :: sum_lndara_noriv_real
    real(8) :: sum_lndara_noriv_virt

    integer(8) :: idx_miss

    real(8) :: opt_thresh_lndfrc_noriv_virt_min
    real(8) :: opt_thresh_lndfrc_excess
    real(8) :: opt_thresh_lndfrc_noriv_virt_excess
    real(8) :: opt_thresh_lndfrc_zero
  end type

  type rm_
    integer(8) :: nij
    integer(8) :: ncx, ncy
    integer(8) :: nkx, nky

    type(file_) :: fin_grdidx_river
    type(file_) :: fin_grdidx_noriv
    type(file_) :: fin_grdidx_ocean

    type(file_) :: fin_grdara_river
    type(file_) :: fin_grdara_noriv
    type(file_) :: fin_grdara_ocean

    type(file_) :: fin_rstidx_river
    type(file_) :: fin_rstidx_noriv
    type(file_) :: fin_rstidx_ocean

    integer(8), pointer :: grdidx_river(:)
    integer(8), pointer :: grdidx_noriv(:)
    integer(8), pointer :: grdidx_ocean(:)

    real(8)   , pointer :: grdara_river(:)
    real(8)   , pointer :: grdara_noriv(:)
    real(8)   , pointer :: grdara_ocean(:)

    real(8) :: sum_grdara_river
    real(8) :: sum_grdara_noriv
    real(8) :: sum_grdara_ocean

    integer(8) :: idx_miss
    real(8)    :: ara_miss
  end type

  type lsm_
    integer(8) :: nij
    integer(8) :: ncx, ncy
    integer(8) :: nkx, nky

    type(file_) :: fout_grdmsk_river
    type(file_) :: fout_grdmsk_noriv
    type(file_) :: fout_grdmsk_noriv_real
    type(file_) :: fout_grdmsk_noriv_virt
    type(file_) :: fout_grdmsk_ocean

    type(file_) :: fout_grdidx_river
    type(file_) :: fout_grdidx_noriv
    type(file_) :: fout_grdidx_noriv_real
    type(file_) :: fout_grdidx_noriv_virt
    type(file_) :: fout_grdidx_ocean

    type(file_) :: fout_grdidx_bnd_river
    type(file_) :: fout_grdidx_bnd_noriv
    type(file_) :: fout_grdidx_bnd_noriv_real
    type(file_) :: fout_grdidx_bnd_noriv_virt

    type(file_) :: fout_grdara_river
    type(file_) :: fout_grdara_noriv
    type(file_) :: fout_grdara_noriv_real
    type(file_) :: fout_grdara_noriv_virt
    type(file_) :: fout_grdara_ocean

    type(file_) :: fout_grdwgt_river
    type(file_) :: fout_grdwgt_noriv
    type(file_) :: fout_grdwgt_noriv_real
    type(file_) :: fout_grdwgt_noriv_virt
    type(file_) :: fout_grdwgt_ocean

    type(file_) :: fout_rstidx_river
    type(file_) :: fout_rstidx_noriv
    type(file_) :: fout_rstidx_noriv_real
    type(file_) :: fout_rstidx_noriv_virt
    type(file_) :: fout_rstidx_ocean

    type(file_) :: fout_rstidx_bnd_river
    type(file_) :: fout_rstidx_bnd_noriv
    type(file_) :: fout_rstidx_bnd_noriv_real
    type(file_) :: fout_rstidx_bnd_noriv_virt

    integer(1), pointer :: grdmsk_river(:)
    integer(1), pointer :: grdmsk_noriv(:)
    integer(1), pointer :: grdmsk_noriv_real(:)
    integer(1), pointer :: grdmsk_noriv_virt(:)
    integer(1), pointer :: grdmsk_ocean(:)

    integer(8), pointer :: grdidx_river(:)
    integer(8), pointer :: grdidx_noriv(:)
    integer(8), pointer :: grdidx_noriv_real(:)
    integer(8), pointer :: grdidx_noriv_virt(:)
    integer(8), pointer :: grdidx_ocean(:)

    integer(8), pointer :: grdidx_bnd_river(:)
    integer(8), pointer :: grdidx_bnd_noriv(:)
    integer(8), pointer :: grdidx_bnd_noriv_real(:)
    integer(8), pointer :: grdidx_bnd_noriv_virt(:)

    real(8)   , pointer :: grdara_river(:)
    real(8)   , pointer :: grdara_noriv(:)
    real(8)   , pointer :: grdara_noriv_real(:)
    real(8)   , pointer :: grdara_noriv_virt(:)
    real(8)   , pointer :: grdara_ocean(:)

    real(8)   , pointer :: grdwgt_river(:)
    real(8)   , pointer :: grdwgt_noriv(:)
    real(8)   , pointer :: grdwgt_noriv_real(:)
    real(8)   , pointer :: grdwgt_noriv_virt(:)
    real(8)   , pointer :: grdwgt_ocean(:)

    integer(8), pointer :: rstidx(:,:)
    integer(8), pointer :: rstidx_tmp(:,:)

    real(8) :: sum_grdara_river
    real(8) :: sum_grdara_noriv
    real(8) :: sum_grdara_noriv_real
    real(8) :: sum_grdara_noriv_virt
    real(8) :: sum_grdara_ocean

    integer(8) :: idx_miss
    real(8)    :: ara_miss
    real(8)    :: wgt_miss

    real(8) :: opt_thresh_grdwgt_noriv_virt_excess
  end type

  type opt_
    type(opt_sys_) :: sys
    type(opt_log_) :: log
  end type
end module def_type
