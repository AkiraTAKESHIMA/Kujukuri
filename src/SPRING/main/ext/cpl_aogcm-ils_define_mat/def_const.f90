module def_const
  implicit none

  real(8), parameter :: THRESH_AGCM_LNDFRC_OGCM = 1d-6
  real(8), parameter :: agcm_opt_thresh_lndfrc_noriv_virt_min_default    = 1d-6
  real(8), parameter :: agcm_opt_thresh_lndfrc_excess_default            = 1d-6
  real(8), parameter :: agcm_opt_thresh_lndfrc_noriv_virt_excess_default = 1d-6
  real(8), parameter :: agcm_opt_thresh_lndfrc_zero_default              = 1d-10
  real(8), parameter :: lsm_opt_thresh_grdwgt_noriv_virt_excess_default  = 1d-6
end module def_const
