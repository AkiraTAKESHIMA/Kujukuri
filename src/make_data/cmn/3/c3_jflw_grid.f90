module c3_jflw_grid
  use c2_jflw_grid, only: &
    jflw_west_of_tx => west_of_tx, &
    jflw_east_of_tx => east_of_tx, &
    jflw_south_of_ty => south_of_ty, &
    jflw_north_of_ty => north_of_ty, &
    jflw_txs_of_lon => txs_of_lon, &
    jflw_txe_of_lon => txs_of_lon, &
    jflw_tys_of_lat => tys_of_lat, &
    jflw_tye_of_lat => tye_of_lat, &
!
    jflw_west_of_gx => west_of_gx, &
    jflw_east_of_gx => east_of_gx, &
    jflw_south_of_gy => south_of_gy, &
    jflw_north_of_gy => north_of_gy, &
    jflw_center_of_gx => center_of_gx, &
    jflw_center_of_gy => center_of_gy, &
    jflw_gxs_of_lon => gxs_of_lon, &
    jflw_gxe_of_lon => gxe_of_lon, &
    jflw_gys_of_lat => gys_of_lat, &
    jflw_gye_of_lat => gye_of_lat, &
    jflw_lonlat_to_gxy => lonlat_to_gxy, &
    jflw_lonlat_to_xy => lonlat_to_xy, &
!
    jflw_tx_of_gx => tx_of_gx, &
    jflw_ty_of_ty => ty_of_gy, &
    jflw_gxs_of_tx => gxs_of_tx, &
    jflw_gxe_of_tx => gxs_of_tx, &
    jflw_gys_of_ty => gys_of_ty, &
    jflw_gye_of_ty => gys_of_ty, &
    jflw_gx_of_x => gx_of_x, &
    jflw_gy_of_y => gy_of_y, &
    jflw_xy_to_gxy => xy_to_gxy, &
    jflw_gx_to_x => gx_to_x, &
    jflw_gy_to_y => gy_to_y, &
    jflw_gxy_to_xy => gxy_to_xy, &
!
    jflw_mean_dist_from_center => mean_dist_from_center, &
    jflw_get_nextxy => get_nextxy, &
    jflw_get_fdr => get_fdr
  implicit none
end module c3_jflw_grid
