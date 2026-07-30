module c3_jflw_io
  use c2_jflw_io, only: &
    jflw_tilename => tilename, &
    jflw_strid    => strid, &
!
    jflw_read_basin_range_from_all  => read_basin_range_from_all, &
    jflw_read_basin_range_from_each => read_basin_range_from_each, &
    jflw_write_basin_range_for_each => write_basin_range_for_each, &
!
    jflw_read_map_from_tile       => read_map_from_tile, &
    jflw_read_basin_map_from_tile => read_basin_map_from_tile, &
!
    jflw_get_f_map_tile      => get_f_map_tile, &
    jflw_get_f_lst_tile      => get_f_lst_tile, &
    jflw_get_f_map_basin     => get_f_map_basin, &
    jflw_get_f_dat_basin     => get_f_dat_basin, &
    jflw_get_dir_bsnara_tile => get_dir_bsnara_tile, &
    jflw_get_f_lst_all       => get_f_lst_all
  implicit none
end module c3_jflw_io
