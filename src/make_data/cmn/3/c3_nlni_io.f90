module c3_nlni_io
  use c2_nlni_io, only: &
    nlni_tilename  => tilename , &
    nlni_strWsCode => strWsCode, &
    nlni_strRvCode => strRvCode, &
!
    nlni_get_wsName => get_wsName, &
!
    nlni_read_map_from_tile => read_map_from_tile, &
!
    nlni_clip_from_tile => clip_from_tile, &
!
    nlni_dirname_resolution    => dirname_resolution   , &
    nlni_get_f_lst_wsCode      => get_f_lst_wsCode     , &
    nlni_get_f_lst_wsCodeRange => get_f_lst_wsCodeRange, &
    nlni_get_f_wsCodeMask      => get_f_wsCodeMask     , &
    nlni_get_f_map_tile        => get_f_map_tile       , &
    nlni_get_dir_bsnara_tile   => get_dir_bsnara_tile  , &
    nlni_get_f_dat_channel     => get_f_dat_channel    , &
    nlni_get_f_shp_lake        => get_f_shp_lake       , &
    nlni_get_f_tbl_landuse     => get_f_tbl_landuse
  implicit none
end module c3_nlni_io
