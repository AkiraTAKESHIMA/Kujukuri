module c3_strnk_io
  use c2_strnk_io, only: &
    strnk_region_idx2str => region_idx2str, &
    strnk_region_str2idx => region_str2idx, &
!
    strnk_read_strrank_all => read_strrank_all, &
    strnk_show_strrank_all => show_strrank_all, &
!
    strnk_get_f_stream_shp             => get_f_stream_shp            , &
    strnk_get_f_stream_dbf             => get_f_stream_dbf            , &
    strnk_get_f_rivernode_shp          => get_f_rivernode_shp         , &
    strnk_get_f_rivernode_dbf          => get_f_rivernode_dbf         , &
    strnk_get_f_lst_tiled_idx          => get_f_lst_tiled_idx         , &
    strnk_get_f_lst_tiled_uid          => get_f_lst_tiled_uid         , &
    strnk_get_f_tmp_networks_fmt       => get_f_tmp_networks_fmt      , &
    strnk_get_f_tmp_networks_lst       => get_f_tmp_networks_lst      , &
    strnk_get_f_tmp_network_entity     => get_f_tmp_network_entity    , &
    strnk_get_f_tmp_network_channel    => get_f_tmp_network_channel   , &
    strnk_get_f_tmp_network_separation => get_f_tmp_network_separation, &
    strnk_get_f_lst_networks_channel   => get_f_lst_networks_channel  , &
    strnk_get_f_lst_networks_chpix     => get_f_lst_networks_chpix    , &
    strnk_get_f_lst_networks_mesh      => get_f_lst_networks_mesh     , &
    strnk_get_f_network_channel        => get_f_network_channel       , &
    strnk_get_f_network_chpix          => get_f_network_chpix         , &
    strnk_get_f_network_mesh           => get_f_network_mesh          , &
    strnk_get_f_entdown                => get_f_entdown               , &
    strnk_get_f_isct_basin             => get_f_isct_basin            , &
    strnk_get_f_eval_basin             => get_f_eval_basin            , &
    strnk_get_f_lst_eval_basin         => get_f_lst_eval_basin        , &
    strnk_get_f_isct_wsys              => get_f_isct_wsys             , &
    strnk_get_f_incons_isct_wsys       => get_f_incons_isct_wsys
  implicit none
end module c3_strnk_io
