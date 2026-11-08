module c3_jflw_const
  use c2_jflw_const, only: &
    JFLW_DIR_JFLW  => DIR_JFLW , &
    JFLW_DIR_ORG   => DIR_ORG  , &
    JFLW_DIR_PRD   => DIR_PRD  , &
    JFLW_DIR_TILED => DIR_TILED, &
    JFLW_DIR_BASIN => DIR_BASIN, &
    JFLW_DIR_ALL   => DIR_ALL  , &
    JFLW_FDR__EAST       => FDR__EAST      , &
    JFLW_FDR__SOUTHEAST  => FDR__SOUTHEAST , &
    JFLW_FDR__SOUTH      => FDR__SOUTH     , &
    JFLW_FDR__SOUTHWEST  => FDR__SOUTHWEST , &
    JFLW_FDR__WEST       => FDR__WEST      , &
    JFLW_FDR__NORTHWEST  => FDR__NORTHWEST , &
    JFLW_FDR__NORTH      => FDR__NORTH     , &
    JFLW_FDR__NORTHEAST  => FDR__NORTHEAST , &
    JFLW_FDR__RIVERMOUTH => FDR__RIVERMOUTH, &
    JFLW_FDR__INLAND     => FDR__INLAND    , &
    JFLW_FDR__MISS       => FDR__MISS      , &
    JFLW_FDR__UNDEF      => FDR__UNDEF     , &
    JFLW_XY__RIVERMOUTH => XY__RIVERMOUTH, &
    JFLW_XY__INLAND     => XY__INLAND    , &
    JFLW_UPG_MISS => UPG_MISS, &
    JFLW_ELV_MISS => ELV_MISS, &
    JFLW_UPA_MISS => UPA_MISS, &
    JFLW_WTH_MISS => WTH_MISS, &
    JFLW_BSN_MISS  => BSN_MISS , &
    JFLW_BSN_UNDEF => BSN_UNDEF, &
    JFLW_LANDUSE_MISS => LANDUSE_MISS, &
    JFLW_MX  => MX , &
    JFLW_MY  => MY , &
    JFLW_NTX => NTX, &
    JFLW_NTY => NTY, &
    JFLW_NGX => NGX, &
    JFLW_NGY => NGY, &
    JFLW_REGION_SOUTH => REGION_SOUTH, &
    JFLW_REGION_NORTH => REGION_NORTH, &
    JFLW_REGION_WEST  => REGION_WEST , &
    JFLW_REGION_EAST  => REGION_EAST , &
    JFLW_TILESIZE_LON => TILESIZE_LON, &
    JFLW_TILESIZE_LAT => TILESIZE_LAT, &
    JFLW_GRIDSIZE_LON => GRIDSIZE_LON, &
    JFLW_GRIDSIZE_LAT => GRIDSIZE_LAT, &
    JFLW_DGT_XY  => DGT_XY , &
    JFLW_DGT_TXY => DGT_TXY, &
    JFLW_DGT_GXY => DGT_GXY, &
!
    jflw_set_resolution => set_resolution
  implicit none
end module c3_jflw_const
