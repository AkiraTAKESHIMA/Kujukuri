module c3_jflw_const
  use c2_jflw_const, only: &
    JFLW_DIR_JFLW  => DIR_JFLW , &
    JFLW_DIR_ORG   => DIR_ORG  , &
    JFLW_DIR_PRD   => DIR_PRD  , &
    JFLW_DIR_TILED => DIR_TILED, &
    JFLW_DIR_BASIN => DIR_BASIN, &
    JFLW_DIR_ALL   => DIR_ALL  , &
    JFLW_FDR_EAST       => FDR_EAST      , &
    JFLW_FDR_SOUTHEAST  => FDR_SOUTHEAST , &
    JFLW_FDR_SOUTH      => FDR_SOUTH     , &
    JFLW_FDR_SOUTHWEST  => FDR_SOUTHWEST , &
    JFLW_FDR_WEST       => FDR_WEST      , &
    JFLW_FDR_NORTHWEST  => FDR_NORTHWEST , &
    JFLW_FDR_NORTH      => FDR_NORTH     , &
    JFLW_FDR_NORTHEAST  => FDR_NORTHEAST , &
    JFLW_FDR_RIVERMOUTH => FDR_RIVERMOUTH, &
    JFLW_FDR_INLAND     => FDR_INLAND    , &
    JFLW_FDR_MISS       => FDR_MISS      , &
    JFLW_FDR_UNDEF      => FDR_UNDEF     , &
    JFLW_XY_RIVERMOUTH => XY_RIVERMOUTH, &
    JFLW_XY_INLAND     => XY_INLAND    , &
    JFLW_UPG_MISS => UPG_MISS, &
    JFLW_ELV_MISS => ELV_MISS, &
    JFLW_UPA_MISS => UPA_MISS, &
    JFLW_WTH_MISS => WTH_MISS, &
    JFLW_BSN_MISS  => BSN_MISS , &
    JFLW_BSN_UNDEF => BSN_UNDEF, &
    JFLW_LANDUSE_MISS => LANDUSE_MISS, &
    JFLW_NX  => NX , &
    JFLW_NY  => NY , &
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
