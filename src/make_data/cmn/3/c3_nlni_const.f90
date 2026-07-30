module c3_nlni_const
  use c2_nlni_const, only: &
    NLNI_DIR_NLNI   => DIR_NLNI, &
    NLNI_DIR_DL     => DIR_DL  , &
    NLNI_DIR_PRD    => DIR_PRD , &
    NLNI_VARNAME_LNDUSE => VARNAME_LNDUSE, &
    NLNI_DGT_WSCODE => DGT_WSCODE, &
    NLNI_DGT_RVCODE => DGT_RVCODE, &
    NLNI_WSCODE_MISS_I        => WSCODE_MISS_I, &
    NLNI_DIV_WSCODE_RVUNKNOWN => DIV_WSCODE_RVUNKNOWN, &
    NLNI_RVNAME_UNKNOWN       => RVNAME_UNKNOWN, &
    NLNI_LNDUSE_MISS => LNDUSE_MISS, &
    NLNI_REGION_WEST  => REGION_WEST , &
    NLNI_REGION_EAST  => REGION_EAST , &
    NLNI_REGION_SOUTH => REGION_SOUTH, &
    NLNI_REGION_NORTH => REGION_NORTH, &
    NLNI_TXMIN => TXMIN, &
    NLNI_TXMAX => TXMAX, &
    NLNI_TYMIN => TYMIN, &
    NLNI_TYMAX => TYMAX, &
    NLNI_NTX => NTX, &
    NLNI_NTY => NTY, &
    NLNI_NX => NX, &
    NLNI_NY => NY, &
    NLNI_NGX => NGX, &
    NLNI_NGY => NGY, &
    NLNI_TILESIZE_LON => TILESIZE_LON, &
    NLNI_TILESIZE_LAT => TILESIZE_LAT, &
    NLNI_CELLSIZE_LON => CELLSIZE_LON, &
    NLNI_CELLSIZE_LAT => CELLSIZE_LAT, &
    NLNI_DGT_TXY    => DGT_TXY   , &
    NLNI_DGT_XY     => DGT_XY    , &
    NLNI_DGT_GXY    => DGT_GXY, &
!
    nlni_set_resolution => set_resolution
end module c3_nlni_const
