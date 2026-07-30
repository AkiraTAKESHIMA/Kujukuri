import os
import sys

import const, util
import s00_preprocess        , \
       s01_make_grid_data_GCM, \
       s02_rasterize_OGCM    , \
       s03_run_FLOW          , \
       s04_make_idxmap_RM    , \
       s05_make_grid_data_RM , \
       s06_make_rt_standard  , \
       s07_define_LSM        , \
       s08_make_grid_data_LSM, \
       s09_make_rt_standard  , \
       s10_make_rt_special   , \
       s11_merge_rt


job = {
   0: 'preprocess', 
   1: 'make_grid_data_GCM',
   2: 'rasterize_OGCM',
   3: 'run_FLOW',
   4: 'make_idxmap_RM',
   5: 'make_grid_data_RM',
   6: 'make_rt_standard-1',
   7: 'define_LSM',
   8: 'make_grid_data_LSM',
   9: 'make_rt_standard-2',
  10: 'make_rt_special',
  11: 'merge_rt',
}

scripts = {
  0: s00_preprocess,
  1: s01_make_grid_data_GCM,
  2: s02_rasterize_OGCM,
  3: s03_run_FLOW,
  4: s04_make_idxmap_RM,
  5: s05_make_grid_data_RM,
  6: s06_make_rt_standard,
  7: s07_define_LSM,
  8: s08_make_grid_data_LSM,
  9: s09_make_rt_standard,
  10: s10_make_rt_special,
  11: s11_merge_rt,
}
step_max = len(scripts.keys()) - 1
