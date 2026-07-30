import os, sys

import const, util
import s00_preprocess    , \
       s01_make_cmf_mat  , \
       s02_make_grid_data, \
       s03_make_rt       , \
       s04_merge_rt


job = {
  0: 'preprocess',
  1: 'make_cmf_mat',
  2: 'make_grid_data',
  3: 'make_rt',
  4: 'merge_rt',
}

scripts = {
  0: s00_preprocess,
  1: s01_make_cmf_mat,
  2: s02_make_grid_data,
  3: s03_make_rt,
  4: s04_merge_rt,
}
step_max = len(scripts.keys()) - 1
