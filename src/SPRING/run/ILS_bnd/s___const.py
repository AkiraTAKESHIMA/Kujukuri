import os
import sys

import const, util
import s00_preprocess  , \
       s01_make_cmf_mat, \
       s02_make_rt


job = {
  0: 'preprocess', 
  1: 'make_cmf_mat',
  2: 'make_rt',
}

scripts = {
  0: s00_preprocess, 
  1: s01_make_cmf_mat,
  2: s02_make_rt,
}
step_max = len(scripts.keys()) - 1
