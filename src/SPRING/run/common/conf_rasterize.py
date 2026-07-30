import os
import sys
import subprocess
import json

import const, util


def block_raster(gs):
    s = '\
\n\
[raster]\n\
  nx: {nx_raster}\n\
  ny: {ny_raster}\n\
  west: {west}\n\
  east: {east}\n\
  south: {south}\n\
  north: {north}\n\
  is_south_to_north: {is_south_to_north}\n\
[end]\n\
'.format(**gs)

    return s


def block_output(dir_out):
    s = f'\
\n\
[output]\n\
  include_min: false\n\
  ratio_min: 0.999999d0\n\
\n\
  dir: "{dir_out}"\n\
  f_area_sum : "area.bin"\n\
  f_ratio_sum: "ratio.bin"\n\
  f_mask     : "mask.bin", int1\n\
[end]\n'

    return s
