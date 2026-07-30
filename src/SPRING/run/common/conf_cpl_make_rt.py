
import os
import sys
import subprocess
import json

import const, util
from util import str_file_bin


def head(dir_out, fname_report='report'):
    s = f'\
#\n\
path_report: "{dir_out}/{fname_report}.txt"\n'

    return s


def block_in_rt(dir_in, nij, rmp):
    s = f'\
\n\
[input_rt_agcm_to_ogcm]\n\
  length: {nij}\n\
  dir: "{dir_in}"\n\
  f_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  f_coef: "coef.bin", dble, 1, {rmp["endian"]}\n\
[end]\n'

    return s


def block_agcm(gs):
    s = f'\
\n\
[input_agcm]\n\
  nij: {gs["nij"]}\n\
  dir: "{gs["dir"]}"\n\
  f_grdidx: {str_file_bin(gs["fin_grdidx"])}\n\
  f_grdara: {str_file_bin(gs["fin_grdara"])}\n\
  f_grdlon: {str_file_bin(gs["fin_grdlon"])}\n\
  f_grdlat: {str_file_bin(gs["fin_grdlat"])}\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s


def block_lsm(gs):
    s = f'\
\n\
[input_lsm]\n\
  nij: {gs["nij"]}\n\
  dir: "{gs["dir"]}"\n\
  f_grdidx: {str_file_bin(gs["fin_grdidx"])}\n\
  f_grdara: {str_file_bin(gs["fin_grdara"])}\n\
  f_grdlon: {str_file_bin(gs["fin_grdlon"])}\n\
  f_grdlat: {str_file_bin(gs["fin_grdlat"])}\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s


def block_rt(dir_out, rmp):
    s = f'\
\n\
[output_rt_lsm_to_agcm]\n\
  dir: "{dir_out}"\n\
  fout_rt_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  fout_rt_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  fout_rt_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  fout_rt_coef: "coef.bin", dble, 1, {rmp["endian"]}\n\
\n\
  mesh_coef: none\n\
  mesh_sort: target\n\
\n\
  mesh_vrf: target\n\
  fout_vrf_grdnum: "vrf/tgt_grdnum.bin"\n\
[end]\n'

    return s


def block_options(opt):
    from conf_opt_earth import opt_earth

    s = '\
\n\
[options]\n\
  old_files: remove\n'

    s += opt_earth(opt['Earth'])

    s += f'\
  method_rivwat: {opt["method_rivwat"]}\n'

    s += '\
[end]\n'

    return s
