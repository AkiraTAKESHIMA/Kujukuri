import os
import sys

import util
from util import str_file_bin

from conf_make_grid_data import head


def block_in_rt(rtName, dir_in, rmp):
    nij = util.get_nij(f'{dir_in}/grid.bin', rmp['dtype_idx'])

    s = f'\
\n\
[input_rt_{rtName}]\n\
  length: {nij}\n\
  dir: "{dir_in}"\n\
  f_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_area: "area.bin", dble, 1, {rmp["endian"]}\n\
[end]\n'

    return s


def block_in_agcm(gs, dir_in):
    s = f'\
\n\
[input_agcm]\n\
  nij: {gs["nij"]}\n\
  dir: "{dir_in}"\n\
  f_grdidx: "grdidx.bin"\n\
  f_grdara: "grdara.bin"\n\
[end]\n'

    return s


"""
Keys of dct_gs can take any of "river", "noriv" and "ocean"
"""
def block_in_rm(dct_gs):
    gs_river = dct_gs['river']

    s = f'\
\n\
[input_rm]\n\
  nx_raster: {gs_river["nx_raster"]}\n\
  ny_raster: {gs_river["ny_raster"]}\n\
  nx_grid: {gs_river["nx_grid"]}\n\
  ny_grid: {gs_river["ny_grid"]}\n'

    for landType in dct_gs.keys():
        gs = dct_gs[landType]
        s += f'\
\n\
  dir: "{gs["dir"]}"\n\
  f_grdidx_{landType}: {str_file_bin(gs["fin_grdidx"])}\n\
  f_rstidx_{landType}: {str_file_bin(gs["fin_rstidx"])}\n\
  f_grdara_{landType}: {str_file_bin(gs["fin_grdara"])}\n'

    s += f'\
\n\
  idx_miss: {gs_river["idx_miss"]}\n\
[end]\n'

    return s


def block_out_rt(rtName, dir_out, rmp):
    s = f'\
\n\
[output_rt_{rtName}]\n\
  dir: "{dir_out}"\n\
  f_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  f_coef: "coef.bin", dble, 1, {rmp["endian"]}\n\
[end]\n'

    return s


def block_out_agcm(dir_out):
    s = f'\
\n\
[output_agcm]\n\
  dir: "{dir_out}"\n\
  f_lndara_ogcm      : "lndara_ogcm.bin"\n\
  f_lndara_river     : "lndara_river.bin"\n\
  f_lndara_noriv_real: "lndara_noriv_real.bin"\n\
  f_lndara_noriv_virt: "lndara_noriv_virt.bin"\n\
  f_lndara_noriv     : "lndara_noriv.bin"\n\
[end]\n'

    return s


"""
Keys of dct_gs can take any of "river", "noriv", "noriv_real" and "noriv_virt"
"""
def block_out_lsm(dct_gs, dir_out):
    s = f'\
\n\
[output_lsm]\n\
  dir: "{dir_out}"\n'

    for landType in dct_gs.keys():
        s += f'\
\n'
        gs = dct_gs[landType]
        for dname in ['grdmsk', 'grdidx', 'grdidx_bnd', 'grdara', 'grdwgt', 
                      'rstidx', 'rstidx_bnd']:
            key = f'fout_{dname}'
            if util.key_val_exist(gs, key):
                s += f'\
  f_{dname}_{landType}: {str_file_bin(gs[f"{key}"])}\n'

    s += f'\
[end]\n'

    return s


def block_options(opt):
    s = '\
\n\
[options]\n\
  old_files: remove\n\
[end]\n'

    return s
