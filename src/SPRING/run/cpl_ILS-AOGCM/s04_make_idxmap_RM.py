import os
import sys
import subprocess
import copy
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_cmf_mat(cnf, update_data):
    lst_landType = ['river', 'river_end', 'noriv', 'ocean']

    RM_pre = cnf[k.m]['RM_pre']
    for landType in lst_landType:
        RM_pre[f'fout_rstidx_{landType}'] = file_bin(f'{env.dir_tmp}/{landType}/rstidx.bin','int4')
        RM_pre[f'fout_grdidx_{landType}'] = file_bin(f'{env.dir_tmp}/{landType}/grdidx.bin','int4')
    RM_pre['fout_rstbsn'] = file_bin(f'{env.dir_tmp}/river/rstbsn.bin','int4')

    f_conf = f'{env.dir_set}/a.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_cmf_mat.block_common(RM_pre))
    fp.write(conf.make_cmf_mat.block_cmf(RM_pre, RM_pre['dir'], ''))
    fp.write(conf.make_cmf_mat.block_options(cnf[k.opt]))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/a.out'
        f_err = f'{env.dir_log}/a.err'
        util.exec_program(const.prog_make_cmf_mat, f_conf, f_log, f_err)

    RM_cmn = {
      'type': 'raster',
      'nx_raster': RM_pre['nx_raster'],
      'ny_raster': RM_pre['ny_raster'],
      'nx_grid': RM_pre['nx_grid'],
      'ny_grid': RM_pre['ny_grid'],
      'west' : RM_pre['west'], 
      'east' : RM_pre['east'], 
      'south': RM_pre['south'], 
      'north': RM_pre['north'], 
      'is_south_to_north': RM_pre['is_south_to_north'], 
      'idx_miss': RM_pre['idx_miss'],
      'dir': RM_pre['dir'],
    }
    RM_simple_cmn = {
      'type': 'latlon',
      'nx': RM_pre['nx_grid'],
      'ny': RM_pre['ny_grid'],
      'nij': RM_pre['nx_grid']*RM_pre['ny_grid'],
      'west' : RM_pre['west'], 
      'east' : RM_pre['east'], 
      'south': RM_pre['south'], 
      'north': RM_pre['north'], 
      'is_south_to_north': RM_pre['is_south_to_north'], 
      'idx_miss': RM_pre['idx_miss'],
      'dir': RM_pre['dir'],
    }

    for landType in lst_landType:
        cnf[k.m][f'RM_{landType}'] = dict(
          **RM_cmn, **{
            'name': f'RM_{landType}',
            'fin_rstidx': RM_pre[f'fout_rstidx_{landType}'],
            'fin_grdidx': RM_pre[f'fout_grdidx_{landType}'],
          })

        cnf[k.m][f'RM_simple_{landType}'] = dict(
          **RM_simple_cmn, **{
            'name': f'RM_simple_{landType}',
            'fin_grdidx': RM_pre[f'fout_grdidx_{landType}'],
          })

        cnf[k.m][f'IO_RM_row_{landType}'] = dict(
          **RM_simple_cmn, **{
            'name': f'IO_RM_row_{landType}',
          })

    del(cnf[k.m]['RM_pre'])

    if update_data:
        for landType in lst_landType:
            util.make_slink(f'{env.dir_tmp}/{landType}/grdidx.bin',
                            f'{env.dir_out}/mesh/RM/{landType}/grid/index.bin')
            util.make_slink(f'{env.dir_tmp}/{landType}/rstidx.bin',
                            f'{env.dir_out}/mesh/RM/{landType}/raster/index.bin')
        util.make_slink(f'{env.dir_tmp}/river/rstbsn.bin',
                        f'{env.dir_out}/mesh/RM/river/raster/basin.bin')


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_cmf_mat(cnf, update_data)

    util.make_new_f_cnf(cnf)
