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
    dir_tmp_cmf = f'{env.dir_tmp}/CMF'
    dir_tmp_mat = f'{env.dir_tmp}/MATSIRO'

    dir_out_cmf = f'{env.dir_out}/CMF'
    dir_out_mat = f'{env.dir_out}/MATSIRO'

    MAT_river, MAT_noriv = None, None
    if 'river' in cnf[k.lt]: MAT_river = cnf[k.m]['MATSIRO_river']
    if 'noriv' in cnf[k.lt]: MAT_noriv = cnf[k.m]['MATSIRO_noriv']

    CMF_pre = cnf[k.m]['CMF_pre']

    is_ok = True
    for landType in cnf[k.lt]:
        is_ok = is_ok and\
        util.key_val_exist(CMF_pre, f'fin_rstidx_{landType}') and\
        util.key_val_exist(CMF_pre, f'fin_grdidx_{landType}') and\
        util.key_val_exist(cnf[k.m][f'MATSIRO_{landType}'], 'fin_rstidx') and\
        util.key_val_exist(cnf[k.m][f'MATSIRO_{landType}'], 'fin_grdidx') and\
        util.key_val_exist(cnf[k.m][f'MATSIRO_{landType}'], 'fin_grdbndidx')

    if is_ok:
        print('CMF and MATSIRO grid data already exist.')
        if update_data:
            for landType in cnf[k.lt]:
                util.make_slink(os.path.join(CMF_pre['dir'], CMF_pre[f'fin_rstidx_{landType}']),
                                os.path.join(dir_out_cmf, f'{landType}/rstidx.bin'))
                util.make_slink(os.path.join(CMF_pre['dir'], CMF_pre[f'fin_grdidx_{landType}']),
                                os.path.join(dir_out_cmf, f'{landType}/grdidx.bin'))
                MAT = cnf[k.m][f'MATSIRO_{landType}']
                util.make_slink(os.path.join(MAT['dir'], MAT['fin_rstidx']),
                                os.path.join(dir_out_mat, f'{landType}/rstidx.bin'))
                util.make_slink(os.path.join(MAT['dir'], MAT['fin_grdidx']),
                                os.path.join(dir_out_mat, f'{landType}/grdidx.bin'))
                util.make_slink(os.path.join(MAT['dir'], MAT['fin_grdbndidx']),
                                os.path.join(dir_out_mat, f'{landType}/grdbndidx.bin'))
        return

    for landType in cnf[k.lt]:
        CMF = cnf[k.m][f'CMF_{landType}']
        CMF_pre[f'fout_rstidx_{landType}'] = file_bin(f'{landType}/rstidx.bin')
        CMF_pre[f'fout_grdidx_{landType}'] = file_bin(f'{landType}/grdidx.bin')

        MAT = cnf[k.m][f'MATSIRO_{landType}']

        MAT['fout_grdmsk'] = file_bin(
             f'{landType}/land_mask.bin', 
             dtype=const.str_dtype_real,
             endian=const.endian_big)

        MAT['fout_rstidx'] = file_bin(f'{landType}/rstidx.bin')
        MAT['fout_grdidx'] = file_bin(f'{landType}/grdidx.bin')

        MAT['fout_rstbndidx'] = file_bin(f'{landType}/rstbndidx.bin')
        MAT['fout_grdbndidx'] = file_bin(f'{landType}/grdbndidx.bin')

        MAT['fout_rstmkbndidx'] = file_bin(f'{landType}/rstmkbndidx.bin')
        MAT['fout_grdmkbndidx'] = file_bin(f'{landType}/grdmkbndidx.bin')

    f_conf = f'{env.dir_set}/a.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_cmf_mat.block_common(CMF_pre))
    fp.write(conf.make_cmf_mat.block_cmf(CMF_pre, CMF_pre['dir'], dir_tmp_cmf))
    fp.write(conf.make_cmf_mat.block_matsiro(MAT_river, MAT_noriv, dir_tmp_mat))
    fp.write(conf.make_cmf_mat.block_options(cnf[k.opt]))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/a.out'
        f_err = f'{env.dir_log}/a.err'
        util.exec_program(const.prog_make_cmf_mat, f_conf, f_log, f_err)

    for landType in cnf[k.lt]:
        CMF = cnf[k.m][f'CMF_{landType}']
        for dname in ['rstidx', 'grdidx']:
            key = f'fin_{dname}'
            util.copy_dict_elem(CMF, CMF_pre, key, f'fout_{dname}_{landType}')
            CMF[key]['path'] = os.path.join(os.getcwd(), dir_tmp_cmf, CMF[key]['path'])

        MAT = cnf[k.m][f'MATSIRO_{landType}']
        for dname in [
                'rstidx'     , 'grdidx'     , 
                'rstbndidx'  , 'grdbndidx'  ,
                'rstmkbndidx', 'grdmkbndidx']:
            key = f'fin_{dname}'
            util.copy_dict_elem(MAT, MAT, f'fin_{dname}', f'fout_{dname}')
            MAT[key]['path'] = os.path.join(os.getcwd(), dir_tmp_mat, MAT[key]['path'])
            del(MAT[f'fout_{dname}'])
    del(cnf[k.m]['CMF_pre'])

    # Make links for output data
    if update_data:
        util.make_slink(f'{env.dir_tmp}/CMF',
                        f'{env.dir_out}/mesh/CMF')
        util.make_slink(f'{env.dir_tmp}/MATSIRO',
                        f'{env.dir_out}/mesh/MATSIRO')


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_cmf_mat(cnf, update_data)

    util.make_new_f_cnf(cnf)

