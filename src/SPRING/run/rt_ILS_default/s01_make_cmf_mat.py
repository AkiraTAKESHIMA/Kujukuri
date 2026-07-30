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


def make_links_cmf(cnf, lst_landType):
    for landType in lst_landType:
        dir_out = lutil.get_outdir_grid('CMF', landType)
        os.makedirs(dir_out, exist_ok=True)
        CMF = cnf[k.m][f'CMF_{landType}']
        for dname in ['rstidx', 'grdidx']:
            util.make_slink(f'{os.path.join(CMF["dir"], CMF[f"fin_{dname}"]["path"])}',
                            f'{dir_out}/{dname}.bin')


def make_links_mat(cnf, lst_landType):
    for landType in lst_landType:
        dir_out = lutil.get_outdir_grid('MATSIRO', landType)
        os.makedirs(dir_out, exist_ok=True)
        MAT = cnf[k.m][f'MATSIRO_{landType}']
        for dname in ['rstidx', 'grdidx', 'grdbndidx']:
            util.make_slink(f'{os.path.join(MAT["dir"], MAT[f"fin_{dname}"]["path"])}',
                            f'{dir_out}/{dname}.bin')


def make_cmf_mat(cnf, update_data):
    dir_tmp_cmf = f'{env.dir_tmp}/CMF'
    dir_tmp_mat = f'{env.dir_tmp}/MATSIRO'

    dir_out_cmf = f'{env.dir_out}/mesh/CMF'
    dir_out_mat = f'{env.dir_out}/mesh/MATSIRO'

    MAT_river, MAT_noriv = None, None
    if 'river' in cnf[k.lt]: MAT_river = cnf[k.m]['MATSIRO_river']
    if 'noriv' in cnf[k.lt]: MAT_noriv = cnf[k.m]['MATSIRO_noriv']

    CMF_pre = cnf[k.m]['CMF_pre']

    if CMF_pre['inputType'] == 'idxmap':
        landType = 'river'
        MAT = cnf[k.m][f'MATSIRO_{landType}']
        for path in [
          util.filepath(CMF_pre, f'fin_rstidx_{landType}'),
          util.filepath(CMF_pre, f'fin_grdidx_{landType}'),
          util.filepath(MAT, 'fin_rstidx'),
          util.filepath(MAT, 'fin_grdidx'),
          util.filepath(MAT, 'fin_grdbndidx'),
        ]:
            if not os.path.isfile(path):
                raise Exception(f'File not found: {path}')

        landType = 'noriv'
        MAT = cnf[k.m][f'MATSIRO_{landType}']
        for path in [
          util.filepath(MAT, 'fin_rstidx'),
          util.filepath(MAT, 'fin_grdidx'),
          util.filepath(MAT, 'fin_grdbndidx'),
        ]:
            if not os.path.isfile(path):
                raise Exception(f'File not found: {path}')

        print('Mesh data of CMF and MATSIRO already exist.')
        if not update_data:
            return

        # Make links
        for landType in ['river']:
            for dname in ['rstidx', 'grdidx']:
                util.make_slink(
                  os.path.join(CMF_pre['dir'], CMF_pre[f'fin_{dname}_{landType}']['path']),
                  f'{dir_out_cmf}/{landType}/{dname}.bin')
        for landType in cnf[k.lt]:
            MAT = cnf[k.m][f'MATSIRO_{landType}']
            for dname in ['rstidx', 'grdidx', 'grdbndidx']:
                util.make_slink(os.path.join(MAT['dir'], MAT[f'fin_{dname}']['path']),
                                os.path.join(dir_out_mat, f'{landType}/{dname}.bin'))
        return

    for landType in cnf[k.lt]:
        CMF = cnf[k.m][f'CMF_{landType}']
        CMF_pre[f'fout_rstidx_{landType}'] = file_bin(f'{landType}/rstidx.bin')
        CMF_pre[f'fout_grdidx_{landType}'] = file_bin(f'{landType}/grdidx.bin')
    for landType in cnf[k.lt]:
        MAT = cnf[k.m][f'MATSIRO_{landType}']
        MAT['fout_rstidx'] = file_bin(f'{landType}/rstidx.bin')
        MAT['fout_grdidx'] = file_bin(f'{landType}/grdidx.bin')
        MAT['fout_grdbndidx'] = file_bin(f'{landType}/grdbndidx.bin')

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
        for dname in ['rstidx', 'grdidx', 'grdbndidx']:
            key = f'fin_{dname}'
            util.copy_dict_elem(MAT, MAT, f'fin_{dname}', f'fout_{dname}')
            MAT[key]['path'] = os.path.join(os.getcwd(), dir_tmp_mat, MAT[key]['path'])
            del(MAT[f'fout_{dname}'])

    make_links_cmf(cnf, ['river'])
    make_links_mat(cnf, cnf[k.lt])

    del(cnf[k.m]['CMF_pre'])


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_cmf_mat(cnf, update_data)

    util.make_new_f_cnf(cnf)
