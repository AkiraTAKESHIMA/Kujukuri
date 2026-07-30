import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


#def make_rtdummy_AGCM_to_RM():


def make_rt(cnf, update_data,
            srcMeshNameFmt, tgtMeshNameFmt, 
            use_grdara_src, use_grdara_tgt, 
            lst_landType):

    if use_grdara_src:
        key_fin_grdara_src = 'fin_grdara'
    else:
        key_fin_grdara_src = ''

    if use_grdara_tgt:
        key_fin_grdara_tgt = 'fin_grdara'
    else:
        key_fin_grdara_tgt = ''

    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        runname = f'rt_{srcMeshName}_to_{tgtMeshName}'
        print(f'{srcMeshName} to {tgtMeshName}')

        dir_tmp = f'{env.dir_tmp}/{runname}'

        f_conf = f'{env.dir_set}/{runname}.conf'
        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.head(dir_tmp))
        fp.write(conf.remap.block_mesh(cnf[k.m][srcMeshName], fin_grdara=key_fin_grdara_src))
        fp.write(conf.remap.block_mesh(cnf[k.m][tgtMeshName], fin_grdara=key_fin_grdara_tgt))
        fp.write(conf.remap.block_remapping(cnf[k.rtc], dir_tmp))
        fp.write(conf.remap.block_options(cnf[k.opt]))
        fp.close()

        if update_data:
            f_log = f'{env.dir_log}/{runname}.out'
            f_err = f'{env.dir_log}/{runname}.err'
            util.exec_program(const.prog_remap, f_conf, f_log, f_err)

    rtName = f'{srcMeshNameFmt}_to_{tgtMeshNameFmt}'
    util.check_landTypes(cnf[k.irt], rtName, lst_landType)
    cnf[k.irt][rtName]['_dir'] = f'{env.dir_tmp}/rt_{rtName}'


def make_rt_all(cnf, update_data):

    # AGCM to RM

    # IO_bnd to LSM
    make_rt(cnf, update_data,
            'IO_LSM_bnd_simple_{landType}', 'LSM_bnd_simple_{landType}', 
            False, False,
            ['river', 'noriv'])

    # IO_met to LSM
    make_rt(cnf, update_data,
            'IO_met', 'LSM_{landType}', 
            False, False, 
            ['river', 'noriv_real', 'noriv_virt'])

    # IO_metnc to LSM
    make_rt(cnf, update_data,
            'IO_metnc', 'LSM_{landType}', 
            False, False,
            ['river', 'noriv_real', 'noriv_virt'])

    # LSM to RM
    make_rt(cnf, update_data,
            'LSM_simple_{landType}', 'RM_simple_{landType}', 
            False, False,
            ['river'])

    # RM to LSM
    make_rt(cnf, update_data,
            'RM_simple_{landType}', 'LSM_simple_{landType}', 
            False, False,
            ['river'])

    # LSM to IO_row
    # rt_area will be incorrect if rt is calculated as if they are latlon grids,
    # so grdara is given to IO_LSM_row. This is different from grid area of latlon grid,
    # so verification data for LSM_latlon_* will be incorrect.
    make_rt(cnf, update_data,
            'LSM_simple_{landType}', 'IO_LSM_row_{landType}', 
            False, True,
            ['river', 'noriv_real', 'noriv_virt'])

    # LSM to IO_latlon
    make_rt(cnf, update_data,
            'LSM_{landType}', 'IO_latlon', 
            False, False,
            ['river', 'noriv_real', 'noriv_virt'])

    # RM to IO_row
    # grdara is given to IO_RM_row for the same reason with LSM to IO_row.
    make_rt(cnf, update_data,
            'RM_simple_{landType}', 'IO_RM_row_{landType}', 
            False, True,
            ['river'])

    # RM to IO_latlon
    make_rt(cnf, update_data, 
            'RM_{landType}', 'IO_latlon', 
            False, False,
            ['river'])


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_rt_all(cnf, update_data)

    util.make_new_f_cnf(cnf)

