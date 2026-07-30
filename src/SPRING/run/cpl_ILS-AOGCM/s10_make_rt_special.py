import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_rt_all(cnf, update_data):
    dir_rt = f'{env.sdir_tmp[istep("make_rt_standard-1")]}/rt_AGCM_to_OGCM_ocean'
    nij_rt = util.get_nij(f'{dir_rt}/grid.bin', cnf[k.rtc]['dtype_idx'])

    for component in ['LSM', 'RM']:
        if component == 'LSM':
            dir_lsm = env.sdir_tmp[istep('make_grid_data_LSM')]
            lst_landType = ['river', 'noriv_real', 'noriv_virt']
        else:
            dir_lsm = env.sdir_tmp[istep('make_grid_data_RM')]
            lst_landType = ['river_end']

        for landType in lst_landType:
            srcMeshName = f'{component}_simple_{landType}'
            rtName = f'{srcMeshName}_to_OGCM_via_AGCM'
            print(f'{srcMeshName} to OGCM via AGCM')
            dir_tmp = f'{env.dir_tmp}/rt_{rtName}'

            f_conf = f'{env.dir_set}/rt_{rtName}.conf'
            print('config: '+f_conf)
            fp = open(f_conf, 'w')
            fp.write(conf.cpl_make_rt.head(env.dir_tmp))
            fp.write(conf.cpl_make_rt.block_in_rt(dir_rt, nij_rt, cnf[k.rtc]))
            fp.write(conf.cpl_make_rt.block_agcm(cnf[k.m]['AGCM']))
            fp.write(conf.cpl_make_rt.block_lsm(cnf[k.m][srcMeshName]))
            fp.write(conf.cpl_make_rt.block_rt(dir_tmp, cnf[k.rtc]))
            fp.write(conf.cpl_make_rt.block_options(cnf[k.opt]))
            fp.close()

            if update_data:
                f_log = f'{env.dir_log}/{rtName}.out'
                f_err = f'{env.dir_log}/{rtName}.err'
                util.exec_program(const.prog_cpl_make_rt, f_conf, f_log, f_err)

        srcMeshName = f'{component}_simple_{{landType}}'
        rtName = f'{srcMeshName}_to_OGCM_via_AGCM'
        util.check_landTypes(cnf[k.irt], rtName, lst_landType)
        cnf[k.irt][rtName]['_dir'] = f'{env.dir_tmp}/rt_{rtName}'


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_rt_all(cnf, update_data)

    util.make_new_f_cnf(cnf)
