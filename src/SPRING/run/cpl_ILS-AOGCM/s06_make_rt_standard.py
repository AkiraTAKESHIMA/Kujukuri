import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_rt(cnf, update_data, srcMeshNameFmt, tgtMeshNameFmt, lst_landType):
    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        runname = f'rt_{srcMeshName}_to_{tgtMeshName}'
        print(f'{srcMeshName} to {tgtMeshName}')

        f_conf = f'{env.dir_set}/{runname}.conf'
        dir_tmp = f'{env.dir_tmp}/{runname}'

        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.remap.head(dir_tmp))
        fp.write(conf.remap.block_mesh(cnf[k.m][srcMeshName]))
        fp.write(conf.remap.block_mesh(cnf[k.m][tgtMeshName]))
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
    make_rt(cnf, update_data, 'OGCM_{landType}', 'AGCM', ['ocean'])

    make_rt(cnf, update_data, 'AGCM', 'OGCM_{landType}', ['ocean'])

    make_rt(cnf, update_data, 'RM_{landType}', 'AGCM', ['river', 'noriv', 'ocean'])


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_rt_all(cnf, update_data)

    util.make_new_f_cnf(cnf)

