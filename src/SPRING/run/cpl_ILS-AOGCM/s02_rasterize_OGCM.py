import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def rasterize(cnf, update_data):
    f_conf = f'{env.dir_set}/a.conf'
    print('config: '+f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.remap.head(env.dir_tmp))
    fp.write(conf.remap.block_mesh(cnf[k.m]['OGCM_land']))
    fp.write(conf.rasterize.block_raster(cnf[k.m]['RM']))
    fp.write(conf.rasterize.block_output(env.dir_tmp))
    fp.write(conf.remap.block_options(cnf[k.opt]))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/a.out'
        f_err = f'{env.dir_log}/a.err'
        util.exec_program(const.prog_rasterize, f_conf, f_log, f_err)

    dir_tmp = os.path.join(os.getcwd(), env.dir_tmp)

    c = cnf['FLOW']
    c['lndmsk'] = dir_tmp+'/mask.bin'


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    rasterize(cnf, update_data)

    util.make_new_f_cnf(cnf)

