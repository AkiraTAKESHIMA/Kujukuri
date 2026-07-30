import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def merge_rt(cnf, update_data, rt, rmp):
    if 'sourceTables' not in rt.keys():
        return

    dir_tmp_make_rt = f'{env.sdir_tmp[istep("make_rt")]}'

    if len(cnf[k.lt]) == 1:
        landType = cnf[k.lt][0]
        rtName = rt['sourceTables'].format(landType=landType)
        dir_rt = f'{dir_tmp_make_rt}/rt_{rtName}'
        util.make_slink(
            f'{dir_rt}',
            f'{env.dir_out}/remapping_table/{rt["name"]}'
        )
        return

    dir_tmp = f'{env.dir_tmp}/rt_{rt["name"]}'

    f_conf = f'{env.dir_set}/rt_{rt["name"]}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.merge_rt.head(dir_tmp))
    fp.write(conf.merge_rt.block_input_bgn())
    for landType in rt[k.lt]:
        rtName = rt['sourceTables'].format(landType=landType)
        dir_rt = f'{dir_tmp_make_rt}/rt_{rtName}'
        nij = util.get_nij(f'{dir_rt}/grid.bin', rmp['dtype_idx'])
        fp.write(conf.merge_rt.block_input_rt(nij, dir_rt, rmp))
    fp.write(conf.merge_rt.block_input_end(opt_idx_duplication=rt['opt_idx_duplication']))
    fp.write(conf.merge_rt.block_output(rmp, dir_tmp, rt))
    fp.write(conf.merge_rt.block_options(cnf[k.opt]))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/rt_{rt["name"]}.out'
        f_err = f'{env.dir_log}/rt_{rt["name"]}.err'
        util.exec_program(const.prog_merge_rt, f_conf, f_log, f_err)

        util.make_slink(
            f'{dir_tmp}', 
            f'{env.dir_out}/remapping_table/{rt["name"]}'
        )



def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    for rtName in cnf[k.rt].keys():
        merge_rt(cnf, update_data, cnf[k.rt][rtName], cnf[k.rtc])

