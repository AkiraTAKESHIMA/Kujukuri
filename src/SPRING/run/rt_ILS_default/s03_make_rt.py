import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_rt(cnf, update_data,
            rt, is_tmp):

    rtNameFmt = rt['name']
    srcMeshNameFmt, tgtMeshNameFmt = util.meshName_from_rtName(rtNameFmt)

    for landType in rt['landTypes']:
        rtName = rtNameFmt.format(landType=landType)
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)

        dir_tmp = f'{env.dir_tmp}/rt_{rtName}'

        f_conf = f'{env.dir_set}/rt_{rtName}.conf'
        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.head(dir_tmp))
        fp.write(conf.remap.block_mesh(cnf[k.m][srcMeshName], rt['use_src_grdara']))
        fp.write(conf.remap.block_mesh(cnf[k.m][tgtMeshName], rt['use_tgt_grdara']))
        fp.write(conf.remap.block_remapping(cnf[k.rtc], dir_tmp))
        fp.write(conf.remap.block_options(cnf[k.opt]))
        fp.close()

        if update_data:
            f_log = f'{env.dir_log}/rt_{rtName}.out'
            f_err = f'{env.dir_log}/rt_{rtName}.err'
            util.exec_program(const.prog_remap, f_conf, f_log, f_err)

            if not is_tmp:
                util.make_slink(f'{dir_tmp}', f'{const.dir_out}/rt_{rtName}')

    rt['_dir'] = dir_tmp


def make_rt_all(cnf, update_data):
    for rtName in cnf[k.irt].keys():
        print(rtName)
        #if rtName != 'MATSIRO_simple_{landType}_to_CMF_simple_{landType}': continue
        rt = cnf[k.irt][rtName]
        make_rt(cnf, update_data, rt, True)

    for rtName in cnf[k.rt].keys():
        rt = cnf[k.rt][rtName]
        if 'sourceTables' in rt.keys(): continue
        make_rt(cnf, update_data, rt, False)


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    # Check 'landType'
    if len(cnf[k.lt]) == 1:
        if cnf[k.lt][0] != 'river':
            raise Exception(f'The list "{k.lt}" must be `["river"]` '\
                            'when its length is 1.')
    elif len(cnf[k.lt]) == 2:
        if not ('river' in cnf[k.lt] and 'noriv' in cnf[k.lt]):
            raise Exception(f'The list "{k.lt}" must be `["river", "noriv"]` '\
                            'when its length is 1.')
    else:
        raise Exception(f'Length of the list "{k.lt}" must be '\
                        'less than or equal to 2.')

    env.set_dir(step)

    make_rt_all(cnf, update_data)

    util.make_new_f_cnf(cnf)

