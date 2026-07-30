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


def merge_rt(cnf, update_data, 
             rtName):
    rt = cnf[k.rt][rtName]
    rtiName = rt['sourceTables']
    rti = cnf[k.irt][rtiName]
    srcMeshNameFmt, tgtMeshNameFmt = util.meshName_from_rtName(rtiName)
    srcMeshName, tgtMeshName = util.meshName_from_rtName(rtName)

    dir_tmp = f'{env.dir_tmp}/rt_{rtName}'

    f_conf = f'{env.dir_set}/rt_{rtName}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.merge_rt.head(dir_tmp))
    fp.write(conf.merge_rt.block_input_bgn())
    for landType in rt['landTypes']:
        srcMeshName_this = srcMeshNameFmt.format(landType=landType)
        tgtMeshName_this = tgtMeshNameFmt.format(landType=landType)
        dir_rt = rti['_dir'].format(landType=landType)
        nij = util.get_nij(dir_rt+'/grid.bin', cnf[k.rtc]['dtype_idx'])
        fp.write(conf.merge_rt.block_input_rt(nij, dir_rt, cnf[k.rtc]))
    fp.write(conf.merge_rt.block_input_end())
    fp.write(conf.merge_rt.block_output(cnf[k.rtc], dir_tmp, rt))
    fp.write(conf.merge_rt.block_options(cnf[k.opt]))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/rt_{rtName}.out'
        f_err = f'{env.dir_log}/rt_{rtName}.err'
        util.exec_program(const.prog_merge_rt, f_conf, f_log, f_err)

    # Make links for output data
    if update_data:
        util.make_slink(f'{env.dir_tmp}/rt_{rtName}',
                        f'{env.dir_out}/remapping_table/{rtName}')



def merge_rt_all(cnf, update_data):

    for rtKey in cnf[k.rt].keys():
        #if rtKey != "RM_to_OGCM_via_AGCM": continue
        print(rtKey)
        merge_rt(cnf, update_data, rtKey)


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    merge_rt_all(cnf, update_data)

    util.make_new_f_cnf(cnf)


