import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_grid_data(cnf, update_data, gsName):

    c = cnf[k.m][gsName]
    dir_tmp = os.path.join(os.getcwd(), env.dir_tmp, gsName)
    c['fout_grdidx'] = file_bin(dir_tmp+'/grdidx.bin'   ,'int4')
    c['fout_grdara'] = file_bin(dir_tmp+'/grdara.bin'   ,'dble')
    c['fout_grdx'  ] = file_bin(dir_tmp+'/grdxyz.bin'   ,'dble',1)
    c['fout_grdy'  ] = file_bin(dir_tmp+'/grdxyz.bin'   ,'dble',2)
    c['fout_grdz'  ] = file_bin(dir_tmp+'/grdxyz.bin'   ,'dble',3)
    c['fout_grdlon'] = file_bin(dir_tmp+'/grdlonlat.bin','dble',1)
    c['fout_grdlat'] = file_bin(dir_tmp+'/grdlonlat.bin','dble',2)

    dir_tmp = os.path.join(env.dir_tmp, gsName)

    f_conf = f'{env.dir_set}/{gsName}.conf'
    print('config: '+f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.head(dir_tmp))
    fp.write(conf.make_grid_data.block_mesh(
               c, c['dir'], '', 
               ['grdidx'], ['idx', 'ara', 'xyz', 'lonlat']))
    fp.write(conf.remap.block_options(cnf[k.opt]))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/{gsName}.out'
        f_err = f'{env.dir_log}/{gsName}.err'
        util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)

    dir_tmp = os.path.join(os.getcwd(), env.dir_tmp, gsName)
    if c['type'] == 'latlon':
        c['nij'] = c['nx'] * c['ny']
    for var in ['idx', 'ara', 'x', 'y', 'z', 'lon', 'lat']:
        c[f'fin_grd{var}'] = c[f'fout_grd{var}']
        del(c[f'fout_grd{var}'])

    if update_data:
        util.make_slink(f'{env.dir_tmp}/{gsName}',
                        f'{env.dir_out}/mesh/{gsName}')


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_grid_data(cnf, update_data, 'AGCM')
    make_grid_data(cnf, update_data, 'OGCM_ocean')

    util.make_new_f_cnf(cnf)

