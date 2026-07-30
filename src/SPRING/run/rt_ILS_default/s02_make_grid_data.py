import os
import sys
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_grid_data(cnf, update_data, meshNameFmt, lst_landType):
    for landType in lst_landType:
        meshBaseName = util.get_meshBaseName(meshNameFmt)
        meshName = meshNameFmt.format(landType=landType)

        dir_tmp = f'{env.dir_tmp}/{meshBaseName}/{landType}'

        c = cnf[k.m][meshName]

        c['fout_grdara'] = file_bin('grdara.bin')

        f_conf = f'{env.dir_set}/{meshName}.conf'
        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.make_grid_data.head(dir_tmp))
        fp.write(conf.make_grid_data.block_mesh(
                   c, c['dir'], dir_tmp,
                   ['rstidx', 'grdidx'],
                   ['ara']))
        fp.write(conf.make_grid_data.block_options(cnf[k.opt]))
        fp.close()

        if update_data:
            f_log = f'{env.dir_log}/{meshName}.out'
            f_err = f'{env.dir_log}/{meshName}.err'
            util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)

            util.make_slink(os.path.join(dir_tmp, c['fout_grdara']['path']),
              os.path.join(lutil.get_outdir_grid(meshBaseName, landType), 'grdara.bin'))

        c['fin_grdara'] = c['fout_grdara']
        c['fin_grdara']['path'] = os.path.join(os.getcwd(), dir_tmp, c['fin_grdara']['path'])
        del(c['fout_grdara'])


def make_grid_data_all(cnf, update_data):
    make_grid_data(cnf, update_data, 'CMF_{landType}', ['river'])
    make_grid_data(cnf, update_data, 'MATSIRO_{landType}', cnf[k.lt])

    for landType in ['river']:
        cin = cnf[k.m][f'CMF_{landType}']

        c = cnf[k.m][f'CMF_simple_{landType}']
        util.copy_dict_elem(c, cin, 'dir')
        util.copy_dict_elem(c, cin, '_dir')
        util.copy_dict_elem(c, cin, 'fin_grdidx')
        util.copy_dict_elem(c, cin, 'fin_grdara')

        c = cnf[k.m][f'IO_CMF_row_{landType}']
        util.copy_dict_elem(c, cin, 'dir')
        util.copy_dict_elem(c, cin, '_dir')
        util.copy_dict_elem(c, cin, 'fin_grdidx')
        util.copy_dict_elem(c, cin, 'fin_grdara')

    for landType in cnf[k.lt]:
        cin = cnf[k.m][f'MATSIRO_{landType}']

        c = cnf[k.m][f'MATSIRO_simple_{landType}']
        for key in ['dir', '_dir', 'fin_grdidx', 'fin_grdara']:
            util.copy_dict_elem(c, cin, key)

        c = cnf[k.m][f'IO_MATSIRO_row_{landType}']
        for key in ['dir', '_dir', 'fin_grdidx', 'fin_grdara']:
            util.copy_dict_elem(c, cin, key)

        c = cnf[k.m][f'MATSIRO_bnd_simple_{landType}']
        for key_in, key_out in zip(
          ['dir', '_dir', 'fin_grdbndidx', 'fin_grdara'],
          ['dir', '_dir', 'fin_grdidx'   , 'fin_grdara']):
            util.copy_dict_elem(c, cin, key_out, key_in)


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    make_grid_data_all(cnf, update_data)

    util.make_new_f_cnf(cnf)
