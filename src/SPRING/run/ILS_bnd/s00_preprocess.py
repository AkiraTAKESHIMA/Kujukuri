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


def preprocess(cnf):
    CMF = cnf[k.m]['CMF']

    meshName = 'CMF_pre'
    cnf[k.m][meshName] = {}
    c = cnf[k.m][meshName]
    for key in [
        'type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
        'west', 'east', 'south', 'north', 'is_south_to_north',
        'idx_miss', 'idx_condition',
        'dir', '_dir', 'fin_catmxy', 'fin_nextxy', 
        'catmxy_index', 'nextxy_index',
        'fin_rstidx_river', 'fin_grdidx_river',
        'fin_rstidx_noriv', 'fin_grdidx_noriv',
    ]:
        util.copy_dict_elem(c, CMF, key)

    for landType in cnf[k.lt]:
        meshName = f'CMF_{landType}'
        cnf[k.m][meshName] = {}
        c = cnf[k.m][meshName]
        for key in [
            'type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
            'west', 'east', 'south', 'north', 'is_south_to_north',
            'idx_miss', 'idx_condition',
        ]:
            util.copy_dict_elem(c, CMF, key)
        util.copy_dict_elem(c, CMF, 'dir')
        util.copy_dict_elem(c, CMF, '_dir')
        util.copy_dict_elem(c, CMF, 'fin_rstidx', f'fin_rstidx_{landType}')
        util.copy_dict_elem(c, CMF, 'fin_grdidx', f'fin_grdidx_{landType}')

    for layer, landType in enumerate(cnf[k.lt]):
        meshName = f'MATSIRO_{landType}'
        if meshName not in cnf[k.m].keys():
            cnf[k.m][meshName] = {}
        c = cnf[k.m][meshName]
        for key in [
            'type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
            'west', 'east', 'south', 'north', 'is_south_to_north',
            'idx_condition',
        ]:
            util.copy_dict_elem(c, CMF, key)
        util.set_dict_default(c, 'idx_miss', -9999)
        c['mx_raster_1deg'] = int(c['nx_raster'] / (c['east']-c['west']))
        c['my_raster_1deg'] = int(c['ny_raster'] / (c['north']-c['south']))
        util.set_dict_default(c, 'dir', '')

    del(cnf[k.m]['CMF'])

    for meshName in cnf[k.m].keys():
        cnf[k.m][meshName]['name'] = meshName



def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    preprocess(cnf)

    util.make_new_f_cnf(cnf)

