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

    # Check which of catmxy or idxmap is input or CMF
    keylst_catmxy = ['fin_catmxy', 'fin_nextxy', 'catmxy_index', 'nextxy_index']
    nkey_catmxy = 0
    for key in keylst_catmxy:
        if key in CMF.keys(): nkey_catmxy += 1

    if nkey_catmxy == 0:
        is_catmxy_used = False
    elif nkey_catmxy == len(keylst_catmxy):
        is_catmxy_used = True
    else:
        raise Exception(f'Missing keys for catmxy: {keylst_catmxy}')

    keylst_idxmap = ['fin_rstidx_river', 'fin_grdidx_river']
    nkey_idxmap = 0
    for key in keylst_idxmap:
        if key in CMF.keys(): nkey_idxmap += 1

    if nkey_idxmap == 0:
        is_idxmap_used = False
    elif nkey_idxmap == len(keylst_idxmap):
        is_idxmap_used = True
    else:
        raise Exception(f'Missing keys for idxmap: {keylst_idxmap}')

    if is_catmxy_used and is_idxmap_used:
        raise Exception('Both inputs for catmxy and inputs for idxmap were specified.\n'\
                        f'For catmxy: {keylst_catmxy}\n'\
                        f'For idxmap: {keylst_idxmap}')
    elif not is_catmxy_used and not is_idxmap_used:
        raise Exception('Neither inputs for catmxy and inputs for idxmap was specified.\n'\
                        f'For catmxy: {keylst_catmxy}\n'\
                        f'For idxmap: {keylst_idxmap}')

    meshName = 'CMF_pre'
    cnf[k.m][meshName] = copy.deepcopy(CMF)

    if is_catmxy_used:
        cnf[k.m][meshName]['inputType'] = 'catmxy'
    elif is_idxmap_used:
        cnf[k.m][meshName]['inputType'] = 'idxmap'

    for layer, landType in enumerate(cnf[k.lt]):
        meshName = f'CMF_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        c = cnf[k.m][meshName]
        for key in ['type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
                    'west', 'east', 'south', 'north', 'is_south_to_north',
                    'idx_miss', 'idx_condition', 'dir', '_dir']:
            util.copy_dict_elem(c, CMF, key)
        util.copy_dict_elem(c, CMF, 'fin_rstidx', f'fin_rstidx_{landType}')
        util.copy_dict_elem(c, CMF, 'fin_grdidx', f'fin_grdidx_{landType}')

        meshName = f'CMF_simple_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        cnf[k.m][meshName] = {
          'type': 'latlon',
          'nx': CMF['nx_grid'],
          'ny': CMF['ny_grid'],
        }
        c = cnf[k.m][meshName]
        for key in ['west', 'east', 'south', 'north', 'is_south_to_north']:
            util.copy_dict_elem(c, CMF, key)

        meshName = f'IO_CMF_row_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        c = cnf[k.m][meshName]
        for key in cnf[k.m][f'CMF_simple_{landType}'].keys():
            util.copy_dict_elem(c, cnf[k.m][f'CMF_simple_{landType}'], key)
        for key_in, key_out in zip(
          ['dir', '_dir', 'fin_grdara'],
          ['dir', '_dir', 'fin_grdara']):
            util.copy_dict_elem(c, CMF, key_out, key_in)
        c['idx_bgn'] = (c['nx']*c['ny'])*layer + 1


    for layer, landType in enumerate(cnf[k.lt]):
        meshName = f'MATSIRO_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        c = cnf[k.m][meshName]
        for key in ['type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
                    'west', 'east', 'south', 'north', 'is_south_to_north',
                    'idx_condition', 'dir', '_dir']:
            util.copy_dict_elem(c, CMF, key)
        util.set_dict_default(c, 'idx_miss', -9999)

        MAT = cnf[k.m][meshName]
        MAT_simple_cmn = {
          'type': 'latlon',
          'nx': MAT['nx_grid'],
          'ny': MAT['ny_grid'],
          'west' : MAT['west'],
          'east' : MAT['east'],
          'south': MAT['south'],
          'north': MAT['north'],
          'is_south_to_north': MAT['is_south_to_north'],
          'idx_miss': MAT['idx_miss'],
        }

        meshName = f'MATSIRO_simple_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        c = cnf[k.m][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        for key in ['dir', '_dir', 'fin_grdidx', 'fin_grdara']:
            util.copy_dict_elem(c, CMF, key)

        meshName = f'MATSIRO_bnd_simple_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        c = cnf[k.m][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        for key in ['dir', '_dir']:
            util.copy_dict_elem(c, MAT, key)
        util.copy_dict_elem(c, MAT, 'fin_grdidx', 'fin_grdbndidx')

        meshName = f'IO_MATSIRO_bnd_simple_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        c = cnf[k.m][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        c['idx_bgn'] = (c['nx']*c['ny'])*layer + 1

        meshName = f'IO_MATSIRO_row_{landType}'
        util.add_mesh(cnf[k.m], meshName)
        c = cnf[k.m][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        for key in ['dir', '_dir', 'fin_grdara']:
            util.copy_dict_elem(c, MAT, key)
        c['idx_bgn'] = (c['nx']*c['ny'])*layer + 1

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
