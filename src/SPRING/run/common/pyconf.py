import os
import sys
import copy

import const, util
from const import k


def format_any(a, d_fill):
    if type(a) is list:
        return [format_any(x, d_fill) for x in a]
    elif type(a) is dict:
        for key in a.keys():
            a[key] = format_any(a[key], d_fill)
        return a
    elif type(a) is str:
        return a.format(**d_fill)
    else:
        return a


def fill_landTypes(cnfin, lst_landType_default=None, landType=None):
    if type(cnfin) is dict:
        cnfout = {}
        for key in cnfin.keys():
            c = cnfin[key]
            if '{landType}' in key:
                if k.lt in cnfin[key]:
                    lst_landType = c[k.lt]
                elif lst_landType_default is not None:
                    lst_landType = lst_landType_default
                else:
                    raise Exception(f'"{k.lt}" is undefined in "{key}" or in the overall settings.')
                for landType in lst_landType:
                    key_new = key.format(landType=landType)
                    #print(f'key_new: {key_new}, landType: {landType}')
                    cnfout[key_new] = copy.deepcopy(c)
                    if k.lt in cnfout[key_new].keys():
                        del(cnfout[key_new][k.lt])
                    cnfout[key_new] = fill_landTypes(cnfout[key_new], lst_landType_default, landType)
            else:
                cnfout[key] = fill_landTypes(c, lst_landType_default, landType)
    elif type(cnfin) is list:
        cnfout = []
        for c in cnfin:
            cnfout.append(fill_landTypes(c))
    elif type(cnfin) is str:
        cnfout = cnfin
        if '{landType}' in cnfout:
            cnfout = cnfout.format(landType=landType)
    else:
        cnfout = cnfin

    return cnfout


def fill_landTypes_meshes(cnf):
    if k.lt in cnf.keys():
        lst_landType_default = cnf[k.lt]
    else:
        lst_landType_default = None
    cnf[k.m] = fill_landTypes(cnf[k.m], lst_landType_default)

    return cnf


def copy_gs_shared(cnfin):
    cnf = copy.deepcopy(cnfin)
    for gsName in cnf[k.m].keys():
        if not 'share' in cnf[k.m][gsName].keys(): continue
        gsName_shared = cnf[k.m][gsName]['share']
        #print(f'{gsName_shared} < {gsName}')
        if gsName_shared not in cnf[k.m].keys():
            raise Exception(f'An undefined grid system "{cnf[k.m][key]["share"]}" '\
                            f'appeared in "{gsName}" > "share".')
        for key in cnf[k.m][gsName_shared].keys():
            if key in cnf[k.m][gsName].keys(): continue
            cnf[k.m][gsName][key] = copy.deepcopy(cnf[k.m][gsName_shared][key])

    return cnf


def set_rt_default(cnf):
    for rtName in cnf[k.irt].keys():
        rt = cnf[k.irt][rtName]
        rt['name'] = rtName

        if 'use_src_grdara' not in rt.keys():
            rt['use_src_grdara'] = False

        if 'use_tgt_grdara' not in rt.keys():
            rt['use_tgt_grdara'] = False

        if 'mesh_coef' not in rt.keys():
            rt['mesh_coef'] = 'target'

        if 'mesh_sort' not in rt.keys():
            rt['mesh_sort'] = 'target'

        if 'opt_coef_sum_modify' not in rt.keys() and \
           'opt_coef_sum_modify_ulim' not in rt.keys():
            rt['opt_coef_sum_modify'] = 1.0

        if 'opt_idx_duplication' not in rt.keys():
            rt['opt_idx_duplication'] = 'stop'

    for rtName in cnf[k.rt].keys():
        rt = cnf[k.rt][rtName]
        rt['name'] = rtName

        if 'sourceTable' not in rt.keys():
            if 'use_src_grdara' not in rt.keys():
                rt['use_src_grdara'] = False
            if 'use_tgt_grdara' not in rt.keys():
                rt['use_tgt_grdara'] = False

        if 'mesh_coef' not in rt.keys():
            rt['mesh_coef'] = 'target'

        if 'mesh_sort' not in rt.keys():
            rt['mesh_sort'] = 'target'

        if 'opt_coef_sum_modify' not in rt.keys() and \
           'opt_coef_sum_modify_ulim' not in rt.keys():
            rt['opt_coef_sum_modify'] = 1.0

        if 'opt_idx_duplication' not in rt.keys():
            rt['opt_idx_duplication'] = 'stop'

    return cnf



def add_cnfkey_dir(cnf):
    if '_dir' in cnf.keys() and 'dir' not in cnf.keys():
        cnf['dir'] = cnf['_dir']

    for key in cnf.keys():
        if type(cnf[key]) is dict:
            cnf[key] = add_cnfkey_dir(cnf[key])

    return cnf


def join_topdir(cnf, dir_top):
    cnf = add_cnfkey_dir(cnf)

    for key in cnf.keys():
        if type(cnf[key]) is dict:
            cnf[key] = join_topdir(cnf[key], dir_top)
        elif type(cnf[key]) is str:
            if key == 'dir':
                cnf[key] = os.path.join(dir_top, cnf[key])
            elif key == '_dir':
                #if 'dir' in cnf.keys():
                #    raise Exception('"_dir" and "dir" cannot be given for one data.')
                cnf['dir'] = os.path.abspath(cnf['_dir'])
    return cnf



