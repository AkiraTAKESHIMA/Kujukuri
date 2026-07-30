import os
import sys
import shutil
import subprocess
import copy
import json
import numpy as np

import const
from const import k


class Env():
    def __init__(self):
        self.job = None
        self.step_max = None
        self.f_cnf = None
        self.runName = None
        self.sdir_tmp = None
        self.sdir_set = None
        self.sdir_log = None
        self.dir_tmp = None
        self.dir_set = None
        self.dir_log = None

    def put_job(self, a):
        self.job = a
        self.step_max = len(a.keys()) - 1

    def put_f_cnf(self, a):
        self.f_cnf = a
        if not os.path.isfile(a):
            raise Exception(f'Config file not found: {a}')

        cnf = json.load(open(a, 'r'))
        self.runName = cnf[k.run]

    def get_job(self):
        return self.job

    def get_f_cnf(self):
        return self.f_cnf

    def set_dir(self, step):
        self.sdir_tmp = {}
        self.sdir_set = {}
        self.sdir_log = {}
        for i in range(self.step_max+1):
            jobId = f'{i:02d}_{self.job[i]}'
            #self.sdir_tmp[i] = os.path.join(self.runName, 'tmp', jobId)
            #self.sdir_set[i] = os.path.join(self.runName, 'set', jobId)
            #self.sdir_log[i] = os.path.join(self.runName, 'log', jobId)
            self.sdir_tmp[i] = 'tmp/' + jobId
            self.sdir_set[i] = 'set/' + jobId
            self.sdir_log[i] = 'log/' + jobId
        self.dir_tmp = self.sdir_tmp[step]
        self.dir_set = self.sdir_set[step]
        self.dir_log = self.sdir_log[step]
        #self.dir_out = os.path.join(self.runName, 'out')
        self.dir_out = 'out'
        os.makedirs(self.dir_tmp, exist_ok=True)
        os.makedirs(self.dir_set, exist_ok=True)
        os.makedirs(self.dir_log, exist_ok=True)
        os.makedirs(self.dir_out, exist_ok=True)

env = Env()


def read_cnf(step):
    if step == 0:
        f = env.f_cnf
    else:
        f = f'{env.sdir_set[step-1]}/newconf.json'
    print(f'[step {step}] Overall settings: {f}')
    return json.load(open(f, 'r'))


def make_new_f_cnf(cnf):
    f = f'{env.dir_set}/newconf.json'
    print('New settings: '+f)
    with open(f, 'w') as fp:
        fp.write(json.dumps(cnf, indent=2))


def exec_program(prog, f_conf, f_log, f_err):
    pc = subprocess.run([prog, f_conf],
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                       encoding='utf-8')

    with open(f_log, 'w') as fp:
        fp.write(pc.stdout)
    with open(f_err, 'w') as fp:
        fp.write(pc.stderr)
    print(f'stdout: {f_log}')
    print(f'stderr: {f_err}')

    if pc.returncode != 0:
        raise Exception(f'Program `{prog}` failed. See log file: {f_log}')
        #print(f'*** Program `{prog}` failed.')


def make_slink(org, dst):
    if not (os.path.isfile(org) or os.path.isdir(org)):
        raise Exception(f'File or directory not found: {org}')
    if os.path.isfile(dst) or os.path.islink(dst):
        os.remove(dst)
    elif os.path.isdir(dst):
        shutil.rmtree(dst)

    os.makedirs(os.path.dirname(dst), exist_ok=True)

    cdir = os.getcwd()
    absorg = os.path.join(cdir, org)
    absdst = os.path.join(cdir, dst)
    pc = subprocess.run([const.command_ln] + const.options_ln.strip().split() + \
                        [absorg, absdst],
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        encoding='utf-8')
    if pc.returncode != 0:
        command = f'{const.command_ln} {const.options_ln} {absorg} {absdst}'
        raise Exception(f'Failed: \n{command}')

    cwd = os.getcwd()

    print(f'linked: {path_abs_to_rel(absdst)} -> {path_abs_to_rel(absorg)}')


def path_abs_to_rel(abspath):
    cwd = os.getcwd() + '/'
    relpath = abspath
    if cwd in abspath:
        if abspath.index(cwd) == 0:
            relpath = abspath[len(cwd):]
    return relpath


def istep(name):
    if env.job is None:
        raise Exception('`env.job` is undefined.')

    for key in env.job.keys():
        if env.job[key] == name:
            return key
    raise Exception(f'Invalid value in $name: {name}')


def key_val_exist(dct, key):
    if key not in dct.keys():
        return False
    elif dct[key] is None:
        return False
    else:
        return True


def get_val(dct: dict, key: str, key_default: str, allow_empty: bool = True):
    if key == '':
        val = None
    else:
        if key is None:
            if key_val_exist(dct, key_default):
                val = dct[key_default]
            else:
                val = None
        else:
            if not key_val_exist(dct, key):
                raise Exception(f'Key "{key}" is not used.')
            val = dct[key]

    if not allow_empty and val is None:
        raise Exception(f'Key "{key}" is not used although `allow_empty` is False.')

    return val


def set_dict_default(d, key, val):
    if key not in d.keys():
        d[key] = val


def copy_dict_elem(dout, din, kout, kin=None):
    if kin is None:
        kin = kout
    if not kout in dout.keys() and kin in din.keys():
        dout[kout] = copy.deepcopy(din[kin])


def filepath(dct, key_file):
    return os.path.join(dct['dir'], dct[key_file]['path'])


def add_mesh(cnf_gs, meshName):
    if meshName not in cnf_gs.keys():
        cnf_gs[meshName] = {}


def check_landTypes(cnf_int_rt, rtName, lst_landType):
    if cnf_int_rt[rtName]['landTypes'] !=  lst_landType:
        raise Exception('The list "landTypes" of the intermediate remapping table '+\
                        f'"{rtName}" is incorrect.')


def get_meshBaseName(meshName):
    if 'landType' in meshName:
        return meshName[:meshName.index('landType')-2]
    else:
        return meshName


def meshName_from_rtName(rtName):
    loc = rtName.index('_to_')
    return rtName[:loc], rtName[loc+4:]


def get_tile_bbox_latlon(tileName):
    if tileName[0] not in ['W','E'] or tileName[4] not in ['N','S'] or len(tileName) != 7:
        raise Exception(('Unexpected format of tileName: {}'\
                       '\ntileName must be like "W140N40" or "E010S20".').format(tileName))

    if tileName[0] == 'W':
        west = -int(tileName[1:4])
    else:
        west = int(tileName[1:4])
    east = west + 10

    if tileName[4] == 'N':
        north = int(tileName[5:7])
    else:
        north = -int(tileName[5:7])
    south = north - 10

    return west, east, south, north


def get_raster_bounds(
      west, east, south, north, mx_1deg, my_1deg,
      twest, teast, tsouth, tnorth):
    dxi = int(np.floor((twest-west) * mx_1deg+1))
    dxf = int(np.ceil((teast-west) * mx_1deg))
    dyi = int(np.floor((north-tnorth) * my_1deg+1))
    dyf = int(np.ceil((north-tsouth) * my_1deg))

    return dxi, dxf, dyi, dyf


def get_nij(f_rt_idx, dtype):
    if os.path.getsize(f_rt_idx) % (byte(dtype)*2) != 0:
        raise Exception('Invalid file size.')
    return int(os.path.getsize(f_rt_idx) / (byte(dtype)*2))


def set_gs_dir(gs, dir_top):
    if 'dir' in gs.keys():
        gs['dir'] = os.path.join(dir_top, gs['dir'])


def set_dir(dir_top, runName, job):
    d = {}
    for step in job.keys():
        d[step] = os.path.join(dir_top, runName, f'{step:02d}_{job[step]}')
    return d


def read_bin(f, d='', miss=None):
    if miss is None:
        return np.fromfile(os.path.join(d, f['path']), dtype_str_to_np(f['dtype']))
    else:
        return np.ma.masked_equal(
                 np.fromfile(os.path.join(d, f['path']), dtype_str_to_np(f['dtype'])), 
                 miss)


def file_bin(path, dtype=None, rec=None, endian=None):
    f = {'path': path}
    if dtype is not None:
        f['dtype'] = dtype
    if endian is not None:
        f['endian'] = str_endian(endian)
    if rec is not None:
        f['rec'] = rec
    return f


def dtype_str_to_np(dtype_str):
    if dtype_str == const.str_dtype_int1:
        return np.int8
    elif dtype_str == const.str_dtype_int2:
        return np.int16
    elif dtype_str == const.str_dtype_int4:
        return np.int32
    elif dtype_str == const.str_dtype_int8:
        return np.int64
    elif dtype_str == const.str_dtype_real:
        return np.float32
    elif dtype_str == const.str_dtype_dble:
        return np.float64


def str_endian(s):
    if s in ['little_endian', 'little']:
        return 'little_endian'
    elif s in ['big_endian', 'big']:
        return 'big_endian'
    else:
        raise Exception(f'Invalid value in $s: {s}')


def get_endian(dct):
    if 'endian' in dct.keys():
        return dct['endian']
    else:
        return ''


def str_file_bin(f):
    if 'path' not in f.keys():
        raise Exception('"path" was not given for the file.')
    s = f'"{f["path"]}"'
    if 'dtype' in f.keys():
        s += f', dtype={f["dtype"]}'
    if 'rec' in f.keys():
        s += f', rec={f["rec"]}'
    if 'endian' in f.keys():
        s += f', endian={f["endian"]}'

    return s


def byte(dtype):
    if dtype == 'int1':
        b = 1
    elif dtype == 'int2':
        b = 2
    elif dtype == 'int4':
        b = 4
    elif dtype == 'int8':
        b = 8
    elif dtype == 'real':
        b = 4
    elif dtype == 'dble':
        b = 8
    else:
        raise Exception(f'Invalid value in $dtype: {dtype}')

    return b

