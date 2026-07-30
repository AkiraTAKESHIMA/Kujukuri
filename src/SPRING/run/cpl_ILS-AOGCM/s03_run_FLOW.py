import os
import sys
import shutil
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def run_FLOW(cnf, update_data):
    #
    # Prepare directories and input data
    #
    dir_flwdir = os.path.join(env.dir_tmp, '1min_flwdir')
    if update_data:
        if os.path.isdir(dir_flwdir):
            shutil.rmtree(dir_flwdir)
        os.makedirs(dir_flwdir, exist_ok=True)

    for var in ['flwdir', 'elevtn', 'lndmsk']:
        org = os.path.join(os.path.abspath(cnf['FLOW']['dir']), cnf['FLOW'][var])
        dst = os.path.join(dir_flwdir, f'{var}.bin')
        if update_data:
            os.symlink(org, dst)

        cnf['FLOW'][var] = f'{var}.bin'
    cnf['FLOW']['dir'] = os.path.join(os.getcwd(), dir_flwdir)

    dir_gcmmap = os.path.join(env.dir_tmp, 'gcmmap')
    if update_data:
        os.makedirs(dir_gcmmap, exist_ok=True)

    dir_tmp = os.path.join(env.dir_tmp, 'tmp')
    if update_data:
        os.makedirs(os.path.join(dir_tmp, '1min'), exist_ok=True)
        os.makedirs(os.path.join(dir_tmp, 'map/1min'), exist_ok=True)

    dir_map = os.path.join(env.dir_tmp, 'map')
    if update_data:
        os.makedirs(dir_map, exist_ok=True)

    #
    # Make a setting file for FLOW
    #
    f_params = os.path.join(env.dir_tmp, 'params.txt')
    fp = open(f_params,'w')
    fp.write(f'\
{cnf[f"{k.m}"]["RM"]["nx_grid"]}  ! nXX\n\
{cnf[f"{k.m}"]["RM"]["ny_grid"]}  ! nYY\n\
""  ! fgcmidx\n')

    e = cnf[k.opt]['Earth']
#    fp.write(f'\
#{cnf[f"{k.opt}"]["Earth"]["geosys"]}  ! geodesic system\n'
    fp.write(f'\
{e["geosys"]}\n')
    if e['geosys'] != 'other':
        fp.write(f'\
{e["rtyp"]}\n')
    else:
        fp.write(f'\
{e["r"]}\n\
{e["finv"]}\n')
#{cnf[f"{k.opt}"]["Earth"]["r"]}  ! radius [m]\n\
#0.d0  ! square of the earth\'s eccentricity\n')
    fp.close()

    #
    # Run FLOW
    #
    dir_log = f'{os.getcwd()}/{env.dir_log}'
    dir_FLOW_src = f'{os.getcwd()}/{const.dir_FLOW_src}'

    cwd = os.getcwd()
    os.chdir(env.dir_tmp)

    for i, prog in enumerate([
      'make_gcmmap.exe',
      'modify_hires.exe', 
      'calc_uparea.exe',
      'const_network.exe',
      'define_catchment.exe',
      'visual_check.exe',
      'gcm_rivermap.exe',
      'set_map.exe',
      'calc_inpmat.exe',
    ]):
        if update_data:
            print(f'Executing {prog}...')
            f_log = f'{dir_log}/{i+1:02d}_{prog}.out'
            f_err = f'{dir_log}/{i+1:02d}_{prog}.err'
            util.exec_program(f'{dir_FLOW_src}/{prog}', '', f_log, f_err)

    os.chdir(cwd)

    RM = cnf[k.m]['RM']
    cnf[k.m]['RM_pre'] = {
      'nx_raster': RM['nx_raster'],
      'ny_raster': RM['ny_raster'],
      'nx_grid': RM['nx_grid'],
      'ny_grid': RM['ny_grid'],
      'west' : RM['west'],
      'east' : RM['east'],
      'south': RM['south'],
      'north': RM['north'],
      'is_south_to_north': RM['is_south_to_north'],
      'dir': os.getcwd(),
      'fin_catmxy': file_bin(f'{env.dir_tmp}/tmp/map/1min/catmxy.bin'),
      'fin_nextxy': file_bin(f'{env.dir_tmp}/map/nextxy.bin'),
      'fin_basin' : file_bin(f'{env.dir_tmp}/map/basin.bin'),
      'catmxy_index': {
        'noriv_coastal': 0,
        'noriv_inland' : -1,
        'ocean'        : -9999,
      },
      'nextxy_index': {
        'river_mouth' : -9,
        'river_inland': -10,
        'ocean'       : -9999,
      },
      'idx_miss': RM['idx_miss'],
    }

    util.make_slink(f'{env.dir_tmp}/tmp/map/1min', 
                    f'{env.dir_out}/mesh/RM_CMF/raster')
    util.make_slink(f'{env.dir_tmp}/map',
                    f'{env.dir_out}/mesh/RM_CMF/grid')


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    run_FLOW(cnf, update_data)

    util.make_new_f_cnf(cnf)
