import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def define_lsm(cnf, update_data):

    RM = cnf[k.m]['RM']
    for landType in ['river', 'noriv', 'noriv_real', 'noriv_virt']:
        dir_tmp = f'{env.dir_tmp}/LSM/{landType}'
        cnf[k.m][f'LSM_{landType}'] = {
          'name': f'LSM_{landType}',
          'type': 'raster', 
          'nx_raster': RM['nx_raster'],
          'ny_raster': RM['ny_raster'],
          'nx_grid': RM['nx_grid'],
          'ny_grid': RM['ny_grid'],
          'west' : RM['west'],
          'east' : RM['east'],
          'south': RM['south'],
          'north': RM['north'],
          'is_south_to_north': RM['is_south_to_north'],
          'idx_miss': RM['idx_miss'],
          'dir': os.getcwd(),
          'fout_grdmsk'    : file_bin(dir_tmp+'/grdmsk.bin'    ,'real',1,'big'),
          'fout_grdidx'    : file_bin(dir_tmp+'/grdidx.bin'    ,'int4'),
          'fout_grdidx_bnd': file_bin(dir_tmp+'/grdidx_bnd.bin','int4'),
          'fout_grdara'    : file_bin(dir_tmp+'/grdara.bin'    ,'dble'),
          'fout_grdwgt'    : file_bin(dir_tmp+'/grdwgt.bin'    ,'dble'),
          'fout_rstidx'    : file_bin(dir_tmp+'/rstidx.bin'    ,'int4'),
          'fout_rstidx_bnd': file_bin(dir_tmp+'/rstidx_bnd.bin','int4'),
        }

    dir_in_rt = f'{env.sdir_tmp[istep("make_rt_standard-1")]}'

    f_conf = f'{env.dir_set}/a.conf'

    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.cpl_define_mat.head(env.dir_tmp))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'ogcm_ocean_to_agcm', 
               f'{dir_in_rt}/rt_OGCM_ocean_to_AGCM', cnf[k.rtc]))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'rm_river_to_agcm',
               f'{dir_in_rt}/rt_RM_river_to_AGCM', cnf[k.rtc]))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'rm_noriv_to_agcm', 
               f'{dir_in_rt}/rt_RM_noriv_to_AGCM', cnf[k.rtc]))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'rm_ocean_to_agcm', 
               f'{dir_in_rt}/rt_RM_ocean_to_AGCM', cnf[k.rtc]))
    fp.write(conf.cpl_define_mat.block_in_agcm(
               cnf[k.m]['AGCM'], 
               f'{env.sdir_tmp[istep("make_grid_data_GCM")]}/AGCM'))
    fp.write(conf.cpl_define_mat.block_in_rm(
               {'river': cnf[k.m]['RM_river'],
                'noriv': cnf[k.m]['RM_noriv'],
                'ocean': cnf[k.m]['RM_ocean']}))
    fp.write(conf.cpl_define_mat.block_out_rt(
               'lsm_river_to_agcm', f'{env.dir_tmp}/rt_LSM_river_to_AGCM', cnf[k.rtc]))
    fp.write(conf.cpl_define_mat.block_out_rt(
               'lsm_noriv_to_agcm', f'{env.dir_tmp}/rt_LSM_noriv_to_AGCM', cnf[k.rtc]))
    fp.write(conf.cpl_define_mat.block_out_rt(
                'agcm_to_lsm_river', f'{env.dir_tmp}/rt_AGCM_to_LSM_river', cnf[k.rtc]))
    fp.write(conf.cpl_define_mat.block_out_rt(
                'agcm_to_lsm_noriv', f'{env.dir_tmp}/rt_AGCM_to_LSM_noriv', cnf[k.rtc]))

    fp.write(conf.cpl_define_mat.block_out_agcm(f'{env.dir_tmp}/AGCM'))
    fp.write(conf.cpl_define_mat.block_out_lsm(
               {'river'     : cnf[k.m]['LSM_river'],
                'noriv'     : cnf[k.m]['LSM_noriv'],
                'noriv_real': cnf[k.m]['LSM_noriv_real'],
                'noriv_virt': cnf[k.m]['LSM_noriv_virt']},
               ''))
    fp.write(conf.cpl_define_mat.block_options(cnf[k.opt]))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/a.out'
        f_err = f'{env.dir_log}/a.err'
        util.exec_program(const.prog_cpl_define_mat, f_conf, f_log, f_err)

    for landType in ['river', 'noriv', 'noriv_real', 'noriv_virt']:
        LSM = cnf[k.m][f'LSM_{landType}']
        LSM_cmn = {
          'type': 'latlon',
          'nx': LSM['nx_grid'],
          'ny': LSM['ny_grid'],
          'nij': LSM['nx_grid']*LSM['ny_grid'],
          'west' : LSM['west'], 
          'east' : LSM['east'], 
          'south': LSM['south'], 
          'north': LSM['north'], 
          'is_south_to_north': LSM['is_south_to_north'], 
          'idx_miss': LSM['idx_miss'],
          'dir': LSM['dir'],
        }

        cnf[k.m][f'LSM_simple_{landType}'] = dict(
          **LSM_cmn, **{
            'name': f'LSM_simple_{landType}',
            'fin_grdidx': LSM['fout_grdidx'],
            'fin_grdara': LSM['fout_grdara'],
          })

        cnf[k.m][f'IO_LSM_row_{landType}'] = dict(
          **LSM_cmn, **{
            'name': f'IO_LSM_row_{landType}',
            #'fin_grdidx': ,
            'fin_grdara': LSM['fout_grdara'],
          })

    for landType in ['river', 'noriv']:
        LSM = cnf[k.m][f'LSM_{landType}']
        LSM_cmn = {
          'type': 'latlon',
          'nx': LSM['nx_grid'],
          'ny': LSM['ny_grid'],
          'nij': LSM['nx_grid']*LSM['ny_grid'],
          'west' : LSM['west'], 
          'east' : LSM['east'], 
          'south': LSM['south'], 
          'north': LSM['north'], 
          'is_south_to_north': LSM['is_south_to_north'], 
          'idx_miss': LSM['idx_miss'],
          'dir': LSM['dir'],
        }

        cnf[k.m][f'LSM_bnd_simple_{landType}'] = dict(
          **LSM_cmn, **{
            'name': f'LSM_bnd_simple_{landType}',
            'fin_grdidx': LSM['fout_grdidx_bnd'],
            'fin_grdara': LSM['fout_grdara'],
          })

        cnf[k.m][f'IO_LSM_bnd_simple_{landType}'] = dict(
          **LSM_cmn, **{
            'name': f'IO_LSM_bnd_simple_{landType}',
            'fin_grdidx': LSM['fout_grdidx_bnd'],
            'fin_grdara': LSM['fout_grdara'],
          })

    for landType in ['river', 'noriv', 'noriv_real', 'noriv_virt']:
        LSM = cnf[k.m][f'LSM_{landType}']
        for dname in ['grdmsk', 'grdidx', 'grdara', 'grdwgt', 'rstidx']:
            LSM[f'fin_{dname}'] = LSM[f'fout_{dname}']
            del(LSM[f'fout_{dname}'])
        for dname in ['grdidx_bnd', 'rstidx_bnd']:
            del(LSM[f'fout_{dname}'])

    for rtName in ['LSM_{landType}_to_AGCM', 'AGCM_to_LSM_{landType}']:
        util.check_landTypes(cnf[k.irt], rtName, ['river', 'noriv'])
        cnf[k.irt][rtName]['_dir'] = f'{env.dir_tmp}/rt_{rtName}'

    # Make links for output data
    if update_data:
        for landType in ['river', 'noriv', 'noriv_real', 'noriv_virt', 'ogcm']:
            util.make_slink(f'{env.dir_tmp}/AGCM/lndara_{landType}.bin',
                            f'{env.dir_out}/mesh/AGCM/lndara_{landType}.bin')
        for landType in ['river', 'noriv', 'noriv_real', 'noriv_virt']:
            for var_org, var_dst in zip(
              ['msk', 'idx', 'idx_bnd', 'ara', 'wgt'], 
              ['mask', 'index', 'index_bnd', 'area', 'weight']):
                util.make_slink(f'{env.dir_tmp}/LSM/{landType}/grd{var_org}.bin',
                                f'{env.dir_out}/mesh/LSM/{landType}/grid/{var_dst}.bin')
            for var_org, var_dst in zip(
              ['idx', 'idx_bnd'],
              ['index', 'index_bnd']):
                util.make_slink(f'{env.dir_tmp}/LSM/{landType}/rst{var_org}.bin',
                                f'{env.dir_out}/mesh/LSM/{landType}/raster/{var_dst}.bin')



def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    define_lsm(cnf, update_data)

    util.make_new_f_cnf(cnf)
