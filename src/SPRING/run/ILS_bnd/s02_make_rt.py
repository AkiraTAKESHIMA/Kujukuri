import os
import sys
import copy
import subprocess
import json
import argparse

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_rt_untiled(cnf, dataName, landType):
    tileName = 'global'
    dir_tmp = f'{env.dir_tmp}/{dataName}'

    f_conf = f'{env.dir_set}/{dataName}/{tileName}_{landType}.conf'
    print(f'config: {f_conf}')
    fp = open(f_conf, 'w')
    fp.write(conf.remap.head(dir_tmp, f'report_{tileName}_{landType}'))
    fp.write(conf.remap.block_mesh(cnf['inputData'][dataName]))
    fp.write(conf.remap.block_mesh(
            cnf[k.m][f'MATSIRO_{landType}'],
            fin_grdidx='fin_grdmkbndidx',
            fin_rstidx='fin_rstmkbndidx'))
    fp.write(conf.remap.block_remapping(cnf[k.rtc], dir_tmp,
            fname_rt_grid=f'mapping_table_idx_{tileName}_{landType}',
            fname_rt_area=f'mapping_table_area_{tileName}_{landType}',
            fname_rt_coef=f'mapping_table_coef_{tileName}_{landType}'))
    fp.write(conf.remap.block_options(cnf[k.opt]))
    fp.close()

    f_log = f'{env.dir_log}/{dataName}/{tileName}_{landType}.out'
    f_err = f'{env.dir_log}/{dataName}/{tileName}_{landType}.err'
    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def make_rt_tiled(cnf, dataName, landType, tileName):
    mat = copy.deepcopy(cnf[k.m][f'MATSIRO_{landType}'])
    dat = copy.deepcopy(cnf['inputData'][dataName])

    dat['name'] = dat['name'] + '_' + tileName
    if dat['type'] == 'latlon':
        west, east, south, north = util.get_tile_bbox_latlon(tileName)
        dat['west']  = west
        dat['east']  = east
        dat['south'] = south
        dat['north'] = north
    elif dat['type'] == 'polygon':
        dat['f_lon_vertex']['path'] = dat['f_lon_vertex']['path'].format(tilename=tileName)
        dat['f_lat_vertex']['path'] = dat['f_lat_vertex']['path'].format(tilename=tileName)
        lon = util.read_bin(dat['f_lon_vertex'], dat['dir'], dat['coord_miss'])
        lat = util.read_bin(dat['f_lat_vertex'], dat['dir'], dat['coord_miss'])
        west, east, south, north = lon.min(), lon.max(), lat.min(), lat.max()
    else:
        raise Exception(f'Invalid value in `dat["type"]`: {dat["type"]}')

    dxi, dxf, dyi, dyf = util.get_raster_bounds(
        mat['west'], mat['east'], mat['south'], mat['north'],
        mat['mx_raster_1deg'], mat['my_raster_1deg'],
        west, east, south, north)
    mat['dxi'] = dxi
    mat['dxf'] = dxf
    mat['dyi'] = dyi
    mat['dyf'] = dyf

    dir_tmp = f'{env.dir_tmp}/{dataName}'

    f_conf = f'{env.dir_set}/{dataName}/{tileName}_{landType}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.remap.head(dir_tmp, f'report_{tileName}_{landType}'))
    fp.write(conf.remap.block_mesh(dat))
    fp.write(conf.remap.block_mesh(
            mat, 
            fin_grdidx='fin_grdmkbndidx',
            fin_rstidx='fin_rstmkbndidx'))
    fp.write(conf.remap.block_remapping(cnf[k.rtc], dir_tmp,
            fname_rt_grid=f'mapping_table_idx_{tileName}_{landType}',
            fname_rt_area=f'mapping_table_area_{tileName}_{landType}',
            fname_rt_coef=f'mapping_table_coef_{tileName}_{landType}'))
    fp.write(conf.remap.block_options(cnf[k.opt]))
    fp.close()

    f_log = f'{env.dir_log}/{dataName}/{tileName}_{landType}.out'
    f_err = f'{env.dir_log}/{dataName}/{tileName}_{landType}.err'
    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def mkdir(dataName, landType):
    os.makedirs(f'{env.dir_set}/{dataName}', exist_ok=True)
    os.makedirs(f'{env.dir_tmp}/{dataName}', exist_ok=True)
    os.makedirs(f'{env.dir_log}/{dataName}', exist_ok=True)


def driv_make_rt(update_data, cnf, 
                 dataName_run, landType_run, tileName_run):
    lst_dataName = cnf['inputData'].keys()

    if dataName_run is not None and dataName_run not in lst_dataName:
        raise Exception(f'Undefined input data "{dataName_run}" was specified.')

    if landType_run is not None and landType_run not in cnf[k.lt]:
        raise Exception(f'Land type "{landType_run}" is invalid. Check the value of "landType" '\
                        'in the configuration file: conf.json')

    for dataName in lst_dataName:
        if dataName_run is not None and dataName != dataName_run: continue

        # Make directories
        for landType in cnf[k.lt]:
            if landType_run is not None and landType != landType_run: continue
            mkdir(dataName, landType)

        # Make remapping tables
        dat = cnf['inputData'][dataName]
        if 'f_list_tiles' in cnf['inputData'][dataName].keys():
            f_list_tiles = os.path.join(dat['dir'], dat['f_list_tiles'])
            if not os.path.isfile(f_list_tiles):
                raise Exception(f'File not found: {f_list_tiles}'+\
                              '\nCheck the values in the setting file.')

            for landType in cnf[k.lt]:
                if landType_run is not None and landType != landType_run: continue

                lst_tileName = [line.strip() for line in open(f_list_tiles, 'r').readlines()]
                if tileName_run is not None and tileName_run not in lst_tileName:
                    raise Exception(f'Tile name "{tileName_run}" is invalid. '+\
                                    f'Check the list of tiles: {f_list_tiles}')
                for tileName in [line.strip() for line in open(f_list_tiles, 'r').readlines()]:
                    if tileName_run is not None and tileName != tileName_run: continue

                    print(f'{dataName} {landType} {tileName}')
                    if update_data:
                        make_rt_tiled(cnf, dataName, landType, tileName)

        else:
            if tileName_run is not None:
                raise Exception(f'Tile name was given but the data "{dataName}" are untiled.')

            for landType in cnf[k.lt]:
                if landType_run is not None and landType != landType_run: continue

                print(f'{dataName} {landType}')
                if update_data:
                    make_rt_untiled(cnf, dataName, landType)

        if update_data:
            util.make_slink(f'{env.dir_tmp}/{dataName}',
                            f'{env.dir_out}/remapping_table/{dataName}')
            

def run(update_data, data, land, tile):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    driv_make_rt(update_data, cnf, data, land, tile)

