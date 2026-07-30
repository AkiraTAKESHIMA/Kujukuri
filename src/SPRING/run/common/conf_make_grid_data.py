import os
import sys

import util
from util import str_file_bin


def head(dir_out):
    s = f'\
#\n\
path_report: "{dir_out}/report.txt"\n'

    return s


def block_mesh(gs, dir_in, dir_out, lst_varname_in, lst_varname_out):
    if gs['type'] == 'latlon':
        return block_mesh_latlon(gs, dir_in, dir_out, lst_varname_in, lst_varname_out)
    elif gs['type'] == 'raster':
        return block_mesh_raster(gs, dir_in, dir_out, lst_varname_in, lst_varname_out)
    elif gs['type'] == 'polygon':
        return block_mesh_polygon(gs, dir_in, dir_out, lst_varname_in, lst_varname_out)



def block_mesh_latlon(gs, dir_in, dir_out, lst_varname_in, lst_varname_out):
    s = f'\
\n\
[mesh_latlon]\n\
  name: {gs["name"]}\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n\
\n'

    s += f'\
  dir: "{dir_in}"\n'

    if util.key_val_exist(gs, 'west'):
        s += f'\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n'
    elif util.key_val_exist(gs, 'f_lon_bound'):
        s += f'\
  f_lon_bound: {str_file_bin(gs["f_lon_bound"])}\n'
    else:
        raise Exception('Longit. of grid lines were not defined.')

    if util.key_val_exist(gs, 'south'):
        s += f'\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n'
    elif util.key_val_exist(gs, 'f_lat_bound'):
        s += f'\
  f_lat_bound: {str_file_bin(gs["f_lat_bound"])}\n'
    else:
        raise Exception('Latit. of grid lines were not defined.')

    if util.key_val_exist(gs, 'coord_unit'):
        s += f'\
  coord_unit: {gs["coord_unit"]}\n'

    if 'grdidx' in lst_varname_in and util.key_val_exist(gs, 'fin_grdidx'):
        s += f'\
  fin_grdidx: {str_file_bin(gs["fin_grdidx"])}\n'
    elif util.key_val_exist(gs, 'idx_bgn'):
        s += f'\
  idx_bgn: {gs["idx_bgn"]}\n'

    s += f'\
  is_south_to_north: {gs["is_south_to_north"]}\n'

    s += part_gs_out(gs, dir_out, lst_varname_out)

    s += part_gs_miss(gs)

    s += '\
[end]\n'

    return s


def block_mesh_raster(gs, dir_in, dir_out, lst_varname_in, lst_varname_out):
    s = f'\
\n\
[mesh_raster]\n\
  name: "{gs["name"]}"\n\
  nx: {gs["nx_raster"]}\n\
  ny: {gs["ny_raster"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
\n\
  dir: "{dir_in}"\n\
  fin_rstidx: {str_file_bin(gs["fin_rstidx"])}\n'

    if 'grdidx' in lst_varname_in and util.key_val_exist(gs, 'fin_grdidx'):
        s += f'\
  fin_grdidx: {str_file_bin(gs["fin_grdidx"])}\n'

    s += f'\
  in_grid_sz: {gs["nx_grid"]}, {gs["ny_grid"]}\n'

    if 'idx_condition' in gs.keys():
        s += f'\
  idx_condition: {gs["idx_condition"]}\n'

    s += part_gs_out(gs, dir_out, lst_varname_out)

    s += part_gs_miss(gs)

    s += '\
[end]\n'

    return s


def block_mesh_polygon(gs, dir_in, dir_out, lst_varname_in, lst_varname_out):
    s = f'\
\n\
[mesh_polygon]\n\
  name: "{gs["name"]}"\n\
  np: {gs["np"]}\n\
  nij: {gs["nij"]}\n'

    if util.key_val_exist(gs, 'dir'):
        s += f'\
  dir: "{dir_in}"\n'

    if util.key_val_exist(gs, 'f_lon_vertex'):
        keys = ['f_lon_vertex', 'f_lat_vertex']
    elif util.key_val_exist(gs, 'f_x_vertex'):
        keys = ['f_x_vertex', 'f_y_vertex', 'f_z_vertex']
    else:
        raise Exception('Coordinates of vertices were not given.')
    for key in keys:
        s += f'\
  {key}: {str_file_bin(gs[key])}\n'

    if util.key_val_exist(gs, 'coord_unit'):
        s += f'\
  coord_unit: {gs["coord_unit"]}\n'

    if util.key_val_exist(gs, 'coord_miss'):
        s += f'\
  coord_miss: {gs["coord_miss"]}\n'

    if util.key_val_exist(gs, 'arc_parallel'):
        s += f'\
  arc_parallel: {gs["arc_parallel"]}\n'
    elif util.key_val_exist(gs, 'f_arctyp'):
        s += f'\
  f_arctyp: {str_file_bin(gs["f_arctyp"])}\n'

    if 'grdidx' in lst_varname_in and util.key_val_exist(gs, 'fin_grdidx'):
        s += f'\
  fin_grdidx: {str_file_bin(gs[f"fin_grdidx"])}\n'
    elif util.key_val_exist(gs, 'idx_bgn'):
        s += f'\
  idx_bgn: {gs["idx_bgn"]}\n'

    s += part_gs_out(gs, dir_out, lst_varname_out)

    s += part_gs_miss(gs)

    s += f'\
[end]\n'

    return s


def part_gs_out(gs, dir_out, lst_varname_out):
    if 'fin_grdidx' in gs.keys():
        out_form = 'index'
    else:
        out_form = 'auto'

    s = f'\
\n\
  out_form: {out_form}\n\
  dir: "{dir_out}"\n'

    if 'idx' in lst_varname_out:
        s += f'\
  fout_grdidx: {str_file_bin(gs["fout_grdidx"])}\n'
    if 'ara' in lst_varname_out:
        s += f'\
  fout_grdara: {str_file_bin(gs["fout_grdara"])}\n'
    if 'xyz' in lst_varname_out:
        s += f'\
  fout_grdx  : {str_file_bin(gs["fout_grdx"])}\n\
  fout_grdy  : {str_file_bin(gs["fout_grdy"])}\n\
  fout_grdz  : {str_file_bin(gs["fout_grdz"])}\n'
    if 'lonlat' in lst_varname_out:
        s += f'\
  fout_grdlon: {str_file_bin(gs["fout_grdlon"])}\n\
  fout_grdlat: {str_file_bin(gs["fout_grdlat"])}\n'

    return s


def part_gs_miss(gs):
    s = ''
    if util.key_val_exist(gs, 'idx_miss'):
        s += f'\
  idx_miss: {gs["idx_miss"]}\n'

    s += f'\
  ara_miss: -1d20\n\
  xyz_miss: -1d20\n\
  lonlat_miss: -1d20\n'

    return s


def block_options(opt):
    from conf_opt_earth import opt_earth

    s = '\
\n\
[options]\n\
  old_files: remove\n'

    s += opt_earth(opt['Earth'])

    s += '\
[end]\n'

    return s
