import os
import sys
import copy
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl


POS_NORMAL = 0
POS_BOUND  = 1
POS_POLAR  = 2

DTYPE_INT1 = 'int1'
DTYPE_INT2 = 'int2'
DTYPE_INT4 = 'int4'
DTYPE_INT8 = 'int8'
DTYPE_REAL = 'real'
DTYPE_DBLE = 'dble'

dict_dtype = {
  DTYPE_INT1: np.int8,
  DTYPE_INT2: np.int16,
  DTYPE_INT4: np.int32,
  DTYPE_INT8: np.int64,
  DTYPE_REAL: np.float32,
  DTYPE_DBLE: np.float64,
}

ENDIAN_LITTLE_LONG  = 'little_endian'
ENDIAN_LITTLE_SHORT = 'little'
ENDIAN_BIG_LONG     = 'big_endian'
ENDIAN_BIG_SHORT    = 'big'

MESHTYPE__LATLON  = 'latlon'
MESHTYPE__RASTER  = 'raster'
MESHTYPE__POLYGON = 'polygon'

MESH__SOURCE = 'source'
MESH__TARGET = 'target'
MESH__NONE   = ''

UNIT_DEGREE = 'degree'
UNIT_RADIAN = 'radian'
UNIT_METER = 'm'
UNIT_KILOMETER = 'km'

COORD_MISS_S_DEFAULT = -1e20
COORD_MISS_C_DEFAULT = -1e20

d2r = np.pi/180.
r2d = 180./np.pi
rad_0deg = 0. * d2r
rad_90deg = 90. * d2r
rad_180deg = 180. * d2r
rad_360deg = 360. * d2r


def get_rgba(cmap, vmin, vmax, val):
    return cmap(min(255,max(0,round((val-vmin) / (vmax-vmin) * 255))))


def gen_cmap_name(cols_):
    if len(cols_) == 1:
        cols = [cols_[0], cols_[0]]
    else:
        cols = cols_

    nmax = float(len(cols)-1)
    color_list = []
    for n, c in enumerate(cols):
        color_list.append((n/float(nmax), c))

    return mpl.colors.LinearSegmentedColormap.from_list('cmap', color_list)


def draw_latlon(lon, lat, val, is_south_to_north, cfg):

    if cfg['cmap'] is None:
        cmap = None
    else:
        cmap = mpl.colormaps[cfg['cmap']]

    # Draw (contourf)
    if cfg['mode'] is None or cfg['mode'] == PLT_MODE_CONTOURF:
        clon = np.empty(lon.size-1)
        for ix in range(lon.size-1):
            if abs(lon[ix+1] - lon[ix]) > rad_180deg:
                clon[ix] = (lon[ix] + lon[ix+1] - rad_360deg) * 0.5
                if clon[ix] < -rad_180deg: clon[ix] += rad_360deg
            else:
              clon[ix] = (lon[ix] + lon[ix+1]) * 0.5

        clat = np.r_[-rad_90deg, np.sort(((lat[:lat.size-1] + lat[1:]) * 0.5)), rad_90deg]

        ix_left = None
        for ix in range(lon.size):
            if abs(lon[ix] - lon[ix-1]) > rad_180deg:
                ix_left = ix

        if ix_left is None:
            clon = np.r_[lon[-1]-rad_360deg,lon,lon[0]+rad_360deg]
        else:
            clon = np.r_[lon[ix_left-1]-rad_360deg,lon[ix_left:],
                         lon[:ix_left],lon[ix_left]+rad_360deg]

        if ix_left is None:
            val_plt = np.c_[val[:,-1],val,val[:,0]]
        else:
            val_plt = np.c_[val[:,ix_left-1],val[:,ix_left:],val[:,:ix_left],val[:,ix_left]]
        val_plt = np.r_[val_plt[:1,:], val_plt, val_plt[-1:,:]]
        val_plt = np.ma.masked_equal(val_plt, cfg['val_miss'])

        if not is_south_to_north:
            val_plt = val_plt[::-1]

        clons, clats = np.meshgrid(clon,clat)

        # Plot
        fig = plt.figure(figsize=cfg['figsize'])
        ax = fig.add_subplot(111)
        xticks = np.arange(-180,181,30)
        yticks = np.arange(-90,91,30)
        ax.axis([-180, 180, -90, 90])
        ax.set_xticks(xticks, xticks)
        ax.set_yticks(yticks, yticks)
        ax.set_aspect('equal')
        ax.set_title('(min,max): ({:9.2e},{:9.2e})'.format(np.nanmin(val), np.nanmax(val)))

        # Paint
        if cmap is not None:
            img = ax.contourf(clons*r2d, clats*r2d, val_plt, levels=500, cmap=cmap,
                              vmin=cfg['vmin'], vmax=cfg['vmax'])

        # Draw grid lines
        if cfg['linewidth'] != 0:
            ax.hlines(lat*r2d, -180, 180, 
                      linewidth=cfg['linewidth'], color=cfg['edgecolor'])
            ax.vlines(lon*r2d, -90, 90, 
                      linewidth=cfg['linewidth'], color=cfg['edgecolor'])

        # Plot colorbar
        norm = mpl.colors.Normalize(vmin=cfg['vmin'], vmax=cfg['vmax'])
        mappable = mpl.cm.ScalarMappable(cmap=cmap, norm=norm)
        mappable._A = []
        cb = fig.colorbar(mappable, ax=ax, aspect=50, pad=0.1,
                          shrink=0.9, orientation='horizontal')

    else:
        raise Exception('Invalid value in $cfg["mode"]: {cfg["mode"]}')

    # Set the title
    if cfg['title'] != '':
      ax.set_title(cfg['title'])

    # Save the figure
    if cfg['path'] != '':
        mkdir(cfg['path'])
        plt.savefig(cfg['path'], bbox_inches='tight', pad_inches=0, dpi=cfg['dpi'])

    # Display the figure
    if cfg['show']:
        plt.show()

    plt.close()


def draw_raster(west, east, south, north, is_south_to_north, 
                rstval, cfg):
    if cfg['cmap'] == 'none':
        cmap = None
    else:
        cmap = mpl.colormaps[cfg['cmap']]

    paint_miss = cfg['val_miss'] is not None and cfg['color_miss'] is not None
    if paint_miss:
        cmap_miss = gen_cmap_name([cfg['color_miss']])
        mask_miss = np.zeros(rstval.shape)
        mask_miss[rstval == cfg['val_miss']] = 1

    fig = plt.figure(figsize=cfg['figsize'])
    ax = fig.add_subplot(111)
    xticks = np.arange(-180,181,30)
    yticks = np.arange(-90,91,30)
    ax.axis([west, east, south, north])
    ax.set_xticks(xticks)
    ax.set_xticklabels(xticks)
    ax.set_yticks(yticks)
    ax.set_yticklabels(yticks)
    ax.set_aspect('equal')
    ax.set_title('(min,max): ({:9.2e},{:9.2e})'.format(np.nanmin(val), np.nanmax(val)))

    if is_south_to_north:
        ydir = -1
    else:
        ydir = 1

    if cmap is not None:
        if paint_miss:
            img_miss = ax.imshow(np.ma.masked_equal(mask_miss,0)[::ydir],
                                 cmap=cmap_miss, interpolation='nearest', vmin=0, vmax=1,
                                 extent=[west,east,south,north])

        img = ax.imshow(np.ma.masked_equal(rstval,cfg['val_miss'])[::ydir],
                        cmap=cmap, interpolation='nearest', 
                        vmin=cfg['vmin'], vmax=cfg['vmax'],
                        extent=[west,east,south,north])
        cb = fig.colorbar(img, ax=ax, aspect=50, pad=0.1,
                          shrink=0.8, orientation='horizontal')

    # Set the title
    if cfg['title'] != '':
        ax.set_title(cfg['title'])

    # Save the figure
    if cfg['path'] != '':
        mkdir(cfg['path'])
        plt.savefig(cfg['path'], bbox_inches='tight', pad_inches=0, dpi=cfg['dpi'])

    # Display the figure
    if cfg['show']:
        plt.show()

    plt.close()


def draw_polygon(lon, lat, coord_miss, val, cfg):
    #ij_debug = np.arange(92160-1,92160-6,-1)
    ij_debug = []

    if cfg['cmap'] == 'none':
        cmap = None
    else:
        cmap = mpl.colormaps[cfg['cmap']]

    lon_plt, lat_plt = [[] for i in range(257)], [[] for i in range(257)]
    for ij, v in enumerate(val):
        #print(ij)
        if len(ij_debug) != 0 and ij not in ij_debug: continue

        if ij in ij_debug:
            print('ij: {}'.format(ij))
            print('lon: {}'.format(lon[ij,:]*r2d))
            print('lat: {}'.format(lat[ij,:]*r2d))

        lon_mod, lat_mod = modify_input(lon[ij,:], lat[ij,:], coord_miss)
        if ij in ij_debug:
            print('lon_mod: {}'.format(lon_mod*r2d))
            print('lat_mod: {}'.format(lat_mod*r2d))

        pos = get_pos(lon_mod, lat_mod, coord_miss)
        if ij in ij_debug:
            print('pos: {}'.format(pos))

        lon_add, lat_add = get_coords_polygon(lon_mod, lat_mod, pos, coord_miss)

        is_miss = False
        if cfg['val_miss'] is not None:
            if v == cfg['val_miss']:
                is_miss = True
        if is_miss:
            ic = 256
        else:
            ic = max(0,min(255,
                   round(255 * (v - cfg['vmin']) / (cfg['vmax']-cfg['vmin']))
                 ))

        for lon_add_each, lat_add_each in zip(lon_add,lat_add):
            lon_plt[ic] += list(lon_add_each*r2d) + [np.nan]
            lat_plt[ic] += list(lat_add_each*r2d) + [np.nan]

    # Draw nothing
    if cfg['linewidth'] == 0 and cmap is None:
        return

    # Draw grid lines and paint
    elif cfg['linewidth'] != 0 and cmap is not None:
        fig = plt.figure(figsize=cfg['figsize'])
        ax = fig.add_subplot(111)

        # Plot grid values
        for ic in range(256):
            ax.fill(lon_plt[ic], lat_plt[ic],
                    facecolor=cmap(ic), 
                    edgecolor=cfg['edgecolor'], linewidth=cfg['linewidth'])

        ax.fill(lon_plt[256], lat_plt[256],
                facecolor=cfg['color_miss'], 
                edgecolor=cfg['edgecolor'], linewidth=cfg['linewidth'])

        # Plot colorbar
        norm = mpl.colors.Normalize(vmin=cfg['vmin'], vmax=cfg['vmax'])
        mappable = mpl.cm.ScalarMappable(cmap=cmap, norm=norm)
        mappable._A = []
        cb = fig.colorbar(mappable, ax=ax, aspect=50, pad=0.1,
                          shrink=0.9, orientation='horizontal')

    # Not draw grid lines but paint
    elif cfg['linewidth'] == 0 and cmap is not None:
        fig = plt.figure(figsize=cfg['figsize'])
        ax = fig.add_subplot(111)

        # Plot grid values
        for ic in range(256):
            ax.fill(lon_plt[ic], lat_plt[ic], facecolor=cmap(ic), 
                    edgecolor=cmap(ic), linewidth=cfg['linewidth_fill'])

        ax.fill(lon_plt[256], lat_plt[256],
                facecolor=cfg['color_miss'], edgecolor=cfg['color_miss'],
                linewidth=cfg['linewidth_fill'])

        # Plot colorbar
        norm = mpl.colors.Normalize(vmin=cfg['vmin'], vmax=cfg['vmax'])
        mappable = mpl.cm.ScalarMappable(cmap=cmap, norm=norm)
        mappable._A = []
        cb = fig.colorbar(mappable, ax=ax, aspect=50, pad=0.1,
                          shrink=0.9, orientation='horizontal')

    # Draw only grid lines
    elif cfg['linewidth'] != 0 and cmap is None:
        fig, ax = plt.subplots()

        # Draw grid lines
        for ic in range(257):
            ax.plot(lon_plt[ic], lat_plt[ic], 
                    linewidth=cfg['linewidth'], color=cfg['edgecolor'])

    # Modify region, aspect and labels
    xticks = np.arange(-180,181,30)
    yticks = np.arange(-90,91,30)
    ax.axis([-180, 180, -90, 90])
    ax.set_xticks(xticks, xticks)
    ax.set_yticks(yticks, yticks)
    ax.set_aspect('equal')
    ax.set_title('(min,max): ({:9.2e},{:9.2e})'.format(np.nanmin(val), np.nanmax(val)))

    # Set the title
    if cfg['title'] != '':
      ax.set_title(cfg['title'])

    # Save the figure
    if cfg['path'] != '':
        mkdir(cfg['path'])
        plt.savefig(cfg['path'], bbox_inches='tight', pad_inches=0, dpi=cfg['dpi'])

    # Display the figure
    if cfg['show']:
        plt.show()

    plt.close()


def get_coords_polygon(lon, lat, pos, coord_miss):
    if pos == POS_POLAR:
        lon_ret, lat_ret = [], []
        if lat[0] > rad_0deg:
            lat_pole = rad_90deg
        else:
            lat_pole = -rad_90deg

        for i in range(len(lon)):
            if abs(lon[i-1]-lon[i]) > rad_180deg:
                if lon[i-1] < lon[i]:
                    lon_add1, lat_add1 \
                      = get_coords_polygon_2(
                          [lon[i-1],lon[i]-rad_360deg,np.nan],
                          [lat[i-1],lat[i],lat_pole],
                          coord_miss)
                    lon_add2, lat_add2 \
                      = get_coords_polygon_2(
                          [lon[i-1]+rad_360deg,lon[i],np.nan],
                          [lat[i-1],lat[i],lat_pole],
                          coord_miss)
                else:
                    lon_add1, lat_add1 \
                      = get_coords_polygon_2(
                          [lon[i-1]-rad_360deg,lon[i],np.nan],
                          [lat[i-1],lat[i],lat_pole], 
                          coord_miss)
                    lon_add2, lat_add2 \
                      = get_coords_polygon_2(
                          [lon[i-1],lon[i]+rad_360deg,np.nan],
                          [lat[i-1],lat[i],lat_pole],
                          coord_miss)
                lon_ret += [lon_add1, lon_add2]
                lat_ret += [lat_add1, lat_add2]
            else:
                lon_add, lat_add \
                  = get_coords_polygon_2(
                      [lon[i-1],lon[i],np.nan],
                      [lat[i-1],lat[i],lat_pole], 
                      coord_miss)
                lon_ret += [lon_add]
                lat_ret += [lat_add]

    elif pos == POS_BOUND:
        lon_add1, lat_add1 \
          = get_coords_polygon_2(
              [lon[i]+rad_360deg if lon[i] < rad_0deg and lon[i] != coord_miss \
               else lon[i] for i in range(len(lon))], 
              lat, coord_miss)
        lon_add2, lat_add2 \
          = get_coords_polygon_2(
              [lon[i]-rad_360deg if lon[i] > rad_0deg and lon[i] != coord_miss \
               else lon[i] for i in range(len(lon))],
              lat, coord_miss)
        lon_ret = [lon_add1, lon_add2]
        lat_ret = [lat_add1, lat_add2]
        
    else:
        lon_add, lat_add = get_coords_polygon_2(lon, lat, coord_miss)
        lon_ret = [lon_add]
        lat_ret = [lat_add]

    return lon_ret, lat_ret


def get_coords_polygon_2(lon, lat, coord_miss):
    lon_add, lat_add = [], []
    for i in range(len(lon)):
        if abs(lat[i]) == rad_90deg:
            lon_add.append(lon[i-1])
            if i == len(lon)-1:
                lon_add.append(lon[0])
            else:
                lon_add.append(lon[i+1])
            lat_add.append(lat[i])
            lat_add.append(lat[i])
        else:
            lon_add.append(lon[i])
            lat_add.append(lat[i])
    if len(lon_add) == 0:
        return np.array([]), np.array([])

    lon_add.append(lon_add[0])
    lat_add.append(lat_add[0])

    lon_add = np.array(lon_add)
    lat_add = np.array(lat_add)

    for i in range(len(lon_add)):
        if lon_add[i] == coord_miss:
            raise Exception('coord_miss was found.')

    return lon_add, lat_add


def modify_input(lon, lat, coord_miss):
    lon_mod, lat_mod = [], []
    for i in range(len(lon)):
        if lat[i] == coord_miss:
            continue
        elif lon[i] == lon[i-1] and lat[i] == lat[i-1]:
            continue
        lon_mod.append(lon[i])
        lat_mod.append(lat[i])
    return np.array(lon_mod), np.array(lat_mod)


def get_pos(lon, lat, coord_miss):

    counter_lon0 = 0
    has_pole = False
    #imax = len(lon)
    #for i, i_next in zip(range(imax), list(range(1,imax-1))+[0]):
    for i in range(len(lon)):
        # polar
        if abs(lat[i]) == rad_90deg or abs(lat[i-1]) == rad_90deg:
            has_pole = True
        # lon0
        elif abs(lon[i] - lon[i-1]) > rad_180deg:
            counter_lon0 += 1
        # normal (meridian)
        elif lon[i] == lon[i-1]:
            pass
        # normal (other than meridian)
        else:
            pass

    if counter_lon0 == 0:
        pos = POS_NORMAL
    elif not has_pole and counter_lon0 % 2 == 1:
        pos = POS_POLAR
    else:
        pos = POS_BOUND

    return pos


def read_coord_polygon(f_lon, f_lat, f_x, f_y, f_z, pmax, ijmax, 
                       coord_unit, coord_miss_s, coord_miss_c):
    if f_lon is not None:
        lon = np.fromfile(f_lon['path'],dtype=dict_dtype[f_lon['dtype']])\
                .reshape(-1,ijmax,pmax)[f_lon['rec']-1]
        lat = np.fromfile(f_lat['path'],dtype=dict_dtype[f_lat['dtype']])\
                .reshape(-1,ijmax,pmax)[f_lat['rec']-1]
        if coord_unit == UNIT_DEGREE:
            mask = lat != coord_miss_s
            lon[mask] *= np.pi/180.
            lat[mask] *= np.pi/180.
        elif coord_unit == UNIT_RADIAN:
            pass
        else:
            raise Exception(f'Invalid value in $coord_unit: {coord_unit}')
            quit()

    else:
        x = np.fromfile(f_x['path'],dtype=dict_dtype[f_x['dtype']])\
              .reshape(-1,ijmax,pmax)[f_x['rec']-1]
        y = np.fromfile(f_y['path'],dtype=dict_dtype[f_y['dtype']])\
              .reshape(-1,ijmax,pmax)[f_y['rec']-1]
        z = np.fromfile(f_z['path'],dtype=dict_dtype[f_z['dtype']])\
              .reshape(-1,ijmax,pmax)[f_z['rec']-1]
        if f_x['endian'] in [ENDIAN_BIG_LONG,ENDIAN_BIG_SHORT]:
            x = x.byteswap()
            y = y.byteswap()
            z = z.byteswap()

        mask = z != coord_miss_c
        lon = np.empty((ijmax,pmax))
        lat = np.empty((ijmax,pmax))
        lon[mask] = np.arctan2(y[mask],x[mask])
        lat[mask] = np.arcsin(z[mask])
        lon[~mask] = coord_miss_s
        lat[~mask] = coord_miss_s


    for ij in range(lon.shape[0]):
        any_positive = False
        has_180 = False
        for p in range(pmax):
            if lon[ij,p] == coord_miss_s:
                continue
            if lon[ij,p] > rad_180deg:
                lon[ij,p] -= rad_360deg
            elif lon[ij,p] <= -rad_180deg:
                lon[ij,p] += rad_360deg

            if rad_0deg < lon[ij,p] < rad_180deg:
                any_positive = True
            elif lon[ij,p] == rad_180deg:
                has_180 = True

        if not any_positive and has_180:
            for p in range(pmax):
                if lon[ij,p] == rad_180deg:
                    lon[ij,p] -= rad_360deg

    return lon, lat


def read_coord_latlon(
      nx, ny, west, east, south, north,
      f_lon_bound, f_lat_bound, coord_unit):

    if coord_unit == UNIT_DEGREE:
        coef = np.pi/180.
    elif coord_unit == UNIT_RADIAN:
        coef = 1.
    else:
        raise Exception('Invalid value in "coord_unit": {}'.format(coord_unit))

    if f_lon_bound is None:
        if west < east:
            lon = np.linspace(west*coef, east*coef, nx+1)[:-1]
        else:
            lon = np.linspace(west*coef, east*coef+rad_360deg, nx+1)[:-1]
    else:
        lon = np.fromfile(f_lon_bound['path'], dtype=dict_dtype[f_lon_bound['dtype']])\
              .reshape(-1,nx+1)[f_lon_bound['rec']-1][:-1] * coef

    if f_lat_bound is None:
        lat = np.linspace(south, north, ny+1) * coef
    else:
        lat = np.fromfile(f_lat_bound['path'], dtype=dict_dtype[f_lat_bound['dtype']])\
              .reshape(-1,ny+1)[f_lat_bound['rec']-1] * coef

    lon[lon > rad_180deg] -= rad_360deg

    return lon, lat


def mkdir(path):
    dirname = os.path.dirname(path)
    if not os.path.isdir(dirname):
        print('mkdir -p {}'.format(dirname))
        os.makedirs(dirname)


def read_cfg(f_set):
    FLAG_NONE = 0
    FLAG_MESH_SEND = 1
    FLAG_MESH_RECV = 2
    FLAG_RMP = 3
    FLAG_OPT = 4
    FLAG_FIG = 5

    BLOCK_NAME_MESH_LATLON  = 'mesh_latlon'
    BLOCK_NAME_MESH_RASTER  = 'mesh_raster'
    BLOCK_NAME_MESH_POLYGON = 'mesh_polygon'
    BLOCK_NAME_REMAP        = 'remapping'
    BLOCK_NAME_OPT          = 'options'
    BLOCK_NAME_FIG          = 'figures'
    BLOCK_END               = 'end'

    flag = FLAG_NONE
    nBlocks_mesh = 0

    iLines_mesh_src = [0,0]
    iLines_mesh_tgt = [0,0]
    iLines_rmp = [0,0]
    iLines_opt = [0,0]
    iLines_fig = [0,0]

    mesh_source = ''
    mesh_target = ''

    lines = open(f_set,'r').readlines()
    nLines = len(lines)
    for iLine in range(nLines):
        line = lines[iLine].strip()
        if len(line) == 0: continue
        if line[0] == '#': continue

        if flag == FLAG_NONE:
            if line[0] == '[' and line[-1] == ']':
                block_name = line[1:-1].strip()
                if block_name in [BLOCK_NAME_MESH_LATLON,
                                  BLOCK_NAME_MESH_RASTER,
                                  BLOCK_NAME_MESH_POLYGON]:
                    if nBlocks_mesh == 0:
                        flag = FLAG_MESH_SEND
                        iLines_mesh_src[0] = iLine + 1
                        if block_name == BLOCK_NAME_MESH_LATLON:
                            mesh_source = MESHTYPE__LATLON
                        elif block_name == BLOCK_NAME_MESH_RASTER:
                            mesh_source = MESHTYPE__RASTER
                        elif block_name == BLOCK_NAME_MESH_POLYGON:
                            mesh_source = MESHTYPE__POLYGON
                    elif nBlocks_mesh == 1:
                        flag = FLAG_MESH_RECV
                        iLines_mesh_tgt[0] = iLine + 1
                        if block_name == BLOCK_NAME_MESH_LATLON:
                            mesh_target = MESHTYPE__LATLON
                        elif block_name == BLOCK_NAME_MESH_RASTER:
                            mesh_target = MESHTYPE__RASTER
                        elif block_name == BLOCK_NAME_MESH_POLYGON:
                            mesh_target = MESHTYPE__POLYGON
                    else:
                        raise Exception('More than 2 blocks of mesh was found')
                    nBlocks_mesh += 1
                elif block_name == BLOCK_NAME_OPT:
                    flag = FLAG_OPT
                    iLines_opt[0] = iLine + 1
                elif block_name == BLOCK_NAME_FIG:
                    flag = FLAG_FIG
                    iLines_fig[0] = iLine + 1
                elif block_name == BLOCK_NAME_REMAP:
                    flag = FLAG_RMP
                    iLines_rmp[0] = iLine + 1
                else:
                    raise Exception(f'Invalid block name: "{block_name}" @ line {iLine+1}')
            else:
                is_ok = False
                if 'path_report' in line:
                    if line.index('path_report') == 0:
                        is_ok = True
                if not is_ok:
                    raise Exception(f'Invalid format of line: "{block_name}" @ line {iLine+1}')
        else:
            if line[0] == '[' and line[-1] == ']':
                block_name = line[1:-1].strip()
                if block_name == BLOCK_END:
                    if flag == FLAG_MESH_SEND:
                        iLines_mesh_src[1] = iLine - 1
                    elif flag == FLAG_MESH_RECV:
                        iLines_mesh_tgt[1] = iLine - 1
                    elif flag == FLAG_RMP:
                        iLines_rmp[1] = iLine - 1
                    elif flag == FLAG_OPT:
                        iLines_opt[1] = iLine - 1
                    elif flag == FLAG_FIG:
                        iLines_fig[1] = iLine - 1
                    else:
                        raise Exception(f'Unknown flag: {flag}')
                    flag = FLAG_NONE
                else:
                    raise Exception('Invalid block name: "{block_name}" @ line {iLine+1}')
            else:
                continue

    print('Block mesh_source: {} ~ {}'.format(iLines_mesh_src[0]+1, iLines_mesh_src[1]+1))
    print('      mesh_target: {} ~ {}'.format(iLines_mesh_tgt[0]+1, iLines_mesh_tgt[1]+1))
    print('      remapping: {} ~ {}'.format(iLines_rmp[0]+1, iLines_rmp[1]+1))
    print('      opt      : {} ~ {}'.format(iLines_opt[0]+1, iLines_opt[1]+1))
    print('      fig      : {} ~ {}'.format(iLines_fig[0]+1, iLines_fig[1]+1))

    def is_key(line, key):
        res = False
        if ':' in line:
            res = line[:line.index(':')].strip() == key

        return res

    def is_key_child(line, key):
        res = False
        if '=' in line:
            res = line[:line.index('=')].strip() == key

        return res

    def remove_quotes(comp):
        comp = comp.strip()
        if comp[0] == comp[-1] == "'" or comp[0] == comp[-1] == '"':
            res = comp[1:-1]
        else:
            res = comp
        return res

    def read_value_int(line, iLine):
        return int(line[line.index(':')+1:])

    def read_value_float(line, iLine):
        return float(line[line.index(':')+1:].replace('d','e'))

    def read_value_bool(line, iLine):
        val = line[line.index(':')+1:].strip().lower()
        if val in ['.true.', 'true']:
            return True
        elif val in ['.false.', 'false']:
            return False
        else:
            raise Exception(f'Invalid value @ line {iLine+1}: {line}')

    def read_value_str(line, iLine):
        line = line.strip()
        return remove_quotes(line.strip()[line.strip().index(':')+1:])

    def read_value_path(line, iLine):
        val = line[line.index(':')+1:].strip()
        if not (val[0] == val[-1] == '"' or val[0] == val[-1] == "'"):
            raise Exception(f'Invalid format @ line {iLine+1}: {line}\n'
                            f'Path must be enclosed in single quotes or double quotes.')
        return val[1:-1]

    def read_value_file(line, iLine, f_default):
        res = f_default.copy()

        comps = line[line.index(':')+1:].strip().split(',')
        for iComp, comp in enumerate(comps):
            if is_key_child(comp,'path'):
                res['path'] = remove_quotes(comp[comp.index('=')+1:].strip())
            elif is_key_child(comp,'dtype'):
                res['dtype'] = remove_quotes(comp[comp.index('=')+1:].strip())
            elif is_key_child(comp,'rec'):
                res['rec'] = int(comp[comp.index('=')+1:].strip())
            elif is_key_child(comp,'endian'):
                res['endian'] = remove_quotes(comp[comp.index('=')+1:].strip())
            else:
                if iComp == 0:
                    res['path'] = remove_quotes(comp)
                elif iComp == 1:
                    res['dtype'] = remove_quotes(comp)
                elif iComp == 2:
                    res['rec'] = int(comp)
                elif iComp == 3:
                    res['endian'] = remove_quotes(comp)
                else:
                    raise Exception(f'Too many components in line {iLine+1}')
                    quit()

        return res

    def read_value_tuple_int(line, iLine):
        return tuple([int(i) for i in line[line.index(':')+1:].split(',')])


    def read_cfg_mesh_latlon(iLines, mesh):
        cfg = {
          'type': MESHTYPE__LATLON,
          'nx': None,
          'ny': None,
          'west': None,
          'east': None,
          'south': None,
          'north': None,
          'is_south_to_north': True,
          'f_lon_bound': None,
          'f_lat_bound': None,
          'coord_unit': UNIT_DEGREE,
        }
        directory = ''

        for iLine in range(iLines[0], iLines[1]+1):
            line = lines[iLine].strip()
            if len(line) == 0: continue
            if line[0] == '#': continue

            if is_key(line, 'name'):
              pass
            elif is_key(line, 'nx'):
                cfg['nx'] = read_value_int(line, iLine)
            elif is_key(line, 'ny'):
                cfg['ny'] = read_value_int(line, iLine)
            elif is_key(line, 'west'):
                cfg['west'] = read_value_float(line, iLine)
            elif is_key(line, 'east'):
                cfg['east'] = read_value_float(line, iLine)
            elif is_key(line, 'south'):
                cfg['south'] = read_value_float(line, iLine)
            elif is_key(line, 'north'):
                cfg['north'] = read_value_float(line, iLine)
            elif is_key(line,'is_south_to_north'):
                cfg['is_south_to_north'] = read_value_bool(line, iLine)
            elif is_key(line,'dir'):
                directory = read_value_path(line, iLine)
            elif is_key(line,'f_lon_bound'):
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['f_lon_bound'] = read_value_file(line, iLine, f_default)
                cfg['f_lon_bound']['path'] = os.path.join(directory, cfg['f_lon_bound']['path'])
            elif is_key(line,'f_lat_bound'):
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['f_lat_bound'] = read_value_file(line, iLine, f_default)
                cfg['f_lat_bound']['path'] = os.path.join(directory, cfg['f_lat_bound']['path'])
            elif is_key(line,'fin_grdidx'):
                continue
            elif is_key(line,'fin_grdara'):
                continue
            elif is_key(line,'fin_grdwgt'):
                continue
            elif is_key(line,'fin_grdx'):
                continue
            elif is_key(line,'fin_grdy'):
                continue
            elif is_key(line,'fin_grdz'):
                continue
            elif is_key(line,'fin_grdlon'):
                continue
            elif is_key(line,'fin_grdlat'):
                continue
            elif is_key(line,'in_grid_sz'):
                continue
            elif is_key(line,'in_grid_lb'):
                continue
            elif is_key(line,'in_grid_ub'):
                continue
            elif is_key(line,'idx_miss'):
                continue
            elif is_key(line,'ara_miss'):
                continue
            elif is_key(line,'wgt_miss'):
                continue
            elif is_key(line,'xyz_miss'):
                continue
            elif is_key(line,'lonlat_miss'):
                continue
            else:
                raise Exception(f'Invalid syntax @ line {iLine+1}: {lien}')

        print(f'[mesh_{mesh}]')
        for key in cfg.keys():
            print(f'  {key}: {cfg[key]}')

        return cfg

    def read_cfg_mesh_raster(iLines, mesh):
        cfg = {
          'type': MESHTYPE__RASTER, 
          'nx': None, 
          'ny': None,
          'xi': None, 
          'xf': None, 
          'yi': None, 
          'yf': None, 
          'west': None,
          'east': None,
          'south': None,
          'north': None,
          'is_south_to_north': True,
          'fin_rstidx': None,
          'fin_grdidx': None,
          'idx_miss': -9999,
          'ncx': None,
          'ncy': None,
        }

        directory = ''

        for iLine in range(iLines[0], iLines[1]+1):
            line = lines[iLine].strip()
            if len(line) == 0: continue
            if line[0] == '#': continue

            if is_key(line, 'name'):
              pass
            elif is_key(line,'nx'):
                cfg['nx'] = read_value_int(line, iLine)
            elif is_key(line,'ny'):
                cfg['ny'] = read_value_int(line, iLine)
            elif is_key(line,'xi'):
                cfg['xi'] = read_value_int(line, iLine)
            elif is_key(line,'xf'):
                cfg['xf'] = read_value_int(line, iLine)
            elif is_key(line,'yf'):
                cfg['yi'] = read_value_int(line, iLine)
            elif is_key(line,'yi'):
                cfg['yf'] = read_value_int(line, iLine)
            elif is_key(line,'west'):
                cfg['west'] = read_value_float(line, iLine)
            elif is_key(line,'east'):
                cfg['east'] = read_value_float(line, iLine)
            elif is_key(line,'south'):
                cfg['south'] = read_value_float(line, iLine)
            elif is_key(line,'north'):
                cfg['north'] = read_value_float(line, iLine)
            elif is_key(line,'is_south_to_north'):
                cfg['is_south_to_north'] = read_value_bool(line, iLine)
            elif is_key(line,'dir'):
                directory = read_value_path(line, iLine)
            elif is_key(line,'fin_rstidx'):
                f_default = {'path':'', 'dtype':DTYPE_INT4, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['fin_rstidx'] = read_value_file(line, iLine, f_default)
                cfg['fin_rstidx']['path'] = os.path.join(directory,cfg['fin_rstidx']['path'])
            elif is_key(line,'fin_rstara'):
                continue
            elif is_key(line,'fin_rstwgt'):
                continue
            elif is_key(line,'in_raster_sz'):
                continue
            elif is_key(line,'in_raster_lb'):
                continue
            elif is_key(line,'in_raster_ub'):
                continue
            elif is_key(line,'fin_grdidx'):
                f_default = {'path':'', 'dtype':DTYPE_INT4, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['fin_grdidx'] = read_value_file(line, iLine, f_default)
                cfg['fin_grdidx']['path'] = os.path.join(directory,cfg['fin_grdidx']['path'])
            elif is_key(line,'fin_grdara'):
                continue
            elif is_key(line,'fin_grdwgt'):
                continue
            elif is_key(line,'fin_grdx'):
                continue
            elif is_key(line,'fin_grdy'):
                continue
            elif is_key(line,'fin_grdz'):
                continue
            elif is_key(line,'fin_grdlon'):
                continue
            elif is_key(line,'fin_grdlat'):
                continue
            elif is_key(line,'in_grid_sz'):
                cfg['ncx'], cfg['ncy'] = read_value_tuple_int(line, iLine)
            elif is_key(line,'in_grid_lb'):
                continue
            elif is_key(line,'in_grid_ub'):
                continue
            elif is_key(line,'idx_miss'):
                cfg['idx_miss'] = read_value_int(line, iLine)
            elif is_key(line,'ara_miss'):
                continue
            elif is_key(line,'wgt_miss'):
                continue
            elif is_key(line,'xyz_miss'):
                continue
            elif is_key(line,'lonlat_miss'):
                continue
            else:
                raise Exception(f'Invalid syntax @ line {iLine+1}: {lien}')

        print(f'[mesh_{mesh}]'.format(mesh))
        for key in cfg.keys():
            print(f'  {key}: {cfg[key]}')

        return cfg


    def read_cfg_mesh_polygon(iLines, mesh):
        cfg = {
          'type': MESHTYPE__POLYGON,
          'np': None,
          'nij': None,
          'f_lon_vertex': None,
          'f_lat_vertex': None,
          'f_x_vertex': None,
          'f_y_vertex': None,
          'f_z_vertex': None,
          'coord_unit': UNIT_DEGREE,
          'coord_miss_s': COORD_MISS_S_DEFAULT,
          'coord_miss_c': COORD_MISS_C_DEFAULT,
          'f_arctyp': None,
          'arc_parallel': False,
        }
        directory = ''
        coord_miss = None

        for iLine in range(iLines[0], iLines[1]+1):
            line = lines[iLine].strip()
            if len(line) == 0: continue
            if line[0] == '#': continue

            if is_key(line, 'name'):
              pass
            elif is_key(line, 'np'):
                cfg['np'] = read_value_int(line, iLine)
            elif is_key(line, 'nij'):
                cfg['nij'] = read_value_int(line, iLine)
            elif is_key(line, 'dir'):
                directory = read_value_path(line, iLine)
            elif is_key(line, 'f_lon_vertex'):
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['f_lon_vertex'] = read_value_file(line, iLine, f_default)
                cfg['f_lon_vertex']['path'] = os.path.join(directory, cfg['f_lon_vertex']['path'])
            elif is_key(line, 'f_lat_vertex'):
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['f_lat_vertex'] = read_value_file(line, iLine, f_default)
                cfg['f_lat_vertex']['path'] = os.path.join(directory, cfg['f_lat_vertex']['path'])
            elif is_key(line, 'f_x_vertex'):
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['f_x_vertex'] = read_value_file(line, iLine, f_default)
                cfg['f_x_vertex']['path'] = os.path.join(directory, cfg['f_x_vertex']['path'])
            elif is_key(line, 'f_y_vertex'):
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['f_y_vertex'] = read_value_file(line, iLine, f_default)
                cfg['f_y_vertex']['path'] = os.path.join(directory, cfg['f_y_vertex']['path'])
            elif is_key(line, 'f_z_vertex'):
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                cfg['f_z_vertex'] = read_value_file(line, iLine, f_default)
                cfg['f_z_vertex']['path'] = os.path.join(directory, cfg['f_z_vertex']['path'])
            elif is_key(line, 'coord_unit'):
                cfg['coord_unit'] = read_value_str(line, iLine)
            elif is_key(line, 'coord_miss'):
                coord_miss = read_value_float(line, iLine)
            elif is_key(line, 'f_arctyp'):
                f_default = {'path':'', 'dtype':DTYPE_INT4, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                f_arctyp = read_value_file(line, iLine, f_default)
                f_arctyp['path'] = os.path.join(directory, f_arctyp['path'])
                cfg['f_arctyp'] = f_arctyp
            elif is_key(line, 'arc_parallel'):
                cfg['arc_parallel'] = read_value_bool(line, iLine)
            elif is_key(line, 'fin_grdidx'):
                continue
            elif is_key(line, 'fin_grdara'):
                continue
            elif is_key(line, 'fin_grdwgt'):
                continue
            elif is_key(line, 'fin_grdx'):
                continue
            elif is_key(line, 'fin_grdy'):
                continue
            elif is_key(line, 'fin_grdz'):
                continue
            elif is_key(line, 'fin_grdlon'):
                continue
            elif is_key(line, 'fin_grdlat'):
                continue
            elif is_key(line, 'in_grid_sz'):
                continue
            elif is_key(line, 'in_grid_lb'):
                continue
            elif is_key(line, 'in_grid_ub'):
                continue
            elif is_key(line, 'idx_miss'):
                continue
            elif is_key(line, 'ara_miss'):
                continue
            elif is_key(line, 'wgt_miss'):
                continue
            elif is_key(line, 'xyz_miss'):
                continue
            elif is_key(line, 'lonlat_miss'):
                continue
            else:
                raise Exception(f'Invalid syntax @ line {iLine+1}: {lien}')

        if coord_miss is not None:
            if cfg['f_lon_vertex'] is not None:
                cfg['coord_miss_s'] = coord_miss
            elif cfg['f_x_vertex'] is not None:
                cfg['coord_miss_c'] = coord_miss
            else:
                raise Exception('Neither f_lon_vertex or f_x_vertex was specified.')

        print(f'[mesh_{mesh}]')
        for key in cfg.keys():
            print(f'  {key}: {cfg[key]}')

        return cfg

    def put_value(key, val, cfg_default, lst_cfg):
        if len(lst_cfg) == 0:
            cfg_default[key] = val
        else:
            lst_cfg[-1][key] = val

    def read_cfg_fig(iLines):
        cfg_default = {
          'figsize': (10,6),
          'dpi': 300,
          'mode': None, 
          'linewidth': 0.0,
          'linewidth_fill': 0.5,
          'edgecolor': 'gray',
          'cmap': 'jet',
          'color_miss': 'none',
          'vmin': None,
          'vmax': None,
          'val_miss': -1e20,
          'title': '',
          'show': True,
        }

        cfg_each_init = {
          'mesh': None,
          'path': None,
          'f_val': None,
          'figsize': None,
          'dpi': None,
          'mode': None,
          'linewidth': None,
          'linewidth_fill': None,
          'edgecolor': None,
          'cmap': None,
          'color_miss': None,
          'vmin': None,
          'vmax': None,
          'val_miss': None,
          'title': None,
          'show': None,
        }

        directory = ''
        lst_cfg = []
        is_default = True

        for iLine in range(iLines[0], iLines[1]+1):
            line = lines[iLine].strip()
            if len(line) == 0: continue
            if line[0] == '#': continue

            # Each figure starts with "mesh"
            if is_key(line, 'mesh'):
                mesh = read_value_str(line, iLine)
                if mesh not in [MESH__SOURCE, MESH__TARGET]:
                    raise Exception(f'Invalid value in $mesh: {mesh} @ line {iLine+1}')
                lst_cfg.append(cfg_each_init.copy())
                lst_cfg[-1]['mesh'] = mesh

            # Unsucceeded variables
            elif is_key(line, 'path_fig'):
                is_default = False
                path = read_value_path(line, iLine)
                if path != "":
                    path = os.path.join(directory, path)
                lst_cfg[-1]['path'] = path

            elif is_key(line, 'f_grdval'): 
                f_default = {'path':'', 'dtype':DTYPE_DBLE, 'rec':1, 'endian':ENDIAN_LITTLE_SHORT}
                f_val = read_value_file(line, iLine, f_default)
                f_val['path'] = os.path.join(directory, f_val['path'])
                lst_cfg[-1]['f_val'] = f_val
                if not os.path.isfile(f_val['path']):
                    raise Exception(f'File not found: {f_val["path"]} @ line {iLine+1}')

            # Succeeded variables
            elif is_key(line, 'figsize'):
                val = read_value_tuple_int(line, iLine)
                put_value('figsize', val, cfg_default, lst_cfg)

            elif is_key(line, 'dpi'):
                val = read_value_float(line, iLine)
                put_value('dpi', val, cfg_default, lst_cfg)

            elif is_key(line, 'linewidth'):
                val = read_value_float(line, iLine)
                put_value('linewidth', val, cfg_default, lst_cfg)

            elif is_key(line, 'linewidth_fill'):
                val = read_value_float(line, iLine)
                put_value('linewidth_fill', val, cfg_default, lst_cfg)

            elif is_key(line, 'edgecolor'):
                val = read_value_str(line, iLine)
                put_value('edgecolor', val, cfg_default, lst_cfg)

            elif is_key(line, 'cmap'):
                val = read_value_str(line, iLine)
                if val not in plt.colormaps() and val != 'none':
                    raise Exception(f'Invalid name of colormap: {val} @ line {iLine+1}\n'\
                                    f'Valid values: {plt.colormaps()}')
                put_value('cmap', val, cfg_default, lst_cfg)

            elif is_key(line, 'color_miss'):
                val = read_value_str(line, iLine)
                put_value('color_miss', val, cfg_default, lst_cfg)

            elif is_key(line, 'vmin'):
                val = read_value_float(line, iLine)
                put_value('vmin', val, cfg_default, lst_cfg)

            elif is_key(line, 'vmax'):
                val = read_value_float(line, iLine)
                put_value('vmax', val, cfg_default, lst_cfg)

            elif is_key(line, 'val_miss'):
                val = read_value_float(line, iLine)
                put_value('val_miss', val, cfg_default, lst_cfg)

            elif is_key(line, 'title'):
                val = read_value_str(line, iLine)
                put_value('title', val, cfg_default, lst_cfg)

            elif is_key(line, 'dir'):
                directory = read_value_path(line, iLine)

            elif is_key(line, 'show'):
                val = read_value_bool(line, iLine)
                put_value('show', val, cfg_default, lst_cfg)

            else:
                raise Exception(f'Invalid syntax: {line} @ line {iLine+1}')

        print('[figures]')
        print('  (Default Settings)')
        for key in cfg_default.keys():
            print(f'    {key}: {cfg_default[key]}')

        for i, cfg_each in enumerate(lst_cfg):
            print(f'  (Figure {i+1} / {len(lst_cfg)})')

            if cfg_each['mesh'] is None:
                raise Exception('"mesh" was not specified.')

            for key in cfg_each.keys():
                if cfg_each[key] is None:
                    if key in cfg_default.keys():
                        cfg_each[key] = copy.copy(cfg_default[key])
                else:
                    print(f'    {key}: {cfg_each[key]}')

        return lst_cfg

    cfg = {
      'mesh_source': None,
      'mesh_target': None,
      'rmp'        : None,
      'opt'        : None,
      'fig'        : None,
    }

    if mesh_source == MESHTYPE__LATLON:
        cfg['mesh_source'] = read_cfg_mesh_latlon(iLines_mesh_src, MESH__SOURCE)
    elif mesh_source == MESHTYPE__RASTER:
        cfg['mesh_source'] = read_cfg_mesh_raster(iLines_mesh_src, MESH__SOURCE)
    elif mesh_source == MESHTYPE__POLYGON:
        cfg['mesh_source'] = read_cfg_mesh_polygon(iLines_mesh_src, MESH__SOURCE)
    elif mesh_souce == '':
        cfg['mesh_source'] = {'type': MESH__NONE}
    else:
        raise Exception(f'Invalid value in $mesh_source: {mesh_source}')

    if mesh_target == MESHTYPE__LATLON:
        cfg['mesh_target'] = read_cfg_mesh_latlon(iLines_mesh_tgt, MESH__TARGET)
    elif mesh_target == MESHTYPE__RASTER:
        cfg['mesh_target'] = read_cfg_mesh_raster(iLines_mesh_tgt, MESH__TARGET)
    elif mesh_target == MESHTYPE__POLYGON:
        cfg['mesh_target'] = read_cfg_mesh_polygon(iLines_mesh_tgt, MESH__TARGET)
    elif mesh_target == '':
        cfg['mesh_target'] = {'type': MESH__NONE}
    else:
        raise Exception(f'Invalid value in $mesh_target: {mesh_target}')

    cfg['fig'] = read_cfg_fig(iLines_fig)

    return cfg


def set_vrange(cfg, val):
    if cfg['vmin'] is None:
        if cfg['val_miss'] is None:
            cfg['vmin'] = val.min()
        else:
            cfg['vmin'] = val[val != cfg['val_miss']].min()

    if cfg['vmax'] is None:
        if cfg['val_miss'] is None:
            cfg['vmax'] = val.max()
        else:
            cfg['vmax'] = val[val != cfg['val_miss']].max()

    return


def run():
    f_cfg = sys.argv[1]
    cfg = read_cfg(f_cfg)

    cfg_mesh_src = cfg['mesh_source']
    cfg_mesh_tgt = cfg['mesh_target']
    cfg_fig      = cfg['fig']

    print('Plotting')

    for i, c1_fig in enumerate(cfg_fig):
        print('  (Figure {} / {})'.format(i+1, len(cfg_fig)))

        # LatLon
        if (c1_fig['mesh'] == MESH__SOURCE and cfg_mesh_src['type'] == MESHTYPE__LATLON) or \
           (c1_fig['mesh'] == MESH__TARGET and cfg_mesh_tgt['type'] == MESHTYPE__LATLON):
            if c1_fig['mesh'] == MESH__SOURCE:
                cfg_mesh = cfg_mesh_src
            else:
                cfg_mesh = cfg_mesh_tgt

            lon, lat = read_coord_latlon(
              cfg_mesh['nx'], cfg_mesh['ny'], 
              cfg_mesh['west'], cfg_mesh['east'], cfg_mesh['south'], cfg_mesh['north'],
              cfg_mesh['f_lon_bound'], cfg_mesh['f_lat_bound'],
              cfg_mesh['coord_unit'])

            f_val = c1_fig['f_val']
            val = np.fromfile(f_val['path'],dtype=dict_dtype[f_val['dtype']])\
                              .reshape(-1,cfg_mesh['ny'],cfg_mesh['nx'])[f_val['rec']-1]
            if f_val['endian'] in [ENDIAN_BIG_LONG,ENDIAN_BIG_SHORT]: val = val.byteswap()

            print('    val min: {}, max: {}'.format(
                  val[val != c1_fig['val_miss']].min(),
                  val[val != c1_fig['val_miss']].max()))

            set_vrange(c1_fig, val)
            print('    cmap: {cmap}, vmin: {vmin}, vmax: {vmax}'.format(**c1_fig))

            print('    Drawing {}'.format(c1_fig['path']))
            draw_latlon(lon, lat, val, cfg_mesh['is_south_to_north'], c1_fig)

        # Raster
        if (c1_fig['mesh'] == MESH__SOURCE and cfg_mesh_src['type'] == MESHTYPE__RASTER) or \
           (c1_fig['mesh'] == MESH__TARGET and cfg_mesh_tgt['type'] == MESHTYPE__RASTER):
            if c1_fig['mesh'] == MESH__SOURCE:
                cfg_mesh = cfg_mesh_src
            else:
                cfg_mesh = cfg_mesh_tgt

            f = cfg_mesh['fin_grdidx']
            grdidx = np.fromfile(f['path'], dtype=dict_dtype[f['dtype']])\
                                 .reshape(-1,cfg_mesh['ncy'],cfg_mesh['ncx'])[f['rec']-1].reshape(-1)
            if f['endian'] in [ENDIAN_BIG_LONG,ENDIAN_BIG_SHORT]: grdidx = grdidx.byteswap()

            if cfg_mesh['idx_miss'] is None:
                idxmin = grdidx.min()
                idxmax = grdidx.max()
            else:
                idxmin = grdidx[grdidx != cfg_mesh['idx_miss']].min()
                idxmax = grdidx[grdidx != cfg_mesh['idx_miss']].max()

            idx_to_loc = np.ones(idxmax+1).astype(np.int64) * -1
            if cfg_mesh['idx_miss'] is None:
                idx_to_loc[grdidx] = range(grdidx.size)
            else:
                for loc in range(grdidx.size):
                    if grdidx[loc] == cfg_mesh['idx_miss']: continue
                    idx_to_loc[grdidx[loc]] = loc

            f = c1_fig['f_val']
            grdval = np.fromfile(f['path'], dtype=dict_dtype[f['dtype']])\
                                 .reshape(-1,cfg_mesh['ncy'],cfg_mesh['ncx'])[f['rec']-1].reshape(-1)
            if f['endian'] in [ENDIAN_BIG_LONG,ENDIAN_BIG_SHORT]: grdval = grdval.byteswap()

            f = cfg_mesh['fin_rstidx']
            rstidx = np.fromfile(f['path'], dtype=dict_dtype[f['dtype']])\
                                 .reshape(-1,cfg_mesh['ny'],cfg_mesh['nx'])[f['rec']-1]
            if f['endian'] in [ENDIAN_BIG_LONG,ENDIAN_BIG_SHORT]: rstidx = rstidx.byteswap()
            rstidx = rstidx.astype(np.int64)

            if cfg_mesh['idx_miss'] is None:
                rstval = grdval[idx_to_loc[rstidx]]
            else:
                rstval = np.ones((cfg_mesh['ny'],cfg_mesh['nx'])) * c1_fig['val_miss']
                rstval[rstidx != cfg_mesh['idx_miss']] = grdval[idx_to_loc[rstidx[rstidx != cfg_mesh['idx_miss']]]]

            print('    val min: {}, max: {}'.format(
                  rstval[rstval != c1_fig['val_miss']].min(),
                  rstval[rstval != c1_fig['val_miss']].max()))

            set_vrange(c1_fig, rstval)
            print('    cmap: {cmap}, vmin: {vmin}, vmax: {vmax}'.format(**c1_fig))

            print('    Drawing {}'.format(c1_fig['path']))
            draw_raster(cfg_mesh['west'], cfg_mesh['east'], cfg_mesh['south'], cfg_mesh['north'], 
                        cfg_mesh['is_south_to_north'],
                        rstval, c1_fig)

        # Polygon
        if (c1_fig['mesh'] == MESH__SOURCE and cfg_mesh_src['type'] == MESHTYPE__POLYGON) or \
           (c1_fig['mesh'] == MESH__TARGET and cfg_mesh_tgt['type'] == MESHTYPE__POLYGON):
            if c1_fig['mesh'] == MESH__SOURCE:
                cfg_mesh = cfg_mesh_src
            else:
                cfg_mesh = cfg_mesh_tgt

            lon, lat = read_coord_polygon(
              cfg_mesh['f_lon_vertex'], cfg_mesh['f_lat_vertex'],
              cfg_mesh['f_x_vertex'], cfg_mesh['f_y_vertex'], cfg_mesh['f_z_vertex'], 
              cfg_mesh['np'], cfg_mesh['nij'], 
              cfg_mesh['coord_unit'], cfg_mesh['coord_miss_s'], cfg_mesh['coord_miss_c'])

            f_val = c1_fig['f_val']
            val = np.fromfile(f_val['path'],dtype=dict_dtype[f_val['dtype']])\
                              .reshape(-1,cfg_mesh['nij'])[f_val['rec']-1]
            if f_val['endian'] in [ENDIAN_BIG_LONG,ENDIAN_BIG_SHORT]: val = val.byteswap()

            print('    val min: {}, max: {}'.format(
                  val[val != c1_fig['val_miss']].min(),
                  val[val != c1_fig['val_miss']].max()))

            set_vrange(c1_fig, val)
            print('    cmap: {cmap}, vmin: {vmin}, vmax: {vmax}'.format(**c1_fig))

            print('    Drawing {}'.format(c1_fig['path']))
            draw_polygon(lon, lat, cfg_mesh['coord_miss_s'], val, c1_fig)


run()
