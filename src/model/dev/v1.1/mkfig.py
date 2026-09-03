import os
import sys
import datetime
import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs

def stime2datetime(s):
    return datetime.datetime(
      int(s[:4]), int(s[5:7]), int(s[8:10]), 
      int(s[11:13]), int(s[14:16]), int(s[17:19])
    )


def read_conf(f_conf):

    def start_from(line, keyword):
        if keyword in line:
            if line.index(keyword) == 0:
                return True
        return False


    def get_nml_block(lines, nml_name):
        for i, line in enumerate(lines):
            if line == f'&{nml_name}\n':
                i0 = i
                i1 = lines[i:].index('/\n') + i
                return lines[i0+1:i1]
        return None


    def get_value(block, name):
        for line in block:
            if start_from(line, name):
                return line[line.index('=')+1:].strip().replace(',', '')
        return None


    lines = open(f_conf, 'r').readlines()

    block = get_nml_block(lines, 'simulation')
    datetime_start = stime2datetime(get_value(block, 'datetime_start'))
    datetime_end = stime2datetime(get_value(block, 'datetime_end'))

    block = get_nml_block(lines, 'output')
    dir_out = get_value(block, 'dir')[1:-1]
    dt_out = float(get_value(block, 'interval'))

    block = get_nml_block(lines, 'domain')
    west = float(get_value(block, 'west'))
    east = float(get_value(block, 'east'))
    south = float(get_value(block, 'south'))
    north = float(get_value(block, 'north'))
    nx = int(get_value(block, 'nx'))
    ny = int(get_value(block, 'ny'))

    block = get_nml_block(lines, 'topography')
    f_elv = get_value(block, 'file_elv')[1:-1]

    block = get_nml_block(lines, 'river_network')
    f_rivnwk = get_value(block, 'file_river_network')[1:-1]

    block = get_nml_block(lines, 'river_crosssection')
    f_width = get_value(block, 'file_width')[1:-1]
    f_depth = get_value(block, 'file_depth')[1:-1]
    f_levee = get_value(block, 'file_levee')[1:-1]

    nt = (datetime_end - datetime_start) / dt_out

    return dir_out, nt, dt_out, \
           (west, east, south, north), (nx, ny), \
           (f_elv,), \
           (f_rivnwk, f_width, f_depth, f_levee)


def read_river(f):
    fp = open(f, 'r')
    nCh = int(fp.readline().strip().split()[1])
    lst_ch = []
    for iCh in range(nCh):
        fp.readline()  # index
        fp.readline()  # index_original
        fp.readline()  # node_outlet
        downleng = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        nPt = int(fp.readline().strip().split()[1])
        lon = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        lat = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        lst_ch.append(dict(lon=lon, lat=lat, downleng=downleng))
    fp.close()

    return lst_ch


def plot_hr_map(
        ax, hr, lst_ch, 
        vmin=None, vmax=None, zorder=1):
    if vmin is None:
        vmin = hr.min()
    if vmax is None:
        vmax = hr.max()
    cm = plt.cm.Blues
    for v, ch in zip(hr, lst_ch):
        vnorm = max(min( (v-vmin) / (vmax-vmin), 1.0),0.0)
        ax.plot(ch['lon'], ch['lat'], linewidth=2, color='w', zorder=zorder)
        ax.plot(ch['lon'], ch['lat'], linewidth=1, color=cm(vnorm), zorder=zorder+1)
    #ax.scatter([ch['lon'][0] for ch in lst_ch], [ch['lat'][0] for ch in lst_ch],
    #           s=30, color='k', marker='x', zorder=zorder+2)

    return ax


def draw_map():
    f_conf = sys.argv[2]
    it = int(sys.argv[3])

    dir_out, nt, dt, \
    (west, east, south, north), (nx, ny), \
    (f_elv, ), \
    (f_rivnwk, f_width, f_depth, f_levee) = read_conf(f_conf)

    lst_ch = read_river(f_rivnwk)
    width = np.fromfile(f_width)
    depth = np.fromfile(f_depth)
    levee = np.fromfile(f_levee)
    for ch, w, d, l in zip(lst_ch, width, depth, levee):
        ch['w'] = w
        ch['d'] = d
        ch['l'] = l

    hr = np.fromfile(f'{dir_out}/hr.bin').reshape(-1,len(lst_ch))
    hs = np.fromfile(f'{dir_out}/hs.bin').reshape(-1,ny,nx)
    sfc = np.fromfile(f'{dir_out}/sfc.bin').reshape(-1,ny,nx)
    print(f'output nt: {hs.shape[0]}')

    hrmin = 0
    hrmax = hr[it].max() * 0.85

    elv = np.fromfile(f_elv,dtype=np.float32).reshape(ny,nx)
    mask = elv < -1e2

    hr_plt = hr[it]
    hs_plt = np.ma.masked_where(mask, hs[it])
    sfc_plt = np.ma.masked_where(mask, sfc[it])
    print(f'hr[{it}] min: {hr_plt.min()} max: {hr_plt.max()}')
    print(f'hs[{it}] min: {hs_plt.min()} max: {hs_plt.max()}')
    print(f'sfc[{it}] min: {sfc_plt.min()} max: {sfc_plt.max()}')

    # River cross sections
    #for param in ('width', 'depth'):
    #    fig = plt.figure(figsize=(8,8))
    #    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    #    vmin_ = 0
    #    vmax_ = np.max([ch[param] for ch in lst_ch])
    #    cm = plt.cm.rainbow
    #    for ch in lst_ch:
    #        vnorm = max(min( (ch[param].mean()-vmin_) / (vmax_-vmin_), 1.0),0.0)
    #        ax.plot(ch['lon'], ch['lat'], linewidth=2, color='w', zorder=1)
    #        ax.plot(ch['lon'], ch['lat'], linewidth=1.5, color=cm(vnorm), zorder=2)
    #    #im = ax.imshow(np.zeros((1,1)), cmap=cm, vmin=vmin_, vmax=vmax_)
    #    #fig.colorbar(im, aspect=40, pad=0.03, orientation='vertical')
    #    ax.set_title(param)

    # Water levels
    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    im = ax.imshow(hs_plt,
            cmap=plt.cm.Blues, interpolation='nearest',
            extent=[west,east,south,north], zorder=1)
    fig.colorbar(im, aspect=50, pad=0.03, orientation='vertical')
    ax = plot_hr_map(
           ax, hr[it], lst_ch, 
           vmin=hrmin, vmax=hrmax, zorder=2)
    #for i, ch in enumerate(lst_ch):
    #    ax.text(ch['lon'].mean(), ch['lat'].mean(), str(i))
    ax.set_extent([west,east,south,north])
    ax.set_title('water level [m]')

    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    im = ax.imshow(sfc_plt,
            cmap=plt.cm.Blues, interpolation='nearest',
            extent=[west,east,south,north], zorder=1)
    fig.colorbar(im, aspect=50, pad=0.03, orientation='vertical')
    ax = plot_hr_map(
           ax, hr[it], lst_ch, 
           vmin=hrmin, vmax=hrmax, zorder=2)
    #for i, ch in enumerate(lst_ch):
    #    ax.text(ch['lon'].mean(), ch['lat'].mean(), str(i))
    ax.set_extent([west,east,south,north])
    ax.set_title('surface water level [m]')


    plt.show()


def draw_wlv_src2out():
    f_conf = sys.argv[2]
    iCh = int(sys.argv[3])
    it = int(sys.argv[4])

    dir_out, nt, dt, \
    (west, east, south, north), (nx, ny), \
    (f_elv, ), \
    (f_rivnwk, f_width, f_depth, f_levee) = read_conf(f_conf)

    lst_ch = read_river(f_rivnwk)

    fp = open(f'{dir_out}/src2out_attr/{iCh:06d}.txt', 'r')
    lst_iCh = np.array([int(v) for v in fp.readline().strip().split()[1:]])
    lst_zb = np.array([float(v) for v in fp.readline().strip().split()[1:]])
    fp.close()

    mCh = len(lst_iCh)

    lst_dist = np.array([lst_ch[iCh_-1]['downleng'].mean() for iCh_ in lst_iCh])

    lst_h = np.fromfile(f'{dir_out}/src2out_hr/{iCh:06d}.bin').reshape(-1,mCh)

    # Plot
    fig, ax = plt.subplots(figsize=(10,6))
    ax2 = ax.twinx()

    ax.plot(lst_dist, lst_zb, lw=1, color='gray')
    ax.plot(lst_dist, lst_zb+lst_h[it,:], lw=1, color='tomato')
    ax2.plot(lst_dist, lst_h[it,:], lw=1, ls='dashed', color='dodgerblue')

    ax.set_ylabel('Elevation (m)')
    ax2.set_ylabel('Water Level (m)')
    ax.set_xlim(lst_dist.max(), 0.)
    ax2.set_xlim(lst_dist.max(), 0.)
    ax.set_title(f't = {it}')

    plt.show()


def draw_waterbudget():
    f_conf = sys.argv[2]

    dir_out, nt, dt, \
    (west, east, south, north), (nx, ny), \
    (f_elv, ), \
    (f_rivnwk, f_width, f_depth, f_levee) = read_conf(f_conf)

    prcp, aevp, pevp, sout, sall, sr, ss, sg, si, inbalance \
    = [], [], [], [], [], [], [], [], [], []
    d = np.array([[float(v) for v in line.strip().split()] for line in open(f'{dir_out}/storage.dat', 'r').readlines()])
    prcp = d[1:,0] - d[:-1,0]
    aevp = d[1:,1] - d[:-1,1]
    pevp = d[1:,2] - d[:-1,2]
    sout = d[1:,3] - d[:-1,3]
    sall = d[1:,4] - d[:-1,4]
    sr   = d[1:,5] - d[:-1,5]
    ss   = d[1:,6] - d[:-1,6]
    sg   = d[1:,7] - d[:-1,7]
    si   = d[1:,8] - d[:-1,8]
    inbalance = d[1:,9]

    t0 = 1
    t1 = prcp.shape[0] - 1

    lw = 1.0
    fs_ticks = 14
    plt.rcParams['font.size'] = 16

    fig = plt.figure(figsize=(12,10))
    ax = fig.add_subplot()
    ax.plot(prcp[t0:], linewidth=lw, color='dodgerblue', label='prcp')
    ax.plot(sout[t0:], linewidth=lw, color='purple', label='disharge')
    ax.plot(sr[t0:], linewidth=lw, color='tomato', label='river')
    ax.plot(ss[t0:], linewidth=lw, color='orange', label='land')
    ax.plot(inbalance[t0:], linewidth=lw, color='k', label='inbalance')
    ax.set_xticks(range(t0,t1,int((t1-t0)/20)))
    ax.tick_params(labelsize=fs_ticks)
    ax.set_xlabel(f'Time (dt={dt} sec)')
    ax.set_ylabel(r'Storage ($\mathrm{m}^3$)')
    ax.legend(loc='upper left')
    plt.show()


task = sys.argv[1]

if task == 'draw_map':
    draw_map()

elif task == 'draw_waterbudget':
    draw_waterbudget()

elif task == 'draw_wlv_src2out':
    draw_wlv_src2out()

else:
    raise Exception(f'Invalid task: {task}')
