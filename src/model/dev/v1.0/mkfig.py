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
    fp = open(f_conf, 'r')
    dir_out = fp.readline().strip().split()[1]
    fp.readline() #
    time_bgn = stime2datetime(fp.readline().strip().split()[1])
    time_end = stime2datetime(fp.readline().strip().split()[1])
    dt_out = int(fp.readline().strip().split()[1])
    nt = (time_end - time_bgn) / dt_out
    fp.readline()  # dt
    fp.readline()  # dt_riv
    fp.readline()  # dt_slo
    fp.readline()  # eps
    fp.readline()  # ddt_min_riv
    fp.readline()  # ddt_min_slo
    fp.readline()  #
    west = float(fp.readline().strip().split()[1])
    east = float(fp.readline().strip().split()[1])
    south = float(fp.readline().strip().split()[1])
    north = float(fp.readline().strip().split()[1])
    nx = int(fp.readline().strip().split()[1])
    ny = int(fp.readline().strip().split()[1])
    fp.close()

    return dir_out, nt, dt_out, (west, east, south, north), nx, ny


def read_river():
    fp = open('river_network.txt', 'r')
    nCh = int(fp.readline().strip().split()[1])
    chs = []
    for iCh in range(nCh):
        fp.readline()
        fp.readline()  # node_outlet
        nPt = int(fp.readline().strip().split()[1])
        lon = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        lat = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        width = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        depth = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        levee = np.array([float(v) for v in fp.readline().strip().split()[1:]])
        chs.append(dict(lon=lon, lat=lat, width=width, depth=depth, levee=levee))
    fp.close()

    return chs


def plot_hr_map(
        ax, hr, chs, 
        vmin=None, vmax=None, zorder=1):
    if vmin is None:
        vmin = hr.min()
    if vmax is None:
        vmax = hr.max()
    cm = plt.cm.Blues
    for v, ch in zip(hr, chs):
        vnorm = max(min( (v-vmin) / (vmax-vmin), 1.0),0.0)
        ax.plot(ch['lon'], ch['lat'], linewidth=3, color='w', zorder=zorder)
        ax.plot(ch['lon'], ch['lat'], linewidth=2, color=cm(vnorm), zorder=zorder+1)

    return ax


def draw_map():
    f_conf = sys.argv[2]
    it = int(sys.argv[3])

    dir_out, nt, dt, (west, east, south, north), nx, ny = read_conf(f_conf)
    chs = read_river()

    hs = np.fromfile(f'{dir_out}/hs.bin').reshape(-1,ny,nx)
    hr = np.fromfile(f'{dir_out}/hr.bin').reshape(-1,len(chs))
    print(f'output nt: {hs.shape[0]}')

    hrmin = 0
    hrmax = hr[it].max() * 0.85

    elv = np.fromfile('elv.bin',dtype=np.float32).reshape(ny,nx)
    mask = elv < -1e2

    hr_plt = hr[it]
    hs_plt = np.ma.masked_where(mask, hs[it])
    print(f'hr[{it}] min: {hr_plt.min()} max: {hr_plt.max()}')
    print(f'hs[{it}] min: {hs_plt.min()} max: {hs_plt.max()}')

    #for param in ('width', 'depth'):
    #    fig = plt.figure(figsize=(8,8))
    #    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    #    vmin_ = 0
    #    vmax_ = np.max([ch[param] for ch in chs])
    #    cm = plt.cm.rainbow
    #    for ch in chs:
    #        vnorm = max(min( (ch[param].mean()-vmin_) / (vmax_-vmin_), 1.0),0.0)
    #        ax.plot(ch['lon'], ch['lat'], linewidth=2, color='w', zorder=1)
    #        ax.plot(ch['lon'], ch['lat'], linewidth=1.5, color=cm(vnorm), zorder=2)
    #    #im = ax.imshow(np.zeros((1,1)), cmap=cm, vmin=vmin_, vmax=vmax_)
    #    #fig.colorbar(im, aspect=40, pad=0.03, orientation='vertical')
    #    ax.set_title(param)

    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    im = ax.imshow(hs_plt,
            cmap=plt.cm.rainbow, interpolation='nearest',
            extent=[west,east,south,north], zorder=1)
    fig.colorbar(im, aspect=50, pad=0.03, orientation='vertical')
    ax = plot_hr_map(
           ax, hr[it], chs, 
           vmin=hrmin, vmax=hrmax, zorder=2)
    for i, ch in enumerate(chs):
        ax.text(ch['lon'].mean(), ch['lat'].mean(), str(i))
    ax.set_extent([west,east,south,north])
    plt.show()



def draw_waterbudget():
    f_conf = sys.argv[2]

    dir_out, nt, dt, *_ = read_conf(f_conf)

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

else:
    raise Exception(f'Invalid task: {task}')
