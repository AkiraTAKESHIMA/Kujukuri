import os
import sys
import copy
import json
import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs

DIR_DAT = '../../dat'


def make_mp4(ts, fps):
    import cv2

    img = cv2.imread(f'tmp/fig/points_t{ts[0]:06d}.png')
    shp = img.shape[:2]
    codec = cv2.VideoWriter_fourcc(*'mp4v')
    video = cv2.VideoWriter(f'tmp/fig/points.mp4', codec, fps, (img.shape[1], img.shape[0]))

    for t in ts:
        img = cv2.imread(f'tmp/fig/points_t{t:06d}.png')
        if img.shape[:2] != shp:
            raise Exception(f'Different image shape: {img.shape[:2]}')
        video.write(img)
    video.release()


def draw_chdir():
    gid = '00014931'

    nwk = json.load(open(f'{DIR_DAT}/StrRank/dat/network_channel/json/{gid}.json', 'r'))

    chdir = np.array([int(line.strip()) for line in open('tmp/chdir1.txt', 'r').readlines()])

    points = []
    lines = open('tmp/point.txt', 'r').readlines()
    for l1, l2 in zip(lines[::2],lines[1::2]):
        points.append(dict(
                lon=[float(v) for v in l1.strip().split()[1:]],
                lat=[float(v) for v in l2.strip().split()[1:]]))

    fig = plt.figure(figsize=(12,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())

    # Channels and directions
    lons = {1: [], 2: [], 9: [], -9: []}
    lats = copy.deepcopy(lons)
    for ch, dir in zip(nwk['channel'], chdir):
        lons[dir] += [np.nan] + ch['lon']
        lats[dir] += [np.nan] + ch['lat']
    for key, color in zip(lons.keys(), ('dodgerblue', 'tomato', 'violet', 'grey')):
        if len(lons[key]) == 0:
            continue
        ax.plot(lons[key], lats[key], linewidth=1, color=color, zorder=2)
    
    # Points from source to outlet
    tint = 1
    #"""
    for t in range(0, len(points), tint):
        print(t)
        art = []
        art.append(ax.scatter(points[t]['lon'], points[t]['lat'],
                           s=2, marker='o', color='dimgray', zorder=4))

        #plt.show()
        fig.savefig(f'tmp/fig/points_t{t:06d}.png', 
            bbox_inches='tight', pad_inches=0.1, dpi=300)

        for a in art:
            a.remove()
    #"""

    make_mp4(range(0, len(points), tint), 2.*tint)


def draw_newnwk():
    gid = sys.argv[1]

    # Read original network data
    nwk = json.load(open(f'{DIR_DAT}/StrRank/dat/network_channel/json/{gid}.json', 'r'))

    # Read new network data
    nwk_new = []
    conn = []
    
    fp = open(f'{DIR_DAT}/StrRank/dat/network_wsconn/{gid}.txt', 'r')
    nNwk = int(fp.readline().strip().split()[1])
    for i in range(nNwk):
        m = int(fp.readline().strip().split()[1])
        lst_wsCode = []
        for j in range(m):
            wsCode = fp.readline().strip()
            lst_wsCode.append(wsCode)
    
        m = int(fp.readline().strip().split()[1])
        lst_jCh = []
        for j in range(m):
            jCh = int(fp.readline().strip()) - 1
            lst_jCh.append(jCh)
    
        nwk_new.append(dict(
            wsCode = lst_wsCode,
            jCh = lst_jCh,
        ))
    
    n = int(fp.readline().strip().split()[1])
    for i in range(n):
        wsCode1, wsCode2 = fp.readline().strip().split()[1:]
        dat = fp.readline().strip().split()
        m = int(dat[1])
        is_removed = dat[3] == 'T'
        ndlon, ndlat, ndelv = [], [], []
        fp.readline()
        for j in range(m):
            lon, lat, elv = [float(v) for v in fp.readline().strip().split()]
            ndlon.append(lon)
            ndlat.append(lat)
            ndelv.append(elv)
        conn.append(dict(
          wsCode = (wsCode1, wsCode2),
          is_removed = is_removed,
          ndlon = ndlon,
          ndlat = ndlat,
          ndelv = ndelv
        ))
    fp.close()

    # Plot
    fig = plt.figure(figsize=(12,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    
    for iNwk, snwk in enumerate(nwk_new):
        color = plt.cm.tab20(float(iNwk) / (nNwk-1))
        lons, lats = [], []
        for jCh in snwk['jCh']:
            lons += [np.nan] + nwk['channel'][jCh]['lon']
            lats += [np.nan] + nwk['channel'][jCh]['lat']
        ax.plot(lons, lats, linewidth=1, color=color, zorder=1)
   
    vmax = np.max([np.max(c['ndelv']) for c in conn]) * 0.95
    for c in conn:
        if c['is_removed']:
            ls = dict(marker='v')
        else:
            ls = dict(marker='o')
        #ax.scatter(c['ndlon'], c['ndlat'], c=c['ndelv'], cmap=plt.cm.winter,
        #    vmin=0, vmax=vmax,
        #    s=20, edgecolor='w', zorder=4, **ls)
   
    ax.coastlines(linewidth=0.5)
    plt.show()



#draw_chdir()
draw_newnwk()
