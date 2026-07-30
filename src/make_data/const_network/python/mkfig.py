import os
import sys
import copy
import numpy as np
import shapefile
import matplotlib.pyplot as plt
import cartopy.crs as ccrs

import NLNI as nlni
import StrRank as strnk
import JflwDir as jflw
import common


def read_wsCodeMask(wsCode:int):
    gxs = 0
    for line in open(f'{common.DIR_DAT}/NLNI/dat/wsCodeMask/range.txt', 'r').readlines()[2:]:
        dat = line.strip().split()
        if int(dat[0]) != wsCode:
            continue
        gxs, gxe, gys, gye = [int(v) for v in dat[1:5]]
        west, east, south, north = [float(v) for v in dat[5:9]]
        break
    if gxs == 0:
        print(f'wsCodeMask was not found: {wsCode}')
        return

    f = f'{common.DIR_DAT}/NLNI/dat/wsCodeMask/{wsCode:06d}.bin'
    mask = np.fromfile(f, dtype=np.int8).reshape(gye-gys+1,gxe-gxs+1)

    return dict(
      mask = mask,
      bbox = (west,east,south,north),
    )

 
def get_bnds_bsnmap(a):
    ny, nx = a.shape
    dx = 1.0 / nx
    dy = 1.0 / ny

    # horizontal lines
    iyy, ixx = np.where(a[:-1,:] != a[1:,:])
    hx = np.array([dx*(ixx), dx*(ixx+1), np.full(ixx.size, np.nan)]).T.reshape(-1)
    hy = np.array([dy*(iyy+1), dy*(iyy+1), np.full(ixx.size, np.nan)]).T.reshape(-1)

    # vertical lines
    iyy, ixx = np.where(a[:,:-1] != a[:,1:])
    vx = np.array([dx*(ixx+1), dx*(ixx+1), np.full(ixx.size, np.nan)]).T.reshape(-1)
    vy = np.array([dy*(iyy), dy*(iyy+1), np.full(ixx.size, np.nan)]).T.reshape(-1)

    return np.r_[hx,vx], np.r_[hy,vy]


def drawChannelsInWscodeMask():
    wsCode = int(sys.argv[2])

    mask = read_wsCodeMask(wsCode)
    #if mask is None:
    #    print(f'wsCodeMask was not found: {wsCode}')
    #    return
    if mask is not None:
        wswest, wseast, wssouth, wsnorth = mask['bbox']
        print('bbox', mask['bbox'])

        txs = nlni.txs_of_lon(wswest)
        txe = nlni.txe_of_lon(wseast)
        tys = nlni.tys_of_lat(wssouth)
        tye = nlni.tye_of_lat(wsnorth)

        dct_iEnt = {}
        for region in strnk.LST_REGION:
            dct_iEnt[region] = []

        for ity in range(tys, tye+1):
            for itx in range(txs, txe+1):
                f = f'{common.DIR_DAT}/StrRank/dat/lst_tiled/{ity:02d}{itx:02d}.txt'
                if not os.path.isfile(f):
                    continue
                fp = open(f, 'r')
                nRegion = int(fp.readline().strip().split()[1])
                for iRegion in range(nRegion):
                    dat = fp.readline().strip().split()
                    region, nEnt = dat[0], int(dat[1])
                    for iiEnt in range(nEnt):
                        iEnt = int(fp.readline().strip()) - 1
                        if iEnt not in dct_iEnt[region]:
                            dct_iEnt[region].append(iEnt)
                fp.close()

        dshp = {}
        for region in strnk.LST_REGION:
            print(region)
            sf = shapefile.Reader(f'{common.DIR_DAT}/StrRank/dl/{region}/StrRank-{region}_Stream.shp', 
              encoding='cp932')
            for iEnt in dct_iEnt[region]:
                shp = sf.shape(iEnt)
                rec = sf.record(iEnt)

                swest, ssouth, seast, snorth = shp.bbox
                if swest >= wseast or seast <= wswest or ssouth >= wsnorth or snorth <= wssouth:
                    continue

                wsCode_shp = int(rec[0])
                if wsCode_shp not in dshp.keys():
                    dshp[wsCode_shp] = []
                dshp[wsCode_shp].append(shp)
            sf.close()

    else:
        dshp = {wsCode: []}
        for region in strnk.LST_REGION:
            print(region)
            sf = shapefile.Reader(f'{common.DIR_DAT}/StrRank/dl/{region}/StrRank-{region}_Stream.shp', 
              encoding='cp932')

            dshp[wsCode] \
              += [sf.shape(i) for i, rec in enumerate(sf.records()) if int(rec[0]) == wsCode]
            sf.close()

    plots = {}
    for key, color in zip(('this', 'other'), ('r', 'pink')):
        plots[key] = dict(
          lon = [np.nan],
          lat = [np.nan],
          plon = [np.nan],
          plat = [np.nan],
          color = color,
        )
    for wsCode_shp in dshp.keys():
        if wsCode_shp == wsCode:
            key = 'this'
        else:
            key = 'other'
        for shp in dshp[wsCode_shp]:
            plots[key]['lon'] += [p[0] for p in shp.points] + [np.nan]
            plots[key]['lat'] += [p[1] for p in shp.points] + [np.nan]
            plots[key]['plon'] += [shp.points[i][0] for i in (0,-1)]
            plots[key]['plat'] += [shp.points[i][1] for i in (0,-1)]

    print('Plotting')
    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    if mask is not None:
        ax.imshow(mask['mask'], cmap=plt.cm.Blues, interpolation='nearest', origin='lower',
          extent=mask['bbox'], zorder=1)

    for key in plots.keys():
        pl = plots[key]
        #if len(pl['lon']) == 0: continue
        ax.plot(pl['lon'], pl['lat'],
          linewidth=1, color=pl['color'],
          zorder=2)

        if key == 'other':
            #if len(pl['plon']) == 0: continue
            ax.scatter(pl['plon'], pl['plat'],
              s=10, color=pl['color'], zorder=2)

    lonmin = np.nanmin([np.nanmin(plots[key]['lon']) for key in plots.keys()])
    lonmax = np.nanmax([np.nanmax(plots[key]['lon']) for key in plots.keys()])
    latmin = np.nanmin([np.nanmin(plots[key]['lat']) for key in plots.keys()])
    latmax = np.nanmax([np.nanmax(plots[key]['lat']) for key in plots.keys()])
    lonbuf = (lonmax - lonmin)*0.05
    latbuf = (latmax - latmin)*0.05
    ax.set_extent([lonmin-lonbuf, lonmax+lonbuf, latmin-latbuf, latmax+latbuf])

    ax.coastlines(linewidth=0.5)
    plt.show()


def get_network_shape(gid):
    dct_idx = {}
    shapes, records = [], []

    f = f'{common.DIR_DAT}/StrRank/dat/network/{gid}.txt'
    #print(f'Reading {f}')
    fp = open(f, 'r')
    mRegion = int(fp.readline().strip().split()[1])
    ikEnt = -1
    for iiRegion in range(mRegion):
        dat = fp.readline().strip().split()
        regionName, mEnt = dat[0], int(dat[1])
        dct_idx[regionName] = []
        fp.readline()

        f_shp = f'{common.DIR_DAT}/StrRank/dl/{regionName}/StrRank-{regionName}_Stream.shp'
        #print(f'Reading {f_shp}')
        sf = shapefile.Reader(f_shp, encoding='cp932')
        for iiEnt in range(mEnt):
          ikEnt += 1
          irEnt = int(fp.readline().strip()) - 1
          shapes.append(sf.shape(irEnt))
          records.append(sf.record(irEnt))
          dct_idx[regionName].append((irEnt, ikEnt))
        sf.close()
    fp.close()

    return shapes, records, dct_idx


def read_group_isct_basin(gid, FRAC_THRESH):
    jflw.set_resolution('1sec')

    f = f'{common.DIR_DAT}/StrRank/dat/isct_basin/{gid}.txt'
    print(f'Reading {f}')
    fp = open(f, 'r')
    fp.readline()  # BBox of shapes

    fp.readline()  # gxy w/o margins
    fp.readline()
    mTile = int(fp.readline().strip().split()[1])
    fp.readline()
    for iiTile in range(mTile):
        fp.readline()

    gxsall, gxeall, gysall, gyeall = [int(v) for v in fp.readline().strip().split()[1:]]

    bbox = [float(v) for v in fp.readline().strip().split()[1:]]

    bsnmap = np.zeros((gyeall-gysall+1,gxeall-gxsall+1), dtype=np.int32)
    print(f'bsnmap {bsnmap.shape}')
    mTile = int(fp.readline().strip().split()[1])
    fp.readline()
    for iiTile in range(mTile):
        dat = fp.readline().strip().split()
        tilename = dat[0]
        tx, ty, xs, xe, ys, ye, gxs, gxe, gys, gye = [int(v) for v in dat[1:]]
        #f = f'{common.DIR_DAT}/J-FlwDir/dat/tiled/bsn/{tilename}.bin'
        #print(f'Reading {f}')
        #bsnmap[gys-gysall:gye+1-gysall,gxs-gxsall:gxe+1-gxsall]\
        #  = np.fromfile(f, dtype=np.int32).reshape(NY,NX)[ys-1:ye,xs-1:xe]
        bsnmap[gys-gysall:gye+1-gysall,gxs-gxsall:gxe+1-gxsall]\
                = jflw.read_map_from_tile('bsn', np.int32, gxs, gxe, gys, gye, 0)

    mBasin = int(fp.readline().strip().split()[1])
    fp.readline()
    lst_bsn = []
    imax = -1
    for iiBasin in range(mBasin):
        dat = fp.readline().strip().split()
        bsnId, frac = int(dat[0]), float(dat[1])
        lst_bsn.append((bsnId, frac))
        if frac >= FRAC_THRESH:
            imax += 1
    fp.close()

    bsnordmap = np.full(bsnmap.shape, -9999)
    bsnordmap[bsnmap == 0] = imax+1
    for i, (bsnId, frac) in enumerate(lst_bsn):
        if frac < FRAC_THRESH:
            break
        print(f'bsnmap {bsnId} -> {i}')
        bsnordmap[bsnmap == bsnId] = i

    x, y = get_bnds_bsnmap(bsnmap)
    x = x * (bbox[1]-bbox[0]) + bbox[0]
    y = bbox[3] - y * (bbox[3]-bbox[2])

    return bbox, bsnordmap, lst_bsn, x, y



# arg2: gid
# arg3: list of wsCode (e.g., [830303,830304,100000])
def drawNetwork():
    gid = sys.argv[2]
    wsCode_bold = sys.argv[3][1:-1].split(',')

    shps, recs, _ = get_network_shape(gid)

    dct_wsCode = {}
    for i, rec in enumerate(recs):
        if rec[0] not in dct_wsCode.keys():
            dct_wsCode[rec[0]] = []
        dct_wsCode[rec[0]].append(i)
    print(f'wsCode: {dct_wsCode.keys()}')

    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    for j, wsCode in enumerate(dct_wsCode.keys()):
        cj = j / (len(dct_wsCode)-1)
        lons, lats = [], []
        for i in dct_wsCode[wsCode]:
            lons += [np.nan] + [p[0] for p in shps[i].points]
            lats += [np.nan] + [p[1] for p in shps[i].points]
        if wsCode in wsCode_bold:
            lw = 2.0
            zorder = 1
        else:
            lw = 0.5
            zorder = 2
        ax.plot(lons, lats, 
                linewidth=lw, color=plt.cm.rainbow(cj), label=wsCode, zorder=zorder)
    ax.coastlines(linewidth=0.5)
    ax.legend()
    plt.show()


def drawNetworks():
    west, east, south, north = [float(v) for v in sys.argv[2:6]]

    iColor = 0
    nColor = 20

    cmap = plt.cm.rainbow
    fig = plt.figure()
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    for line in open(f'{common.DIR_DAT}/StrRank/dat/network/all.txt', 'r').readlines()[2:]:
        dat = line.strip().split()
        gid = strnk.gid_i2s(int(dat[1]))
        nwest, neast, nsouth, nnorth = [float(v) for v in dat[4:8]]
        if nwest > east or neast < west or nsouth > north or nnorth < south:
            continue
        shapes, *_ = get_network_shape(gid)
        lons, lats = [], []
        for shp in shapes:
            lons += [np.nan] + [p[0] for p in shp.points]
            lats += [np.nan] + [p[1] for p in shp.points]
        ax.plot(lons, lats, linewidth=0.5, color=cmap(np.cos(np.pi*iColor/nColor)))

        iColor += 1
    ax.coastlines(linewidth=0.5)
    ax.set_extent([west,east,south,north])
    plt.show()


def drawNetworkAndBasins():
    FRAC_THRESH = 0.02

    gid = strnk.gid_i2s(int(sys.argv[2]))

    bbox, bsnordmap, lst_bsn, bndx, bndy = read_group_isct_basin(gid, FRAC_THRESH)

    kBsn = 0
    for bsn in lst_bsn:
        if bsn[1] < FRAC_THRESH:
            break
        kBsn += 1
    print(f'Basins (fraction>{FRAC_THRESH}) {kBsn}')

    shapes, *_ = get_network_shape(gid)

    cmap = copy.deepcopy(plt.cm.jet)
    cmap.set_over('dimgray')

    print('Plotting')

    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    ax.imshow(np.ma.masked_not_equal(bsnordmap,-9999),
      cmap=plt.cm.binary, interpolation='nearest', extent=bbox,
      vmin=-9999-1, vmax=-9999+3, 
      zorder=1)
    im = ax.imshow(np.ma.masked_equal(bsnordmap,-9999), 
      cmap=cmap, interpolation='nearest', extent=bbox,
      vmin=0, vmax=kBsn-1, 
      zorder=2)
    fig.colorbar(im, aspect=50, pad=0.08, orientation='vertical', extend='neither')

    ax.plot(bndx, bndy, linewidth=1, color='k', zorder=3)

    lons, lats = [], []
    for shp in shapes:
        lons += [np.nan] + [p[0] for p in shp.points]
        lats += [np.nan] + [p[1] for p in shp.points]
    ax.plot(lons, lats, color='w', linewidth=1, zorder=4)
    #ax.scatter(lons, lats, color='r', marker='x', linewidth=1, zorder=4)

    #ax.set_extent((bwest-lonmargin, beast+lonmargin, bsouth-latmargin, bnorth+latmargin))
    ax.set_extent(bbox)
    ax.coastlines(linewidth=0.5)
    plt.show()


def drawNetworkBasinConsistency():
    import json

    BSNID_MAX = 10000
    FRAC_THRESH = 0.02

    gid = strnk.gid_i2s(int(sys.argv[2]))

    plot_bsnid = False
    plot_elv = False

    shapes, records, _ = read_group_channels(gid)

    jflw.set_resolution('1sec')

    gxs, gxe, gys, gye = jflw.NGX, 1, jflw.NGY, 1
    west, east, south, north = jflw.REGION_EAST, jflw.REGION_WEST, jflw.REGION_NORTH, jflw.REGION_SOUTH
    ds = json.load(open(f'{common.DIR_DAT}/StrRank/dat/eval_basin/{gid}.json', 'r'))
    for bsn in ds["basins"]:
        #if bsn["frac_of_network"] < FRAC_THRESH:
        #    continue
        bsnId = bsn["index"]
        (gxs_this, gxe_this, gys_this, gye_this), (west_this, east_this, south_this, north_this) \
          = jflw.read_basin_range(bsnId)
        #print(bsnId, gxs_this, gxe_this, gys_this, gye_this)
        gxs = min(gxs, gxs_this)
        gxe = max(gxe, gxe_this)
        gys = min(gys, gys_this)
        gye = max(gye, gye_this)
        west = min(west, west_this)
        east = max(east, east_this)
        south = min(south, south_this)
        north = max(north, north_this)
    #print(gxs, gxe, gys, gye)
    print(west, east, south, north)
    bsnmap = jflw.read_map_from_tile('bsn', np.int32, gxs, gxe, gys, gye, jflw.BSNID_MISS)

    elvmap = jflw.read_map_from_tile('elv', np.float32, gxs, gxe, gys, gye, jflw.ELV_MISS)
    elvmap = np.ma.masked_equal(elvmap, jflw.ELV_MISS)
    print(f'bsnmap {bsnmap.shape}')
    print(f'elv min: {elvmap.min():10.3e}, max: {elvmap.max():10.3e}')

    bsnmap[bsnmap > BSNID_MAX] = jflw.BSNID_MISS-1

    bndx, bndy = get_bnds_bsnmap(bsnmap)
    bndx = bndx * (east-west) + west
    bndy = north - bndy * (north-south)

    if plot_bsnid:
        nBasin_signif = len([bsn for bsn in ds["basins"] if bsn["frac_of_network"] >= FRAC_THRESH])
        print(f'Basins (fraction >= 0.02) {nBasin_signif}')
        bsnordmap = np.full(bsnmap.shape, nBasin_signif+1)

        for iiBsn, bsn in enumerate(ds["basins"]):
            if bsn["frac_of_network"] < FRAC_THRESH:
                continue
            bsnId = bsn["index"]
            print(f'{bsn["frac_of_network"]:5.3f} {bsnId} -> {iiBsn}')
            bsnordmap[bsnmap == bsnId] = iiBsn
        bsnordmap[bsnmap == jflw.BSNID_MISS] = -9999
        bsnordmap[bsnmap == jflw.BSNID_MISS-1] = nBasin_signif

    # Plot
    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())

    if plot_bsnid:
        cmap = plt.cm.rainbow
        cmap.set_over('dimgray')
        im = ax.imshow(np.ma.masked_equal(bsnordmap,-9999), 
            cmap=cmap, interpolation='nearest',
            extent=[west,east,south,north],
            vmin=0, vmax=nBasin_signif)
        fig.colorbar(im, aspect=50, pad=0.08, orientation='horizontal')

    if plot_elv:
        cmap = plt.cm.gist_earth
        im = ax.imshow(np.ma.masked_equal(elvmap,jflw.ELV_MISS),
            cmap=cmap, interpolation='nearest',
            extent=[west,east,south,north])
        fig.colorbar(im, aspect=50, pad=0.08, orientation='horizontal')

    ax.plot(bndx, bndy, linewidth=1, color='k')

    lons, lats = [], []
    for shp in shapes:
        lons += [np.nan] + [p[0] for p in shp.points]
        lats += [np.nan] + [p[1] for p in shp.points]
    ax.plot(lons, lats, color='gray', linewidth=1, zorder=4)

    # gid = 14931
    #dct_wsId = {
    #  '890906': 'tomato', # 筑後川
    #  '890905': 'yellowgreen', # 嘉瀬川
    #  '890907': 'dodgerblue', # 矢部川
    #}
    # gid = 

    for wsId in dct_wsId.keys():
        lons, lats = [], []
        for i, rec in enumerate(records):
            if rec[0] != wsId: continue
            lons += [np.nan] + [p[0] for p in shapes[i].points]
            lats += [np.nan] + [p[1] for p in shapes[i].points]
        ax.plot(lons, lats, color=dct_wsId[wsId], linewidth=1, zorder=5)


    #ax.set_extent([west,east,south,north])
    ax.set_extent([west-1,east+1,south-1,north+1])
    ax.coastlines(linewidth=0.5)

    plt.show()


def drawNetworkBasinConsistencySummary():

    LENG_THRESH = 1.e4

    def get_rank(score):
        if score > 0.95:
            return 1
        elif score > 0.85:
            return 2
        elif score > 0.65:
            return 3
        elif score > 0.35:
            return 4
        else:
            return 5

    d = {
      1: {
        'color': 'royalblue',
      },
      2: {
        'color': 'darkturquoise',
      },
      3: {
        'color': 'seagreen',
      },
      4: {
        'color': 'gold',
      },
      5: {
        'color': 'tomato',
      },
    }

    fig = plt.figure(figsize=(8,8))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())

    fp = open(f'{common.DIR_DAT}/StrRank/dat/eval_basin/summary.txt', 'r')
    for line in fp.readlines()[1:]:
        dat = line.strip().split()
        gid, leng, status, score = dat[0], float(dat[1]), int(dat[2]), float(dat[3])
        if leng < LENG_THRESH:
            break

        rank = get_rank(score)

        shapes, _ = read_group_channels(gid)
        lons, lats = [], []
        for shp in shapes:
            lons += [np.nan] + [p[0] for p in shp.points]
            lats += [np.nan] + [p[1] for p in shp.points]
        ax.plot(lons, lats, linewidth=0.5, color=d[rank]['color'])
    ax.coastlines(linewidth=0.5)
    fp.close()

    plt.show()


job = sys.argv[1]

if job == 'drawChannelsInWscodeMask':
    drawChannelsInWscodeMask()

elif job == 'drawNetwork':
    drawNetwork()

elif job == 'drawNetworks':
    drawNetworks()

elif job == 'drawNetworkAndBasins':
    drawNetworkAndBasins()

elif job == 'drawNetworkBasinConsistency':
    drawNetworkBasinConsistency()

elif job == 'drawNetworkBasinConsistencySummary':
    drawNetworkBasinConsistencySummary()
