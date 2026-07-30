import os
import sys
import subprocess
import copy
import datetime
import random
import numpy as np
#import statistics
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import cartopy.crs as ccrs
from cartopy.mpl.ticker import LatitudeFormatter,LongitudeFormatter
#import PIL
#import bz2
#import torch
#import netCDF4
import json
import shapefile
#===============================================================
#
#===============================================================
DIR_PROJECT = '/data10/atakeshima/Kujukuri/dat'
DIR_JFLWDIR = DIR_PROJECT + '/J-FlwDir'
DIR_STRRANK = DIR_PROJECT + '/StrRank'

LST_REGION = [
  'Hokkaido',
  'Honshu',
  'Shikoku',
  'Kyushu',
  'Okinawa',
]

ENC = 'cp932'

FIELD_INDEX_STREAM = dict(
  WSysCode  = 0,
  RiverCode = 1,
  RiverName = 2,
  SectType  = 3,
  StrRank   = 4,
  StrLength = 5,
  StrDz     = 6,
  StrSlope  = 7,
  MaxDist   = 8,
  NdUpStr   = 9,
  NdDownStr = 10,
)

FIELD_INDEX_RIVERNODE = dict(
  NodeID     = 0,
  WSysCode   = 1,
  Elevation  = 2,
  ElevSource = 3,
  Distance   = 4,
  MaxStrRank = 5,
  EndPoint   = 6,
)

RANK_MAX = 8
COLOR_STRRANK = {
  '1': 'silver',
  '2': 'slateblue',
  '3': 'dodgerblue',
  '4': 'mediumseagreen',
  '5': 'yellowgreen',
  '6': 'orange',
  '7': 'tomato',
  '8': 'firebrick',
}
#===============================================================
#
#===============================================================
def divideIntoWsys():
    region = sys.argv[2]

    rank_max = 0

    tables_field_index = dict(
      Stream    = FIELD_INDEX_STREAM   ,
      RiverNode = FIELD_INDEX_RIVERNODE,
    )

    for var in ('Stream', 'RiverNode'):
        field_index = tables_field_index[var]
        idx_WSysCode = field_index['WSysCode']

        f = f'{DIR_STRRANK}/dl/{region}/StrRank-{region}_{var}.shp'
        print('Reading ' + f)
        sf = shapefile.Reader(f, encoding=ENC)
        print(f'n = {sf.numShapes}')

        dset = {}
        for rec in sf.records():
            wsCode = rec[idx_WSysCode]
            dset[wsCode] = dict(
                shp=[], 
                rec=[],
            )
        print('dset prepared.')

        fields = sf.fields

        for shp, rec in zip(sf.shapes(), sf.records()):
            wsCode = rec[idx_WSysCode]
            dset[wsCode]['shp'].append(shp)
            dset[wsCode]['rec'].append(rec)

            if var == 'Stream':
                rank_max = max(rank_max, int(rec[FIELD_INDEX_STREAM['StrRank']]))

        sf.close()

        for wsCode in dset.keys():
            #if wsCode != '880807': continue
            f = f'{DIR_STRRANK}/dat/StrRank/{wsCode}/{var}.shp'
            os.makedirs(os.path.dirname(f), exist_ok=True)
            print('  Writing ' + f)
            wf = shapefile.Writer(f, encoding=ENC)
            for field in fields[1:]:
                wf.field(*field)
            for shp, rec in zip(dset[wsCode]['shp'], dset[wsCode]['rec']):
                wf.shape(shp)
                wf.record(*rec)
            #break

    print(f'Max rank: {rank_max}')
#===============================================================
# [Note]
# StrDz や StrSlope には誤りがあり、分流の判定に用いることはできない
#===============================================================
def draw_StrRank():
    wsCode = sys.argv[2]

    f = f'{DIR_STRRANK}/dat/StrRank/{wsCode}/Stream.shp'
    print('Reading ' + f)
    with shapefile.Reader(f, encoding=ENC) as sf:
        shps = sf.shapes()
        recs = sf.records()

    fig = plt.figure(figsize=(12,12))
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    ax.coastlines(linewidth=0.5)

    rank_max = 0
    for rec in recs:
        rank_max = max(rank_max, int(rec[FIELD_INDEX_STREAM['StrRank']]))
    print(f'Max rank: {rank_max}')

    nodes = {}
    west, east, south, north = 360, 0, 90, -90
    for shp, rec in zip(shps, recs):
        #TABLE_COLOR_STRRANK[rec[FIELD_INDEX_STREAM['StrRank']]]
        lons = [p[0] for p in shp.points]
        lats = [p[1] for p in shp.points]
        rank = int(rec[FIELD_INDEX_STREAM['StrRank']])
        ax.plot(lons, lats, linewidth=1, 
                #color=plt.cm.Blues(float(rec[FIELD_INDEX_STREAM['StrRank']])/rank_max))
                color=COLOR_STRRANK[str(rank)], zorder=rank)

        west = min(west, np.min(lons))
        east = max(east, np.max(lons))
        south = min(south, np.min(lats))
        north = max(north, np.max(lats))

        NdUpStr   = rec[FIELD_INDEX_STREAM['NdUpStr']]
        NdDownStr = rec[FIELD_INDEX_STREAM['NdDownStr']]

    with open(f'{DIR_JFLWDIR}/dat/all/1sec/Jaccard.txt', 'r') as rf:
        lst_id_jflw = []
        rf.readline()
        rf.readline()
        rf.readline()
        while True:
            dat_jflw = rf.readline().strip().split()
            if len(dat_jflw) == 0:
                break
            n = int(dat_jflw[4])
            for i in range(n):
                dat_nlni = rf.readline().strip().split()
                if dat_nlni[0] == wsCode:
                    id_jflw = dat_jflw[0].zfill(7)
                    Jaccard = float(dat_jflw[3])
                    if Jaccard < 0.01: continue
                    print(f'Overlapping with J-FlwDir basin {id_jflw} ({Jaccard:5.3f}).')
                    lst_id_jflw.append(id_jflw)

    if len(lst_id_jflw) == 1:
        id_jflw = lst_id_jflw[0]
        with open(f'{DIR_JFLWDIR}/dat/basin/1sec/range/{id_jflw}.txt', 'r') as rf:
            nx = int(rf.readline().strip().split()[1])
            ny = int(rf.readline().strip().split()[1])
            rf.readline()
            rf.readline()
            west = float(rf.readline().strip().split()[1])
            east = float(rf.readline().strip().split()[1])
            south = float(rf.readline().strip().split()[1])
            north = float(rf.readline().strip().split()[1])
        elv = np.ma.masked_equal( 
            np.fromfile(f'{DIR_JFLWDIR}/dat/basin/1sec/elv/{id_jflw}.bin', dtype=np.float32),
            -9999
        ).reshape(ny,nx)

        im = ax.imshow(
            np.ma.masked_equal(elv,-9999), 
            cmap=plt.cm.gist_earth, interpolation='nearest',
            extent=[west, east, south, north]
        )

    elif len(lst_id_jflw) > 1:
        elv_min, elv_max = 1e20, -1e20
        for id_jflw in lst_id_jflw:
            elv = np.ma.masked_equal( 
                np.fromfile(f'{DIR_JFLWDIR}/dat/basin/1sec/elv/{id_jflw}.bin', dtype=np.float32),
                -9999
            ).reshape(ny,nx)
            elv_min = min(elv_min, elv.min())
            elv_max = max(elv_max, elv.max())

        west, east, south, north = 360, 0, 90, -90
        for id_jflw in lst_id_jflw:
            with open(f'{DIR_JFLWDIR}/dat/basin/1sec/range/{id_jflw}.txt', 'r') as rf:
                nx = int(rf.readline().strip().split()[1])
                ny = int(rf.readline().strip().split()[1])
                rf.readline()
                rf.readline()
                bwest = float(rf.readline().strip().split()[1])
                beast = float(rf.readline().strip().split()[1])
                bsouth = float(rf.readline().strip().split()[1])
                bnorth = float(rf.readline().strip().split()[1])
            west  = min(west, bwest)
            east  = max(east, beast)
            south = min(south, bsouth)
            north = max(north, bnorth)
            elv = np.ma.masked_equal( 
                np.fromfile(f'{DIR_JFLWDIR}/dat/basin/1sec/elv/{id_jflw}.bin', dtype=np.float32),
                -9999
            ).reshape(ny,nx)

            im = ax.imshow(
                np.ma.masked_equal(elv,-9999), vmin=elv_min, vmax=elv_max,
                cmap=plt.cm.gist_earth, interpolation='nearest',
                extent=[bwest, beast, bsouth, bnorth]
            )

    if len(lst_id_jflw) > 0:
        fig.colorbar(im, aspect=50, pad=0.05, orientation='vertical')

    lonmargin = (east-west)*0.05
    latmargin = (north-south)*0.05
    west , east  = west-lonmargin , east+lonmargin
    south, north = south-latmargin, north+latmargin

    ax.set_extent([west, east, south, north])

    plt.show()


if __name__ == '__main__':
    action = sys.argv[1]

    if action == 'divideIntoWsys':
        divideIntoWsys()

    elif action == 'draw_StrRank':
        draw_StrRank()

    else:
        raise Exception(f'Invalid action: {action}')
