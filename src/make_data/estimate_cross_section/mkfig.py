import os
import sys
import json
import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs


DIR_DAT = '../../../dat'


def drawRectCrossSectionParams():
    uid = sys.argv[2]

    fp = open(f'{DIR_DAT}/StrRank/dat/network_mesh/1sec/domain/{uid}.txt', 'r')
    nx = int(fp.readline().strip().split()[1])
    ny = int(fp.readline().strip().split()[1])
    bbox = [float(v) for v in fp.readline().strip().split()[1:]]

    elvmap = np.ma.masked_equal(
      np.fromfile(
        f'{DIR_DAT}/StrRank/dat/network_mesh/1sec/elv/{uid}.bin',
        dtype=np.float32,
      ).reshape(ny,nx), 
      -9999,
    )

    nwk = json.load(open(f'{DIR_DAT}/StrRank/dat/network_channel/json/{uid}.json', 'r'))

    chscale = np.fromfile(f'{DIR_DAT}/StrRank/dat/network_upperarea/{uid}.bin').reshape(2,-1)
    chupa, chscale = chscale[0,:], chscale[1,:]

    def draw(var, title):
        fig = plt.figure(figsize=(12,8))
        ax = fig.add_subplot(projection=ccrs.PlateCarree())

        vmin, vmax = elvmap.min(), elvmap.max()
        vcenter = (vmin + vmax) * 0.5
        vrange = (vmax - vmin) * 1.15 * 0.5
        vmin = vcenter - vrange
        vmax = vcenter + vrange
        ax.imshow(elvmap, cmap=plt.cm.binary, interpolation='nearest',
                  vmin=vmin, vmax=vmax, extent=bbox, zorder=1)

        cm = plt.cm.jet
        vmax = var.max()
        for ch in nwk['channel']:
            ax.plot(ch['lon'], ch['lat'], lw=2, color='w', zorder=2)
        for ch, v in zip(nwk['channel'], var):
            ax.plot(ch['lon'], ch['lat'], lw=1.5, color=cm((v/vmax)), zorder=3)

        ax.set_title(title)

        return fig, ax

    sw, cw = 0.427, 6.387
    sd, cd = 0.134, 2.685
    se, ce = 0.159, 2.984

    chwidth = cw * chscale**sw
    chdepth = cd * chscale**sd
    chelvdf = ce * chscale**se

    draw(chwidth, 'Width')
    draw(chdepth, 'Depth')
    draw(chelvdf, 'River Bed to Levee Top')
    plt.show()


task = sys.argv[1]

if task == 'rectCrossSectionParams':
    drawRectCrossSectionParams()
