import os
import sys
import numpy as np
import matplotlib.pyplot as plt
import shapefile
import cartopy.crs as ccrs


DIR_DAT = '/data10/atakeshima/Kujukuri/dat'
ENC = 'cp932'


def draw_nlni_dam(fig, ax, sf, bbox):
    bwest, bsouth, beast, bnorth = bbox
    for shp in sf.shapes():
        lon, lat = shp.points[0]
        if lon < bwest or beast < lon or lat < bsouth or bnorth < lat:
            continue
        ax.scatter(lon, lat, s=20, marker='s', facecolor='w', color='k')


def bbox_intersect(a, b):
    awest, asouth, aeast, anorth = a
    bwest, bsouth, beast, bnorth = b
    if aeast < bwest or beast < awest or anorth < bsouth or bnorth < asouth:
        return False
    else:
        return True


def makeNodeDict(sf, bbox):
    c = {}
    for k, (shp, rec) in enumerate(zip(sf.shapes(), sf.records())):
        if not bbox_intersect(bbox, shp.bbox):
            continue

        for i in (0, -1):
            lon = shp.points[i][0]
            lat = shp.points[i][1]
            if lon in c.keys():
                if lat in c[lon].keys():
                    c[lon][lat].append((k,i))
                else:
                    c[lon][lat] = [(k,i)]
            else:
                c[lon] = {lat:[(k,i)]}
    return c


def draw_nlni_river(fig, ax, sf, bbox):
    c = makeNodeDict(sf, bbox)

    for shp, rec in zip(sf.shapes(), sf.records()):
        if not bbox_intersect(bbox, shp.bbox):
            continue

        ax.plot([p[0] for p in shp.points], [p[1] for p in shp.points],
                 linewidth=1, color='royalblue', zorder=1)

        for i in (0, -1):
            lon, lat = shp.points[i]
            if len(c[lon][lat]) == 1:
                color = 'dodgerblue'
            elif len(c[lon][lat]) == 2:
                color = 'gray'
            elif len(c[lon][lat]) >= 3:
                color = 'tomato'
            ax.scatter(lon, lat, s=20, facecolor='w', edgecolor=color, zorder=2)

    return fig, ax


def draw_osm_waterway(fig, ax, sf, bbox):
    c = makeNodeDict(sf, bbox)

    for shp, rec in zip(sf.shapes(), sf.records()):
        if not bbox_intersect(bbox, shp.bbox):
            continue

        if rec[2] in ['river', 'stream', 'tidal_channel', 'flowline']:
            color = 'royalblue'
        elif rec[2] in ['canal', 'pressurised', 'drain', 'ditch', 'link', 'fairway',
                        'fish_pass', 'canoe_pass']:
            color = 'deepskyeblue'
        else:
            color = 'silver'
        ax.plot([p[0] for p in shp.points], [p[1] for p in shp.points], 
                linewidth=1, color=color, zorder=1)

        for i in (0, -1):
            lon, lat = shp.points[i]
            if len(c[lon][lat]) == 1:
                color = 'dodgerblue'
            elif len(c[lon][lat]) == 2:
                color = 'gray'
            elif len(c[lon][lat]) >= 3:
                color = 'tomato'
            ax.scatter(lon, lat, s=20, facecolor='w', edgecolor=color, zorder=2)

    return fig, ax



def findSameGroupRivers(j, i, idx_group, nGroup, sf, dct_lonlat):
    if idx_group[j] != 0:
        return
    idx_group[j] = nGroup-1

    lon, lat = sf.shape(j).points[i]
    for (jj, ii) in dct_lonlat[lon][lat]:
        if jj == j and ii == i:
            continue
        findSameGroupRivers(jj, -(ii+1), idx_group, nGroup, sf, dct_lonlat)


def divideIntoGroups():
    sf = shapefile.Reader(
      f'{DIR_DAT}/NLNI/dl/river/W05-08_12_GML/W05-08_12-g_Stream.shp', 
      encoding=ENC
    )

    dct_lonlat = makeNodeDict(sf, sf.bbox)

    idx_group = np.zeros((sf.numShapes), dtype=np.int32)
    nGroup = 0
    for j, shp in enumerate(sf.shapes()):
        if idx_group[j] != 0:
            continue

        nGroup += 1
        for i in (0, -1):
            findSameGroupRivers(j, i, idx_group, nGroup, sf, dct_lonlat)



def compare():
    sf_nlni_river = shapefile.Reader(
      f'{DIR_DAT}/NLNI/dl/river/W05-08_12_GML/W05-08_12-g_Stream.shp', 
      encoding=ENC
    )
    sf_osm_waterway = shapefile.Reader(
      f'{DIR_DAT}/OSM/dl/Geofabrik/Japan/260313/Kanto/gis_osm_waterways_free_1.shp', 
      encoding=ENC
    )

    bbox = [140.17779194, 35.32494139, 140.39263199, 35.48104833]

    fig1 = plt.figure()
    ax = fig1.add_subplot(projection=ccrs.PlateCarree())
    fig1, ax = draw_nlni_river(fig1, ax, sf_nlni_river, bbox)
    plt.show()


if __name__ == '__main__':

    compare()

    #divideIntoGroups()
