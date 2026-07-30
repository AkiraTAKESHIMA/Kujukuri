import numpy as np

REGION_WEST = 122.
REGION_EAST = 148.
REGION_SOUTH = 20.
REGION_NORTH = 46.
TILESIZE_LON = 1.0
TILESIZE_LAT = 2.0/3
TXMIN = 22
TXMAX = 48
TYMIN = 30
TYMAX = 68

def txs_of_lon(lon):
    return int((TXMIN-1) + np.floor((lon-REGION_WEST) / TILESIZE_LON) + 1)

def txe_of_lon(lon):
    return int((TXMIN-1) + np.ceil((lon-REGION_WEST) / TILESIZE_LON))

def tys_of_lat(lat):
    return int((TYMIN-1) + np.floor((lat-REGION_SOUTH) / TILESIZE_LAT) + 1)

def tye_of_lat(lat):
    return int((TYMIN-1) + np.ceil((lat-REGION_SOUTH) / TILESIZE_LAT))
