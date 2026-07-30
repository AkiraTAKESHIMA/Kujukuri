import os
import sys
import numpy as np

import common

DIR_TOP = f'{common.DIR_DAT}/J-FlwDir'
DIR_PRD = f'{DIR_TOP}/dat'
DIR_ORG = f'{DIR_TOP}/v1.4/bin'

resolution = ''
NTX = 27
NTY = 22
NX = 0
NY = 0
NGX = 0
NGY = 0
REGION_SOUTH = 24.0
REGION_NORTH = 46.0
REGION_WEST = 122.0
REGION_EAST = 149.0
TILESIZE_LON = 1.0
TILESIZE_LAT = 1.0

BSNID_MISS = 0
ELV_MISS = -9999.0


def set_resolution(resolution):
    global RESOLUTION
    global NX, NY, NGX, NGY

    RESOLUTION = resolution

    if resolution == '1sec':
        NX = 3600
        NY = 3600
        NGX = NX * NTX
        NGY = NY * NTY


def read_basin_range(bsnId):
    f = f'{DIR_PRD}/basin/{RESOLUTION}/range/{bsnId:07d}.txt'
    if not os.path.isfile(f):
        f = f'{DIR_PRD}/tiled/basin_range/all.txt'
        #print(f)
        fp = open(f, 'r')
        for i in range(bsnId-1):
            nTile = int(fp.readline().strip().split()[1])
            fp.readline()
            for iTile in range(nTile):
                fp.readline()
        dat = fp.readline().strip().split()
        nTile, mgx, mgy, gxs, gxe, gys, gye = [int(v) for v in dat[1:]]
        west, east, south, north = [float(v) for v in fp.readline().strip().split()]
        fp.close()
    else:
        #print(f)
        fp = open(f, 'r')
        fp.readline()  # nx
        fp.readline()  # ny
        gxs, gxe = [int(v) for v in fp.readline().strip().split()[1:]]
        gys, gye = [int(v) for v in fp.readline().strip().split()[1:]]
        west = float(fp.readline().strip().split()[1])
        east = float(fp.readline().strip().split()[1])
        south = float(fp.readline().strip().split()[1])
        north = float(fp.readline().strip().split()[1])
        fp.close()

    return (gxs, gxe, gys, gye), (west, east, south, north)


def tilename(tx, ty):
    return f'n{int(REGION_NORTH-1)-ty+1:02d}e{int(REGION_WEST)+tx-1:03d}'


def gx_to_x(gx):
    tx = int((gx-1) / NX + 1)
    x = gx - (tx-1) * NX
    return tx, x


def gy_to_y(gy):
    ty = int((gy-1) / NY + 1)
    y = gy - (ty-1) * NY
    return ty, y


def read_map_from_tile(var, dtype, gxs, gxe, gys, gye, miss):
    txs, xs = gx_to_x(gxs)
    txe, xe = gx_to_x(gxe)
    tys, ys = gy_to_y(gys)
    tye, ye = gy_to_y(gye)

    dat = np.full((gye-gys+1,gxe-gxs+1), miss)
    for ity in range(tys, tye+1):
        gy0 = (ity-1)*NY
        ys = max(gys-gy0,1)
        ye = min(gye-gy0,NY)
        gys_this = ys + gy0 - (gys-1)
        gye_this = ye + gy0 - (gys-1)
        for itx in range(txs, txe+1):
            gx0 = (itx-1)*NY
            xs = max(gxs-gx0,1)
            xe = min(gxe-gx0,NX)
            gxs_this = xs + gx0 - (gxs-1)
            gxe_this = xe + gx0 - (gxs-1)
            if var in ['bsn']:
                f = f'{DIR_PRD}/tiled/{var}/{tilename(itx,ity)}.bin'
            elif var in ['dir', 'elv', 'upa', 'upg', 'wth']:
                f = f'{DIR_ORG}/{var}/{tilename(itx,ity)}_{var}.bin'
            else:
                raise Exception(f'Invalid value in `var`: {var}')
            if not os.path.isfile(f):
                continue
            dat[gys_this-1:gye_this,gxs_this-1:gxe_this]\
                    = np.fromfile(f, dtype=dtype).reshape(NY,NX)[ys-1:ye,xs-1:xe]
    return dat



