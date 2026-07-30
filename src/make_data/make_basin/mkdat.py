import os
import sys
import subprocess
import numpy as np
import PIL
import matplotlib.pyplot as plt

dir_JFlwDir_dat_base = '../v1.4'
dir_JFlwDir_dat_basin = '../dat/basin_each'

def conv_dir():
    dir_tiff = os.path.join(dir_JFlwDir_dat_base,'tiff/dir')
    dir_bin  = os.path.join(dir_JFlwDir_dat_base,'bin/dir')
    cp = subprocess.run(['ls', dir_tiff], encoding='utf-8', stdout=subprocess.PIPE)
    for fname in cp.stdout.strip().split('\n'):
        print(fname)
        val = np.array(PIL.Image.open(os.path.join(dir_tiff,fname),'r'),dtype=np.int8)
        #val[val ==    1] = 1  # east
        #val[val ==    2] = 2  # southeast   
        val[val ==    4] = 3  # south
        val[val ==    8] = 4  # southwest
        val[val ==   16] = 5  # west
        val[val ==   32] = 6  # northwest
        val[val ==   64] = 7  # north
        val[val == -128] = 8  # northeast
        #val[val ==    0] = 0  # river mouth
        #val[val ==   -1] = -1  # inland depression
        #val[val ==   -9] = -9  # undefined (ocean)
        if ((val < 0) & (val > 8) & (val != 0) & (val != -1) & (val != -9)).any():
            raise Exception('Unexpected value in val')
        val.tofile(os.path.join(dir_bin,fname.split('.')[0]+'.bin'))

def conv_int4(var):
    dir_tiff = os.path.join(dir_JFlwDir_dat_base,f'tiff/{var}')
    dir_bin  = os.path.join(dir_JFlwDir_dat_base,f'bin/{var}')
    cp = subprocess.run(['ls', dir_tiff], encoding='utf-8', stdout=subprocess.PIPE)
    for fname in cp.stdout.strip().split('\n'):
        print(fname)
        val = np.array(PIL.Image.open(os.path.join(dir_tiff,fname),'r'),dtype=np.int32)
        val.tofile(os.path.join(dir_bin,fname.split('.')[0]+'.bin'))

def conv_real(var):
    dir_tiff = os.path.join(dir_JFlwDir_dat_base,f'tiff/{var}')
    dir_bin  = os.path.join(dir_JFlwDir_dat_base,f'bin/{var}')
    cp = subprocess.run(['ls', dir_tiff], encoding='utf-8', stdout=subprocess.PIPE)
    for fname in cp.stdout.strip().split('\n'):
        print(fname)
        val = np.array(PIL.Image.open(os.path.join(dir_tiff,fname),'r'),dtype=np.float32)
        val.tofile(os.path.join(dir_bin,fname.split('.')[0]+'.bin'))



if __name__ == '__main__':
    var = sys.argv[1]
    if var in ['dir']:
        conv_dir()
    elif var in ['upg']:
        conv_int4(var)
    elif var in ['elv', 'upa', 'wth']:
        conv_real(var)
    else:
        raise Exception(f'Invalid value in $var: {var}')
