import os
import numpy as np
import matplotlib.pyplot as plt

dir_bnd = '/data7/akira/ILS/ILS_bnd_20220420/ILS_data/sample/bnd'
dir_fig = '../fig/matsiro/bnd'

list_landType = [
  'river',
  'noriv',
]

list_varName = [
  'gridx',
  'slidx',
  'grtans', 
  'grzsd',
  'gralb', 
  'gralbn',
  'grlai', 
]

for landType in list_landType:
    mask = np.fromfile('../out/land_mask/land_mask_{}.bin'.format(landType),
                       dtype=np.float32).reshape(360,720).byteswap()

    for varName in list_varName:
        path_fig = os.path.join(dir_fig,'{}_{}.png'.format(varName,landType))
        if os.path.isfile(path_fig): continue

        print('Drawing {}_{}'.format(varName,landType))
        path_dat = os.path.join(dir_bnd,'{}_{}.bin'.format(varName,landType))
        if not os.path.isfile(path_dat):
            print('File not found')
            continue

        dat = np.fromfile(path_dat, dtype=np.float32)\
              .reshape(-1,360,720).byteswap().mean(axis=0)

        plt.imshow(np.ma.masked_where(mask==0,dat), cmap=plt.cm.jet, interpolation='nearest')
        plt.colorbar(aspect=50, pad=0.08, orientation='horizontal')
        plt.title('{}_{}'.format(varName,landType))
        plt.savefig(path_fig, bbox_inches='tight', pad_inches=0.1)
        plt.show()
        plt.close()
