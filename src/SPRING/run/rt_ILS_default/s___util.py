import os
import sys
import shutil
import copy

import const, util
from util import env
import pyconf


def adjust_config(cnf):

    cnf = pyconf.fill_landTypes_meshes(cnf)
    cnf = pyconf.copy_gs_shared(cnf)  
    cnf = pyconf.set_rt_default(cnf)
    cnf = pyconf.join_topdir(cnf, cnf['dir_top'])

    return cnf


def get_outdir_grid(meshBaseName, landType):
    return os.path.join(env.dir_out, 'grid', meshBaseName, landType)

