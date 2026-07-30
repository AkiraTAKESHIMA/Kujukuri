import os
import sys
import shutil
import copy

import pyconf


def adjust_config(cnf):

    cnf = pyconf.fill_landTypes_meshes(cnf)
    cnf = pyconf.copy_gs_shared(cnf)
    cnf = pyconf.set_rt_default(cnf)
    cnf = pyconf.join_topdir(cnf, cnf['dir_top'])

    return cnf

