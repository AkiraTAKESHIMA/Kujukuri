import os
import sys
import copy

import const, util
from const import k
from util import istep, file_bin
import pyconf

import s___const as lconst


def adjust_config(cnf):

    cnf = pyconf.fill_landTypes_meshes(cnf)
    cnf = pyconf.copy_gs_shared(cnf)
    #cnf = pyconf.set_rt_default(cnf)
    cnf = pyconf.join_topdir(cnf, cnf['dir_top'])

    return cnf

