import os
import sys
import subprocess
import copy
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def preprocess(cnf):
    for meshName in cnf[k.m]:
        cnf[k.m][meshName]['name'] = meshName


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    preprocess(cnf)

    util.make_new_f_cnf(cnf)

