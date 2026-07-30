import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('../../../src/run/rt_ILS_default')
import const, util, conf
from util import env
import s___const as lconst

parser = argparse.ArgumentParser()
parser.add_argument('-s', '--step', help=f'step number (<= {lconst.step_max})', type=int)
parser.add_argument('-x', help='not update data', action='store_true')
parser.add_argument('-f', '--config', help='config file', default='conf.json')
args = parser.parse_args()

update_data = not args.x

env.put_job(lconst.job)
env.put_f_cnf(args.config)


if args.step is None:
    for step in range(env.step_max+1):
        lconst.scripts[step].run(update_data)

else:
    if args.step > env.step_max:
        raise Exception(f'Step must be less than or equal to {env.step_max}.')

    for step in range(args.step):
        lconst.scripts[step].run(False)
    lconst.scripts[args.step].run(update_data)

