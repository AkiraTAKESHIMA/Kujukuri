import conf_make_grid_data as make_grid_data
import conf_remap as remap
import conf_merge_rt as merge_rt
import conf_rasterize as rasterize
import conf_make_cmf_mat as make_cmf_mat
import conf_cpl_define_mat as cpl_define_mat
import conf_cpl_make_rt as cpl_make_rt


def head(dir_tmp: str):
    s = f'\
#\n\
path_report: "{dir_tmp}/report.txt"\n'

    return s

