import os

dir_bin = '../../../bin'
prog_make_grid_data = f'{dir_bin}/std/make_grid_data.exe'
prog_rasterize      = f'{dir_bin}/std/rasterize.exe'
prog_remap          = f'{dir_bin}/std/remap.exe'
prog_merge_rt       = f'{dir_bin}/std/merge_remapping_tables.exe'
prog_make_cmf_mat   = f'{dir_bin}/ext/make_cmf_mat.exe'
prog_cpl_define_mat = f'{dir_bin}/ext/cpl_aogcm-ils_define_mat.exe'
prog_cpl_make_rt    = f'{dir_bin}/ext/cpl_aogcm-ils_make_rt_for_ogcm.exe'

dir_FLOW_src = '../../../src/ext/FLOW_free'

command_ln = 'ln'
options_ln = '-s'

str_dtype_int1 = 'int1'
str_dtype_int2 = 'int2'
str_dtype_int4 = 'int4'
str_dtype_int8 = 'int8'
str_dtype_real = 'real'
str_dtype_dble = 'dble'

endian_big    = 'big'
endian_little = 'little'


class Key():
    def __init__(self):
        self.run = 'runName'
        self.lt = 'landTypes'
        self.m = 'meshes'
        self.rt = 'remappingTables'
        self.rtc = 'remappingTables_common'
        self.irt = 'intermediateRemappingTables'
        self.opt = 'options'

k = Key()
