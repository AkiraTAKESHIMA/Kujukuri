.SUFFIXES:

DIR_MAKE_CMF_MAT = make_cmf_mat
DIR_CPL_AOGCM_ILS_DEFINE_MAT = cpl_aogcm-ils_define_mat
DIR_CPL_AOGCM_ILS_MAKE_RT_FOR_OGCM = cpl_aogcm-ils_make_rt_for_ogcm

all:
	$(MAKE) -C $(DIR_MAKE_CMF_MAT)
	$(MAKE) -C $(DIR_CPL_AOGCM_ILS_DEFINE_MAT)
	$(MAKE) -C $(DIR_CPL_AOGCM_ILS_MAKE_RT_FOR_OGCM)

install:
	$(MAKE) -C $(DIR_MAKE_CMF_MAT) install
	$(MAKE) -C $(DIR_CPL_AOGCM_ILS_DEFINE_MAT) install
	$(MAKE) -C $(DIR_CPL_AOGCM_ILS_MAKE_RT_FOR_OGCM) install

clean:
	$(MAKE) -C $(DIR_MAKE_CMF_MAT) clean
	$(MAKE) -C $(DIR_CPL_AOGCM_ILS_DEFINE_MAT) clean
	$(MAKE) -C $(DIR_CPL_AOGCM_ILS_MAKE_RT_FOR_OGCM) clean
