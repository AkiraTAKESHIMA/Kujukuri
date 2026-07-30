program main
  use lib_const
  use lib_log
  use c1_type_opt
  use c2_type_rt
  use def_type
  use mod_set, only: &
        read_settings
  use mod_main, only: &
        make_rt
  use mod_finalize, only: &
        finalize
  implicit none
  type(rt_)      :: rt_in_agcm_to_ogcm_ocean
  type(rt_)      :: rt_out_lsm_to_agcm
  type(agcm_)    :: agcm
  type(lsm_)     :: lsm
  type(opt_ext_) :: opt_ext

  call logbgn('program main', '', '+tr')
  !-------------------------------------------------------------
  call read_settings(&
         rt_in_agcm_to_ogcm_ocean, &
         rt_out_lsm_to_agcm, &
         agcm, lsm, opt_ext)

  call make_rt(&
         rt_in_agcm_to_ogcm_ocean, &
         rt_out_lsm_to_agcm, &
         agcm, lsm, opt_ext)

  call finalize()
  !-------------------------------------------------------------
  call logret('program main', '')
end program main
