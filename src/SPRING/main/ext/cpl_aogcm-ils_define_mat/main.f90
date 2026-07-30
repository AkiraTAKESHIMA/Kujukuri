program main
  use lib_log
  use def_type
  use mod_set, only: &
        read_settings
  use mod_check_input, only: &
        check_relations_input
  use mod_define_mat, only: &
        define_mat
  use mod_finalize, only: &
        finalize
  implicit none
  type(rt_in_)  :: rt_in
  type(rt_out_) :: rt_out
  type(agcm_)   :: agcm
  type(rm_)     :: rm
  type(lsm_)    :: lsm

  call logbgn('program main', '', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_settings(rt_in, rt_out, agcm, rm, lsm)

  call check_relations_input(rt_in, rm)

  call define_mat(rt_in, rt_out, agcm, rm, lsm)

  call finalize()
  !-------------------------------------------------------------
  call logret('program main', '')
end program main
