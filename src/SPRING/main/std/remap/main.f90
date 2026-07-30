program main
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c1_type_timer
  use c1_timer, only: &
        init_ctimer, &
        clear_ctimer
  use c1_gs_define, only: &
        set_gs
  use c1_gs_grid_core, only: &
        make_grdidx
  use c2_type_rt
  use c2_rt_main_io, only: &
        read_rt_main
  use c3_rt_driv, only: &
        make_rt
  use def_type
  use mod_set, only: &
        read_settings
  use mod_remap, only: &
        remap
  use mod_finalize, only: &
        finalize
  implicit none
  type(gs_)  :: s  ! source
  type(gs_)  :: t  ! target
  type(rt_)  :: rt
  logical :: calc_coef = .true.
  logical :: calc_vrf  = .true.
  logical :: output    = .true.

  call logbgn('program remap', '', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call init_ctimer(16)

  call read_settings(s, t, rt)

  selectcase( rt%status )

  case( RT_STATUS__MAKE )
    call traperr( set_gs(s) )
    call traperr( set_gs(t) )
    call traperr( make_rt(s, t, rt, calc_coef, calc_vrf, output) )

  case( RT_STATUS__READ )
    call traperr( make_grdidx(s) )
    call traperr( make_grdidx(t) )
    call traperr( read_rt_main(rt%main) )

  case( RT_STATUS__NONE )
    call errend(msg_unexpected_condition()//&
              '\n  rt%status == RT_STATUS__NONE')

  case default
    call errend(msg_invalid_value('rt%status', rt%status))
  endselect

  call remap(s, t, rt)

  call finalize(s, t, rt)

  call clear_ctimer()
  !-------------------------------------------------------------
  call logret()
end program main
