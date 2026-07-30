module c2_rt_main_finish
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public proceduers
  !-------------------------------------------------------------
  public :: finish_rt_main
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_main_finish'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function finish_rt_main(rt, s, t, output) result(info)
  use c2_rt_stats, only: &
        get_rt_main_stats     , &
        report_rt_main_summary
  use c2_rt_main_util, only: &
        modify_rt_area               , &
        remove_zero                  , &
        check_coef_after_modification, &
        sort_rt
  use c2_rt_main_coef, only: &
        calc_rt_coef
  use c2_rt_main_io, only: &
        write_rt_main
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finish_rt_main'
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(inout), target :: s, t
  logical  , intent(in), optional :: output

  logical :: output_

  type(gs_)       , pointer :: a  ! coef grid
  type(gs_common_), pointer :: ac
  type(grid_)     , pointer :: g
  type(rt_main_)  , pointer :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  output_ = .false.
  if( present(output) ) output_ = output

  rtm => rt%main

  selectcase( rtm%mesh_coef )
  case( MESH__SOURCE )
    a => s
  case( MESH__TARGET )
    a => t
  case default
    info = 1
    call errret(msg_invalid_value('rtm%mesh_coef', rtm%mesh_coef))
    return
  endselect

  ac => a%cmn
  g => ac%grid
  !-------------------------------------------------------------
  ! Case: Empty
  !-------------------------------------------------------------
  if( rtm%nij == 0_8 )then
    !-----------------------------------------------------------
    ! Case: Empty file is allowed.
    if( rtm%allow_empty )then
      call logwrn('No valid data exists. Empty files are generated.')

      if( write_rt_main(rtm) /= 0 )then
        info = 1; call errret(); return
      endif

      call logret(PRCNAM, MODNAM)
      return
    !-----------------------------------------------------------
    ! Case: Empty file is not allowed.
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nrtm%nij == 0')
      return
    endif
  endif
  !-------------------------------------------------------------
  ! Modify area
  !-------------------------------------------------------------
  if( modify_rt_area(rtm, g%idx, g%idxarg, g%ara) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  allocate(rtm%coef(rtm%nij))

  if( calc_rt_coef(rtm, g%idx, g%idxarg, g%ara) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Remove zero
  !-------------------------------------------------------------
  if( remove_zero(rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  if( rtm%ijsize == 0_8 )then
    if( rtm%allow_empty )then
      call logwrn('The remapping table is empty.')
      call logret(PRCNAM, MODNAM)
      return
    else
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nrtm%ijsize == 0')
      return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_coef_after_modification(rtm%coef, rtm%opt_coef) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Sort
  !-------------------------------------------------------------
  if( sort_rt(rtm) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  if( get_rt_main_stats(rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  if( output_ )then
    if( report_rt_main_summary(rtm) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  if( output_ )then
    if( write_rt_main(rtm) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function finish_rt_main
!===============================================================
!
!===============================================================
end module c2_rt_main_finish
