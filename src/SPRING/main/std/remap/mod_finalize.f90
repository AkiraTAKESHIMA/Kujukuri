module mod_finalize
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_math
  use c1_type_gs
  use c1_type_timer
  use c1_timer, only: &
        point_ctimer
  use c2_type_rt
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: finalize
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_finalize'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine finalize(s, t, rt)
  use c1_file, only: &
        report           , &
        close_report_file
  use c1_gs_base, only: &
        clear_mesh
  use c2_rt_base, only: &
        clear_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize'
  type(gs_), intent(inout) :: s, t
  type(rt_), intent(inout) :: rt

  type(ctimer_), pointer :: ct
  type(timer_), pointer :: timer
  type(timer_elem_), pointer :: te, te1, te2
  real(8) :: time_total
  integer :: i
  integer :: cl
  character(:), allocatable :: msg

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call point_ctimer(ct)
  timer => ct%timer
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call traperr( clear_mesh(s) )
  call traperr( clear_mesh(t) )

  call traperr( clear_rt(rt) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( timer%is_active )then
    time_total = sum(timer%elem(:timer%n)%time)
    call logmsg('total (original): '//str(time_total))

    te1 => timer%elem(get_idx_timer(timer, 'searching_intersecting_grids (2)'))
    te2 => timer%elem(get_idx_timer(timer, 'intersection'))
    call add(te2%time, -te1%time)
    te1%name = ''
    te1%time = 0.d0

    cl = 0
    do i = 1, timer%n
      cl = max(cl, len_trim(timer%elem(i)%name))
    enddo

    call report('------ Process Time ------')
    allocate(character(1) :: msg)

    time_total = sum(timer%elem(:timer%n)%time)
    do i = 1, timer%n
      te => timer%elem(i)
      if( te%name == '' ) cycle
      msg = str(te%name,cl)//' '//str(te%time)//' (sec) '//&
          ' ('//str(te%time/time_total*1d2,'f6.2')//' %)'
      call logmsg(msg)
      call report(msg)
    enddo
    msg = str('Total',cl)//' '//str(time_total)//' (sec)'
    call logmsg(msg)
    call report(msg)
  endif

  call traperr( close_report_file() )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine finalize
!===============================================================
!
!===============================================================
end module mod_finalize
