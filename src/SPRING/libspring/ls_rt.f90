module ls_rt
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
  use ls_base, only: &
        logopt, &
        assert_initialized
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: initialize
  public :: finalize

  public :: point_rt

  public :: spring_make_rmptbl
  public :: spring_clear_rmptbl

  public :: spring_get_rmptbl_length
  public :: spring_get_rmptbl_data

  public :: spring_print_rmptbl_name
  public :: spring_print_rmptbl
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'ls_rt'

  type(rt_), pointer :: lst_rt(:)
  integer, save :: nmax_rt = 0

  logical :: is_initialized = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function initialize(size_lst_rt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'initialize'
  integer, intent(in) :: size_lst_rt

  integer :: i

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .false.) /= 0 )then
    info = 1; call errret(); return
  endif
  is_initialized = .true.

  allocate(lst_rt(size_lst_rt))
  do i = 1, size(lst_rt)
    lst_rt(i)%nam = ''
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function initialize
!===============================================================
!
!===============================================================
integer(4) function finalize() result(info)
  use c2_rt_base, only: &
        free_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'finalize'

  integer :: i

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif
  is_initialized = .false.

  do i = 1, size(lst_rt)
    if( lst_rt(i)%nam == '' ) cycle
    if( free_rt(lst_rt(i)) /= 0 )then
      info = 1; call errret(); return
    endif
  enddo
  deallocate(lst_rt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function finalize
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer(4) function point_rt(name, rt) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'point_rt'
  character(*), intent(in) :: name
  type(rt_)   , pointer    :: rt

  integer :: i

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif

  do i = 1, nmax_rt
    if( lst_rt(i)%nam == trim(name) )then
      rt => lst_rt(i)
      call logret(PRCNAM, MODNAM)
      return
    endif
  enddo

  info = 1
  call errret('Remapping table "'//str(name)//'" is undefined.')
  return
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function point_rt
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer(4) function spring_make_rmptbl(&
    name, sname, tname) result(info)
  use c2_rt_base, only: &
        init_rt, &
        set_default_values_rt
  use c3_rt_driv, only: &
        make_rt
  use ls_gs, only: &
        point_mesh
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_make_rmptbl'
  character(*), intent(in) :: name
  character(*), intent(in) :: sname, tname

  type(gs_), pointer :: s, t
  type(rt_), pointer :: rt
  integer :: i_rt
  logical, parameter :: output    = .false.
  logical, parameter :: calc_coef = .true.
  logical, parameter :: make_vrf  = .false.

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif

  if( name == '' )then
    info = 1
    call errret(msg_invalid_value('name', name)//&
              '\n$name must not be an empty string.')
    return
  endif

  nullify(rt)
  do i_rt = 1, size(lst_rt)
    if( lst_rt(i_rt)%nam == '' )then
      rt => lst_rt(i_rt)
      nmax_rt = max(i_rt, nmax_rt)
      exit
    endif
  enddo

  if( .not. associated(rt) )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nNo slot for remapping table is left.')
    return
  endif
  !-------------------------------------------------------------
  ! Prepare
  !-------------------------------------------------------------
  if( init_rt(rt) /= 0 )then
    info = 1; call errret(); return
  endif
  rt%nam = name
  rt%snam = sname
  rt%tnam = tname
  rt%id = 'lst_rt('//str(i_rt)//')'
  if( set_default_values_rt(rt) /= 0 )then
    info = 1; call errret(); return
  endif

  if( point_mesh(sname, s) /= 0 )then
    info = 1; call errret(); return
  endif
  if( point_mesh(tname, t) /= 0 )then
    info = 1; call errret(); return
  endif
  s%cmn%is_source = .true.
  t%cmn%is_source = .false.
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  if( make_rt(s, t, rt, calc_coef, make_vrf, output) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(s, t)

  nullify(rt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_make_rmptbl
!===============================================================
!
!===============================================================
integer(4) function spring_clear_rmptbl(name) result(info)
  use c2_rt_base, only: &
        free_rt
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_clear_rmptbl'
  character(*), intent(in) :: name

  type(rt_), pointer :: rt

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( point_rt(name, rt) /= 0 )then
    info = 1; call errret(); return
  endif

  if( free_rt(rt) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(rt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_clear_rmptbl
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer(4) function spring_get_rmptbl_length(name, nij) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_get_rmptbl_length'
  character(*), intent(in) :: name
  integer(8)  , intent(out) :: nij

  type(rt_), pointer :: rt

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( point_rt(name, rt) /= 0 )then
    info = 1; call errret(); return 
  endif
  nij = rt%main%nij

  nullify(rt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_get_rmptbl_length
!===============================================================
!
!===============================================================
integer(4) function spring_get_rmptbl_data(&
    name, sidx, tidx, area, coef) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_get_rmptbl_data'
  character(*), intent(in)  :: name
  integer(8)  , intent(out), optional :: sidx(:)
  integer(8)  , intent(out), optional :: tidx(:)
  real(8)     , intent(out), optional :: area(:)
  real(8)     , intent(out), optional :: coef(:)

  type(rt_), pointer :: rt
  type(rt_main_), pointer :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif

  if( point_rt(name, rt) /= 0 )then
    info = 1; call errret(); return
  endif
  rtm => rt%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(sidx) )then
    if( rtm%nij /= size(sidx) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nSize of $sidx is invalid.'//&
                '\n  Size of $sidx             : '//str(size(sidx))//&
                '\n  Actual length of the table: '//str(rtm%nij))
      return
    endif
    sidx(:) = rtm%sidx(:)
  endif

  if( present(tidx) )then
    if( rtm%nij /= size(tidx) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nSize of $tidx is invalid.'//&
                '\n  Size of $tidx             : '//str(size(tidx))//&
                '\n  Actual length of the table: '//str(rtm%nij))
      return
    endif
    tidx(:) = rtm%tidx(:)
  endif

  if( present(area) )then
    if( rtm%nij /= size(area) )then
      info = 1
      call errend(msg_unexpected_condition()//&
                '\nSize of $area is invalid.'//&
                '\n  Size of $area             : '//str(size(area))//&
                '\n  Actual length of the table: '//str(rtm%nij))
      return
    endif
    area(:) = rtm%area(:rtm%nij)
  endif

  if( present(coef) )then
    if( rtm%nij /= size(coef) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nSize of $coef is invalid.'//&
                '\n  Size of $coef             : '//str(size(coef))//&
                '\n  Actual length of the table: '//str(rtm%nij))
      return
    endif
    coef(:) = rtm%coef(:)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtm)
  nullify(rt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_get_rmptbl_data
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine spring_print_rmptbl_name()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_print_rmptbl_name'

  integer :: i

  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Remapping tables:')
  do i = 1, size(lst_rt)
    if( lst_rt(i)%nam == '' ) cycle
    call logmsg('  ('//str(i,dgt(size(lst_rt)))//') '//str(lst_rt(i)%nam))
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine spring_print_rmptbl_name
!===============================================================
!
!===============================================================
integer(4) function spring_print_rmptbl(name) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_print_rmptbl'
  character(*), intent(in) :: name

  type(rt_)     , pointer :: rt
  type(rt_main_), pointer :: rtm

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( point_rt(name, rt) /= 0 )then
    info = 1; call errret(); return
  endif
  rtm => rt%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Remapping table "'//str(rt%nam)//'"')
  call logmsg('  Source grid: '//str(rt%snam))
  call logmsg('  Target grid: '//str(rt%tnam))
  call logmsg('  Length: '//str(rtm%nij))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtm)
  nullify(rt)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_print_rmptbl
!===============================================================
!
!===============================================================
end module ls_rt
