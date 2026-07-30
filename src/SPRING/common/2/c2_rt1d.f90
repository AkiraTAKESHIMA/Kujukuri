module c2_rt1d
  use lib_const
  use lib_log
  use lib_array
  use c1_type_opt
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_rt1d
  public :: alloc_rt1d
  public :: free_rt1d_data
  public :: clear_rt1d
  public :: reshape_rt1d
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt1d'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function init_rt1d(rt1d) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_rt1d'
  type(rt1d_), intent(inout), target :: rt1d(:)

  type(rt1d_), pointer :: rt1

  integer(8) :: ijs, ije, ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ijs = lbound(rt1d, 1)
  ije = ubound(rt1d, 1)

  rt1d(:)%ijsize = 0_8
  rt1d(:)%mij    = 0_8

  do ij = ijs, ije
    rt1 => rt1d(ij)
    nullify(rt1%idx)
    nullify(rt1%ara)
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function init_rt1d
!===============================================================
!
!===============================================================
integer(4) function alloc_rt1d(rt1d, n, ijsize) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'alloc_rt1d'
  type(rt1d_), pointer :: rt1d(:)
  integer(8) , intent(in) :: n
  integer(8) , intent(in) :: ijsize

  type(rt1d_), pointer :: rt1
  integer(8) :: i

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(rt1d(n))
  do i = 1_8, n
    rt1 => rt1d(i)
    rt1%mij = 0_8
    rt1%ijsize = ijsize
    allocate(rt1%idx(ijsize))
    allocate(rt1%ara(ijsize))
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function alloc_rt1d
!===============================================================
!
!===============================================================
integer(4) function free_rt1d_data(rt1d) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'free_rt1d_data'
  type(rt1d_), intent(inout), target :: rt1d(:)

  type(rt1d_), pointer :: rt1
  integer(8) :: ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, size(rt1d)
    rt1 => rt1d(ij)

    if( rt1%ijsize > 0_8 )then
      rt1%ijsize = 0_8
      rt1%mij    = 0_8
      rt1%idx_self = 0_8
      call realloc(rt1%idx, 0)
      call realloc(rt1%ara, 0)
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function free_rt1d_data
!===============================================================
!
!===============================================================
integer(4) function clear_rt1d(rt1d) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_rt1d'
  type(rt1d_), pointer :: rt1d(:)

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( free_rt1d_data(rt1d) /= 0 )then
    info = 1; call errret(); return
  endif
  deallocate(rt1d)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function clear_rt1d
!===============================================================
!
!===============================================================
integer(4) function reshape_rt1d(rt1d, self_is_source, rtm) result(info)
  use c1_opt_ctrl, only: &
        get_opt_earth
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'reshape_rt1d'
  type(rt1d_)   , intent(in)   , target :: rt1d(:)
  logical       , intent(in)            :: self_is_source
  type(rt_main_), intent(inout), target :: rtm

  type(opt_earth_) :: earth
  type(rt1d_), pointer :: rt1
  integer(8) :: ij

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call get_opt_earth(earth)

  rtm%nij = sum(rt1d(:)%mij)
  rtm%ijsize = rtm%nij
  call realloc(rtm%sidx, rtm%ijsize)
  call realloc(rtm%tidx, rtm%ijsize)
  call realloc(rtm%area, rtm%ijsize)
  call logmsg('Length: '//str(rtm%ijsize))

  if( rtm%ijsize == 0_8 )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( self_is_source )then
    rtm%nij = 0_8
    do ij = 1_8, size(rt1d)
      rt1 => rt1d(ij)
      if( rt1%mij > 0_8 )then
        rtm%sidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx_self
        rtm%tidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx(:rt1%mij)
        rtm%area(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%ara(:rt1%mij)
        rtm%nij = rtm%nij + rt1%mij
      endif
    enddo  ! ij_1d/
  else
    rtm%nij = 0_8
    do ij = 1_8, size(rt1d)
      rt1 => rt1d(ij)
      if( rt1%mij > 0_8 )then
        rtm%sidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx(:rt1%mij)
        rtm%tidx(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%idx_self
        rtm%area(rtm%nij+1_8:rtm%nij+rt1%mij) = rt1%ara(:rt1%mij)
        rtm%nij = rtm%nij + rt1%mij
      endif
    enddo  ! ij_1d/
  endif

  rtm%area(:) = rtm%area(:) * earth%r**2

  call logmsg('area min: '//str(minval(rtm%area))//&
                 ', max: '//str(maxval(rtm%area))//&
            '\n     sum: '//str(sum(rtm%area)))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function reshape_rt1d
!===============================================================
!
!===============================================================
end module c2_rt1d
