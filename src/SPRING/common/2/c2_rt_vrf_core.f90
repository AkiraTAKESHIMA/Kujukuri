module c2_rt_vrf_core
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calc_grdnum
  public :: calc_grdara_rt
  public :: calc_rerr_grdara
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_vrf_core'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function calc_grdnum(&
    rtm   , is_source , val_miss,        &
    grdmsk, grdidx_fmt, arg_fmt , grdnum &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_grdnum'
  type(rt_main_), intent(inout) :: rtm
  logical       , intent(in)    :: is_source
  integer(8)    , intent(in)    :: val_miss
  logical(1)    , intent(in)    :: grdmsk(:)
  integer(8)    , intent(in)    :: grdidx_fmt(:)
  integer(8)    , intent(in)    :: arg_fmt(:)
  integer(8)    , intent(out)   :: grdnum(:)

  logical :: is_sorted
  integer(8), pointer :: grdidx_rtm(:)
  integer(8) :: nij, ij
  integer(8) :: loc

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_sorted = size(arg_fmt) == 1

  nij = size(grdidx_fmt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdmsk) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(grdmsk) /= size(grdidx_fmt)'//&
              '\n  size(grdmsk)    : '//str(size(grdmsk))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  if( size(grdnum) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(grdnum) /= size(grdidx_fmt)'//&
              '\n  size(grdnum)    : '//str(size(grdnum))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  if( size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(arg_fmt) /= 1 .and. size(arg_fmt) /= size(grdidx_fmt)'//&
              '\n  size(arg_fmt)   : '//str(size(arg_fmt))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%nij == 0_8 )then
    call logmsg('The remapping table is empty.')
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( is_source )then
    grdidx_rtm => rtm%sidx
  else
    grdidx_rtm => rtm%tidx
  endif

  if( is_sorted )then
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, loc)
      if( loc == 0_8 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nIndex '//str(grdidx_rtm(ij))//' was not found'//&
                    ' in the set of indices $grdidx_fmt.')
        return
      endif
      call add(grdnum(loc))
    enddo
  else
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, arg_fmt, loc)
      if( loc == 0_8 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nIndex '//str(grdidx_rtm(ij))//' was not found'//&
                    ' in the set of indices $grdidx_fmt.')
        return
      endif
      call add(grdnum(arg_fmt(loc)))
    enddo
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, nij
    if( .not. grdmsk(ij) ) grdnum(ij) = val_miss
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx_rtm)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_grdnum
!===============================================================
!
!===============================================================
integer(4) function calc_grdara_rt(&
    rtm   , is_source , val_miss,           &
    grdmsk, grdidx_fmt, arg_fmt , grdara_rt &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_grdara_rt'
  type(rt_main_), intent(in)  :: rtm
  logical       , intent(in)  :: is_source
  real(8)       , intent(in)  :: val_miss
  logical(1)    , intent(in)  :: grdmsk(:)     !(nij)
  integer(8)    , intent(in)  :: grdidx_fmt(:) !(nij)
  integer(8)    , intent(in)  :: arg_fmt(:)    !(nij)
  real(8)       , intent(out) :: grdara_rt(:)  !(nij)

  logical :: is_sorted
  integer(8), pointer :: grdidx_rtm(:)
  real(8)   , pointer :: grdara_rtm(:)
  integer(8) :: nij, ij
  integer(8) :: loc

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  is_sorted = size(arg_fmt) == 1

  nij = size(grdidx_fmt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdmsk) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(grdmsk) /= size(grdidx_fmt)'//&
              '\n  size(grdmsk)    : '//str(size(grdmsk))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  if( size(grdara_rt) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(grdara_rt) /= size(grdidx_fmt)'//&
              '\n  size(grdara_rt) : '//str(size(grdara_rt))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  if( size(arg_fmt) /= 1 .and. size(arg_fmt) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(arg_fmt) /= 1 .and. size(arg_fmt) /= size(grdidx_fmt)'//&
              '\n  size(arg_fmt)   : '//str(size(arg_fmt))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  grdara_rt(:) = 0.d0

  if( rtm%nij == 0_8 )then
    call logmsg('The remapping table is empty.')
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( is_source )then
    grdidx_rtm => rtm%sidx
  else
    grdidx_rtm => rtm%tidx
  endif
  grdara_rtm => rtm%area
  !-------------------------------------------------------------
  ! Calc. grid area from remapping table
  !-------------------------------------------------------------
  grdara_rt(:) = 0.d0

  if( is_sorted )then
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, loc)
      if( loc == 0_8 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nIndex '//str(grdidx_rtm(ij))//' was not found'//&
                    ' in the set of indices $grdidx_fmt.')
        return
      endif
      call add(grdara_rt(loc), grdara_rtm(ij))
    enddo  ! ij/
  else
    do ij = 1_8, rtm%nij
      call search(grdidx_rtm(ij), grdidx_fmt, arg_fmt, loc)
      if( loc == 0_8 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nIndex '//str(grdidx_rtm(ij))//' was not found'//&
                    ' in the set of indices $grdidx_fmt.')
        return
      endif
      call add(grdara_rt(arg_fmt(loc)), grdara_rtm(ij))
    enddo  ! ij/
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ij = 1_8, nij
    if( .not. grdmsk(ij) ) grdara_rt(ij) = val_miss
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx_rtm)
  nullify(grdara_rtm)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_grdara_rt
!===============================================================
!
!===============================================================
integer(4) function calc_rerr_grdara(&
    grdara_true, grdara_rt, val_miss, grdmsk, rerr_grdara, &
    u &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_rerr_grdara'
  real(8)   , intent(in)  :: grdara_true(:)
  real(8)   , intent(in)  :: grdara_rt(:)
  real(8)   , intent(in)  :: val_miss
  logical(1), intent(in)  :: grdmsk(:)
  real(8)   , intent(out) :: rerr_grdara(:)
  type(gs_) , intent(in), target :: u

  type(gs_common_), pointer :: uc
  type(gs_raster_), pointer :: ur
  integer(8) :: nij, ij
  logical :: is_zero_grdara_true_allowed

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nij = size(rerr_grdara)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(grdmsk) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(grdmsk) /= size(rerr_grdara)'//&
              '\n  size(grdmsk)    : '//str(size(grdmsk))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  if( size(grdara_true) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(grdara_true) /= size(rerr_grdara)'//&
              '\n  size(grdara_true): '//str(size(grdara_true))//&
              '\n  size(grdidx_fmt) : '//str(nij))
    return
  endif
  if( size(grdara_rt) /= nij )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nsize(grdara_rt) /= size(rerr_grdara)'//&
              '\n  size(grdara_rt) : '//str(size(grdara_rt))//&
              '\n  size(grdidx_fmt): '//str(nij))
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  uc => u%cmn
  selectcase( u%typ )
  case( MESHTYPE__RASTER )
    ur => u%raster
    is_zero_grdara_true_allowed = &
      uc%f_grid_in%ara%path /= '' .or. &
      ur%idx_condition == IDX_CONDITION__RST_IN_GRD
  case( MESHTYPE__LATLON, &
        MESHTYPE__POLYGON )
    is_zero_grdara_true_allowed = uc%f_grid_in%ara%path /= ''
  endselect

  do ij = 1_8, nij
    if( grdmsk(ij) )then
      if( grdara_true(ij) > 0.d0 )then
        rerr_grdara(ij) = (grdara_rt(ij) - grdara_true(ij)) / grdara_true(ij)
      elseif( grdara_true(ij) == 0.d0 )then
        if( is_zero_grdara_true_allowed )then
          if( grdara_rt(ij) /= 0.d0 )then
            info = 1
            call errret(msg_unexpected_condition()//&
                      '\nGrid area calculated from remapping table is not zero '//&
                        'although its true value is zero.'//&
                      '\n  ij: '//str(ij)//&
                      '\n  Grid area from rt: '//str(grdara_rt(ij))//&
                      '\n  True value       : '//str(grdara_true(ij)))
            return
          endif
          rerr_grdara(ij) = val_miss
        else
          info = 1
          call errret(msg_unexpected_condition()//&
                    '\nThe true value of grid area is equal to zero.'//&
                    '\n  ij: '//str(ij)//&
                    '\n  Grid area: '//str(grdara_true(ij)))
          return
        endif
      else
        info = 1
        call errret(msg_unexpected_condition()//&
                 '\nThe true value of grid area is negative.'//&
                 '\n  ij: '//str(ij)//&
                 '\n  area: '//str(grdara_true(ij)))
        return
      endif
    else
      rerr_grdara(ij) = val_miss
    endif
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_rerr_grdara
!===============================================================
!
!===============================================================
end module c2_rt_vrf_core
