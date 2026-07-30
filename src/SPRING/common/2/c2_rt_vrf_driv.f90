module c2_rt_vrf_driv
  use lib_const
  use lib_base
  use lib_log
  use lib_util
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
  public :: make_rt_vrf
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_vrf_driv'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function make_rt_vrf(rt, a) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt_vrf'
  type(rt_)       , intent(inout), target :: rt
  type(gs_)       , intent(inout)         :: a

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('mesh: '//str(a%nam))

  if( make_rt_vrf_grid(rt, a) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function make_rt_vrf
!===============================================================
!
!===============================================================
integer(4) function make_rt_vrf_grid(rt, a) result(info)
  ! common1
  use c1_opt_ctrl, only: &
        get_opt_log
  ! common2
  use c2_rt_base, only: &
        free_rt_vrf_grid
  use c2_rt_stats, only: &
        report_rt_vrf_summary
  use c2_rt_stats, only: &
        update_rt_vrf_min_max
  use c2_rt_vrf_core, only: &
        calc_grdnum     , &
        calc_grdara_rt  , &
        calc_rerr_grdara
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt_vrf_grid'
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(in)   , target :: a

  type(gs_common_)    , pointer :: ac
  type(rt_vrf_)       , pointer :: rtv
  type(file_grid_out_), pointer :: fg_out
  type(grid_)         , pointer :: g

  logical :: make_vrf_grdidx     , &
             make_vrf_grdara_true, &
             make_vrf_grdara_rt  , &
             make_vrf_rerr_grdara, &
             make_vrf_grdnum

  type(opt_log_) :: opt_log

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ac => a%cmn
  if( .not. ac%is_valid )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( ac%is_source )then
    rtv => rt%vrf_src
  else
    rtv => rt%vrf_tgt
  endif

  fg_out => ac%f_grid_out
  g => ac%grid

  if( free_rt_vrf_grid(rtv) /= 0 )then
    info = 1; call errret(); return
  endif

  make_vrf_grdidx      = rtv%f%out_grdidx%path      /= ''
  make_vrf_grdara_true = rtv%f%out_grdara_true%path /= ''
  make_vrf_grdara_rt   = rtv%f%out_grdara_rt%path   /= ''
  make_vrf_rerr_grdara = rtv%f%out_rerr_grdara%path /= ''
  make_vrf_grdnum      = rtv%f%out_grdnum%path      /= ''

  make_vrf_grdara_true = make_vrf_grdara_true .or. make_vrf_rerr_grdara
  make_vrf_grdara_rt   = make_vrf_grdara_rt   .or. make_vrf_rerr_grdara

  if( .not. (make_vrf_grdidx      .or. &
             make_vrf_grdara_true .or. &
             make_vrf_grdara_rt   .or. &
             make_vrf_rerr_grdara .or. &
             make_vrf_grdnum) )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  call get_opt_log(opt_log)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( make_vrf_grdara_true ) call realloc(rtv%grdara_true, g%nij)
  if( make_vrf_grdara_rt   ) call realloc(rtv%grdara_rt  , g%nij)
  if( make_vrf_rerr_grdara ) call realloc(rtv%rerr_grdara, g%nij)
  if( make_vrf_grdnum      ) call realloc(rtv%grdnum     , g%nij)

  rtv%grdara_true_min = 0.d0
  rtv%grdara_true_max = 0.d0
  rtv%grdara_rt_min   = 0.d0
  rtv%grdara_rt_max   = 0.d0
  rtv%rerr_grdara_min = 0.d0
  rtv%rerr_grdara_max = 0.d0
  rtv%grdnum_min      = 0_8
  rtv%grdnum_max      = 0_8
  rtv%idx_grdara_true_min = ac%idx_miss
  rtv%idx_grdara_true_max = ac%idx_miss
  rtv%idx_grdara_rt_min   = ac%idx_miss
  rtv%idx_grdara_rt_max   = ac%idx_miss
  rtv%idx_rerr_grdara_min = ac%idx_miss
  rtv%idx_rerr_grdara_max = ac%idx_miss
  rtv%idx_grdnum_min      = ac%idx_miss
  rtv%idx_grdnum_max      = ac%idx_miss
  !-------------------------------------------------------------
  ! Calc. true value of grid area
  !-------------------------------------------------------------
  if( make_vrf_grdara_true )then
    call logent('Calculating true value of grid area', PRCNAM, MODNAM)

    call cpval(g%ara, rtv%grdara_true)

    call logext()
  endif
  !-------------------------------------------------------------
  ! Calc. grid area from remapping table
  !-------------------------------------------------------------
  if( make_vrf_grdara_rt )then
    call logent('Calculating grid area from remapping table', PRCNAM, MODNAM)

    if( calc_grdara_rt(&
          rt%main,                     & ! inout
          ac%is_source, rtv%dval_miss, & ! in
          g%msk, g%idx, g%idxarg,      & ! in
          rtv%grdara_rt                & ! out
        ) /= 0 )then
      info = 1; call errret(); return
    endif
    call logext()
  endif
  !-------------------------------------------------------------
  ! Calc. relative error of grid area
  !-------------------------------------------------------------
  if( make_vrf_rerr_grdara )then
    call logent('Calculating relative error of grid area', PRCNAM, MODNAM)

    if( calc_rerr_grdara(&
          rtv%grdara_true, rtv%grdara_rt, rtv%dval_miss, & ! in
          g%msk,                                         & ! in
          rtv%rerr_grdara,                               & ! out
          a                                              & ! in
        ) /= 0 )then
      info = 1; call errret(); return
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  ! Calc. grdnum
  !-------------------------------------------------------------
  if( make_vrf_grdnum )then
    call logent('Counting the number of times that each grid appears in the table', PRCNAM, MODNAM)

    if( calc_grdnum(&
           rt%main,                     & ! inout
           ac%is_source, rtv%ival_miss, & ! in
           g%msk, g%idx, g%idxarg,      & ! in
           rtv%grdnum                   & ! out
        ) /= 0 )then
      info = 1; call errret(); return
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  ! Get stats.
  !-------------------------------------------------------------
  call logent('Getting min. and max.', PRCNAM, MODNAM)

  if( make_vrf_grdara_true )then
    if( update_rt_vrf_min_max(&
          g%idx                  , rtv%grdara_true        , &
          ac%idx_miss            , rtv%dval_miss          , &
          rtv%grdara_true_min    , rtv%grdara_true_max    , &
          rtv%idx_grdara_true_min, rtv%idx_grdara_true_max  &
        ) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( make_vrf_grdara_rt )then
    if( update_rt_vrf_min_max(&
          g%idx                , rtv%grdara_rt        , &
          ac%idx_miss          , rtv%dval_miss        , &
          rtv%grdara_rt_min    , rtv%grdara_rt_max    , &
          rtv%idx_grdara_rt_min, rtv%idx_grdara_rt_max  &
        ) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( make_vrf_rerr_grdara )then
    if( update_rt_vrf_min_max(&
          g%idx                  , rtv%rerr_grdara        , &
          ac%idx_miss            , rtv%dval_miss          , &
          rtv%rerr_grdara_min    , rtv%rerr_grdara_max    , &
          rtv%idx_rerr_grdara_min, rtv%idx_rerr_grdara_max  &
        ) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( make_vrf_grdnum )then
    if( update_rt_vrf_min_max(&
          g%idx             , rtv%grdnum        , &
          ac%idx_miss       , rtv%ival_miss     , &
          rtv%grdnum_min    , rtv%grdnum_max    , &
          rtv%idx_grdnum_min, rtv%idx_grdnum_max  &
        ) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  call logext()
  !-------------------------------------------------------------
  ! Summary
  !-------------------------------------------------------------
  if( opt_log%print_summary )then
    call logent('Summary', PRCNAM, MODNAM)

    if( report_rt_vrf_summary(&
          make_vrf_grdara_true, make_vrf_grdara_rt, &
          make_vrf_rerr_grdara, make_vrf_grdnum   , &
          ac%idx_miss, rtv) /= 0 )then
      info = 1; call errret(); return
    endif

    call logext()
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(rtv)
  nullify(fg_out)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function make_rt_vrf_grid
!===============================================================
!
!===============================================================
end module c2_rt_vrf_driv
