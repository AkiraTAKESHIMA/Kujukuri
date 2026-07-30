module c2_rt_vrf_io
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
  public :: write_rt_vrf
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_vrf_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function write_rt_vrf(rt, u) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_rt_vrf'
  type(rt_), intent(inout), target :: rt
  type(gs_), intent(inout)         :: u

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('mesh: '//str(u%nam))

  if( u%is_source )then
    if( write_rt_vrf_grid(rt%vrf_src, u%cmn) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    if( write_rt_vrf_grid(rt%vrf_tgt, u%cmn) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function write_rt_vrf
!===============================================================
!
!===============================================================
integer(4) function write_rt_vrf_grid(rtv, uc) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_rt_vrf_grid'
  type(rt_vrf_)   , intent(in), target :: rtv
  type(gs_common_), intent(in), target :: uc

  type(file_grid_out_), pointer :: fg_out
  type(grid_)         , pointer :: g
  type(file_), pointer :: f

  logical :: output_vrf_grdidx     , &
             output_vrf_grdara_true, &
             output_vrf_grdara_rt  , &
             output_vrf_rerr_grdara, &
             output_vrf_grdnum

  integer :: cl_varname

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  g => uc%grid
  fg_out => uc%f_grid_out

  output_vrf_grdidx      = rtv%f%out_grdidx%path      /= ''
  output_vrf_grdara_true = rtv%f%out_grdara_true%path /= ''
  output_vrf_grdara_rt   = rtv%f%out_grdara_rt%path   /= ''
  output_vrf_rerr_grdara = rtv%f%out_rerr_grdara%path /= ''
  output_vrf_grdnum      = rtv%f%out_grdnum%path      /= ''

  cl_varname = 0
  if( output_vrf_grdidx      ) cl_varname = max(cl_varname, len_trim(varname_grdidx     ))
  if( output_vrf_grdara_true ) cl_varname = max(cl_varname, len_trim(varname_grdara_true))
  if( output_vrf_grdara_rt   ) cl_varname = max(cl_varname, len_trim(varname_grdara_rt  ))
  if( output_vrf_rerr_grdara ) cl_varname = max(cl_varname, len_trim(varname_rerr_grdara))
  if( output_vrf_grdnum      ) cl_varname = max(cl_varname, len_trim(varname_grdnum     ))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: The grid system has any valid grid
  if( uc%is_valid )then
    if( output_vrf_grdidx )then
      f => rtv%f%out_grdidx
      call logmsg('Writing '//str(varname_grdidx,cl_varname)//' '//str(fileinfo(f)))
      if( wbin(g%idx, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_grdara_true )then
      f => rtv%f%out_grdara_true
      call logmsg('Writing '//str(varname_grdara_true,cl_varname)//' '//str(fileinfo(f)))
      if( wbin(rtv%grdara_true, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_grdara_rt )then
      f => rtv%f%out_grdara_rt
      call logmsg('Writing '//str(varname_grdara_rt,cl_varname)//' '//str(fileinfo(f)))
      if( wbin(rtv%grdara_rt, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_rerr_grdara )then
      f => rtv%f%out_rerr_grdara
      call logmsg('Writing '//str(varname_rerr_grdara,cl_varname)//' '//str(fileinfo(f)))
      if( wbin(rtv%rerr_grdara, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_grdnum )then
      f => rtv%f%out_grdnum
      call logmsg('Writing '//str(varname_grdnum,cl_varname)//' '//str(fileinfo(f)))
      if( wbin(rtv%grdnum, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: The grid system has no valid grid
  else
    if( output_vrf_grdidx )then
      f => rtv%f%out_grdidx
      call logmsg('Making an empty file for '//str(varname_grdidx,cl_varname))
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_grdara_true )then
      f => rtv%f%out_grdara_true
      call logmsg('Making an empty file for '//str(varname_grdara_true,cl_varname))
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_grdara_rt )then
      f => rtv%f%out_grdara_rt
      call logmsg('Making an empty file for '//str(varname_grdara_rt,cl_varname))
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_rerr_grdara )then
      f => rtv%f%out_rerr_grdara
      call logmsg('Making an empty file for '//str(varname_rerr_grdara,cl_varname))
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    if( output_vrf_grdnum )then
      f => rtv%f%out_grdnum
      call logmsg('Making an empty file for '//str(varname_grdnum,cl_varname))
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(fg_out)
  nullify(g)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function write_rt_vrf_grid
!===============================================================
!
!===============================================================
end module c2_rt_vrf_io
