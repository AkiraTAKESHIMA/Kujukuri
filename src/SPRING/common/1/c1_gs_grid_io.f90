module c1_gs_grid_io
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use c1_const
  use c1_type_gs
  use c1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: write_grid_data
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_gs_grid_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function write_grid_data(uc) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_grid_data'
  type(gs_common_), intent(inout), target :: uc

  type(file_grid_out_), pointer :: fg_out
  type(grid_)         , pointer :: g
  type(file_), pointer :: f

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fg_out => uc%f_grid_out
  g => uc%grid
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( uc%is_valid )then
    f => fg_out%idx
    if( f%path /= '' )then
      call logmsg('Writing idx '//str(fileinfo(f)))
      if( wbin(g%idx, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%msk
    if( f%path /= '' )then
      call logmsg('Writing msk '//str(fileinfo(f)))
      if( wbin(g%msk, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%uwa
    if( f%path /= '' )then
      call logmsg('Writing uwa '//str(fileinfo(f)))
      if( wbin(g%uwa, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%ara
    if( f%path /= '' )then
      call logmsg('Writing ara '//str(fileinfo(f)))
      if( wbin(g%ara, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%wgt
    if( f%path /= '' )then
      call logmsg('Writing wgt '//str(fileinfo(f)))
      if( wbin(g%wgt, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%x
    if( f%path /= '' )then
      call logmsg('Writing x   '//str(fileinfo(f)))
      if( wbin(g%x, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%y
    if( f%path /= '' )then
      call logmsg('Writing y   '//str(fileinfo(f)))
      if( wbin(g%y, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%z
    if( f%path /= '' )then
      call logmsg('Writing z   '//str(fileinfo(f)))
      if( wbin(g%z, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%lon
    if( f%path /= '' )then
      call logmsg('Writing lon '//str(fileinfo(f)))
      if( wbin(g%lon, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => fg_out%lat
    if( f%path /= '' )then
      call logmsg('Writing lat '//str(fileinfo(f)))
      if( wbin(g%lat, f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  !-------------------------------------------------------------
  ! Case: No valid data exists
  else
    call logmsg('No valid data exists. Empty files are generated.')

    if( fg_out%idx%path /= '' )then
      if( make_empty_file(fg_out%idx%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%msk%path /= '' )then
      if( make_empty_file(fg_out%msk%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%uwa%path /= '' )then
      if( make_empty_file(fg_out%uwa%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%ara%path /= '' )then
      if( make_empty_file(fg_out%ara%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%wgt%path /= '' )then
      if( make_empty_file(fg_out%wgt%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%x%path   /= '' )then
      if( make_empty_file(fg_out%x%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%y%path   /= '' )then
      if( make_empty_file(fg_out%y%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%z%path   /= '' )then
      if( make_empty_file(fg_out%z%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%lon%path /= '' )then
      if( make_empty_file(fg_out%lon%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
    if( fg_out%lat%path /= '' )then
      if( make_empty_file(fg_out%lat%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function write_grid_data
!===============================================================
!
!===============================================================
end module c1_gs_grid_io
