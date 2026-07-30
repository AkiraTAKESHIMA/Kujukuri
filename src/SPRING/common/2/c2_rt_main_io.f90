module c2_rt_main_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_rt_main
  public :: write_rt_main

  public :: copy_tmp_data
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_main_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function read_rt_main(rtm) result(info)
  use c2_rt_stats, only: &
        get_rt_main_stats
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_rt_main'
  type(rt_main_), intent(inout), target :: rtm

  type(file_), pointer :: f

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( rtm%nij <= 0_8 )then
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nrtm%nij <= 0')
    return
  endif

  rtm%ijsize = rtm%nij

  f => rtm%f%sidx
  if( f%path /= '' )then
    call realloc(rtm%sidx, rtm%ijsize, clear=.true.)
    if( rbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    call realloc(rtm%sidx, 0)
  endif

  f => rtm%f%tidx
  if( f%path /= '' )then
    call realloc(rtm%tidx, rtm%ijsize, clear=.true.)
    if( rbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    call realloc(rtm%tidx, 0)
  endif

  f => rtm%f%area
  if( f%path /= '' )then
    call realloc(rtm%area, rtm%ijsize, clear=.true.)
    if( rbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    call realloc(rtm%area, 0)
  endif

  f => rtm%f%coef
  if( f%path /= '' )then
    call realloc(rtm%coef, rtm%ijsize, clear=.true.)
    if( rbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    call realloc(rtm%coef, 0)
  endif
  !-------------------------------------------------------------
  if( get_rt_main_stats(rtm) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function read_rt_main
!===============================================================
!
!===============================================================
integer(4) function write_rt_main(rtm) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_rt_main'
  type(rt_main_), intent(in), target :: rtm

  type(file_), pointer :: f

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  if( rtm%nij > 0_8 )then
    f => rtm%f%sidx
    if( f%path /= '' )then
      call logmsg('Writing sidx '//str(fileinfo(f)))
      if( wbin(rtm%sidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call logmsg('Writing tidx '//str(fileinfo(f)))
      if( wbin(rtm%tidx(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call logmsg('Writing area '//str(fileinfo(f)))
      if( wbin(rtm%area(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call logmsg('Writing coef '//str(fileinfo(f)))
      if( wbin(rtm%coef(:rtm%nij), f%path, f%dtype, f%endian, f%rec) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  else
    f => rtm%f%sidx
    if( f%path /= '' )then
      call logmsg('Writing sidx '//str(fileinfo(f))//' (empty)')
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => rtm%f%tidx
    if( f%path /= '' )then
      call logmsg('Writing tidx '//str(fileinfo(f))//' (empty)')
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => rtm%f%area
    if( f%path /= '' )then
      call logmsg('Writing area '//str(fileinfo(f))//' (empty)')
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif

    f => rtm%f%coef
    if( f%path /= '' )then
      call logmsg('Writing coef '//str(fileinfo(f))//' (empty)')
      if( make_empty_file(f%path) /= 0 )then
        info = 1; call errret(); return
      endif
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function write_rt_main
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
integer(4) function copy_tmp_data(f, f_tmp, nij, memory_ulim) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'copy_tmp_data'
  type(file_), intent(in) :: f
  type(file_), intent(in) :: f_tmp
  integer(8) , intent(in) :: nij
  real(8)    , intent(in) :: memory_ulim

  integer(8) :: mij, ijs, ije
  integer    :: nDivs, iDiv
  integer(1), allocatable :: dat_int1(:)
  integer(2), allocatable :: dat_int2(:)
  integer(4), allocatable :: dat_int4(:)
  integer(8), allocatable :: dat_int8(:)
  real(4)   , allocatable :: dat_real(:)
  real(8)   , allocatable :: dat_dble(:)

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%rec == 1 )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  call logmsg('Copy from: '//str(fileinfo(f_tmp))//&
          '\n     to  : '//str(fileinfo(f)))

  if( memory_ulim == 0.d0 )then
    mij = nij
  else
    mij = int((memory_ulim*1d6) / 8*4,8)
  endif

  selectcase( f_tmp%dtype )
  case( DTYPE_INT1 )
    allocate(dat_int1(mij))
  case( DTYPE_INT2 )
    allocate(dat_int2(mij))
  case( DTYPE_INT4 )
    allocate(dat_int4(mij))
  case( DTYPE_INT8 )
    allocate(dat_int8(mij))
  case( DTYPE_REAL )
    allocate(dat_real(mij))
  case( DTYPE_DBLE )
    allocate(dat_dble(mij))
  case default
    info = 1
    call errret(msg_invalid_value('f_tmp%dtype', f_tmp%dtype))
    return
  endselect

  nDivs = int((nij-1_8) / mij + 1_8, 4)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ije = 0_8
  do iDiv = 1, nDivs
    ijs = ije + 1_8
    ije = min(ijs + mij - 1_8, nij)
    call logmsg('div '//str(iDiv)//' ij: '//str((/ijs,ije/),dgt(nij),' ~ '))

    selectcase( f_tmp%dtype )
    case( dtype_int1 )
      if( rbin(dat_int1(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
               f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
      if( wbin(dat_int1(:ije-ijs+1_8), f%path, f%dtype, &
               f%endian, f%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
    case( dtype_int2 )
      if( rbin(dat_int2(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
               f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
      if( wbin(dat_int2(:ije-ijs+1_8), f%path, f%dtype, &
               f%endian, f%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
    case( dtype_int4 )
      if( rbin(dat_int4(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
               f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
      if( wbin(dat_int4(:ije-ijs+1_8), f%path, f%dtype, &
               f%endian, f%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
    case( dtype_int8 )
      if( rbin(dat_int8(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
               f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
      if( wbin(dat_int8(:ije-ijs+1_8), f%path, f%dtype, &
               f%endian, f%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
    case( dtype_real )
      if( rbin(dat_real(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
               f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
      if( wbin(dat_real(:ije-ijs+1_8), f%path, f%dtype, &
               f%endian, f%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
    case( dtype_dble )
      if( rbin(dat_dble(:ije-ijs+1_8), f_tmp%path, f_tmp%dtype, &
               f_tmp%endian, f_tmp%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
      if( wbin(dat_dble(:ije-ijs+1_8), f%path, f%dtype, &
               f%endian, f%rec, sz=nij, lb=ijs) /= 0 )then
        info = 1; call errret(); return
      endif
    case default
      info = 1
      call errret(msg_invalid_value('f_tmp%dtype', f_tmp%dtype))
      return
    endselect
  enddo  ! iDiv/
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( allocated(dat_int1) ) deallocate(dat_int1)
  if( allocated(dat_int2) ) deallocate(dat_int2)
  if( allocated(dat_int4) ) deallocate(dat_int4)
  if( allocated(dat_int8) ) deallocate(dat_int8)
  if( allocated(dat_real) ) deallocate(dat_real)
  if( allocated(dat_dble) ) deallocate(dat_dble)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function copy_tmp_data
!===============================================================
!
!===============================================================
end module c2_rt_main_io
