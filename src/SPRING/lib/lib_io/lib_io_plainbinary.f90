module lib_io_plainbinary
  use lib_const
  use lib_base
  use lib_log
  use lib_array, only: &
        cpval
  use lib_io_base, only: &
        unit_number     , &
        byte_of_dtype   , &
        endian_name_long
  use lib_io_file, only: &
        filesize        , &
        check_permission, &
        make_empty_file , &
        remove
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: rbin
  public :: wbin
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface rbin
    module procedure rb__log1_1d
    module procedure rb__log4_1d
    module procedure rb__int1_1d
    module procedure rb__int2_1d
    module procedure rb__int4_1d
    module procedure rb__int8_1d
    module procedure rb__real_1d
    module procedure rb__dble_1d
    module procedure rb__int1_2d
    module procedure rb__int2_2d
    module procedure rb__int4_2d
    module procedure rb__int8_2d
    module procedure rb__real_2d
    module procedure rb__dble_2d
    module procedure rb__int1_3d
    module procedure rb__int2_3d
    module procedure rb__int4_3d
    module procedure rb__int8_3d
    module procedure rb__real_3d
    module procedure rb__dble_3d
    module procedure rb__as1d__log1_2d
    module procedure rb__as1d__log4_2d
    module procedure rb__as1d__int1_2d
    module procedure rb__as1d__int2_2d
    module procedure rb__as1d__int4_2d
    module procedure rb__as1d__int8_2d
    module procedure rb__as1d__real_2d
    module procedure rb__as1d__dble_2d
  end interface

  interface wbin
    module procedure wb__log1_1d
    module procedure wb__log4_1d
    module procedure wb__int1_1d
    module procedure wb__int2_1d
    module procedure wb__int4_1d
    module procedure wb__int8_1d
    module procedure wb__real_1d
    module procedure wb__dble_1d
    module procedure wb__int1_2d
    module procedure wb__int2_2d
    module procedure wb__int4_2d
    module procedure wb__int8_2d
    module procedure wb__real_2d
    module procedure wb__dble_2d
    module procedure wb__int1_3d
    module procedure wb__int2_3d
    module procedure wb__int4_3d
    module procedure wb__int8_3d
    module procedure wb__real_3d
    module procedure wb__dble_3d
  end interface

  interface read_block
    module procedure read_block__log1
    module procedure read_block__log4
    module procedure read_block__int1
    module procedure read_block__int2
    module procedure read_block__int4
    module procedure read_block__int8
    module procedure read_block__real
    module procedure read_block__dble
  end interface

  interface write_block
    module procedure write_block__log1
    module procedure write_block__log4
    module procedure write_block__int1
    module procedure write_block__int2
    module procedure write_block__int4
    module procedure write_block__int8
    module procedure write_block__real
    module procedure write_block__dble
  end interface

  interface fill_block
    module procedure fill_block__log1
    module procedure fill_block__log4
    module procedure fill_block__int1
    module procedure fill_block__int2
    module procedure fill_block__int4
    module procedure fill_block__int8
    module procedure fill_block__real
    module procedure fill_block__dble
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_io_plainbinary'

  integer(8), parameter :: THRESH_DATASIZE = 100000000_8
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function rb__log1_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__log1_1d'
  logical(1)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_LOG1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__log1_1d
!===============================================================
!
!===============================================================
integer(4) function rb__log4_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__log4_1d'
  logical(4)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_LOG4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__log4_1d
!===============================================================
!
!===============================================================
integer(4) function rb__int1_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int1_1d'
  integer(1)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int1_1d
!===============================================================
!
!===============================================================
integer(4) function rb__int2_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int2_1d'
  integer(2)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int2_1d
!===============================================================
!
!===============================================================
integer(4) function rb__int4_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int4_1d'
  integer(4)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int4_1d
!===============================================================
!
!===============================================================
integer(4) function rb__int8_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int8_1d'
  integer(8)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int8_1d
!===============================================================
!
!===============================================================
integer(4) function rb__real_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__real_1d'
  real(4)     , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__real_1d
!===============================================================
!
!===============================================================
integer(4) function rb__dble_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__dble_1d'
  real(8)     , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(f, dtype_, sz_, rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__dble_1d
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
integer(4) function rb__int1_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int1_2d'
  integer(1)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int1_2d
!===============================================================
!
!===============================================================
integer(4) function rb__int2_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int2_2d'
  integer(2)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int2_2d
!===============================================================
!
!===============================================================
integer(4) function rb__int4_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int4_2d'
  integer(4)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int4_2d
!===============================================================
!
!===============================================================
integer(4) function rb__int8_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int8_2d'
  integer(8)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int8_2d
!===============================================================
!
!===============================================================
integer(4) function rb__real_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__real_2d'
  real(4)     , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__real_2d
!===============================================================
!
!===============================================================
integer(4) function rb__dble_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__dble_2d'
  real(8)     , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__dble_2d
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
integer(4) function rb__int1_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int1_3d'
  integer(1)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  if( read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int1_3d
!===============================================================
!
!===============================================================
integer(4) function rb__int2_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int2_3d'
  integer(2)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  if( read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int2_3d
!===============================================================
!
!===============================================================
integer(4) function rb__int4_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int4_3d'
  integer(4)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  if( read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int4_3d
!===============================================================
!
!===============================================================
integer(4) function rb__int8_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__int8_3d'
  integer(8)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  if( read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__int8_3d
!===============================================================
!
!===============================================================
integer(4) function rb__real_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__real_3d'
  real(4)     , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  if( read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__real_3d
!===============================================================
!
!===============================================================
integer(4) function rb__dble_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__dble_3d'
  real(8)     , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  if( read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__dble_3d
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
integer(4) function rb__as1d__log1_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__log1_2d'
  logical(1)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_LOG1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__log1_2d
!===============================================================
!
!===============================================================
integer(4) function rb__as1d__log4_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__log4_2d'
  logical(4)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_LOG4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__log4_2d
!===============================================================
!
!===============================================================
integer(4) function rb__as1d__int1_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__int1_2d'
  integer(1)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__int1_2d
!===============================================================
!
!===============================================================
integer(4) function rb__as1d__int2_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__int2_2d'
  integer(2)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__int2_2d
!===============================================================
!
!===============================================================
integer(4) function rb__as1d__int4_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__int4_2d'
  integer(4)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__int4_2d
!===============================================================
!
!===============================================================
integer(4) function rb__as1d__int8_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__int8_2d'
  integer(8)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__int8_2d
!===============================================================
!
!===============================================================
integer(4) function rb__as1d__real_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__real_2d'
  real(4)     , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__real_2d
!===============================================================
!
!===============================================================
integer(4) function rb__as1d__dble_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'rb__as1d__dble_2d'
  real(8)     , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( check_input_file(&
        f, dtype_, product(sz_), rec_, check_recl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_input_file_stream(un, f, endian_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  if( read_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  if( read_block(-1, dat(:), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function rb__as1d__dble_2d
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
integer(4) function wb__log1_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__log1_1d'
  logical(1)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical(1)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical(1)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_LOG1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = logical(.true.,1)  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__log1_1d
!===============================================================
!
!===============================================================
integer(4) function wb__log4_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__log4_1d'
  logical(4)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical(4)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical(4)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_LOG4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = logical(.true.,4)  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__log4_1d
!===============================================================
!
!===============================================================
integer(4) function wb__int1_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int1_1d'
  integer(1)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(1)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(1)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_1  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int1_1d
!===============================================================
!
!===============================================================
integer(4) function wb__int2_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int2_1d'
  integer(2)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(2)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(2)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_2  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int2_1d
!===============================================================
!
!===============================================================
integer(4) function wb__int4_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int4_1d'
  integer(4)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(4)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(4)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_4  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int4_1d
!===============================================================
!
!===============================================================
integer(4) function wb__int8_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int8_1d'
  integer(8)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(8)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(8)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_8  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int8_1d
!===============================================================
!
!===============================================================
integer(4) function wb__real_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__real_1d'
  real(4)     , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  real(4)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  real(4)             :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0.0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__real_1d
!===============================================================
!
!===============================================================
integer(4) function wb__dble_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__dble_1d'
  real(8)     , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  real(8)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  real(8)             :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0.d0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, sz_, replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    if( write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n) /= 0 )then
      info = 1; call errret(); return
    endif
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:), dtype_, un, pos, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__dble_1d
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
integer(4) function wb__int1_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int1_2d'
  integer(1)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(1)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(1)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_1  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int1_2d
!===============================================================
!
!===============================================================
integer(4) function wb__int2_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int2_2d'
  integer(2)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(2)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(2)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_2  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int2_2d
!===============================================================
!
!===============================================================
integer(4) function wb__int4_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int4_2d'
  integer(4)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(4)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(4)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_4  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int4_2d
!===============================================================
!
!===============================================================
integer(4) function wb__int8_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int8_2d'
  integer(8)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(8)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(8)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_8  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int8_2d
!===============================================================
!
!===============================================================
integer(4) function wb__real_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__real_2d'
  real(4)     , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(4)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  real(4)             :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__real_2d
!===============================================================
!
!===============================================================
integer(4) function wb__dble_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__dble_2d'
  real(8)     , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(8)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  real(8)             :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.d0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      if( write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n) /= 0 )then
        info = 1; call errret(); return
      endif
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__dble_2d
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
integer(4) function wb__int1_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int1_3d'
  integer(1)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(1)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(1)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_1  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int1_3d
!===============================================================
!
!===============================================================
integer(4) function wb__int2_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int2_3d'
  integer(2)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(2)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(2)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_2  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int2_3d
!===============================================================
!
!===============================================================
integer(4) function wb__int4_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int4_3d'
  integer(4)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(4)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(4)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_4  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int4_3d
!===============================================================
!
!===============================================================
integer(4) function wb__int8_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__int8_3d'
  integer(8)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(8)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(8)          :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_8  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__int8_3d
!===============================================================
!
!===============================================================
integer(4) function wb__real_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__real_3d'
  real(4)     , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(4)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  real(4)             :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__real_3d
!===============================================================
!
!===============================================================
integer(4) function wb__dble_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'wb__dble_3d'
  real(8)     , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(8)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  real(8)             :: fill_  !--dtype--
  logical             :: replace_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3
  integer :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.d0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_name_long(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( prep_output_file(f, dtype_, product(sz_), replace_, fs) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  if( open_output_file_stream(un, f, endian_, replace_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  if( write_block(0, dat(:,1,1), dtype_, un, 0_8, nn) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)
  if( nfill_max > 0_8 )then
    if( fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata)) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        if( write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n) /= 0 )then
          info = 1; call errret(); return
        endif

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
            info = 1; call errret(); return
          endif
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
          info = 1; call errret(); return
        endif
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      if( fill_block(1, fill_, dtype_, un, pos, nfill) /= 0 )then
        info = 1; call errret(); return
      endif
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  if( write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8) /= 0 )then
    info = 1; call errret(); return
  endif

  ! Deallocate
  if( nfill_max > 0_8 )then
    if( fill_block(-1, fill_, dtype_, un, 0_8, 0_8) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( close_file(un) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function wb__dble_3d
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
integer(4) function read_block__log1(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__log1'
  integer     , intent(in) :: job
  logical(1)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  logical(4), allocatable, save :: tmpl4(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  case( 0 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); continue
    case( DTYPE_LOG4 ); allocate(tmpl4(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_LOG1 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_LOG4 )
      read(un,pos=pos) tmpl4(:n)
      call cpval(tmpl4(:n), dat(:n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); continue
    case( DTYPE_LOG4 ); deallocate(tmpl4)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__log1
!===============================================================
!
!===============================================================
integer(4) function read_block__log4(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__log4'
  integer     , intent(in) :: job
  logical(4)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  logical(1), allocatable, save :: tmpl1(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); allocate(tmpl1(n))
    case( DTYPE_LOG4 ); continue
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_LOG1 )
      read(un,pos=pos) tmpl1(:n)
      call cpval(tmpl1(:n), dat(:n))
    case( DTYPE_LOG4 )
      read(un,pos=pos) dat(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); deallocate(tmpl1)
    case( DTYPE_LOG4 ); continue
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__log4
!===============================================================
!
!===============================================================
integer(4) function read_block__int1(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__int1'
  integer     , intent(in) :: job
  integer(1)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); continue
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      call cpval(tmpi2(:n), dat(:n))
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      call cpval(tmpi4(:n), dat(:n))
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      call cpval(tmpi8(:n), dat(:n))
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      call cpval(tmpr4(:n), dat(:n))
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      call cpval(tmpr8(:n), dat(:n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); continue
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__int1
!===============================================================
!
!===============================================================
integer(4) function read_block__int2(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__int2'
  integer     , intent(in) :: job
  integer(2)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); continue
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      call cpval(tmpi1(:n), dat(:n))
    case( DTYPE_INT2 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      call cpval(tmpi4(:n), dat(:n))
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      call cpval(tmpi8(:n), dat(:n))
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      call cpval(tmpr4(:n), dat(:n))
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      call cpval(tmpr8(:n), dat(:n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); continue
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__int2
!===============================================================
!
!===============================================================
integer(4) function read_block__int4(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__int4'
  integer     , intent(in) :: job
  integer(4)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); continue
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      call cpval(tmpi1(:n), dat(:n))
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      call cpval(tmpi2(:n), dat(:n))
    case( DTYPE_INT4 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      call cpval(tmpi8(:n), dat(:n))
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      call cpval(tmpr4(:n), dat(:n))
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      call cpval(tmpr8(:n), dat(:n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); continue
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__int4
!===============================================================
!
!===============================================================
integer(4) function read_block__int8(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__int8'
  integer     , intent(in) :: job
  integer(8)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); continue
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      call cpval(tmpi1(:n), dat(:n))
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      call cpval(tmpi2(:n), dat(:n))
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      call cpval(tmpi4(:n), dat(:n))
    case( DTYPE_INT8 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      call cpval(tmpr4(:n), dat(:n))
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      call cpval(tmpr8(:n), dat(:n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); continue
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__int8
!===============================================================
!
!===============================================================
integer(4) function read_block__real(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__real'
  integer     , intent(in) :: job
  real(4)     , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); continue
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      call cpval(tmpi1(:n), dat(:n))
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      call cpval(tmpi2(:n), dat(:n))
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      call cpval(tmpi4(:n), dat(:n))
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      call cpval(tmpi8(:n), dat(:n))
    case( DTYPE_REAL )
      read(un,pos=pos) dat(:n)
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      call cpval(tmpr8(:n), dat(:n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); continue
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__real
!===============================================================
!
!===============================================================
integer(4) function read_block__dble(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_block__dble'
  integer     , intent(in) :: job
  real(8)     , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); continue
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      call cpval(tmpi1(:n), dat(:n))
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      call cpval(tmpi2(:n), dat(:n))
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      call cpval(tmpi4(:n), dat(:n))
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      call cpval(tmpi8(:n), dat(:n))
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      call cpval(tmpr4(:n), dat(:n))
    case( DTYPE_DBLE )
      read(un,pos=pos) dat(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); continue
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function read_block__dble
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
integer(4) function write_block__log1(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__log1'
  integer     , intent(in) :: job
  logical(1)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  logical(4), allocatable, save :: tmpl4(:)
  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); continue
    case( DTYPE_LOG4 ); allocate(tmpl4(n))
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_LOG1 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_LOG4 )
      call cpval(dat(:n), tmpl4(:n))
      write(un,pos=pos) tmpl4(:n)
    case( DTYPE_INT1 )
      where( dat(:n) ); tmpi1(:n) = INT1_TRUE
      elsewhere       ; tmpi1(:n) = INT1_FALSE
      endwhere
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      where( dat(:n) ); tmpi2(:n) = INT2_TRUE
      elsewhere       ; tmpi2(:n) = INT2_FALSE
      endwhere
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      where( dat(:n) ); tmpi4(:n) = INT4_TRUE
      elsewhere       ; tmpi4(:n) = INT4_FALSE
      endwhere
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      where( dat(:n) ); tmpi8(:n) = INT8_TRUE
      elsewhere       ; tmpi8(:n) = INT8_FALSE
      endwhere
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      where( dat(:n) ); tmpr4(:n) = REAL_TRUE
      elsewhere       ; tmpr4(:n) = REAL_FALSE
      endwhere
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      where( dat(:n) ); tmpr8(:n) = DBLE_TRUE
      elsewhere       ; tmpr8(:n) = DBLE_FALSE
      endwhere
      write(un,pos=pos) tmpr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); continue
    case( DTYPE_LOG4 ); deallocate(tmpl4)
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__log1
!===============================================================
!
!===============================================================
integer(4) function write_block__log4(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__log4'
  integer     , intent(in) :: job
  logical(4)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  logical(1), allocatable, save :: tmpl1(:)
  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); allocate(tmpl1(n))
    case( DTYPE_LOG4 ); continue
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Read
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_LOG1 )
      call cpval(dat(:n), tmpl1(:n))
      write(un,pos=pos) tmpl1(:n)
    case( DTYPE_LOG4 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_INT1 )
      where( dat(:n) ); tmpi1(:n) = INT1_TRUE
      elsewhere       ; tmpi1(:n) = INT1_FALSE
      endwhere
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      where( dat(:n) ); tmpi2(:n) = INT2_TRUE
      elsewhere       ; tmpi2(:n) = INT2_FALSE
      endwhere
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      where( dat(:n) ); tmpi4(:n) = INT4_TRUE
      elsewhere       ; tmpi4(:n) = INT4_FALSE
      endwhere
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      where( dat(:n) ); tmpi8(:n) = INT8_TRUE
      elsewhere       ; tmpi8(:n) = INT8_FALSE
      endwhere
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      where( dat(:n) ); tmpr4(:n) = REAL_TRUE
      elsewhere       ; tmpr4(:n) = REAL_FALSE
      endwhere
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      where( dat(:n) ); tmpr8(:n) = DBLE_TRUE
      elsewhere       ; tmpr8(:n) = DBLE_FALSE
      endwhere
      write(un,pos=pos) tmpr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); deallocate(tmpl1)
    case( DTYPE_LOG4 ); continue
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__log4
!===============================================================
!
!===============================================================
integer(4) function write_block__int1(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__int1'
  integer     , intent(in) :: job
  integer(1)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); continue
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_INT2 )
      call cpval(dat(:n), tmpi2(:n))
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      call cpval(dat(:n), tmpi4(:n))
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      call cpval(dat(:n), tmpi8(:n))
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      call cpval(dat(:n), tmpr4(:n))
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      call cpval(dat(:n), tmpr8(:n))
      write(un,pos=pos) tmpr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); continue
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__int1
!===============================================================
!
!===============================================================
integer(4) function write_block__int2(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__int2'
  integer     , intent(in) :: job
  integer(2)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); continue
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      call cpval(dat(:n), tmpi1(:n))
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_INT4 )
      call cpval(dat(:n), tmpi4(:n))
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      call cpval(dat(:n), tmpi8(:n))
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      call cpval(dat(:n), tmpr4(:n))
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      call cpval(dat(:n), tmpr8(:n))
      write(un,pos=pos) tmpr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); continue
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__int2
!===============================================================
!
!===============================================================
integer(4) function write_block__int4(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__int4'
  integer     , intent(in) :: job
  integer(4)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); continue
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      call cpval(dat(:n), tmpi1(:n))
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      call cpval(dat(:n), tmpi2(:n))
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_INT8 )
      call cpval(dat(:n), tmpi8(:n))
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      call cpval(dat(:n), tmpr4(:n))
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      call cpval(dat(:n), tmpr8(:n))
      write(un,pos=pos) tmpr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); continue
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__int4
!===============================================================
!
!===============================================================
integer(4) function write_block__int8(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__int8'
  integer     , intent(in) :: job
  integer(8)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); continue
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      call cpval(dat(:n), tmpi1(:n))
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      call cpval(dat(:n), tmpi2(:n))
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      call cpval(dat(:n), tmpi4(:n))
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_REAL )
      call cpval(dat(:n), tmpr4(:n))
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      call cpval(dat(:n), tmpr8(:n))
      write(un,pos=pos) tmpr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); continue
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__int8
!===============================================================
!
!===============================================================
integer(4) function write_block__real(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__real'
  integer     , intent(in) :: job
  real(4)     , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(8)   , allocatable, save :: tmpr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); continue
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      call cpval(dat(:n), tmpi1(:n))
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      call cpval(dat(:n), tmpi2(:n))
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      call cpval(dat(:n), tmpi4(:n))
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      call cpval(dat(:n), tmpi8(:n))
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      write(un,pos=pos) dat(:n)
    case( DTYPE_DBLE )
      call cpval(dat(:n), tmpr8(:n))
      write(un,pos=pos) tmpr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); continue
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__real
!===============================================================
!
!===============================================================
integer(4) function write_block__dble(&
    job, dat, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_block__dble'
  integer     , intent(in) :: job
  real(8)     , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); continue
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      call cpval(dat(:n), tmpi1(:n))
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      call cpval(dat(:n), tmpi2(:n))
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      call cpval(dat(:n), tmpi4(:n))
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      call cpval(dat(:n), tmpi8(:n))
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      call cpval(dat(:n), tmpr4(:n))
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      write(un,pos=pos) dat(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); continue
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function write_block__dble
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
integer(8) pure function get_nfill_max(&
    ndim, fs, present_fill, sz, lb, ub) result(nfill_max)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_nfill_max'
  integer   , intent(in) :: ndim
  integer(8), intent(in) :: fs
  logical   , intent(in) :: present_fill
  integer(8), intent(in) :: sz(:), lb(:), ub(:)

  nfill_max = 0_8

  if( lb(1) > 1_8 .and. (fs == 0_8 .or. present_fill) )then
    nfill_max = max(nfill_max, lb(1)-1_8)
  endif
  if( ub(1) < sz(1) .and. (fs == 0_8 .or. present_fill) )then
    nfill_max = max(nfill_max, sz(1)-ub(1))
  endif

  if( ndim >= 2 )then
    if( lb(2) > 1_8 .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, sz(1)*(lb(2)-1_8))
    endif
    if( ub(2) < sz(2) .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, sz(1)*(sz(2)-ub(2)))
    endif
  endif

  if( ndim >= 3 )then
    if( lb(3) > 1_8 .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, product(sz(:2))*(lb(3)-1_8))
    endif
    if( ub(3) < sz(3) .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, product(sz(:2))*(sz(3)-ub(3)))
    endif
  endif
end function get_nfill_max
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
integer(4) function fill_block__log1(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__log1'
  integer     , intent(in) :: job
  logical(1)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  logical(1), allocatable, save :: filll1(:)
  logical(4), allocatable, save :: filll4(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); allocate(filll1(n)); filll1(:) = logical(fill,1)
    case( DTYPE_LOG4 ); allocate(filll4(n)); filll4(:) = logical(fill,4)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); write(un,pos=pos) filll1(:n)
    case( DTYPE_LOG4 ); write(un,pos=pos) filll4(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); deallocate(filll1)
    case( DTYPE_LOG4 ); deallocate(filll4)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__log1
!===============================================================
!
!===============================================================
integer(4) function fill_block__log4(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__log4'
  integer     , intent(in) :: job
  logical(4)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  logical(1), allocatable, save :: filll1(:)
  logical(4), allocatable, save :: filll4(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); allocate(filll1(n)); filll1(:) = logical(fill,1)
    case( DTYPE_LOG4 ); allocate(filll4(n)); filll4(:) = logical(fill,4)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); write(un,pos=pos) filll1(:n)
    case( DTYPE_LOG4 ); write(un,pos=pos) filll4(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_LOG1 ); deallocate(filll1)
    case( DTYPE_LOG4 ); deallocate(filll4)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__log4
!===============================================================
!
!===============================================================
integer(4) function fill_block__int1(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__int1'
  integer     , intent(in) :: job
  integer(1)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__int1
!===============================================================
!
!===============================================================
integer(4) function fill_block__int2(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__int2'
  integer     , intent(in) :: job
  integer(2)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Ccase: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__int2
!===============================================================
!
!===============================================================
integer(4) function fill_block__int4(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__int4'
  integer     , intent(in) :: job
  integer(4)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__int4
!===============================================================
!
!===============================================================
integer(4) function fill_block__int8(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__int8'
  integer     , intent(in) :: job
  integer(8)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__int8
!===============================================================
!
!===============================================================
integer(4) function fill_block__real(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__real'
  integer     , intent(in) :: job
  real(4)     , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__real
!===============================================================
!
!===============================================================
integer(4) function fill_block__dble(&
    job, fill, dtype, un, pos, n) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'fill_block__dble'
  integer     , intent(in) :: job
  real(8)     , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Allocate
  !-------------------------------------------------------------
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Write
  !-------------------------------------------------------------
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: Deallocate
  !-------------------------------------------------------------
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      info = 1
      call erradd(msg_invalid_value('dtype', dtype), &
                  '', PRCNAM, MODNAM)
      return
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  !-------------------------------------------------------------
  case default
    info = 1
    call erradd(msg_invalid_value('job', job), &
                '', PRCNAM, MODNAM)
    return
  endselect
end function fill_block__dble
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
integer(4) function check_input_file(&
    path, dtype, sz, rec, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input_file'
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  integer     , intent(in)  :: rec
  logical     , intent(in)  :: check_recl

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  if( check_permission(&
        path, action_read, allow_empty=.false.) /= 0 )then
    info = 1; call errret(); return
  endif

  if( check_input_filesize(&
        path, dtype, sz, rec, check_recl) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_input_file
!===============================================================
!
!===============================================================
integer(4) function prep_output_file(&
    path, dtype, sz, replace, fs) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'prep_output_file'
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  logical     , intent(in)  :: replace
  integer(8)  , intent(out) :: fs

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  fs = 0_8

  if( access(path,' ') == 0 )then
    if( check_permission(&
          path, action_for_replace(replace), allow_empty=.false.) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( replace .and. access(path,' ') == 0 )then
    if( remove(path, dir=.false., output=.false.) /= 0 )then
      info = 1; call errret(); return
    endif
  endif

  if( check_output_filesize(path, dtype, sz, fs) /= 0 )then
    info = 1; call errret(); return
  endif

  if( access(path,' ') /= 0 )then
    if( make_empty_file(path, .false.) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function prep_output_file
!===============================================================
!
!===============================================================
integer(4) function check_input_filesize(&
    path, dtype, sz, rec, check_recl) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_input_filesize'
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  integer     , intent(in)  :: rec
  logical     , intent(in)  :: check_recl

  integer(8) :: fs
  integer    :: byte
  integer(8) :: recl
  integer(8) :: fs_min

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fs = filesize(path)
  byte = byte_of_dtype(dtype)
  recl = byte * sz
  fs_min = recl * rec

  if( mod(fs, recl) /= 0_8 )then
    if( check_recl )then
      info = 1
      call errret('File size is invalid.'//&
                '\n  Path: '//str(path)//&
                '\n  Data type: '//str(dtype)//' ('//str(byte)//' byte)'//&
                '\n  Record length: '//str(recl)//&
                '\n  File size    : '//str(fs)//&
                '\nFile size must be a multiple number of record length.')
      return
    endif
  else
    if( fs < fs_min )then
      info = 1
      call errret('File size is invalid.'//&
                '\n  Path: '//str(path)//&
                '\n  Data type: '//str(dtype)//' ('//str(byte)//' byte)'//&
                '\n  File size                 : '//str(fs)//&
                '\n  Record length             : '//str(recl)//&
                '\n  Record number             : '//str(rec)//&
                '\n  Expected minimum file size: '//str(fs_min))
      return
    endif
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_input_filesize
!===============================================================
!
!===============================================================
integer(4) function check_output_filesize(&
    path, dtype, sz, fs) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_output_filesize'
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  integer(8)  , intent(out) :: fs

  integer    :: byte
  integer(8) :: recl

  integer :: access

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( access(path,' ') /= 0 )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  fs = filesize(path)
  byte = byte_of_dtype(dtype)
  recl = byte * sz

  if( mod(fs, recl) /= 0_8 )then
    info = 1
    call errret('File size is invalid.'//&
              '\n  Path: '//str(path)//&
              '\n  File size    : '//str(fs)//&
              '\n  Record length: '//str(recl)//&
              '\nFile size must be a multiple number of record length.')
    return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function check_output_filesize
!===============================================================
!
!===============================================================
integer(4) function open_input_file_stream(&
    un, path, endian) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'open_input_file_stream'
  integer     , intent(in)  :: un
  character(*), intent(in)  :: path
  character(*), intent(in)  :: endian

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( endian )
  case( ENDIAN_BIG, ENDIAN_BIG_SHORT, &
        ENDIAN_LITTLE, ENDIAN_LITTLE_SHORT )
    open(un, file=path, &
         form='unformatted', access='stream', &
         action='read', status='old', convert=endian, &
         iostat=info)
  case( ENDIAN_UNDEF )
    open(un, file=path, &
         form='unformatted', access='stream', &
         action='read', status='old', &
         iostat=info)
  case default
    info = 1
    call errret(msg_invalid_value('endian', endian))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function open_input_file_stream
!===============================================================
!
!===============================================================
integer(4) function open_output_file_stream(&
    un, path, endian, replace) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'open_output_file_stream'
  integer     , intent(in)  :: un
  character(*), intent(in)  :: path
  character(*), intent(in)  :: endian
  logical     , intent(in)  :: replace

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  open(un, file=path, &
       form='unformatted', access='stream', &
       action=action_for_replace(replace), &
       status='unknown', convert=endian, &
       iostat=info)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function open_output_file_stream
!===============================================================
!
!===============================================================
integer(4) function close_file(un) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'close_file'
  integer, intent(in)  :: un

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  close(un, iostat=info)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function close_file
!===============================================================
!
!===============================================================
character(CLEN_KEY) function action_for_replace(replace) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'action_for_replace'
  logical, intent(in) :: replace

  if( replace )then
    res = ACTION_WRITE
  else
    res = ACTION_READWRITE
  endif
end function action_for_replace
!===============================================================
!
!===============================================================
end module lib_io_plainbinary
