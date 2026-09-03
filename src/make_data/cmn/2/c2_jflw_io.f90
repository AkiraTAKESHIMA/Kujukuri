module c2_jflw_io
  use lib_const
  use lib_base
  use lib_io
  use lib_array
  use c1_const
  use c2_jflw_const
  use c2_jflw_grid, only: &
        xy_to_gxy, &
        gxy_to_xy, &
        gx_of_x, &
        gy_of_y, &
        gxs_of_lon, &
        gxe_of_lon, &
        gys_of_lat, &
        gye_of_lat
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: tilename
  public :: intId
  public :: strId

  public :: read_basin_domain_from_all
  public :: read_basin_domain_from_each
  public :: write_basin_domain_for_each

  public :: read_map_from_tile
  public :: read_basin_map_from_tile

  public :: get_f_map_tile
  public :: get_f_lst_tile
  public :: get_f_map_basin
  public :: get_f_dat_basin
  public :: get_dir_bsnara_tile
  public :: get_f_lst_all
  public :: get_dir_rt
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_map_from_tile
    module procedure read_map_from_tile__int1
    module procedure read_map_from_tile__int4
    module procedure read_map_from_tile__real
  end interface

  interface read_basin_map_from_tile
    module procedure read_basin_map_from_tile__int1
    module procedure read_basin_map_from_tile__int4
    module procedure read_basin_map_from_tile__real
    module procedure read_basin_map_from_tile__dble
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_jflw_io'
  !-------------------------------------------------------------
  ! Interfaces for intrisic functions
  !-------------------------------------------------------------
  interface
    integer function access(f, mode)
      character(*), intent(in) :: f
      character(*), intent(in) :: mode
    end function access
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
character(7) function tilename(tx, ty)
  implicit none
  integer, intent(in) :: tx, ty

  tilename = 'n'//str(int(REGION_NORTH-1)-ty+1,-2)//&
             'e'//str(int(REGION_WEST)+tx-1,-3)
end function tilename
!===============================================================
!
!===============================================================
integer function intId(s) result(i)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'intId'
  character(*), intent(in) :: s

  integer :: ios

  read(s,*,iostat=ios) i

  if( ios /= 0 )then
    call errend('Failed to convert character to integer: '//str(s), &
      '', PRCNAM, MODNAM)
  endif
end function intId
!===============================================================
!
!===============================================================
character(DGT_BSNID_MAX) function strId(id) result(s)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'strId'
  integer, intent(in) :: id

  if( dgt(id) > DGT_BSNID_MAX )then
    call errend('Failed to convert integer to character: '//str(id), &
      '', PRCNAM, MODNAM)
  endif

  s = str(id, -DGT_BSNID_MAX)
end function strId
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
subroutine read_basin_domain_from_all(&
    resl, id, &
    gxi, gxf, gyi, gyf, west, east, south, north)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_domain_from_all'
  character(*), intent(in) :: resl
  character(*), intent(in) :: id
  integer, intent(out) :: gxi, gxf, gyi, gyf
  real(8), intent(out), optional :: west, east, south, north

  real(8) :: west_, east_, south_, north_

  integer :: iid
  integer :: n, i
  character :: c_
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_lst_tile(resl, 'basin_domain'), status='old')
  read(un,*)

  do iid = 1, intId(id)-1
    read(un,*) c_, n
    read(un,*)
    do i = 1, n
      read(un,*)
    enddo
  enddo

  ! raster range in global index
  read(un,*) c_, n, gxi, gxf, gyi, gyf
  ! bbox
  read(un,*) west_, east_, south_, north_
  ! raster ranges in tiles
  do i = 1, n
    read(un,*)
  enddo

  close(un)

  if( present(west) ) west = west_
  if( present(east) ) east = east_
  if( present(south) ) south = south_
  if( present(north) ) north = north_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_domain_from_all
!===============================================================
!
!===============================================================
subroutine read_basin_domain_from_each(&
    resl, id, &
    gxi, gxf, gyi, gyf, west, east, south, north)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_domain_from_each'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: id
  integer     , intent(out) :: gxi, gxf, gyi, gyf
  real(8)     , intent(out) :: west, east, south, north

  integer :: un
  character :: c_
  integer :: n, i

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_dat_basin(resl, 'domain', id), status='old')
  read(un,*)
  read(un,*)
  read(un,*) c_, gxi, gxf
  read(un,*) c_, gyi, gyf
  read(un,*) c_, west
  read(un,*) c_, east
  read(un,*) c_, south
  read(un,*) c_, north
  read(un,*) n
  ! raster ranges in tiles
  do i = 1, n
    read(un,*)
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_domain_from_each
!===============================================================
!
!===============================================================
subroutine write_basin_domain_for_each(&
    resl, id, &
    gxi, gxf, gyi, gyf, west, east, south, north)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_basin_domain_for_each'
  character(*), intent(in) :: resl
  character(*), intent(in) :: id
  integer     , intent(in) :: gxi, gxf, gyi, gyf
  real(8)     , intent(in) :: west, east, south, north

  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( resl == RESOLUTION_1SEC )then
    call logmsg('Need not to make a new file of basin domain.')
    call logret(PRCNAM, MODNAM)
    return
  endif

  open(newunit=un, file=get_f_dat_basin(resl, 'domain', id), status='replace')
  write(un,"(a)") 'nx '//str(gxf-gxi+1,DGT_GXY)
  write(un,"(a)") 'ny '//str(gyf-gyi+1,DGT_GXY)
  write(un,"(a)") 'gx '//str((/gxi,gxf/),DGT_GXY)
  write(un,"(a)") 'gy '//str((/gyi,gyf/),DGT_GXY)
  write(un,"(a)") 'west  '//str(west ,'f20.15')
  write(un,"(a)") 'east  '//str(east ,'f20.15')
  write(un,"(a)") 'south '//str(south,'f20.15')
  write(un,"(a)") 'north '//str(north,'f20.15')
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_basin_domain_for_each
!===============================================================
!
!===============================================================
subroutine read_map_from_tile__int1(&
    resl, var, dtype, miss, gxs, gys, &
    dat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_map_from_tile__int1'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: var
  character(*), intent(in)  :: dtype
  integer(1)  , intent(in)  :: miss
  integer     , intent(in)  :: gxs, gys
  integer(1)  , intent(out) :: dat(gxs:,gys:)

  integer :: gxe, gye
  integer :: txs, txe, tys, tye
  integer :: xs, xe, ys, ye
  integer :: itx, ity
  integer :: xs_this, xe_this, ys_this, ye_this
  integer :: gxs_this, gxe_this, gys_this, gye_this
  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  gxe = gxs + size(dat,1) - 1
  gye = gys + size(dat,2) - 1

  call gxy_to_xy(gxs, gys, txs, xs, tys, ys)
  call gxy_to_xy(gxe, gye, txe, xe, tye, ye)

  dat(:,:) = miss
  do ity = tys, tye
    ys_this = 1
    ye_this = NY
    if( ity == tys ) ys_this = ys
    if( ity == tye ) ye_this = ye
    gys_this = gy_of_y(ity, ys_this)
    gye_this = gy_of_y(ity, ye_this)
    do itx = txs, txe
      xs_this = 1
      xe_this = NX
      if( itx == txs ) xs_this = xs
      if( itx == txe ) xe_this = xe
      gxs_this = gx_of_x(itx, xs_this)
      gxe_this = gx_of_x(itx, xe_this)

      f = get_f_map_tile(resl, var, itx, ity)
      if( access(f, ' ') /= 0 ) cycle

      call traperr( rbin(&
             dat(gxs_this:gxe_this,gys_this:gye_this), f, &
             dtype, sz=int((/NX,NY/),8), lb=int((/xs_this,ys_this/),8)) )
    enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_map_from_tile__int1
!===============================================================
!
!===============================================================
subroutine read_map_from_tile__int4(&
    resl, var, dtype, miss, gxs, gys, &
    dat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_map_from_tile__int4'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: var
  character(*), intent(in)  :: dtype
  integer(4)  , intent(in)  :: miss
  integer     , intent(in)  :: gxs, gys
  integer(4)  , intent(out) :: dat(gxs:,gys:)

  integer :: gxe, gye
  integer :: txs, txe, tys, tye
  integer :: xs, xe, ys, ye
  integer :: itx, ity
  integer :: xs_this, xe_this, ys_this, ye_this
  integer :: gxs_this, gxe_this, gys_this, gye_this
  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  gxe = gxs + size(dat,1) - 1
  gye = gys + size(dat,2) - 1

  call gxy_to_xy(gxs, gys, txs, xs, tys, ys)
  call gxy_to_xy(gxe, gye, txe, xe, tye, ye)

  dat(:,:) = miss
  do ity = tys, tye
    ys_this = 1
    ye_this = NY
    if( ity == tys ) ys_this = ys
    if( ity == tye ) ye_this = ye
    gys_this = gy_of_y(ity, ys_this)
    gye_this = gy_of_y(ity, ye_this)
    do itx = txs, txe
      xs_this = 1
      xe_this = NX
      if( itx == txs ) xs_this = xs
      if( itx == txe ) xe_this = xe
      gxs_this = gx_of_x(itx, xs_this)
      gxe_this = gx_of_x(itx, xe_this)

      f = get_f_map_tile(resl, var, itx, ity)
      if( access(f, ' ') /= 0 ) cycle

      call traperr( rbin(&
             dat(gxs_this:gxe_this,gys_this:gye_this), f, &
             dtype, sz=int((/NX,NY/),8), lb=int((/xs_this,ys_this/),8)) )
    enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_map_from_tile__int4
!===============================================================
!
!===============================================================
subroutine read_map_from_tile__real(&
    resl, var, dtype, miss, gxs, gys, &
    dat)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_map_from_tile__real'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: var
  character(*), intent(in)  :: dtype
  real(4)     , intent(in)  :: miss
  integer     , intent(in)  :: gxs, gys
  real(4)     , intent(out) :: dat(gxs:,gys:)

  integer :: gxe, gye
  integer :: txs, txe, tys, tye
  integer :: xs, xe, ys, ye
  integer :: itx, ity
  integer :: xs_this, xe_this, ys_this, ye_this
  integer :: gxs_this, gxe_this, gys_this, gye_this
  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  gxe = gxs + size(dat,1) - 1
  gye = gys + size(dat,2) - 1

  call gxy_to_xy(gxs, gys, txs, xs, tys, ys)
  call gxy_to_xy(gxe, gye, txe, xe, tye, ye)

  dat(:,:) = miss
  do ity = tys, tye
    ys_this = 1
    ye_this = NY
    if( ity == tys ) ys_this = ys
    if( ity == tye ) ye_this = ye
    gys_this = gy_of_y(ity, ys_this)
    gye_this = gy_of_y(ity, ye_this)
    do itx = txs, txe
      xs_this = 1
      xe_this = NX
      if( itx == txs ) xs_this = xs
      if( itx == txe ) xe_this = xe
      gxs_this = gx_of_x(itx, xs_this)
      gxe_this = gx_of_x(itx, xe_this)

      f = get_f_map_tile(resl, var, itx, ity)
      if( access(f, ' ') /= 0 ) cycle

      call traperr( rbin(&
             dat(gxs_this:gxe_this,gys_this:gye_this), f, &
             dtype, sz=int((/NX,NY/),8), lb=int((/xs_this,ys_this/),8)) )
    enddo  ! itx/
  enddo  ! ity/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_map_from_tile__real
!===============================================================
!
!===============================================================
subroutine read_basin_map_from_tile__int1(&
    resl, id, var, dat, dtype, gxi, gyi, miss, bsn)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_map_from_tile__int1'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: id
  character(*), intent(in)  :: var
  character(*), intent(in)  :: dtype
  integer     , intent(in)  :: gxi, gyi
  integer(1)  , intent(out) :: dat(gxi:,gyi:)
  integer(1)  , intent(in)  :: miss
  integer(4)  , intent(in), optional :: bsn(gxi:,gyi:)

  integer(1) :: undef
  integer :: tx, ty
  integer :: xi, xf, yi, yf
  integer :: lgxi, lgxf, lgyi, lgyf
  integer :: n, i
  integer :: id_
  character :: c_
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  undef = miss - 1_1
  dat(:,:) = undef

  open(newunit=un, file=get_f_lst_tile(resl, 'basin_domain'), status='old')
  read(un,*)
  do id_ = 1, intId(id)-1
    read(un,*) c_, n
    read(un,*)  ! bbox
    do i = 1, n
      read(un,*)
    enddo
  enddo
  read(un,*) c_, n
  read(un,*)  ! bbox
  do i = 1, n
    read(un,*) c_, tx, ty, xi, xf, yi, yf
    call xy_to_gxy(tx, ty, xi, yi, lgxi, lgyi)
    call xy_to_gxy(tx, ty, xf, yf, lgxf, lgyf)

    if( any(dat(lgxi:lgxf,lgyi:lgyf) /= undef) )then
      call errend(msg_unexpected_condition()//&
                  ' Data are overlapping.')
    endif

    call traperr( rbin(&
           dat(lgxi:lgxf,lgyi:lgyf), get_f_map_tile(resl,var,tx,ty), &
           dtype, sz=int((/NX,NY/),8), lb=int((/xi,yi/),8)) )
  enddo  ! i/
  close(un)

  if( present(bsn) )then
    where( bsn /= intId(id) ) dat = miss
  else
    where( dat == undef ) dat = miss
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_map_from_tile__int1
!===============================================================
!
!===============================================================
subroutine read_basin_map_from_tile__int4(&
    resl, id, var, dat, dtype, gxi, gyi, miss, bsn)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_map_from_tile__int4'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: id
  character(*), intent(in)  :: var
  character(*), intent(in)  :: dtype
  integer     , intent(in)  :: gxi, gyi
  integer(4)  , intent(out) :: dat(gxi:,gyi:)
  integer(4)  , intent(in)  :: miss
  integer(4)  , intent(in), optional :: bsn(gxi:,gyi:)

  integer(4) :: undef
  integer :: tx, ty
  integer :: xi, xf, yi, yf
  integer :: lgxi, lgxf, lgyi, lgyf
  integer :: n, i
  integer :: id_
  character :: c_
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  undef = miss - 1
  dat(:,:) = undef

  open(newunit=un, file=get_f_lst_tile(resl, 'basin_domain'), status='old')
  read(un,*)
  do id_ = 1, intId(id)-1
    read(un,*) c_, n
    read(un,*)  ! bbox
    do i = 1, n
      read(un,*)
    enddo
  enddo
  read(un,*) c_, n
  read(un,*)  ! bbox
  do i = 1, n
    read(un,*) c_, tx, ty, xi, xf, yi, yf
    call xy_to_gxy(tx, ty, xi, yi, lgxi, lgyi)
    call xy_to_gxy(tx, ty, xf, yf, lgxf, lgyf)

    if( any(dat(lgxi:lgxf,lgyi:lgyf) /= undef) )then
      call errend(msg_unexpected_condition()//&
                  ' Data are overlapping.')
    endif

    call traperr( rbin(&
           dat(lgxi:lgxf,lgyi:lgyf), get_f_map_tile(resl,var,tx,ty), &
           dtype, sz=int((/NX,NY/),8), lb=int((/xi,yi/),8)) )
  enddo  ! i/
  close(un)

  if( present(bsn) )then
    where( bsn /= intId(id) ) dat = miss
  else
    where( bsn == undef ) dat = miss
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_map_from_tile__int4
!===============================================================
!
!===============================================================
subroutine read_basin_map_from_tile__real(&
    resl, id, var, dat, dtype, gxi, gyi, miss, bsn)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_map_from_tile__real'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: id
  character(*), intent(in)  :: var
  character(*), intent(in)  :: dtype
  integer     , intent(in)  :: gxi, gyi
  real(4)     , intent(out) :: dat(gxi:,gyi:)
  real(4)     , intent(in)  :: miss
  integer(4)  , intent(in), optional :: bsn(gxi:,gyi:)

  real(4) :: undef
  integer :: tx, ty
  integer :: xi, xf, yi, yf
  integer :: lgxi, lgxf, lgyi, lgyf
  integer :: n, i
  integer :: id_
  character :: c_
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  undef = miss - 1.0
  dat(:,:) = undef

  open(newunit=un, file=get_f_lst_tile(resl, 'basin_domain'), status='old')
  read(un,*)
  do id_ = 1, intId(id)-1
    read(un,*) c_, n
    read(un,*)  ! bbox
    do i = 1, n
      read(un,*)
    enddo
  enddo
  read(un,*) c_, n
  read(un,*)  ! bbox
  do i = 1, n
    read(un,*) c_, tx, ty, xi, xf, yi, yf
    call xy_to_gxy(tx, ty, xi, yi, lgxi, lgyi)
    call xy_to_gxy(tx, ty, xf, yf, lgxf, lgyf)

    if( any(dat(lgxi:lgxf,lgyi:lgyf) /= undef) )then
      call errend(msg_unexpected_condition()//&
                  ' Data are overlapping.')
    endif

    call traperr( rbin(&
           dat(lgxi:lgxf,lgyi:lgyf), get_f_map_tile(resl,var,tx,ty), &
           dtype, sz=int((/NX,NY/),8), lb=int((/xi,yi/),8)) )
  enddo  ! i/
  close(un)

  if( present(bsn) )then
    where( bsn /= intId(id) ) dat = miss
  else
    where( bsn == undef ) dat = miss
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_map_from_tile__real
!===============================================================
!
!===============================================================
subroutine read_basin_map_from_tile__dble(&
    resl, id, var, dat, dtype, gxi, gyi, miss, bsn)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_map_from_tile__dble'
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: id
  character(*), intent(in)  :: var
  character(*), intent(in)  :: dtype
  integer     , intent(in)  :: gxi, gyi
  real(8)     , intent(out) :: dat(gxi:,gyi:)
  real(8)     , intent(in)  :: miss
  integer(4)  , intent(in), optional :: bsn(gxi:,gyi:)

  real(4) :: undef
  integer :: tx, ty
  integer :: xi, xf, yi, yf
  integer :: lgxi, lgxf, lgyi, lgyf
  integer :: n, i
  integer :: id_
  character :: c_
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  undef = miss - 1.d0
  dat(:,:) = undef

  open(newunit=un, file=get_f_lst_tile(resl, 'basin_domain'), status='old')
  read(un,*)
  do id_ = 1, intId(id)-1
    read(un,*) c_, n
    read(un,*)  ! bbox
    do i = 1, n
      read(un,*)
    enddo
  enddo
  read(un,*) c_, n
  read(un,*)  ! bbox
  do i = 1, n
    read(un,*) c_, tx, ty, xi, xf, yi, yf
    call xy_to_gxy(tx, ty, xi, yi, lgxi, lgyi)
    call xy_to_gxy(tx, ty, xf, yf, lgxf, lgyf)

    if( any(dat(lgxi:lgxf,lgyi:lgyf) /= undef) )then
      call errend(msg_unexpected_condition()//&
                  ' Data are overlapping.')
    endif

    call traperr( rbin(&
           dat(lgxi:lgxf,lgyi:lgyf), get_f_map_tile(resl,var,tx,ty), &
           dtype, sz=int((/NX,NY/),8), lb=int((/xi,yi/),8)) )
  enddo  ! i/
  close(un)

  if( present(bsn) )then
    where( bsn /= intId(id) ) dat = miss
  else
    where( bsn == undef ) dat = miss
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_map_from_tile__dble
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
character(CLEN_PATH) function get_f_map_tile(&
    resl, var, tx, ty) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_map_tile'
  character(*), intent(in) :: resl
  character(*), intent(in) :: var
  integer     , intent(in), optional :: tx, ty

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( var )

  case( 'dir', &
        'elv', &
        'upg', &
        'upa', &
        'wth' )
    if( resl == RESOLUTION_1SEC )then
      f = joined(DIR_ORG, trim(var)//'/'//tilename(tx,ty)//'_'//trim(var)//'.bin')
    else
      f = joined(DIR_TILED, trim(resl)//'/'//trim(var)//'/'//tilename(tx,ty)//'.bin')
    endif
  
  case( 'bsn_parts'     , &
        'bsn_tmp'       , &
        'bsn'           , &
        'bsn_final_lres', &
        'Jaccard'       , &
        'landuse'       )
    f = joined(DIR_TILED, trim(resl)//'/'//trim(var)//'/'//tilename(tx,ty)//'.bin')

  case( 'Jaccard_lres' )
    f = joined(DIR_TILED, trim(resl)//'/Jaccard/lres.bin')

  case default
    call errend(msg_invalid_value('var', var))
  endselect

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_map_tile
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_tile(&
    resl, var, tx, ty) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_tile'
  character(*), intent(in) :: resl
  character(*), intent(in) :: var
  integer     , intent(in), optional :: tx, ty

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( var )

  case( 'rivend'   , &
        'rivend_id', &
        'source'   , &
        'upper' )
    f = joined(DIR_TILED, trim(resl)//'/'//trim(var)//'/'//tilename(tx,ty)//'.txt')

  case( 'basin_domain' )
    if( present(tx) )then
      f = joined(DIR_TILED, trim(resl)//'/'//trim(var)//'/'//tilename(tx,ty)//'.txt')
    else
      f = joined(DIR_TILED, trim(resl)//'/'//trim(var)//'/all.txt')
    endif

  case( 'id_all', &
        'id_area' )
    f = joined(DIR_TILED, trim(resl)//'/'//trim(var)//'.txt')

  case default
    call errend(msg_invalid_value('var', var))
  endselect

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_tile
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_map_basin(&
    resl, var, bsnId) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_map_basin'
  character(*), intent(in) :: resl
  character(*), intent(in) :: var
  character(*), intent(in) :: bsnId

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  f = joined(DIR_BASIN, &
             trim(resl)//'/'//trim(var)//'/'//trim(bsnId)//'.bin')

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_map_basin
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_dat_basin(&
    resl, var, id) result(f)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_dat_basin'
  character(*), intent(in) :: resl
  character(*), intent(in) :: var
  character(*), intent(in) :: id

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( var )

  case( 'domain'      , &
        'river'       , &
        'channel'     , &
        'obs'         , &
        'channel_topo', &
        'riv_plt'     )
    f = joined(DIR_BASIN,&
               trim(resl)//'/'//trim(var)//'/'//trim(id)//'.txt')

  case default
    call errend(msg_invalid_value('var', var), &
                '', PRCNAM, MODNAM)
  endselect

  call traperr( mkdir(dirname(f)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_dat_basin
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_dir_bsnara_tile(&
    tx,ty) result(res)
  implicit none
  integer, intent(in) :: tx, ty

  res = joined(DIR_TILED, 'bsnara/'//tilename(tx,ty))
end function get_dir_bsnara_tile
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_all(resl, var) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_lst_all'
  character(*), intent(in) :: resl
  character(*), intent(in) :: var

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  selectcase( var )

  case( 'Jaccard' )
    res = joined(DIR_ALL, trim(resl)//'/'//trim(var)//'.txt')

  case default
    call errend(msg_invalid_value('var', var))
  endselect

  call traperr( mkdir(DIR_ALL) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_lst_all
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_dir_rt(resl_src, resl_tgt) result(res)
  implicit none
  character(*), intent(in) :: resl_src, resl_tgt

  res = joined(DIR_TILED, str(resl_tgt)//'/rt_from_jflw-'//str(resl_src))

  call traperr( mkdir(res) )
end function get_dir_rt
!===============================================================
!
!===============================================================
end module c2_jflw_io
