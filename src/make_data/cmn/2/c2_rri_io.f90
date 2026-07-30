module c2_rri_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c1_const
  use c2_rri_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
!  public :: get_f_basin_range
!  public :: get_f_topo

  public :: read_basin_range
  public :: write_basin_range
  public :: read_topo
  public :: write_topo
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_topo
    module procedure read_topo__int4
    module procedure read_topo__real
  end interface

  interface write_topo
    module procedure write_topo__int1
    module procedure write_topo__int4
    module procedure write_topo__real
    module procedure write_topo__dble
  end interface
  !-------------------------------------------------------------
  ! Private module procedure
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rri_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_basin_range(&
    resolution, bsnId) result(res)
  implicit none
  character(*), intent(in) :: resolution
  integer     , intent(in) :: bsnId

  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_basin_range'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  res = joined(DIR_RRI,'range/'//str(resolution)//'/'//&
               str(bsnId,-DGT_BSNID_MAX)//'.txt')

  call traperr( mkdir(dirname(res)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_basin_range
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_topo(&
    resolution, varname, bsnId, ext) result(res)
  implicit none
  character(*), intent(in) :: resolution
  character(*), intent(in) :: varname
  integer     , intent(in) :: bsnId
  character(*), intent(in) :: ext

  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_topo'

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  res = joined(DIR_TOPO, str(resolution)//'/'//str(bsnId,-DGT_BSNID_MAX)//&
               '/'//str(varname)//'.'//str(ext))

  call traperr( mkdir(dirname(res)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_topo
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
subroutine read_basin_range(&
    resolution, bsnId, &
    gxi, gxf, gyi, gyf, west, east, south, north)
  use c2_rri_grid, only: &
        ratio_resolution
  implicit none
  character(*), intent(in)  :: resolution
  integer     , intent(in)  :: bsnId
  integer     , intent(out) :: gxi, gxf, gyi, gyf
  real(8)     , intent(out) :: west, east, south, north

  character :: c_
  integer :: n
  character(CLEN_PATH) :: f
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_range'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_basin_range(RESOLUTION_1SEC,bsnId)
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*)
  read(un,*)
  read(un,*) c_, gxi, gxf
  read(un,*) c_, gyi, gyf
  read(un,*) c_, west
  read(un,*) c_, east
  read(un,*) c_, south
  read(un,*) c_, north
  close(un)

  call logmsg('Grid indices are converted from '//&
              str(RESOLUTION_1SEC)//' to '//str(resolution)//'.')
  n = ratio_resolution(resolution)
  if( mod(gxi-1,n) /= 0 .or. mod(gxf,n) /= 0 .or. &
      mod(gyi-1,n) /= 0 .or. mod(gyf,n) /= 0 )then
    call errend('Invalid value in $gxi, $gxf, $gyi or $gyf.')
  endif
  gxi = (gxi-1) / n + 1
  gxf = gxf / n
  gyi = (gyi-1) / n + 1
  gyf = gyf / n
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_range
!===============================================================
!
!===============================================================
subroutine write_basin_range(&
    bsnId, &
    gxi, gxf, gyi, gyf, west, east, south, north)
  implicit none
  integer, intent(in) :: bsnId
  integer, intent(in) :: gxi, gxf, gyi, gyf
  real(8), intent(in) :: west, east, south, north

  integer :: mgx, mgy
  character(CLEN_PATH) :: f
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'write_basin_range'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  mgx = gxf - gxi + 1
  mgy = gyf - gyi + 1

  f = get_f_basin_range(RESOLUTION_1SEC,bsnId)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(a)") 'nx '//str(mgx,dgt(max(mgx,mgy)))
  write(un,"(a)") 'ny '//str(mgy,dgt(max(mgx,mgy)))
  write(un,"(a)") 'gx '//str((/gxi,gxf/),dgt(max(gxf,gyf)),' ')
  write(un,"(a)") 'gy '//str((/gyi,gyf/),dgt(max(gxf,gyf)),' ')
  write(un,"(a)") 'west  '//str(west,'f20.15')
  write(un,"(a)") 'east  '//str(east,'f20.15')
  write(un,"(a)") 'south '//str(south,'f20.15')
  write(un,"(a)") 'north '//str(north,'f20.15')
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_basin_range
!===============================================================
!
!===============================================================
subroutine read_topo__int4(dat, miss, resolution, varname, bsnId)
  implicit none
  integer(4)  , intent(out) :: dat(:,:)
  integer(4)  , intent(out) :: miss
  character(*), intent(in)  :: resolution
  character(*), intent(in)  :: varname
  integer     , intent(in)  :: bsnId

  integer :: ngx, ngy, igy
  character :: c_
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'read_topo__int4'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_topo(resolution, varname, bsnId, 'txt'), status='old')
  read(un,*) c_, ngx
  read(un,*) c_, ngy
  if( size(dat,1) /= ngx .or. size(dat,2) /= ngy )then
    call errend('Incorrect shape of array.')
  endif
  read(un,*)
  read(un,*)
  read(un,*)
  read(un,*) c_, miss
  do igy = 1, ngy
    read(un,*) dat(:,igy)
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_topo__int4
!===============================================================
!
!===============================================================
subroutine read_topo__real(dat, miss, resolution, varname, bsnId)
  implicit none
  real(4)     , intent(out) :: dat(:,:)
  real(4)     , intent(out) :: miss
  character(*), intent(in)  :: resolution
  character(*), intent(in)  :: varname
  integer     , intent(in)  :: bsnId

  integer :: ngx, ngy, igy
  character :: c_
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'read_topo__int4'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_topo(resolution, varname, bsnId, 'txt'), status='old')
  read(un,*) c_, ngx
  read(un,*) c_, ngy
  if( size(dat,1) /= ngx .or. size(dat,2) /= ngy )then
    call errend('Incorrect shape of array.')
  endif
  read(un,*)
  read(un,*)
  read(un,*)
  read(un,*) c_, miss
  do igy = 1, ngy
    read(un,*) dat(:,igy)
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_topo__real
!===============================================================
!
!===============================================================
subroutine write_topo__int1(&
    dat, miss, resolution, varname, bsnId, &
    west, south, cellsize)
  implicit none
  integer(1)  , intent(in) :: dat(:,:)
  integer(1)  , intent(in) :: miss
  character(*), intent(in) :: resolution
  character(*), intent(in) :: varname
  integer     , intent(in) :: bsnId
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'write_topo__int4'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_topo(resolution, varname, bsnId, 'txt'), status='replace')
  write(un,"(a)") 'ncols '//str(size(dat,1))
  write(un,"(a)") 'nrows '//str(size(dat,2))
  write(un,"(a)") 'xllcorner '//str(west,'f20.15')
  write(un,"(a)") 'yllcorner '//str(south,'f20.15')
  write(un,"(a)") 'cellsize  '//str(cellsize,'f20.15')
  write(un,"(a)") 'NODATA_value '//str(miss)
  do iy = 1, size(dat,2)
    write(un,"(a)") str(dat(:,iy))
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_topo__int1
!===============================================================
!
!===============================================================
subroutine write_topo__int4(&
    dat, miss, resolution, varname, bsnId, &
    west, south, cellsize)
  implicit none
  integer(4)  , intent(in) :: dat(:,:)
  integer(4)  , intent(in) :: miss
  character(*), intent(in) :: resolution
  character(*), intent(in) :: varname
  integer     , intent(in) :: bsnId
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'write_topo__int4'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_topo(resolution, varname, bsnId, 'txt'), status='replace')
  write(un,"(a)") 'ncols '//str(size(dat,1))
  write(un,"(a)") 'nrows '//str(size(dat,2))
  write(un,"(a)") 'xllcorner '//str(west,'f20.15')
  write(un,"(a)") 'yllcorner '//str(south,'f20.15')
  write(un,"(a)") 'cellsize  '//str(cellsize,'f20.15')
  write(un,"(a)") 'NODATA_value '//str(miss)
  do iy = 1, size(dat,2)
    write(un,"(a)") str(dat(:,iy))
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_topo__int4
!===============================================================
!
!===============================================================
subroutine write_topo__real(&
    dat, miss, resolution, varname, bsnId, &
    west, south, cellsize)
  implicit none
  real(4)     , intent(in) :: dat(:,:)
  real(4)     , intent(in) :: miss
  character(*), intent(in) :: resolution
  character(*), intent(in) :: varname
  integer     , intent(in) :: bsnId
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'write_topo__real'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_topo(resolution, varname, bsnId, 'txt'), status='replace')
  write(un,"(a)") 'ncols '//str(size(dat,1))
  write(un,"(a)") 'nrows '//str(size(dat,2))
  write(un,"(a)") 'xllcorner '//str(west,'f20.15')
  write(un,"(a)") 'yllcorner '//str(south,'f20.15')
  write(un,"(a)") 'cellsize  '//str(cellsize,'f20.15')
  write(un,"(a)") 'NODATA_value '//str(miss)
  do iy = 1, size(dat,2)
    write(un,"(a)") str(dat(:,iy),'f10.3')
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_topo__real
!===============================================================
!
!===============================================================
subroutine write_topo__dble(&
    dat, miss, resolution, varname, bsnId, &
    west, south, cellsize)
  implicit none
  real(8)     , intent(in) :: dat(:,:)
  real(8)     , intent(in) :: miss
  character(*), intent(in) :: resolution
  character(*), intent(in) :: varname
  integer     , intent(in) :: bsnId
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  integer :: un

  character(CLEN_PROC), parameter :: PRCNAM = 'write_topo__dble'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=get_f_topo(resolution, varname, bsnId, 'txt'), status='replace')
  write(un,"(a)") 'ncols '//str(size(dat,1))
  write(un,"(a)") 'nrows '//str(size(dat,2))
  write(un,"(a)") 'xllcorner '//str(west,'f20.15')
  write(un,"(a)") 'yllcorner '//str(south,'f20.15')
  write(un,"(a)") 'cellsize  '//str(cellsize,'f20.15')
  write(un,"(a)") 'NODATA_value '//str(miss)
  do iy = 1, size(dat,2)
    write(un,"(a)") str(dat(:,iy),'es12.5')
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_topo__dble
!===============================================================
!
!===============================================================
end module c2_rri_io
