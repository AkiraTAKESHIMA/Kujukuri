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
  public :: get_f_data

  public :: read_basin_domain
  public :: write_basin_domain
  public :: read_map
  public :: write_map
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_map
    module procedure read_map__int4
    module procedure read_map__real
  end interface

  interface write_map
    module procedure write_map__int1
    module procedure write_map__int4
    module procedure write_map__real
    module procedure write_map__dble
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
character(CLEN_PATH) function get_f_data(&
  basinType, resl, varName, bsnId &
) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_data'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: bsnId

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  res = joined(DIR_RRI, str(basinType)//'/'//str(resl)//&
          '/'//str(bsnId,-DGT_BSNID_MAX)//'/'//str(varName)//'.txt')

  call traperr( mkdir(dirname(res)) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function get_f_data
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
subroutine read_basin_domain(&
  basinType, resl, bsnId, &
  gxi, gxf, gyi, gyf, west, east, south, north &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_domain'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: bsnId
  integer     , intent(out) :: gxi, gxf, gyi, gyf
  real(8)     , intent(out) :: west, east, south, north

  character :: c_
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_data(basinType, resl, 'domain', bsnId)
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
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_domain
!===============================================================
!
!===============================================================
subroutine write_basin_domain(&
  basinType, resl, bsnId, &
  gxi, gxf, gyi, gyf, west, east, south, north &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_basin_domain'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: bsnId
  integer, intent(in) :: gxi, gxf, gyi, gyf
  real(8), intent(in) :: west, east, south, north

  integer :: mgx, mgy
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  mgx = gxf - gxi + 1
  mgy = gyf - gyi + 1

  f = get_f_data(basinType, resl, 'domain', bsnId)
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
end subroutine write_basin_domain
!===============================================================
!
!===============================================================
subroutine read_map__int4(&
  basinType, resl, varName, bsnId, &
  dat, miss &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_map__int4'
  character(*), intent(in)  :: basinType
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: varName
  character(*), intent(in)  :: bsnId
  integer(4)  , intent(out) :: dat(:,:)
  integer(4)  , intent(out) :: miss

  integer :: ngx, ngy, igy
  character :: c_
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_data(basinType, resl, varName, bsnId)
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
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
end subroutine read_map__int4
!===============================================================
!
!===============================================================
subroutine read_map__real(&
  basinType, resl, varName, bsnId, &
  dat, miss &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_map__int4'
  character(*), intent(in)  :: basinType
  character(*), intent(in)  :: resl
  character(*), intent(in)  :: varName
  character(*), intent(in)  :: bsnId
  real(4)     , intent(out) :: dat(:,:)
  real(4)     , intent(out) :: miss

  integer :: ngx, ngy, igy
  character :: c_
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_data(basinType, resl, varName, bsnId)
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
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
end subroutine read_map__real
!===============================================================
!
!===============================================================
subroutine write_map__int1(&
  basinType, resl, varName, bsnId, &
  dat, miss, west, south, cellsize &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_map__int4'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: bsnId
  integer(1)  , intent(in) :: dat(:,:)
  integer(1)  , intent(in) :: miss
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_data(basinType, resl, varName, bsnId)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
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
end subroutine write_map__int1
!===============================================================
!
!===============================================================
subroutine write_map__int4(&
  basinType, resl, varName, bsnId, &
  dat, miss, west, south, cellsize &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_map__int4'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: bsnId
  integer(4)  , intent(in) :: dat(:,:)
  integer(4)  , intent(in) :: miss
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_data(basinType, resl, varName, bsnId)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
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
end subroutine write_map__int4
!===============================================================
!
!===============================================================
subroutine write_map__real(&
  basinType, resl, varName, bsnId, &
  dat, miss, west, south, cellsize &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_map__real'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: bsnId
  real(4)     , intent(in) :: dat(:,:)
  real(4)     , intent(in) :: miss
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_data(basinType, resl, varName, bsnId)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
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
end subroutine write_map__real
!===============================================================
!
!===============================================================
subroutine write_map__dble(&
  basinType, resl, varName, bsnId, &
  dat, miss, west, south, cellsize &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_map__dble'
  character(*), intent(in) :: basinType
  character(*), intent(in) :: resl
  character(*), intent(in) :: varName
  character(*), intent(in) :: bsnId
  real(8)     , intent(in) :: dat(:,:)
  real(8)     , intent(in) :: miss
  real(8)     , intent(in) :: west, south, cellsize

  integer :: iy
  character(CLEN_PATH) :: f
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_data(basinType, resl, varName, bsnId)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
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
end subroutine write_map__dble
!===============================================================
!
!===============================================================
end module c2_rri_io
