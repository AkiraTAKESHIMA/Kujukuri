program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_math
  use c1_type_opt, only: &
        opt_earth_
  use def_consts
  use mod_utils, only: &
        read_conf_earth, &
        nextxy
  implicit none

  integer :: ix, iy
  integer :: xs, xe, ys, ye

  integer :: nXX, nYY
  integer :: iXX, iYY

  integer :: idx

  integer   , allocatable :: gcmidx(:,:)
  integer   , allocatable :: gcmx(:,:), gcmy(:,:)
  integer(1), allocatable :: lmask(:,:)
  integer   , allocatable :: pixnum(:,:)
  integer   , allocatable :: lndnum(:,:)
  real(8)   , allocatable :: grdare(:,:)
  real(8)   , allocatable :: lndare(:,:)
  real(8)   , allocatable :: grlndf(:,:)
  real(8)   , allocatable :: grlon(:,:), grlat(:,:)
  real(8)   , allocatable :: pixlon(:), pixlat(:)
  real(8)   , allocatable :: pixare(:)

  type(opt_earth_) :: earth

  ! input
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: flmask  = '1min_flwdir/lndmsk.bin'
  character(128) :: fgcmidx

  ! output
  character(128), parameter :: fgcmxy    = 'gcmmap/gcmxy.bin'
  character(128), parameter :: fpixnum   = 'gcmmap/pixnum.bin'
  character(128), parameter :: flndare   = 'gcmmap/lndare.bin'
  character(128), parameter :: fgrlndf   = 'gcmmap/grlndf.bin'
  character(128), parameter :: fgrlonlat = 'gcmmap/grlonlat.bin'

  call logbgn('program make_gcmmap', '', '+tr')
  !-------------------------------------------------------------
  ! Read params.
  !-------------------------------------------------------------
  call logmsg('Reading params '//str(fparams))
  open(11, file=fparams, status='old')

  read(11,*) nXX
  read(11,*) nYY
  read(11,*) fgcmidx

  call read_conf_earth(earth, 11)

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(gcmx(nx,ny))
  allocate(gcmy(nx,ny))
  allocate(lmask(nx,ny))
  allocate(pixnum(nXX,nYY))
  allocate(lndnum(nXX,nYY))
  allocate(grdare(nXX,nYY))
  allocate(lndare(nXX,nYY))
  allocate(grlndf(nXX,nYY))
  allocate(grlon(nXX,nYY))
  allocate(grlat(nXX,nYY))

  allocate(pixlon(0:nx))
  allocate(pixlat(0:ny))
  allocate(pixare(ny))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating gcmx and gcmy')

  if( fgcmidx == '' )then
    do iYY = 1, nYY
      ys = ny/nYY * (iYY-1) + 1
      ye = ny/nYY * iYY

      do iXX = 1, nXX
        xs = nx/nXX * (iXX-1) + 1
        xe = nx/nXX * iXX

        gcmx(xs:xe,ys:ye) = iXX
        gcmy(xs:xe,ys:ye) = iYY
      enddo
    enddo
  else
    allocate(gcmidx(nx,ny))
    call traperr( rbin(gcmidx, fgcmidx) )
    call logmsg('idx min: '//str(minval(gcmidx))//', max: '//str(maxval(gcmidx)))

    do iy = 1, ny
      do ix = 1, nx
        idx = gcmidx(ix,iy)
        gcmx(ix,iy) = mod(idx-1,nXX) + 1
        gcmy(ix,iy) = (idx-1)/nXX + 1
      enddo
    enddo

    deallocate(gcmidx)
  endif

  call logmsg('x: '//str(minval(gcmx))//' - '//str(maxval(gcmx)))
  call logmsg('y: '//str(minval(gcmy))//' - '//str(maxval(gcmy)))

  call logmsg('Writing gcmxy '//str(fgcmxy))
  call traperr( wbin(gcmx, fgcmxy, rec=1) )
  call traperr( wbin(gcmy, fgcmxy, rec=2) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating pixel lonlat')

  do ix = 0, nx
    pixlon(ix) = -rad_180deg + rad_360deg*(dble(ix)/nx)
  enddo
  pixlon(nx) = rad_180deg

  do iy = 0, ny
    pixlat(iy) = rad_90deg - rad_180deg*(dble(iy)/ny)
  enddo
  pixlat(ny) = -rad_90deg

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating pixel area')

  selectcase( earth%shptyp )
  case( EARTH_SHPTYP__SPHERE )
    pixare(:) = area_sphere_rect(pixlat(0:ny-1), pixlat(1:ny)) * rad_360deg/nx
  case( EARTH_SHPTYP__ELLIPS )
    pixare(:) = area_ellips_rect(pixlat(0:ny-1), pixlat(1:ny), earth%e2) * rad_360deg/nx
  case default
    call errend(msg_invalid_value('earth%shptyp', earth%shptyp))
  endselect

  pixare(:) = pixare(:) * earth%r**2
  
  call logmsg('surface area: '//str(sum(pixare)*nx*1d-6)//' km2')

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Reading land mask '//trim(flmask))
  call traperr( rbin(lmask, flmask) )

  pixnum(:,:) = 0
  lndnum(:,:) = 0
  grdare(:,:) = 0.d0
  lndare(:,:) = 0.d0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating pixel number, land area and land fraction')

  do iy = 1, ny
    do ix = 1, nx
      iXX = gcmx(ix,iy)
      iYY = gcmy(ix,iy)
      grdare(iXX,iYY) = grdare(iXX,iYY) + pixare(iy)
      pixnum(iXX,iYY) = pixnum(iXX,iYY) + 1
      if( lmask(ix,iy) == 1 )then
        lndare(iXX,iYY) = lndare(iXX,iYY) + pixare(iy)
        lndnum(iXX,iYY) = lndnum(iXX,iYY) + 1
      endif
    enddo
  enddo
  call logmsg('Total grid area '//str(sum(grdare)*1d-6)//' km2')
  call logmsg('Total land area '//str(sum(lndare)*1d-6)//' km2')

  do iYY = 1, nYY
    do iXX = 1, nXX
      if( lndnum(iXX,iYY) == pixnum(iXX,iYY) )then
        grlndf(iXX,iYY) = 1.d0
      else
        grlndf(iXX,iYY) = min(1.d0, lndare(iXX,iYY) / grdare(iXX,iYY))
      endif
    enddo
  enddo
  call logmsg('pixnum min '//str(minval(pixnum))//' max '//str(maxval(pixnum)))
  call logmsg('grlndf min '//str(minval(grlndf))//' max '//str(maxval(grlndf)))

  call logmsg('Writing pixel number  '//str(fpixnum))
  call traperr( wbin(pixnum, fpixnum) )

  call logmsg('Writing land area     '//str(flndare))
  call traperr( wbin(lndare, flndare) )

  call logmsg('Writing land fraction '//str(fgrlndf))
  call traperr( wbin(grlndf, fgrlndf) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating grid lonlat')

  grlon(:,:) = 0.0
  grlat(:,:) = 0.0

  do iy = 1, ny
    do ix = 1, nx
      iXX = gcmx(ix,iy)
      iYY = gcmy(ix,iy)
      grlon(iXX,iYY) = grlon(iXX,iYY) + (pixlon(ix-1)+pixlon(ix))*0.5d0 * (pixare(iy)/grdare(iXX,iYY))
      grlat(iXX,iYY) = grlat(iXX,iYY) + (pixlat(iy-1)+pixlat(iy))*0.5d0 * (pixare(iy)/grdare(iXX,iYY))
    enddo
  enddo
  grlon(:,:) = grlon(:,:) * d2r
  grlat(:,:) = grlat(:,:) * d2r

  call logmsg('grlon min '//str(minval(grlon))//' max '//str(maxval(grlon)))
  call logmsg('grlat min '//str(minval(grlat))//' max '//str(maxval(grlat)))

  call logmsg('Writing grid lonlat '//trim(fgrlonlat))
  call traperr( wbin(grlon, fgrlonlat, rec=1) )
  call traperr( wbin(grlat, fgrlonlat, rec=2) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(pixlon)
  deallocate(pixlat)
  deallocate(pixare)

  deallocate(gcmx)
  deallocate(gcmy)
  deallocate(lmask)
  deallocate(pixnum)
  deallocate(lndnum)
  deallocate(grdare)
  deallocate(lndare)
  deallocate(grlndf)

  deallocate(grlon)
  deallocate(grlat)
  !-------------------------------------------------------------
  call logret()
end program main
