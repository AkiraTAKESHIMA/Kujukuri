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

  integer :: ix, iy, jx, jy, kx, ky

  integer(1), allocatable :: flwdir(:,:)
  real(8)   , allocatable :: upa(:,:)
  real(4)   , allocatable :: upg(:,:)

  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixare(:)

  integer(1), allocatable:: upgrid(:,:), upnow(:,:)

  integer              :: isAgain
  integer              :: iseq, jseq, nseq
  integer,allocatable  :: seqx(:), seqy(:)

  type(opt_earth_) :: earth

  ! input files
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: rfile1 = 'tmp/1min/flwdir.bin'

  ! output files
  character(128), parameter :: wfile1 = 'tmp/1min/uparea.bin'
  character(128), parameter :: wfile2 = 'tmp/1min/upgrid.bin'

  call logbgn('program calc_uparea', '', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Reading params '//str(fparams))
  open(11, file=fparams, status='old')

  read(11,*) ! nXX
  read(11,*) ! nYY
  read(11,*) ! fgcmidx

  call read_conf_earth(earth, 11)

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(flwdir(nx,ny))
  allocate(upa(nx,ny))
  allocate(upg(nx,ny))

  allocate(upgrid(nx,ny))
  allocate(upnow(nx,ny))
  allocate(pixare(ny))

  allocate(seqx(nx*ny))
  allocate(seqy(nx*ny))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating pixel area')

  allocate(pixlat(0:ny))

  do iy = 0, ny
    pixlat(iy) = rad_90deg - rad_180deg * (dble(iy)/ny)
  enddo
  pixlat(ny) = -rad_90deg

  selectcase( earth%shptyp )
  case( EARTH_SHPTYP__SPHERE )
    pixare(:) = area_sphere_rect(pixlat(0:ny-1), pixlat(1:ny)) * rad_360deg/nx
  case( EARTH_SHPTYP__ELLIPS )
    pixare(:) = area_ellips_rect(pixlat(0:ny-1), pixlat(1:ny), earth%e2) * rad_360deg/nx
  case default
    call errend(msg_invalid_value('earth%shptyp', earth%shptyp))
  endselect

  pixare(:) = pixare(:) * earth%r**2

  deallocate(pixlat)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating uparea and upgrid')

  call traperr( rbin(flwdir, rfile1) )

  upgrid(:,:)=0
  upnow(:,:)=0

  do iy=1, ny
    do ix=1, nx
      if( flwdir(ix,iy)>0 )then
        call nextxy(ix,iy,jx,jy,nx,flwdir(ix,iy))
        upgrid(jx,jy)=upgrid(jx,jy)+1_1
      endif
    end do
  end do

  upa(:,:)=0
  upg(:,:)=0
  do iy=1, ny
    do ix=1, nx
      if( flwdir(ix,iy)==-9 )then
        upa(ix,iy)=-9999
        upg(ix,iy)=-9999
        upgrid(ix,iy)=-9_1
      endif
    end do
  end do

  seqx(:)=0
  seqy(:)=0

  isAgain=0
  jseq=0
  do iy=1, ny
    do ix=1, nx
      if( upgrid(ix,iy)==0 )then
        upa(ix,iy)=upa(ix,iy)+pixare(iy)
        upg(ix,iy)=upg(ix,iy)+1
        if( flwdir(ix,iy)>0 )then
          call nextxy(ix,iy,jx,jy,nx,flwdir(ix,iy))
          upa(jx,jy)=upa(jx,jy)+upa(ix,iy)
          upg(jx,jy)=upg(jx,jy)+upg(ix,iy)
          upnow(jx,jy)=upnow(jx,jy)+1_1
          if( upnow(jx,jy)==upgrid(jx,jy) )then
            jseq=jseq+1
            isAgain=isAgain+1
            seqx(jseq)=jx
            seqy(jseq)=jy
          endif
        endif
      endif
    end do
  end do

  nseq=jseq
  do while( isAgain>0 )
    isAgain=0
    jseq=0
    do iseq=1, nseq
      jx=seqx(iseq)
      jy=seqy(iseq)
      upa(jx,jy)=upa(jx,jy)+pixare(jy)
      upg(jx,jy)=upg(jx,jy)+1
      if( flwdir(jx,jy)>0 )then
        call nextxy(jx,jy,kx,ky,nx,flwdir(jx,jy))
        upa(kx,ky)=upa(kx,ky)+upa(jx,jy)
        upg(kx,ky)=upg(kx,ky)+upg(jx,jy)
        upnow(kx,ky)=upnow(kx,ky)+1_1
        if( upnow(kx,ky)==upgrid(kx,ky) )then
          jseq=jseq+1
          isAgain=1
          seqx(jseq)=kx
          seqy(jseq)=ky
        endif
      endif
    end do
    nseq=jseq
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Writing uparea '//str(wfile1))
  call traperr( wbin(upa, wfile1) )

  call logmsg('Writing upgrid '//str(wfile2))
  call traperr( wbin(upg, wfile2) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(seqx)
  deallocate(seqy)

  deallocate(upgrid)
  deallocate(upnow)
  deallocate(pixare)

  deallocate(flwdir)
  deallocate(upa)
  deallocate(upg)
  !-------------------------------------------------------------
  call logret()
end program main
