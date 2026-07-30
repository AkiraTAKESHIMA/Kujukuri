program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use def_consts
  use mod_utils, only: &
        read_conf_earth, &
        nextxy
  implicit none

  integer :: ix, iy, jx, jy, kx, ky
  integer :: dix, diy

  integer :: iXX, iYY, jXX,jYY
  integer :: nXX, nYY

  integer(1), allocatable :: dir(:,:)
  integer   , allocatable :: out_x(:,:)       !! ix             of outlet pixel
  integer   , allocatable :: out_y(:,:)       !! iy             of outlet pixel
  integer   , allocatable :: catmXX(:,:)      !! catchment(iXX,iYY) of pixel (ix,iy)
  integer   , allocatable :: catmYY(:,:)      !! 
  integer   , allocatable :: gcmx(:,:)        !! gcm grid (iXX,iYY) of hires pixel (ix,iy)
  integer   , allocatable :: gcmy(:,:)        !! 
  integer   , allocatable :: upg(:,:)

  integer   , allocatable ::  check(:,:)

  ! files
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: fgcmxy = 'gcmmap/gcmxy.bin'

  character(128), parameter :: rfile1 = 'tmp/1min/flwdir.bin'
  character(128), parameter :: rfile2 = 'tmp/map/out_xy.bin'
  character(128), parameter :: rfile3 = 'tmp/map/1min/catmxy.bin'
  character(128), parameter :: rfile4 = 'tmp/1min/upgrid.bin'

  character(128), parameter :: wfile1 = 'tmp/map/visual.bin'

  call logbgn('program visual_check', '', '+tr')
  !-------------------------------------------------------------
  ! Read params.
  !-------------------------------------------------------------
  call logmsg('Reading params '//str(fparams))
  open(11, file=fparams, status='old')

  read(11,*) nXX
  read(11,*) nYY
  read(11,*) ! fgcmidx

  !call read_conf_earth(earth, 11)

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(dir(nx,ny),upg(nx,ny))
  allocate(out_x(nXX,nYY),out_y(nXX,nYY))
  allocate(catmXX(nx,ny),catmYY(nx,ny))
  allocate(gcmx(nx,ny),gcmy(nx,ny))
  allocate(check(nx,ny))

  call traperr( rbin(dir, rfile1) )

  call traperr( rbin(out_x, rfile2, rec=1) )
  call traperr( rbin(out_y, rfile2, rec=2) )

  call traperr( rbin(catmXX, rfile3, rec=1) )
  call traperr( rbin(catmYY, rfile3, rec=2) )

  call traperr( rbin(upg, rfile4) )

  call traperr( rbin(gcmx, fgcmxy, rec=1) )
  call traperr( rbin(gcmy, fgcmxy, rec=2) )

  check(:,:)=-9999
  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)

        do dix=-3, 3
          do diy=-3, 3
            jx=ix+dix
            jy=iy+diy

            if( jx<1 .or. jx>nx .or. jy<1 .or. jy>ny ) cycle  ![ADD Takeshima 2023/02/13]

            jXX=gcmx(jx,jy)
            jYY=gcmy(jx,jy)
            if( jXX>iXX ) jx=jx-7
            if( jXX<iXX ) jx=jx+7
            if( jYY>iYY ) jy=jy-7
            if( jYY<iYY ) jy=jy+7

            if( jx>0 .and. jx<=nx .and. jy>0 .and. jy<=ny )then
              check(jx,jy)=5
            endif
          end do
        end do
      endif
    end do
  end do

  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
          ix=jx
          iy=jy
          do while( dir(jx,jy)>=1 .and. dir(jx,jy)<=8 .and. &
                    catmxx(ix,iy)==catmxx(jx,jy) .and. catmyy(ix,iy)==catmyy(jx,jy) )
            if( check(jx,jy)==-9999 )then
              check(jx,jy)=3
            endif
            call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
            jx=kx
            jy=ky
          end do
        endif
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( catmxx(ix,iy)<=0 .and. catmxx(ix,iy)/=-9999 .and. check(ix,iy)<=0 )     check(ix,iy)=-1
    end do
  end do

  do iy=1, ny-1
    do ix=1, nx-1
      if( check(ix,iy)==3 )then
        if( check(ix+1,iy+1)==-9999 ) check(ix+1,iy+1)=11
      endif

      if( check(ix,iy)==-9999 )then
        if( catmxx(ix,iy)/=catmxx(ix+1,iy) .or. catmyy(ix,iy)/=catmyy(ix+1,iy) )then
          check(ix,iy)=-2
          if( check(ix+1,iy+1)==-9999 ) check(ix+1,iy+1)=-2
        endif
        if( catmxx(ix,iy)/=catmxx(ix,iy+1) .or. catmyy(ix,iy)/=catmyy(ix,iy+1) )then
          check(ix,iy)=-2
          if( check(ix+1,iy+1)==-9999 ) check(ix+1,iy+1)=-2
        endif
      endif

      if( check(ix,iy)<0 )then
        if( upg(ix,iy)>1000 ) check(ix,iy)=1
      endif

      if( check(ix,iy)<0 )then
        if( gcmx(ix,iy)/=gcmx(ix+1,iy) .or. gcmy(ix,iy)/=gcmy(ix+1,iy) )then
          check(ix,iy)=-4
          if( check(ix+1,iy+1)==-9999 ) check(ix+1,iy+1)=-4
        endif
        if( gcmx(ix,iy)/=gcmx(ix,iy+1) .or. gcmy(ix,iy)/=gcmy(ix,iy+1) )then
          check(ix,iy)=-4
          if( check(ix+1,iy+1)==-9999 ) check(ix+1,iy+1)=-4
        endif
      endif

    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( check(ix,iy)==11 ) check(ix,iy)=3
    end do
  end do

  call traperr( wbin(check, wfile1) )
  !-------------------------------------------------------------
  call logret()
end program main
