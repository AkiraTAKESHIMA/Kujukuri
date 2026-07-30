program main
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use def_consts
  use mod_utils, only: &
        read_conf_earth, &
        nextxy
  implicit none

  ! high-res flow direction map grid
  integer :: ix, iy, jx, jy, kx, ky

  ! low-res GCM map grid
  integer ::  nXX, nYY                        !! river network domain (nXX,nYY)
  integer ::  iXX, iYY, jXX, jYY
  character(128), parameter :: fgcmxy  = 'gcmmap/gcmxy.bin'
  character(128), parameter :: fgrlndf = 'gcmmap/grlndf.bin'
  character(128), parameter :: fpixnum = 'gcmmap/pixnum.bin'

  ! parameters
  real(8), parameter :: dst_mth = 10.d3   !! downstream distance at river mouth [m]
  real(8), parameter :: len_top = 5.d3    !! channel length at topmost grid     [m]

  ! input GCM
  real(8), allocatable :: grlndf(:,:)     !! land fraction (0-1)
  integer, allocatable :: gcmx(:,:)       !! gcm grid (iXX,iYY) of hires pixel (ix,iy)
  integer, allocatable :: gcmy(:,:)       !! 
  integer, allocatable :: pixnum(:,:)     !! # of pixels in grid (iXX,iYY)

  ! input hires
  integer(1), allocatable :: dir(:,:)        !! flow direction (1-8: dir, 0:mouth, -1:inland, -9:ocean)
  integer(1), allocatable :: inland(:,:)     !! 1:river to ocean, 2:inland river, 0:ocean
  real(8)   , allocatable :: upa(:,:)        !! drainage area of hires pixel   [km2]
  real(8)   , allocatable :: upg(:,:)        !! drainage pixels of hires pixel [pixel]
  real(8)   , allocatable :: lon(:), lat(:)  !! lon/lat of hires pixel (center)
  real(8)   , allocatable :: elv(:,:)        !! elvation of hires pixel [m]

  ! 2d params
  !! minimum dst to next grid     [pixel]    ( pixnum^0.5 ) * 0.35
  real(8), allocatable :: dst_min(:,:)
  !! minimum upa for outlet pixel [pixels]   ( pixnum ) * 0.2
  real(8), allocatable :: upg_min(:,:) 

  ! output river map
  integer, allocatable :: nextXX(:,:)     !! downstream (jXX,jYY) of grid (iXX,iYY)
  integer, allocatable :: nextYY(:,:)     !! 
  real(8), allocatable :: nxtdst(:,:)     !! downstrem distance   [m]
  real(8), allocatable :: rivlen(:,:)     !! river channel length [m]
  real(8), allocatable :: out_elv(:,:)    !! elvation      of outlet pixel [m]

  real(8), allocatable :: out_lon(:,:)    !! lon            of outlet pixel
  real(8), allocatable :: out_lat(:,:)    !! lat            of outlet pixel
  real(8), allocatable :: out_upa(:,:)    !! drainage area  of outlet pixel [m2]

  integer, allocatable :: out_x(:,:)      !! outlet ix      of outlet pixel
  integer, allocatable :: out_y(:,:)      !! outlet iy      of outlet pixel

  ! local (low-res)
  integer, allocatable :: out_num(:,:)    !! #    of potential outlet pixel
  real(8), allocatable :: upg_max(:,:)    !! maximum upa

  ! local (high-res)
  integer, allocatable :: outlet(:,:)     !! 0:potential, 1:outlet, 2:not outlet
  integer, allocatable :: out_XX(:,:)     !! iXX of outlet pixel
  integer, allocatable :: out_YY(:,:)     !! iYY of outlet pixel

  ! local upgrid
  integer(1), allocatable :: locdir(:,:)
  real(8)   , allocatable :: locupg(:,:)  !! local drainage grid
  real(8)   , allocatable :: tmpupg(:,:)

  ! local sort
  integer                 :: iseq, nseq      !! used for heap sort
  real(8)   , allocatable :: sort_area(:,:)
  integer(8), allocatable :: arg(:)

  ! local etc
  real(8)                 :: dst           !! river channel length to downstream
  real(8)                 :: area_this     !! upstream area
  integer                 :: again, step   !! calculation repeat?

  ! files
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: rfile1 = 'tmp/1min/flwdir.bin'
  character(128), parameter :: rfile2 = 'tmp/1min/uparea.bin'
  character(128), parameter :: rfile3 = 'tmp/1min/elevtn.bin'
  character(128), parameter :: rfile4 = 'tmp/1min/upgrid.bin'
  character(128), parameter :: rfile5 = 'tmp/1min/inland.bin'

  character(128), parameter :: wfile1 = 'tmp/map/out_xy.bin'
  character(128), parameter :: wfile2 = 'tmp/map/uparea.bin'
  character(128), parameter :: wfile3 = 'tmp/map/lonlat.bin'
  character(128), parameter :: wfile4 = 'tmp/map/elevtn.bin'
  character(128), parameter :: wfile5 = 'tmp/map/nextxy.bin'
  character(128), parameter :: wfile6 = 'tmp/map/nxtdst.bin'
  character(128), parameter :: wfile7 = 'tmp/map/rivlen.bin'

  call logbgn('program const_network', '', '+tr')
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
  ! GCM
  allocate(grlndf(nXX,nYY),gcmx(nx,ny),gcmy(nx,ny),pixnum(nXX,nYY))

  ! hires
  allocate(dir(nx,ny),inland(nx,ny))
  allocate(upa(nx,ny),elv(nx,ny),upg(nx,ny))
  allocate(lon(nx),lat(ny))

  ! map
  allocate(nextXX(nXX,nYY),nextYY(nXX,nYY),nxtdst(nXX,nYY),rivlen(nXX,nYY))
  allocate(out_lon(nXX,nYY),out_lat(nXX,nYY),out_elv(nXX,nYY),out_upa(nXX,nYY))

  ! param
  allocate(dst_min(nXX,nYY),upg_min(nXX,nYY))

  ! local
  allocate(outlet(nx,ny),out_XX(nx,ny),out_YY(nx,ny))
  allocate(out_x(nXX,nYY),  out_y(nXX,nYY),  out_num(nXX,nYY),upg_max(nXX,nYY))

  allocate(locdir(nx,ny),locupg(nx,ny))

  !sort
  nseq=nXX*nYY
  allocate(sort_area(nseq,3))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Reading files')

  call logmsg('Reading land fraction in GCM grid '//str(fgrlndf))
  call traperr( rbin(grlndf, fgrlndf) )

  call logmsg('Reading pixel number in GCM grid  '//str(fpixnum))
  call traperr( rbin(pixnum, fpixnum) )

  call logmsg('Reading pixel of GCM grid index x '//str(fgcmxy))
  call logmsg('Reading pixel of GCM grid index y '//str(fgcmxy))
  call traperr( rbin(gcmx, fgcmxy, rec=1) )
  call traperr( rbin(gcmy, fgcmxy, rec=2) )

  call logmsg('Reading flow direction '//str(rfile1))
  call traperr( rbin(dir, rfile1) )

  call logmsg('Reading drainage area  '//str(rfile2))
  call traperr( rbin(upa, rfile2) )

  call logmsg('Reading elevation      '//str(rfile3))
  call traperr( rbin(elv, rfile3) )

  call logmsg('Reading drainage pixel '//str(rfile4))
  call traperr( rbin(upg, rfile4, dtype=dtype_real) )

  call logmsg('Reading inland pixel   '//str(rfile5))
  call traperr( rbin(inland, rfile5) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Setting lonlat')

  do ix=1, nx
    lon(ix)=-180.d0 + (dble(ix)-0.5d0) * (360.d0/nx)
  end do
  do iy=1, ny
    lat(iy)=  90.d0 - (dble(iy)-0.5d0) * (180.d0/ny)
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Setting threshold')

  do iYY=1, nYY
    do iXX=1, nXX
      dst_min(iXX,iYY)=pixnum(iXX,iYY)**0.5 * 0.35
      upg_min(iXX,iYY)=pixnum(iXX,iYY)      * 0.2
    end do
  end do

  call logmsg('dst_min '//str(dst_min(nXX/2,nYY/2)))
  call logmsg('upg_min '//str(upg_min(nXX/2,nYY/2)))

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iy=1, ny
    do ix=1, nx
      if( inland(ix,iy)==2 ) upg(ix,iy)=upg(ix,iy)*0.1
    end do
  end do
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating local drainage area')

  locdir(:,:)=dir(:,:)
  do iy=1, ny
    do ix=1, nx
      iXX=gcmx(ix,iy)
      iYY=gcmy(ix,iy)
      if( locdir(ix,iy)>=1 .and. locdir(ix,iy)<=8 .and. upg(ix,iy)>upg_min(iXX,iYY)*0.5 )then
        call nextxy(ix,iy,jx,jy,nx,locdir(ix,iy))
        jXX=gcmx(jx,jy)
        jYY=gcmy(jx,jy)
        if( (iXX/=jXX .or. iYY/=jYY) )then
          locdir(ix,iy)=0
        endif
      endif
    end do
  end do
  call local_upg(nx,ny,locdir,locupg)

  allocate(tmpupg(nx,ny))
  tmpupg(:,:)=locupg(:,:)
  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)>0 .and. locdir(ix,iy)==0 )then
        iXX=gcmx(ix,iy)
        iYY=gcmy(ix,iy)
        dst=0
        area_this=tmpupg(ix,iy)
        call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
        do while( dst<dst_min(iXX,iYY)*5. .and. dir(jx,jy)>0 )
          dst=dst+1
          if( locdir(jx,jy)==0 )then
            jXX=gcmx(jx,jy)
            jYY=gcmy(jx,jy)
            if( iXX==jXX .and. iYY==jYY ) locupg(jx,jy)=locupg(jx,jy)+area_this
          endif
          call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
          jx=kx
          jy=ky
        end do
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  upg_max(:,:)=0
  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)==0 )then
        iXX=gcmx(ix,iy)
        iYY=gcmy(ix,iy)
        if( upg(ix,iy)>pixnum(iXX,iYY) )then
          if( upg(ix,iy)>upg_max(iXX,iYY) )then
            upg_max(iXX,iYY)=upg(ix,iy)
          endif
        endif
      endif
    end do
  end do

  outlet(:,:)=0
  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)==0  )then
        iXX=gcmx(ix,iy)
        iYY=gcmy(ix,iy)
        if( upg(ix,iy)>pixnum(iXX,iYY) )then
          if( upg(ix,iy)==upg_max(iXX,iYY) )then
            !! river mouth pixel, use global upgrid
            outlet(ix,iy)=1
          endif
        endif
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( locdir(ix,iy)==0 .and. dir(ix,iy)>0 )then
        call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
        do while( locdir(jx,jy)>=1 .and. locdir(jx,jy)<=8 )
          call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
          jx=kx
          jy=ky
        end do
        jXX=gcmx(jx,jy)
        jYY=gcmy(jx,jy)
        if( outlet(jx,jy)==0 .and. dir(jx,jy)==0 )then
          !! if downstream is river mouth, use global upgrid
          outlet(ix,iy)=2
        elseif( (outlet(jx,jy)==0 .or. outlet(jx,jy)==2) .and. upg_max(jXX,jYY)>0 )then
          do while( dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
            call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
            jx=kx
            jy=ky
          end do
          !! if downstream is river mouth, use global upgrid
          if( outlet(jx,jy)==0 ) outlet(ix,iy)=3
        endif
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      !! if not river mouth, use local upgrid
      if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 .and. outlet(ix,iy)==0 )then
        upg(ix,iy)=locupg(ix,iy)
        if( locdir(ix,iy)>0 ) upg(ix,iy)=upg(ix,iy)*0.1
      endif
    end do
  end do
  !-------------------------------------------------------------
  ! find outlet pixel with maximum drainage area
  !-------------------------------------------------------------
  call logent('SEARCH POTENTIAL OUTLET')

  outlet(:,:)=2
  out_num(:,:)=0
  !-------------------------------------------------------------
  call logent('search mouth')

  upg_max(:,:)=0
  do iy=1, ny
    do ix=1, nx
      iXX=gcmx(ix,iy)
      iYY=gcmy(ix,iy)
      if( upg(ix,iy)>upg_min(iXX,iYY) )then
        if( dir(ix,iy)==0  )then
          out_num(iXX,iYY)=-1                        !! river mouth exist
          if( upg(ix,iy)>upg_max(iXX,iYY) )then
            upg_max(iXX,iYY)=upg(ix,iy)
          endif
        endif
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)==0  )then
        iXX=gcmx(ix,iy)
        iYY=gcmy(ix,iy)
        if( grlndf(iXX,iYY)>0 .and. out_num(iXX,iYY)==-1 )then
          if( upg(ix,iy)==upg_max(iXX,iYY) )then
            outlet(ix,iy)=0
          endif
        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  call logent('search river')

  do iy=1, ny
    do ix=1, nx
      iXX=gcmx(ix,iy)
      iYY=gcmy(ix,iy)
      !! if pixel drainage area > threshold
      if( upg(ix,iy)>upg_min(iXX,iYY) )then
        if( out_num(iXX,iYY)>=0 )then
          if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
            call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
            jXX=gcmx(jx,jy)
            jYY=gcmy(jx,jy)
            !! if downstream is another grid, set potential outlet
            if( (iXX/=jXX .or. iYY/=jYY) )then
              outlet(ix,iy)=0
              out_num(iXX,iYY)=out_num(iXX,iYY)+1
            endif
          !! mouth & inland pixels are potential outlets
          else
            outlet(ix,iy)=0
            out_num(iXX,iYY)=out_num(iXX,iYY)+1
          endif
        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  call logent('search small river')

  upg_max(:,:)=0
  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)/=-9 )then
        iXX=gcmx(ix,iy)
        iYY=gcmy(ix,iy)
        if( grlndf(iXX,iYY)>0 .and. out_num(iXX,iYY)==0 )then
          if( upg(ix,iy)>upg_max(iXX,iYY) )then
            upg_max(iXX,iYY)=upg(ix,iy)
          endif
        endif
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)/=-9 )then
        iXX=gcmx(ix,iy)
        iYY=gcmy(ix,iy)
        if( grlndf(iXX,iYY)>0 .and. out_num(iXX,iYY)==0 )then
          if( upg(ix,iy)==upg_max(iXX,iYY) )then
            outlet(ix,iy)=0
            out_num(iXX,iYY)=1
          endif
        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  call logent('check no hires land grid')

  do iYY=1, nYY
    do iXX=1, nXX
      if( grlndf(iXX,iYY)>0 .and. out_num(iXX,iYY)==0 )then
        print *, '  - no land pixel', iXX, iYY
        grlndf(iXX,iYY)=-9
      endif
      if( out_num(iXX,iYY)==-1 ) out_num(iXX,iYY)=1
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('SELECT OUTLET PIXEL FROM POTENTIAL OUTLET')

  again=1
  step=1

  do while( again>0 .and. step<=5 )
    again=0
    !-----------------------------------------------------------
    call logent('finding outlet pixel with max upgrea')

    out_x(:,:)=0
    out_y(:,:)=0
    upg_max(:,:)=0
    do iy=1, ny
      do ix=1, nx
        if( upg(ix,iy)>0 )then  !! yamadai
          iXX=gcmx(ix,iy)
          iYY=gcmy(ix,iy)
          if( grlndf(iXX,iYY)>0 )then
            if( upg(ix,iy)>upg_max(iXX,iYY) .and. outlet(ix,iy)/=2 )then
              out_x(iXX,iYY)=ix
              out_y(iXX,iYY)=iy
              upg_max(iXX,iYY)=upg(ix,iy)
            endif
          endif
        endif
      end do
    end do

    iseq=0
    sort_area(:,:)=0
    do iYY=1, nYY
      do iXX=1, nXX
        if( out_x(iXX,iYY)>0 )then
          ix=out_x(iXX,iYY)
          iy=out_y(iXX,iYY)
          outlet(ix,iy)=1                 !! selected as outlet pixel
          iseq=iseq+1
          sort_area(iseq,1)=upg(ix,iy)
          sort_area(iseq,2)=iXX
          sort_area(iseq,3)=iYY
        endif
      end do
    end do

    call logext()
    !-----------------------------------------------------------
    if( step>=5 ) cycle
    !-----------------------------------------------------------
    ! sort large upg first
    allocate(arg(nseq))
    call argsort(sort_area(:,1), arg)
    call sort(sort_area(:,1), arg)
    call sort(sort_area(:,2), arg)
    call sort(sort_area(:,3), arg)
    deallocate(arg)
    !-----------------------------------------------------------
    call logent('reject outlet if downstream distant < threshold')

    do iseq=nseq, 1, -1
      if( sort_area(iseq,1)>0 )then
        iXX=int(sort_area(iseq,2))
        iYY=int(sort_area(iseq,3))
        if( out_x(iXX,iYY)>0 )then
          dst=0
          ix=out_x(iXX,iYY)
          iy=out_y(iXX,iYY)
          if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then   !! river mouth pixel is not checked.
            call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
            dst=dst+1
            do while( outlet(jx,jy)/=1 .and. dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
              call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
              jx=kx
              jy=ky
              dst=dst+1
            end do
            if( dst<dst_min(iXX,iYY) .and. outlet(jx,jy)==1 )then
              if( out_num(iXX,iYY)>1 )then              !! if more potential outlet exist
                ix=out_x(iXX,iYY)
                iy=out_y(iXX,iYY)
                if( outlet(ix,iy)==1 ) then             !! if downstream outlet is reached
                  outlet(ix,iy)=2                       !!   reject upstream outlet
                  out_num(iXX,iYY)=out_num(iXX,iYY)-1
                  upg_max(iXX,iYY)=-999
                  out_x(iXX,iYY)=0
                  out_y(iXX,iYY)=0
                  again=again+1
                endif
              endif
            endif               
          endif
        endif
      endif
    end do

    call logmsg('# of rejected = '//str(again))
    call logext()
    !-----------------------------------------------------------
    step=step+1
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  out_XX(:,:)=0
  out_YY(:,:)=0
  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        out_XX(ix,iy)=iXX
        out_YY(ix,iy)=iYY
      endif
    end do
  end do
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tmpupg(:,:)=0
  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        area_this=upa(ix,iy)
        if( dir(ix,iy)>0 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
          jXX=gcmx(jx,jy)
          jYY=gcmy(jx,jy)
          if( out_x(jXX,jYY)==0 ) cycle
          do while( dir(jx,jy)>0 .and. out_XX(jx,jy)==0 )
            call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
            jXX=gcmx(kx,ky)
            jYY=gcmy(kx,ky)
            if( out_x(jXX,jYY)==0 ) exit
            jx=kx
            jy=ky
          end do
          if( out_XX(jx,jy)==0 ) tmpupg(jx,jy)=max(tmpupg(jx,jy),area_this)
        endif
      endif
    end do
  end do

  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        area_this=upa(ix,iy)
        if( dir(ix,iy)>0 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
          jXX=gcmx(jx,jy)
          jYY=gcmy(jx,jy)
          if( out_x(jXX,jYY)==0 ) cycle
          do while( dir(jx,jy)>0 .and. out_XX(jx,jy)==0 )
            call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
            jXX=gcmx(kx,ky)
            jYY=gcmy(kx,ky)
            if( out_x(jXX,jYY)==0 ) exit
            jx=kx
            jy=ky
          end do

          if( area_this==tmpupg(jx,jy) )then
            out_x(iXX,iYY)=jx
            out_y(iXX,iYY)=jy
            out_XX(jx,jy)=iXX
            out_YY(jx,jy)=iYY
            out_XX(ix,iy)=0
            out_YY(ix,iy)=0
            jXX=gcmx(jx,jy)
            jYY=gcmy(jx,jy)
            if( iXX<30 .and. jXX>nXX-30 ) jXX=jXX-nXX
            if( iXX>nXX-30 .and. jXX<30 ) jXX=jXX+nXX
!            print *, abs(iXX-jXX)+abs(iYY-jYY), 'mouth moved', iXX,iYY, jXX, jYY, &
!                     lon(ix), lat(iy), lon(jx), lat(jy)
          endif
        endif
      endif
    end do
  end do
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('set downstream xy')

  nextXX(:,:)=-9999
  nextYY(:,:)=-9999

  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then

        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
          do while( out_XX(jx,jy)==0 .and. dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
            call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
            jx=kx
            jy=ky
          end do

          if( out_XX(jx,jy)/=0 )then
            nextXX(iXX,iYY)=out_XX(jx,jy)
            nextYY(iXX,iYY)=out_YY(jx,jy)
          else
            nextXX(iXX,iYY)=dir(jx,jy)-9   !! mouth:-9, inland:-10
            nextYY(iXX,iYY)=dir(jx,jy)-9
          endif
        else
          nextXX(iXX,iYY)=dir(ix,iy)-9     !! mouth:-9, inland:-10
          nextYY(iXX,iYY)=dir(ix,iy)-9
        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  ! decide relation between GDBD cell which is recongnized as maximum drainage cell in TRIP grid
  !-------------------------------------------------------------
  call logent('setting river parameters')

  out_upa(:,:)=-9999
  out_lon(:,:)=-9999
  out_lat(:,:)=-9999
  out_elv(:,:)=-9999
  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        out_upa(iXX,iYY)=upa(ix,iy)*1.e6
        out_lon(iXX,iYY)=lon(ix)
        out_lat(iXX,iYY)=lat(iy)
        out_elv(iXX,iYY)=elv(ix,iy)
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  ! downstream distance
  !-------------------------------------------------------------
  call logent('calc nextdst')

  nxtdst(:,:)=-9999
  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        nxtdst(iXX,iYY)=0.
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
          nxtdst(iXX,iYY) = nxtdst(iXX,iYY) + rgetlen( lon(ix),lat(iy),lon(jx),lat(jy) )
          ix=jx
          iy=jy
          do while( out_XX(ix,iy)==0 .and. dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )
            call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
            nxtdst(iXX,iYY) = nxtdst(iXX,iYY) + rgetlen( lon(ix),lat(iy),lon(jx),lat(jy) )
            ix=jx
            iy=jy
          end do
        else
          nxtdst(iXX,iYY)=dst_mth
        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('channel length')

  upg_max(:,:)=0
  rivlen(:,:)=-9999

  do iYY=1, nYY
    do iXX=1, nXX
      if( out_x(iXX,iYY)>0 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        area_this=out_upa(iXX,iYY)            
        dst=0
        
        if( dir(ix,iy)>=1 .and. dir(ix,iy)<=8 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
          dst = dst + rgetlen( lon(ix),lat(iy),lon(jx),lat(jy) )
          do while( out_XX(jx,jy)==0 .and. dir(jx,jy)>=1 .and. dir(jx,jy)<=8 )
            call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
            dst = dst + rgetlen( lon(jx),lat(jy),lon(kx),lat(ky) )
            jx=kx
            jy=ky
          end do
          jXX=nextXX(iXX,iYY)
          jYY=nextYY(iXX,iYY)
          if( jXX>0 ) then
            if ( area_this > upg_max(jXX,jYY) )then
              rivlen(jXX,jYY) = dst
              upg_max(jXX,jYY)=area_this
            endif
          endif
        endif
      endif
    end do
  end do
      
  do iYY=1, nYY
    do iXX=1, nXX
      if( nextXX(iXX,iYY)/=-9999 )then
        if( rivlen(iXX,iYY)==-9999 ) rivlen(iXX,iYY)=len_top
      endif
    end do
  end do

  do iYY=1, nYY
    do iXX=1, nXX
      if( nextXX(iXX,iYY)>0 )then
        jXX=nextXX(iXX,iYY)
        jYY=nextYY(iXX,iYY)
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        jx=out_x(jXX,jYY)
        jy=out_y(jXX,jYY)

        if( iXX<30 .and. jXX>nXX-30 ) jXX=jXX-nXX
        if( iXX>nXX-30 .and. jXX<30 ) jXX=jXX+nXX
!        if( abs(iXX-jXX)+abs(iYY-jYY)>=4 )then
!          print *, abs(iXX-jXX)+abs(iYY-jYY), 'distant downstream', iXX,iYY, jXX, jYY,  &
!                   lon(ix), lat(iy), lon(jx), lat(jy)
!        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Saving river maps')

  call logmsg('Writing outlet pixel   '//str(wfile1))
  call traperr( wbin(out_x, wfile1, rec=1) )
  call traperr( wbin(out_y, wfile1, rec=2) )

  call logmsg('Writing drainage area  '//str(wfile2))
  call traperr( wbin(out_upa, wfile2) )

  call logmsg('Writing lon lat        '//str(wfile3))
  call traperr( wbin(out_lon, wfile3, rec=1) )
  call traperr( wbin(out_lat, wfile3, rec=2) )

  call logmsg('Writing elvation       '//str(wfile4))
  call traperr( wbin(out_elv, wfile4) )

  call logmsg('Writing nextxy         '//str(wfile5))
  call traperr( wbin(nextXX, wfile5, rec=1) )
  call traperr( wbin(nextYY, wfile5, rec=2) )

  call logmsg('Writing next distance  '//str(wfile6))
  call traperr( wbin(nxtdst, wfile6) )

  call logmsg('Writing channel length '//str(wfile7))
  call traperr( wbin(rivlen, wfile7) )

  call logext()
  !-------------------------------------------------------------
  call logret()
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
real(8) function rgetlen(rlon1, rlat1, rlon2, rlat2)
  ! ================================================
  ! to   get the length (m) between (rlon1, rlat1) to (rlon2, rlat2)
  ! by   nhanasaki
  ! on   1st Nov 2003
  ! at   IIS,UT
  !
  !     see page 643 of Rika-Nenpyo (2000)
  !     at the final calculation, earth is assumed to be a sphere
  ! ================================================
  implicit none
  real                ::  rpi                !! Pi
  double precision    ::  de2                !! eccentricity powered by 2
  double precision    ::  da                 !! the radius of the earth

  real(8)             ::  rlon1              !! longitude of the origin
  real(8)             ::  rlon2              !! longitude of the destination
  real(8)             ::  rlat1              !! latitude of the origin
  real(8)             ::  rlat2              !! latitude of the destination
  double precision    ::  dsinlat1           !! sin(lat1)
  double precision    ::  dsinlon1           !! sin(lon1)
  double precision    ::  dcoslat1           !! cos(lat1)
  double precision    ::  dcoslon1           !! cos(lon1)
  double precision    ::  dsinlat2           !! sin(lat2) 
  double precision    ::  dsinlon2           !! sin(lon2)
  double precision    ::  dcoslat2           !! cos(lat2)
  double precision    ::  dcoslon2           !! cos(lon2)
  double precision    ::  dh1                !! hegiht of the origin
  double precision    ::  dn1                !! intermediate val of calculation
  double precision    ::  dx1                !! X coordinate of the origin
  double precision    ::  dy1                !! Y coordinate of the origin
  double precision    ::  dz1                !! Z coordinate of the origin
  double precision    ::  dh2                !! height of the destination
  double precision    ::  dn2                !! intermediate val of calculation
  double precision    ::  dx2                !! X coordinate of the destination
  double precision    ::  dy2                !! Y coordinate of the destination
  double precision    ::  dz2                !! Z coordinate of the destination

  double precision    ::  dlen               !! length between origin and destination
  double precision    ::  drad               !! half of the angle
  ! parameters
  data             da/6378137.0/
  data             de2/0.006694470/
  data             rpi/3.141592/      
  ! ================================================
  ! (lon1,lat1) --> (x1,y1,z1)
  ! ================================================
  dh1=0
  dh2=0

  dsinlat1 = dble(sin(rlat1 * rpi/180))
  dsinlon1 = dble(sin(rlon1 * rpi/180))
  dcoslat1 = dble(cos(rlat1 * rpi/180))
  dcoslon1 = dble(cos(rlon1 * rpi/180))

  dn1 = da/(sqrt(1.0-de2*dsinlat1*dsinlat1))
  dx1 = (dn1+dh1)*dcoslat1*dcoslon1
  dy1 = (dn1+dh1)*dcoslat1*dsinlon1
  dz1 = (dn1*(1-de2)+dh1)*dsinlat1
   ! ================================================
   ! (lon2,lat2) --> (x2,y2,z2)
   ! ================================================
  dsinlat2 = dble(sin(rlat2 * rpi/180))
  dsinlon2 = dble(sin(rlon2 * rpi/180))
  dcoslat2 = dble(cos(rlat2 * rpi/180))
  dcoslon2 = dble(cos(rlon2 * rpi/180))

  dn2 = da/(sqrt(1.0-de2*dsinlat2*dsinlat2))
  dx2 = (dn2+dh2)*dcoslat2*dcoslon2
  dy2 = (dn2+dh2)*dcoslat2*dsinlon2
  dz2 = (dn2*(1-de2)+dh2)*dsinlat2      
  ! ================================================
  ! Calculate length
  ! ================================================
  dlen=sqrt((dx1-dx2)**2+(dy1-dy2)**2+(dz1-dz2)**2)
  drad=dble(asin(real(dlen/2/da)))
  rgetlen=real(drad*2*da)
end function rgetlen
!---------------------------------------------------------------
subroutine local_upg(nx,ny,dir,upg)
  implicit none
  integer    :: ix, iy, jx, jy, kx, ky
  integer    :: nx, ny

  integer(1) :: dir(nx,ny)
  real(8)    :: upg(nx,ny)


  integer(1) :: upgrid(nx,ny),upnow(nx,ny)

  integer    :: isAgain
  integer    :: iseq, jseq, nseq
  integer    :: seqx(nx*ny),seqy(nx*ny)

  upgrid(:,:)=0
  upnow(:,:)=0

  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)>0 )then
        call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
        upgrid(jx,jy)=upgrid(jx,jy)+1_1
      endif
    end do
  end do

  upg(:,:)=0
  do iy=1, ny
    do ix=1, nx
      if( dir(ix,iy)==-9 )then
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
        upg(ix,iy)=upg(ix,iy)+1
        if( dir(ix,iy)>0 )then
          call nextxy(ix,iy,jx,jy,nx,dir(ix,iy))
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
      upg(jx,jy)=upg(jx,jy)+1
      if( dir(jx,jy)>0 )then
        call nextxy(jx,jy,kx,ky,nx,dir(jx,jy))
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
end subroutine local_upg
!-------------------------------------------------------------
end program main


