program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use def_consts
  use mod_utils
  implicit none

  ! hi-res pixel
  integer :: ix, iy

  ! lo-res GCM map grid
  integer ::  iXX, iYY, jXX, jYY, kXX, kYY
  integer ::  nXX, nYY                        !! river network domain (nXX,nYY)

  ! flood plain layers
  integer :: m

  ! params.
  real(8), parameter :: dst_mth = 10.d3    !! downstream distance at river mouth [m]
  real(8), parameter :: len_top = 5.d3     !! channel length at topmost grid     [m]

  ! input GCM
  real(8), allocatable :: lndare(:,:)     !! land fraction (0-1)
  integer, allocatable :: gcmx(:,:)       !! gcm grid (iXX,iYY) of hires pixel (ix,iy)
  integer, allocatable :: gcmy(:,:)       !! 

  real(8), allocatable :: grlon(:,:)
  real(8), allocatable :: grlat(:,:)

  ! output river map
  integer, allocatable :: nextXX(:,:)     !! downstream (jXX,jYY) of grid (iXX,iYY)
  integer, allocatable :: nextYY(:,:)     !! 

  real(8), allocatable :: elevtn(:,:)     !! elvation      of outlet pixel [m]
  real(8), allocatable :: fldhgt(:,:,:)   !! floodplain elevation profile [m]

  real(8), allocatable :: nxtdst(:,:)     !! downstrem distance   [m]
  real(8), allocatable :: rivlen(:,:)     !! river channel length [m]

  real(8), allocatable :: uparea(:,:)     !! drainage area  of outlet pixel [m2]
  real(8), allocatable :: grarea(:,:)     !! catchment area  of outlet pixel [m2]

  real(8), allocatable :: out_lon(:,:)    !! lon            of outlet pixel
  real(8), allocatable :: out_lat(:,:)    !! lat            of outlet pixel

  integer, allocatable :: out_x(:,:)      !! outlet ix      of outlet pixel
  integer, allocatable :: out_y(:,:)      !! outlet iy      of outlet pixel

  ! files
  character(128), parameter :: fparams = 'params.txt'

  character(128), parameter :: fgcmxy    = 'gcmmap/gcmxy.bin'
  character(128), parameter :: flndare   = 'gcmmap/lndare.bin'
  character(128), parameter :: fgrlonlat = 'gcmmap/grlonlat.bin'

  character(128), parameter :: rfile1 = 'tmp/map/nextxy.bin'
  character(128), parameter :: rfile2 = 'tmp/map/elevtn.bin'
  character(128), parameter :: rfile3 = 'tmp/map/fldhgt.bin'
  character(128), parameter :: rfile4 = 'tmp/map/nxtdst.bin'
  character(128), parameter :: rfile5 = 'tmp/map/rivlen.bin'
  character(128), parameter :: rfile6 = 'tmp/map/grarea.bin'
  character(128), parameter :: rfile7 = 'tmp/map/uparea.bin'
  character(128), parameter :: rfile8 = 'tmp/map/lonlat.bin'
  character(128), parameter :: rfile9 = 'tmp/map/out_xy.bin'

  character(128), parameter :: wfile1 = 'map/nextxy.bin'
  character(128), parameter :: wfile2 = 'map/elevtn.bin'
  character(128), parameter :: wfile3 = 'map/fldhgt.bin'
  character(128), parameter :: wfile4 = 'map/nxtdst.bin'
  character(128), parameter :: wfile5 = 'map/rivlen.bin'
  character(128), parameter :: wfile6 = 'map/grarea.bin'
  character(128), parameter :: wfile7 = 'map/uparea.bin'
  character(128), parameter :: wfile8 = 'map/lonlat.bin'

  call logbgn('program gcm_rivermap', '', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Reading params '//str(fparams))

  open(11, file=fparams, status='old')

  read(11,*) nXX
  read(11,*) nYY
  !read(11,*) fgcmidx

  !call read_conf_earth(earth, 11)

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! GCM
  allocate(lndare(nXX,nYY),gcmx(nx,ny),gcmy(nx,ny),grlon(nXX,nYY),grlat(nXX,nYY))

  ! map
  allocate(nextXX(nXX,nYY),nextYY(nXX,nYY))
  allocate(elevtn(nXX,nYY),fldhgt(nXX,nYY,nflp))
  allocate(nxtdst(nXX,nYY),rivlen(nXX,nYY))
  allocate(grarea(nXX,nYY),uparea(nXX,nYY))
  allocate(out_lon(nXX,nYY),out_lat(nXX,nYY))

  !
      allocate(out_x(nXX,nYY),out_y(nXX,nYY))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('READ FILES')

  call logmsg('Reading lndare '//trim(flndare))
  call traperr( rbin(lndare, flndare) )

  call logmsg('Reading gcmxy '//trim(fgcmxy))
  call traperr( rbin(gcmx, fgcmxy, rec=1) )
  call traperr( rbin(gcmy, fgcmxy, rec=2) )

  call logmsg('Reading grlonlat '//trim(fgrlonlat))
  call traperr( rbin(grlon, fgrlonlat, rec=1) )
  call traperr( rbin(grlat, fgrlonlat, rec=2) )
  !-------------------------------------------------------------
  call logmsg('Reading nextxy '//trim(rfile1))
  call traperr( rbin(nextXX, rfile1, rec=1) )
  call traperr( rbin(nextYY, rfile1, rec=2) )

  call logmsg('Reading elvation '//trim(rfile2))
  call traperr( rbin(elevtn, rfile2) )

  call logmsg('Reading floodplain topography '//trim(rfile3))
  do m = 1, nflp
    call traperr( rbin(fldhgt(:,:,m), rfile3, rec=m) )
  enddo

  call logmsg('Reading next distance '//trim(rfile4))
  call traperr( rbin(nxtdst, rfile4) )
      
  call logmsg('Reading channel length '//trim(rfile5))
  call traperr( rbin(rivlen, rfile5) )

  call logmsg('Reading catchment area '//trim(rfile6))
  call traperr( rbin(grarea, rfile6) )

  call logmsg('Reading drainage area '//trim(rfile7))
  call traperr( rbin(uparea, rfile7) )

  call logmsg('Reading lon lat '//trim(rfile8))
  call traperr( rbin(out_lon, rfile8, rec=1) )
  call traperr( rbin(out_lat, rfile8, rec=2) )

  call logmsg('Reading outlet pixel '//trim(rfile9))
  call traperr( rbin(out_x, rfile9, rec=1) )
  call traperr( rbin(out_y, rfile9, rec=2) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Modify river mask')

  do iYY=1, nYY
    do iXX=1, nXX
      if( nextXX(iXX,iYY)==-9 )then
        ix=out_x(iXX,iYY)
        iy=out_y(iXX,iYY)
        jXX=gcmx(ix,iy)
        jYY=gcmy(ix,iy)
        if( iXX/=jXX .or. iYY/=jYY) then
          if( nextXX(jXX,jYY)<0 )then
            print *, 'moved 1', iXX, iYY, jXX, jYY, nextXX(jXX,jYY), nextYY(jXX,jYY)
          else
            do while( nextXX(jXX,jYY)>0 )
              kXX=nextXX(jXX,jYY)
              kYY=nextYY(jXX,jYY)
              jXX=kXX
              jYY=kYY
            end do
            print *, 'moved 2', iXX, iYY, jXX, jYY, nextXX(jXX,jYY), nextYY(jXX,jYY)
          endif
          nextXX(iXX,iYY)=jXX
          nextYY(iXX,iYY)=jYY
          uparea(jXX,jYY)=uparea(jXX,jYY)+uparea(iXX,iYY)
        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Modify no land pixel grid')

  do iYY=1, nYY
    do iXX=1, nXX
      if( lndare(iXX,iYY)>0 .and. nextXX(iXX,iYY)==-9999 )then
        nextXX(iXX,iYY)=-9
        nextYY(iXX,iYY)=-9
        elevtn(iXX,iYY)=0
        do m=1, nflp
          fldhgt(iXX,iYY,m)=100.*real(m)/real(nflp)
        end do
        nxtdst(iXX,iYY)=dst_mth
        rivlen(iXX,iYY)=len_top
        grarea(iXX,iYY)=lndare(iXX,iYY)
        uparea(iXX,iYY)=lndare(iXX,iYY)
        out_lon(iXX,iYY)=grlon(iXX,iYY)
        out_lat(iXX,iYY)=grlat(iXX,iYY)
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do iYY=1, nYY
    do iXX=1, nXX
      if( nextXX(iXX,iYY)>0 )then
        jXX=nextXX(iXX,iYY)
        jYY=nextYY(iXX,iYY)

        if( elevtn(jXX,jYY)>elevtn(iXX,iYY) )then
          call logmsg('NegativeSlope '//str((/iXX,iYY,jXX,jYY/))//&
                      ' '//str((/elevtn(iXX,iYY),elevtn(jXX,jYY)/))//&
                      ' '//str((/out_lon(iXX,iYY),out_lat(iXX,iYY)/)))
        endif
      endif
      if( nextXX(iXX,iYY)==-9 )then
        if( elevtn(iXX,iYY)>0 )then
          call logmsg('NotSeaLevel '//str((/iXX,iYY/))//&
                      ' '//str(elevtn(iXX,iYY))//&
                      ' '//str((/out_lon(iXX,iYY),out_lat(iXX,iYY)/)))
          elevtn(iXX,iYY)=0
        endif
      endif
    end do
  end do
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('SAVE RIVER MAPS')

  call logmsg('Writing nextxy '//trim(wfile1))
  call traperr( wbin(nextXX, wfile1, rec=1) )
  call traperr( wbin(nextYY, wfile1, rec=2) )

  call logmsg('Writing elvation '//trim(wfile2))
  call traperr( wbin(elevtn, wfile2, dtype=dtype_real) )

  call logmsg('Writing floodplain topography '//trim(wfile3))
  do m = 1, nflp
    call traperr( wbin(fldhgt(:,:,m), wfile3, dtype=dtype_real) )
  enddo

  call logmsg('Writing next distance '//trim(wfile4))
  call traperr( wbin(nxtdst, wfile4, dtype=dtype_real) )
      
  call logmsg('Writing channel length '//trim(wfile5))
  call traperr( wbin(rivlen, wfile5, dtype=dtype_real) )

  call logmsg('Writing catchment area '//trim(wfile6))
  call traperr( wbin(grarea, wfile6, dtype=dtype_real) )

  call logmsg('Writing drainage area '//trim(wfile7))
  call traperr( wbin(uparea, wfile7, dtype=dtype_real) )

  call logmsg('Writing lon lat '//trim(wfile8))
  call traperr( wbin(out_lon, wfile8, dtype=dtype_real, rec=1) )
  call traperr( wbin(out_lat, wfile8, dtype=dtype_real, rec=2) )

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logret()
end program main
