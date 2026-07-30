program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use c1_type_opt, only: &
        opt_earth_
  use def_consts
  use mod_utils, only: &
        read_conf_earth, &
        nextxy
  implicit none

  ! index TRIP
  integer :: iXX, iYY, jXX, jYY
  integer :: nXX, nYY

  real(8) :: west, east, north, south, gsize

  ! parameters
  real, parameter :: dst_mth = 10.d3  !! downstream distance at river mouth [m]

  ! input
  integer, allocatable :: nextX(:,:)                  !! next grid X
  integer, allocatable :: nextY(:,:)                  !! flow direction conbined
  ! output
  integer, allocatable :: rivseq(:,:)                 !! river sequence
  real(8), allocatable :: grarea(:,:)                 !! drainage area (GRID base)
  real(8), allocatable :: uparea(:,:)                 !! upa drainage area (GRID base)
  real(8), allocatable :: nxtdst(:,:)                 !! next dstistance
  integer, allocatable :: color(:,:)                  !! color of basin
  integer, allocatable :: basin(:,:)                  !! basin ID
  integer, allocatable :: upgrid(:,:)                 !! upstream grids
 
  real(8), allocatable :: lon(:,:)
  real(8), allocatable :: lat(:,:)
  ! local
  integer, allocatable :: seqxy(:,:)
  integer              :: nseq, nseq_max
  integer              :: n, m, nmax
  real(8)              :: upa
  integer              :: upg
  real(8)              :: lon1, lon2, lat1, lat2
  integer              :: nbsn, basin_this, nbsn_max
  integer, allocatable :: bsn_mask(:,:)
  integer              :: color_this, color_max, grid
  integer              :: col_used(10), icol
  integer              :: checkpoint

  integer   , allocatable :: basin_grid(:), basin_new(:)
  integer(8), allocatable :: basin_order(:)

  ! earth's params
  type(opt_earth_) :: earth

  ! file
  character(128), parameter :: fparams   = 'params.txt'   !! GCM dimention file

  character(128), parameter :: fgcmxy    = 'gcmmap/gcmxy.bin'
  character(128), parameter :: fgrlonlat = 'gcmmap/grlonlat.bin'
  character(128), parameter :: flndare   = 'gcmmap/lndare.bin'

  character(128), parameter :: rfile1 = 'map/nextxy.bin'

  character(128), parameter :: fmapdim    = 'map/mapdim.txt'
  character(128), parameter :: fmapparams = 'map/params.txt'
  character(128), parameter :: wfile1 = 'map/rivseq.bin'
  character(128), parameter :: wfile2 = 'map/grarea_grid.bin'
  character(128), parameter :: wfile3 = 'map/uparea_grid.bin'
  character(128), parameter :: wfile4 = 'map/upgrid.bin'
  character(128), parameter :: wfile5 = 'map/nxtdst_grid.bin'
  character(128), parameter :: wfile6 = 'map/basin.bin'
  character(128), parameter :: wfile7 = 'map/bsncol.bin'

  call logbgn('program set_map', '', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Reading params '//str(fparams))
  open(11, file=fparams, status='old')

  read(11,*) nXX
  read(11,*) nYY
  read(11,*) ! fgcmidx

  call read_conf_earth(earth, 11)

  close(11)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(nextx(nXX,nYY), nexty(nXX,nYY), rivseq(nXX,nYY),grarea(nXX,nYY),uparea(nXX,nYY))
  allocate(nxtdst(nXX,nYY),color(nXX,nYY), basin(nXX,nYY), upgrid(nXX,nYY),lon(nXX,nYY),lat(nXX,nYY))
  allocate(seqxy(nXX*nYY,2))

  west=-180.0
  east= 180.0
  north= 90.0
  south=-90.0
  gsize=(360.*180./real(nXX)/real(nYY)) **0.5

  call traperr( rbin(nextX, rfile1, rec=1) )
  call traperr( rbin(nextY, rfile1, rec=2) )

  call traperr( rbin(lon, fgrlonlat, rec=1) )
  call traperr( rbin(lat, fgrlonlat, rec=2) )

  call traperr( rbin(grarea, flndare) )

  open(11, file=fmapdim, status='replace')
  write(11,'(i10,5x,a)') nXX, '!! nXX'
  write(11,'(i10,5x,a)') nYY, '!! nYY'
  write(11,'(i10,5x,a)') nflp, '!! floodplain layer'
  close(11)

  open(11, file=fmapparams, status='replace')
  write(11,'(f12.3,5x,a)') west,  '!! west  edge [deg]'
  write(11,'(f12.3,5x,a)') north, '!! north edge [deg]'
  write(11,'(i12,5x,a)')   nXX,   '!! grid number (east-west)'
  write(11,'(i12,5x,a)')   nYY,   '!! grid number (north-south)'
  write(11,'(f12.8,5x,a)') gsize, '!! average grid size [deg]'
  write(11,'(i12,5x,a)')   nflp,  '!! floodplain layer'
  write(11,'(i12,5x,a)')   1,     '!! grid number (east-west)'
  write(11,'(f12.8,5x,a)') 1./60.,'!! hires pixel size [deg]'
  close(11)
  !-------------------------------------------------------------
  ! calc river sequence
  !-------------------------------------------------------------
  call logent('SET_MAP calc river sequence')

  rivseq=1
  do iYY=1, nYY
    do iXX=1, nXX
      if( nextX(iXX,iYY)>0)then
        jXX=nextX(iXX,iYY)
        jYY=nextY(iXX,iYY)
        rivseq(jXX,jYY)=0
      elseif( nextX(iXX,iYY)==-9999 )then
        rivseq(iXX,iYY)=-9999
      endif
    end do
  end do

  nseq=2
  n=0
  do iYY=1, nYY
    do iXX=1, nXX
      if( rivseq(iXX,iYY)==1 .and. nextX(iXX,iYY)>0 )then
        jXX=nextX(iXX,iYY)
        jYY=nextY(iXX,iYY)
        rivseq(jXX,jYY)=nseq
        n=n+1
        seqxy(n,1)=jXX
        seqxy(n,2)=jYY
      endif
    end do
  end do
  nmax=n
  call logmsg('nmax = '//str(nmax))

  checkpoint = 12
  do while (nmax>0)
    nseq=nseq+1
    m=0
    do n=1, nmax
      iXX=seqxy(n,1)
      iYY=seqxy(n,2)
      if( nextX(iXX,iYY)>0 )then
        jXX=nextX(iXX,iYY)
        jYY=nextY(iXX,iYY)
        rivseq(jXX,jYY)=nseq
        m=m+1
        seqxy(m,1)=jXX
        seqxy(m,2)=jYY
      endif
      if (nmax==checkpoint) then
        print"(1x'n='i5' XX='i3' YY='i3' nextx='i3' nexty='i3' nseq='i3' m='i5)",&
               n,iXX,iYY,nextx(iXX,iYY),nexty(iXX,iYY),nseq,m
      endif
    end do

    nmax=m
  end do

  nseq_max=nseq
  call logmsg('nseqmax = '//str(nseq_max))

  call traperr( wbin(rivseq, wfile1, dtype=dtype_real) )

  call logext()
  !-------------------------------------------------------------
  ! calc upa drainage area
  !-------------------------------------------------------------
  call logent('SET_MAP calc upa drainage area')

  do iYY=1, nYY
    do iXX=1, nXX
      if( nextX(iXX,iYY)/=-9999 )then
        uparea(iXX,iYY)=0
        upgrid(iXX,iYY)=0
      else
        uparea(iXX,iYY)=-9999
        upgrid(iXX,iYY)=-9999
      endif
    end do
  end do

  do iYY=1, nYY
    do iXX=1, nXX
      if( rivseq(iXX,iYY)==1 ) then
        jXX=iXX
        jYY=iYY
        uparea(jXX,jYY)=grarea(jXX,jYY)
        upgrid(jXX,jYY)=1
        upa=uparea(jXX,jYY)
        upg=upgrid(jXX,jYY)
        do while( nextX(jXX,jYY)>0 )  !! if river reaches mouth, end loop
          call nextGRID(jXX,jYY,nextX(jXX,jYY),nextY(jXX,jYY))
          if( uparea(jXX,jYY)==0 )then                     !! grids firstly checked
            uparea(jXX,jYY)=upa+grarea(jXX,jYY)
            upa=uparea(jXX,jYY)
            upgrid(jXX,jYY)=upg+1
            upg=upgrid(jXX,jYY)
          else                                       !! grids already checked
            uparea(jXX,jYY)=uparea(jXX,jYY)+upa
            upgrid(jXX,jYY)=upgrid(jXX,jYY)+upg
          endif
        end do
      endif
    end do
  end do

  call traperr( wbin(grarea, wfile2, dtype=dtype_real) )

  call traperr( wbin(uparea, wfile3, dtype=dtype_real) )

  call traperr( wbin(upgrid, wfile4) )

  call logext()
  !-------------------------------------------------------------
  ! clac distance to next grid
  !-------------------------------------------------------------
  call logent('SET_MAP calc distance to next grid')

  do iYY=1, nYY
    do iXX=1, nXX
      selectcase( nextX(iXX,iYY) )
      case( 0: )
      !if( nextX(iXX,iYY)>0 ) then
        jXX=nextX(iXX,iYY)
        jYY=nextY(iXX,iYY)
        lon1=lon(iXX,iYY)
        lon2=lon(jXX,jYY)
        lat1=lat(iXX,iYY)
        lat2=lat(jYY,jYY)
        if( lon1<-90 .and. lon2>270 ) lon2=lon2-360
        if( lon1>270 .and. lon2<-90 ) lon2=lon2+360
        selectcase( earth%shptyp )
        case( EARTH_SHPTYP__SPHERE )
          nxtdst(iXX,iYY)=rgetlen_sphere(lon1,lat1,lon2,lat2)
        case( EARTH_SHPTYP__ELLIPS )
          nxtdst(iXX,iYY)=rgetlen_ellips(lon1,lat1,lon2,lat2)
        case default
          call errend(msg_invalid_value('earth%shptyp', earth%shptyp))
        endselect
      case( -9999 )
        nxtdst(iXX,iYY)=-9999
      case default
        nxtdst(iXX,iYY)=dst_mth*1000.
      endselect
      !elseif( nextX(iXX,iYY)/=-9999 )then
      !  nxtdst(iXX,iYY)=dst_mth*1000.
      !elseif( nextX(iXX,iYY)==-9999 )then
      !  nxtdst(iXX,iYY)=-9999
      !endif
    end do
  end do

  call traperr( wbin(nxtdst, wfile5, dtype=dtype_real) )

  call logext()
  !-------------------------------------------------------------
  ! calc basin
  !-------------------------------------------------------------
  call logent('SET_MAP calc basin')

  basin=0
  nbsn=0

  do iYY=1, nYY
    do iXX=1, nXX
      if( rivseq(iXX,iYY)==1 ) then
        nbsn=nbsn+1

        ! loop until river reach mouth or already decided river
        !-------------------------------------------------------
        jXX=iXX
        jYY=iYY
        basin(jXX,jYY)=nbsn
        do while( nextX(jXX,jYY)>0 )
          if ( basin(nextX(jXX,jYY),nextY(jXX,jYY))/=0 ) exit 
          call nextGRID(jXX,jYY,nextX(jXX,jYY),nextY(jXX,jYY))
          basin(jXX,jYY)=nbsn
        end do

        ! again loop in case basinIS is already decided
        !-------------------------------------------------------
        if( nextX(jXX,jYY)>0  )then
          if ( basin(nextX(jXX,jYY),nextY(jXX,jYY))>0 ) then
            nbsn=nbsn-1
            basin_this=int( basin(nextX(jXX,jYY),nextY(jXX,jYY)) )
            jXX=iXX
            jYY=iYY
            basin(jXX,jYY)=basin_this
            do while( basin(nextX(jXX,jYY),nextY(jXX,jYY))/=basin_this )
              call nextGRID(jXX,jYY,nextX(jXX,jYY),nextY(jXX,jYY))
              basin(jXX,jYY)=basin_this
            end do
          endif
        endif
        !-------------------------------------------------------
      endif
    end do
  end do
  nbsn_max=nbsn
  call logmsg('number of basin = '//str(nbsn_max))

  allocate(basin_grid(nbsn_max))
  allocate(basin_order(nbsn_max))
  allocate(basin_new(nbsn_max))
  do iYY=1, nYY
    do iXX=1, nXX
      if( nextx(iXX,iYY)<0 .and. nextx(iXX,iYY)/=-9999 ) then
        nbsn=basin(iXX,iYY)
        basin_grid(nbsn)=upgrid(iXX,iYY)
        basin_order(nbsn)=nbsn
      endif
    end do
  end do

  call argsort(basin_grid, basin_order)
  !call heap_sort2(nbsn_max,basin_grid,basin_order)

  do nbsn=1, nbsn_max
    basin_this=int(basin_order(nbsn),4)
    basin_new(basin_this)=nbsn_max-nbsn+1
  end do

  do iYY=1, nYY
    do iXX=1, nXX
      if( nextx(iXX,iYY)/=-9999 ) then
        nbsn=basin(iXX,iYY)
        basin(iXX,iYY)=basin_new(nbsn)
      else
        basin(iXX,iYY)=-9999
      endif
    end do
  end do

  call traperr( wbin(basin, wfile6) )

  call logext()
  !-------------------------------------------------------------
  ! decide color of each basin for use in GMT
  !-------------------------------------------------------------
  call logent('SET_MAP color basin')

  color=-9999
  color_max=0
  allocate(bsn_mask(nbsn_max,4))
  bsn_mask(:,1)=999999
  bsn_mask(:,2)=999999
  bsn_mask(:,3)=-999999
  bsn_mask(:,4)=-999999
  do iYY=1, nYY
    do iXX=1, nXX
      if( basin(iXX,iYY)>0 )then
        nbsn=int( basin(iXX,iYY) )
        bsn_mask(nbsn,1) = min( bsn_mask(nbsn,1),iXX )
        bsn_mask(nbsn,2) = min( bsn_mask(nbsn,2),iYY )
        bsn_mask(nbsn,3) = max( bsn_mask(nbsn,3),iXX )
        bsn_mask(nbsn,4) = max( bsn_mask(nbsn,4),iYY )
      endif
    end do
  end do
  do nbsn=1, nbsn_max
    if( bsn_mask(nbsn,1)<=1 .or. bsn_mask(nbsn,3)>=nXX )then
      bsn_mask(nbsn,1) = 1
      bsn_mask(nbsn,2) = max(bsn_mask(nbsn,2)-1,1 )
      bsn_mask(nbsn,3) = nXX
      bsn_mask(nbsn,4) = min(bsn_mask(nbsn,4)+1,nYY)
    else
      bsn_mask(nbsn,1) = bsn_mask(nbsn,1)-1
      bsn_mask(nbsn,2) = max(bsn_mask(nbsn,2)-1,1 )
      bsn_mask(nbsn,3) = bsn_mask(nbsn,3)+1
      bsn_mask(nbsn,4) = min(bsn_mask(nbsn,4)+1,nYY)
    endif
  end do

  ! ==================
  ! = count basin grids num and
  ! = check color used in neighbour basin
  ! = (step1: large basin)
  ! ==================
  do nbsn=1, nbsn_max
    col_used=0
    grid=0
    do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
      do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
        if( basin(iXX,iYY)==nbsn )then
          grid=grid+1

          jXX=iXX
          jYY=iYY+1
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

          jXX=iXX+1
          jYY=iYY
          if( east-west==360 .and. jXX>nXX ) jXX=1
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

          jXX=iXX
          jYY=iYY-1
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

          jXX=iXX-1
          jYY=iYY
          if( east-west==360 .and. jXX==0 ) jXX=nXX
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

        endif
      end do
    end do

    ! === decide color for large basin
    if( grid>=20 )then
      icol=2
      do while( col_used(icol)==1 )
        icol=icol+1
      end do
      color_this=icol
      if( color_max<color_this )color_max=color_this

      do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
        do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
          if(basin(iXX,iYY)==nbsn) color(iXX,iYY)=color_this
        end do
      end do
    endif
  end do

  ! ==================
  ! = (step2: small basin)
  ! ==================
  do nbsn=1, nbsn_max
    col_used=0
    grid=0
    do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
      do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
        if( basin(iXX,iYY)==nbsn )then
          grid=grid+1

          jXX=iXX
          jYY=iYY+1
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

          jXX=iXX+1
          jYY=iYY
          if( east-west==360 .and. jXX>nXX ) jXX=1
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

          jXX=iXX
          jYY=iYY-1
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

          jXX=iXX-1
          jYY=iYY
          if( east-west==360 .and. jXX==0 ) jXX=nXX
          if( jXX>=1 .and. jXX<=nXX .and. jYY>=1 .and. jYY<=nYY) then
            if( color(jXX,jYY)>0 ) col_used(color(jXX,jYY))=1
          endif

        endif
      end do
    end do

    ! === decide color for small
    if( grid<20 )then
      icol=2
      do while(col_used(icol)==1)
        icol=icol+1
      end do
      color_this=icol
      if(grid==1) color_this=1
      if(color_max<color_this) color_max=color_this

      do iYY=bsn_mask(nbsn,2), bsn_mask(nbsn,4)
        do iXX=bsn_mask(nbsn,1), bsn_mask(nbsn,3)
          if(basin(iXX,iYY)==nbsn) color(iXX,iYY)=color_this
        end do
      end do
    endif
  end do
  call logmsg('number of color = '//str(color_max))

  call traperr( wbin(color, wfile7) )

  call logext()
  !-------------------------------------------------------------
  call logret()
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine nextGRID(iXX, iYY, jXX, jYY)
  implicit none
  integer            ::  iXX, iYY, jXX, jYY

  iXX=jXX
  iYY=jYY
end subroutine nextGRID
!---------------------------------------------------------------
real(8) function rgetlen_ellips(rlon1, rlat1, rlon2, rlat2) result(l)
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
  real(8) ::  rlon1              !! longitude of the origin
  real(8) ::  rlon2              !! longitude of the destination
  real(8) ::  rlat1              !! latitude of the origin
  real(8) ::  rlat2              !! latitude of the destination
  real(8) ::  dsinlat1           !! sin(lat1)
  real(8) ::  dsinlon1           !! sin(lon1)
  real(8) ::  dcoslat1           !! cos(lat1)
  real(8) ::  dcoslon1           !! cos(lon1)
  real(8) ::  dsinlat2           !! sin(lat2) 
  real(8) ::  dsinlon2           !! sin(lon2)
  real(8) ::  dcoslat2           !! cos(lat2)
  real(8) ::  dcoslon2           !! cos(lon2)
  real(8) ::  dh1                !! hegiht of the origin
  real(8) ::  dn1                !! intermediate val of calculation
  real(8) ::  dx1                !! X coordinate of the origin
  real(8) ::  dy1                !! Y coordinate of the origin
  real(8) ::  dz1                !! Z coordinate of the origin
  real(8) ::  dh2                !! height of the destination
  real(8) ::  dn2                !! intermediate val of calculation
  real(8) ::  dx2                !! X coordinate of the destination
  real(8) ::  dy2                !! Y coordinate of the destination
  real(8) ::  dz2                !! Z coordinate of the destination

  real(8) ::  dlen               !! length between origin and destination
  real(8) ::  drad               !! half of the angle
  ! ================================================
  ! (lon1,lat1) --> (x1,y1,z1)
  ! ================================================
  dh1=0.d0
  dh2=0.d0

  dsinlat1 = sin(rlat1 * d2r)
  dsinlon1 = sin(rlon1 * d2r)
  dcoslat1 = cos(rlat1 * d2r)
  dcoslon1 = cos(rlon1 * d2r)

  dn1 = earth%r/(sqrt(1.d0-earth%e2*dsinlat1*dsinlat1))
  dx1 = (dn1+dh1)*dcoslat1*dcoslon1
  dy1 = (dn1+dh1)*dcoslat1*dsinlon1
  dz1 = (dn1*(1-earth%e2)+dh1)*dsinlat1
  ! ================================================
  ! (lon2,lat2) --> (x2,y2,z2)
  ! ================================================
  dsinlat2 = sin(rlat2 * d2r)
  dsinlon2 = sin(rlon2 * d2r)
  dcoslat2 = cos(rlat2 * d2r)
  dcoslon2 = cos(rlon2 * d2r)

  dn2 = earth%r/(sqrt(1.d0-earth%e2*dsinlat2*dsinlat2))
  dx2 = (dn2+dh2)*dcoslat2*dcoslon2
  dy2 = (dn2+dh2)*dcoslat2*dsinlon2
  dz2 = (dn2*(1-earth%e2)+dh2)*dsinlat2      
  ! ================================================
  ! Calculate length
  ! ================================================
  dlen = sqrt((dx1-dx2)**2+(dy1-dy2)**2+(dz1-dz2)**2)
  drad = asin(real(dlen*0.5d0/earth%r))
  l = real(drad*2*earth%r)
end function rgetlen_ellips
!---------------------------------------------------------------
real(8) function rgetlen_sphere(rlon1, rlat1, rlon2, rlat2) result(l)
  implicit none
  real(8), intent(in) :: rlon1, rlat1, rlon2, rlat2
  real(8) :: londiff

  londiff = abs(rlon2 - rlon1)
  if( londiff > 180.d0 ) londiff = 360.d0 - londiff

  l = real(acos(min(1.d0, max(-1.d0, &
           sin(rlat1*d2r)*sin(rlat2*d2r) &
            + cos(rlat1*d2r)*cos(rlat2*d2r)*cos(londiff*d2r)))) * 2.d0)
end function rgetlen_sphere
!---------------------------------------------------------------
end program main
