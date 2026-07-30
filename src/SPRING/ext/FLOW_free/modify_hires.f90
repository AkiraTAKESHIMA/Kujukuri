program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c1_type_opt, only: &
        opt_earth_
  use def_consts
  use mod_utils, only: &
        read_conf_earth, &
        nextxy
  implicit none

  integer :: ix, iy, jx, jy, kx, ky

  real(8), parameter :: sealev = 0.d0

  integer(1), allocatable ::  flwdir(:,:)
  real(8)   , allocatable ::  elevtn(:,:)
  integer(1), allocatable ::  lndmsk(:,:)
  integer(1), allocatable ::  inland(:,:)

  real(8)    ::  elv_this
  integer(1) ::  mask_this

  ! input files
  !! flow direction
  character(128), parameter :: fflwdir = '1min_flwdir/flwdir.bin'
  !! elevation 
  character(128), parameter :: felevtn = '1min_flwdir/elevtn.bin'
  !! land mask (including land below 0m)
  character(128), parameter :: flndmsk = '1min_flwdir/lndmsk.bin'

  ! output files
  !! flow direction
  character(128), parameter :: wflwdir = 'tmp/1min/flwdir.bin'
  !! elevation 
  character(128), parameter :: welevtn = 'tmp/1min/elevtn.bin'
  !! land mask (including land below 0m)
  character(128), parameter :: wlndmsk = 'tmp/1min/mask.bin'
  !! 0:ocean, 1:river to ovean 2:inland river
  character(128), parameter :: winland = 'tmp/1min/inland.bin'

  call logbgn('program modify_hires', '', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(flwdir(nx,ny))
  allocate(elevtn(nx,ny))
  allocate(lndmsk(nx,ny))
  allocate(inland(nx,ny))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Reading flow direction '//str(fflwdir))
  call traperr( rbin(flwdir, fflwdir, dtype=dtype_int4) )

  call logmsg('Reading elevation      '//str(felevtn))
  call traperr( rbin(elevtn, felevtn, dtype=dtype_real) )

  call logmsg('Reading land mask      '//str(flndmsk))
  call traperr( rbin(lndmsk, flndmsk) )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Modifying hires flow direction map')

  do iy=1, ny
    do ix=1, nx
      if( lndmsk(ix,iy)==0 )then
        flwdir(ix,iy)=-9                                     !! ocean
      elseif( lndmsk(ix,iy)>0 )then
        if( flwdir(ix,iy)>=1 .and. flwdir(ix,iy)<=8 )then
          call nextxy(ix,iy,jx,jy,nx,flwdir(ix,iy))
          if( lndmsk(jx,jy)==0 ) flwdir(ix,iy)=0             !! river mouth
        else
          flwdir(ix,iy)=-1                                   !! inland mouth
        endif
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Modifying hires elevation map')

  do iy=1, ny
    do ix=1, nx
      if( lndmsk(ix,iy)==3 ) elevtn(ix,iy)=1.d20     !! originally ocean but changed to land
      if( lndmsk(ix,iy)==0 ) elevtn(ix,iy)=-9999     !! ocean
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( lndmsk(ix,iy)==1 .or. lndmsk(ix,iy)==2 )then      !! land
        if( flwdir(ix,iy)>=1 .and. flwdir(ix,iy)<=8 )then
          call nextxy(ix,iy,jx,jy,nx,flwdir(ix,iy))
          if( lndmsk(jx,jy)==3 )then
            elv_this=elevtn(ix,iy)
            do while( flwdir(jx,jy)>=1 .and. flwdir(jx,jy)<=8 )    !! route to mouth
              call nextxy(jx,jy,kx,ky,nx,flwdir(jx,jy))
              jx=kx
              jy=ky
            end do
            if( lndmsk(jx,jy)==3 .or. lndmsk(jx,jy)==11 )then    !! mouth is originally ocean
              elevtn(jx,jy)=min(elevtn(jx,jy),elv_this)
              lndmsk(jx,jy)=11
            endif
          endif
        endif
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( lndmsk(ix,iy)==3 )then
        jx=ix
        jy=iy
        do while( flwdir(jx,jy)>=1 .and. flwdir(jx,jy)<=8 .and. lndmsk(jx,jy)==3 )
          call nextxy(jx,jy,kx,ky,nx,flwdir(jx,jy))
          jx=kx
          jy=ky
        end do
        elv_this=elevtn(jx,jy)

        jx=ix
        jy=iy
        do while( flwdir(jx,jy)>=1 .and. flwdir(jx,jy)<=8 .and. lndmsk(jx,jy)==3 )
          elevtn(jx,jy)=elv_this
          lndmsk(jx,jy)=11
          call nextxy(jx,jy,kx,ky,nx,flwdir(jx,jy))
          jx=kx
          jy=ky
        end do
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( elevtn(ix,iy)==1.e20 ) elevtn(ix,iy)=0
      if( lndmsk(ix,iy)==11 ) lndmsk(ix,iy)=3
    end do
  end do

  inland(:,:)=0
  do iy=1, ny
    do ix=1, nx
      if( flwdir(ix,iy)==0  ) inland(ix,iy)=1
      if( flwdir(ix,iy)==-1 ) inland(ix,iy)=2
      if( flwdir(ix,iy)>0 )   inland(ix,iy)=-1
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( inland(ix,iy)==-1 )then
        jx=ix
        jy=iy
        do while( flwdir(jx,jy)>=1 .and. flwdir(jx,jy)<=8 .and. inland(jx,jy)==-1 )
          call nextxy(jx,jy,kx,ky,nx,flwdir(jx,jy))
          jx=kx
          jy=ky
        end do
        mask_this=inland(jx,jy)

        jx=ix
        jy=iy
        do while( flwdir(jx,jy)>=1 .and. flwdir(jx,jy)<=8 .and. inland(jx,jy)==-1 )
          inland(jx,jy)=mask_this
          call nextxy(jx,jy,kx,ky,nx,flwdir(jx,jy))
          jx=kx
          jy=ky
        end do
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( flwdir(ix,iy)==0 )then
        if( elevtn(ix,iy)<0 ) print *, ix, iy, elevtn(ix,iy)
        elevtn(ix,iy)=0
      endif
    end do
  end do

  do iy=1, ny
    do ix=1, nx
      if( lndmsk(ix,iy)==1 )then
        if( elevtn(ix,iy)< 0 ) elevtn(ix,iy)=0
      endif
    end do
  end do

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Writing flow direction '//str(wflwdir))
  call traperr( wbin(flwdir, wflwdir) )

  call logmsg('Writing land mask      '//str(wlndmsk))
  call traperr( wbin(lndmsk, wlndmsk) )

  call logmsg('Writing elevation      '//str(welevtn))
  call traperr( wbin(elevtn, welevtn) )

  call logmsg('Writing inland         '//str(winland))
  call traperr( wbin(inland, winland) )
  !-------------------------------------------------------------
  call logret()
end program main
