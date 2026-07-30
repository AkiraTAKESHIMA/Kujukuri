program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_type_opt, only: &
        opt_earth_
  use def_consts
  use mod_utils, only: &
        read_conf_earth, &
        nextxy
  implicit none

  integer, parameter :: nCX = 360
  integer, parameter :: nCY = 180

  integer :: nXX, nYY, iXX, iYY, XX, YY
  integer :: ix, iy, xs, xe, ys, ye
  integer :: iCX, iCY, iCXY
  integer :: mx, my
  integer :: ninp, i
  integer, allocatable :: catmXX(:,:), catmYY(:,:)
  real(8), allocatable :: pixlat(:)
  real(8), allocatable :: pixare(:)
  integer, allocatable :: inpx(:,:,:)
  integer, allocatable :: inpy(:,:,:)
  real(8), allocatable :: inpa(:,:,:)

  type map_iCXY_
    integer :: n
    integer :: nmax
    integer, pointer :: iCXY(:)
  end type
  type(map_iCXY_), pointer :: map_iCXY(:,:)
  type(map_iCXY_), pointer :: miCXY

  logical :: found

  ! earth's params
  type(opt_earth_) :: earth

  ! file
  character(128), parameter :: fparams  = 'params.txt'
  character(128), parameter :: rfile1   = 'tmp/map/1min/catmxy.bin'
  character(128), parameter :: finpmat  = 'map/inpmat.bin'
  character(128), parameter :: fdiminfo = 'map/diminfo.txt'

  call logbgn('program calc_inpmat', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(11, file=fparams, status='old')
  read(11,*) nXX
  read(11,*) nYY
  read(11,*) ! fgcmidx

  call read_conf_earth(earth, 11)

  close(11)

  allocate(catmXX(nx,ny))
  allocate(catmYY(nx,ny))

  call traperr( rbin(catmXX, rfile1, rec=1) )
  call traperr( rbin(catmYY, rfile1, rec=2) )

  mx = nx / nCX
  my = ny / nCY

  allocate(pixare(ny))
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
  ! Count num. of layers
  !-------------------------------------------------------------
  call logent('Counting num. of layers')

  allocate(map_iCXY(nXX,nYY))
  map_iCXY(:,:)%n = 0
  map_iCXY(:,:)%nmax = 10
  do iYY = 1, nYY
    do iXX = 1, nXX
      miCXY => map_iCXY(iXX,iYY)
      allocate(miCXY%iCXY(miCXY%nmax))
      miCXY%iCXY(:) = 0
    enddo
  enddo

  do iCY = 1, nCY
    ys = (iCY-1)*my + 1
    ye = iCY*my

    do iCX = 1, nCX
      xs = (iCX-1)*mx + 1
      xe = iCX*mx

      iCXY = (iCY-1)*nCX + iCX

      do iy = ys, ye
        do ix = xs, xe
          XX = catmXX(ix,iy)
          YY = catmYY(ix,iy)

          if( XX <= 0 ) cycle

          miCXY => map_iCXY(XX,YY)

          found = .false.
          do i = 1, miCXY%n
            if( miCXY%iCXY(i) == iCXY )then
              found = .true.
              exit
            endif
          enddo

          if( .not. found )then
            if( miCXY%n == miCXY%nmax )then
              miCXY%nmax = miCXY%nmax * 2
              call realloc(miCXY%iCXY, miCXY%nmax, clear=.false.)
            endif
            call add(miCXY%n)
            miCXY%iCXY(miCXY%n) = iCXY
          endif

        enddo  ! ix/
      enddo  ! iy/

    enddo  ! iCX/
  enddo  ! iCY/

  ninp = maxval(map_iCXY(:,:)%n)

  call logmsg('ninp: '//str(ninp))

  call logext()
  !-------------------------------------------------------------
  ! Make inpmat
  !-------------------------------------------------------------
  call logent('Making inpmat')

  allocate(inpx(nXX,nYY,ninp))
  allocate(inpy(nXX,nYY,ninp))
  allocate(inpa(nXX,nYY,ninp))

  inpx(:,:,:) = 0
  inpy(:,:,:) = 0
  inpa(:,:,:) = 0.d0

  do iCY = 1, nCY
    ys = (iCY-1)*my + 1
    ye = iCY*my

    do iCX = 1, nCX
      xs = (iCX-1)*mx + 1
      xe = iCX*mx

      iCXY = (iCY-1)*nCX + iCX

      do iy = ys, ye
        do ix = xs, xe
          XX = catmXX(ix,iy)
          YY = catmYY(ix,iy)

          if( XX <= 0 ) cycle

          miCXY => map_iCXY(XX,YY)

          found = .false.
          do i = 1, miCXY%n
            if( miCXY%iCXY(i) == iCXY )then
              found = .true.
              exit
            endif
          enddo

          if( .not. found )then
            call errend(msg_unexpected_condition()//&
                      '\n  iCXY '//str(iCXY)//' was not found in the list.')
          endif

          inpx(XX,YY,i) = iCX
          inpy(XX,YY,i) = iCY
          call add(inpa(XX,YY,i), pixare(iy))
        enddo  ! ix/
      enddo  ! iy/

    enddo  ! iCX/
  enddo  ! iCY/

  nullify(miCXY)
  deallocate(map_iCXY)

  call logext()
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  call logent('Outputting')

  ! inpmat
  !-------------------------------------------------------------
  do i = 1, ninp
    call traperr( wbin(inpx(:,:,i), finpmat, dtype=dtype_int4, rec=ninp*0+i) )
  enddo

  do i = 1, ninp
    call traperr( wbin(inpy(:,:,i), finpmat, dtype=dtype_int4, rec=ninp*1+i) )
  enddo

  do i = 1, ninp
    call traperr( wbin(inpa(:,:,i), finpmat, dtype=dtype_real, rec=ninp*2+i) )
  enddo

  ! diminfo
  !-------------------------------------------------------------
  open(11, file=fdiminfo, status='replace')

  write(11,'(i10,5x,a)') nXX,          '!! nXX'
  write(11,'(i10,5x,a)') nYY,          '!! nYY'
  write(11,'(i10,5x,a)') nflp,         '!! floodplain layer'
  write(11,'(i10,5x,a)') nCX,          '!! input nXX '
  write(11,'(i10,5x,a)') nCY,          '!! input nYY '
  write(11,'(i10,5x,a)') ninp,         '!! input nLL'
  write(11,'(a)')        trim(filename(finpmat))
  write(11,'(f12.3,5x,a)') -180.,       '!! west  edge'
  write(11,'(f12.3,5x,a)') 180.,        '!! east  edge'
  write(11,'(f12.3,5x,a)') 90.,         '!! north edge'
  write(11,'(f12.3,5x,a)') -90.,        '!! south edge'
  close(11)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(inpx)
  deallocate(inpy)
  deallocate(inpa)
  !-------------------------------------------------------------
  call logret()
end program main
