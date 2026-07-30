program main
  use libspring, only: &
        spring_initialize         , &
        spring_finalize           , &
        spring_set_logopt         , &
        spring_define_mesh_latlon , &
        spring_define_mesh_raster , &
        spring_clear_mesh         , &
        spring_print_meshes_name  , &
        spring_print_mesh         , &
        spring_make_rmptbl        , &
        spring_clear_rmptbl       , &
        spring_get_rmptbl_length  , &
        spring_get_rmptbl_data    , &
        spring_print_rmptbl_name  , &
        spring_print_rmptbl       , &
        spring_remap              , &
        spring_print_error_message
  implicit none
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call remap_latlon_latlon()
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine remap_latlon_latlon()
  implicit none
  integer :: nsx, nsy, isx, isy
  integer :: ntx, nty, itx, ity
  logical :: s_is_south_to_north, t_is_south_to_north
  real(8), allocatable :: slon(:), slat(:)
  real(8), allocatable :: tlon(:), tlat(:)
  real(8) :: smiss, tmiss
  real(8), allocatable :: sdat(:,:), tdat(:,:)

  integer(8) :: nij
  integer(8), allocatable :: rtsidx(:), rttidx(:)
  real(8)   , allocatable :: rtarea(:), rtcoef(:)

  real(4), allocatable :: tmpr4(:,:)
  !-------------------------------------------------------------
  ! Gridsystem $s
  !-------------------------------------------------------------
  nsx = 720
  nsy = 360
  smiss = -9999.d0

  allocate(slon(0:nsx))
  allocate(slat(0:nsy))
  do isx = 0, nsx
    slon(isx) = -1.8d2 + 3.6d2*isx/nsx
  enddo
  do isy = 0, nsy
    slat(isy) = 9.d1 - 1.8d2*isy/nsy
  enddo
  s_is_south_to_north = .false.
  !-------------------------------------------------------------
  ! Grid system $t
  !-------------------------------------------------------------
  ntx = 288
  nty = 192
  tmiss = -9999.d0

  allocate(tlon(0:ntx))
  allocate(tlat(0:nty))
  do itx = 0, ntx
    tlon(itx) = 0.d0 + 3.6d2*itx/ntx
  enddo
  do ity = 0, nty
    tlat(ity) = -9.d1 + 1.8d2*ity/nty
  enddo
  t_is_south_to_north = .true.
  !-------------------------------------------------------------
  ! Read data of the source grid $s
  !-------------------------------------------------------------
  allocate(sdat(nsx,nsy))
  allocate(tdat(ntx,nty))

!  allocate(tmpr4(nsx,nsy))
!  open(11, file='../../dat/grid/test/latlon_05deg_elevtn.bin', form='unformatted', &
!       access='direct', recl=4*size(sdat), status='old')
!  read(11, rec=1) tmpr4
!  close(11)
!  sdat = real(tmpr4,8)
!  deallocate(tmpr4)
  sdat(:,:) = 1.d0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( spring_initialize(logopt='') /= 0 )then
    call spring_print_error_message()
  endif

  ! Define grid systems
  if( spring_define_mesh_latlon(&
         '05deg', nsx, nsy, &
         lon=slon, lat=slat, origin='north') /= 0 )then
    call spring_print_error_message()
  endif
  if( spring_define_mesh_latlon(&
         '288x192', ntx, nty, &
         0.d0, 360.d0, -90.d0, 90.d0) /= 0 )then
    call spring_print_error_message()
  endif

  ! Make remapping table
  if( spring_make_rmptbl('rt1', '05deg', '288x192') /= 0 )then
    call spring_print_error_message()
  endif

  ! Get generated remapping table
  if( spring_get_rmptbl_length('rt1', nij) /= 0 )then
    call spring_print_error_message()
  endif
  print*, 'nij ',nij

  ! Get remapping table and check values
  allocate(rtsidx(nij))
  allocate(rttidx(nij))
  allocate(rtarea(nij))
  allocate(rtcoef(nij))
  if( spring_get_rmptbl_data(&
        'rt1', rtsidx, rttidx, rtarea, rtcoef) /= 0 )then
    call spring_print_error_message()
  endif
  print*, 'sidx', minval(rtsidx), maxval(rtsidx)
  print*, 'tidx', minval(rttidx), maxval(rttidx)
  print*, 'area', minval(rtarea), maxval(rtarea)
  print*, 'coef', minval(rtcoef), maxval(rtcoef)

  ! Remap
  if( spring_remap('rt1', sdat, tdat, smiss, tmiss) /= 0 )then
    call spring_print_error_message()
  endif
  print*, 'sdat', minval(sdat), maxval(sdat)
  print*, 'tdat', minval(tdat), maxval(tdat)

!  open(11, file='test/out/latlon_288x192_elevtn.bin', form='unformatted', &
!       access='direct', recl=8*size(tdat), status='replace')
!  write(11, rec=1) tdat
!  close(11)
  !-------------------------------------------------------------
end subroutine remap_latlon_latlon
!===============================================================
!
!===============================================================
end program main
