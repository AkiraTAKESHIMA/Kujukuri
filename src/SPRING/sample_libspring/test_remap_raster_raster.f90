program main
  use libspring
  implicit none
  integer :: nadx, nady
  real(8) :: awest, aeast, asouth, anorth
  integer, allocatable :: aidxmap(:,:)
  integer :: aidx_miss

  integer :: nbdx, nbdy
  integer, allocatable :: bidxmap(:,:)
  integer :: bidx_miss

  character(1) :: c_
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( spring_initialize(logopt='') /= 0 )then
    call spring_print_error_message()
  endif
  !-------------------------------------------------------------
  ! Grid system "a" (Numerical Land Information mesh5 watersystem code)
  !-------------------------------------------------------------
  open(11, file='../../dat/grid_system/NLI/wsCode/mesh5/5340.txt', status='old')
  read(11,*)
  read(11,*) c_, awest, asouth, aeast, anorth
  read(11,*) c_, nadx, nady
  close(11)
  aidx_miss = -9999

  allocate(aidxmap(nadx,nady))
  open(11, file='../../dat/grid_system/NLI/wsCode/mesh5/5340.bin', &
       form='unformatted', access='direct', recl=nadx*nady*4, &
       convert='little_endian', status='old')
  read(11, rec=1) aidxmap
  close(11)

  if( spring_define_mesh_raster(&
        'NLI', nadx, nady, awest, aeast, asouth, anorth, &
        aidxmap, aidx_miss) /= 0 )then
    call spring_print_error_message()
  endif
  !-------------------------------------------------------------
  ! Grid system "b" (CaMa-Flood 1min)
  !-------------------------------------------------------------
  nbdx = 21600
  nbdy = 10800
  bidx_miss = -9999

  allocate(bidxmap(nbdx,nbdy))
  open(11, file='../../dat/grid_system/CaMa-Flood/CaMa_v407/glb_06min/matsiro/1min/raster/index_river.bin', &
       form='unformatted', access='direct', recl=nbdx*nbdy*4, &
       convert='little_endian', status='old')
  read(11, rec=1) bidxmap
  close(11)

  if( spring_define_mesh_raster(&
        'CMF', nbdx, nbdy, idx=bidxmap, idx_miss=bidx_miss) /= 0 )then
    call spring_print_error_message()
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( spring_make_rmptbl('rt1', 'NLI', 'CMF') /= 0 )then
    call spring_print_error_message()
  endif
  !-------------------------------------------------------------
  ! Test "clear_mesh"
  !-------------------------------------------------------------
  call spring_print_meshes_name()

  if( spring_clear_mesh('CMF') /= 0 )then
    call spring_print_error_message()
  endif

  call spring_print_meshes_name()
  !-------------------------------------------------------------
  ! Test "clear_rt"
  !-------------------------------------------------------------
  call spring_print_rmptbl_name() 

  if( spring_print_rmptbl('rt1') /= 0 )then
    call spring_print_error_message()
  endif

  if( spring_clear_rmptbl('rt1') /= 0 )then
    call spring_print_error_message()
  endif

  call spring_print_rmptbl_name()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( spring_finalize() /= 0 )then
    call spring_print_error_message()
  endif
end program main
