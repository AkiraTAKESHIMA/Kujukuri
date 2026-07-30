module mod_check_input
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_io
  use lib_math
  use c1_const
  use c2_type_rt
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: check_relations_input
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_check_input'

  character(CLEN_KEY), parameter :: MODE_RERR_TINY     = 'tiny'
  character(CLEN_KEY), parameter :: MODE_RERR_NEGATIVE = 'negative'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine check_relations_input(rt_in, rm)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_relations_input'
  type(rt_in_), intent(in) :: rt_in
  type(rm_)   , intent(in) :: rm

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! rt_in%rm_river_to_agcm and rm%grdara_river
  !-------------------------------------------------------------
  call logent('Checking relations of rt_rm_river_to_agcm and grid_rm_river', PRCNAM, MODNAM)

  call check_relations_rt_grid(&
         rt_in%rm_river_to_agcm%main, MESH__SOURCE, &
         rm%fin_grdidx_river, rm%fin_grdara_river, rm%nij, rm%idx_miss, &
         MODE_RERR_TINY)

  call logext()
  !-------------------------------------------------------------
  ! rt_in%rm_noriv_to_agcm and rm%grdara_noriv
  !-------------------------------------------------------------
  call logent('Checking relations of rt_rm_noriv_to_agcm and grid_rm_noriv', PRCNAM, MODNAM)

  call check_relations_rt_grid(&
         rt_in%rm_noriv_to_agcm%main, MESH__SOURCE, &
         rm%fin_grdidx_noriv, rm%fin_grdara_noriv, rm%nij, rm%idx_miss, &
         MODE_RERR_TINY)

  call logext()
  !-------------------------------------------------------------
  ! rt_in%rm_ocean_to_agcm and rm%grdara_ocean
  !-------------------------------------------------------------
  call logent('Checking relations of rt_rm_ocean_to_agcm and grid_rm_ocean', PRCNAM, MODNAM)

  call check_relations_rt_grid(&
         rt_in%rm_ocean_to_agcm%main, MESH__SOURCE, &
         rm%fin_grdidx_ocean, rm%fin_grdara_ocean, rm%nij, rm%idx_miss, &
         MODE_RERR_TINY)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_relations_input
!===============================================================
!
!===============================================================
subroutine check_relations_rt_grid(&
    rtm, mesh_which, &
    f_grdidx, f_grdara, ngij, idx_miss, &
    mode_rerr_grdara)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_relations_rt_grid'
  type(rt_main_), intent(in), target :: rtm
  character(*)  , intent(in)         :: mesh_which
  type(file_)   , intent(in), target :: f_grdidx, f_grdara
  integer(8)    , intent(in)         :: ngij
  integer(8)    , intent(in)         :: idx_miss
  character(*)  , intent(in)         :: mode_rerr_grdara

  type(file_), pointer :: f
  integer(8), allocatable :: rt_grid(:)
  real(8)   , allocatable :: rt_area(:)
  integer(8), allocatable :: grdidx(:)
  integer(8), allocatable :: grdidxarg(:)
  real(8)   , allocatable :: grdara(:)
  real(8)   , allocatable :: grdara_rt(:)
  real(8)   , allocatable :: rerr_grdara(:)
  integer(8) :: gij
  integer(8) :: rtij
  integer(8) :: loc
  real(8), parameter :: val_miss = -1d20
  real(8), parameter :: rerr_grdara_llim = 1d-5

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Length of rt: '//str(rtm%nij))
  allocate(rt_grid(rtm%nij))
  allocate(rt_area(rtm%nij))

  call logmsg('Length of grid data: '//str(ngij))
  allocate(grdidx(ngij))
  allocate(grdidxarg(ngij))
  allocate(grdara(ngij))
  allocate(grdara_rt(ngij))
  allocate(rerr_grdara(ngij))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking grdidx and grdara', PRCNAM, MODNAM)

  f => f_grdidx
  if( f%path /= '' )then
    call logmsg('Reading grdidx')
    call traperr( rbin(grdidx, f%path, f%dtype, f%endian, f%rec) )
  else
    do gij = 1_8, ngij
      grdidx(gij) = gij
    enddo
  endif

  call logmsg('  min: '//str(minval(grdidx,mask=grdidx/=idx_miss))//&
               ' max: '//str(maxval(grdidx,mask=grdidx/=idx_miss)))

  call argsort(grdidx, grdidxarg)

  f => f_grdara
  call logmsg('Reading grdara')
  call traperr( rbin(grdara, f%path, f%dtype, f%endian, f%rec) )

  call logmsg('  min: '//str(minval(grdara,mask=grdidx/=idx_miss))//&
               ' max: '//str(maxval(grdara,mask=grdidx/=idx_miss)))

  do gij = 1_8, ngij
    if( (grdidx(gij) == idx_miss .and. grdara(gij) > 0.d0) .or. &
        (grdidx(gij) /= idx_miss .and. grdara(gij) <= 0.d0) )then
      call errend(msg_unexpected_condition()//&
                '\n  gij: '//str(gij)//&
                '\n  grdidx: '//str(grdidx(gij))//&
                '\n  grdara: '//str(grdara(gij)))
    endif
  enddo

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating grdara from rt', PRCNAM, MODNAM)

  selectcase( mesh_which )
  case( MESH__SOURCE )
    f => rtm%f%sidx
    call logmsg('Reading rt_sidx')
    call traperr( rbin(rt_grid, f%path, f%dtype, f%endian, f%rec) )
  case( MESH__TARGET )
    f => rtm%f%tidx
    call logmsg('Reading rt_tidx')
    call traperr( rbin(rt_grid, f%path, f%dtype, f%endian, f%rec) )
  case default
    call errend(msg_invalid_value('mesh_which', mesh_which))
  endselect

  f => rtm%f%area
  call logmsg('Reading rt_area')
  call traperr( rbin(rt_area, f%path, f%dtype, f%endian, f%rec) )

  grdara_rt(:) = 0.d0
  do rtij = 1_8, rtm%nij
    call search(rt_grid(rtij), grdidx, grdidxarg, loc)
    if( loc == 0_8 )then
      call errend(msg_unexpected_condition()//&
                '\n  Index '//str(rt_grid(rtij))//' in rt was not found in grdidx.')
    endif
    call add(grdara_rt(grdidxarg(loc)), rt_area(rtij))
  enddo

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Checking rerr of grdara', PRCNAM, MODNAM)

  rerr_grdara(:) = val_miss
  do gij = 1_8, ngij
    if( grdidx(gij) == idx_miss ) cycle

    rerr_grdara(gij) = (grdara_rt(gij) - grdara(gij)) / grdara(gij)

    selectcase( mode_rerr_grdara )
    case( MODE_RERR_TINY )
      if( abs(rerr_grdara(gij)) > rerr_grdara_llim )then
        call errend(msg_unexpected_condition()//&
                  '\n  abs(rerr_grdara) > rerr_grdara_llim'//&
                  '\n  gij: '//str(gij)//&
                  '\n  idx: '//str(grdidx(gij))//&
                  '\n  grdara     : '//str(grdara(gij))//&
                  '\n  grdara_rt  : '//str(grdara_rt(gij))//&
                  '\n  rerr_grdara: '//str(rerr_grdara(gij)))
      endif
    case( MODE_RERR_NEGATIVE )
      if( rerr_grdara(gij) > rerr_grdara_llim )then
        call errend(msg_unexpected_condition()//&
                  '\n  rerr_grdara > 0.0'//&
                  '\n  gij: '//str(gij)//&
                  '\n  idx: '//str(grdidx(gij))//&
                  '\n  grdara     : '//str(grdara(gij))//&
                  '\n  grdara_rt  : '//str(grdara_rt(gij))//&
                  '\n  rerr_grdara: '//str(rerr_grdara(gij)))
      endif
    case default
      call errend(msg_invalid_value('mode_rerr_grdara', mode_rerr_grdara))
    endselect
  enddo  ! gij/

  call logmsg('min: '//str(minval(rerr_grdara,mask=grdidx/=idx_miss))//&
             ' max: '//str(maxval(rerr_grdara,mask=grdidx/=idx_miss)))

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(rt_grid)
  deallocate(rt_area)

  deallocate(grdidx)
  deallocate(grdidxarg)
  deallocate(grdara)
  deallocate(grdara_rt)
  deallocate(rerr_grdara)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_relations_rt_grid
!===============================================================
!
!===============================================================
end module mod_check_input
