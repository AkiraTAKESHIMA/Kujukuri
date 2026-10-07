module mod_driver
  use lib_const
  use lib_base
  use lib_log
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: prep_driver
  public :: exec_simulation
  public :: finalize
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  real(8), allocatable :: hr(:,:), hr_idx(:)
  real(8), allocatable :: hs(:,:), hs_idx(:)
  real(8), allocatable :: hg(:,:), hg_idx(:)
  real(8), allocatable :: gampt_ff(:,:), gampt_ff_idx(:)
  real(8), allocatable :: gampt_f(:,:), gampt_f_idx(:)
  real(8), allocatable :: qrs(:,:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_driver()
  use mod_forcing, only: &
    prep_forcing
  use mod_river, only: &
    prep_river
  use mod_slope, only: &
    prep_slope
  use mod_rivslo, only: &
    prep_rivslo
  implicit none

  call prep_forcing()

  call prep_river()

  call prep_slope()

  call prep_rivslo()

  allocate(hr(nx,ny))
  allocate(hr_idx(riv_count))

  allocate(hs(nx,ny))
  allocate(hs_idx(slo_count))

  allocate(hg(nx,ny))
  allocate(hg_idx(slo_count))

  allocate(gampt_ff(nx,ny))
  allocate(gampt_ff_idx(slo_count))

  allocate(gampt_f(nx,ny))
  allocate(gampt_f_idx(slo_count))

  allocate(qrs(nx,ny))

  call load_initial_conditions()
end subroutine prep_driver
!===============================================================
!
!===============================================================
subroutine load_initial_conditions()
  use mod_base
  implicit none

  real(8), allocatable :: inith(:,:), inith_idx(:)
  integer :: i, j
  integer :: k
  integer :: un

  hr_idx = -0.1d0
  hs = -0.1d0
  hg = -0.1d0
  gampt_ff = 0.d0
  !-------------------------------------------------------------
  ! hr_idx
  ! if init_riv_switch = 1 => read from file
  !-------------------------------------------------------------
  hr_idx = 0.d0

  if(init_riv_switch .eq. 1) then
    allocate( inith_idx(riv_count) )
    inith = 0.d0
    open(newunit=un, file = initfile_riv, status = "old")
    do k = 1, riv_count
      read(un,*) inith_idx(k)
    enddo
    close(un)
    where( inith_idx >= 0.d0 ) hr_idx = inith_idx
    deallocate( inith_idx )
  endif
  !-------------------------------------------------------------
  ! hs
  ! if init_slo_switch = 1 => read from file
  !-------------------------------------------------------------
  where( domain == 1 )
    hs = 0.d0
  elsewhere( domain == 2 )
    hs = 0.d0
  endwhere

  if(init_slo_switch .eq. 1) then
    allocate( inith(ny, nx) )
    inith = 0.d0
    open(13, file = initfile_slo, status = "old")
    do i = 1, ny
      read(13,*) (inith(i,j), j = 1, nx)
    enddo
    where(inith .le. 0.d0) inith = 0.d0
    where(domain.eq.1 .and. inith .ge. 0.d0) hs = inith
    deallocate( inith )
    close(13)
  endif
  !-------------------------------------------------------------
  ! hg
  ! if init_gw_switch = 1 => read from file
  !-------------------------------------------------------------
  if(init_gw_switch .eq. 1) then
    allocate( inith(ny, nx) )
    inith = 0.d0
    open(13, file = initfile_gw, status = "old")
    do i = 1, ny
      read(13,*) (inith(i,j), j = 1, nx)
    enddo
    where(inith .le. 0.d0) inith = 0.d0
    where(domain.eq.1 .and. inith .ge. 0.d0) hg = inith
    deallocate( inith )
    close(13)
  else
    hg_idx(:) = 0.d0
    call reshape_slo_idx2ij( hg_idx, hg )
  endif
  !-------------------------------------------------------------
  ! gampt_ff
  ! if init_gampt_ff_switch = 1 => read from file
  !-------------------------------------------------------------
  if(init_gampt_ff_switch .eq. 1) then
    allocate( inith(ny, nx) )
    inith = 0.d0
    open(13, file = initfile_gampt_ff, status = "old")
    do i = 1, ny
      read(13,*) (inith(i,j), j = 1, nx)
    enddo
    where(inith .le. 0.d0) inith = 0.d0
    where(domain.eq.1) gampt_ff = inith
    deallocate( inith )
    close(13)
  endif
end subroutine load_initial_conditions
!===============================================================
!
!===============================================================
subroutine exec_simulation()
  use mod_base
  use mod_forcing, only: &
    load_rain
  use mod_budget, only: &
    calc_water_budget
  use mod_river, only: &
    advance_river, &
    outflow_river
  use mod_slope, only: &
    advance_slope, &
    infilt       , &
    outflow_slope
  use mod_gwat, only: &
    advance_gwat
  use mod_rivslo, only: &
    funcrs
  implicit none
  real(8) :: time, time_next
  integer :: it_model
  integer :: it_out

  integer :: un_hr, un_hs

  open(newunit=un_hr, file=trim(dir_out)//'/hr.bin', &
       form='unformatted', access='direct', recl=8_8*nx*ny, status='replace')
  open(newunit=un_hs, file=trim(dir_out)//'/hs.bin', &
       form='unformatted', access='direct', recl=8_8*nx*ny, status='replace')

  time = 0.d0

  call calc_water_budget(time, hr_idx, hs, gampt_ff, hg)

  it_out = 1

  do it_model = 1, nt_model
    time_next = it_model * dt_model
    call logmsg('t: '//str(it_model)//' / '//str(nt_model)//&
        ' time: '//str(time)//' - '//str(time_next))

    call load_rain(time, time_next)
    !-----------------------------------------------------------
    ! 2D -> 1D
    !-----------------------------------------------------------
    call reshape_slo_ij2idx( hs, hs_idx )
    call reshape_slo_ij2idx( hg, hg_idx )
    call reshape_slo_ij2idx( gampt_ff, gampt_ff_idx )
    !-----------------------------------------------------------
    ! River
    !-----------------------------------------------------------
    call advance_river( time, hr_idx )
    !-----------------------------------------------------------
    ! Slope
    !-----------------------------------------------------------
    call advance_slope( time, hs_idx )
    !-----------------------------------------------------------
    ! Ground water
    !-----------------------------------------------------------
    if( gw_switch /= 0 )then
      call advance_gwat( time, hs_idx, gampt_ff_idx, hg_idx )
    endif
    !-----------------------------------------------------------
    ! River-slope interactions
    !-----------------------------------------------------------
    call funcrs( hr_idx, hs_idx, qrs )
    !-----------------------------------------------------------
    ! Infiltration (Green-Ampt)
    !-----------------------------------------------------------
    call infilt( hs_idx, gampt_ff_idx, gampt_f_idx )
    !-----------------------------------------------------------
    ! 1D -> 2D
    !-----------------------------------------------------------
    call reshape_slo_idx2ij( hs_idx, hs )
    call reshape_slo_idx2ij( gampt_ff_idx, gampt_ff )
    call reshape_slo_idx2ij( gampt_f_idx, gampt_f )

    call reshape_slo_idx2ij( hg_idx, hg )
    !-----------------------------------------------------------
    ! Set water depth 0 at outlets
    !-----------------------------------------------------------
    call outflow_river(hr_idx)
    call outflow_slope(hs)
    !-----------------------------------------------------------
    ! Update the time
    !-----------------------------------------------------------
    time = time_next
    !-----------------------------------------------------------
    ! Summary
    !-----------------------------------------------------------
    print*, 'max hr: ',maxval(hr_idx),' loc: ',maxloc(hr_idx)
    print*, 'max hs: ',maxval(hs),' loc: ',maxloc(hs)
    if( gw_switch == 1 ) print*, 'max hg: ', maxval(hg),' loc: ',maxloc(hg)

    do while( it_out * dt_out <= time )
      print*, 'output ', it_out
      write(un_hr, rec=it_out) hr_idx
      write(un_hs, rec=it_out) hs
      it_out = it_out + 1
    enddo

    call calc_water_budget(time, hr_idx, hs, gampt_ff, hg)
  enddo  ! it_model = 1, nt_model

  close(un_hr)
  close(un_hs)
end subroutine exec_simulation
!===============================================================
!
!===============================================================
subroutine finalize()
  use mod_forcing, only: &
    finalize_forcing 
  implicit none

  call finalize_forcing()
end subroutine finalize
!===============================================================
!
!===============================================================
end module mod_driver
