module mod_driver
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
  implicit none

  call prep_forcing()

  call prep_river()

  call prep_slope()

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

  real(8), allocatable :: inith(:,:)
  integer :: i, j

  hr = -0.1d0
  hs = -0.1d0
  hg = -0.1d0
  gampt_ff = 0.d0
  !-------------------------------------------------------------
  ! hr
  ! if init_riv_switch = 1 => read from file
  !-------------------------------------------------------------
  where( riv == 1 ) hr = 0.d0

  if(init_riv_switch .eq. 1) then
    allocate( inith(ny, nx) )
    inith = 0.d0
    open(13, file = initfile_riv, status = "old")
    do i = 1, ny
      read(13,*) (inith(i,j), j = 1, nx)
    enddo
    where(inith .le. 0.d0) inith = 0.d0
    where(riv.eq.1 .and. inith .ge. 0.d0) hr = inith
    deallocate( inith )
    close(13)
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
  integer :: i, j

  integer :: un_hr, un_hs

  open(newunit=un_hr, file=trim(dir_out)//'/hr.bin', &
       form='unformatted', access='direct', recl=8_8*nx*ny, status='replace')
  open(newunit=un_hs, file=trim(dir_out)//'/hs.bin', &
       form='unformatted', access='direct', recl=8_8*nx*ny, status='replace')

  time = 0.d0

  call calc_water_budget(time, hr, hs, gampt_ff, hg)

  it_out = 1

  do it_model = 1, nt_model
    time_next = it_model * dt_model
    print"(1x,2(a,i6),2(a,f10.2))", &
      't: ',it_model,' / ',nt_model, ' time: ', time, ' - ', time_next

    call load_rain(time, time_next)
    !-----------------------------------------------------------
    ! 2D -> 1D
    !-----------------------------------------------------------
    call reshape_riv_ij2idx( hr, hr_idx )
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
    ! 1D -> 2D
    !-----------------------------------------------------------
    call reshape_riv_idx2ij( hr_idx, hr )
    call reshape_slo_idx2ij( hs_idx, hs )
    call reshape_slo_idx2ij( hg_idx, hg )
    call reshape_slo_idx2ij( gampt_ff_idx, gampt_ff )
    !-----------------------------------------------------------
    ! River-slope interactions
    !-----------------------------------------------------------
    if( riv_thresh >= 0 ) call funcrs( hr, hs, qrs )

    call reshape_riv_ij2idx( hr, hr_idx )
    call reshape_slo_ij2idx( hs, hs_idx )
    !-----------------------------------------------------------
    ! Infiltration (Green-Ampt)
    !-----------------------------------------------------------
    call infilt( hs_idx, gampt_ff_idx, gampt_f_idx )

    call reshape_slo_idx2ij( hs_idx, hs )
    call reshape_slo_idx2ij( gampt_ff_idx, gampt_ff )
    call reshape_slo_idx2ij( gampt_f_idx, gampt_f )
    !-----------------------------------------------------------
    ! Set water depth 0 at outlets
    !-----------------------------------------------------------
    call outflow_river(hr)
    call outflow_slope(hs)
    !-----------------------------------------------------------
    ! Update the time
    !-----------------------------------------------------------
    time = time_next
    !-----------------------------------------------------------
    ! Summary
    !-----------------------------------------------------------
    print*, 'max hr: ',maxval(hr),' loc: ',maxloc(hr)
    print*, 'max hs: ',maxval(hs),' loc: ',maxloc(hs)
    if( gw_switch == 1 ) print*, 'max hg: ', maxval(hg),' loc: ',maxloc(hg)

    where( domain == DOMAIN__OUTSIDE ) hr = -0.1d0

    do while( it_out * dt_out <= time )
      print*, 'output ', it_out
      write(un_hr, rec=it_out) hr
      write(un_hs, rec=it_out) hs
      it_out = it_out + 1
    enddo

    call calc_water_budget(time, hr, hs, gampt_ff, hg)
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
