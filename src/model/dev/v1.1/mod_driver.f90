module mod_driver
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use lib_io
  use def_const
  use def_type
  use mod_global
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: run
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PATH), parameter :: MODNAM = 'mod_driver'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine run()
!subroutine run(&
!  time, grid, solver, slope, river, forcing, output &
!)
  use mod_base, only: &
    ij2idx, &
    idx2ij
  use mod_time, only: &
    strftime, &
    advance_time
  use mod_forcing, only: &
    open_file_forcing, &
    close_file_forcing, &
    load_forcing_data
  use mod_slope, only: &
    init_mod_slope, &
    advance_slope, &
    h2sfc
  use mod_river, only: &
    init_mod_river, &
    init_river_storage, &
    advance_river
  use mod_interaction, only: &
    interact_slope_river
  use mod_budget, only: &
    calc_storage, &
    calc_storage_inbalance
  implicit none
  character(CLEN_PATH), parameter :: PRCNAM = 'run'
!  type(time_), intent(inout) :: time
!  type(grid_), intent(in) :: grid
!  type(solver_), intent(in) :: solver
!  type(static_slope_), intent(in) :: slope
!  type(static_river_), intent(in) :: river
!  type(forcing_), intent(inout) :: forcing
!  type(output_), intent(in) :: output

  ! State variables
  real(8), allocatable :: hs(:,:)
  real(8), allocatable :: hg(:,:)
  real(8), allocatable :: hr_idx(:)
  real(8), allocatable :: hs_idx(:)
  real(8), allocatable :: hg_idx(:)

  ! Diagnostic variables
  real(8), allocatable :: sfc(:,:)
  real(8), allocatable :: sfc_idx(:)

  ! Flux
  real(8), allocatable :: qp(:,:)
  real(8), allocatable :: qp_idx(:)
  real(8), allocatable :: qr_ave_idx(:)  !(nCh)
  real(8), allocatable :: qs_ave_idx(:,:)  !(nDir,nGrid)
  real(8), allocatable :: qrs(:,:)  ! (nx,ny)
  real(8), allocatable :: vro_idx(:)  !(nCh)

  ! ?
  real(8), allocatable :: gampt_ff(:,:)  !(nx,ny)
  real(8), allocatable :: gampt_f(:,:)  !(nx,ny)
  real(8), allocatable :: gampt_ff_idx(:)  !(nGrid)

  ! Water budget
  real(8) :: prcp_sum, aevp_sum, pevp_sum, &
             sr, ss, sg, si, &
             sout, sinit, sinbl
  real(8) :: vsr_tot
  real(8) :: prcp_sum_prev, aevp_sum_prev, pevp_sum_prev, &
             sr_prev, ss_prev, sg_prev, si_prev, sout_prev
  real(8) :: prcp_sum_inc, aevp_sum_inc, &
             sr_inc, ss_inc, sg_inc, si_inc, sout_inc
  real(8) :: inc_inbalance
  real(8) :: slo_inbl, riv_inbl

  ! Time
  integer :: count_model_max

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(qp(slope%nx,slope%ny))
  allocate(qp_idx(slope%nGrid))

  allocate(hr_idx(river%nCh))

  allocate(hs(slope%nx,slope%ny))
  allocate(hs_idx(slope%nGrid))

  allocate(sfc(slope%nx,slope%ny))
  allocate(sfc_idx(slope%nGrid))

  allocate(hg(slope%nx,slope%ny))
  allocate(hg_idx(slope%nGrid))
  allocate(gampt_ff(slope%nx,slope%ny))
  allocate(gampt_ff_idx(slope%nGrid))
  allocate(gampt_f(slope%nx,slope%ny))

  allocate(qr_ave_idx(river%nCh))
  allocate(qs_ave_idx(slope%nDir,slope%nGrid))
  allocate(qrs(slope%nx,slope%ny))
  allocate(vro_idx(river%nCh))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  hr_idx(:) = 0.d0

  hs_idx(:) = 0.d0
  call idx2ij(slope, hs_idx, hs)

  sfc_idx(:) = 0.d0
  call idx2ij(slope, sfc_idx, sfc)

  hg_idx(:) = 0.d0
  call idx2ij(slope, hg_idx, hg)
  gampt_ff(:,:) = 0.d0
  gampt_f(:,:) = 0.d0
  qrs(:,:) = 0.d0
  vro_idx(:) = 0.d0
  !-------------------------------------------------------------
  ! Init. modules
  !-------------------------------------------------------------
  call init_mod_river()
  call init_mod_slope()
  !-------------------------------------------------------------
  ! Init. river storage
  !-------------------------------------------------------------
  call init_river_storage(hr_idx)
  !-------------------------------------------------------------
  ! Initial storage calculation
  !-------------------------------------------------------------
  prcp_sum = 0.d0
  aevp_sum = 0.d0
  pevp_sum = 0.d0
  sout = 0.d0
  si = 0.d0
  sg = 0.d0
  call calc_storage(&
    slope, river, &
    hr_idx, hs, hg, gampt_ff, &
    vro_idx, &
    sr, ss, si, sg, sout &
  )
  sinit = sr + ss + si + sg

  call calc_storage_inbalance(&
    prcp_sum, aevp_sum, sout, sr, ss, si, sg, sinit, sinbl &
  )
  !-------------------------------------------------------------
  ! Output initial state
  !-------------------------------------------------------------
  call traperr( wbin( &
     hs, joined(output%dir,'hs1.bin'), rec=1, replace=.true. &
  ) )

  call traperr( wbin( &
     hs, joined(output%dir,'hs.bin'), rec=1, replace=.true. &
  ) )

  call traperr( wbin( &
     sfc, joined(output%dir,'sfc.bin'), rec=1, replace=.true. &
  ) )

  call traperr( wbin( &
    hr_idx, joined(output%dir,'hr1.bin'), rec=1, replace=.true. &
  ) )

  call traperr( wbin( &
    hr_idx, joined(output%dir,'hr.bin'), rec=1, replace=.true. &
  ) )

  call output_river_src2out_attr(&
    river, output%dir &
  )

  call output_river_src2out_var(&
    river, output%dir, 'hr', hr_idx, 1 &
  )
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call open_file_forcing(forcing, grid)
  !-------------------------------------------------------------
  ! Run
  !-------------------------------------------------------------
  call logent('Executing the simulation')

  time%t_now = time%t_start
  time%t_forcing_next = time%t_now
  time%datetime_now = time%datetime_start
  time%count_forcing = 0
  time%count_model = 0
  count_model_max = (time%t_end - time%t_start) / time%dt_model

  do while( time%t_now < time%t_end )
    call add(time%count_model)

    call logent('Datetime: '//strftime(time%datetime_now)//&
      ' (t = '//str(time%count_model)//'/'//str(count_model_max)//')')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    prcp_sum_prev = prcp_sum
    aevp_sum_prev = aevp_sum
    pevp_sum_prev = pevp_sum
    sr_prev = sr
    ss_prev = ss
    sg_prev = sg
    si_prev = si
    sout_prev = sout
    !-----------------------------------------------------------
    ! Load forcing data
    !-----------------------------------------------------------
    call logent('Forcing Data')

    call load_forcing_data(time, forcing, qp)
    ! DEBUG
    !qp = 0.d0
    call ij2idx(slope, qp, qp_idx)

    call add(prcp_sum, sum(qp_idx(:)*slope%area_idx(:))*time%dt_model)

    call logext()
    !-----------------------------------------------------------
    ! River calculation
    !-----------------------------------------------------------
    call logent('River Calculation')

    call advance_river(&
      hr_idx, & ! inout
      vro_idx, &  ! out
      qr_ave_idx & ! out
    )

    call traperr( wbin( &
      hr_idx, joined(output%dir,'hr1.bin'), rec=time%count_model+1 &
    ) )

    call logext()
    !-----------------------------------------------------------
    ! Slope calculation
    !-----------------------------------------------------------
!if( .false. )then
    call logent('Slope Calculation')

    call ij2idx(slope, gampt_ff, gampt_ff_idx)

    call advance_slope(&
!      time, solver, slope, & ! in
      qp_idx, hs_idx, gampt_ff_idx, & ! inout
      qs_ave_idx & ! out
    )

    call idx2ij(slope, hs_idx, hs)
    call idx2ij(slope, gampt_ff_idx, gampt_ff)

    call traperr( wbin( &
       hs, joined(output%dir,'hs1.bin'), rec=time%count_model+1 &
    ) )

    call logext()
!endif
    !-----------------------------------------------------------
    ! Slope - river interaction
    !-----------------------------------------------------------
!if( .false. )then
    call logent('Slope - River Interaction')

    call interact_slope_river(&
      time, slope, river, & ! in
      hr_idx, hs, vsr_tot & ! inout
    )

    call logext()
!endif
    !-----------------------------------------------------------
    ! Outflow from river
    !-----------------------------------------------------------
!    call logent('Outflow from River')
!
!    call update_river_outlet(river, hr_idx, vro_idx, time%dt_model)
!
!    call logext()
    !-----------------------------------------------------------
    ! 2D -> 1D
    !-----------------------------------------------------------
    call ij2idx(slope, hs, hs_idx)

    !call h2sfc(slope, hs_idx, sfc_idx)
    call h2sfc(hs_idx, sfc_idx)
    call idx2ij(slope, sfc_idx, sfc)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    time%t_now = time%t_now + time%dt_model
    call advance_time(time%datetime_now, int(time%dt_model))
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call logent('Water Budget')

    call calc_storage(&
      slope, river, &
      hr_idx, hs, hg, gampt_ff, &
      vro_idx, &
      sr, ss, si, sg, sout &
    )

    prcp_sum_inc = prcp_sum - prcp_sum_prev
    aevp_sum_inc = aevp_sum - aevp_sum_prev
    sr_inc = sr - sr_prev
    ss_inc = ss - ss_prev
    si_inc = si - si_prev
    sg_inc = sg - sg_prev
    sout_inc = sout - sout_prev
    inc_inbalance &
        = (prcp_sum_inc - aevp_sum_inc) &
        - (sr_inc + ss_inc + sg_inc + si_inc + sout_inc)

    slo_inbl = ss - (ss_prev + prcp_sum_inc - vsr_tot)
    riv_inbl = sr - (sr_prev + vsr_tot - sum(vro_idx))

    !call logmsg('inc prcp_sum: '//str(prcp_sum_inc)//&
    !            ' sr: '//str(sr_inc)//' ss: '//str(ss_inc)//&
    !            ' sout: '//str(sout_inc)//&
    !          '\n    inbalance: '//str(inc_inbalance))
    call logmsg('slo inc: '//str(ss-ss_prev)//' inbl: '//str(slo_inbl))
    call logmsg('riv inc: '//str(sr-sr_prev)//' inbl: '//str(riv_inbl))

    call calc_storage_inbalance(&
        prcp_sum, aevp_sum, sout, sr, ss, si, sg, sinit, sinbl &
    )
    call logmsg('total budget inbalance: '//str(sinbl))

    call logext()
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call logent('Output')

    call traperr( wbin( &
       hs, joined(output%dir,'hs.bin'), rec=time%count_model+1 &
    ) )

    call traperr( wbin( &
       sfc, joined(output%dir,'sfc.bin'), rec=time%count_model+1 &
    ) )

    call traperr( wbin( &
      hr_idx, joined(output%dir,'hr.bin'), rec=time%count_model+1 &
    ) )

    call output_river_src2out_var( &
      river, output%dir, 'hr', hr_idx, time%count_model+1 &
    )

    call logext()
    !-----------------------------------------------------------
    call logext()

    !if( time%count_model == count_model_debug ) exit
  enddo  ! while t < t_end

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file_forcing(forcing)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine run
!===============================================================
!
!===============================================================
subroutine output_river_src2out_attr(&
  river, dir_out &
)
  implicit none
  type(static_river_), intent(in) :: river
  character(*), intent(in) :: dir_out

  type(source_), pointer :: src
  real(8), allocatable :: zb_out(:)
  integer :: iSource
  integer :: iiCh, k
  character(CLEN_PATH) :: f
  integer :: un

  call traperr( mkdir(joined(dir_out, 'src2out_attr')) )

  do iSource = 1, river%nSource
    src => river%source(iSource)

    allocate(zb_out(src%nCh))

    do iiCh = 1, src%nCh
      k = src%iCh(iiCh)
      zb_out(iiCh) = river%channel(k)%zb
    enddo  ! iiCh/

    f = joined(dir_out, 'src2out_attr/'//str(iSource,-6)//'.txt')
    open(newunit=un, file=f, status='replace')
    write(un,"(a)") 'iCh '//str(src%iCh)
    write(un,"(a)") 'zb '//str(zb_out)
    close(un)

    deallocate(zb_out)
  enddo  ! iSource/
end subroutine
!===============================================================
!
!===============================================================
subroutine output_river_src2out_var(river, dir_out, name, var_idx, rec)
  implicit none
  type(static_river_), intent(in) :: river
  character(*), intent(in) :: dir_out
  character(*), intent(in) :: name
  real(8), intent(in) :: var_idx(:)
  integer, intent(in) :: rec

  type(source_), pointer :: src
  real(8), allocatable :: var_out(:)
  integer :: iSource
  integer :: iiCh
  character(CLEN_PATH) :: f

  call traperr( mkdir(joined(dir_out, 'src2out_'//str(name))) )

  do iSource = 1, river%nSource
    src => river%source(iSource)

    allocate(var_out(src%nCh))

    do iiCh = 1, src%nCh
      var_out(iiCh) = var_idx(src%iCh(iiCh))
    enddo

    f = joined(dir_out, 'src2out_'//str(name)//'/'//str(iSource,-6)//'.bin')
    call traperr( wbin(var_out, f, rec=rec, replace=rec==1) )

    deallocate(var_out)
  enddo  ! iSource/
end subroutine output_river_src2out_var
!===============================================================
end module mod_driver
