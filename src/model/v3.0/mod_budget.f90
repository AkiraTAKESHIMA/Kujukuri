module mod_budget
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: init_storage
  public :: update_accum_rain
  public :: update_accum_out
  public :: update_cpssto_riv
  public :: update_cpssto_slo
  public :: update_cpssto_infilt
  public :: update_cpssto_gw
  public :: calc_water_budget
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------

  ! State variables
  real(8) :: accum_rain, accum_evp, accum_out
  real(8) :: sto_riv   , sto_riv_init   , cpssto_riv
  real(8) :: sto_slo   , sto_slo_init   , cpssto_slo
  real(8) :: sto_infilt, sto_infilt_init, cpssto_infilt
  real(8) :: sto_gw    , sto_gw_init    , cpssto_gw
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_storage(hr_idx, hs, gampt_ff, hg)
  implicit none
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(in) :: hs(:,:)
  real(8), intent(in) :: gampt_ff(:,:)
  real(8), intent(in) :: hg(:,:)

  call calc_storage(&
    hr_idx, hs, gampt_ff, hg, &
    sto_riv_init, sto_slo_init, sto_infilt_init, sto_gw_init &
  )
end subroutine init_storage
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine update_accum_rain(rain, ddt)
  implicit none
  real(8), intent(in) :: rain(:)
  real(8), intent(in) :: ddt

  accum_rain = accum_rain + sum(rain) * area * ddt
end subroutine update_accum_rain
!===============================================================
!
!===============================================================
subroutine update_accum_out(out_this)
  implicit none
  real(8), intent(in) :: out_this

  accum_out = accum_out + out_this
end subroutine update_accum_out
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine update_cpssto_riv(sto_add)
  implicit none
  real(8), intent(in) :: sto_add

  cpssto_riv = cpssto_riv + sto_add
end subroutine update_cpssto_riv
!===============================================================
!
!===============================================================
subroutine update_cpssto_slo(sto_add)
  implicit none
  real(8), intent(in) :: sto_add

  cpssto_slo = cpssto_slo + sto_add
end subroutine update_cpssto_slo
!===============================================================
!
!===============================================================
subroutine update_cpssto_infilt(sto_add)
  implicit none
  real(8), intent(in) :: sto_add

  cpssto_infilt = cpssto_infilt + sto_add
end subroutine update_cpssto_infilt
!===============================================================
!
!===============================================================
subroutine update_cpssto_gw(sto_add)
  implicit none
  real(8), intent(in) :: sto_add

  cpssto_gw = cpssto_gw + sto_add
end subroutine update_cpssto_gw
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine calc_water_budget(&
    time, &
    hr_idx, hs, gampt_ff, hg &
)
  implicit none
  real(8), intent(in) :: time
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(in) :: hs(:,:)
  real(8), intent(in) :: gampt_ff(:,:)
  real(8), intent(in) :: hg(:,:)

  real(8) :: inbalance

  integer, save :: un
  real(8), save :: time_prev = -1.d0
  real(8), save :: accum_rain_prev, accum_evp_prev, accum_out_prev
  real(8), save :: sto_riv_prev, sto_slo_prev, sto_infilt_prev, sto_gw_prev
  real(8), save :: cpssto_riv_prev, cpssto_slo_prev, cpssto_infilt_prev, cpssto_gw_prev
  real(8), save :: accum_rain_inc, accum_evp_inc, accum_out_inc
  real(8), save :: sto_riv_inc, sto_slo_inc, sto_infilt_inc, sto_gw_inc

  call calc_storage(&
    hr_idx, hs, gampt_ff, hg, &
    sto_riv, sto_slo, sto_infilt, sto_gw &
  )

  if( time == 0.d0 )then
    accum_rain = 0.d0
    accum_evp  = 0.d0
    accum_out  = 0.d0

    sto_riv_init = sto_riv
    sto_slo_init = sto_slo
    sto_infilt_init = sto_infilt
    sto_gw_init = sto_gw

    cpssto_riv = 0.d0
    cpssto_slo = 0.d0
    cpssto_infilt = 0.d0
    cpssto_gw = 0.d0

    open(newunit=un, file=trim(dir_out)//'/storage.dat', status='replace')
    write(un,"(a)") 'time,rain,evp,out,riv,slo,infilt,gw,cpsriv,cpsslo,cpsinfilt,cpsgw,inbl'

    accum_rain_prev = 0.d0
    accum_evp_prev = 0.d0
    accum_out_prev = 0.d0

    sto_riv_prev = sto_riv_init
    sto_slo_prev = sto_slo_init
    sto_infilt_prev = sto_infilt_init
    sto_gw_prev = sto_gw_init

    cpssto_riv_prev = cpssto_riv
    cpssto_slo_prev = cpssto_slo
    cpssto_infilt_prev = cpssto_infilt
    cpssto_gw_prev = cpssto_gw
  endif

  inbalance = &
    accum_rain - accum_evp - accum_out &
    - ( &
        (sto_riv + sto_slo + sto_infilt + sto_gw) &
        - (cpssto_riv + cpssto_slo + cpssto_infilt + cpssto_gw) &
        - (sto_riv_init + sto_slo_init + sto_infilt_init + sto_gw_init) &
      )

  accum_rain_inc = accum_rain - accum_rain_prev
  accum_evp_inc  = accum_evp - accum_evp_prev
  accum_out_inc  = accum_out - accum_out_prev
  sto_riv_inc    = (sto_riv - cpssto_riv) - (sto_riv_prev - cpssto_riv_prev)
  sto_slo_inc    = (sto_slo - cpssto_slo) - (sto_slo_prev - cpssto_slo_prev)
  sto_infilt_inc = (sto_infilt - cpssto_infilt) - (sto_infilt_prev - cpssto_infilt_prev)
  sto_gw_inc     = (sto_gw - cpssto_gw) - (sto_gw_prev - cpssto_gw_prev)

  write(un,"(i8,12(',',es9.2))") &
    int(time), &
    accum_rain, accum_evp, accum_out, &
    sto_riv, sto_slo, sto_infilt, sto_gw, &
    cpssto_riv, cpssto_slo, cpssto_infilt, cpssto_gw, &
    inbalance

  inbalance &
    = (accum_rain_inc - accum_evp_inc - accum_out_inc) &
    - (sto_riv_inc + sto_slo_inc + sto_infilt_inc + sto_gw_inc)
  print"(1x,a,6(1x,es9.2))", &
    'budget', accum_rain_inc, accum_out_inc, &
    sto_riv_inc, sto_slo_inc, sto_infilt_inc, &
    inbalance

  if( time == time_prev )then
    print*, 'closed budget file. time: ', time
    close(un)
  endif

  time_prev = time
  accum_rain_prev    = accum_rain
  accum_evp_prev     = accum_evp
  accum_out_prev     = accum_out
  sto_riv_prev       = sto_riv
  sto_slo_prev       = sto_slo
  sto_infilt_prev    = sto_infilt
  sto_gw_prev        = sto_gw
  cpssto_riv_prev    = cpssto_riv
  cpssto_slo_prev    = cpssto_slo
  cpssto_infilt_prev = cpssto_infilt
  cpssto_gw_prev     = cpssto_gw
end subroutine calc_water_budget
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine calc_storage(&
  hr_idx, hs, gampt_ff, hg, &
  sto_riv, sto_slo, sto_infilt, sto_gw &
)
  use mod_section, only: &
    hr2vr
  implicit none
  real(8), intent(in) :: hr_idx(:)
  real(8), intent(in) :: hs(:,:)
  real(8), intent(in) :: gampt_ff(:,:)
  real(8), intent(in) :: hg(:,:)
  real(8), intent(out) :: sto_riv, sto_slo, sto_infilt, sto_gw

  real(8) :: vr
  integer :: k
  integer :: ix, iy

  sto_riv = 0.d0
  sto_slo = 0.d0
  sto_infilt = 0.d0
  sto_gw = 0.d0

  do k = 1, riv_count
    call hr2vr(hr_idx(k), k, vr)
    sto_riv = sto_riv + vr
  enddo

  do iy = 1, ny
  do ix = 1, nx
    if( domain(ix,iy) == DOMAIN__OUTSIDE ) cycle

    sto_slo = sto_slo + hs(ix,iy) * area
    sto_infilt = sto_infilt + gampt_ff(ix,iy) * area
    sto_gw = sto_gw - hg(ix,iy) * gammag_idx(slo_ij2idx(ix,iy)) * area
  enddo  ! ix/
  enddo  ! iy/
end subroutine calc_storage
!===============================================================
!
!===============================================================
end module mod_budget
