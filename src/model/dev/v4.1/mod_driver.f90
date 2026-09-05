module mod_driver
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: prep_mod_driver
  public :: exec_simulation
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_mod_driver()
  use mod_forcing, only: &
    load_forcing
  use mod_river, only: &
    prep_mod_river
  use mod_slope, only: &
    prep_mod_slope
  implicit none

  call load_forcing()

  call prep_mod_river()

  call prep_mod_slope()
end subroutine prep_mod_driver
!===============================================================
!
!===============================================================
subroutine exec_simulation()
  use mod_base
  use mod_river, only: &
    advance_river
  use mod_slope, only: &
    advance_slope
  use mod_rivslo, only: &
    funcrs
  implicit none
  real(8), allocatable :: hr(:,:), hr_idx(:)
  real(8), allocatable :: hs(:,:), hs_idx(:)
  real(8), allocatable :: qrs(:,:)
  real(8) :: time_now, time_next
  integer :: it_model

  allocate(hr(nx, ny))
  allocate(hr_idx(riv_count))

  allocate(hs(nx, ny))
  allocate(hs_idx(slo_count))

  allocate(qrs(nx,ny))

  hr(:,:) = 0.d0
  hr_idx(:) = 0.d0
  hs(:,:) = 0.d0
  hs_idx(:) = 0.d0

  time_next = 0.d0

  do it_model = 1, nt_model
    print"(a,i0,a,i0)", 't: ',it_model,' / ',nt_model
    time_now = time_next
    time_next = it_model * dt_model

    call advance_river(time_now, time_next, hr_idx)

    call advance_slope(time_now, time_next, hs_idx)

    call reshape_riv_idx2ij(hr_idx, hr)
    call reshape_slo_idx2ij(hs_idx, hs)

    if( riv_thresh >= 0 ) call funcrs(hr, hs, qrs)

    call reshape_riv_ij2idx(hr, hr_idx)
    call reshape_slo_ij2idx(hs, hs_idx)
  enddo  ! it_model = 1, nt_model
end subroutine exec_simulation
!===============================================================
!
!===============================================================
end module mod_driver
