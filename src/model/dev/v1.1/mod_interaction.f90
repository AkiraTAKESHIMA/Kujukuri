module mod_interaction
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use lib_io
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: interact_slope_river

  public :: calc_discharge_over_levee
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_interaction'

  logical :: debug = .true.
  integer :: k_ = 1654
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine interact_slope_river(&
  time, slope, river, hr_idx, hs, vsr_tot &
)
  use mod_base, only: &
    hr2vr, &
    vr2hr
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'interact_slope_river'
  type(time_), intent(in) :: time
  type(static_slope_), intent(in) :: slope
  type(static_river_), intent(in) :: river
  real(8), intent(inout) :: hr_idx(:)
  real(8), intent(inout) :: hs(:,:)
  real(8), intent(out) :: vsr_tot

  type(channel_), pointer :: ch
  type(ch_mesh_), pointer :: mesh
  real(8) :: vr_idx(size(hr_idx))
  real(8) :: hsr_mesh, vsr_mesh
  integer :: k, iMesh, x, y

  real(8) :: vs_before, vs_after
  real(8) :: vr_before, vr_after
  real(8) :: vsr

  real(8), allocatable :: hr_idx_prev(:)
  real(8), allocatable :: hs_prev(:,:)
  integer :: dgt_xy, dgt1
  logical :: is_first

  !-------------------------------------------------------------
  ! DEBUG
  !-------------------------------------------------------------
  dgt_xy = dgt(max(slope%nx,slope%ny))
  dgt1 = dgt_xy*2 + 1 - dgt(river%nCh)

  allocate(hr_idx_prev(river%nCh))
  allocate(hs_prev(slope%nx,slope%ny))

  vs_before = 0.d0
  vs_after = 0.d0
  vr_before = 0.d0
  vr_after = 0.d0
  vsr_tot = 0.d0

!  call logmsg('ch#'//str(k_)//' hr: '//str(hr_idx(k_)))
  call logmsg('hr min: '//str(minval(hr_idx))//' max: '//str(maxval(hr_idx)))
  call logmsg('hs min: '//str(minval(hs))//' max: '//str(maxval(hs)))
  !-------------------------------------------------------------

  do k = 1, river%nCh
    ch => river%channel(k)

    ! TMP
    call hr2vr(ch%area, hr_idx(k), vr_idx(k))

    !-----------------------------------------------------------
    ! DEBUG
    !-----------------------------------------------------------
!    hr_idx_prev(k) = hr_idx(k)

!    call add(vr_before, vr_idx(k))
!    do iMesh = 1, ch%nMesh
!      mesh => ch%mesh(iMesh)
!      if( mesh%is_outside_domain ) cycle
!      x = mesh%x
!      y = mesh%y
!      call add(vs_before, hs(x,y)*slope%area(x,y))

!      hs_prev(x,y) = hs(x,y)
!    enddo
    !-----------------------------------------------------------

    vsr = 0.d0
    do iMesh = 1, ch%nMesh
      mesh => ch%mesh(iMesh)
      x = mesh%x
      y = mesh%y

      if( mesh%is_outside_domain ) cycle

      call calc_discharge_slo2riv(&
        hs(x,y), hr_idx(k), vr_idx(k)*mesh%area/ch%area, &
        ch%depth, ch%levee, &
        mesh%leng, slope%area(x,y), mesh%area, time%dt_model, &
        hsr_mesh, vsr_mesh &
      )

      !---------------------------------------------------------
      ! DEBUG
      !---------------------------------------------------------
!      if( hr_idx(k)-ch%depth > hs(x,y) )then
!        call logmsg('ch#'//str(k,dgt(river%nCh))//&
!          ' hr: '//str(hr_idx(k),'es10.3')//' hr-depth: '//str(hr_idx(k)-ch%depth,'es10.3')//&
!          ' levee: '//str(ch%levee,'es10.3')//&
!          ' hs('//str((/x,y/),dgt_xy,',')//') '//str(hs(x,y),'es10.3')//&
!          ' hsr '//str(hsr_mesh,'es10.3'))
!      endif
      !---------------------------------------------------------

      call add(hs(x,y), -hsr_mesh)
      call add(vsr, vsr_mesh)
    enddo  ! iMesh/

    call add(vr_idx(k), vsr)
    ! TMP
    call vr2hr(ch%area, vr_idx(k), hr_idx(k))

    !-----------------------------------------------------------
    ! DEBUG
    !-----------------------------------------------------------
!    is_first = .true.
!    do iMesh = 1, ch%nMesh
!      mesh => ch%mesh(iMesh)
!      x = mesh%x
!      y = mesh%y
!      if( mesh%is_outside_domain ) cycle
!      if( hr_idx_prev(k) > hs_prev(x,y) .and. hr_idx(k) < hs(x,y) )then
!        if( is_first )then
!          call logmsg('hr('//str(k,dgt(river%nCh))//') prev: '//&
!            str(hr_idx_prev(k))//' now: '//str(hr_idx(k)))
!          is_first = .false.
!        endif
!        call logmsg('hs('//str((/x,y/),dgt_xy,',')//') prev: '//&
!          str(hs_prev(x,y))//' now: '//str(hs(x,y)))
!      endif
!    enddo  ! iMesh/

!    call add(vr_after, vr_idx(k))
!    do iMesh = 1, ch%nMesh
!      mesh => ch%mesh(iMesh)
!      if( mesh%is_outside_domain ) cycle
!      x = mesh%x
!      y = mesh%y
!      call add(vs_after, hs(x,y)*slope%area(x,y))
!    enddo

    call add(vsr_tot, vsr)
    !-----------------------------------------------------------
  enddo  ! k/

  ! DEBUG
  call logmsg('discharge from slope to river: '//str(vsr_tot))
end subroutine interact_slope_river
!===============================================================
!
!===============================================================
subroutine calc_discharge_slo2riv(&
  hs_org, hr_org, vr_org, &
  depth, levee, &
  leng_isct, area_slo, area_riv, dt, &
  hsr, vsr &
)
  use mod_base, only: &
    vr2hr
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_dicharge'
  real(8), intent(in) :: hs_org
  real(8), intent(in) :: hr_org
  real(8), intent(in) :: vr_org
  real(8), intent(in) :: depth
  real(8), intent(in) :: levee
  real(8), intent(in) :: leng_isct
  real(8), intent(in) :: area_slo
  real(8), intent(in) :: area_riv
  real(8), intent(in) :: dt
  real(8), intent(out) :: hsr  ![m]
  real(8), intent(out) :: vsr  ![m3]

  real(8) :: hs, hr, vr
  real(8) :: hs_top, hr_top
  real(8) :: h1, h2

  real(8), parameter :: mu1 = (2.d0/3.d0) ** (3.d0/2.d0)
  real(8), parameter :: mu2 = 0.35d0
  real(8), parameter :: mu3 = 0.91d0

  ! TMP: rectangular cross section

  hs = hs_org
  hr = hr_org
  vr = vr_org

  hs_top = hs
  hr_top = hr - depth
  !-------------------------------------------------------------
  ! Case a: (levee = 0 and hr_top < 0) or &
  !         (levee > 0 and hr_top < 0 and hs_top <= levee)
  ! -> From slope to river: step fall (vsr: positive)
  if( (levee == 0.d0 .and. hr_top < 0.d0) .or. &
      (levee > 0.d0 .and. hr_top < 0.d0 .and. hs_top <= levee) )then

    hsr = min(mu1 * hs_top * sqrt(GRAVITY * hs_top) * leng_isct * 2 / area_slo * dt, hs)
    vsr = hsr * area_slo

    hs = hs - hsr

    vr = vr + vsr
    call vr2hr(area_riv, vr, hr)

    ! Avoid hs_top < hr_top
    if( hs < hr - depth )then
      hs = hs_org
      hr = hr_org

      hsr = hs - (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
      vsr = hsr * area_slo
      hs = hs - hsr
      hr = (hr*area_riv + vsr) / area_riv
    endif
  !-------------------------------------------------------------
  ! Case b: levee > 0 and hr_top <= levee and hr_top >= 0
  ! -> No exchange
  elseif( levee > 0.d0 .and. hr_top >= 0.d0 .and. &
          hs_top <= levee .and. hr_top <= levee )then

    hsr = 0.d0
    vsr = 0.d0
  !-------------------------------------------------------------
  ! Case c: hs <= hr_top and hr_top >= levee
  ! -> From river to slope: overtopping (vsr: negative)
  ! (incl. hs = 0 and hr_top > 0)
  elseif( hs_top <= hr_top .and. hr_top >= levee )then

    h1 = hr_top - levee
    h2 = hs_top - levee
    !if( h2/h1 <= 2.d0/3.d0 )then
    !  vsr = - min(mu2 * h1 * sqrt(2.d0 * GRAVITY * h1) * leng_isct * 2 * dt, vr)
    !else
    !  vsr = - min(mu3 * h2 * sqrt(2.d0 * GRAVITY * (h1-h2)) * leng_isct * 2 * dt, vr)
    !endif
    call calc_discharge_over_levee(h1, h2, vsr)
    vsr = - min(vsr * leng_isct * 2.d0 * dt, vr)

    hsr = vsr / area_slo


    vr = vr + vsr
    call vr2hr(area_riv, vr, hr)

    hs = hs - hsr

    ! Avoid hs_top > hr_top
    if( hs > hr - depth )then
      hs = hs_org
      hr = hr_org

      hsr = hs - (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
      vsr = hsr * area_slo
      hs = hs - hsr
      hr = (hr*area_riv + vsr) / area_riv
    endif
  !-------------------------------------------------------------
  ! Case d: hs > hr_top and hs >= levee
  ! -> From slope to river: overtopping (hsr: positive)
  elseif( hs_top > hr_top .and. hs_top >= levee )then

    h1 = hs_top - levee
    h2 = hr_top - levee
    !if( h2/h1 <= 2.d0/3.d0 )then
    !  hsr = min(mu2 * h1 * sqrt(2.d0 * GRAVITY * h1) * leng_isct * 2 / area_slo * dt, hs)
    !else
    !  hsr = min(mu3 * h2 * sqrt(2.d0 * GRAVITY * (h1-h2)) * leng_isct * 2 / area_slo * dt, hs)
    !endif
    !vsr = hsr * area_slo
    call calc_discharge_over_levee(h1, h2, vsr)
    vsr = min(vsr * leng_isct * 2.d0 * dt, hs*area_slo)
    hsr = vsr / area_slo

    hs = hs - hsr

    vr = vr + vsr
    call vr2hr(area_riv, vr, hr)

    ! Avoid hs_top < hr_top
    if( hs < hr - depth )then
      hs = hs_org
      hr = hr_org

      hsr = hs - (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
      vsr = hsr * area_slo
      hs = hs - hsr
      hr = (hr*area_riv + vsr) / area_riv
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call errend('Not matched any case.'//&
      '\nhs: '//str(hs)//' hr: '//str(hr)//' depth: '//str(depth)//' levee: '//str(levee)//&
      '\nhs_top: '//str(hs_top)//' hr_top: '//str(hr_top), &
      '', PRCNAM, MODNAM)
  endif
end subroutine calc_discharge_slo2riv
!===============================================================
!
!===============================================================
subroutine calc_discharge_over_levee(h1, h2, q)
  implicit none
  real(8), intent(in) :: h1, h2  ! [m] h1 > h2
  real(8), intent(out) :: q  ! discharge from 1 to 2 [m3/s]

  real(8), parameter :: mu2 = 0.35d0
  real(8), parameter :: mu3 = 0.91d0

  if( h2/h1 <= 2.d0/3.d0 )then
    q = mu2 * h1 * sqrt(2.d0 * GRAVITY * h1)
  else
    q = mu3 * h2 * sqrt(2.d0 * GRAVITY * (h1-h2))
  endif
end subroutine calc_discharge_over_levee
!===============================================================
!
!===============================================================
end module mod_interaction
