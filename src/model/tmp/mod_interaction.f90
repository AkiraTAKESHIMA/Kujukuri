module mod_interaction
  implicit none

contains
!===============================================================
!
!===============================================================
subroutine interact_slope_river(
)

  do k = 1, static%river%nCh
    ch => static%river%channel(k)

    call hr2vr(river%hr_idx(k), k, river%vr_idx(k))

    vsr = 0.d0
    do iMesh = 1, ch%nMesh
      mesh => ch%mesh(iMesh)
      x = mesh%x
      y = mesh%y

      if( mesh%is_outside_domain ) cycle

      call calc_discharge_slo2riv(&
          river%hs(x,y), river%hr_idx(k), vr_idx(k)*mesh%area/ch%area, &
          k, ch%depth, ch%levee, &
          mesh%leng, area_slo(x,y), mesh%area, &
          hsr_mesh, vsr_mesh)

      ! Update state variables
      call add(river%hs(x,y), -hsr_mesh)
      call add(vsr, vsr_mesh)

!      if( debug )then
!        call add(vs_after, state%hs(x,y)*static%slope%area(x,y))
!      endif
    enddo  ! iMesh/

    call add(state%vr_idx(k), vsr)
    call vr2hr(state%vr_idx(k), k, state%hr_idx(k))

    vsr_tot = vsr_tot + vsr
  enddo  ! k/
end subroutine interact_slope_river
!===============================================================
!
!===============================================================
subroutine calc_discharge_slo2riv(&
  hs_org, hr_org, vr_org, &
  k, depth, levee, &
  leng_isct, area_slo, area_riv, &
  hsr, vsr &
)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_discharge_slo2riv'
  real(8), intent(in) :: hs_org
  real(8), intent(in) :: hr_org
  real(8), intent(in) :: vr_org
  integer, intent(in) :: k
  real(8), intent(in) :: depth
  real(8), intent(in) :: levee
  real(8), intent(in) :: leng_isct
  real(8), intent(in) :: area_slo
  real(8), intent(in) :: area_riv
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

!    if( debug )then
!      call logmsg('Case a')
!    endif

    hsr = min(mu1 * hs_top * sqrt(gravity * hs_top) * leng_isct * 2 / area_slo * dt, hs)
    vsr = hsr * area_slo

    hs = hs - hsr

    vr = vr + vsr
    call vr2hr(vr, k, hr)

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

!    if( debug )then
!      call logmsg('Case b')
!    endif

    hsr = 0.d0
    vsr = 0.d0
  !-------------------------------------------------------------
  ! Case c: hs <= hr_top and hr_top >= levee
  ! -> From river to slope: overtopping (vsr: negative)
  ! (incl. hs = 0 and hr_top > 0)
  elseif( hs_top <= hr_top .and. hr_top >= levee )then

!    if( debug )then
!      call logmsg('Case c')
!    endif

    h1 = hr_top - levee
    h2 = hs_top - levee
    if( h2/h1 <= 2.d0/3.d0 )then
      vsr = - min(mu2 * h1 * sqrt(2.d0 * gravity * h1) * leng_isct * 2 * dt, vr)
    else
      vsr = - min(mu3 * h2 * sqrt(2.d0 * gravity * (h1-h2)) * leng_isct * 2 * dt, vr)
    endif
    hsr = vsr / area_slo

!    if( debug )then
!      call logmsg('vsr '//str(vsr)//' vr '//str(vr)//' hr '//str(vr/area_riv))
!      call logmsg('hsr (slo) '//str(hsr)//' (riv) '//str(vsr/area_riv))
!    endif

    vr = vr + vsr
    call vr2hr(vr, k, hr)

    hs = hs - hsr

!    if( debug )then
!      call logmsg('hr '//str(hr)//' hs '//str(hs)//' hr_top '//str(hr-depth))
!    endif

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

!    if( debug )then
!      call logmsg('Case d')
!    endif

    h1 = hs_top - levee
    h2 = hr_top - levee
    if( h2/h1 <= 2.d0/3.d0 )then
      hsr = min(mu2 * h1 * sqrt(2.d0 * gravity * h1) * leng_isct * 2 / area_slo * dt, hs)
    else
      hsr = min(mu3 * h2 * sqrt(2.d0 * gravity * (h1-h2)) * leng_isct * 2 / area_slo * dt, hs)
    endif
    vsr = hsr * area_slo

    hs = hs - hsr

    vr = vr + vsr
    call vr2hr(vr, k, hr)

!print*, '(1)', hs, hr-depth, hsr, hr

    ! Avoid hs_top < hr_top
    if( hs < hr - depth )then
      hs = hs_org
      hr = hr_org

!print*, 'hs_new', (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
!print*, hs, hr_top, area_slo, area_riv

      hsr = hs - (hs*area_slo + hr_top*area_riv) / (area_slo + area_riv)
      vsr = hsr * area_slo
      hs = hs - hsr
      hr = (hr*area_riv + vsr) / area_riv
    endif

!print*, '(2)', hs, hr-depth, hsr, hr
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    call errend('Not matched any case.', PRCNAM, MODNAM)
  endif
end subroutine calc_discharge_slo2riv
!===============================================================
!
!===============================================================
end module mod_interaction
