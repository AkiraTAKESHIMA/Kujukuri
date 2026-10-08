module mod_rivslo
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: prep_rivslo
  public :: funcrs
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_rivslo'

  type vrs_
    real(8) :: total
    real(8), pointer :: val(:)
  end type

  type(vrs_), pointer :: vrs(:)  ! volume discharge from riv to slo
  type(vrs_), pointer :: vsr(:)  ! volume discharge from slo to riv
  real(8), allocatable :: vr_idx(:)
  real(8), allocatable :: vs_idx(:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_rivslo()
  implicit none
  integer :: iCh, iSlo

  allocate(vrs(riv_count))
  do iCh = 1, riv_count
    allocate(vrs(iCh)%val(channel(iCh)%isct%nSlo))
  enddo

  allocate(vsr(slo_count))
  do iSlo = 1, slo_count
    allocate(vsr(iSlo)%val(slo_riv_isct(iSlo)%nCh))
  enddo

  allocate(vr_idx(riv_count))
  allocate(vs_idx(slo_count))
end subroutine prep_rivslo
!===============================================================
!
!===============================================================
subroutine funcrs( hr_idx, hs_idx, qrs_idx )
  use mod_section, only: &
    sec_h2b, &
    hr2vr, &
    vr2hr
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'funcrs'
  real(8), intent(inout) :: hr_idx(:), hs_idx(:)
  real(8), intent(out) :: qrs_idx(:,:)  !(nSlo_isct_max,riv_count)

  type(channel_), pointer :: ch
  real(8) :: hr_top, hs_top, h1, h2
  real(8) :: vsr_this
  real(8) :: leng
  real(8) :: area_riv
  integer :: iCh, jCh
  integer :: iSlo, jSlo
  real(8) :: shrink
  real(8) :: vrs_posi
  real(8) :: vsr_posi


  real(8), parameter :: mu1 = (2.d0 / 3.d0) ** (3.d0 / 2.d0)
  real(8), parameter :: mu2 = 0.35d0
  real(8), parameter :: mu3 = 0.91d0

  integer :: iCh_debug = 0
  logical :: debug_this

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  ! Calc. discharge
  !-------------------------------------------------------------
  call logent('calc. discharge', opt='-p -x2')

  do iCh = 1, riv_count
    vrs(iCh)%val(:) = 0.d0
  enddo

  do iSlo = 1, slo_count
    vsr(iSlo)%val(:) = 0.d0
  enddo

  do iCh = 1, riv_count
    debug_this = iCh == iCh_debug

    ch => channel(iCh)
    hr_top = hr_idx(iCh) - ch%depth
    if( debug_this )then
      call logmsg('hr: '//str(hr_idx(iCh))//' hr_top: '//str(hr_top))
    endif

    do jSlo = 1, ch%isct%nSlo
      if( ch%isct%domain(jSlo) == DOMAIN__OUTSIDE ) cycle

      iSlo = ch%isct%iSlo(jSlo)
      jCh = ch%isct%jCh(jSlo)
      leng = ch%isct%leng(jSlo)
      area_riv = ch%isct%area(jSlo)

      hs_top = hs_idx(iSlo)
      if( debug_this )then
        call logmsg('slo#'//str(iSlo)//' leng: '//str(leng))
        call logmsg('hs: '//str(hs_idx(iSlo))//' hs_top: '//str(hs_top))
      endif

      call calc_discharge(vsr_this)

      if( debug_this )then
        call logmsg('vsr: '//str(vsr_this)//&
            '\nhs_top: '//str(hs_top)//' + '//str(-vsr_this/area)//&
             ' -> '//str(hs_top-vsr_this/area)//&
            '\nhr_top: '//str(hr_top)//' + '//str(vsr_this/area_riv)//&
            ' -> '//str(hr_top+vsr_this/area_riv))
      endif

      vrs(iCh)%val(jSlo) = -vsr_this
      vsr(iSlo)%val(jCh) = vsr_this
    enddo  ! jSlo/

    if( debug_this )then
      call logmsg('vrs: '//str(sum(vrs(iCh)%val)))
    endif
  enddo  ! iCh/

  do iCh = 1, riv_count
    vrs(iCh)%total = sum(vrs(iCh)%val)
  enddo

  do iSlo = 1, slo_count
    vsr(iSlo)%total = sum(vsr(iSlo)%val)
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Modify discharge from river to slope
  !-------------------------------------------------------------
  call logent('modify disc. from river to slope', opt='-p -x2')

  do iCh = 1, riv_count
    debug_this = iCh == iCh_debug

    ch => channel(iCh)

    call hr2vr(hr_idx(iCh), iCh, vr_idx(iCh))

    if( vr_idx(iCh) >= vrs(iCh)%total ) cycle

    if( vrs(iCh)%total > vr_idx(iCh) )then
      call errend('ch#'//str(iCh)//' vr: '//str(vr_idx(iCh))//&
          ' vrs: '//str(vrs(iCh)%total))
    endif

    vrs_posi = sum(vrs(iCh)%val, mask=vrs(iCh)%val>0.d0)

    shrink = 1.d0 - (vrs(iCh)%total - vr_idx(iCh)) / vrs_posi
    if( shrink < 0.d0 ) shrink = 0.d0

    if( debug_this )then
      call logmsg('ch#'//str(iCh)//' vr: '//str(vr_idx(iCh))//&
        ' vrs tot: '//str(vrs(iCh)%total)//' posi: '//str(vrs_posi)//&
        ' shrink: '//str(shrink)//' -> tot: '//str(vrs(iCh)%total*shrink))
    endif

    do jSlo = 1, ch%isct%nSlo
      if( vrs(iCh)%val(jSlo) <= 0.d0 ) cycle

      iSlo = ch%isct%iSlo(jSlo)
      jCh = ch%isct%jCh(jSlo)

      vrs(iCh)%val(jSlo) = vrs(iCh)%val(jSlo) * shrink
      vsr(iSlo)%val(jCh) = vsr(iSlo)%val(jCh) * shrink
    enddo  ! jSlo/

    vrs(iCh)%total = sum(vrs(iCh)%val)
    call trap_vr_exceedance()
  enddo  ! iCh/

  do iSlo = 1, slo_count
    vsr(iSlo)%total = sum(vsr(iSlo)%val)
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Modify discharge from slope to river
  !-------------------------------------------------------------
  call logent('modify disc. from slope to river', opt='-p -x2')

  do iSlo = 1, slo_count
    vs_idx(iSlo) = hs_idx(iSlo) * area

    if( vs_idx(iSlo) >= vsr(iSlo)%total ) cycle

    vsr_posi = sum(vsr(iSlo)%val, mask=vsr(iSlo)%val>0.d0)

    shrink = 1.d0 - (vsr(iSlo)%total - vs_idx(iSlo)) / vsr_posi
    if( shrink < 0.d0 ) shrink = 0.d0

    !call logmsg('slo#'//str(iSlo)//' vs: '//str(vs_idx(iSlo))//&
    !    ' vsr tot: '//str(vsr(iSlo)%total)//' posi: '//str(vsr_posi)//&
    !    ' shrink: '//str(shrink))

    do jCh = 1, slo_riv_isct(iSlo)%nCh
      if( vsr(iSlo)%val(jCh) <= 0.d0 ) cycle

      iCh = slo_riv_isct(iSlo)%iCh(jCh)
      jSlo = slo_riv_isct(iSlo)%jSlo(jCh)

      vsr(iSlo)%val(jCh) = vsr(iSlo)%val(jCh) * shrink
      vrs(iCh)%val(jSlo) = vrs(iCh)%val(jSlo) * shrink
    enddo  ! jCh/

    vsr(iSlo)%total = sum(vsr(iSlo)%val)
    call trap_vs_exceedance()
  enddo  ! iSlo/

  do iCh = 1, riv_count
    vrs(iCh)%total = sum(vrs(iCh)%val)
  enddo

  call logext()
  !-------------------------------------------------------------
  ! assert consistency
  !-------------------------------------------------------------
  call logent('assert consistency', opt='-p -x2')

  do iCh = 1, riv_count
    ch => channel(iCh)
    do jSlo = 1, ch%isct%nSlo
      if( ch%isct%domain(jSlo) == DOMAIN__OUTSIDE ) cycle
      iSlo = ch%isct%iSlo(jSlo)
      jCh = ch%isct%jCh(jSlo)

      if( vrs(iCh)%val(jSlo) /= -vsr(iSlo)%val(jCh) )then
        call errend('vrs and -vsr mismatch.'//&
            '\nvrs('//str(iCh)//'%val('//str(jSlo)//'): '//str(vrs(iCh)%val(jSlo))//&
            '\nvsr('//str(iSlo)//'%val('//str(jCh)//'): '//str(vsr(iSlo)%val(jCh)))
      endif
    enddo  ! jSlo/
  enddo  ! iCh/

  call logext()
  !-------------------------------------------------------------
  ! update water level
  !-------------------------------------------------------------
  call logent('update water level', opt='-p -x2')

  do iCh = 1, riv_count
    if( vrs(iCh)%total == 0.d0 )cycle

    ch => channel(iCh)

    call trap_vr_exceedance()

    vr_idx(iCh) = vr_idx(iCh) - vrs(iCh)%total
    call vr2hr(vr_idx(iCh), iCh, hr_idx(iCh))
  enddo  ! iCh/

  do iSlo = 1, slo_count
    if( vsr(iSlo)%total == 0.d0 ) cycle

    call trap_vs_exceedance()

    vs_idx(iSlo) = vs_idx(iSlo) - vsr(iSlo)%total
    hs_idx(iSlo) = vs_idx(iSlo) / area
  enddo  ! iSlo/

  call logext()
  !-------------------------------------------------------------
  ! calc. $qrs_idx
  !-------------------------------------------------------------
  qrs_idx(:,:) = 0.d0
  do iCh = 1, riv_count
    ch => channel(iCh)
    do jSlo = 1, ch%isct%nSlo
      call add(qrs_idx(jSlo,iCh), vrs(iCh)%val(jSlo))
    enddo
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
!
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine calc_discharge(vsr)
  implicit none
  real(8), intent(out) :: vsr

  real(8) :: hr_tmp, hs_tmp

  hr_top = hr_idx(iCh) - ch%depth
  hs_top = hs_idx(iSlo)

  !-------------------------------------------------------------
  ! (Case a) : (height = 0 and hr_top < 0) or (height > 0 and hr_top < 0 and hs_top <= height)
  ! -> From slope to river : step fall (hsr : positive)
  if( ( ch%height == 0.d0 .and. hr_top < 0.d0 ) .or. &
      ( ch%height > 0.d0 .and. hr_top < 0.d0 .and. hs_top <= ch%height ) )then

    if( debug_this )then
      call logmsg('case a')
    endif

    vsr = mu1 * hs_top * sqrt(GRAVITY * hs_top) * dt_model * leng * 2.d0
    if( vsr > vs_idx(iSlo) ) vsr = vs_idx(iSlo)

    call vr2hr(vr_idx(iCh)+vsr, iCh, hr_tmp)
    hs_tmp = (vs_idx(iSlo) - vsr) / area

    ! avoid the situation of hr_top > hs_top
    hr_top = hr_tmp - ch%depth
    hs_top = hs_tmp
    if( hr_top > hs_top )then
      call calc_discharge_for_same_level(vsr)
    endif

  !-------------------------------------------------------------
  ! (Case b) : 0 <= hr_top <= height and hs_top <= height
  ! -> No exchange
  elseif( 0.d0 <= hr_top .and. hr_top <= ch%height .and. &
          hs_top <= ch%height )then

    if( debug_this )then
      call logmsg('case b')
    endif

    vsr = 0.d0

  !-------------------------------------------------------------
  ! (Case c) : hs <= hr and hr >= height
  ! -> From river to slope : overtopping (vsr : negative)
  ! (incl. hs = 0 and hr > 0)
  elseif( hs_top <= hr_top .and. hr_top >= ch%height )then

    if( debug_this )then
      call logmsg('case c')
    endif

    h1 = hr_top - ch%height
    h2 = hs_top - ch%height
    if( h2 / h1 <= 2.d0 / 3.d0 )then
      vsr = -mu2 * h1 * sqrt(2.d0*GRAVITY*h1) * dt_model * leng * 2.d0
    else
      vsr = -mu3 * h2 * sqrt(2.d0*GRAVITY*(h1-h2)) * dt_model * leng * 2.d0
    endif

    if( debug_this )then
      call logmsg('1 vsr: '//str(vsr)//' hr_top: '//str(hr_top)//' area: '//str(area_riv))
      if( vsr < -h1 * area_riv )then
        call logmsg('2 vsr: '//str(-h1*area_riv))
      endif
    endif

    vsr = max(vsr, -(hr_top - ch%height) * area_riv)

    call vr2hr(vr_idx(iCh)+vsr, iCh, hr_tmp)
    hs_tmp = (vs_idx(iSlo) - vsr) / area

    ! avoid situation of hs_top > hr_top
    hr_top = hr_tmp - ch%depth
    hs_top = hs_tmp
    if( debug_this )then
      call logmsg('hr_top: '//str(hr_top)//' hs_top: '//str(hs_top))
    endif
    if( hs_top > hr_top )then
      call calc_discharge_for_same_level(vsr)
    endif
  !-------------------------------------------------------------
  ! (Case d) : hs > hr & hs >= height
  ! -> From slope to river : overtopping (vsr : positive)
  ! (incl. hs = 0 and hr > 0)
  elseif( hs_top >= hr_top .and. hs_top >= ch%height )then

    if( debug_this )then
      call logmsg('case d')
    endif

    h1 = hs_top - ch%height
    h2 = hr_top - ch%height
    if( h2 / h1 <= 2.d0 / 3.d0 )then
      vsr = mu2 * h1 * sqrt(2.d0 * GRAVITY * h1) * dt_model * leng * 2.d0
    else
      vsr = mu3 * h2 * sqrt(2.d0 * GRAVITY * (h1-h2)) * dt_model * leng * 2.d0
    endif

    vsr = min(vsr, (hs_top - ch%height) * area_riv)

    call vr2hr(vr_idx(iCh)+vsr, iCh, hr_tmp)
    hs_tmp = (vs_idx(iSlo) - vsr) / area

    ! avoid the situation of hr_top > hs_top
    hr_top = hr_tmp - ch%depth
    hs_top = hs_tmp
    if( hr_top > hs_top )then
      call calc_discharge_for_same_level(vsr)
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    ! Condition not considered above
    stop "Error : RivSlo"
  endif
end subroutine calc_discharge
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine calc_discharge_for_same_level(vsr)
  implicit none
  real(8), intent(out) :: vsr

  hr_top = hr_idx(iCh) - ch%depth
  hs_top = hs_idx(iSlo)

  ! TMP
  vsr = (hs_top - hr_top) * (area * area_riv) / (area + area_riv)
end subroutine calc_discharge_for_same_level
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine trap_vr_exceedance()
  implicit none

  if( (vrs(iCh)%total - vr_idx(iCh)) / max(vr_idx(iCh),1d-6) > 1d-6 )then
    call errend('ch#'//str(iCh)//' vr: '//str(vr_idx(iCh))//&
        ' vrs: '//str(vrs(iCh)%total))
  endif
end subroutine trap_vr_exceedance
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine trap_vs_exceedance()
  implicit none

  if( (vsr(iSlo)%total - vs_idx(iSlo)) / max(vs_idx(iSlo),1d-6) > 1d-6 )then
    call errend('slo#'//str(iSlo)//' vs: '//str(vs_idx(iSlo))//&
        ' vsr: '//str(vsr(iSlo)%total)//&
        ' error: '//str((vsr(iSlo)%total - vs_idx(iSlo)) / max(vs_idx(iSlo),1d-6)))
  endif
end subroutine trap_vs_exceedance
!---------------------------------------------------------------
end subroutine funcrs
!===============================================================
!
!===============================================================
end module mod_rivslo
