module mod_rivslo
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: funcrs
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine funcrs( hr_idx, hs, qrs )
  use mod_section, only: &
    hr_update, &
    sec_h2b
  implicit none
  real(8), intent(inout) :: hr_idx(:), hs(:,:)
  real(8), intent(out) :: qrs(:,:)

  type(channel_), pointer :: ch
  real(8) :: hr_top, hs_top, h1, h2
  real(8) :: b
  real(8) :: dhs_tmp
  real(8) :: leng
  integer :: k
  integer :: iGrid
  integer :: x, y
  real(8), parameter :: mu1 = (2.d0 / 3.d0) ** (3.d0 / 2.d0)
  real(8), parameter :: mu2 = 0.35d0
  real(8), parameter :: mu3 = 0.91d0

  qrs(:,:) = 0.d0

  do k = 1, riv_count
    ch => channel(k)

    do iGrid = 1, ch%isct%nGrid
      if( ch%isct%domain(iGrid) == DOMAIN__OUTSIDE ) cycle

      x = ch%isct%x(iGrid)
      y = ch%isct%y(iGrid)
      leng = ch%isct%leng(iGrid)

      hs_top = hs(x,y)
      hr_top = hr_idx(k) - ch%depth

      call calc_dhs(dhs_tmp)

      qrs(x,y) = qrs(x,y) - dhs_tmp / dt_model
      !dhs_riv(k) = dhs_riv(k) + dhs_tmp * area / ch%area
    enddo  ! iGrid/
  enddo  ! k/
!---------------------------------------------------------------
!
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
!
!---------------------------------------------------------------
subroutine calc_dhs(dhs)
  implicit none
  real(8), intent(out) :: dhs

  !-------------------------------------------------------------
  ! (Case a) : (height = 0 and hr_top < 0) or (height > 0 and hr_top < 0 and hs_top <= height)
  ! -> From slope to river : step fall (dhs : negative)
  if( ( ch%height == 0.d0 .and. hr_top < 0.d0 ) .or. &
      ( ch%height > 0.d0 .and. hr_top < 0.d0 .and. hs_top <= ch%height ) )then

    dhs = -mu1 * hs_top * sqrt(GRAVITY * hs_top) * dt_model * leng * 2.d0 / area
    if( dhs < -hs(x,y) ) dhs = -hs(x,y)

  !-------------------------------------------------------------
  ! (Case b) : 0 <= hr_top <= height and hs_top <= height
  ! -> No exchange
  elseif( 0.d0 <= hr_top .and. hr_top <= ch%height .and. &
          hs_top <= ch%height )then

    dhs = 0.d0

  !-------------------------------------------------------------
  ! (Case c) : hs <= hr and hr >= height
  ! -> From river to slope : overtopping (dhs : positive)
  ! (incl. hs = 0 and hr > 0)
  elseif( hs_top <= hr_top .and. hr_top >= ch%height )then

    h1 = hr_top - ch%height
    h2 = hs_top - ch%height
    if( h2 / h1 <= 2.d0 / 3.d0 )then
      dhs = mu2 * h1 * sqrt(2.d0*GRAVITY*h1) * dt_model * leng * 2.d0 / area
    else
      dhs = mu3 * h2 * sqrt(2.d0*GRAVITY*(h1-h2)) * dt_model * leng * 2.d0 / area
    endif

    call sec_h2b(hr_idx(k), k, b)
    dhs = min(dhs, (hr_top - ch%height) * (leng * b) / area)

  !-------------------------------------------------------------
  ! (Case d) : hs > hr & hs >= height
  ! -> From slope to river : overtopping (dhs : negative)
  ! (incl. hs = 0 and hr > 0)
  elseif( hs_top >= hr_top .and. hs_top >= ch%height )then

    h1 = hs_top - ch%height
    h2 = hr_top - ch%height
    if( h2 / h1 <= 2.d0 / 3.d0 )then
      dhs = -mu2 * h1 * sqrt(2.d0 * GRAVITY * h1) * dt_model * leng * 2.d0 / area
    else
      dhs = -mu3 * h2 * sqrt(2.d0 * GRAVITY * (h1-h2)) * dt_model * leng * 2.d0 / area
    endif

    dhs = max(dhs, -(hs_top - ch%height))

  !-------------------------------------------------------------
  ! Case: ERROR
  else
    ! Condition not considered above
    stop "Error : RivSlo"
  endif
end subroutine calc_dhs
!---------------------------------------------------------------
!
!---------------------------------------------------------------
end subroutine funcrs
!===============================================================
!
!===============================================================
end module mod_rivslo
