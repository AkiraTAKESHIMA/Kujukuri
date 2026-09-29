module mod_forcing
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: prepare_rain
  public :: prepare_evp

  public :: get_rain
  public :: get_evp

  public :: evp
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer :: tt_max_rain
  integer :: nx_rain, ny_rain
  integer, allocatable :: t_rain(:)
  integer, allocatable :: rain_i(:), rain_j(:)
  real(8), allocatable :: qp(:,:,:)

  integer, save :: tt_max_evp
  integer, save :: nx_evp, ny_evp
  integer, allocatable, save :: t_evp(:)
  integer, allocatable, save :: evp_i(:), evp_j(:)
  real(8), allocatable :: qe(:,:,:)
  real(8), allocatable :: aevp(:,:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prepare_rain()
  use mod_globals
  implicit none
  integer :: i, j
  integer :: t, tt
  integer :: ios
  real(8) :: rdummy

  open( 11, file = rainfile, status = 'old' )

  tt = 0
  do
    read(11, *, iostat = ios) t, nx_rain, ny_rain
    do i = 1, ny_rain
      read(11, *, iostat = ios) (rdummy, j = 1, nx_rain)
    enddo
    if( ios.lt.0 ) exit
    tt = tt + 1
  enddo
  tt_max_rain = tt - 1

  allocate(t_rain(0:tt_max_rain))
  allocate(qp(0:tt_max_rain, ny_rain, nx_rain))

  write(*,*) 'rain', tt_max_rain, nx_rain, ny_rain

  rewind(11)

  qp = 0.d0
  !qp_t = 0.d0 ! added by T.Sayamaa on Dec 7, 2022 v1.4.2.7
  do tt = 0, tt_max_rain
    read(11, *) t_rain(tt), nx_rain, ny_rain
    do i = 1, ny_rain
      read(11, *) (qp(tt, i, j), j = 1, nx_rain)
    enddo
  enddo

  close(11)
  ! unit convert from (mm/h) to (m/s)
  qp = qp / 3600.d0 / 1000.d0

  allocate(rain_j(nx))
  allocate(rain_i(ny))

  do j = 1, nx
    rain_j(j) = int( (xllcorner + (dble(j) - 0.5d0) * cellsize - xllcorner_rain) / cellsize_rain_x ) + 1
  enddo
  do i = 1, ny
    rain_i(i) = ny_rain - int( (yllcorner + (dble(ny) - dble(i) + 0.5d0) * cellsize - yllcorner_rain) / cellsize_rain_y )
  enddo

  write(*,*) "done: reading rain file"
end subroutine prepare_rain
!===============================================================
!
!===============================================================
subroutine prepare_evp()
  use mod_globals
  implicit none
  integer :: i, j
  integer :: t, tt
  integer :: ios
  real(8) :: rdummy

  if( evp_switch == 0 ) return

  open( 11, file = evpfile, status = 'old' )

  tt = 0
  do
    read(11, *, iostat = ios) t, nx_evp, ny_evp
    do i = 1, ny_evp
      read(11, *, iostat = ios) (rdummy, j = 1, nx_evp)
    enddo
    if( ios.lt.0 ) exit
    tt = tt + 1
  enddo
  tt_max_evp = tt - 1

  allocate( t_evp(0:tt_max_evp) )
  allocate( qe(0:tt_max_evp, ny_evp, nx_evp) )

  rewind(11)

  qe = 0.d0
  do tt = 0, tt_max_evp
    read(11, *) t_evp(tt), nx_evp, ny_evp
    do i = 1, ny_evp
      read(11, *) (qe(tt, i, j), j = 1, nx_evp)
    enddo
  enddo

  close(11)

  ! unit convert from (mm/h) to (m/s)
  qe = qe / 3600.d0 / 1000.d0

  allocate( aevp(ny, nx) )
  aevp(:,:) = 0.d0
  where( domain == 0 ) aevp = -0.1d0

  allocate(evp_j(nx))
  allocate(evp_i(ny))

  do j = 1, nx
    evp_j(j) = int( (xllcorner + (dble(j) - 0.5d0) * cellsize - xllcorner_evp) / cellsize_evp_x ) + 1
  enddo
  do i = 1, ny
    evp_i(i) = ny_evp - int( (yllcorner + (dble(ny) - dble(i) + 0.5d0) * cellsize - yllcorner_evp) / cellsize_evp_y )
  enddo

  write(*,*) "done: reading evp file"
end subroutine prepare_evp
!===============================================================
!
!===============================================================
subroutine get_rain(qp_t)
  use mod_globals
  implicit none
  real(8), intent(out) :: qp_t(ny, nx)

  integer :: itemp, jtemp
  integer :: i, j

  itemp = -1
  do jtemp = 1, tt_max_rain
    if( t_rain(jtemp-1) .lt. time + ddt .and. time + ddt .le. t_rain(jtemp) ) itemp = jtemp
  enddo

  do i = 1, ny
    if(rain_i(i) .lt. 1 .or. rain_i(i) .gt. ny_rain ) cycle
    do j = 1, nx
      if(rain_j(j) .lt. 1 .or. rain_j(j) .gt. nx_rain ) cycle
      qp_t(i, j) = qp(itemp, rain_i(i), rain_j(j))
    enddo
  enddo
end subroutine get_rain
!===============================================================
!
!===============================================================
subroutine get_evp(qe_t)
  use mod_globals
  implicit none
  real(8), intent(out) :: qe_t(ny, nx)

  integer :: itemp, jtemp
  integer :: i, j

  itemp = -1
  do jtemp = 1, tt_max_evp
    if( t_evp(jtemp-1) .lt. time .and. time .le. t_evp(jtemp) ) itemp = jtemp
  enddo
  do i = 1, ny
    do j = 1, nx
      qe_t(i, j) = qe(itemp, evp_i(i), evp_j(j))
    enddo
  enddo
end subroutine get_evp
!===============================================================
!
!===============================================================
subroutine evp( &
  qe_t_idx, hs_idx, gampt_ff_idx, aevp_tsas, &
  aevp_sum, pevp_sum &
 )
  use mod_globals
  implicit none
  real(8), intent(in) :: qe_t_idx(slo_count)
  real(8), intent(inout) :: hs_idx(slo_count)
  real(8), intent(inout) :: gampt_ff_idx(slo_count)
  real(8), intent(inout) :: aevp_tsas(slo_count)
  real(8), intent(inout) :: aevp_sum
  real(8), intent(inout) :: pevp_sum

  real(8) :: qe_t_temp
  integer :: i, j, k, itemp, jtemp

  ! evp_switch = 1 : Allow ET from gampt_ff_idx
  !            = 2 : Do not take ET from gampt_ff_idx

  do k = 1, slo_count
    i = slo_idx2i(k)
    j = slo_idx2j(k)
    qe_t_temp = qe_t_idx(k)

    ! from ver 1.3.3
    !if( dm_idx(k) .gt. 0.d0 .and. hs_idx(k) .lt. dm_idx(k) ) then
    ! qe_t_temp = qe_t_idx(k) * hs_idx(k) / dm_idx(k)
    !endif

    if( evp_switch .eq. 1 ) then
      aevp(i, j) = min( qe_t_temp, (hs_idx(k) + gampt_ff_idx(k)) / dt )
    else
      aevp(i, j) = min( qe_t_temp, hs_idx(k) / dt )
    endif
    aevp_tsas(k) = min( qe_t_temp, hs_idx(k) / dt )

    hs_idx(k) = hs_idx(k) - aevp(i, j) * dt
    if( hs_idx(k) .lt. 0.d0 ) then

      if( evp_switch .eq. 1 ) gampt_ff_idx(k) = gampt_ff_idx(k) + hs_idx(k)

      hs_idx(k) = 0.d0
      if( gampt_ff_idx(k) .lt. 0.d0 ) then
        gampt_ff_idx(k) = 0.d0
      endif
    endif
    aevp_sum = aevp_sum + aevp(i, j) * dt * area
    pevp_sum = pevp_sum + qe_t_idx(k) * dt * area
  enddo
end subroutine evp
!===============================================================
!
!===============================================================
end module mod_forcing

