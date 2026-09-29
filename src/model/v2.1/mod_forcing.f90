module mod_forcing
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: prep_forcing
  public :: finalize_forcing
  public :: load_rain
  public :: get_rain
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer :: un_rain

  type ds_rain_
    logical :: is_loaded
    real(8) :: time
    real(8), pointer :: val(:,:)  !(nx,ny)
  end type
  type(ds_rain_), allocatable, target :: ds_rain(:)  !(0:nt_rain)

  ! Workspace
  real(8), allocatable :: rain_prev(:,:)
  real(8), allocatable :: qrain_prev(:,:)
  real(8), allocatable :: rain_tmp(:,:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prep_forcing()
  implicit none

  type(ds_rain_), pointer :: ds
  integer :: it_rain
  integer :: iy
  integer :: ios

  open(newunit=un_rain, file=rainfile, status='old')

  nt_rain = -1
  do
    read(un_rain,*,iostat=ios)
    if( ios /= 0 ) exit
    nt_rain = nt_rain + 1
    do iy = 1, ny
      read(un_rain,*)
    enddo
  enddo

  allocate(ds_rain(0:nt_rain))

  rewind(un_rain)
  do it_rain = 0, nt_rain
    ds => ds_rain(it_rain)
    ds%is_loaded = .false.

    read(un_rain,*) ds%time
    do iy = 1, ny
      read(un_rain,*) ! rain
    enddo
  enddo

  rewind(un_rain)

  allocate(rain_prev(nx,ny))
  allocate(qrain_prev(nx,ny))
  allocate(rain_tmp(nx,ny))
end subroutine prep_forcing
!===============================================================
!
!===============================================================
subroutine finalize_forcing()
  implicit none

  close(un_rain)

  deallocate(rain_prev)
  deallocate(qrain_prev)
  deallocate(rain_tmp)
end subroutine finalize_forcing
!===============================================================
!
!===============================================================
subroutine load_rain(time_start, time_end)
  implicit none
  real(8), intent(in) :: time_start, time_end

  type(ds_rain_), pointer :: ds
  integer :: it, its, ite
  integer :: iy
  real(8) :: time
  integer, save :: ts_prev = -1
  integer, save :: te_prev = -1

  ! Get $its such that time(its-1) <= time_start < time(its)
  call get_t_start(time_start, its)

  ! Get $ite such that time(ite-1) < time_end <= time(ite)
  call get_t_end(time_end, ite)

  ! Move to the line of time(its)
  if( its < te_prev )then
    do it = te_prev, its, -1
      do iy = 1, ny
        backspace(un_rain)  ! rain
      enddo
      backspace(un_rain)

      read(un_rain,*)   time
      backspace(un_rain)
      print"(1x,a,f10.2)", 'read_rain | back time: ', time
    enddo
  else
    do it = te_prev+1, its-1
      read(un_rain,*)   time
      do iy = 1, ny
        read(un_rain,*)  ! rain
      enddo
      print"(1x,a,f10.2)", 'read_rain | skip time: ', time
    enddo
  endif

  ! Free arrays not to be used
  do it = ts_prev, te_prev
    if( it >= its .or. it <= ite ) cycle

    ds => ds_rain(it)
    if( .not. ds%is_loaded ) cycle
    ds%is_loaded = .false.
    deallocate(ds%val)
  enddo

  ! Read data
  print"(2(a,i0,2(a,f10.2),a))", &
    ' loading rain: t=',its,' (', ds_rain(its-1)%time, ' - ', ds_rain(its)%time, ')', &
                ' - t=',ite,' (', ds_rain(ite-1)%time, ' - ', ds_rain(ite)%time, ')'

  do it = its, ite
    ds => ds_rain(it)

    read(un_rain,*) time
    if( time /= ds%time )then
      print"(1x,a)", '*** ERROR *** time of rain mismatch.'
      print*, ds%time, time
      stop 1
    endif

    if( .not. ds%is_loaded )then
      allocate(ds%val(nx,ny))
      ds%is_loaded = .true.
      do iy = 1, ny
        read(un_rain,*) ds%val(:,iy)
      enddo

      ! mm/h -> m/s
      ds%val = ds%val / 3600.d0 / 1000.d0
    endif
  enddo

  ts_prev = its
  te_prev = ite
end subroutine load_rain
!===============================================================
!
!===============================================================
subroutine get_rain(time_start, time_end, qrain, is_updated)
  implicit none
  real(8), intent(in) :: time_start, time_end
  real(8), intent(out) :: qrain(:,:)
  logical, intent(out) :: is_updated

  type(ds_rain_), pointer :: ds
  integer :: it, its, ite
  integer, save :: its_prev = -9999
  integer, save :: ite_prev = -9999

  if( its_prev /= -9999 )then
    if( its_prev == ite_prev .and. &
        ds_rain(its_prev-1)%time <= time_start .and. &
        time_end <= ds_rain(ite_prev)%time &
    )then
      is_updated = .false.
      qrain = qrain_prev
      !print"(4(a,f10.2),1x,l1)", &
      !  ' time ', time_start, ' - ', time_end, &
      !  ' rain ', ds_rain(its_prev-1)%time, ' - ', ds_rain(ite_prev)%time, &
      !  is_updated
      return
    endif
  endif

  is_updated = .true.

  ! Get $its such that time(its-1) <= time_start < time(its)
  call get_t_start(time_start, its)

  ! Get $ite such that time(ite-1) < time_end <= time(ite)
  call get_t_end(time_end, ite)

  !print"(4(a,f10.2),1x,l1)", &
  !  ' time ', time_start, ' - ', time_end, &
  !  ' rain ', ds_rain(its-1)%time, ' - ', ds_rain(ite)%time, &
  !  is_updated

  ! Read data and calc. weighted mean
  if( its == ite )then
    qrain = ds_rain(its)%val
  else
    qrain = 0.d0

    ds => ds_rain(its)
    qrain = qrain + ds%val * ( (ds_rain(its)%time - time_start) / (time_end - time_start) )

    ds => ds_rain(ite)
    qrain = qrain + ds%val * ( (time_end - ds_rain(ite-1)%time) / (time_end - time_start) )

    do it = its+1, ite-1
      ds => ds_rain(it)
      qrain = qrain + ds%val * ( (ds%time - ds_rain(it-1)%time) / (time_end - time_start) )
    enddo
  endif

  its_prev = its
  ite_prev = ite
  qrain_prev = qrain
end subroutine get_rain
!===============================================================
! time(its-1) <= time_start < time(its)
!===============================================================
subroutine get_t_start(time_start, its)
  implicit none
  real(8), intent(in) :: time_start
  integer, intent(out) :: its

  if( time_start <= ds_rain(0)%time )then
    its = 0
  else
    its = 0
    do while( ds_rain(its)%time <= time_start )
      its = its + 1
    enddo
  endif
end subroutine get_t_start
!===============================================================
! time(ite-1) < time_end <= time(ite)
!===============================================================
subroutine get_t_end(time_end, ite)
  implicit none
  real(8), intent(in) :: time_end
  integer, intent(out) :: ite

  if( ds_rain(nt_rain)%time < time_end )then
    ite = nt_rain + 1
  else
    ite = -1
    do while( ds_rain(ite)%time < time_end )
      ite = ite + 1
    enddo
  endif
end subroutine get_t_end
!===============================================================
!
!===============================================================
end module mod_forcing
