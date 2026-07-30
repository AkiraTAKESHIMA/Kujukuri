module mod_util
  use lib_const
  use lib_base
  use lib_time
  use def_const
  use def_type
  use mod_param
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: sub_slo_ij2idx
  public :: sub_slo_idx2ij
  public :: sub_slo_idx2ij4

  public :: stime2time
  public :: progress_time
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_util'
  !-------------------------------------------------------------
contains
!===============================================================
! 2D -> 1D
!===============================================================
subroutine sub_slo_ij2idx(a, a_idx)
  implicit none
  real(8), intent(in) :: a(:,:)  !(nx,ny)
  real(8), intent(out) :: a_idx(:)  !(nSlo)
  integer :: k

  do k = 1, nSlo
    a_idx(k) = a(slo_idx2i(k),slo_idx2j(k))
  enddo
end subroutine sub_slo_ij2idx
!===============================================================
! 1D -> 2D
!===============================================================
subroutine sub_slo_idx2ij(a_idx, a)
  implicit none
  real(8), intent(in) :: a_idx(:)  !(nSlo)
  real(8), intent(out) :: a(:,:)  !(nx,ny)
  integer :: k

  a(:,:) = 0.d0
  do k = 1, nSlo
    a(slo_idx2i(k),slo_idx2j(k)) = a_idx(k)
  enddo
end subroutine sub_slo_idx2ij
!===============================================================
!
!===============================================================
subroutine sub_slo_idx2ij4(a_idx, a)
  implicit none
  real(8), intent(in) :: a_idx(:,:)  !(i4,nSlo)
  real(8), intent(out) :: a(:,:,:)  !(i4,nx,ny)
  integer :: k

  a(:,:,:) = 0.d0
  do k = 1, nSlo
    a(:,slo_idx2i(k),slo_idx2j(k)) = a_idx(:,k)
  enddo
end subroutine sub_slo_idx2ij4
!===============================================================
!
!===============================================================
subroutine stime2time(stime, time)
  implicit none
  character(*), intent(in) :: stime
  integer, intent(out) :: time(:)

  read(stime(:4),*) time(1)  ! year
  read(stime(5:6),*) time(2)  ! month
  read(stime(7:8),*) time(3)  ! day
  read(stime(9:10),*) time(4)  ! hour
  read(stime(11:12),*) time(5)  ! minutes
end subroutine stime2time
!===============================================================
!
!===============================================================
integer function get_dt_minutes(hours, minutes) result(res)
  implicit none
  integer, intent(in), optional :: hours
  integer, intent(in), optional :: minutes

  if( present(hours) )then
    res = hours * 60
  elseif( present(minutes) )then
    res = minutes
  endif
end function get_dt_minutes
!===============================================================
!
!===============================================================
subroutine progress_time(time, dt_sec)
  implicit none
  integer, intent(inout) :: time(:)
  integer, intent(in) :: dt_sec  ! seconds

  integer :: daymax
  integer :: dt  ! minutes

  dt = dt_sec / 60

  time(5) = time(5) + dt
  if( time(5) > 59 )then
    time(4) = time(4) + time(5) / 60  ! hour
    time(5) = mod(time(5), 60)

    if( time(4) > 23 )then
      time(3) = time(3) + time(4) / 24  ! day
      time(4) = mod(time(4), 24)

      daymax = days(time(1), time(2))
      if( time(3) > daymax )then
        time(2) = time(2) + 1  ! month
        time(3) = time(3) - daymax

        if( time(2) > 12 )then
          time(1) = time(1) + 1  ! year
          time(2) = mod(time(2), 12)
        endif
      endif
    endif
  endif
end subroutine progress_time
!===============================================================
!
!===============================================================
end module mod_util
