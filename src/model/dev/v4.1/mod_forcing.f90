module mod_forcing
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: load_forcing
  public :: get_prcp
  public :: get_timestep_prcp
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine load_forcing()
  implicit none

  integer :: it_prcp
  integer :: iy
  integer :: un
  integer :: ios

  open(newunit=un, file=prcpfile, status='old')

  nt_prcp = -1
  do
    read(un,*,iostat=ios)
    if( ios /= 0 ) exit
    nt_prcp = nt_prcp + 1
    do iy = 1, ny
      read(un,*)
    enddo
  enddo

  allocate(prcp_all(nx,ny,0:nt_prcp))
  allocate(time_prcp(0:nt_prcp))

  rewind(un)
  do it_prcp = 0, nt_prcp
    read(un,*) time_prcp(it_prcp)
    do iy = 1, ny
      read(un,*) prcp_all(:,iy,it_prcp)
    enddo
  enddo
  close(un)

  ! mm/h -> m/s
  prcp_all = prcp_all / 3600.d0 / 1000.d0
end subroutine load_forcing
!===============================================================
!
!===============================================================
subroutine get_prcp(it, prcp)
  implicit none
  integer, intent(in) :: it
  real(8), intent(out) :: prcp(:,:)

  prcp(:,:) = prcp_all(:,:,it)
end subroutine get_prcp
!===============================================================
!
!===============================================================
subroutine get_timestep_prcp(time, it)
  implicit none
  real(8), intent(in) :: time
  integer, intent(out) :: it

  do it = 1, nt_prcp
    if( time_prcp(it-1) < time .and. time <= time_prcp(it) ) return
  enddo
end subroutine get_timestep_prcp
!===============================================================
!
!===============================================================
end module mod_forcing
