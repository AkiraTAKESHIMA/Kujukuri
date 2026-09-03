module mod_forcing
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: open_file_forcing
  public :: close_file_forcing
  public :: load_forcing_data
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine open_file_forcing(forcing, grid)
  implicit none
  type(forcing_), intent(inout) :: forcing
  type(grid_), intent(in) :: grid

  open(newunit=forcing%un_prcp, file=forcing%file_prcp, &
       form='unformatted', access='direct', recl=4_8*grid%nx*grid%ny, &
       action='read', status='old')

  allocate(forcing%prcp_real(grid%nx, grid%ny))
end subroutine open_file_forcing
!===============================================================
!
!===============================================================
subroutine close_file_forcing(forcing)
  implicit none
  type(forcing_), intent(inout) :: forcing

  deallocate(forcing%prcp_real)

  close(forcing%un_prcp)
end subroutine close_file_forcing
!===============================================================
!
!===============================================================
subroutine load_forcing_data(time, forcing, qp)
  implicit none
  type(time_), intent(inout) :: time
  type(forcing_), intent(inout) :: forcing
  real(8), intent(inout) :: qp(:,:)

  if( time%t_now < time%t_forcing_next ) return

  time%t_forcing_next = time%t_forcing_next + time%dt_forcing
  time%count_forcing = time%count_forcing + 1

  read(forcing%un_prcp, rec=time%count_forcing) forcing%prcp_real

  where( forcing%prcp_real == forcing%prcp_miss )
    forcing%prcp_real = 0.d0
  endwhere

  ! real(4) -> real(8)
  qp = real(forcing%prcp_real,8)

  ! [mm/h] -> [m/s]
  qp(:,:) = qp(:,:) / (3.6d2 * 1d3)
end subroutine load_forcing_data
!===============================================================
end module mod_forcing
