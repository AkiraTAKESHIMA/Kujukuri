module mod_forcing
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_math
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: open_file_prcp
  public :: close_file_prcp
  public :: read_prcp
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_forcing'

  real(4), allocatable :: prcp(:,:)

  integer :: un_prcp
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine open_file_prcp(config, grid)
  implicit none
  type(config_), intent(in) :: config
  type(grid_), intent(in) :: grid

  open(newunit=un_prcp, file=config%input%forcing%file_prcp, &
       form='unformatted', access='direct', recl=4_8*grid%nx*grid%ny, &
       action='read', status='old')

  allocate(prcp(grid%nx, grid%ny))
end subroutine open_file_prcp
!===============================================================
!
!===============================================================
subroutine close_file_prcp()
  implicit none

  close(un_prcp)

  deallocate(prcp)
end subroutine close_file_prcp
!===============================================================
!
!===============================================================
subroutine read_prcp(config, static, time, forcing)
  use mod_util, only: &
    sub_slo_ij2idx
  implicit none
  type(config_), intent(in) :: config
  type(static_), intent(in) :: static
  type(time_), intent(inout) :: time
  type(forcing_), intent(inout) :: forcing

  if( time%t_now < time%t_forcing_next ) return

  time%t_forcing_next = time%t_forcing_next + time%dt_forcing
  time%count_forcing = time%count_forcing + 1

  !call logmsg('forcing t step: '//str(time%count_forcing))
  read(un_prcp, rec=time%count_forcing) prcp

  where( prcp == config%input%forcing%prcp_miss )
    prcp = 0.0
  endwhere

  forcing%prcp = real(prcp, 8)

  ! [mm/h] -> [m/s]
  forcing%prcp(:,:) = forcing%prcp(:,:) / (3.6d2 * 1d3)

  call sub_slo_ij2idx(static%slope, forcing%prcp, forcing%prcp_idx)
end subroutine read_prcp
!===============================================================
!
!===============================================================
end module mod_forcing
