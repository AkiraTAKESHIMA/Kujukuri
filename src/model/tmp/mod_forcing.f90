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

  real(4), allocatable :: qp_real(:,:)

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

  allocate(qp_real(grid%nx, grid%ny))
end subroutine open_file_prcp
!===============================================================
!
!===============================================================
subroutine close_file_prcp()
  implicit none

  close(un_prcp)

  deallocate(qp_real)
end subroutine close_file_prcp
!===============================================================
!
!===============================================================
subroutine read_prcp(it, qp)
  implicit none
  integer, intent(in) :: it
  real(8), intent(out) :: qp(:,:)  !(nx,ny)

  read(un_prcp, rec=it) qp_real
  qp = real(qp_real, 8)

  ! [mm/h] -> [m/s]
  qp(:,:) = qp(:,:) / (3.6d2 * 1d3)
end subroutine read_prcp
!===============================================================
!
!===============================================================
end module mod_forcing
