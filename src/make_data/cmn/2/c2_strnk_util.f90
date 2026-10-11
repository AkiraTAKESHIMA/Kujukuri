module c2_strnk_util
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_array
  use lib_io
  use c2_strnk_const
  use c2_strnk_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: clear_cmn_network
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_strnk_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine clear_cmn_network(nwk)
  implicit none
  type(cmn_network_), intent(inout) :: nwk

  nwk%uid = ''
  deallocate(nwk%uid)

  nwk%nCh = 0
  nwk%nWsys = 0
  nwk%nNode = 0
  deallocate(nwk%wsys_)
  deallocate(nwk%channel_)
  if( associated(nwk%node_) ) deallocate(nwk%node_)
end subroutine clear_cmn_network
!===============================================================
!
!===============================================================
end module c2_strnk_util
