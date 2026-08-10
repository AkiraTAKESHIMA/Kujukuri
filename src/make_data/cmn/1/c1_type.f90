module c1_type
  implicit none

  type cmn_node_
    real(8) :: lon, lat
    integer :: typ
    real(8) :: elv
    real(8) :: downleng
    integer :: iNode  ! index in network
  end type

  type cmn_channel_
    character(:), allocatable :: wsCode
    character(:), allocatable :: rvCode
    character(:), allocatable :: rvName
    integer :: n
    real(8), pointer :: lon(:), lat(:)
    type(cmn_node_), pointer :: node_(:)
    real(8) :: leng
  end type

  type cmn_watsys_
    character(:), allocatable :: wsCode
    real(8) :: leng
  end type

  type cmn_nwknode_
    real(8) :: lon, lat
    integer :: typ
    real(8) :: elv
    real(8) :: downleng
    integer :: nCh
    integer, pointer :: jCh(:)
    integer, pointer :: jNode(:)
    integer, pointer :: iNode(:)  ! Indices of nwknode on the opposite side of channels
  end type

  type cmn_network_
    character(:), allocatable :: uid
    integer :: nWsys
    type(cmn_watsys_), pointer :: wsys_(:)
    integer :: nCh
    type(cmn_channel_), pointer :: channel_(:)
    integer :: nNode
    type(cmn_nwknode_), pointer :: node_(:)
  end type
end module c1_type
