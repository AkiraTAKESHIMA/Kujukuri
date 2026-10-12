module def_type
  use lib_const
  use c1_const
  use c2_strnk_const, only: &
    DGT_NWKID
  use c2_strnk_type, only: &
    cmn_node_, &
    cmn_channel_, &
    cmn_watsys_, &
    cmn_nwknode_, &
    cmn_network_
  implicit none
  public
  !-------------------------------------------------------------
  ! Public module variables (type)
  !-------------------------------------------------------------
  type, extends(cmn_node_) :: node_
    logical :: is_outlet
  end type

  type isct_
    integer :: n
    integer, pointer :: gx(:), gy(:)
    real(8), pointer :: leng(:)
    integer :: gxs, gxe, gys, gye
  end type

  type, extends(cmn_channel_) :: channel_
    type(node_), pointer :: node(:)
    real(8) :: west, east, south, north
    type(isct_) :: isct
    logical :: is_isolated
  end type

  type, extends(cmn_watsys_) :: watsys_
  end type

  type, extends(cmn_nwknode_) :: nwknode_
    integer :: iNode_to_outlet
    integer :: jjCh_to_outlet
  end type

  type, extends(cmn_network_) :: network_
    type(watsys_), pointer :: wsys(:)
    type(channel_), pointer :: channel(:)
    type(nwknode_), pointer :: node(:)
    real(8) :: west, east, south, north
    integer :: gxs, gxe, gys, gye
    integer, pointer :: jCh(:)
  end type

  type nwkattr_
    character(DGT_NWKID) :: id
    integer :: nCh
    real(8) :: leng
    real(8) :: west, east, south, north
    type(isct_) :: isct
  end type
end module def_type
