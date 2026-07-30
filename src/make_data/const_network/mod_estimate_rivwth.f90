module mod_estimate_rivwth
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: estimateRivwth
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type node_adj_
    integer :: i
  end type

  type node_
    integer :: i  ! index in network
    integer :: n_down
    type(node_adj_), pointer :: down(:)  !(n_down)
  end type

  type network_
    integer :: nNode
    type(node_), pointer :: node(:)
  end type
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine estimateRivwth()
  implicit none

  nwk%nNode = nwk%nEnt*2
  allocate(nwk%node(nwk%nNode))

  iNode = 0
  do iEnt = 1, nwk%nEnt
    part => nwk%ent(iEnt)%part
    do iPt = 1, part%nPoint, part%nPoint-1
      call add(iNode)
      node => nwk%node(iNode)
      node%i = iNode
      node%lon = part%x(iPt)
      node%lat = part%y(iPt)
      lst_node_lon(iNode) = node%lon
      lst_node_lat(iNode) = node%lat
    enddo  ! iPt/
  enddo  ! iEnt/

end subroutine estimateRivwth
!===============================================================
!
!===============================================================
recursive subroutine go_down()
  implicit none

  selectcase( node%typ )
  case( NODETYPE_NEW__OUTLET )
    return
  case( NODETYPE_NEW__INTER )
    selectcase( node%n_down )
    case( 0 )
    case( 1 )
      node => nwk%node(node%down(1)%i)
    case( 2: )
      do iDown = 1, node%n_down
        call go_down(nwk%node(node%down(iDown)%i))
      enddo
    endselect
  case( NODETYPE_NEW__SRC )

  case default

  endselect
end subroutine go_down
!===============================================================
!
!===============================================================
end module mod_estimate_rivwth
