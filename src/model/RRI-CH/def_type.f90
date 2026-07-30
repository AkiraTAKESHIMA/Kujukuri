module def_type
  implicit none

  type pt_
    real(8) :: lon, lat
    integer :: x, y
    real(8) :: leng  ! distance to the next point
    real(8) :: width
    real(8) :: depth
    real(8) :: levee
    real(8) :: zs
    real(8) :: zb
  end type

  type nd_conn_
    integer :: iCh  ! serial number
    integer :: jNode  ! 1 or 2
  end type

  type nd_
    real(8) :: lon, lat
    integer :: nNode_conn
    type(nd_conn_), pointer :: node_conn(:)  !(nNode_conn)
    integer :: iNode  ! serial number
    logical :: is_outlet
  end type

  type ch_conn_
    integer :: jNode_self  ! 1 or 2
    integer :: iCh  ! serial number
    integer :: jNode  ! 1 or 2
  end type

  type ch_mesh_
    integer :: x, y
    real(8) :: leng
    real(8) :: area
    logical :: is_outside_domain
  end type

  type ch_
    integer :: nPt
    type(pt_), pointer :: pt(:)  !(nPt)
    type(nd_), pointer :: node(:)  !(2)
    integer :: nCh_conn
    type(ch_conn_), pointer :: ch_conn(:)  !(nCh_conn)
    real(8) :: leng
    real(8) :: width
    real(8) :: depth
    real(8) :: levee
    real(8) :: zb
    real(8) :: area
    integer :: nMesh
    type(ch_mesh_), pointer :: mesh(:)  !(nMesh)
  end type

  type outlet_
    integer :: iCh
    integer :: jNode
    integer :: x, y
  end type
end module def_type
