module mod_util
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: copy_cmnnwk
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_util'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine copy_cmnnwk(cmn, nwk)
  implicit none
  type(cmn_network_), intent(in) :: cmn
  type(network_), intent(out) :: nwk

  type(cmn_watsys_), pointer :: cwsys
  type(cmn_channel_), pointer :: cch
  type(cmn_node_), pointer :: cnode
  type(watsys_), pointer :: wsys
  type(channel_), pointer :: ch
  type(node_), pointer :: node
  integer :: jWsys
  integer :: jCh
  integer :: jNode

  allocate(character(1) :: nwk%id)
  nwk%id = cmn%id

  nwk%nWsys = cmn%nWsys
  allocate(nwk%wsys(nwk%nWsys))
  do jWsys = 1, nwk%nWsys
    cwsys => cmn%wsys_(jWsys)
    wsys => nwk%wsys(jWsys)

    allocate(character(1) :: wsys%wsCode)
    wsys%wsCode = cwsys%wsCode

    wsys%leng = cwsys%leng
  enddo  ! jWsys/

  nwk%nCh = cmn%nCh
  allocate(nwk%channel(nwk%nCh))
  do jCh = 1, nwk%nCh
    cch => cmn%channel_(jCh)
    ch => nwk%channel(jCh)

    allocate(character(1) :: ch%wsCode)
    allocate(character(1) :: ch%rvCode)
    allocate(character(1) :: ch%rvName)
    ch%wsCode = cch%wsCode
    ch%rvCode = cch%rvCode
    ch%rvName = cch%rvName

    ch%n = cch%n
    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))
    ch%lon(:) = cch%lon(:)
    ch%lat(:) = cch%lat(:)

    ch%leng = cch%leng

    allocate(ch%node(2))
    do jNode = 1, 2
      cnode => cch%node_(jNode)
      node => ch%node(jNode)
      node%lon = cnode%lon
      node%lat = cnode%lat
      node%typ = cnode%typ
      node%elv = cnode%elv
      node%downleng = cnode%downleng
      node%iNode = cnode%iNode
    enddo  ! jNode/
  enddo  ! jCh/
end subroutine copy_cmnnwk
!===============================================================
!
!===============================================================
end module mod_util
