module c1_io
  use lib_const
  use lib_base
  use lib_log
  use c1_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_network
  public :: read_network_size
  public :: write_network
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_network(f, nwk)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_network'
  character(*), intent(in) :: f
  type(cmn_network_), intent(inout) :: nwk

  type(cmn_watsys_), pointer :: wsys
  type(cmn_channel_), pointer :: ch
  type(cmn_node_), pointer :: node
  integer :: jWsys
  integer :: jCh
  integer :: jNode
  integer :: clen_wsCode, clen_rvCode, clen_rvName

  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. allocated(nwk%uid) )then
    call errend('ID of the network is undefined.')
  endif

  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')

  read(un) nwk%nWsys
  read(un) nwk%nCh
  read(un) nwk%nNode

  allocate(nwk%wsys_(nwk%nWsys))
  allocate(nwk%channel_(nwk%nCh))

  read(un) clen_wsCode
  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys_(jWsys)
    allocate(character(clen_wsCode) :: wsys%wsCode)
    read(un) wsys%wsCode, wsys%leng
  enddo

  do jCh = 1, nwk%nCh
    ch => nwk%channel_(jCh)

    read(un) clen_wsCode, clen_rvCode, clen_rvName
    allocate(character(clen_wsCode) :: ch%wsCode)
    allocate(character(clen_rvCode) :: ch%rvCode)
    allocate(character(clen_rvName) :: ch%rvName)
    read(un) ch%wsCode
    read(un) ch%rvCode
    read(un) ch%rvName

    read(un) ch%n
    allocate(ch%lon(ch%n))
    allocate(ch%lat(ch%n))
    read(un) ch%lon(:)
    read(un) ch%lat(:)

    read(un) ch%leng

    allocate(ch%node_(2))
    ch%node_(1)%lon = ch%lon(1)
    ch%node_(1)%lat = ch%lat(1)
    ch%node_(2)%lon = ch%lon(ch%n)
    ch%node_(2)%lat = ch%lat(ch%n)
    do jNode = 1, 2
      node => ch%node_(jNode)
      read(un) node%iNode, node%typ, node%elv, node%downleng
    enddo  ! jNode/
  enddo  ! jCh/

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_network
!===============================================================
!
!===============================================================
subroutine read_network_size(f, nWsys, nCh, nNode)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_network_size'
  character(*), intent(in) :: f
  integer, intent(out), optional :: nWsys, nCh, nNode

  integer :: nWsys_, nCh_, nNode_
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  open(newunit=un, file=f, form='unformatted', access='sequential', status='old')

  read(un) nWsys_
  read(un) nCh_
  read(un) nNode_

  close(un)

  if( present(nWsys) ) nWsys = nWsys_
  if( present(nCh) ) nCh = nCh_
  if( present(nNode) ) nNode = nNode_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_network_size
!===============================================================
!
!===============================================================
subroutine write_network(f, nwk)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_network'
  character(*), intent(in) :: f
  type(cmn_network_), intent(in) :: nwk

  type(cmn_watsys_), pointer :: wsys
  type(cmn_channel_), pointer :: ch
  type(cmn_node_), pointer :: node
  integer :: jWsys
  integer :: jCh
  integer :: jNode

  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  open(newunit=un, file=f, form='unformatted', access='sequential', status='replace')

  write(un) nwk%nWsys
  write(un) nwk%nCh
  write(un) nwk%nNode

  write(un) len_trim(nwk%channel_(1)%wsCode)
  do jWsys = 1, nwk%nWsys
    wsys => nwk%wsys_(jWsys)
    write(un) wsys%wsCode, wsys%leng
  enddo

  do jCh = 1, nwk%nCh
    ch => nwk%channel_(jCh)

    write(un) len(ch%wsCode), len(ch%rvCode), len(ch%rvName)
    write(un) ch%wsCode
    write(un) ch%rvCode
    write(un) ch%rvName
    write(un) ch%n
    write(un) ch%lon(:)
    write(un) ch%lat(:)
    write(un) ch%leng
    do jNode = 1, 2
      node => ch%node_(jNode)
      write(un) node%iNode, node%typ, node%elv, node%downleng
    enddo  ! jNode/
  enddo  ! iiCh/

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_network
!===============================================================
!
!===============================================================
end module c1_io
