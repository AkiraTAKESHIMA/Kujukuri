module c3_joint_grid
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: conv_fdr_jflw2rri
  public :: conv_fdr_rri2jflw
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_joint_grid'
  !-------------------------------------------------------------
  ! Inerfaces
  !-------------------------------------------------------------
  interface conv_fdr_jflw2rri
    module procedure conv_fdr_jflw2rri__2d
    module procedure conv_fdr_jflw2rri__1d
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine conv_fdr_jflw2rri__2d(fdr, fdr_rri)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'conv_fdr_jflw2rri__2d'
  integer(1), intent(in) :: fdr(:,:)
  integer(4), intent(out) :: fdr_rri(:,:)

  integer :: i

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do i = 1, size(fdr,2)
    call conv_fdr_jflw2rri__1d(fdr(:,i), fdr_rri(:,i))
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine conv_fdr_jflw2rri__2d
!===============================================================
!
!===============================================================
subroutine conv_fdr_jflw2rri__1d(fdr, fdr_rri)
  use c3_rri_const, only: &
    RRI_FDR__WEST      , &
    RRI_FDR__SOUTHWEST , &
    RRI_FDR__SOUTH     , &
    RRI_FDR__SOUTHEAST , &
    RRI_FDR__EAST      , &
    RRI_FDR__NORTHEAST , &
    RRI_FDR__NORTH     , &
    RRI_FDR__NORTHWEST , &
    RRI_FDR__RIVERMOUTH, &
    RRI_FDR__MISS      
  use c2_jflw_const, only: &
    FDR__WEST      , &
    FDR__SOUTHWEST , &
    FDR__SOUTH     , &
    FDR__SOUTHEAST , &
    FDR__EAST      , &
    FDR__NORTHEAST , &
    FDR__NORTH     , &
    FDR__NORTHWEST , &
    FDR__RIVERMOUTH, &
    FDR__INLAND    , &
    FDR__MISS
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'conv_fdr_jflw2rri__1d'
  integer(1), intent(in) :: fdr(:)
  integer(4), intent(out) :: fdr_rri(:)

  integer :: i

  do i = 1, size(fdr)
    selectcase( fdr(i) )
    case( FDR__WEST )
      fdr_rri(i) = RRI_FDR__WEST
    case( FDR__SOUTHWEST )
      fdr_rri(i) = RRI_FDR__SOUTHWEST
    case( FDR__SOUTH )
      fdr_rri(i) = RRI_FDR__SOUTH
    case( FDR__SOUTHEAST )
      fdr_rri(i) = RRI_FDR__SOUTHEAST
    case( FDR__EAST )
      fdr_rri(i) = RRI_FDR__EAST
    case( FDR__NORTHEAST )
      fdr_rri(i) = RRI_FDR__NORTHEAST
    case( FDR__NORTH )
      fdr_rri(i) = RRI_FDR__NORTH
    case( FDR__NORTHWEST )
      fdr_rri(i) = RRI_FDR__NORTHWEST
    case( FDR__RIVERMOUTH, &
          FDR__INLAND )
      fdr_rri(i) = RRI_FDR__RIVERMOUTH
    case( FDR__MISS )
      fdr_rri(i) = RRI_FDR__MISS
    case default
      call errend(msg_invalid_value('fdr', fdr(i)), &
                  '', PRCNAM, MODNAM)
    endselect
  enddo
end subroutine conv_fdr_jflw2rri__1d
!===============================================================
!
!===============================================================
subroutine conv_fdr_rri2jflw(fdr_rri, fdr)
  use c3_rri_const, only: &
    RRI_FDR__WEST      , &
    RRI_FDR__SOUTHWEST , &
    RRI_FDR__SOUTH     , &
    RRI_FDR__SOUTHEAST , &
    RRI_FDR__EAST      , &
    RRI_FDR__NORTHEAST , &
    RRI_FDR__NORTH     , &
    RRI_FDR__NORTHWEST , &
    RRI_FDR__RIVERMOUTH, &
    RRI_FDR__MISS      
  use c2_jflw_const, only: &
    FDR__WEST      , &
    FDR__SOUTHWEST , &
    FDR__SOUTH     , &
    FDR__SOUTHEAST , &
    FDR__EAST      , &
    FDR__NORTHEAST , &
    FDR__NORTH     , &
    FDR__NORTHWEST , &
    FDR__RIVERMOUTH, &
    FDR__MISS
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'conv_fdr_rri2jflw'
  integer(4), intent(in)  :: fdr_rri(:,:)
  integer(1), intent(out) :: fdr(:,:)

  integer :: igx, igy

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do igy = 1, size(fdr_rri,2)
  do igx = 1, size(fdr_rri,1)
    selectcase( fdr_rri(igx,igy) )
    case( RRI_FDR__EAST )
      fdr(igx,igy) = FDR__EAST
    case( RRI_FDR__SOUTHEAST )
      fdr(igx,igy) = FDR__SOUTHEAST
    case( RRI_FDR__SOUTH )
      fdr(igx,igy) = FDR__SOUTH
    case( RRI_FDR__SOUTHWEST )
      fdr(igx,igy) = FDR__SOUTHWEST
    case( RRI_FDR__WEST )
      fdr(igx,igy) = FDR__WEST
    case( RRI_FDR__NORTHWEST )
      fdr(igx,igy) = FDR__NORTHWEST
    case( RRI_FDR__NORTH )
      fdr(igx,igy) = FDR__NORTH
    case( RRI_FDR__NORTHEAST )
      fdr(igx,igy) = FDR__NORTHEAST
    case( RRI_FDR__RIVERMOUTH )
      fdr(igx,igy) = FDR__RIVERMOUTH
    case( RRI_FDR__MISS )
      fdr(igx,igy) = FDR__MISS
    case default
      call errend(msg_invalid_value('fdr_rri', fdr_rri(igx,igy)))
    endselect
  enddo
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine conv_fdr_rri2jflw
!===============================================================
!
!===============================================================
end module c3_joint_grid
