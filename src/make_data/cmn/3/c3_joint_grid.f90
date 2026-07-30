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
  use c2_rri_const, only: &
        RRI_FDR_WEST       => FDR_WEST      , &
        RRI_FDR_SOUTHWEST  => FDR_SOUTHWEST , &
        RRI_FDR_SOUTH      => FDR_SOUTH     , &
        RRI_FDR_SOUTHEAST  => FDR_SOUTHEAST , &
        RRI_FDR_EAST       => FDR_EAST      , &
        RRI_FDR_NORTHEAST  => FDR_NORTHEAST , &
        RRI_FDR_NORTH      => FDR_NORTH     , &
        RRI_FDR_NORTHWEST  => FDR_NORTHWEST , &
        RRI_FDR_RIVERMOUTH => FDR_RIVERMOUTH, &
        RRI_FDR_MISS       => FDR_MISS
  use c2_jflw_const, only: &
        FDR_WEST      , &
        FDR_SOUTHWEST , &
        FDR_SOUTH     , &
        FDR_SOUTHEAST , &
        FDR_EAST      , &
        FDR_NORTHEAST , &
        FDR_NORTH     , &
        FDR_NORTHWEST , &
        FDR_RIVERMOUTH, &
        FDR_INLAND    , &
        FDR_MISS
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'conv_fdr_jflw2rri__1d'
  integer(1), intent(in) :: fdr(:)
  integer(4), intent(out) :: fdr_rri(:)

  integer :: i

  do i = 1, size(fdr)
    selectcase( fdr(i) )
    case( FDR_WEST )
      fdr_rri(i) = RRI_FDR_WEST
    case( FDR_SOUTHWEST )
      fdr_rri(i) = RRI_FDR_SOUTHWEST
    case( FDR_SOUTH )
      fdr_rri(i) = RRI_FDR_SOUTH
    case( FDR_SOUTHEAST )
      fdr_rri(i) = RRI_FDR_SOUTHEAST
    case( FDR_EAST )
      fdr_rri(i) = RRI_FDR_EAST
    case( FDR_NORTHEAST )
      fdr_rri(i) = RRI_FDR_NORTHEAST
    case( FDR_NORTH )
      fdr_rri(i) = RRI_FDR_NORTH
    case( FDR_NORTHWEST )
      fdr_rri(i) = RRI_FDR_NORTHWEST
    case( FDR_RIVERMOUTH, &
          FDR_INLAND )
      fdr_rri(i) = RRI_FDR_RIVERMOUTH
    case( FDR_MISS )
      fdr_rri(i) = RRI_FDR_MISS
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
  use c2_rri_const, only: &
        RRI_FDR_WEST       => FDR_WEST      , &
        RRI_FDR_SOUTHWEST  => FDR_SOUTHWEST , &
        RRI_FDR_SOUTH      => FDR_SOUTH     , &
        RRI_FDR_SOUTHEAST  => FDR_SOUTHEAST , &
        RRI_FDR_EAST       => FDR_EAST      , &
        RRI_FDR_NORTHEAST  => FDR_NORTHEAST , &
        RRI_FDR_NORTH      => FDR_NORTH     , &
        RRI_FDR_NORTHWEST  => FDR_NORTHWEST , &
        RRI_FDR_RIVERMOUTH => FDR_RIVERMOUTH, &
        RRI_FDR_MISS       => FDR_MISS
  use c2_jflw_const, only: &
        FDR_WEST      , &
        FDR_SOUTHWEST , &
        FDR_SOUTH     , &
        FDR_SOUTHEAST , &
        FDR_EAST      , &
        FDR_NORTHEAST , &
        FDR_NORTH     , &
        FDR_NORTHWEST , &
        FDR_RIVERMOUTH, &
        FDR_MISS
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
    case( RRI_FDR_EAST )
      fdr(igx,igy) = FDR_EAST
    case( RRI_FDR_SOUTHEAST )
      fdr(igx,igy) = FDR_SOUTHEAST
    case( RRI_FDR_SOUTH )
      fdr(igx,igy) = FDR_SOUTH
    case( RRI_FDR_SOUTHWEST )
      fdr(igx,igy) = FDR_SOUTHWEST
    case( RRI_FDR_WEST )
      fdr(igx,igy) = FDR_WEST
    case( RRI_FDR_NORTHWEST )
      fdr(igx,igy) = FDR_NORTHWEST
    case( RRI_FDR_NORTH )
      fdr(igx,igy) = FDR_NORTH
    case( RRI_FDR_NORTHEAST )
      fdr(igx,igy) = FDR_NORTHEAST
    case( RRI_FDR_RIVERMOUTH )
      fdr(igx,igy) = FDR_RIVERMOUTH
    case( RRI_FDR_MISS )
      fdr(igx,igy) = FDR_MISS
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
