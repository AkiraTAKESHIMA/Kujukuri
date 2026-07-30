module lib_array_copy
  use lib_array_realloc
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: cpval
  public :: cparr
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface cpval
    module procedure cpval__log1_to_log1_1d
    module procedure cpval__log1_to_log4_1d
    module procedure cpval__log4_to_log1_1d
    module procedure cpval__log4_to_log4_1d
    module procedure cpval__int1_to_int1_1d
    module procedure cpval__int1_to_int2_1d
    module procedure cpval__int1_to_int4_1d
    module procedure cpval__int1_to_int8_1d
    module procedure cpval__int1_to_real_1d
    module procedure cpval__int1_to_dble_1d
    module procedure cpval__int2_to_int1_1d
    module procedure cpval__int2_to_int2_1d
    module procedure cpval__int2_to_int4_1d
    module procedure cpval__int2_to_int8_1d
    module procedure cpval__int2_to_real_1d
    module procedure cpval__int2_to_dble_1d
    module procedure cpval__int4_to_int1_1d
    module procedure cpval__int4_to_int2_1d
    module procedure cpval__int4_to_int4_1d
    module procedure cpval__int4_to_int8_1d
    module procedure cpval__int4_to_real_1d
    module procedure cpval__int4_to_dble_1d
    module procedure cpval__int8_to_int1_1d
    module procedure cpval__int8_to_int2_1d
    module procedure cpval__int8_to_int4_1d
    module procedure cpval__int8_to_int8_1d
    module procedure cpval__int8_to_real_1d
    module procedure cpval__int8_to_dble_1d
    module procedure cpval__real_to_int1_1d
    module procedure cpval__real_to_int2_1d
    module procedure cpval__real_to_int4_1d
    module procedure cpval__real_to_int8_1d
    module procedure cpval__real_to_real_1d
    module procedure cpval__real_to_dble_1d
    module procedure cpval__dble_to_int1_1d
    module procedure cpval__dble_to_int2_1d
    module procedure cpval__dble_to_int4_1d
    module procedure cpval__dble_to_int8_1d
    module procedure cpval__dble_to_real_1d
    module procedure cpval__dble_to_dble_1d
    module procedure cpval__log1_to_log1_2d
    module procedure cpval__log1_to_log4_2d
    module procedure cpval__log4_to_log1_2d
    module procedure cpval__log4_to_log4_2d
    module procedure cpval__int1_to_int1_2d
    module procedure cpval__int1_to_int2_2d
    module procedure cpval__int1_to_int4_2d
    module procedure cpval__int1_to_int8_2d
    module procedure cpval__int1_to_real_2d
    module procedure cpval__int1_to_dble_2d
    module procedure cpval__int2_to_int1_2d
    module procedure cpval__int2_to_int2_2d
    module procedure cpval__int2_to_int4_2d
    module procedure cpval__int2_to_int8_2d
    module procedure cpval__int2_to_real_2d
    module procedure cpval__int2_to_dble_2d
    module procedure cpval__int4_to_int1_2d
    module procedure cpval__int4_to_int2_2d
    module procedure cpval__int4_to_int4_2d
    module procedure cpval__int4_to_int8_2d
    module procedure cpval__int4_to_real_2d
    module procedure cpval__int4_to_dble_2d
    module procedure cpval__int8_to_int1_2d
    module procedure cpval__int8_to_int2_2d
    module procedure cpval__int8_to_int4_2d
    module procedure cpval__int8_to_int8_2d
    module procedure cpval__int8_to_real_2d
    module procedure cpval__int8_to_dble_2d
    module procedure cpval__real_to_int1_2d
    module procedure cpval__real_to_int2_2d
    module procedure cpval__real_to_int4_2d
    module procedure cpval__real_to_int8_2d
    module procedure cpval__real_to_real_2d
    module procedure cpval__real_to_dble_2d
    module procedure cpval__dble_to_int1_2d
    module procedure cpval__dble_to_int2_2d
    module procedure cpval__dble_to_int4_2d
    module procedure cpval__dble_to_int8_2d
    module procedure cpval__dble_to_real_2d
    module procedure cpval__dble_to_dble_2d
    module procedure cpval__log1_to_log1_3d
    module procedure cpval__log1_to_log4_3d
    module procedure cpval__log4_to_log1_3d
    module procedure cpval__log4_to_log4_3d
    module procedure cpval__int1_to_int1_3d
    module procedure cpval__int1_to_int2_3d
    module procedure cpval__int1_to_int4_3d
    module procedure cpval__int1_to_int8_3d
    module procedure cpval__int1_to_real_3d
    module procedure cpval__int1_to_dble_3d
    module procedure cpval__int2_to_int1_3d
    module procedure cpval__int2_to_int2_3d
    module procedure cpval__int2_to_int4_3d
    module procedure cpval__int2_to_int8_3d
    module procedure cpval__int2_to_real_3d
    module procedure cpval__int2_to_dble_3d
    module procedure cpval__int4_to_int1_3d
    module procedure cpval__int4_to_int2_3d
    module procedure cpval__int4_to_int4_3d
    module procedure cpval__int4_to_int8_3d
    module procedure cpval__int4_to_real_3d
    module procedure cpval__int4_to_dble_3d
    module procedure cpval__int8_to_int1_3d
    module procedure cpval__int8_to_int2_3d
    module procedure cpval__int8_to_int4_3d
    module procedure cpval__int8_to_int8_3d
    module procedure cpval__int8_to_real_3d
    module procedure cpval__int8_to_dble_3d
    module procedure cpval__real_to_int1_3d
    module procedure cpval__real_to_int2_3d
    module procedure cpval__real_to_int4_3d
    module procedure cpval__real_to_int8_3d
    module procedure cpval__real_to_real_3d
    module procedure cpval__real_to_dble_3d
    module procedure cpval__dble_to_int1_3d
    module procedure cpval__dble_to_int2_3d
    module procedure cpval__dble_to_int4_3d
    module procedure cpval__dble_to_int8_3d
    module procedure cpval__dble_to_real_3d
    module procedure cpval__dble_to_dble_3d
  end interface

  interface cparr
    module procedure cparr__int1_1d
    module procedure cparr__int1_2d
    module procedure cparr__int1_3d
    module procedure cparr__int2_1d
    module procedure cparr__int2_2d
    module procedure cparr__int2_3d
    module procedure cparr__int4_1d
    module procedure cparr__int4_2d
    module procedure cparr__int4_3d
    module procedure cparr__int8_1d
    module procedure cparr__int8_2d
    module procedure cparr__int8_3d
    module procedure cparr__real_1d
    module procedure cparr__real_2d
    module procedure cparr__real_3d
    module procedure cparr__dble_1d
    module procedure cparr__dble_2d
    module procedure cparr__dble_3d
  end interface
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine cpval__log1_to_log1_1d(ain, aout)
  implicit none
  logical(1), intent(in) :: ain(:)
  logical(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = logical(ain(i),1)
  enddo
end subroutine cpval__log1_to_log1_1d
!===============================================================
!
!===============================================================
subroutine cpval__log1_to_log4_1d(ain, aout)
  implicit none
  logical(1), intent(in) :: ain(:)
  logical(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = logical(ain(i),4)
  enddo
end subroutine cpval__log1_to_log4_1d
!===============================================================
!
!===============================================================
subroutine cpval__log4_to_log1_1d(ain, aout)
  implicit none
  logical(4), intent(in) :: ain(:)
  logical(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = logical(ain(i),1)
  enddo
end subroutine cpval__log4_to_log1_1d
!===============================================================
!
!===============================================================
subroutine cpval__log4_to_log4_1d(ain, aout)
  implicit none
  logical(4), intent(in) :: ain(:)
  logical(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = logical(ain(i),4)
  enddo
end subroutine cpval__log4_to_log4_1d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int1_1d(ain, aout)
  implicit none
  integer(1), intent(in) :: ain(:)
  integer(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),1)
  enddo
end subroutine cpval__int1_to_int1_1d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int2_1d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:)
  integer(2), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),2)
  enddo
end subroutine cpval__int1_to_int2_1d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int4_1d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:)
  integer(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),4)
  enddo
end subroutine cpval__int1_to_int4_1d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int8_1d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:)
  integer(8), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),8)
  enddo
end subroutine cpval__int1_to_int8_1d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_real_1d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:)
  real(4)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),4)
  enddo
end subroutine cpval__int1_to_real_1d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_dble_1d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:)
  real(8)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),8)
  enddo
end subroutine cpval__int1_to_dble_1d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int1_1d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:)
  integer(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),1)
  enddo
end subroutine cpval__int2_to_int1_1d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int2_1d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:)
  integer(2), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),2)
  enddo
end subroutine cpval__int2_to_int2_1d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int4_1d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:)
  integer(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),4)
  enddo
end subroutine cpval__int2_to_int4_1d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int8_1d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:)
  integer(8), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),8)
  enddo
end subroutine cpval__int2_to_int8_1d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_real_1d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:)
  real(4)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),4)
  enddo
end subroutine cpval__int2_to_real_1d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_dble_1d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:)
  real(8)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),8)
  enddo
end subroutine cpval__int2_to_dble_1d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int1_1d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:)
  integer(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),1)
  enddo
end subroutine cpval__int4_to_int1_1d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int2_1d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:)
  integer(2), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),2)
  enddo
end subroutine cpval__int4_to_int2_1d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int4_1d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:)
  integer(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),4)
  enddo
end subroutine cpval__int4_to_int4_1d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int8_1d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:)
  integer(8), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),8)
  enddo
end subroutine cpval__int4_to_int8_1d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_real_1d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:)
  real(4)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),4)
  enddo
end subroutine cpval__int4_to_real_1d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_dble_1d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:)
  real(8)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),8)
  enddo
end subroutine cpval__int4_to_dble_1d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int1_1d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:)
  integer(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),1)
  enddo
end subroutine cpval__int8_to_int1_1d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int2_1d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:)
  integer(2), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),2)
  enddo
end subroutine cpval__int8_to_int2_1d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int4_1d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:)
  integer(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),4)
  enddo
end subroutine cpval__int8_to_int4_1d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int8_1d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:)
  integer(8), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),8)
  enddo
end subroutine cpval__int8_to_int8_1d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_real_1d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:)
  real(4)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),4)
  enddo
end subroutine cpval__int8_to_real_1d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_dble_1d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:)
  real(8)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),8)
  enddo
end subroutine cpval__int8_to_dble_1d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int1_1d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:)
  integer(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),1)
  enddo
end subroutine cpval__real_to_int1_1d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int2_1d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:)
  integer(2), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),2)
  enddo
end subroutine cpval__real_to_int2_1d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int4_1d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:)
  integer(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),4)
  enddo
end subroutine cpval__real_to_int4_1d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int8_1d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:)
  integer(8), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),8)
  enddo
end subroutine cpval__real_to_int8_1d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_real_1d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:)
  real(4)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),4)
  enddo
end subroutine cpval__real_to_real_1d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_dble_1d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:)
  real(8)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),8)
  enddo
end subroutine cpval__real_to_dble_1d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int1_1d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:)
  integer(1), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),1)
  enddo
end subroutine cpval__dble_to_int1_1d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int2_1d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:)
  integer(2), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),2)
  enddo
end subroutine cpval__dble_to_int2_1d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int4_1d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:)
  integer(4), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),4)
  enddo
end subroutine cpval__dble_to_int4_1d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int8_1d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:)
  integer(8), intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = int(ain(i),8)
  enddo
end subroutine cpval__dble_to_int8_1d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_real_1d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:)
  real(4)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),4)
  enddo
end subroutine cpval__dble_to_real_1d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_dble_1d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:)
  real(8)   , intent(out) :: aout(:)
  integer(8) :: i

  do i = 1, size(ain)
    aout(i) = real(ain(i),8)
  enddo
end subroutine cpval__dble_to_dble_1d
!===============================================================
!
!===============================================================
subroutine cpval__log1_to_log1_2d(ain, aout)
  implicit none
  logical(1), intent(in)  :: ain(:,:)
  logical(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = ain(i1,i2)
    enddo
  enddo
end subroutine cpval__log1_to_log1_2d
!===============================================================
!
!===============================================================
subroutine cpval__log1_to_log4_2d(ain, aout)
  implicit none
  logical(1), intent(in)  :: ain(:,:)
  logical(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = ain(i1,i2)
    enddo
  enddo
end subroutine cpval__log1_to_log4_2d
!===============================================================
!
!===============================================================
subroutine cpval__log4_to_log1_2d(ain, aout)
  implicit none
  logical(4), intent(in)  :: ain(:,:)
  logical(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = ain(i1,i2)
    enddo
  enddo
end subroutine cpval__log4_to_log1_2d
!===============================================================
!
!===============================================================
subroutine cpval__log4_to_log4_2d(ain, aout)
  implicit none
  logical(4), intent(in)  :: ain(:,:)
  logical(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = ain(i1,i2)
    enddo
  enddo
end subroutine cpval__log4_to_log4_2d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int1_2d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:)
  integer(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),1)
    enddo
  enddo
end subroutine cpval__int1_to_int1_2d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int2_2d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:)
  integer(2), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),2)
    enddo
  enddo
end subroutine cpval__int1_to_int2_2d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int4_2d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:)
  integer(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int1_to_int4_2d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int8_2d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:)
  integer(8), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int1_to_int8_2d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_real_2d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:)
  real(4)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int1_to_real_2d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_dble_2d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:)
  real(8)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int1_to_dble_2d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int1_2d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:)
  integer(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),1)
    enddo
  enddo
end subroutine cpval__int2_to_int1_2d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int2_2d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:)
  integer(2), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),2)
    enddo
  enddo
end subroutine cpval__int2_to_int2_2d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int4_2d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:)
  integer(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int2_to_int4_2d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int8_2d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:)
  integer(8), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int2_to_int8_2d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_real_2d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:)
  real(4)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int2_to_real_2d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_dble_2d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:)
  real(8)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int2_to_dble_2d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int1_2d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:)
  integer(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),1)
    enddo
  enddo
end subroutine cpval__int4_to_int1_2d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int2_2d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:)
  integer(2), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),2)
    enddo
  enddo
end subroutine cpval__int4_to_int2_2d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int4_2d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:)
  integer(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int4_to_int4_2d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int8_2d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:)
  integer(8), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int4_to_int8_2d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_real_2d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:)
  real(4)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int4_to_real_2d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_dble_2d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:)
  real(8)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int4_to_dble_2d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int1_2d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:)
  integer(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),1)
    enddo
  enddo
end subroutine cpval__int8_to_int1_2d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int2_2d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:)
  integer(2), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),2)
    enddo
  enddo
end subroutine cpval__int8_to_int2_2d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int4_2d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:)
  integer(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int8_to_int4_2d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int8_2d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:)
  integer(8), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int8_to_int8_2d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_real_2d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:)
  real(4)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__int8_to_real_2d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_dble_2d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:)
  real(8)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__int8_to_dble_2d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int1_2d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:)
  integer(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),1)
    enddo
  enddo
end subroutine cpval__real_to_int1_2d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int2_2d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:)
  integer(2), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),2)
    enddo
  enddo
end subroutine cpval__real_to_int2_2d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int4_2d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:)
  integer(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__real_to_int4_2d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int8_2d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:)
  integer(8), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__real_to_int8_2d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_real_2d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:)
  real(4)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__real_to_real_2d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_dble_2d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:)
  real(8)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__real_to_dble_2d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int1_2d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:)
  integer(1), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),1)
    enddo
  enddo
end subroutine cpval__dble_to_int1_2d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int2_2d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:)
  integer(2), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),2)
    enddo
  enddo
end subroutine cpval__dble_to_int2_2d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int4_2d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:)
  integer(4), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__dble_to_int4_2d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int8_2d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:)
  integer(8), intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = int(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__dble_to_int8_2d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_real_2d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:)
  real(4)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),4)
    enddo
  enddo
end subroutine cpval__dble_to_real_2d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_dble_2d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:)
  real(8)   , intent(out) :: aout(:,:)
  integer(8) :: i1, i2

  do i2 = 1, size(ain,2)
    do i1 = 1, size(ain,1)
      aout(i1,i2) = real(ain(i1,i2),8)
    enddo
  enddo
end subroutine cpval__dble_to_dble_2d
!===============================================================
!
!===============================================================
subroutine cpval__log1_to_log1_3d(ain, aout)
  implicit none
  logical(1), intent(in)  :: ain(:,:,:)
  logical(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = ain(i1,i2,i3)
  enddo
  enddo
  enddo
end subroutine cpval__log1_to_log1_3d
!===============================================================
!
!===============================================================
subroutine cpval__log1_to_log4_3d(ain, aout)
  implicit none
  logical(1), intent(in)  :: ain(:,:,:)
  logical(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = ain(i1,i2,i3)
  enddo
  enddo
  enddo
end subroutine cpval__log1_to_log4_3d
!===============================================================
!
!===============================================================
subroutine cpval__log4_to_log1_3d(ain, aout)
  implicit none
  logical(4), intent(in)  :: ain(:,:,:)
  logical(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = ain(i1,i2,i3)
  enddo
  enddo
  enddo
end subroutine cpval__log4_to_log1_3d
!===============================================================
!
!===============================================================
subroutine cpval__log4_to_log4_3d(ain, aout)
  implicit none
  logical(4), intent(in)  :: ain(:,:,:)
  logical(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = ain(i1,i2,i3)
  enddo
  enddo
  enddo
end subroutine cpval__log4_to_log4_3d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int1_3d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:,:)
  integer(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),1)
  enddo
  enddo
  enddo
end subroutine cpval__int1_to_int1_3d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int2_3d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:,:)
  integer(2), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),2)
  enddo
  enddo
  enddo
end subroutine cpval__int1_to_int2_3d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int4_3d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:,:)
  integer(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int1_to_int4_3d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_int8_3d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:,:)
  integer(8), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int1_to_int8_3d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_real_3d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:,:)
  real(4)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int1_to_real_3d
!===============================================================
!
!===============================================================
subroutine cpval__int1_to_dble_3d(ain, aout)
  implicit none
  integer(1), intent(in)  :: ain(:,:,:)
  real(8)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int1_to_dble_3d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int1_3d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:,:)
  integer(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),1)
  enddo
  enddo
  enddo
end subroutine cpval__int2_to_int1_3d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int2_3d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:,:)
  integer(2), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),2)
  enddo
  enddo
  enddo
end subroutine cpval__int2_to_int2_3d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int4_3d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:,:)
  integer(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int2_to_int4_3d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_int8_3d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:,:)
  integer(8), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int2_to_int8_3d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_real_3d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:,:)
  real(4)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int2_to_real_3d
!===============================================================
!
!===============================================================
subroutine cpval__int2_to_dble_3d(ain, aout)
  implicit none
  integer(2), intent(in)  :: ain(:,:,:)
  real(8)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int2_to_dble_3d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int1_3d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:,:)
  integer(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),1)
  enddo
  enddo
  enddo
end subroutine cpval__int4_to_int1_3d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int2_3d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:,:)
  integer(2), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),2)
  enddo
  enddo
  enddo
end subroutine cpval__int4_to_int2_3d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int4_3d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:,:)
  integer(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int4_to_int4_3d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_int8_3d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:,:)
  integer(8), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int4_to_int8_3d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_real_3d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:,:)
  real(4)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int4_to_real_3d
!===============================================================
!
!===============================================================
subroutine cpval__int4_to_dble_3d(ain, aout)
  implicit none
  integer(4), intent(in)  :: ain(:,:,:)
  real(8)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int4_to_dble_3d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int1_3d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:,:)
  integer(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),1)
  enddo
  enddo
  enddo
end subroutine cpval__int8_to_int1_3d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int2_3d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:,:)
  integer(2), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),2)
  enddo
  enddo
  enddo
end subroutine cpval__int8_to_int2_3d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int4_3d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:,:)
  integer(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int8_to_int4_3d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_int8_3d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:,:)
  integer(8), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int8_to_int8_3d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_real_3d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:,:)
  real(4)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__int8_to_real_3d
!===============================================================
!
!===============================================================
subroutine cpval__int8_to_dble_3d(ain, aout)
  implicit none
  integer(8), intent(in)  :: ain(:,:,:)
  real(8)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__int8_to_dble_3d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int1_3d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:,:)
  integer(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),1)
  enddo
  enddo
  enddo
end subroutine cpval__real_to_int1_3d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int2_3d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:,:)
  integer(2), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),2)
  enddo
  enddo
  enddo
end subroutine cpval__real_to_int2_3d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int4_3d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:,:)
  integer(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__real_to_int4_3d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_int8_3d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:,:)
  integer(8), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__real_to_int8_3d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_real_3d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:,:)
  real(4)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__real_to_real_3d
!===============================================================
!
!===============================================================
subroutine cpval__real_to_dble_3d(ain, aout)
  implicit none
  real(4)   , intent(in)  :: ain(:,:,:)
  real(8)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__real_to_dble_3d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int1_3d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:,:)
  integer(1), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),1)
  enddo
  enddo
  enddo
end subroutine cpval__dble_to_int1_3d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int2_3d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:,:)
  integer(2), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),2)
  enddo
  enddo
  enddo
end subroutine cpval__dble_to_int2_3d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int4_3d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:,:)
  integer(4), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__dble_to_int4_3d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_int8_3d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:,:)
  integer(8), intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = int(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__dble_to_int8_3d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_real_3d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:,:)
  real(4)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),4)
  enddo
  enddo
  enddo
end subroutine cpval__dble_to_real_3d
!===============================================================
!
!===============================================================
subroutine cpval__dble_to_dble_3d(ain, aout)
  implicit none
  real(8)   , intent(in)  :: ain(:,:,:)
  real(8)   , intent(out) :: aout(:,:,:)
  integer(8) :: i1, i2, i3

  do i3 = 1, size(ain,3)
  do i2 = 1, size(ain,2)
  do i1 = 1, size(ain,1)
    aout(i1,i2,i3) = real(ain(i1,i2,i3),8)
  enddo
  enddo
  enddo
end subroutine cpval__dble_to_dble_3d
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine cparr__int1_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int1_1d
!===============================================================
!
!===============================================================
subroutine cparr__int1_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int1_2d
!===============================================================
!
!===============================================================
subroutine cparr__int1_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int1_3d
!===============================================================
!
!===============================================================
subroutine cparr__int2_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int2_1d
!===============================================================
!
!===============================================================
subroutine cparr__int2_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int2_2d
!===============================================================
!
!===============================================================
subroutine cparr__int2_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int2_3d
!===============================================================
!
!===============================================================
subroutine cparr__int4_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int4_1d
!===============================================================
!
!===============================================================
subroutine cparr__int4_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int4_2d
!===============================================================
!
!===============================================================
subroutine cparr__int4_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int4_3d
!===============================================================
!
!===============================================================
subroutine cparr__int8_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int8_1d
!===============================================================
!
!===============================================================
subroutine cparr__int8_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int8_2d
!===============================================================
!
!===============================================================
subroutine cparr__int8_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__int8_3d
!===============================================================
!
!===============================================================
subroutine cparr__real_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 1
  real(byte), intent(in) :: arr_in(:)
  real(byte), pointer    :: arr_out(:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__real_1d
!===============================================================
!
!===============================================================
subroutine cparr__real_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 2
  real(byte), intent(in) :: arr_in(:,:)
  real(byte), pointer    :: arr_out(:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__real_2d
!===============================================================
!
!===============================================================
subroutine cparr__real_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 3
  real(byte), intent(in) :: arr_in(:,:,:)
  real(byte), pointer    :: arr_out(:,:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__real_3d
!===============================================================
!
!===============================================================
subroutine cparr__dble_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 1
  real(byte), intent(in) :: arr_in(:)
  real(byte), pointer    :: arr_out(:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__dble_1d
!===============================================================
!
!===============================================================
subroutine cparr__dble_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 2
  real(byte), intent(in) :: arr_in(:,:)
  real(byte), pointer    :: arr_out(:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__dble_2d
!===============================================================
!
!===============================================================
subroutine cparr__dble_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 3
  real(byte), intent(in) :: arr_in(:,:,:)
  real(byte), pointer    :: arr_out(:,:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine cparr__dble_3d
!===============================================================
!
!===============================================================
end module lib_array_copy
