module lib_array_realloc
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedues
  !-------------------------------------------------------------
  public :: realloc
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface realloc
    module procedure realloc_log1_1d_bnd4
    module procedure realloc_log1_1d_bnd8
    module procedure realloc_log1_2d_bnd4
    module procedure realloc_log1_2d_bnd8
    module procedure realloc_log1_3d_bnd4
    module procedure realloc_log1_3d_bnd8
    module procedure realloc_log4_1d_bnd4
    module procedure realloc_log4_1d_bnd8
    module procedure realloc_log4_2d_bnd4
    module procedure realloc_log4_2d_bnd8
    module procedure realloc_log4_3d_bnd4
    module procedure realloc_log4_3d_bnd8
    module procedure realloc_int1_1d_bnd4
    module procedure realloc_int1_1d_bnd8
    module procedure realloc_int1_2d_bnd4
    module procedure realloc_int1_2d_bnd8
    module procedure realloc_int1_3d_bnd4
    module procedure realloc_int1_3d_bnd8
    module procedure realloc_int2_1d_bnd4
    module procedure realloc_int2_1d_bnd8
    module procedure realloc_int2_2d_bnd4
    module procedure realloc_int2_2d_bnd8
    module procedure realloc_int2_3d_bnd4
    module procedure realloc_int2_3d_bnd8
    module procedure realloc_int4_1d_bnd4
    module procedure realloc_int4_1d_bnd8
    module procedure realloc_int4_2d_bnd4
    module procedure realloc_int4_2d_bnd8
    module procedure realloc_int4_3d_bnd4
    module procedure realloc_int4_3d_bnd8
    module procedure realloc_int8_1d_bnd4
    module procedure realloc_int8_1d_bnd8
    module procedure realloc_int8_2d_bnd4
    module procedure realloc_int8_2d_bnd8
    module procedure realloc_int8_3d_bnd4
    module procedure realloc_int8_3d_bnd8
    module procedure realloc_real_1d_bnd4
    module procedure realloc_real_1d_bnd8
    module procedure realloc_real_2d_bnd4
    module procedure realloc_real_2d_bnd8
    module procedure realloc_real_3d_bnd4
    module procedure realloc_real_3d_bnd8
    module procedure realloc_dble_1d_bnd4
    module procedure realloc_dble_1d_bnd8
    module procedure realloc_dble_2d_bnd4
    module procedure realloc_dble_2d_bnd8
    module procedure realloc_dble_3d_bnd4
    module procedure realloc_dble_3d_bnd8

    module procedure realloc_log1_1d_bnd4_dim
    module procedure realloc_log1_1d_bnd8_dim
    module procedure realloc_log1_2d_bnd4_dim
    module procedure realloc_log1_2d_bnd8_dim
    module procedure realloc_log1_3d_bnd4_dim
    module procedure realloc_log1_3d_bnd8_dim
    module procedure realloc_log4_1d_bnd4_dim
    module procedure realloc_log4_1d_bnd8_dim
    module procedure realloc_log4_2d_bnd4_dim
    module procedure realloc_log4_2d_bnd8_dim
    module procedure realloc_log4_3d_bnd4_dim
    module procedure realloc_log4_3d_bnd8_dim
    module procedure realloc_int1_1d_bnd4_dim
    module procedure realloc_int1_1d_bnd8_dim
    module procedure realloc_int1_2d_bnd4_dim
    module procedure realloc_int1_2d_bnd8_dim
    module procedure realloc_int1_3d_bnd4_dim
    module procedure realloc_int1_3d_bnd8_dim
    module procedure realloc_int2_1d_bnd4_dim
    module procedure realloc_int2_1d_bnd8_dim
    module procedure realloc_int2_2d_bnd4_dim
    module procedure realloc_int2_2d_bnd8_dim
    module procedure realloc_int2_3d_bnd4_dim
    module procedure realloc_int2_3d_bnd8_dim
    module procedure realloc_int4_1d_bnd4_dim
    module procedure realloc_int4_1d_bnd8_dim
    module procedure realloc_int4_2d_bnd4_dim
    module procedure realloc_int4_2d_bnd8_dim
    module procedure realloc_int4_3d_bnd4_dim
    module procedure realloc_int4_3d_bnd8_dim
    module procedure realloc_int8_1d_bnd4_dim
    module procedure realloc_int8_1d_bnd8_dim
    module procedure realloc_int8_2d_bnd4_dim
    module procedure realloc_int8_2d_bnd8_dim
    module procedure realloc_int8_3d_bnd4_dim
    module procedure realloc_int8_3d_bnd8_dim
    module procedure realloc_real_1d_bnd4_dim
    module procedure realloc_real_1d_bnd8_dim
    module procedure realloc_real_2d_bnd4_dim
    module procedure realloc_real_2d_bnd8_dim
    module procedure realloc_real_3d_bnd4_dim
    module procedure realloc_real_3d_bnd8_dim
    module procedure realloc_dble_1d_bnd4_dim
    module procedure realloc_dble_1d_bnd8_dim
    module procedure realloc_dble_2d_bnd4_dim
    module procedure realloc_dble_2d_bnd8_dim
    module procedure realloc_dble_3d_bnd4_dim
    module procedure realloc_dble_3d_bnd8_dim

    module procedure realloc_char_1d_size4
    module procedure realloc_log1_1d_size4
    module procedure realloc_log1_1d_size8
    module procedure realloc_log1_2d_size4
    module procedure realloc_log1_2d_size8
    module procedure realloc_log1_3d_size4
    module procedure realloc_log1_3d_size8
    module procedure realloc_log4_1d_size4
    module procedure realloc_log4_1d_size8
    module procedure realloc_log4_2d_size4
    module procedure realloc_log4_2d_size8
    module procedure realloc_log4_3d_size4
    module procedure realloc_log4_3d_size8
    module procedure realloc_int1_1d_size4
    module procedure realloc_int1_1d_size8
    module procedure realloc_int1_2d_size4
    module procedure realloc_int1_2d_size8
    module procedure realloc_int1_3d_size4
    module procedure realloc_int1_3d_size8
    module procedure realloc_int2_1d_size4
    module procedure realloc_int2_1d_size8
    module procedure realloc_int2_2d_size4
    module procedure realloc_int2_2d_size8
    module procedure realloc_int2_3d_size4
    module procedure realloc_int2_3d_size8
    module procedure realloc_int4_1d_size4
    module procedure realloc_int4_1d_size8
    module procedure realloc_int4_2d_size4
    module procedure realloc_int4_2d_size8
    module procedure realloc_int4_3d_size4
    module procedure realloc_int4_3d_size8
    module procedure realloc_int8_1d_size4
    module procedure realloc_int8_1d_size8
    module procedure realloc_int8_2d_size4
    module procedure realloc_int8_2d_size8
    module procedure realloc_int8_3d_size4
    module procedure realloc_int8_3d_size8
    module procedure realloc_real_1d_size4
    module procedure realloc_real_1d_size8
    module procedure realloc_real_2d_size4
    module procedure realloc_real_2d_size8
    module procedure realloc_real_3d_size4
    module procedure realloc_real_3d_size8
    module procedure realloc_dble_1d_size4
    module procedure realloc_dble_1d_size8
    module procedure realloc_dble_2d_size4
    module procedure realloc_dble_2d_size8
    module procedure realloc_dble_3d_size4
    module procedure realloc_dble_3d_size8
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_array_realloc'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine realloc_log1_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_log1_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_log1_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_log1_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  logical(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_log1_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_log1_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_log1_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_log1_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_log1_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  logical(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_log1_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_log1_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_log1_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_log1_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_log1_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  logical(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_log1_3d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_log4_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_log4_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_log4_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_log4_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  logical(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_log4_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_log4_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_log4_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_log4_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_log4_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  logical(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_log4_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_log4_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_log4_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_log4_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_log4_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill

  logical       :: clear_
  logical(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  logical(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = .true.
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_log4_3d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int1_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int1_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int1_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int1_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int1_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int1_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int1_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int1_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int1_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8)    :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int1_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int1_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int1_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int1_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int1_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8)    :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int1_3d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int2_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int2_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int2_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int2_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int2_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int2_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int2_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int2_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int2_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int2_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int2_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int2_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int2_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int2_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int2_3d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int4_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int4_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int4_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int4_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int4_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int4_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int4_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int4_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int4_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int4_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int4_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int4_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int4_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int4_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int4_3d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int8_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int8_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int8_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int8_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int8_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int8_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int8_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int8_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int8_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int8_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_int8_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_int8_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_int8_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_int8_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill

  logical       :: clear_
  integer(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  integer(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_int8_3d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_real_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_real_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_real_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_real_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  real(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_real_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_real_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_real_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_real_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_real_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  real(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_real_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_real_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_real_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_real_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_real_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  real(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_real_3d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_dble_1d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_dble_1d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_dble_1d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_dble_1d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  real(byte), allocatable :: tmp(:)
  integer(8) :: i1

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1)))
        do i1 = l_tmp(1), u_tmp(1)
          tmp(i1) = arr(i1)
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i1 = l_tmp(1), u_tmp(1)
      arr(i1) = tmp(i1)
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_dble_1d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_dble_2d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_dble_2d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_dble_2d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_dble_2d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  real(byte), allocatable :: tmp(:,:)
  integer(8) :: i1, i2

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2)))
        do i2 = l_tmp(2), u_tmp(2)
          do i1 = l_tmp(1), u_tmp(1)
            tmp(i1,i2) = arr(i1,i2)
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i2 = l_tmp(2), u_tmp(2)
      do i1 = l_tmp(1), u_tmp(1)
        arr(i1,i2) = tmp(i1,i2)
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_dble_2d_bnd8
!===============================================================
!
!===============================================================
subroutine realloc_dble_3d_bnd4(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  call realloc_dble_3d_bnd8(arr, int(l,8), int(u,8), clear_, fill_)
end subroutine realloc_dble_3d_bnd4
!===============================================================
!
!===============================================================
subroutine realloc_dble_3d_bnd8(arr, l, u, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l(ndim), u(ndim)
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill

  logical    :: clear_
  real(byte) :: fill_

  integer(8) :: l_tmp(ndim), u_tmp(ndim)
  real(byte), allocatable :: tmp(:,:,:)
  integer(8) :: i1, i2, i3

  clear_ = .false.
  fill_ = 0
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill

  if( associated(arr) )then
    if( all(lbound(arr) == l .and. ubound(arr) == u) )then
      if( clear_ ) arr = fill_
      return
    endif

    if( .not. clear_ )then
      l_tmp = max(int(l,8), int(lbound(arr),8))
      u_tmp = min(int(u,8), int(ubound(arr),8))
      if( all(l_tmp <= u_tmp) )then
        allocate(tmp(l_tmp(1):u_tmp(1),l_tmp(2):u_tmp(2),l_tmp(3):u_tmp(3)))
        do i3 = l_tmp(3), u_tmp(3)
          do i2 = l_tmp(2), u_tmp(2)
            do i1 = l_tmp(1), u_tmp(1)
              tmp(i1,i2,i3) = arr(i1,i2,i3)
            enddo
          enddo
        enddo
      endif
    endif

    deallocate(arr)
  endif

  allocate(arr(l(1):u(1),l(2):u(2),l(3):u(3)))

  arr = fill_

  if( .not. clear_ .and. allocated(tmp) )then
    do i3 = l_tmp(3), u_tmp(3)
      do i2 = l_tmp(2), u_tmp(2)
        do i1 = l_tmp(1), u_tmp(1)
          arr(i1,i2,i3) = tmp(i1,i2,i3)
        enddo
      enddo
    enddo
    deallocate(tmp)
  endif
end subroutine realloc_dble_3d_bnd8
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
subroutine realloc_log1_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log1_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log1_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_log1_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log1_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log1_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_log1_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log1_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log1_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_log1_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log1_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log1_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_log1_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log1_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log1_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_log1_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log1_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log1_3d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_log4_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log4_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log4_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_log4_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log4_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log4_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_log4_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log4_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log4_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_log4_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log4_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log4_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_log4_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log4_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log4_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_log4_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  logical(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  logical(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  logical(byte) :: fill_

  clear_ = .false.
  fill_ = .true.
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_log4_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_log4_3d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int1_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int1_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int1_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int1_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int1_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int1_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int1_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int1_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int1_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int1_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int1_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int1_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int1_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int1_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int1_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int1_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int1_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int1_3d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int2_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int2_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int2_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int2_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int2_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int2_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int2_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int2_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int2_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int2_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte), intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int2_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int2_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int2_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int2_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int2_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int2_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int2_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int2_3d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int4_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int4_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int4_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int4_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int4_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int4_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int4_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int4_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int4_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int4_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int4_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int4_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int4_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int4_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int4_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int4_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int4_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int4_3d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int8_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int8_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int8_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int8_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int8_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int8_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int8_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int8_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int8_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int8_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int8_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int8_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_int8_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int8_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int8_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_int8_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  integer(byte)    , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  integer(byte)    , intent(in), optional :: fill
  integer(8)    :: l_(ndim), u_(ndim)
  integer       :: d_
  logical       :: clear_
  integer(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_int8_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_int8_3d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_real_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_real_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_real_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_real_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_real_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_real_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_real_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_real_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_real_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_real_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_real_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_real_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_real_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_real_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_real_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_real_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_real_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_real_3d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_dble_1d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_dble_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_dble_1d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_dble_1d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 1
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_dble_1d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_dble_1d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_dble_2d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_dble_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_dble_2d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_dble_2d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 2
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_dble_2d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_dble_2d_bnd8_dim
!===============================================================
!
!===============================================================
subroutine realloc_dble_3d_bnd4_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 4
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_dble_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_dble_3d_bnd4_dim
!===============================================================
!
!===============================================================
subroutine realloc_dble_3d_bnd8_dim(arr, l, u, d, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndim = 3
  integer, parameter :: byte_bnd = 8
  real(byte)       , pointer              :: arr(:,:,:)
  integer(byte_bnd), intent(in)           :: l, u
  integer          , intent(in), optional :: d
  logical          , intent(in), optional :: clear
  real(byte)       , intent(in), optional :: fill
  integer(8) :: l_(ndim), u_(ndim)
  integer    :: d_
  logical    :: clear_
  real(byte) :: fill_

  clear_ = .false.
  fill_ = 0
  d_ = 1
  if( present(clear) ) clear_ = clear
  if( present(fill) ) fill_ = fill
  if( present(d) ) d_ = d

  l_ = lbound(arr)
  u_ = ubound(arr)
  l_(d_) = l
  u_(d_) = u

  call realloc_dble_3d_bnd8(arr, l_, u_, clear_, fill_)
end subroutine realloc_dble_3d_bnd8_dim
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
subroutine realloc_char_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte_sz = 4
  character(*)    , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  character(*)    , intent(in), optional :: fill

  logical :: clear_
  character(:), allocatable :: fill_
  integer(byte_sz) :: n
  character(len(arr(1))), allocatable :: tmp(:)

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    if( present(clear) ) clear_ = clear

    allocate(character(1) :: fill_)
    fill_ = ''
    if( present(fill) ) fill_ = fill

    if( associated(arr) )then
      n = size(arr)

      if( .not. clear_ )then
        allocate(tmp(n))
        tmp(:) = arr(:)
      endif

      deallocate(arr)
    endif

    allocate(arr(sz))
    arr = fill_

    if( allocated(tmp) )then
      arr(:min(sz,n)) = tmp(:min(sz,n))
      deallocate(tmp)
    endif
  endif
end subroutine realloc_char_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_log1_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 4
  logical(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  logical(byte)   , intent(in), optional :: fill
  logical       :: clear_
  logical(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = .true.
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_log1_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_log1_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_log1_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 8
  logical(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  logical(byte)   , intent(in), optional :: fill
  logical       :: clear_
  logical(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = .true.
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_log1_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_log1_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_log1_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log1_2d_size4'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 4
  logical(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log1_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_log1_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log1_2d_size8'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 8
  logical(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log1_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_log1_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log1_3d_size4'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 4
  logical(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log1_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_log1_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log1_3d_size8'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 8
  logical(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    allocate(arr(0,0,0))
    deallocate(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log1_3d_size8
!===============================================================
!
!===============================================================
subroutine realloc_log4_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  logical(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  logical(byte)   , intent(in), optional :: fill
  logical       :: clear_
  logical(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = .true.
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_log4_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_log4_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_log4_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  logical(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  logical(byte)   , intent(in), optional :: fill
  logical       :: clear_
  logical(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = .true.
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_log4_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_log4_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_log4_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log4_2d_size4'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  logical(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log4_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_log4_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log4_2d_size8'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  logical(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log4_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_log4_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log4_3d_size4'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  logical(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    allocate(arr(0,0,0))
    deallocate(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log4_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_log4_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_log4_3d_size8'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  logical(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    allocate(arr(0,0,0))
    deallocate(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_log4_3d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int1_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int1_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int1_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int1_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int1_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int1_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int1_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int1_2d_size4'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int1_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int1_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int1_2d_size8'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int1_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int1_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int1_3d_size4'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    allocate(arr(0,0,0))
    deallocate(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int1_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int1_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int1_3d_size8'
  integer, parameter :: byte = 1
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    allocate(arr(0,0,0))
    deallocate(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int1_3d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int2_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int2_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int2_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int2_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int2_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int2_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int2_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int2_2d_size4'
  integer, parameter :: byte = 2
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int2_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int2_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int2_2d_size8'
  integer, parameter :: byte = 2
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int2_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int2_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int2_3d_size4'
  integer, parameter :: byte = 2
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int2_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int2_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int2_3d_size8'
  integer, parameter :: byte = 2
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int2_3d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int4_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int4_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int4_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int4_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int4_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int4_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int4_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int4_2d_size4'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int4_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int4_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int4_2d_size8'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int4_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int4_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int4_3d_size4'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int4_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int4_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int4_3d_size8'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int4_3d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int8_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int8_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int8_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int8_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  integer(byte)   , intent(in), optional :: fill
  logical       :: clear_
  integer(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_int8_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_int8_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int8_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int8_2d_size4'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int8_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int8_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int8_2d_size8'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int8_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_int8_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int8_3d_size4'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 4
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int8_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_int8_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_int8_3d_size8'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 8
  integer(byte)   , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_int8_3d_size8
!===============================================================
!
!===============================================================
subroutine realloc_real_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  real(byte)      , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  real(byte)      , intent(in), optional :: fill
  logical    :: clear_
  real(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_real_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_real_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_real_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  real(byte)      , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  real(byte)      , intent(in), optional :: fill
  logical    :: clear_
  real(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_real_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_real_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_real_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_real_2d_size4'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  real(byte)      , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_real_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_real_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_real_2d_size8'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  real(byte)      , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_real_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_real_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_real_3d_size4'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 4
  real(byte)      , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_real_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_real_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_real_3d_size8'
  integer, parameter :: byte = 4
  integer, parameter :: byte_sz = 8
  real(byte)      , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_real_3d_size8
!===============================================================
!
!===============================================================
subroutine realloc_dble_1d_size4(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 4
  real(byte)      , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  real(byte)      , intent(in), optional :: fill
  logical    :: clear_
  real(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_dble_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_dble_1d_size4
!===============================================================
!
!===============================================================
subroutine realloc_dble_1d_size8(arr, sz, clear, fill)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 8
  real(byte)      , pointer              :: arr(:)
  integer(byte_sz), intent(in)           :: sz
  logical         , intent(in), optional :: clear
  real(byte)      , intent(in), optional :: fill
  logical    :: clear_
  real(byte) :: fill_

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    clear_ = .false.
    fill_ = 0
    if( present(clear) ) clear_ = clear
    if( present(fill) ) fill_ = fill

    call realloc_dble_1d_bnd8(arr, int((/1/),8), int((/sz/),8), clear_, fill_)
  endif
end subroutine realloc_dble_1d_size8
!===============================================================
!
!===============================================================
subroutine realloc_dble_2d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_dble_2d_size4'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 4
  real(byte)      , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_dble_2d_size4
!===============================================================
!
!===============================================================
subroutine realloc_dble_2d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_dble_2d_size8'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 8
  real(byte)      , pointer    :: arr(:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_dble_2d_size8
!===============================================================
!
!===============================================================
subroutine realloc_dble_3d_size4(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_dble_3d_size4'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 4
  real(byte)      , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_dble_3d_size4
!===============================================================
!
!===============================================================
subroutine realloc_dble_3d_size8(arr, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'realloc_dble_3d_size8'
  integer, parameter :: byte = 8
  integer, parameter :: byte_sz = 8
  real(byte)      , pointer    :: arr(:,:,:)
  integer(byte_sz), intent(in) :: sz

  if( sz <= 0 )then
    if( associated(arr) ) deallocate(arr)
    nullify(arr)
  else
    call errend(msg_unexpected_condition()//&
              '\nsz > 0', '', PRCNAM, MODNAM)
  endif
end subroutine realloc_dble_3d_size8
!===============================================================
!
!===============================================================
end module lib_array_realloc
