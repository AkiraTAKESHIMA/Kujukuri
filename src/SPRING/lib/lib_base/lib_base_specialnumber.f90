module lib_base_specialnumber
  use lib_const
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: is_nan
  public :: is_nan_arr
  public :: is_inf
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface is_nan
    module procedure is_nan__real_0d
    module procedure is_nan__real_1d
    module procedure is_nan__real_2d
    module procedure is_nan__dble_0d
    module procedure is_nan__dble_1d
    module procedure is_nan__dble_2d
  end interface

  interface is_nan_arr
    module procedure is_nan_arr__real_1d
    module procedure is_nan_arr__real_2d
    module procedure is_nan_arr__dble_1d
    module procedure is_nan_arr__dble_2d
  end interface

  interface is_inf
    module procedure is_inf4
    module procedure is_inf8
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_base_specialnumber'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
logical function is_nan__real_0d(x) result(res)
  implicit none
  real(4), intent(in) :: x

  integer(4) :: n
  integer :: i

  n = transfer(x, 0_4)

  ! Exponent part
  res = .true.
  do i = 23, 30
    if( .not. btest(n,i) )then
      res = .false.
      return
    endif
  enddo

  ! Fraction part
  res = .false.
  do i = 0, 22
    if( btest(n,i) )then
      res = .true.
      return
    endif
  enddo
end function is_nan__real_0d
!===============================================================
!
!===============================================================
logical function is_nan__real_1d(x, opt) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_nan__real_1d'
  real(4)     , intent(in) :: x(:)
  character(*), intent(in) :: opt

  integer(8) :: i1

  selectcase( opt )
  case( 'all' )
    res = .true.
    do i1 = 1_8, size(x,1)
      if( .not. is_nan__real_0d(x(i1)) )then
        res = .false.
        return
      endif
    enddo
  case( 'any' )
    res = .false.
    do i1 = 1_8, size(x,1)
      if( is_nan__real_0d(x(i1)) )then
        res = .true.
        return
      endif
    enddo
  case default
    call errend(msg_invalid_value('opt', opt), &
                '', PRCNAM, MODNAM)
  endselect
end function is_nan__real_1d
!===============================================================
!
!===============================================================
logical function is_nan__real_2d(x, opt) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_nan__real_2d'
  real(4)     , intent(in) :: x(:,:)
  character(*), intent(in) :: opt

  integer(8) :: i1, i2

  selectcase( opt )
  case( 'all' )
    res = .true.
    do i1 = 1_8, size(x,1)
    do i2 = 1_8, size(x,2)
      if( .not. is_nan__real_0d(x(i1,i2)) )then
        res = .false.
        return
      endif
    enddo
    enddo
  case( 'any' )
    res = .false.
    do i1 = 1_8, size(x,1)
    do i2 = 1_8, size(x,2)
      if( is_nan__real_0d(x(i1,i2)) )then
        res = .true.
        return
      endif
    enddo
    enddo
  case default
    call errend(msg_invalid_value('opt', opt), &
                '', PRCNAM, MODNAM)
  endselect
end function is_nan__real_1d
!===============================================================
!
!===============================================================
logical function is_nan__dble_0d(x) result(res)
  implicit none
  real(8), intent(in) :: x

  integer(8) :: n
  integer :: i

  n = transfer(x, 0_8)

  ! Exponent part
  res = .true.
  do i = 52, 62
    if( .not. btest(n,i) )then
      res = .false.
      return
    endif
  enddo

  ! Fraction part
  res = .false.
  do i = 0, 51
    if( btest(n,i) )then
      res = .true.
      return
    endif
  enddo
end function is_nan__dble_0d
!===============================================================
!
!===============================================================
logical function is_nan__dble_1d(x, opt) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_nan__dble_1d'
  real(8)     , intent(in) :: x(:)
  character(*), intent(in) :: opt

  integer(8) :: i1

  selectcase( opt )
  case( 'all' )
    res = .true.
    do i1 = 1_8, size(x,1)
      if( .not. is_nan__dble_0d(x(i1)) )then
        res = .false.
        return
      endif
    enddo
  case( 'any' )
    res = .false.
    do i1 = 1_8, size(x,1)
      if( is_nan__dble_0d(x(i1)) )then
        res = .true.
        return
      endif
    enddo
  case default
    call errend(msg_invalid_value('opt', opt), &
                '', PRCNAM, MODNAM)
  endselect
end function is_nan__dble_1d
!===============================================================
!
!===============================================================
logical function is_nan__dble_2d(x, opt) result(res)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'is_nan__dble_2d'
  real(8)     , intent(in) :: x(:,:)
  character(*), intent(in) :: opt

  integer(8) :: i1, i2

  selectcase( opt )
  case( 'all' )
    res = .true.
    do i1 = 1_8, size(x,1)
    do i2 = 1_8, size(x,2)
      if( .not. is_nan__real_0d(x(i1,i2)) )then
        res = .false.
        return
      endif
    enddo
    enddo
  case( 'any' )
    res = .false.
    do i1 = 1_8, size(x,1)
    do i2 = 1_8, size(x,2)
      if( is_nan__real_0d(x(i1,i2)) )then
        res = .true.
        return
      endif
    enddo
    enddo
  case default
    call errend(msg_invalid_value('opt', opt), &
                '', PRCNAM, MODNAM)
  endselect
end function is_nan__dble_2d
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
function is_nan_arr__real_1d(x) result(res)
  implicit none
  real(4), intent(in) :: x(:)
  logical             :: res(size(x))

  integer(8) :: i

  do i = 1_8, size(x)
    res(i) = is_nan(x(i))
  enddo
end function is_nan_arr__real_1d
!===============================================================
!
!===============================================================
function is_nan_arr__real_2d(x) result(res)
  implicit none
  real(4), intent(in) :: x(:,:)
  logical             :: res(size(x,1),size(x,2))
  integer(8) :: i, j

  do j = 1_8, size(x,2)
    do i = 1_8, size(x,1)
      res(i,j) = is_nan(x(i,j))
    enddo
  enddo
end function is_nan_arr__real_2d
!===============================================================
!
!===============================================================
function is_nan_arr__dble_1d(x) result(res)
  implicit none
  real(8), intent(in) :: x(:)
  logical             :: res(size(x))
  integer(8) :: i

  do i = 1_8, size(x)
    res(i) = is_nan(x(i))
  enddo
end function is_nan_arr__dble_1d
!===============================================================
!
!===============================================================
function is_nan_arr__dble_2d(x) result(res)
  implicit none
  real(8), intent(in) :: x(:,:)
  logical             :: res(size(x,1),size(x,2))
  integer(8) :: i, j

  do j = 1_8, size(x,2)
    do i = 1_8, size(x,1)
      res(i,j) = is_nan(x(i,j))
    enddo
  enddo
end function is_nan_arr__dble_2d
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
logical function is_inf4(x) result(res)
  implicit none
  real(4), intent(in) :: x

  integer(4) :: n
  integer :: i

  n = transfer(x, 0_4)

  ! Exponent part
  res = .true.
  do i = 23, 30
    if( .not. btest(n,i) )then
      res = .false.
      return
    endif
  enddo

  ! Fraction part
  res = .true.
  do i = 0, 22
    if( btest(n,i) )then
      res = .false.
      return
    endif
  enddo
end function is_inf4
!===============================================================
!
!===============================================================
logical function is_inf8(x) result(res)
  implicit none
  real(8), intent(in) :: x

  integer(8) :: n
  integer :: i

  n = transfer(x, 0_8)

  ! Exponent part
  res = .true.
  do i = 52, 62
    if( .not. btest(n,i) )then
      res = .false.
      return
    endif
  enddo

  ! Fraction part
  res = .true.
  do i = 0, 51
    if( btest(n,i) )then
      res = .false.
      return
    endif
  enddo
end function is_inf8
!===============================================================
!
!===============================================================
end module lib_base_specialnumber
