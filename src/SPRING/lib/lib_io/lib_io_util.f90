module lib_io_util
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: fchar

  public :: read_char
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface read_char
    module procedure read_char_int1
    module procedure read_char_int2
    module procedure read_char_int4
    module procedure read_char_int8
    module procedure read_char_real
    module procedure read_char_dble
    module procedure read_char_log1
    module procedure read_char_log2
    module procedure read_char_log4
    module procedure read_char_log8
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function fchar(c) result(res)
  implicit none
  character(*), intent(in) :: c

  read(c,*) res
end function fchar
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
subroutine read_char_int1(c, i)
  implicit none
  character(*), intent(in)  :: c
  integer(1)  , intent(out) :: i

  read(c,*) i
end subroutine read_char_int1
!===============================================================
!
!===============================================================
subroutine read_char_int2(c, i)
  implicit none
  character(*), intent(in)  :: c
  integer(2)  , intent(out) :: i

  read(c,*) i
end subroutine read_char_int2
!===============================================================
!
!===============================================================
subroutine read_char_int4(c, i)
  implicit none
  character(*), intent(in)  :: c
  integer(4)  , intent(out) :: i

  read(c,*) i
end subroutine read_char_int4
!===============================================================
!
!===============================================================
subroutine read_char_int8(c, i)
  implicit none
  character(*), intent(in)  :: c
  integer(8)  , intent(out) :: i

  read(c,*) i
end subroutine read_char_int8
!===============================================================
!
!===============================================================
subroutine read_char_real(c, r)
  implicit none
  character(*), intent(in)  :: c
  real(4)     , intent(out) :: r

  read(c,*) r
end subroutine read_char_real
!===============================================================
!
!===============================================================
subroutine read_char_dble(c, r)
  implicit none
  character(*), intent(in)  :: c
  real(8)     , intent(out) :: r

  read(c,*) r
end subroutine read_char_dble
!===============================================================
!
!===============================================================
subroutine read_char_log1(c, l)
  implicit none
  character(*), intent(in)  :: c
  logical(1)  , intent(out) :: l

  read(c,*) l
end subroutine read_char_log1
!===============================================================
!
!===============================================================
subroutine read_char_log2(c, l)
  implicit none
  character(*), intent(in)  :: c
  logical(2)  , intent(out) :: l

  read(c,*) l
end subroutine read_char_log2
!===============================================================
!
!===============================================================
subroutine read_char_log4(c, l)
  implicit none
  character(*), intent(in)  :: c
  logical(4)  , intent(out) :: l

  read(c,*) l
end subroutine read_char_log4
!===============================================================
!
!===============================================================
subroutine read_char_log8(c, l)
  implicit none
  character(*), intent(in)  :: c
  logical(8)  , intent(out) :: l

  read(c,*) l
end subroutine read_char_log8
!===============================================================
!
!===============================================================
end module lib_io_util
