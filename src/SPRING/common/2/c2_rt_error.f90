module c2_rt_error
  use lib_const
  use lib_base
  use lib_log
  use lib_array
  use lib_math
  use lib_io
  use c1_const
  use c1_type_opt
  use c1_type_gs
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: raise_error_coef_negative
  public :: raise_error_coef_small
  public :: raise_error_coef_above_thresh
  public :: raise_error_coef_sum_above_thresh
  public :: raise_error_val_sum_non_positive
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_error'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine raise_error_coef_negative(&
    ij, coef, zero_negative)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'raise_error_coef_negative'
  integer(8), intent(in) :: ij
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: zero_negative

  call erradd(msg_unexpected_condition()//&
            '\ncoef(ij) <= zero_negative'//&
            '\n  ij: '//str(ij)//&
            '\n  coef(ij): '//str(coef)//&
            '\n  zero_negative: '//str(zero_negative), &
              '', PRCNAM, MODNAM)
end subroutine raise_error_coef_negative
!===============================================================
!
!===============================================================
subroutine raise_error_coef_small(&
    ij, coef, zero_positive, zero_negative)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'raise_error_coef_small'
  integer(8), intent(in) :: ij
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: zero_positive, zero_negative

  call erradd(msg_unexpected_condition()//&
            '\ncoef(ij) > zero_negative .and. coef(ij) < zero_positive'//&
            '\n  ij: '//str(ij)//&
            '\n  coef(ij): '//str(coef)//&
            '\n  zero_positive: '//str(zero_positive)//&
            '\n  zero_negative: '//str(zero_negative), &
              '', PRCNAM, MODNAM)
end subroutine raise_error_coef_small
!===============================================================
!
!===============================================================
subroutine raise_error_coef_above_thresh(&
    ij, sidx, tidx, coef, error_excess)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'raise_error_coef_above_thresh'
  integer(8), intent(in) :: ij
  integer(8), intent(in) :: sidx, tidx
  real(8)   , intent(in) :: coef
  real(8)   , intent(in) :: error_excess

  call erradd(msg_unexpected_condition()//&
            '\ncoef(ij) >= 1.0 + error_excess'//&
            '\n  ij: '//str(ij)//&
            '\n  sidx: '//str(sidx)//&
            '\n  tidx: '//str(tidx)//&
            '\n  coef(ij)-1.0: '//str(coef-1.d0)//&
            '\n  error_excess: '//str(error_excess), &
              '', PRCNAM, MODNAM)
end subroutine raise_error_coef_above_thresh
!===============================================================
!
!===============================================================
subroutine raise_error_coef_sum_above_thresh(&
    ijs, ije, coef_sum, sum_error_excess)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'raise_error_coef_sum_above_thresh'
  integer(8), intent(in) :: ijs, ije
  real(8)   , intent(in) :: coef_sum
  real(8)   , intent(in) :: sum_error_excess

  call erradd(msg_unexpected_condition()//&
            '\nsum(coef(ijs:ije)) >= 1.0 + sum_error_excess'//&
            '\n  ijs: '//str(ijs)//&
            '\n  ije: '//str(ije)//&
            '\n  sum(coef(ijs:ije))-1.0: '//str(coef_sum-1.d0)//&
            '\n  sum_error_excess: '//str(sum_error_excess), &
              '', PRCNAM, MODNAM)
end subroutine raise_error_coef_sum_above_thresh
!===============================================================
!
!===============================================================
subroutine raise_error_val_sum_non_positive(&
    sidx, tidx, val, nam, ijs, ije, grid_coef)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'raise_error_val_sum_non_positive'
  integer(8)  , intent(in) :: sidx(:), tidx(:)
  real(8)     , intent(in) :: val(:)
  character(*), intent(in) :: nam
  integer(8)  , intent(in) :: ijs, ije
  character(*), intent(in) :: grid_coef

  integer(8) :: ij

  call erradd(msg_unexpected_condition()//&
            '\n'//str(nam)//'_sum <= 0'//&
            '\n  grid_coef: '//str(grid_coef)//&
            '\n  '//str(nam)//'_sum: '//str(sum(val(ijs:ije)))//&
            '\n  ijs: '//str(ijs)//&
            '\n  ije: '//str(ije), &
              '', PRCNAM, MODNAM)
  do ij = ijs, ije
    call errapd('  ij '//str(ij,dgt(ije))//&
                ' sidx '//str(sidx(ij))//' tidx '//str(tidx(ij))//&
                ' '//str(nam)//' '//str(val(ij)))
  enddo
end subroutine raise_error_val_sum_non_positive
!===============================================================
!
!===============================================================
end module c2_rt_error
