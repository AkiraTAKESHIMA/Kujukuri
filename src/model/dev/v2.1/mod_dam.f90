! RRI_Dam.f90
! RRI_Mod_Dam.f90

module mod_dam
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: dam_read
  public :: dam_prepare
  public :: dam_operation
  public :: gate_operation
  public :: dam_checkstate
  public :: dam_write
  public :: dam_write_cnt
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  public :: dam_num
  public :: dam_name

  public :: dam_kind
  public :: dam_ix, dam_iy
  public :: dam_loc
  public :: dam_state
  public :: damflg

  public :: dam_qin
  public :: dam_vol
  public :: dam_vol_temp
  public :: dam_volmax
  public :: dam_qout
  public :: dam_floodq
  public :: dam_maxfloodq
  public :: dam_rate
  !-------------------------------------------------------------
  ! Module variables
  !-------------------------------------------------------------
  integer :: dam_num                             ! number of dam
  character(256), allocatable :: dam_name(:)     ! dam name

  integer, allocatable :: dam_kind(:)            ! dam id number
  integer, allocatable :: dam_ix(:), dam_iy(:)   ! dam location (x, y)
  integer, allocatable :: dam_loc(:)             ! dam location (k)
  integer, allocatable :: dam_state(:)           ! dam status (full:1, not full:0)
  integer, allocatable :: damflg(:)              ! dam exist at each grid-cell (exist:1, not exist:0)

  real(8), allocatable :: dam_qin(:)             ! dam inflow
  real(8), allocatable :: dam_vol(:)             ! storage volume
  real(8), allocatable :: dam_vol_temp(:)        ! storage volume (temporary)
  real(8), allocatable :: dam_volmax(:)          ! maximum storage
  real(8), allocatable :: dam_qout(:)            ! dam outflow
  real(8), allocatable :: dam_floodq(:)          ! flood discharge
  real(8), allocatable :: dam_maxfloodq(:)       ! max flood discharge (for non-const. cont.)
  real(8), allocatable :: dam_rate(:)            ! discharge increasing rate
  !-------------------------------------------------------------
contains
!===============================================================
! reading dam control file
!===============================================================
subroutine dam_read()
  use mod_globals
  implicit none
    
  integer :: i, ios
    
allocate( damflg(riv_count), dam_qin(riv_count) )
damflg(:) = 0

if( dam_switch.eq.1 ) then

 open(99, file=damfile, status="old" )
 read(99,*) dam_num
 allocate( dam_name(dam_num), dam_kind(dam_num) &
           , dam_ix(dam_num), dam_iy(dam_num) &
           , dam_vol(dam_num), dam_vol_temp(dam_num) &
           , dam_volmax(dam_num), dam_state(dam_num) &
           , dam_qout(dam_num), dam_loc(dam_num) &
           , dam_floodq(dam_num), dam_maxfloodq(dam_num) &
           , dam_rate(dam_num))

 dam_vol(:) = 0.d0
 dam_state(:) = 0
 dam_floodq(:) = 0.d0
 dam_maxfloodq(:) = 0.d0
 dam_rate(:) = 0.d0

 read(99,*,iostat = ios) dam_name(1), dam_iy(1), dam_ix(1), dam_volmax(1), dam_floodq(1), dam_maxfloodq(1), dam_rate(1), dam_vol(1)
 rewind(99)
 read(99,*) dam_num
 !if(ios .gt. 0) then ! old version (not recommended) bug fix on Nov 27, 2021
 if(ios .ne. 0) then ! old version (not recommended)
  do i = 1, dam_num
   read(99,*) dam_name(i), dam_iy(i), dam_ix(i), dam_volmax(i), dam_floodq(i)
   dam_loc(i) = riv_ij2idx( dam_iy(i), dam_ix(i) )
   damflg(dam_loc(i)) = i
  enddo
 else ! new version
  do i = 1, dam_num
   read(99,*) dam_name(i), dam_iy(i), dam_ix(i), dam_volmax(i), dam_floodq(i), dam_maxfloodq(i), dam_rate(i), dam_vol(i)
   dam_loc(i) = riv_ij2idx( dam_iy(i), dam_ix(i) )
   damflg(dam_loc(i)) = i
  enddo
 end if
 close(99)
end if
end subroutine dam_read
!===============================================================
! calculating inflow to dam
!===============================================================
subroutine dam_prepare(qr_idx)
  use mod_globals
  implicit none

  integer :: i, k, kk
  real(8) :: qr_idx(riv_count), vr_idx(riv_count)

!dam_qin(:) = 0.d0
!do k = 1, riv_count
! kk = down_riv_idx(k)
! dam_qin(kk) = dam_qin(kk) + qr_idx(k)
!enddo

! modified by TS on June 16, 2016
dam_qin(:) = qr_idx(:)

end subroutine dam_prepare
!===============================================================
!
!===============================================================
subroutine dam_operation(k)
  use mod_globals, only :ddt
  implicit none

  integer :: k
  real(8) :: qdiff
    
dam_qout(damflg(k)) = 0.d0

if ( dam_maxfloodq(damflg(k)) .eq. 0 .and. dam_rate(damflg(k)) .eq. 0 ) then
 ! constant flood peak cut
 if ( dam_qin(k) .lt. dam_floodq(damflg(k)) ) then
  if( dam_vol(damflg(k)) .le. 0 ) then ! ’Ç‰Á
   dam_qout(damflg(k)) = dam_qin(k)
  else ! ’Ç‰Á
   if( dam_qin(k) .lt. 0.25 * dam_floodq(damflg(k)) ) then
    dam_qout(damflg(k)) = 0.25 * dam_floodq(damflg(k)) ! ’Ç‰Á
    qdiff = (dam_qin(k) - dam_qout(damflg(k))) ! ’Ç‰Á
    dam_vol_temp(damflg(k)) = dam_vol_temp(damflg(k)) + qdiff * ddt ! ’Ç‰Á
   else
    dam_qout(damflg(k)) = dam_qin(k)
   endif
  endif
 else
  if (dam_state(damflg(k)) .eq. 0) then
  ! still have space
   dam_qout(damflg(k)) = dam_floodq(damflg(k))
   qdiff = (dam_qin(k) - dam_qout(damflg(k)))
   dam_vol_temp(damflg(k)) = dam_vol_temp(damflg(k)) + qdiff * ddt
  else
   ! no more space
   dam_qout(damflg(k)) = dam_qin(k)
  end if
 end if

else
 ! non-constant flood peak cut
 if ( dam_qin(k) .lt. dam_floodq(damflg(k)) ) then
  if( dam_vol(damflg(k)) .le. 0 ) then ! ’Ç‰Á
   dam_qout(damflg(k)) = dam_qin(k)
  else ! ’Ç‰Á
   if( dam_qin(k) .lt. 0.25 * dam_floodq(damflg(k)) ) then
    dam_qout(damflg(k)) = 0.25 * dam_floodq(damflg(k)) ! ’Ç‰Á
    qdiff = (dam_qin(k) - dam_qout(damflg(k))) ! ’Ç‰Á
    dam_vol_temp(damflg(k)) = dam_vol_temp(damflg(k)) + qdiff * ddt ! ’Ç‰Á
   else
    dam_qout(damflg(k)) = dam_qin(k)
   endif
  endif
 else
  if (dam_state(damflg(k)) .eq. 0) then
  ! still have space
   dam_qout(damflg(k)) = dam_floodq(damflg(k)) + &
      dam_rate(damflg(k)) * (dam_qin(k) - dam_floodq(damflg(k)))
   if( dam_qout(damflg(k)) .gt. dam_maxfloodq(damflg(k)) ) &
      dam_qout(damflg(k))  = dam_maxfloodq(damflg(k))
   qdiff = (dam_qin(k) - dam_qout(damflg(k)))
   dam_vol_temp(damflg(k)) = dam_vol_temp(damflg(k)) + qdiff * ddt
  else
   ! no more space
   dam_qout(damflg(k)) = dam_qin(k)
  end if
 end if
end if

end subroutine dam_operation
!===============================================================
!
!===============================================================
subroutine gate_operation(k, hr_k, hr_kk)
  use mod_globals, only: &
    down_riv_idx, zb_riv_idx, depth, riv_idx2i, riv_idx2j
  implicit none

  integer :: k, kk, i, j
  real(8) :: hr_k, hr_kk

dam_qout(damflg(k)) = 0.d0
kk = down_riv_idx(k)
!i = riv_idx2i(k)
!j = riv_idx2j(k)
dam_qout(damflg(k)) = dam_qin(k)
if( dam_qout(damflg(k)) .ge. dam_floodq(damflg(k)) ) &
 dam_qout(damflg(k)) = dam_floodq(damflg(k))
! zb_riv = zs - depth
! dam_floodq -> gate_close_level
!if ( hr_kk + zb_riv_idx(kk) - zb_riv_idx(k) .gt. dam_floodq(damflg(k)) ) then
! dam_qout(damflg(k)) = 0.d0 ! gate close
 ! dam_floodq -> pump_operate_level
!if( (hr_k - depth(i, j)) .gt. dam_maxfloodq(damflg(k)) ) then
! dam_qout(damflg(k)) = dam_rate(damflg(k))
! if( dam_qout(damflg(k)) .ge. dam_qin(k) ) dam_qout(damflg(k)) = dam_qin(k)
!endif
!else
! dam_qout(damflg(k)) = dam_qin(k)
!endif

end subroutine gate_operation
!===============================================================
!
!===============================================================
subroutine dam_checkstate(qr_ave_idx)
  use mod_globals, only: &
    riv_count, area
  implicit none
  real(8) :: qr_ave_idx(riv_count)
  integer :: i
    
do i=1, dam_num
 dam_vol_temp(i) = dam_vol_temp(i) / 6.d0
 dam_vol(i) = dam_vol(i) + dam_vol_temp(i)
 dam_state(i) = 0 ! added on January 8, 2021
 if (dam_vol(i) .gt. dam_volmax(i)) dam_state(i) = 1
end do
    
end subroutine dam_checkstate
!===============================================================
!
!===============================================================
subroutine dam_write()
  implicit none
  integer i

write(1001,'(10000f20.5)') (dam_vol(i), dam_qin(dam_loc(i)), dam_qout(i), i = 1, dam_num) ! v1.4

end subroutine dam_write
!===============================================================
!
!===============================================================
subroutine dam_write_cnt()
  implicit none
  integer i

write(1002,'(i10)') dam_num
do i = 1, dam_num
 write(1002,'(a10, 2i8, 10000f20.5)') dam_name(i), dam_iy(i), dam_ix(i), dam_volmax(i), dam_floodq(i), dam_maxfloodq(i), dam_rate(i), dam_vol(i)
enddo

end subroutine dam_write_cnt
!===============================================================
!
!===============================================================
end module mod_dam
