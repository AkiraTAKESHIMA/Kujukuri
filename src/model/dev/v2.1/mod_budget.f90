! RRI_Sub.f90

module mod_budget
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: storage_calc
  !-------------------------------------------------------------
contains
!===============================================================
! storage calculation
!===============================================================
subroutine storage_calc(hs, hr, hg, ss, sr, si, sg)
  use mod_globals
  use mod_section, only: &
    hr2vr
  implicit none

  real(8) hs(ny, nx), hr(ny, nx), hg(ny, nx)
  real(8) ss, sr, si, sg
  real(8) vr_temp ! add v1.4
  integer i, j, k

ss = 0.d0
sr = 0.d0
si = 0.d0
sg = 0.d0
do i = 1, ny
 do j = 1, nx
  if( domain(i,j) .eq. 0 ) cycle
  ss = ss + hs(i,j) * area
  !if(riv_thresh.ge.0 .and. riv(i,j).eq.1) sr = sr + hr(i,j) * area * area_ratio(i,j)
  ! modified v1.4
  if( riv_thresh.ge.0 .and. riv(i,j).eq.1 ) then
   call hr2vr(hr(i, j), riv_ij2idx(i,j), vr_temp)
   sr = sr + vr_temp
  endif
  si = si + gampt_ff(i,j) * area
  sg = sg - hg(i,j) * gammag_idx(slo_ij2idx(i,j)) * area ! storage deficit
 enddo
enddo

end subroutine storage_calc
!===============================================================
!
!===============================================================
end module mod_budget
