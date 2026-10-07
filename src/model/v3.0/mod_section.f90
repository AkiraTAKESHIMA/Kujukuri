module mod_section
  use lib_const
  use lib_base
  use lib_log
  use def_const
  use def_static
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_section
  public :: sec_hq_riv
  public :: hr2vr
  public :: vr2hr
  public :: sec_h2b
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------

  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_section()
  use mod_base, only: &
    int2char
  implicit none

end subroutine set_section
!===============================================================
!
!===============================================================
subroutine sec_hq_riv(h, dh, k, q)
  implicit none

  real(8) h, dh, q, p, n, a
  integer k, id, div_max, i

id = sec_map_idx(k)
div_max = sec_div(id)

if( h .le. sec_hr(id, 1) ) then

 p = sec_peri(id, 1)
 n = sec_ns_river(id, 1)
 a = h * sec_b(id, 1)

elseif( h .gt. sec_hr(id, div_max) ) then

 p = sec_peri(id, div_max)
 n = sec_ns_river(id, div_max)
 a = sec_area(id, div_max) + (h - sec_hr(id, div_max)) * sec_b(id, div_max)

else

 do i = 2, div_max

  if( h .le. sec_hr(id, i) ) then
   p = sec_peri(id, i)
   n = sec_ns_river(id, i)
   a = sec_area(id, i - 1) + (h - sec_hr(id, i - 1)) * sec_b(id, i)
   exit
  endif

 enddo
endif

q = 1.d0 / n  * ( a / p ) ** (2.d0 / 3.d0) * sqrt( abs(dh) ) * a

end subroutine sec_hq_riv
!===============================================================
!
!===============================================================
subroutine hr2vr(hr, k, vr)
  implicit none
  real(8), intent(in) :: hr
  integer, intent(in) :: k
  real(8), intent(out) :: vr

  vr = hr * channel(k)%area
end subroutine hr2vr
!===============================================================
!
!===============================================================
subroutine vr2hr( vr, k, hr )
  implicit none
  real(8), intent(in) :: vr
  integer, intent(in) :: k
  real(8), intent(out) :: hr

  hr = vr / channel(k)%area
end subroutine vr2hr
!===============================================================
!
!===============================================================
subroutine sec_h2b(h, k, b)
  implicit none

  real(8) h, b
  integer k, id, div_max, i

!id = sec_map_idx(k)
!if( id .le. 0 ) then
if( .true. )then

 b = channel(k)%width

else

 div_max = sec_div(id)

 do i = 1, div_max
  if( h .le. sec_hr(id, i) ) then
   b = sec_b(id, i)
   exit
  endif
  b = sec_b(id, div_max)
 enddo

endif

end subroutine sec_h2b
!===============================================================
!
!===============================================================
end module mod_section
