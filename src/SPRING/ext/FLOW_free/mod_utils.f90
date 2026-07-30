module mod_utils
  use lib_const
  use lib_base
  use lib_log
  use c1_type_opt, only: &
        opt_earth_
  use c1_opt_set, only: &
        set_default_values_opt_earth, &
        set_values_opt_earth
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_conf_earth

  public :: nextxy
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_utils'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_conf_earth(earth, un)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_conf_earth'
  type(opt_earth_), intent(out) :: earth
  integer, intent(in) :: un

  integer :: n_r, n_finv, n_f, n_e2

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_default_values_opt_earth(earth)

  read(un,*) earth%geosys
  earth%geosys = lower(earth%geosys)

  selectcase( earth%geosys )
  case( EARTH_GEOSYS__WGS84, &
        EARTH_GEOSYS__GRS80 )
    read(un,*) earth%rtyp
    n_r    = 0
    n_finv = 0
    n_f    = 0
    n_e2   = 0
  case( EARTH_GEOSYS__OTHER )
    read(un,*) earth%r  ! meter
    read(un,*) earth%finv
    n_r    = 1
    n_finv = 1
    n_f    = 0
    n_e2   = 0
  case default
    call errend(msg_invalid_value('earth%geosys', earth%geosys))
  endselect

  call traperr( set_values_opt_earth(&
         earth, n_r, n_finv, n_f, n_e2) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_conf_earth
!===============================================================
!
!===============================================================
subroutine nextxy(ix, iy, jx, jy, nx, dir)
  implicit none
  integer   , intent(in)  :: ix, iy
  integer   , intent(out) :: jx, jy
  integer   , intent(in)  :: nx
  integer(1), intent(in)  :: dir

  !jx = ix
  !jy = iy

  selectcase( dir )
  case( 2_1, 3_1, 4_1 )
  !if( dir==2 .or. dir==3 .or. dir==4 )then
    jx = ix + 1
    if( jx > nx ) jx = 1
  case( 6_1, 7_1, 8_1 )
  !elseif( dir==6 .or. dir==7 .or. dir==8 )then
    jx = ix - 1
    if( jx == 0 ) jx = nx
  case default
    jx = ix
  endselect

  selectcase( dir )
  case( 1_1, 2_1, 8_1 )
  !if( dir==1 .or. dir==2 .or. dir==8 )then
    jy = iy - 1
  case( 4_1, 5_1, 6_1 )
  !elseif( dir==4 .or. dir==5 .or. dir==6 )then
    jy = iy + 1
  case default
    jy = iy
  endselect
end subroutine nextxy
!===============================================================
!
!===============================================================
end module mod_utils
