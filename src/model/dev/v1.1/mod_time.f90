module mod_time
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_math
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: strftime
  public :: advance_time
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_time'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function strftime(time) result(res)
  implicit none
  integer, intent(in) :: time(:)
  character(:), allocatable :: res

  allocate(character(1) :: res)
  res = str(time(1),-4)//'-'//str(time(2),-2)//'-'//str(time(3),-2)//&
  'T'//str(time(5),-2)//'-'//str(time(6),-2)//'-'//str(time(7),-2)
end function strftime
!===============================================================
!
!===============================================================
subroutine advance_time(time, sec)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'advance_time'
  integer, intent(inout) :: time(:)
  integer, intent(in) :: sec

  integer :: dymax

  time(7) = time(7) + sec

  if( time(7) >= 60 )then  ! sec
    call add(time(6), time(7) / 60)
    time(7) = mod(time(7), 60)
    if( time(6) >= 60 )then  ! min
      call add(time(5), time(6) / 60)
      time(6) = mod(time(6), 60)
      if( time(5) >= 24 )then  ! hr
        call add(time(3), time(5) / 24)
        time(5) = mod(time(5), 24)

        dymax = days(time(1), time(2))
        do while( time(3) > dymax )
          call add(time(3), dymax)
          call add(time(2))
          selectcase( time(2) )  ! month
          case( 1:12 )
            continue
          case( 13 )
            time(2) = 1
          case default
            call errend(msg_invalid_value('time(2)', time(2)), &
              '', PRCNAM, MODNAM)
          endselect
          dymax = days(time(1), time(2))
        enddo
      endif
    endif
  endif
end subroutine advance_time
!===============================================================
!
!===============================================================
end module mod_time
