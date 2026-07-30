module lib_time_timer
  use lib_const
  use lib_time_base
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: timer_
  public :: timer_elem_

  public :: init_timer
  public :: clear_timer
  public :: start_timer
  public :: stop_timer
  public :: add_time
  public :: get_idx_timer
  !------------------------------------------------------------
  ! Public module variables (types)
  !------------------------------------------------------------
  type timer_elem_
    character(:), allocatable :: name
    integer :: t0(8)
    real(8) :: time
    logical :: is_active
  end type

  type timer_
    integer :: n = 0
    type(timer_elem_), pointer :: elem(:)
    logical :: is_active = .false.
  end type
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'get_idx_timer'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_timer(timer, sz)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'init_timer'
  type(timer_), intent(out) :: timer
  integer, intent(in) :: sz

  type(timer_elem_), pointer :: te
  integer :: i

  if( timer%is_active )then
    write(STDERR, "(a)") '****** ERROR ******'
    write(STDERR, "(a)") 'MOD__'//trim(MODNAM)//'__PROC__'//trim(PRCNAM)
    write(STDERR, "(a)") 'Timer is already used.'
    stop 1
  endif

  timer%n = 0
  timer%is_active = .true.

  allocate(timer%elem(sz))
  do i = 1, sz
    te => timer%elem(i)
    te%time = 0.d0
    allocate(character(1) :: te%name)
    te%name = ''
    te%is_active = .false.
  enddo
  nullify(te)
end subroutine init_timer
!===============================================================
!
!===============================================================
subroutine clear_timer(timer)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'clear_timer'
  type(timer_), intent(inout) :: timer

  if( .not. timer%is_active )then
    write(STDERR, "(a)") '****** ERROR ******'
    write(STDERR, "(a)") 'MOD__'//trim(MODNAM)//'__PROC__'//trim(PRCNAM)
    write(STDERR, "(a)") 'Timer is inactive.'
    stop 1
  endif
  deallocate(timer%elem)
end subroutine clear_timer
!===============================================================
!
!===============================================================
subroutine start_timer(timer, name)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'start_timer'
  type(timer_), intent(inout) :: timer
  character(*), intent(in) :: name

  type(timer_elem_), pointer :: te

  if( .not. timer%is_active ) return

  te => timer%elem(get_idx_timer(timer, name))

  if( te%is_active )then
    write(STDERR, "(a)") '****** ERROR ******'
    write(STDERR, "(a)") 'MOD__'//trim(MODNAM)//'__PROC__'//trim(PRCNAM)
    write(STDERR, "(a)") 'Timer for the process "'//trim(name)//&
        '" has already been started.'
    stop 1
  endif
  te%is_active = .true.

  te%t0 = date_and_time_values()
  nullify(te)
end subroutine start_timer
!===============================================================
!
!===============================================================
subroutine stop_timer(timer, name)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'stop_timer'
  type(timer_), intent(inout) :: timer
  character(*), intent(in) :: name

  type(timer_elem_), pointer :: te

  if( .not. timer%is_active ) return

  te => timer%elem(get_idx_timer(timer, name))

  if( .not. te%is_active )then
    write(STDERR, "(a)") '****** ERROR ******'
    write(STDERR, "(a)") 'MOD__'//trim(MODNAM)//'__PROC__'//trim(PRCNAM)
    write(STDERR, "(a)") 'Timer for the process "'//trim(name)//&
        '" has not been started.'
    stop 1
  endif
  te%is_active = .false.

  te%time = te%time + timediff(te%t0, date_and_time_values())
  nullify(te)
end subroutine stop_timer
!===============================================================
!
!===============================================================
subroutine add_time(timer, name, time)
  implicit none
  type(timer_), intent(inout) :: timer
  character(*), intent(in) :: name
  real(8), intent(in) :: time

  type(timer_elem_), pointer :: te

  if( .not. timer%is_active ) return

  te => timer%elem(get_idx_timer(timer, name))
  te%time = te%time + time
  nullify(te)
end subroutine add_time
!===============================================================
!
!===============================================================
integer function get_idx_timer(timer, name) result(i)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_idx_timer'
  type(timer_), intent(inout) :: timer
  character(*), intent(in) :: name

  integer, save :: i_prev = 1
  type(timer_elem_), pointer :: te

  if( timer%elem(i_prev)%name == name )then
    i = i_prev
  else
    do i = 1, timer%n
      if( timer%elem(i)%name == name ) exit
    enddo
    if( i == timer%n+1 )then
      if( i > size(timer%elem) )then
        write(STDERR, "(a)") '****** ERROR ******'
        write(STDERR, "(a)") 'MOD__'//trim(MODNAM)//'__PROC__'//trim(PRCNAM)
        write(STDERR, "(a)") 'Size of %elem reached the limit.'
        stop 1
      endif
      timer%n = i
      te => timer%elem(i)
      te%name = trim(name)
      nullify(te)
    endif
    i_prev = i
  endif
end function get_idx_timer
!===============================================================
!
!===============================================================
end module lib_time_timer
