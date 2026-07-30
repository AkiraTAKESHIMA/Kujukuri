module ls_manage
  use lib_const
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: spring_initialize
  public :: spring_finalize
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'ls_manage'

  logical :: is_initialized = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function spring_initialize(&
    num_grdsys, num_rmptbl, logopt) result(info)
  use ls_base, only: &
        LOGOPT_DEFAULT    , &
        set_logopt        , &
        assert_initialized
  use ls_gs, only: &
        initialize_gs => initialize
  use ls_rt, only: &
        initialize_rt => initialize
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_initialize'
  integer     , intent(in), optional :: num_grdsys
  integer     , intent(in), optional :: num_rmptbl
  character(*), intent(in), optional :: logopt

  integer :: num_grdsys_
  integer :: num_rmptbl_
  character(:), allocatable :: logopt_

  integer, parameter :: NUM_GRDSYS_DEFAULT = 4
  integer, parameter :: NUM_RMPTBL_DEFAULT = 16

  allocate(character(1) :: logopt_)
  logopt_ = LOGOPT_DEFAULT
  if( present(logopt) ) logopt_ = logopt

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt_)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .false.) /= 0 )then
    info = 1; call errret(); return
  endif
  is_initialized = .true.

  if( set_logopt(logopt_) /= 0 )then
    info = 1; call errret(); return
  endif

  num_grdsys_ = NUM_GRDSYS_DEFAULT
  num_rmptbl_ = NUM_RMPTBL_DEFAULT
  if( present(num_grdsys) ) num_grdsys_ = num_grdsys
  if( present(num_rmptbl) ) num_rmptbl_ = num_rmptbl

  if( initialize_gs(num_grdsys_) /= 0 )then
    info = 1; call errret(); return
  endif
  if( initialize_rt(num_rmptbl_) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(logopt_)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_initialize
!===============================================================
!
!===============================================================
integer(4) function spring_finalize() result(info)
  use ls_base, only: &
        LOGOPT_DEFAULT    , &
        logopt            , &
        set_logopt        , &
        assert_initialized
  use ls_gs, only: &
        finalize_gs => finalize
  use ls_rt, only: &
        finalize_rt => finalize
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'spring_finalize'

  info = 0
  call logbgn(PRCNAM, MODNAM, logopt())
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( assert_initialized(is_initialized, .true.) /= 0 )then
    info = 1; call errret(); return
  endif
  is_initialized = .false.

  if( finalize_gs() /= 0 )then
    info = 1; call errret(); return
  endif
  if( finalize_rt() /= 0 )then
    info = 1; call errret(); return
  endif

  if( set_logopt(LOGOPT_DEFAULT) /= 0 )then
    info = 1; call errret(); return
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function spring_finalize
!===============================================================
!
!===============================================================
end module ls_manage
