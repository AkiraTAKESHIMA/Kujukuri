module lib_log_proc
  use lib_const
  use lib_base
  use lib_time , only: &
    date_and_time_values, &
    timediff
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: logbgn
  public :: logret
  public :: logent
  public :: logext
  public :: logmsg
  public :: logwrn
  public :: logerr
  public :: errend
  public :: errret
  public :: errnew
  public :: erradd
  public :: setlog
  public :: traperr
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_log_proc'

  type currentCmd_
    logical :: tf     ! Command [true|false]
    integer :: depth  ! Where this command was given
  end type

  type cmdHist_
    integer :: nCmd               ! The number of recursive commands
    integer, pointer :: depth(:)  ! Depth where each recursive command was given
    logical, pointer :: tf(:)  ! Command [true|false]
  end type

  type cmdDict_
    type(currentCmd_) :: currentCmd
    type(cmdHist_)    :: cmdHist
  end type

  type cmd_
    type(cmdDict_) :: echoPrc  ! Process (self)
    type(cmdDict_) :: echoPcc  ! Process (child)
    type(cmdDict_) :: echoMsg  ! Message
    type(cmdDict_) :: echoWrn  ! Warning
    type(cmdDict_) :: echoErr
    type(cmdDict_) :: echoBar
    type(cmdDict_) :: stopErr
    type(cmdDict_) :: msrTime
  end type

  type set_
    logical, pointer :: echoPrc(:), &!(0:DEPTH_MAX)
                        echoPcc(:), &
                        echoMsg(:), &
                        echoWrn(:), &
                        echoErr(:), &
                        echoBar(:), &
                        stopErr(:), &
                        msrTime(:)
    integer, pointer :: indent(:)
    integer, pointer :: indentInc(:)
    integer, pointer :: time_bgn(:,:)  !(0:DEPTH_MAX,8)
    character(CLEN_PROC), pointer :: mod(:)
    character(CLEN_PROC), pointer :: prc(:)
    character(CLEN_LINE), pointer :: stp(:)
    logical             , pointer :: is_prc(:)
  end type

  type(cmd_) :: cmd
  type(set_) :: set

  integer :: depth

  logical :: makeNewLine_prev

  integer, parameter :: LTRUE = 0
  integer, parameter :: LFALS = 1
  integer, parameter :: LMISS = -1

  integer, parameter :: ECHOPRC_DEF = LTRUE
  integer, parameter :: ECHOPCC_DEF = LTRUE
  integer, parameter :: ECHOMSG_DEF = LTRUE
  integer, parameter :: ECHOWRN_DEF = LTRUE
  integer, parameter :: ECHOERR_DEF = LTRUE
  integer, parameter :: ECHOBAR_DEF = LTRUE
  integer, parameter :: STOPERR_DEF = LTRUE
  integer, parameter :: MSRTIME_DEF = LFALS

  integer, parameter :: INDENTINC_PRC = 2
  integer, parameter :: INDENT_MISS = -9999

  integer, parameter :: DEPTH_MAX = 20
  integer, parameter :: CLEN_OPT = 32

  integer, parameter :: STOP_CODE_ERROR = 1

  type err_
    integer :: code
    character(:), allocatable :: mod, prc, stp
    character(:), allocatable :: msg
  end type

  type(err_), allocatable, target :: err(:)
  integer :: nmax_err
  integer :: n_err
  integer, parameter :: NMAX_ERR_INIT = DEPTH_MAX

  logical :: is_initialized = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine initialize()
  implicit none

  if( .not. is_initialized )then
    is_initialized = .true.

    call init_cmd(cmd%echoPrc, set%echoPrc, ECHOPRC_DEF)
    call init_cmd(cmd%echoPcc, set%echoPcc, ECHOPCC_DEF)
    call init_cmd(cmd%echoMsg, set%echoMsg, ECHOMSG_DEF)
    call init_cmd(cmd%echoWrn, set%echoWrn, ECHOWRN_DEF)
    call init_cmd(cmd%echoErr, set%echoErr, ECHOERR_DEF)
    call init_cmd(cmd%echoBar, set%echoBar, ECHOBAR_DEF)
    call init_cmd(cmd%stopErr, set%stopErr, STOPERR_DEF)
    call init_cmd(cmd%msrTime, set%msrTime, MSRTIME_DEF)

    allocate(set%indent(0:DEPTH_MAX))
    allocate(set%indentInc(1:DEPTH_MAX))
    set%indent(0) = 1

    allocate(set%time_bgn(0:DEPTH_MAX,8))

    allocate(set%mod(0:DEPTH_MAX))
    allocate(set%prc(0:DEPTH_MAX))
    allocate(set%stp(0:DEPTH_MAX))
    allocate(set%is_prc(0:DEPTH_MAX))

    allocate(err(NMAX_ERR_INIT))
    nmax_err = NMAX_ERR_INIT
    n_err = 0

    depth = 0

    makeNewLine_prev = .true.
  endif
end subroutine initialize
!===============================================================
!
!===============================================================
subroutine logbgn(prc, mod, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'logbgn'
  character(*), intent(in) :: prc
  character(*), intent(in) :: mod
  character(*), intent(in), optional :: opt

  integer :: val_echoPrc
  integer :: val_echoPcc
  integer :: val_echoMsg
  integer :: val_echoWrn
  integer :: val_echoErr
  integer :: val_echoBar
  integer :: val_stopErr
  integer :: val_msrTime

  logical :: isRec_echoPrc
  logical :: isRec_echoPcc
  logical :: isRec_echoMsg
  logical :: isRec_echoWrn
  logical :: isRec_echoErr
  logical :: isRec_echoBar
  logical :: isRec_stopErr
  logical :: isRec_msrTime

  logical :: forceEcho

  integer :: indent
  integer :: indentInc

  character(CLEN_OPT) :: opt_, opt1
  integer :: ios
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call initialize()
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  val_echoPrc = LMISS
  val_echoPcc = LMISS
  val_echoMsg = LMISS
  val_echoWrn = LMISS
  val_echoErr = LMISS
  val_echoBar = LMISS
  val_stopErr = LMISS
  val_msrTime = LMISS

  isRec_echoPrc = .false.
  isRec_echoPcc = .false.
  isRec_echoMsg = .false.
  isRec_echoWrn = .false.
  isRec_echoErr = .false.
  isRec_echoBar = .false.
  isRec_stopErr = .false.
  isRec_msrTime = .false.

  forceEcho   = .false.

  indent = INDENT_MISS
  indentInc = 0

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
    opt_ = adjustl(opt_(len_trim(opt1)+1:))

    selectcase( opt1 )
    case( '+p' )
      val_echoPrc = LTRUE
    case( '-p' )
      val_echoPrc = LFALS
    case( '+pr' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '-pr' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '+pc' )
      val_echoPcc = LTRUE
    case( '-pc' )
      val_echoPcc = LFALS
    case( '+pcr' )
      val_echoPcc = LTRUE
      isRec_echoPcc = .true.
    case( '-pcr' )
      val_echoPcc = LFALS
      isRec_echoPcc = .true.
    case( '+c' )
      val_echoMsg = LTRUE
    case( '-c' )
      val_echoMsg = LFALS
    case( '+cr' )
      val_echoMsg = LTRUE
      isRec_echoMsg = .true.
    case( '-cr' )
      val_echoMsg = LFALS
      isRec_echoMsg = .true.
    case( '+w' )
      val_echoWrn = LTRUE
    case( '-w' )
      val_echoWrn = LFALS
    case( '+wr' )
      val_echoWrn = LTRUE
      isRec_echoWrn = .true.
    case( '-wr' )
      val_echoWrn = LFALS
      isRec_echoWrn = .true.
    case( '+a' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      val_echoMsg = LTRUE
      val_echoWrn = LTRUE
    case( '-a' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      val_echoMsg = LFALS
      val_echoWrn = LFALS
    case( '+ar' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      val_echoMsg = LTRUE
      val_echoWrn = LTRUE
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
      isRec_echoMsg = .true.
      isRec_echoWrn = .true.
    case( '-ar' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      val_echoMsg = LFALS
      val_echoWrn = LFALS
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
      isRec_echoMsg = .true.
      isRec_echoWrn = .true.
    case( '+e' )
      val_echoErr = LTRUE
    case( '-e' )
      val_echoErr = LFALS
    case( '+er' )
      val_echoErr = LTRUE
      isRec_echoErr = .true.
    case( '-er' )
      val_echoErr = LFALS
      isRec_echoErr = .true.
    case( '+b' )
      val_echoBar = LTRUE
    case( '-b' )
      val_echoBar = LFALS
    case( '+br' )
      val_echoBar = LTRUE
      isRec_echoBar = .true.
    case( '-br' )
      val_echoBar = LFALS
      isRec_echoBar = .true.
    case( '+q' )
      val_stopErr = LTRUE
    case( '-q' )
      val_stopErr = LFALS
    case( '+qr' )
      val_stopErr = LTRUE
      isRec_stopErr = .true.
    case( '-qr' )
      val_stopErr = LFALS
      isRec_stopErr = .true.
    case( '+t' )
      val_msrTime = LTRUE
    case( '-t' )
      val_msrTime = LFALS
    case( '+tr' )
      val_msrTime = LTRUE
      isRec_msrTime = .true.
    case( '-tr' )
      val_msrTime = LFALS
      isRec_msrTime = .true.
    case( 'f' )
      forceEcho = .true.
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentInc
        indentInc = -indentInc
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call errend(ERROR_CODE__SYNTAX, &
          'Invalid format of option: '//trim(opt), &
          MODNAM, PRCNAM)
      endif
    endselect
  enddo
  !-------------------------------------------------------------
  ! Update the status and commands
  !-------------------------------------------------------------
  depth = depth + 1

  call fwrd_dict(cmd%echoPrc, set%echoPrc, depth, val_echoPrc, isRec_echoPrc)
  call fwrd_dict(cmd%echoPcc, set%echoPcc, depth, val_echoPcc, isRec_echoPcc)
  call fwrd_dict(cmd%echoMsg, set%echoMsg, depth, val_echoMsg, isRec_echoMsg)
  call fwrd_dict(cmd%echoWrn, set%echoWrn, depth, val_echoWrn, isRec_echoWrn)
  call fwrd_dict(cmd%echoErr, set%echoErr, depth, val_echoErr, isRec_echoErr)
  call fwrd_dict(cmd%echoBar, set%echoBar, depth, val_echoBar, isRec_echoBar)
  call fwrd_dict(cmd%stopErr, set%stopErr, depth, val_stopErr, isRec_stopErr)
  call fwrd_dict(cmd%msrTime, set%msrTime, depth, val_msrTime, isRec_msrTime)

  set%mod(depth) = trim(mod)
  set%prc(depth) = trim(prc)
  set%stp(depth) = ''

  set%indent(depth) = set%indent(depth-1) + indentInc
  set%indentInc(depth) = indentInc

  set%echoPrc(depth) = get_tf(val_echoPrc, set%echoPcc(depth-1))
  !-------------------------------------------------------------
  ! Print the message
  !-------------------------------------------------------------
  if( set%echoPrc(depth) )then
    call echo_lines(STDOUT, '[+ '//strprc(mod, prc)//']', &
                    set%indent(depth), .true.)
  endif
  !-------------------------------------------------------------
  ! Start the timer
  !-------------------------------------------------------------
  if( set%msrTime(depth) )then
    set%time_bgn(depth,:) = date_and_time_values()
  endif
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------
  set%indent(depth) = set%indent(depth) + INDENTINC_PRC
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  makeNewLine_prev = .true.
  !-------------------------------------------------------------
end subroutine logbgn
!===============================================================
!
!===============================================================
subroutine logret(prc, mod)
  implicit none
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod

  character(:), allocatable :: prc_, mod_
  character(:), allocatable :: c_, c
  !-------------------------------------------------------------
  ! Check consistencies
  !-------------------------------------------------------------
  if( present(prc) ) call check_prc(prc)
  if( present(mod) ) call check_mod(mod)
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------

  ! Skip the steps
  do while( set%stp(depth) /= '' )
    depth = depth - 1

    call back_dict(cmd%echoPrc, set%echoPrc, depth)
    call back_dict(cmd%echoPcc, set%echoPcc, depth)
    call back_dict(cmd%echoMsg, set%echoMsg, depth)
    call back_dict(cmd%echoWrn, set%echoWrn, depth)
    call back_dict(cmd%echoErr, set%echoErr, depth)
    call back_dict(cmd%echoBar, set%echoBar, depth)
    call back_dict(cmd%stopErr, set%stopErr, depth)
    call back_dict(cmd%msrTime, set%msrTime, depth)
  enddo

  set%indent(depth) = set%indent(depth) - set%indentInc(depth) - INDENTINC_PRC
  !-------------------------------------------------------------
  ! Print the message
  !-------------------------------------------------------------
  if( set%echoPrc(depth) )then
    allocate(character(1) :: mod_, prc_)
    mod_ = set%mod(depth)
    prc_ = set%prc(depth)
    if( present(mod) ) mod_ = trim(mod)
    if( present(prc) ) prc_ = trim(prc)

    if( set%msrTime(depth) )then
      allocate(character(8) :: c_)
      write(c_,"(f8.3)") timediff(set%time_bgn(depth,:), date_and_time_values())
      allocate(character(1) :: c)
      c = '[- '//strprc(mod_, prc_)//' ('//trim(c_)//' sec)]'
    else
      allocate(character(1) :: c)
      c = '[- '//strprc(mod_, prc_)//']'
    endif
    call echo_lines(STDOUT, c, set%indent(depth), .true.)

    deallocate(mod_, prc_)
  endif
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  depth = depth - 1

  call back_dict(cmd%echoPrc, set%echoPrc, depth)
  call back_dict(cmd%echoPcc, set%echoPcc, depth)
  call back_dict(cmd%echoMsg, set%echoMsg, depth)
  call back_dict(cmd%echoWrn, set%echoWrn, depth)
  call back_dict(cmd%echoErr, set%echoErr, depth)
  call back_dict(cmd%echoBar, set%echoBar, depth)
  call back_dict(cmd%stopErr, set%stopErr, depth)
  call back_dict(cmd%msrTime, set%msrTime, depth)

  makeNewLine_prev = .true.
  !-------------------------------------------------------------
end subroutine logret
!===============================================================
!
!===============================================================
subroutine logent(stp, prc, mod, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'logent'
  character(*), intent(in) :: stp
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: opt

  integer :: val_echoPrc
  integer :: val_echoPcc
  integer :: val_echoMsg
  integer :: val_echoWrn
  integer :: val_echoErr
  integer :: val_echoBar
  integer :: val_stopErr
  integer :: val_msrTime

  logical :: isRec_echoPrc
  logical :: isRec_echoPcc
  logical :: isRec_echoMsg
  logical :: isRec_echoWrn
  logical :: isRec_echoErr
  logical :: isRec_echoBar
  logical :: isRec_stopErr
  logical :: isRec_msrTime

  logical :: forceEcho

  integer :: indent
  integer :: indentInc

  character(CLEN_OPT) :: opt_, opt1
  integer :: ios
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call initialize()
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  val_echoPrc = LMISS
  val_echoPcc = LMISS
  val_echoMsg = LMISS
  val_echoWrn = LMISS
  val_echoErr = LMISS
  val_echoBar = LMISS
  val_stopErr = LMISS
  val_msrTime = LMISS

  isRec_echoPrc = .false.
  isRec_echoPcc = .false.
  isRec_echoMsg = .false.
  isRec_echoWrn = .false.
  isRec_echoErr = .false.
  isRec_echoBar = .false.
  isRec_stopErr = .false.
  isRec_msrTime = .false.

  forceEcho   = .false.

  indent = INDENT_MISS
  indentInc = 0

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
    opt_ = adjustl(opt_(len_trim(opt1)+1:))

    selectcase( opt1 )
    case( '+p' )
      val_echoPrc = LTRUE
    case( '-p' )
      val_echoPrc = LFALS
    case( '+pr' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '-pr' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '+pc' )
      val_echoPcc = LTRUE
    case( '-pc' )
      val_echoPcc = LFALS
    case( '+pcr' )
      val_echoPcc = LTRUE
      isRec_echoPcc = .true.
    case( '-pcr' )
      val_echoPcc = LFALS
      isRec_echoPcc = .true.
    case( '+c' )
      val_echoMsg = LTRUE
    case( '-c' )
      val_echoMsg = LFALS
    case( '+cr' )
      val_echoMsg = LTRUE
      isRec_echoMsg = .true.
    case( '-cr' )
      val_echoMsg = LFALS
      isRec_echoMsg = .true.
    case( '+w' )
      val_echoWrn = LTRUE
    case( '-w' )
      val_echoWrn = LFALS
    case( '+wr' )
      val_echoWrn = LTRUE
      isRec_echoWrn = .true.
    case( '-wr' )
      val_echoWrn = LFALS
      isRec_echoWrn = .true.
    case( '+a' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      val_echoMsg = LTRUE
      val_echoWrn = LTRUE
    case( '-a' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      val_echoMsg = LFALS
      val_echoWrn = LFALS
    case( '+ar' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      val_echoMsg = LTRUE
      val_echoWrn = LTRUE
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
      isRec_echoMsg = .true.
      isRec_echoWrn = .true.
    case( '-ar' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      val_echoMsg = LFALS
      val_echoWrn = LFALS
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
      isRec_echoMsg = .true.
      isRec_echoWrn = .true.
    case( '+e' )
      val_echoErr = LTRUE
    case( '-e' )
      val_echoErr = LFALS
    case( '+er' )
      val_echoErr = LTRUE
      isRec_echoErr = .true.
    case( '-er' )
      val_echoErr = LFALS
      isRec_echoErr = .true.
    case( '+b' )
      val_echoBar = LTRUE
    case( '-b' )
      val_echoBar = LFALS
    case( '+br' )
      val_echoBar = LTRUE
      isRec_echoBar = .true.
    case( '-br' )
      val_echoBar = LFALS
      isRec_echoBar = .true.
    case( '+q' )
      val_stopErr = LTRUE
    case( '-q' )
      val_stopErr = LFALS
    case( '+qr' )
      val_stopErr = LTRUE
      isRec_stopErr = .true.
    case( '-qr' )
      val_stopErr = LFALS
      isRec_stopErr = .true.
    case( '+t' )
      val_msrTime = LTRUE
    case( '-t' )
      val_msrTime = LFALS
    case( '+tr' )
      val_msrTime = LTRUE
      isRec_msrTime = .true.
    case( '-tr' )
      val_msrTime = LFALS
      isRec_msrTime = .true.
    case( 'f' )
      forceEcho = .true.
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentInc
        indentInc = -indentInc
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call errend(ERROR_CODE__SYNTAX, &
          'Invalid format of option: '//trim(opt), &
          MODNAM, PRCNAM)
      endif
    endselect
  enddo
  !-------------------------------------------------------------
  ! Update statuses and commands
  !-------------------------------------------------------------
  depth = depth + 1

  call fwrd_dict(cmd%echoPrc, set%echoPrc, depth, val_echoPrc, isRec_echoPrc)
  call fwrd_dict(cmd%echoPcc, set%echoPcc, depth, val_echoPcc, isRec_echoPcc)
  call fwrd_dict(cmd%echoMsg, set%echoMsg, depth, val_echoMsg, isRec_echoMsg)
  call fwrd_dict(cmd%echoWrn, set%echoWrn, depth, val_echoWrn, isRec_echoWrn)
  call fwrd_dict(cmd%echoErr, set%echoErr, depth, val_echoErr, isRec_echoErr)
  call fwrd_dict(cmd%echoBar, set%echoBar, depth, val_echoBar, isRec_echoBar)
  call fwrd_dict(cmd%stopErr, set%stopErr, depth, val_stopErr, isRec_stopErr)
  call fwrd_dict(cmd%msrTime, set%msrTime, depth, val_msrTime, isRec_msrTime)

  if( present(mod) )then
    set%mod(depth) = trim(mod)
  else
    set%mod(depth) = set%mod(depth-1)
  endif
  if( present(prc) )then
    set%prc(depth) = trim(prc)
  else
    set%prc(depth) = set%prc(depth-1)
  endif
  set%stp(depth) = trim(stp)

  set%indent(depth) = set%indent(depth-1) + indentInc
  set%indentInc(depth) = indentInc

  set%echoPrc(depth) = get_tf(val_echoPrc, set%echoPcc(depth-1))
  !-------------------------------------------------------------
  ! Print the message
  !-------------------------------------------------------------
  if( set%echoPrc(depth) )then
    call echo_lines(STDOUT, trim(set%stp(depth)), &
                    set%indent(depth), .true.)
  endif
  !-------------------------------------------------------------
  ! Start the timer
  !-------------------------------------------------------------
  if( set%msrTime(depth) )then
    set%time_bgn(depth,:) = date_and_time_values()
  endif
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------
  set%indent(depth) = set%indent(depth) + INDENTINC_PRC
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  makeNewLine_prev = .true.
end subroutine logent
!===============================================================
!
!===============================================================
subroutine logext(stp, prc, mod)
  implicit none
  character(*), intent(in), optional :: stp
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: mod
  !-------------------------------------------------------------
  ! Check consistencies
  !-------------------------------------------------------------
  if( present(stp) ) call check_stp(stp)
  if( present(prc) ) call check_prc(prc)
  if( present(mod) ) call check_mod(mod)
  !-------------------------------------------------------------
  ! Update the indent
  !-------------------------------------------------------------
  set%indent(depth) = set%indent(depth) - set%indentInc(depth) - INDENTINC_PRC
  !-------------------------------------------------------------
  ! Update the status
  !-------------------------------------------------------------
  depth = depth - 1

  call back_dict(cmd%echoPrc, set%echoPrc, depth)
  call back_dict(cmd%echoPcc, set%echoPcc, depth)
  call back_dict(cmd%echoMsg, set%echoMsg, depth)
  call back_dict(cmd%echoWrn, set%echoWrn, depth)
  call back_dict(cmd%echoErr, set%echoErr, depth)
  call back_dict(cmd%echoBar, set%echoBar, depth)
  call back_dict(cmd%stopErr, set%stopErr, depth)
  call back_dict(cmd%msrTime, set%msrTime, depth)

  makeNewLine_prev = .true.
end subroutine logext
!===============================================================
!
!===============================================================
subroutine logmsg(msg, un, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'logmsg'
  character(*), intent(in) :: msg
  integer     , intent(in), optional :: un
  character(*), intent(in), optional :: opt

  integer :: un_

  integer :: val_echoMsg

  logical :: echoMsg

  logical :: forceEcho
  logical :: makeNewLine
  integer :: indent, indentInc
  character(CLEN_OPT) :: opt_, opt1
  integer :: ios
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  un_ = STDOUT
  if( present(un) ) un_ = un
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  val_echoMsg = LMISS

  forceEcho   = .false.
  makeNewLine = .true.

  indent = INDENT_MISS
  indentInc = 0

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
    opt_ = adjustl(opt_(len_trim(opt1)+1:))

    selectcase( opt1 )
    case( '+c' )
      val_echoMsg = LTRUE
    case( '-c' )
      val_echoMsg = LFALS
    case( 'f' )
      forceEcho = .true.
    case( '+n' )
      makeNewLine = .true.
    case( '-n' )
      makeNewLine = .false.
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentInc
        indentInc = -indentInc
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call errend(ERROR_CODE__SYNTAX, &
          'Invalid format of option: '//trim(opt), &
          MODNAM, PRCNAM)
      endif
    endselect
  enddo

  if( forceEcho )then
    echoMsg = .true.
  else
    echoMsg = get_tf(val_echoMsg, set%echoMsg(depth))
  endif
  !-------------------------------------------------------------
  ! Set indent
  !-------------------------------------------------------------
  if( indent == INDENT_MISS ) indent = set%indent(depth) + indentInc
  !-------------------------------------------------------------
  ! Print message
  !-------------------------------------------------------------
  if( echoMsg )then
    call echo_lines(un_, msg, indent, makeNewLine)
  endif
  !-------------------------------------------------------------
end subroutine logmsg
!===============================================================
!
!===============================================================
subroutine logwrn(ec, msg, mod, prc, stp, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'logwrn'
  integer, intent(in) :: ec
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: stp
  character(*), intent(in), optional :: opt

  character(:), allocatable :: mod_, prc_, stp_

  integer :: val_echoPrc
  integer :: val_echoMsg
  integer :: val_echoBar

  logical :: echoPrc
  logical :: echoMsg
  logical :: echoBar
  logical :: forceEcho
  logical :: makeNewLine
  integer :: indent, indentInc
  character(CLEN_OPT) :: opt_, opt1
  integer :: ios

  character(:), allocatable :: head
  integer :: i
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  allocate(character(1) :: mod_, prc_, stp_)
  mod_ = trim(set%mod(depth))
  prc_ = trim(set%prc(depth))
  stp_ = trim(set%stp(depth))
  if( present(mod) ) mod_ = trim(mod)
  if( present(prc) ) prc_ = trim(prc)
  if( present(stp) ) stp_ = trim(stp)
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  val_echoPrc = LMISS
  val_echoMsg = LMISS
  val_echoBar = LMISS

  makeNewLine = .true.
  forceEcho = .false.

  indent = INDENT_MISS
  indentInc = 0

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
    opt_ = adjustl(opt_(len_trim(opt1)+1:))

    selectcase( opt1 )
    case( '+b' )
      val_echoBar = LTRUE
    case( '-b' )
      val_echoBar = LFALS
    case( '+p' )
      val_echoPrc = LTRUE
    case( '-p' )
      val_echoPrc = LFALS
    case( '+c' )
      val_echoMsg = LTRUE
    case( '-c' )
      val_echoMsg = LFALS
    case( '+a' )
      val_echoBar = LTRUE
      val_echoPrc = LTRUE
      val_echoMsg = LTRUE
    case( '-a' )
      val_echoBar = LFALS
      val_echoPrc = LFALS
      val_echoMsg = LFALS
    case( 'f' )
      forceEcho = .true.
    case( '+n' )
      makeNewLine = .true.
    case( '-n' )
      makeNewLine = .false.
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentInc
        indentInc = -indentInc
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call errend(ERROR_CODE__SYNTAX, &
          'Invalid format of option: '//trim(opt), &
          MODNAM, PRCNAM)
      endif
    endselect
  enddo

  if( forceEcho )then
    echoBar = .true.
    echoPrc = .true.
    echoMsg = .true.
  else
    echoBar = get_tf(val_echoBar, set%echoWrn(depth))
    echoPrc = get_tf(val_echoPrc, set%echoWrn(depth))
    echoMsg = get_tf(val_echoMsg, set%echoWrn(depth))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( echoBar )then
    allocate(character(1) :: head)
    head = wrnHead(ec)
    call echo_lines(STDOUT, head//strprc(mod_,prc_,stp_), set%indent(depth), .true.)
  endif
  if( echoMsg )then
    call echo_lines(STDOUT, trim(msg), set%indent(depth), makeNewLine)
  endif
  if( echoBar )then
    do i = 1, len_trim(head)
      head(i:i) = '!'
    enddo
    call echo_lines(STDOUT, head, set%indent(depth), .true.)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(stp_, prc_, mod_)
  !-------------------------------------------------------------
end subroutine logwrn
!===============================================================
!
!===============================================================
subroutine logerr(ec, msg, mod, prc, stp, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'logerr'
  integer, intent(in) :: ec
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: stp
  character(*), intent(in), optional :: opt

  character(:), allocatable :: stp_, prc_, mod_

  integer :: val_echoPrc
  integer :: val_echoMsg
  integer :: val_echoBar

  logical :: echoPrc
  logical :: echoMsg
  logical :: echoBar
  logical :: forceEcho

  logical :: makeNewLine
  integer :: indent, indentInc
  character(CLEN_OPT) :: opt_, opt1
  integer :: ios

  character(:), allocatable :: head
  integer :: i
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(character(1) :: stp_, prc_, mod_)
  stp_ = trim(set%stp(depth))
  prc_ = trim(set%prc(depth))
  mod_ = trim(set%mod(depth))
  if( present(stp) ) stp_ = trim(stp)
  if( present(prc) ) prc_ = trim(prc)
  if( present(mod) ) mod_ = trim(mod)
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  val_echoPrc = LMISS
  val_echoMsg = LMISS
  val_echoBar = LMISS

  makeNewLine = .true.

  indent = INDENT_MISS
  indentInc = 0

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
    opt_ = adjustl(opt_(len_trim(opt1)+1:))

    selectcase( opt1 )
    case( '+b' )
      val_echoBar = LTRUE
    case( '-b' )
      val_echoBar = LFALS
    case( '+p' )
      val_echoPrc = LTRUE
    case( '-p' )
      val_echoPrc = LFALS
    case( '+c' )
      val_echoMsg = LTRUE
    case( '-c' )
      val_echoMsg = LFALS
    case( '+a' )
      val_echoBar = LTRUE
      val_echoPrc = LTRUE
      val_echoMsg = LTRUE
    case( '-a' )
      val_echoBar = LFALS
      val_echoPrc = LFALS
      val_echoMsg = LFALS
    case( 'f' )
      forceEcho = .true.
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentInc
        indentInc = -indentInc
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call errend(ERROR_CODE__SYNTAX, &
          'Invalid format of option: '//trim(opt), &
          MODNAM, PRCNAM)
      endif
    endselect
  enddo

  if( forceEcho )then
    echoBar = .true.
    echoPrc = .true.
    echoMsg = .true.
  else
    echoBar = get_tf(val_echoBar, set%echoErr(depth))
    echoPrc = get_tf(val_echoPrc, set%echoErr(depth))
    echoMsg = get_tf(val_echoMsg, set%echoErr(depth))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( echoBar )then
    allocate(character(1) :: head)
    head = errHead(ec)
    call echo_lines(STDERR, head//'in '//strprc(mod_,prc_,stp_), set%indent(depth), .true.)
  endif
  if( echoMsg )then
    call echo_lines(STDERR, trim(msg), set%indent(depth), makeNewLine)
  endif
  if( echoBar )then
    do i = 1, len_trim(head)
      head(i:i) = '!'
    enddo
    call echo_lines(STDERR, head, set%indent(depth), .true.)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(stp_, prc_, mod_)
  !-------------------------------------------------------------
end subroutine logerr
!===============================================================
!
!===============================================================
recursive subroutine errend(ec, msg, mod, prc, stp, opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'errend'
  integer, intent(in), optional :: ec
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: stp
  character(*), intent(in), optional :: opt

  integer :: ec_
  character(:), allocatable :: msg_
  character(:), allocatable :: mod_, prc_, stp_

  integer :: val_stopErr
  integer :: val_echoBar

  logical :: stopErr
  logical :: echoBar

  character(CLEN_OPT) :: opt_, opt1
  integer :: i
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call initialize()
  !-------------------------------------------------------------
  ! Read inputs
  !-------------------------------------------------------------
  ec_ = ERROR_CODE__BASE
  if( present(ec) ) ec_ = ec

  allocate(character(1) :: msg_, mod_, prc_, stp_)
  msg_ = ''
  mod_ = trim(set%mod(depth))
  prc_ = trim(set%prc(depth))
  stp_ = trim(set%stp(depth))
  if( present(msg) ) msg_ = trim(msg)
  if( present(mod) ) mod_ = trim(mod)
  if( present(prc) ) prc_ = trim(prc)
  if( present(stp) ) stp_ = trim(stp)
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  val_stopErr = LMISS
  val_echoBar = LMISS

  opt_ = ''
  if( present(opt) ) opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
    opt_ = adjustl(opt_(len_trim(opt1)+1:))

    selectcase( opt1 )
    case( '+q' )
      val_stopErr = LTRUE
    case( '-q' )
      val_stopErr = LFALS
    case( '+b' )
      val_echoBar = LTRUE
    case( '-b' )
      val_echoBar = LFALS
    case default
      call errend(ERROR_CODE__SYNTAX, 'Invalid option: '//trim(opt), MODNAM, PRCNAM)
    endselect
  enddo

  stopErr = get_tf(val_stopErr, set%stopErr(depth))
  echoBar = get_tf(val_echoBar, set%echoWrn(depth))
  !-------------------------------------------------------------
  ! Add error message
  !-------------------------------------------------------------
  if( msg_ /= '' )then
    call errnew(ec_, msg_, mod_, prc_, stp_)
  endif
  !-------------------------------------------------------------
  ! Print messages
  !-------------------------------------------------------------
  if( echoBar )then
    call echo_lines(STDERR, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', 0, .true.)
  endif

  do i = n_err, 1, -1
    call echo_lines(STDERR, errType(err(i)%code)//' in '//strprc(err(i)%mod, err(i)%prc, err(i)%stp), 0, .true.)
    call echo_lines(STDERR, err(i)%msg, 2, .true.)
  enddo

  if( echoBar )then
    call echo_lines(STDERR, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', 0, .true.)
  endif

!  write(STDOUT, "(a)") '------ PROCESSES ------'
!  do i = 1, depth
!    call echo_lines(STDERR, strprc(set%mod(i), set%prc(i), set%stp(i)), 0, .true.)
!  enddo
  !-------------------------------------------------------------
  ! Clear stored error messages
  !-------------------------------------------------------------
  call clear_err()
  !-------------------------------------------------------------
  ! Stop program
  !-------------------------------------------------------------
  if( stopErr )then
    stop STOP_CODE_ERROR
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(msg_, stp_, prc_, mod_)
  !-------------------------------------------------------------
end subroutine errend
!===============================================================
!
!===============================================================
subroutine errret(ec, msg, mod, prc)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'errret'
  integer, intent(in), optional :: ec
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: prc

  integer :: ec_
  character(:), allocatable :: msg_
  character(:), allocatable :: errmsg
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ec_ = ERROR_CODE__BASE
  if( present(ec) ) ec_ = ec

  allocate(character(1) :: msg_)
  msg_ = ''
  if( present(msg) ) msg_ = trim(msg)
  !-------------------------------------------------------------
  ! Case: `logbgn` was not called in the upper procedure
  ! Thus, need not to call `logret` here.
  if( present(mod) .and. present(prc) )then
    call errnew(ec_, msg_, mod, prc, '')
  !-------------------------------------------------------------
  ! Case: `logbgn` was called in the upper procedure
  ! Thus, need to call `logret` here.
  elseif( .not. present(mod) .and. .not. present(prc) )then
    do
      if( set%stp(depth) == '' )then
        call errnew(ec_, msg_, set%mod(depth), set%prc(depth), '')
        call logret()
        exit
      else
        call errnew(ec_, msg_, set%mod(depth), set%prc(depth), set%stp(depth))
        call logret()
      endif
    enddo
  !-------------------------------------------------------------
  ! Case: ERROR
  else
    allocate(character(1) :: errmsg)
    errmsg = 'present(mod) .neqv. present(prc)'
    if( present(mod) ) errmsg = trim(errmsg)//'\nmod: '//trim(mod)
    if( present(prc) ) errmsg = trim(errmsg)//'\nprc: '//trim(prc)
    call errend(ERROR_CODE__BASE, errmsg, MODNAM, PRCNAM)
  endif
  !-------------------------------------------------------------
  deallocate(msg_)
end subroutine errret
!===============================================================
!
!===============================================================
subroutine errnew(ec, msg, mod, prc, stp)
  implicit none
  integer     , intent(in), optional :: ec
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: stp

  integer :: ec_
  character(:), allocatable :: msg_
  character(:), allocatable :: mod_, prc_, stp_

  call initialize()

  ec_ = ERROR_CODE__BASE
  if( present(ec) ) ec_ = ec

  allocate(character(1) :: msg_)
  msg_ = '(no message)'
  if( present(msg) ) msg_ = trim(msg)

  allocate(character(1) :: mod_, prc_, stp_)
  mod_ = trim(set%mod(depth))
  prc_ = trim(set%prc(depth))
  stp_ = trim(set%stp(depth))
  if( present(mod) ) mod_ = trim(mod)
  if( present(prc) ) prc_ = trim(prc)
  if( present(stp) ) stp_ = trim(stp)

  if( n_err == nmax_err ) call extend_err()
  n_err = n_err + 1
  allocate(character(1) :: err(n_err)%mod, err(n_err)%prc, err(n_err)%stp)
  allocate(character(1) :: err(n_err)%msg)
  err(n_err)%code = ec_
  err(n_err)%mod = trim(mod_)
  err(n_err)%prc = trim(prc_)
  err(n_err)%stp = trim(stp_)
  err(n_err)%msg = trim(msg)
end subroutine errnew
!===============================================================
! Append error message
!===============================================================
subroutine erradd(msg, newline)
  implicit none
  character(*), intent(in) :: msg
  logical     , intent(in), optional :: newline

  logical :: newline_

  newline_ = .true.
  if( present(newline) ) newline_ = newline

  if( newline_ )then
    err(n_err)%msg = trim(err(n_err)%msg)//'\n'//trim(msg)
  else
    err(n_err)%msg = trim(err(n_err)%msg)//trim(msg)
  endif
end subroutine erradd
!===============================================================
!
!===============================================================
subroutine setlog(opt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setlog'
  character(*), intent(in) :: opt

  integer :: val_echoPrc
  integer :: val_echoPcc
  integer :: val_echoMsg
  integer :: val_echoWrn
  integer :: val_echoErr
  integer :: val_echoBar
  integer :: val_stopErr
  integer :: val_msrTime

  logical :: isRec_echoPrc
  logical :: isRec_echoPcc
  logical :: isRec_echoMsg
  logical :: isRec_echoWrn
  logical :: isRec_echoErr
  logical :: isRec_echoBar
  logical :: isRec_stopErr
  logical :: isRec_msrTime

  character(32) :: opt_, opt1
  integer :: indent
  integer :: indentInc
  integer :: ios
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call initialize()
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  val_echoPrc = LMISS
  val_echoPcc = LMISS
  val_echoMsg = LMISS
  val_echoWrn = LMISS
  val_echoErr = LMISS
  val_echoBar = LMISS
  val_stopErr = LMISS
  val_msrTime = LMISS

  isRec_echoPrc = .false.
  isRec_echoPcc = .false.
  isRec_echoMsg = .false.
  isRec_echoWrn = .false.
  isRec_echoErr = .false.
  isRec_echoBar = .false.
  isRec_stopErr = .false.
  isRec_msrTime = .false.

  indent = INDENT_MISS
  indentInc = 0

  opt_ = adjustl(opt)

  do while( len_trim(opt_) > 0 )
    read(opt_, *) opt1
    opt_ = adjustl(opt_(len_trim(opt1)+1:))

    selectcase( opt1 )
    case( '+p' )
      val_echoPrc = LTRUE
    case( '-p' )
      val_echoPrc = LFALS
    case( '+pr' )
      val_echoPrc = LTRUE
      val_echoPcc = LTRUE
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '-pr' )
      val_echoPrc = LFALS
      val_echoPcc = LFALS
      isRec_echoPrc = .true.
      isRec_echoPcc = .true.
    case( '+pc' )
      val_echoPcc = LTRUE
    case( '-pc' )
      val_echoPcc = LFALS
    case( '+pcr' )
      val_echoPcc = LTRUE
      isRec_echoPcc = .true.
    case( '-pcr' )
      val_echoPcc = LFALS
      isRec_echoPcc = .true.
    case( '+c' )
      val_echoMsg = LTRUE
    case( '-c' )
      val_echoMsg = LFALS
    case( '+cr' )
      val_echoMsg = LTRUE
      isRec_echoMsg = .true.
    case( '-cr' )
      val_echoMsg = LFALS
      isRec_echoMsg = .true.
    case( '+b' )
      val_echoBar = LTRUE
    case( '-b' )
      val_echoBar = LFALS
    case( '+br' )
      val_echoBar = LTRUE
      isRec_echoBar = .true.
    case( '-br' )
      val_echoBar = LFALS
      isRec_echoBar = .true.
    case( '+w' )
      val_echoWrn = LTRUE
    case( '-w' )
      val_echoWrn = LFALS
    case( '+wr' )
      val_echoWrn = LTRUE
      isRec_echoWrn = .true.
    case( '-wr' )
      val_echoWrn = LFALS
      isRec_echoWrn = .true.
    case( '+e' )
      val_echoErr = LTRUE
    case( '-e' )
      val_echoErr = LFALS
    case( '+er' )
      val_echoErr = LTRUE
      isRec_echoErr = .true.
    case( '-er' )
      val_echoErr = LFALS
      isRec_echoErr = .true.
    case( '+a' )
      val_echoPrc = LTRUE
      val_echoMsg = LTRUE
      val_echoBar = LTRUE
      val_echoWrn = LTRUE
      val_echoErr = LTRUE
    case( '-a' )
      val_echoPrc = LFALS
      val_echoMsg = LFALS
      val_echoBar = LFALS
      val_echoWrn = LFALS
      val_echoErr = LFALS
    case( '+q' )
      val_stopErr = LTRUE
    case( '-q' )
      val_stopErr = LFALS
    case( '+qr' )
      val_stopErr = LTRUE
      isRec_stopErr = .true.
    case( '-qr' )
      val_stopErr = LFALS
      isRec_stopErr = .true.
    case( '+t' )
      val_msrTime = LTRUE
    case( '-t' )
      val_msrTime = LFALS
    case( '+tr' )
      val_msrTime = LTRUE
      isRec_msrTime = .true.
    case( '-tr' )
      val_msrTime = LFALS
      isRec_msrTime = .true.
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentInc
        indentInc = -indentInc
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call errend(ERROR_CODE__SYNTAX, &
          'Invalid format option: '//trim(opt_), &
          MODNAM, PRCNAM)
      endif
    endselect
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call fwrd_dict(cmd%echoPcc, set%echoPcc, &
                 depth, val_echoPcc, isRec_echoPcc)
  call fwrd_dict(cmd%echoMsg, set%echoMsg, &
                 depth, val_echoMsg, isRec_echoMsg)
  call fwrd_dict(cmd%echoBar, set%echoBar, &
                 depth, val_echoBar, isRec_echoBar)
  call fwrd_dict(cmd%echoWrn, set%echoWrn, &
                 depth, val_echoWrn, isRec_echoWrn)
  call fwrd_dict(cmd%echoErr, set%echoErr, &
                 depth, val_echoErr, isRec_echoErr)
  call fwrd_dict(cmd%stopErr, set%stopErr, &
                 depth, val_stopErr, isRec_stopErr)
  call fwrd_dict(cmd%msrTime, set%msrTime, &
                 depth, val_msrTime, isRec_msrTime)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( indent == INDENT_MISS )then
    set%indent(depth) = set%indent(depth) + indentInc
  else
    set%indent(depth) = indent
  endif
  !-------------------------------------------------------------
end subroutine setlog
!===============================================================
!
!===============================================================
subroutine traperr(info)
  implicit none
  integer, intent(in) :: info

  if( info /= 0 )then
    call errend(ERROR_CODE__BASE)
  endif
end subroutine traperr
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
subroutine check_mod(mod)
  implicit none
  character(*), intent(in) :: mod

  integer :: idp

  if( trim(mod) /= trim(set%mod(depth)) )then
    write(STDOUT,"(a)") '*** INTERNAL ERROR in '//&
      'MOD__lib_log_proc__PROC__check_mod ***'
    write(STDOUT,"(a)") &
          'Input   mod: '//trim(mod)
    write(STDOUT,"(a)") &
          'Current mod: '//trim(set%mod(depth))
    do idp = 1, depth
      write(STDOUT,"(a,1x,i0)") '@ depth ',idp
      write(STDOUT,"(2x,a)") &
        '  mod: '//trim(set%mod(idp))//&
         ' prc: '//trim(set%prc(idp))//&
         ' stp: '//trim(set%stp(idp))
    enddo
    stop STOP_CODE_ERROR
  endif
end subroutine check_mod
!===============================================================
!
!===============================================================
subroutine check_prc(prc)
  implicit none
  character(*), intent(in) :: prc

  integer :: idp

  if( trim(prc) /= trim(set%prc(depth)) )then
    write(STDOUT,"(a)") '*** INTERNAL ERROR in '//&
      'MOD__lib_log_proc__PROC__check_prc ***'
    write(STDOUT,"(a)") &
          'Input   prc: '//trim(prc)
    write(STDOUT,"(a)") &
          'Current prc: '//trim(set%prc(depth))
    do idp = 1, depth
      write(STDOUT,"(a,1x,i0)") '@ depth ',idp
      write(STDOUT,"(a)") &
        '  mod: '//trim(set%mod(idp))//&
         ' prc: '//trim(set%prc(idp))//&
         ' stp: '//trim(set%stp(idp))
    enddo
    stop STOP_CODE_ERROR
  endif
end subroutine check_prc
!===============================================================
!
!===============================================================
subroutine check_stp(stp)
  implicit none
  character(*), intent(in) :: stp

  integer :: idp

  if( trim(stp) /= trim(set%stp(depth)) )then
    write(STDOUT,"(a)") '*** INTERNAL ERROR in '//&
      'MOD__lib_log_proc__PROC__check_stp ***'
    write(STDOUT,"(a)") &
          'Input   stp: '//trim(stp)
    write(STDOUT,"(a)") &
          'Current stp: '//trim(set%stp(depth))
    do idp = 1, depth
      write(STDOUT,"(a,1x,i0)") '@ depth ',idp
      write(STDOUT,"(a)") &
        '  mod: '//trim(set%mod(idp))//&
         ' prc: '//trim(set%prc(idp))//&
         ' stp: '//trim(set%stp(idp))
    enddo
    stop STOP_CODE_ERROR
  endif
end subroutine check_stp
!===============================================================
!
!===============================================================
subroutine init_cmd(cmd, set, val_def)
  implicit none
  type(cmdDict_), intent(inout) :: cmd
  logical       , pointer       :: set(:)
  integer       , intent(in)    :: val_def

  cmd%currentCmd%depth = 0
  cmd%currentCmd%tf = val_def == LTRUE

  allocate(cmd%cmdHist%depth(0:DEPTH_MAX), &
           cmd%cmdHist%tf(0:DEPTH_MAX))
  cmd%cmdHist%nCmd = 0
  cmd%cmdHist%depth(0) = 0
  cmd%cmdHist%tf(0) = val_def == LTRUE

  allocate(set(0:DEPTH_MAX))
  set(0) = val_def == LTRUE
end subroutine init_cmd
!===============================================================
!
!===============================================================
subroutine fwrd_dict(cmd, set, depth, val, isRec)
  implicit none
  type(cmdDict_), intent(inout) :: cmd
  logical       , intent(inout) :: set(0:)
  integer       , intent(in)    :: depth
  integer       , intent(in)    :: val
  logical       , intent(in)    :: isRec

  integer :: idp

  if( isRec )then
    cmd%currentCmd%depth = depth
    cmd%currentCmd%tf = val == LTRUE
    cmd%cmdHist%nCmd = cmd%cmdHist%nCmd + 1
    cmd%cmdHist%depth(cmd%cmdHist%nCmd) = depth
    cmd%cmdHist%tf(cmd%cmdHist%nCmd) = val == LTRUE
    do idp = depth, DEPTH_MAX
      set(idp) = val == LTRUE
    enddo
  else
    selectcase( val )
    case( LTRUE, LFALS )
      set(depth) = val == LTRUE
    case( LMISS )
      set(depth) = cmd%cmdHist%tf(cmd%cmdHist%nCmd)
    endselect
  endif
end subroutine fwrd_dict
!===============================================================
!
!===============================================================
subroutine back_dict(cmd, set, depth)
  implicit none
  type(cmdDict_), intent(inout) :: cmd
  logical       , intent(inout) :: set(0:)
  integer       , intent(in)    :: depth

  integer :: idp

  if( depth < cmd%currentCmd%depth )then
    cmd%cmdHist%nCmd = cmd%cmdHist%nCmd - 1
    idp = cmd%cmdHist%depth(cmd%cmdHist%nCmd)
    cmd%currentCmd%depth = idp
    cmd%currentCmd%tf = cmd%cmdHist%tf(idp)
  endif

  do idp = depth+1, DEPTH_MAX
    set(idp) = cmd%currentCmd%tf
  enddo
end subroutine back_dict
!===============================================================
!
!===============================================================
recursive logical function get_tf(val, tf_def) result(tf)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_tf'
  integer, intent(in) :: val
  logical, intent(in) :: tf_def

  character(128) :: c

  selectcase( val )
  case( LTRUE )
    tf = .true.
  case( LFALS )
    tf = .false.
  case( LMISS )
    tf = tf_def
  case default
    write(c,"(i0)") val
    call errend(ERROR_CODE__INTERNAL, &
           'val: '//trim(c), &
           MODNAM, PRCNAM)
  endselect
end function get_tf
!===============================================================
!
!===============================================================
subroutine clear_err()
  implicit none
  integer :: i

  do i = 1, n_err
    deallocate(err(i)%stp, err(i)%prc, err(i)%mod)
    deallocate(err(i)%msg)
  enddo
  n_err = 0
end subroutine clear_err
!===============================================================
!
!===============================================================
subroutine extend_err()
  implicit none
  type(err_), allocatable :: e(:)
  integer :: i

  nmax_err = n_err*2

  allocate(e(nmax_err))
  do i = 1, n_err
    allocate(character(1) :: e(i)%mod, e(i)%prc, e(i)%stp, e(i)%msg)
    e(i)%code = err(i)%code
    e(i)%mod = trim(err(i)%mod)
    e(i)%prc = trim(err(i)%prc)
    e(i)%stp = trim(err(i)%stp)
    e(i)%msg = trim(err(i)%msg)
  enddo

  deallocate(err)
  allocate(err(nmax_err))

  do i = 1, n_err
    allocate(character(1) :: err(i)%mod, err(i)%prc, err(i)%stp, err(i)%msg)
    err(i)%code = e(i)%code
    err(i)%mod = trim(e(i)%mod)
    err(i)%prc = trim(e(i)%prc)
    err(i)%stp = trim(e(i)%stp)
    err(i)%msg = trim(e(i)%msg)
  enddo

  deallocate(e)
end subroutine extend_err
!===============================================================
!
!===============================================================
end module lib_log_proc
