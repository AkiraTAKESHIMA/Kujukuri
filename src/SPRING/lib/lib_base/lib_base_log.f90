module lib_base_log
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: echo_lines

  public :: strprc

  public :: errType
  public :: wrnType
  public :: errHead
  public :: wrnHead

  public :: errmsg_base
  public :: errmsg_value
  public :: errmsg_condition
  public :: errmsg_call
  public :: errmsg_argument
  public :: errmsg_io
  public :: errmsg_syntax
  public :: errmsg_arrayindex
  public :: errmsg_internal
  public :: errmsg_noimplementation
  public :: wrnmsg_base
  public :: wrnmsg_condition
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine echo_lines(un, msg, idt, adv)
  implicit none
  integer     , intent(in) :: un
  character(*), intent(in) :: msg
  integer     , intent(in) :: idt
  logical     , intent(in) :: adv

  character(len_trim(msg)) :: msg_

  character(16) :: wfmt
  character(4)  :: advance
  integer :: loc
  !-------------------------------------------------------------
  ! Start a new line after writing the message if $adv is true
  !-------------------------------------------------------------
  if( adv )then
    advance = 'yes'
  else
    advance = 'no'
  endif
  !-------------------------------------------------------------
  ! Modify indent
  !-------------------------------------------------------------
  if( idt == 0 )then
    wfmt = "(a)"
  else
    write(wfmt,"(a,i0,a)") '(',idt,'x,a)'
  endif
  !-------------------------------------------------------------
  ! Write the message
  !-------------------------------------------------------------
  if( index(msg,'\n') == 0 )then
    write(un, wfmt, advance=advance) trim(msg)

  else
    msg_ = msg

    do
      loc = index(msg_,'\n')

      selectcase( loc )

      case( 0 )
        write(un, wfmt, advance=advance) trim(msg_)
        exit

      case( 1 )
        write(un, wfmt) ''

      case( 2: )
        write(un, wfmt) trim(msg_(:loc-1))

      endselect

      if( loc+1 == len_trim(msg_) )then
        exit
      else
        msg_ = msg_(loc+2:)
      endif
    enddo

  endif
end subroutine echo_lines
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
function strprc(mod, prc, stp) result(s)
  implicit none
  character(*), intent(in), optional :: mod
  character(*), intent(in), optional :: prc
  character(*), intent(in), optional :: stp
  character(:), allocatable :: s

  character(:), allocatable :: mod_, prc_, stp_

  allocate(character(1) :: s)

  allocate(character(1) :: mod_, prc_, stp_)
  mod_ = ''
  prc_ = ''
  stp_ = ''
  if( present(mod) ) mod_ = trim(mod)
  if( present(prc) ) prc_ = trim(prc)
  if( present(stp) ) stp_ = trim(stp)

  if( mod_ == '' )then
    if( prc_ == '' )then
      if( stp_ == '' )then
        s = ''
      else
        s = 'step "'//trim(stp_)//'"'
      endif
    else
      if( stp_ == '' )then
        s = 'PROC__'//trim(prc_)
      else
        s = 'PROC__'//trim(prc_)//' step "'//trim(stp_)//'"'
      endif
    endif
  else
    if( prc_ == '' )then
      if( stp_ == '' )then
        if( index(mod_, 'program') == 1 .and. len_trim(mod_) > len('program ') )then
          s = 'PROGRAM '//trim(mod_(len('program ')+1:))
        else
          s = 'MOD__'//trim(mod_)
        endif
      else
        s = 'MOD__'//trim(mod_)//' step "'//trim(stp_)//'"'
      endif
    else
      if( stp_ == '' )then
        s = 'MOD__'//trim(mod_)//'__PROC__'//trim(prc_)
      else
        s = 'MOD__'//trim(mod_)//'__PROC__'//trim(prc_)//' step "'//trim(stp_)//'"'
      endif
    endif
  endif
end function strprc
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
function errType(ec) result(res)
  implicit none
  integer, intent(in) :: ec
  character(:), allocatable :: res

  allocate(character(1) :: res)

  selectcase( ec )
  case( ERROR_CODE__BASE )
    res = trim(ERROR_TYPE__BASE)
  case( ERROR_CODE__VALUE )
    res = trim(ERROR_TYPE__VALUE)
  case( ERROR_CODE__CONDITION )
    res = trim(ERROR_TYPE__CONDITION)
  case( ERROR_CODE__CALL )
    res = trim(ERROR_TYPE__CALL)
  case( ERROR_CODE__ARGUMENT )
    res = trim(ERROR_TYPE__ARGUMENT)
  case( ERROR_CODE__IO )
    res = trim(ERROR_TYPE__IO)
  case( ERROR_CODE__SYNTAX )
    res = trim(ERROR_TYPE__SYNTAX)
  case( ERROR_CODE__ARRAYINDEX )
    res = trim(ERROR_TYPE__ARRAYINDEX)
  case( ERROR_CODE__INTERNAL )
    res = trim(ERROR_TYPE__INTERNAL)
  case( ERROR_CODE__NOIMPLEMENTATION )
    res = trim(ERROR_TYPE__NOIMPLEMENTATION)
  case( ERROR_CODE__CHILDPROCESS )
    res = trim(ERROR_TYPE__CHILDPROCESS)
  case default
    res = trim(ERROR_TYPE__UNKNOWN)
  endselect
end function errType
!===============================================================
!
!===============================================================
function wrnType(ec) result(res)
  implicit none
  integer, intent(in) :: ec
  character(:), allocatable :: res

  allocate(character(1) :: res)

  selectcase( ec )
  case( ERROR_CODE__BASE )
    res = trim(WARNING_TYPE__BASE)
  case( ERROR_CODE__VALUE )
    res = trim(WARNING_TYPE__VALUE)
  case( ERROR_CODE__CONDITION )
    res = trim(WARNING_TYPE__CONDITION)
  case( ERROR_CODE__CALL )
    res = trim(WARNING_TYPE__CALL)
  case( ERROR_CODE__ARGUMENT )
    res = trim(WARNING_TYPE__ARGUMENT)
  case( ERROR_CODE__IO )
    res = trim(WARNING_TYPE__IO)
  case( ERROR_CODE__SYNTAX )
    res = trim(WARNING_TYPE__SYNTAX)
  case( ERROR_CODE__ARRAYINDEX )
    res = trim(WARNING_TYPE__ARRAYINDEX)
  case( ERROR_CODE__INTERNAL )
    res = trim(WARNING_TYPE__INTERNAL)
  case( ERROR_CODE__NOIMPLEMENTATION )
    res = trim(WARNING_TYPE__NOIMPLEMENTATION)
  case( ERROR_CODE__CHILDPROCESS )
    res = trim(WARNING_TYPE__CHILDPROCESS)
  case default
    res = trim(WARNING_TYPE__UNKNOWN)
  endselect
end function wrnType
!===============================================================
!
!===============================================================
function errHead(ec) result(res)
  implicit none
  integer, intent(in) :: ec
  character(:), allocatable :: res

  allocate(character(1) :: res)

  res = '!!!!!! '//errType(ec)//' !!!!!! '
end function errHead
!===============================================================
!
!===============================================================
function wrnHead(ec) result(res)
  implicit none
  integer, intent(in) :: ec
  character(:), allocatable :: res

  allocate(character(1) :: res)

  res = '!!!!!! '//wrnType(ec)//' !!!!!! '
end function wrnHead
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
function errmsg_base(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('Error', prc, msg)
end function errmsg_base
!===============================================================
!
!===============================================================
function errmsg_value(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('ValueError', prc, msg)
end function errmsg_value
!===============================================================
!
!===============================================================
function errmsg_condition(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('ConditionError', prc, msg)
end function errmsg_condition
!===============================================================
!
!===============================================================
function errmsg_call(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('CallError', prc, msg)
end function errmsg_call
!===============================================================
!
!===============================================================
function errmsg_argument(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('ArgumentError', prc, msg)
end function errmsg_argument
!===============================================================
!
!===============================================================
function errmsg_io(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('IOError', prc, msg)
end function errmsg_io
!===============================================================
!
!===============================================================
function errmsg_syntax(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('SyntaxError', prc, msg)
end function errmsg_syntax
!===============================================================
!
!===============================================================
function errmsg_arrayindex(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('ArrayIndexError', prc, msg)
end function errmsg_arrayindex
!===============================================================
!
!===============================================================
function errmsg_internal(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('InternalError', prc, msg)
end function errmsg_internal
!===============================================================
!
!===============================================================
function errmsg_noimplementation(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('NoImplementationError', prc, msg)
end function errmsg_noimplementation
!===============================================================
!
!===============================================================
function wrnmsg_base(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('Warning', prc, msg)
end function wrnmsg_base
!===============================================================
!
!===============================================================
function wrnmsg_condition(prc) result(msg)
  implicit none
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  call make_msg('ConditionWarning', prc, msg)
end function wrnmsg_condition
!===============================================================
!
!===============================================================
subroutine make_msg(body, prc, msg)
  implicit none
  character(*), intent(in) :: body
  character(*), intent(in) :: prc
  character(:), allocatable :: msg

  allocate(character(1) :: msg)
  if( prc == '' )then
    msg = '!!!!!! '//trim(body)//' !!!!!!'
  else
    msg = '!!!!!! '//trim(body)//' !!!!!! Message from: '//trim(prc)
  endif
end subroutine make_msg
!===============================================================
!
!===============================================================
end module lib_base_log
