module c1_opt_set
  use lib_const
  use lib_base
  use lib_log
  use lib_util
  use lib_io
  use lib_math
  use c1_const
  use c1_type_gs
  use c1_type_opt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: set_default_values_opt_sys
  public :: set_default_values_opt_log
  public :: set_default_values_opt_earth

  public :: set_values_opt_earth

  public :: echo_settings_opt_sys
  public :: echo_settings_opt_log
  public :: echo_settings_opt_earth
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  public :: key_old_files
  public :: key_dir_intermediates
  public :: key_remove_intermediates
  public :: key_memory_ulim

  public :: KEY_EARTH_GEOSYS
  public :: KEY_EARTH_RTYP
  public :: KEY_EARTH_R
  public :: KEY_EARTH_FINV
  public :: KEY_EARTH_F
  public :: KEY_EARTH_E2
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: key_old_files            = 'old_files'
  character(CLEN_VAR), parameter :: key_dir_intermediates    = 'dir_intermediates'
  character(CLEN_VAR), parameter :: key_remove_intermediates = 'remove_intermediates'
  character(CLEN_VAR), parameter :: key_memory_ulim          = 'memory_ulim'

  character(CLEN_VAR), parameter :: KEY_EARTH_GEOSYS = 'earth_geosys'
  character(CLEN_VAR), parameter :: KEY_EARTH_RTYP   = 'earth_rtyp'
  character(CLEN_VAR), parameter :: KEY_EARTH_R      = 'earth_r'
  character(CLEN_VAR), parameter :: KEY_EARTH_FINV   = 'earth_finv'
  character(CLEN_VAR), parameter :: KEY_EARTH_F      = 'earth_f'
  character(CLEN_VAR), parameter :: KEY_EARTH_E2     = 'earth_e2'
  !-----------------------------------------------------------
  ! Private module variables
  !-----------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c1_opt_set'
  !-----------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_sys(sys)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_sys'
  type(opt_sys_), intent(out) :: sys

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sys%old_files   = OPT_OLD_FILES_STOP
  sys%remove_im   = .true.
  sys%dir_im      = ''
  sys%memory_ulim = 0.d0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_default_values_opt_sys
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_log(log)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_log'
  type(opt_log_), intent(inout) :: log

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  log%print_summary = .true.
  log%write_summary = .true.
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_default_values_opt_log
!===============================================================
!
!===============================================================
subroutine set_default_values_opt_earth(earth)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_default_values_opt_earth'
  type(opt_earth_), intent(out) :: earth

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  earth%geosys = EARTH_GEOSYS__DEFAULT
  earth%rtyp   = EARTH_RTYP__DEFAULT
  earth%shptyp = ''
  earth%r    = 0.d0
  earth%finv = 0.d0
  earth%f    = 0.d0
  earth%e2   = 0.d0
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine set_default_values_opt_earth
!===============================================================
!
!===============================================================
integer(4) function set_values_opt_earth(&
    earth, n_r, n_finv, n_f, n_e2) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'set_values_opt_earth'
  type(opt_earth_), intent(inout) :: earth
  integer         , intent(in)    :: n_r, n_f, n_finv, n_e2

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( earth%geosys == '' )then
    call logmsg('Default value was set for %geosys, geodesic system.')
    earth%geosys = EARTH_GEOSYS__DEFAULT
  endif
  
  selectcase( earth%geosys )
  !-------------------------------------------------------------
  ! Case: Geosys = WGS84
  case( EARTH_GEOSYS__WGS84 )
    if( earth%rtyp == '' )then
      call logmsg('Default value was set for %rtyp, type of radius.')
      earth%rtyp = EARTH_RTYP__DEFAULT
    endif

    selectcase( earth%rtyp )

    case( EARTH_RTYP__ELLIPS )
      earth%shptyp = EARTH_SHPTYP__ELLIPS
      earth%r = EARTH_CONST__WGS84_R_SEMIMAJOR

    case( EARTH_RTYP__MEAN )
      earth%shptyp = EARTH_SHPTYP__SPHERE
      earth%r = EARTH_CONST__WGS84_R_MEAN

    case( EARTH_RTYP__VOLMETRIC )
      earth%shptyp = EARTH_SHPTYP__SPHERE
      earth%r = EARTH_CONST__WGS84_R_VOLMETRIC

    case( EARTH_RTYP__AUTHALIC )
      earth%shptyp = EARTH_SHPTYP__SPHERE
      earth%r = EARTH_CONST__WGS84_R_AUTHALIC

    case default
      info = 1
      call errret(msg_invalid_value('earth%rtyp', earth%rtyp))
      return
    endselect

    selectcase( earth%shptyp )
    case( EARTH_SHPTYP__SPHERE )
      earth%finv = 0.d0
      earth%f    = 0.d0
      earth%e2   = 0.d0
    case( EARTH_SHPTYP__ELLIPS )
      earth%finv = EARTH_CONST__WGS84_FINV
      earth%f    = EARTH_CONST__WGS84_F
      earth%e2   = EARTH_CONST__WGS84_E2
    endselect
  !-------------------------------------------------------------
  ! Case: Geosys = GRS80
  case( EARTH_GEOSYS__GRS80 )
    if( earth%rtyp == '' )then
      call logmsg('Default value was set for %rtyp, type of radius.')
      earth%rtyp = EARTH_RTYP__DEFAULT
    endif

    selectcase( earth%rtyp )

    case( EARTH_RTYP__ELLIPS )
      earth%shptyp = EARTH_SHPTYP__ELLIPS
      earth%r = EARTH_CONST__GRS80_R_SEMIMAJOR

    case( EARTH_RTYP__MEAN )
      earth%shptyp = EARTH_SHPTYP__SPHERE
      earth%r = EARTH_CONST__GRS80_R_MEAN

    case( EARTH_RTYP__VOLMETRIC )
      earth%shptyp = EARTH_SHPTYP__SPHERE
      earth%r = EARTH_CONST__GRS80_R_VOLMETRIC

    case( EARTH_RTYP__AUTHALIC )
      earth%shptyp = EARTH_SHPTYP__SPHERE
      earth%r = EARTH_CONST__GRS80_R_AUTHALIC

    case default
      info = 1
      call errret(msg_invalid_value('earth%rtyp', earth%rtyp))
      return
    endselect

    selectcase( earth%shptyp )
    case( EARTH_SHPTYP__SPHERE )
      earth%finv = 0.d0
      earth%f    = 0.d0
      earth%e2   = 0.d0
    case( EARTH_SHPTYP__ELLIPS )
      earth%finv = EARTH_CONST__GRS80_FINV
      earth%f    = EARTH_CONST__GRS80_F
      earth%e2   = EARTH_CONST__GRS80_E2
    endselect
  !-------------------------------------------------------------
  ! Case: Geosys = other
  case( EARTH_GEOSYS__OTHER )
    if( n_r == 0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nRadius was not specified.'//&
                '\nExplicit specification of constants is required '//&
                  'when geodesic system is "'//str(earth%geosys)//'".')
      return
    endif
    if( n_finv == 0 .and. n_f == 0 .and. n_e2 == 0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nFlattening was not specified.'//&
                '\nExplicit specification of constants is required '//&
                  'when geodesic system is "'//str(earth%geosys)//'".')
      return
    endif

    ! Set flattening

    ! Case: Both finv and f were given
    if( n_finv > 0 .and. n_f > 0 )then
      call logwrn(msg_undesirable_condition()//&
                '\nBoth finv and f were specified.')

    ! Case: Only finv was given
    elseif( n_finv > 0 .and. n_f == 0 )then
      if( earth%finv == 0.d0 )then
        call logwrn(msg_undesirable_condition()//&
                  '\n  finv == 0'//&
                  '\nThis is interpreted as flattening f being 0.')
        earth%f = 0.d0
      else
        earth%f = 1.d0 / earth%finv
      endif

    ! Case: Only f was given
    elseif( n_f > 0 .and. n_finv == 0 )then
      if( earth%f == 0.d0 )then
        earth%finv = 0.d0
      else
        earth%finv = 1.d0 / earth%f
      endif

    ! Case: Neither finv or f was given
    elseif( n_finv == 0 .and. n_f == 0 )then
      if( earth%e2 == 0.d0 )then
        earth%finv = 0.d0
        earth%f    = 0.d0
      else
        earth%finv = (1.d0 + sqrt(1.d0 - earth%e2)) / earth%e2
        earth%f    = ((1-sqrt(1.d0-earth%e2))**2 + earth%e2) * 0.5d0
      endif
    endif

    ! Set eccentricity
    if( n_e2 == 0 )then
      earth%e2 = earth%f * (2.d0 - earth%f)
    endif

    ! Shape type
    if( earth%f == 0.d0 )then
      earth%shptyp = EARTH_SHPTYP__SPHERE
    else
      earth%shptyp = EARTH_SHPTYP__ELLIPS
    endif

    ! Radius type (not used)
    earth%rtyp = ''
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    info = 1
    call errret(msg_invalid_value('earth%geosys', earth%geosys))
    return
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function set_values_opt_earth
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_sys(sys)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt_sys'
  type(opt_sys_), intent(in) :: sys

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('System')
  call logmsg('  Treatment of old files: '//str(sys%old_files))
  call logmsg('  Output directory of intermediates: '//str(sys%dir_im))
  call logmsg('  Remove intermediates: '//str(sys%remove_im))

  if( sys%memory_ulim == 0.d0 )then
    call logmsg('  Upper limit of memory: (not limited)')
  else
    call logmsg('  Upper limit of memory: '//str(sys%memory_ulim)//' MB')
  endif
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt_sys
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_log(log)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt_log'
  type(opt_log_), intent(in) :: log

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Log')
  call logmsg('  Print summary: '//str(log%print_summary))
  call logmsg('  Write summary: '//str(log%write_summary))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt_log
!===============================================================
!
!===============================================================
subroutine echo_settings_opt_earth(earth)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_settings_opt_earth'
  type(opt_earth_), intent(in) :: earth

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('Earth')
  call logmsg('  Geodesic system: '//str(earth%geosys))
  call logmsg('  Shape          : '//str(earth%shptyp))
  selectcase( earth%shptyp )
  case( EARTH_SHPTYP__SPHERE )
    call logmsg('  Radius: '//str(earth%r,'es22.15')//' m')

  case( EARTH_SHPTYP__ELLIPS )
    call logmsg('  Semi-major axis       : '//str(earth%r,'es22.15')//' m')
    call logmsg('  Inverse of flattening : '//str(earth%finv,'es22.15'))
    call logmsg('  Flattening            : '//str(earth%f,'es22.15'))
    call logmsg('  Square of eccentricity: '//str(earth%e2,'es22.15'))
  case default
    call errend(msg_invalid_value('earth%shptyp', earth%shptyp))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_settings_opt_earth
!===============================================================
!
!===============================================================
end module c1_opt_set
