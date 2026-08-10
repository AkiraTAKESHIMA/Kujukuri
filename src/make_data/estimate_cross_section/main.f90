program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use mod_estimate_cross_section, only: &
        determineChannelScale, &
        estimateCrossSection
  implicit none
  character(CLEN_KEY) :: task

  character(32) :: uid
  character(CLEN_PATH) :: f_conf
  logical :: overwrite

  !-------------------------------------------------------------
  ! Read arguments
  !-------------------------------------------------------------
  call addarg('task', 's', '')

  call parsearg(iend=1)

  task = arg_char('task')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( task )

  !
  !-------------------------------------------------------------
  case( 'determineChannelScale' )
    call addarg('-uid', '', '', .false., 'Network id')
    call parsearg()

    uid = arg_char('-uid')

    call determineChannelScale(uid)


  ! Main Step 2. Estimate Rectangular Cross Section Shapes
  !-------------------------------------------------------------
  case( 'estimateCrossSection' )
    call addarg('f_conf', 's', 'Configuration file')
    call addarg('-uid', '', '', .false., 'Network id')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite')
    call parsearg()

    f_conf = arg_char('f_conf')
    uid = arg_char('-uid')
    overwrite = arg_flag('-w')

    call estimateCrossSection(f_conf, uid, overwrite)

  endselect
end program main
