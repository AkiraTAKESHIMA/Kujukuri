program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  implicit none
  integer :: nx, ny
  logical :: kinematic, overwrite
  integer :: r
  character(32) :: method

  call logbgn('program main', '', '+tr')

  call addarg('nx', 0, 'Grid number in x-axis direction')
  call addarg('ny', 0, 'Grid number in y-axis direction')
  call addarg('output', '', 'Output directory')
  call addarg('-k', '--kinematic', .false., .false., 'Use kinematic mode')
  call addarg('', '--overwrite', .false., .false., 'Overwrite')
  call addarg('-r', '--iteration', 100, .true., 'Iteration limit')
  call addarg('-m', '--method', '1st_order_conservative', .false., 'Interpolation Method')

  call logmsg('------ Test showarg', opt='x0')
  call showarg()

  call logmsg('------ Test parsearg', opt='x0')
  call parsearg()

  nx = arg_int4('nx')
  ny = arg_int4('ny')
  kinematic = arg_flag('-k')
  overwrite = arg_flag('--overwrite')
  r = arg_int4('-r')
  method = arg_char('-m')

  call logmsg('nx '//str(nx))
  call logmsg('ny '//str(ny))
  call logmsg('kinematic '//str(kinematic))
  call logmsg('overwrite '//str(overwrite))
  call logmsg('r '//str(r))
  call logmsg('method '//str(method))

  call logret()
end program main
