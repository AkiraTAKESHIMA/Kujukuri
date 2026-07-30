program main
  use lib_log
  use def_type
  use mod_utils, only: &
        remove_im
  use mod_set, only: &
        read_settings
  use mod_grid, only: &
        merge_grid_data
  use mod_main, only: &
        merge_rt
  use mod_finalize, only: &
        finalize
  implicit none
  type(input_)  :: input
  type(output_) :: output
  type(opt_)    :: opt

  call logbgn('program main', '', '+tr')
  !-------------------------------------------------------------
  call read_settings(input, output, opt)

  call merge_grid_data(input, output, opt)

  call merge_rt(input, output, opt)

  call remove_im(output%path_grid_im, opt%sys%remove_im)

  call finalize()
  !-------------------------------------------------------------
  call logret('program main', '')
end program main
