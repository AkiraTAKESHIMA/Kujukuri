program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use mod_make_model_data, only: &
        makeModelNetworkData
        !makeModelCrossSectionData
  implicit none
  character(CLEN_KEY) :: task

  character(CLEN_PATH) :: productName
  real(8) :: leng
  character(32) :: uid

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

  ! Main Step 1. Make model network data
  !-------------------------------------------------------------
  case( 'makeModelNetworkData' )
    call addarg('productName', 's', 'Product name')
    call addarg('leng', 0.d0, 'Standard length of divided sections')
    call addarg('-uid', '', '', .false., 'Network id')
    call parsearg()

    productName = arg_char('productName')
    leng = arg_dble('leng')
    uid = arg_char('-uid')

    call makeModelNetworkData(productName, leng, uid)

  endselect
end program main
