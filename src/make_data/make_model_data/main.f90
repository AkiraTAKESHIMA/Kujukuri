program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use mod_make_model_data, only: &
        makeModelNetworkData, &
        makeModelCrossSectionData
  implicit none
  character(CLEN_KEY) :: task

  character(CLEN_PATH) :: name_network
  character(CLEN_PATH) :: name_crssct
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

    name_network = arg_char('productName')
    leng = arg_dble('leng')
    uid = arg_char('-uid')

    call makeModelNetworkData(name_network, leng, uid)

  !
  !-------------------------------------------------------------
  case( 'makeModelCrossSectionData' )
    call addarg('name_network', 's', 'Product name of the model network data')
    call addarg('name_crosssection', 's', 'Product name of the cross section data')
    call addarg('-uid', '', '', .false., 'Network id')
    call parsearg()

    name_network = arg_char('name_network')
    name_crssct = arg_char('name_crosssection')
    uid = arg_char('-uid')

    call makeModelCrossSectionData(name_network, name_crssct, uid)

  ! 
  !-------------------------------------------------------------
  case default
    call errend(msg_invalid_value('task', task))
  endselect
end program main
