program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use mod_calc_watsys_corresp, only: &
    calcWatsysAreas           , &
    makeWsCodeRemappingTables , &
    mergeWsCodeRemappingTables, &
    calcWatsysCorresp
  use mod_make_handy_data, only: &
    makeWsCodeMasks       , &
    divideStrrankIntoTiles
  use mod_eval_wscode, only: &
    evalWsCodeConsistency
  use mod_connect_channels, only: &
    connectChannels    , &
    postConnectChannels
  use mod_separate_networks, only: &
    separateNetworks        , &
    makeSeparatedNetworkData
  use mod_calc_network_mesh_intersections, only: &
    calcNetworkMeshIntersections
  use mod_eval_basin, only: &
    calcNetworkBasinIntersections, &
    evalNetworkBasinConsistencies
  implicit none
  character(CLEN_KEY) :: task

  character(CLEN_KEY) :: region
  character(32) :: uid

  character(CLEN_PROC), parameter :: PRCNAM = 'program const_river'

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

  ! Main Step 1. Reconstruct networks
  !-------------------------------------------------------------
  case( 'connectChannels' )
    call connectChannels()

  case( 'postConnectChannels' )
    call postConnectChannels()

  case( 'separateNetworks' )
    call addarg('-tmpuid', '', '', .false., 'Temporal network id')
    call parsearg()

    uid = arg_char('-tmpuid')

    call separateNetworks(uid)

  case( 'makeSeparatedNetworkData' )
    call addarg('-uid', '', '', .false., 'Network id')
    call parsearg()

    uid = arg_char('-uid')

    call makeSeparatedNetworkData(uid)

  ! Sub Step 1. Finding correspondent NLNI water systems for 
  !   each J-FlwDir basin
  !-------------------------------------------------------------
  case( 'calcWatsysAreas' )
    call calcWatsysAreas()

  case( 'makeWsCodeRemappingTables' )
    call makeWsCodeRemappingTables()

  case( 'mergeWsCodeRemappingTables' )
    call mergeWsCodeRemappingTables()

  case( 'calcWatsysCorresp' )
    call calcWatsysCorresp()

  ! Sub Step 2. Evaluate consistency between StrRank wsCode
  !   and wsCode mesh of NLNI
  !-------------------------------------------------------------
  case( 'makeWsCodeMasks' )
    call makeWsCodeMasks()

  case( 'divideStrrankIntoTiles' )
    call divideStrrankIntoTiles()

  case( 'evalWsCodeConsistency' )
    call addarg('region', 's', 'Region name ["Hokkaido", "Honshu", "Shikoku", "Kyushu", "Okinawa"]')
    call parsearg()

    region = arg_char('region')

    call evalWsCodeConsistency(region)

  !
  !-------------------------------------------------------------
  case( 'calcNetworkMeshIntersections' )
    call addarg('-uid', '', '', .false., 'Network id')
    call parsearg()

    uid = arg_char('-uid')

    call calcNetworkMeshIntersections(uid)

  ! Sub Step 3. Calc. relations of Strrank channels and 
  !   FlwDir basins
  !-------------------------------------------------------------
  case( 'calcNetworkBasinIntersections' )
    call calcNetworkBasinIntersections()

  case( 'evalNetworkBasinConsistencies' )
    call evalNetworkBasinConsistencies()

  ! ERROR
  !-------------------------------------------------------------
  case default
    call errend(msg_invalid_value('task', task))
  endselect
end program main
