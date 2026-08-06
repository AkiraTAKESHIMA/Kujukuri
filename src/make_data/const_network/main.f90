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
        separateNetworks, &
        makeModelNetworkData
  use mod_modify_channeldir, only: &
        modifyChannelDir
  use mod_modify_flwdir, only: &
        modifyFlwdir
  use mod_eval_basin, only: &
        calcNetworkBasinIntersections, &
        evalNetworkBasinConsistencies
  implicit none
  character(CLEN_KEY) :: task

  character(CLEN_KEY) :: region
  character(32) :: uid
  character(CLEN_KEY) :: resolution
  character(CLEN_KEY) :: resl_in, resl_out
  real(8) :: leng
  character(CLEN_PATH) :: name_leng

  character(CLEN_PROC), parameter :: PRCNAM = 'program const_river'

  call logbgn(PRCNAM, '', '+tr -p -x2')
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
    call parsearg(istart=2)
    uid = arg_char('-tmpuid')

    call separateNetworks(uid)

  case( 'makeModelNetworkData' )
    call addarg('name_leng', 's', 'Product name')
    call addarg('leng', 0.d0, 'Standard length of divided sections')
    call addarg('-uid', '', '', .false., 'Network id')
    call parsearg()
    name_leng = arg_char('name_leng')
    leng = arg_dble('leng')
    uid = arg_char('-uid')
  
    call makeModelNetworkData(name_leng, leng, uid)

  ! Main Step 2. Modify flow direction
  !-------------------------------------------------------------
  case( 'modifyChannelDir' )
    call addarg('-uid', '', '', .false., 'Network index')
    call parsearg(istart=2)
    uid = arg_char('-uid')

    call modifyChannelDir(uid)

  case( 'modifyFlwdir' )
    call addarg('resolution', 's', 'Resolution')
    call addarg('-uid', '', '', .false., 'Global index of network')
    call parsearg(istart=2)
    resolution = arg_char('resolution')
    uid = arg_char('-uid')

    call modifyFlwdir(trim(resolution), trim(uid))

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
    call parsearg(istart=2)
    region = arg_char('region')

    call evalWsCodeConsistency(region)

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
  !-------------------------------------------------------------
  call logret(PRCNAM, '')
end program main
