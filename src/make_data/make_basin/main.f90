program main
  use lib_base
  use lib_log
  use lib_io
  use lib_util
  use c2_jflw_const, &
        set_resolution => set_resolution
  use mod_make_basin, only: &
        findRiverEnds      , &
        setBasinIds        , &
        getBasinInTile     , &
        getUpperBasin      , &
        updateUpperBasin   , &
        mergeAllBasins     , &
        makeTiledBasinMaps , &
        checkBasins        , &
        renewBasinList     , &
        makeNewBasinMaps   , &
        makeTiledBasinLists, &
        makeLowresBasinMaps, &
        makeBasinDomainData, &
        makeBasinTopoMap
  implicit none
  character(CLEN_VAR) :: task
  integer :: tx, ty
  integer :: bsnId_min, bsnId_max, bsnId
  character(CLEN_VAR) :: var

  character(CLEN_PROC), parameter :: PRCNAM = 'program make_basin'

  call logbgn(PRCNAM, '', '+tr')
  !-------------------------------------------------------------
  ! Read arguments
  !-------------------------------------------------------------
  call addarg('task', '', 'task')

  call parsearg(iend=1)

  task = arg_char('task')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call set_resolution('1sec')
  !-------------------------------------------------------------
  ! Run
  !-------------------------------------------------------------
  selectcase( task )
  !-------------------------------------------------------------
  ! Process tiled data to make basin maps
  !-------------------------------------------------------------
  ! Step 1.1
  case( 'findRiverEnds' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call findRiverEnds(tx, ty)

  ! Step 1.2
  case( 'setBasinIds' )
    call parsearg(istart=2)
    call setBasinIds()

  ! Step 1.3
  case( 'getBasinInTile' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call getBasinInTile(tx, ty)

  ! Step 1.4
  case( 'getUpperBasin' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call getUpperBasin(tx, ty)

  ! Step S1 (debugging before step 1.5)
  case( 'updateUpperBasin' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call updateUpperBasin(tx, ty)

  ! Step 1.5
  case( 'mergeAllBasins' )
    call mergeAllBasins()

  ! Step 1.6
  case( 'makeTiledBasinMaps' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call makeTiledBasinMaps(tx, ty)

  ! Step S2
  case( 'checkBasins' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call checkBasins(tx, ty)

  ! Step 1.7
  case( 'renewBasinList' )
    call parsearg(istart=2)

    call renewBasinList()

  ! Step 1.8
  case( 'makeNewBasinMaps' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call makeNewBasinMaps(tx, ty)

  ! Step 1.9
  case( 'makeTiledBasinLists' )
    call parsearg(istart=2)

    call makeTiledBasinLists()

  ! Step S3 (for drawing figures)
  case( 'makeLowresBasinMaps' )
    call addarg('tx', 0, '1 - 27')
    call addarg('ty', 0, '1 - 22')
    call parsearg(istart=2)
    tx = arg_int4('tx')
    ty = arg_int4('ty')

    call makeLowresBasinMaps(tx, ty)
  !-------------------------------------------------------------
  ! Make data of each basin
  !-------------------------------------------------------------
  ! Step 2.1
  case( 'makeBasinDomainData' )
    call addarg('bsnId_min', 0, '(description)')
    call addarg('bsnId_max', 0, '(description)')
    call parsearg(istart=2)
    bsnId_min = arg_int4('bsnId_min')
    bsnId_max = arg_int4('bsnId_max')

    call makeBasinDomainData(bsnId_min, bsnId_max)

  ! Step 2.2
  case( 'makeBasinTopoMap' )
    call addarg('var', '', '(description)')
    call addarg('bsnId', 0, '(description)')
    call parsearg(istart=2)
    var   = arg_char('var')
    bsnId = arg_int4('bsnId')

    call makeBasinTopoMap(var, bsnId)
  !-------------------------------------------------------------
  ! ERROR
  !-------------------------------------------------------------
  case default
    call errend(msg_invalid_value('task', task))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, '')
end program main
