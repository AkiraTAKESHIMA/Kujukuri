program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c1_const
  use c2_strnk_const, only: &
    DGT_NWKID
  use mod_remap, only: &
    makeRemappingTables, &
    remap                   
  use mod_mesh, only: &
    calcMeshIntersections, &
    makeHiresNetworkMask , &
    trimCatm          
  use mod_fitflwdir, only: &
    fitFlwDirToRiverNetwork
  use mod_topo, only: &
    calcUpperArea           , &
    removeRiverChannelEffect, &
    fillDepressions         , &
    scaleUp              
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'program make_mesh_data'

  character(CLEN_KEY) :: task

  character(CLEN_KEY) :: resl
  character(DGT_NWKID) :: id
  character(CLEN_KEY) :: catmType
  character(CLEN_KEY) :: name_src
  character(CLEN_KEY) :: resl_src
  character(CLEN_KEY) :: var
  integer             :: modification
  character(CLEN_KEY) :: outfmt
  logical :: overwrite
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! 1.a StrRank流域マスクを作成する
  !   1. calcHiresMeshIntersections
  !   2. makeHiresNetworkMask
  ! 1.b 地表面データの解像度を変換する
  !   1. makeRemappingTables
  !   2. remap
  ! 2. 流域を切り出す
  !   1. trimCatm
  ! 3. 流域地形を補正する
  !   1.a. removeRiverChannelEffect
  !   1.b. fitFlwDirToRiverNetwork
  !   2.   fillDepressions
  ! 4. 上流域面積を計算する
  ! 5. 流域の解像度を変換する


  ! 3.1.a removeRiverChannelEffect
  !     b firFlwDirToRiverNetwork
  !  .2   fillDepressions
  !  .3   calcUpperArea
  !  .4   scaleUp
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

  ! Main 1.a.1
  !-------------------------------------------------------------
  case( 'calcMeshIntersections' )
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-id', '', '', .false., 'Network id')
    call parsearg()

    resl = lower(arg_char('resl'))
    id = arg_char('-id')

    call calcMeshIntersections(resl, id)

  ! Main 1.a.2
  !-------------------------------------------------------------
  case( 'makeHiresNetworkMask' )
    call addarg('-id', '', '', .false., 'Network id')
    call parsearg()

    id = arg_char('-id')

    call makeHiresNetworkMask(id)

  ! Main 1.b.1
  !-------------------------------------------------------------
  case( 'makeRemappingTables' )
    call addarg('resl', 's', 'Resolution of J-FlwDir mesh')
    call addarg('name_src', 's', 'Name of source mesh')
    call addarg('resl_src', 's', 'Resolution of source mesh')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    resl = lower(arg_char('resl'))
    name_src = arg_char('name_src')
    resl_src = lower(arg_char('resl_src'))
    overwrite = arg_flag('--overwrite')

    call makeRemappingTables(resl, name_src, resl_src, overwrite)

  ! Main 1.b.2
  !-------------------------------------------------------------
  case( 'remap' )
    call addarg('resl', 's', 'Resolution of J-FlwDir mesh')
    call addarg('name_src', 's', 'Name of source mesh')
    call addarg('resl_src', 's', 'Resolution of source mesh')
    call addarg('var', 's', 'Variable')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    resl = lower(arg_char('resl'))
    name_src = arg_char('name_src')
    resl_src = lower(arg_char('resl_src'))
    var = lower(arg_char('var'))
    overwrite = arg_flag('-w')

    call remap(resl, name_src, resl_src, var, overwrite)

  ! Main 2
  !-------------------------------------------------------------
  case( 'trimCatm' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('id', 's', 'Catchment id')
    call addarg('var', 's', 'Comma-separated variable names')
    call addarg('-o', '--output-format', 'default', .false., 'Comma-separated list of output formats {default,RRI}')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl = lower(arg_char('resl'))
    id = arg_char('id')
    var = lower(arg_char('var'))
    outfmt = lower(arg_char('-o'))
    overwrite = arg_flag('-w')

    call trimCatm(catmType, resl, id, var, outfmt, overwrite)

  ! Main 3.1
  !-------------------------------------------------------------
  case( 'fitFlwDirToRiverNetwork' )
    call addarg('networkType', 's', 'Network type {network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-m', '--modified', MODE_TOPOMODIFICATION__AUTO, .false., 'Select modification mode')
    call addarg('-id', '', '', .false., 'Network id')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    catmType = lower(arg_char('networkType'))
    resl = lower(arg_char('resl'))
    modification = arg_int4('-m')
    id = arg_char('-id')
    overwrite = arg_flag('-w')

    call fitFlwDirToRiverNetwork(catmType, resl, modification, id, overwrite)

  ! Main 3.2
  !-------------------------------------------------------------
  case( 'calcUpperArea' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-m', '--modified', MODE_TOPOMODIFICATION__AUTO, .false., 'Select modification mode')
    call addarg('-id', '', '', .false., 'Catchment id')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl = lower(arg_char('resl'))
    modification = arg_int4('-m')
    id = arg_char('-id')
    overwrite = arg_flag('-w')

    call calcUpperArea(catmType, resl, modification, id, overwrite)

  ! Main 3.3
  !-------------------------------------------------------------
  case( 'removeRiverChannelEffect' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-m', '--modified', MODE_TOPOMODIFICATION__AUTO, .false., 'Select modification mode')
    call addarg('-id', '', '', .false., 'Catchment id')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl = lower(arg_char('resl'))
    modification = arg_int4('-m')
    id = arg_char('-id')
    overwrite = arg_flag('-w')

    call removeRiverChannelEffect(catmType, resl, modification, id, overwrite)

  ! Main 3.4
  !-------------------------------------------------------------
  case( 'fillDepressions' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-m', '--modified', MODE_TOPOMODIFICATION__AUTO, .false., 'Select modification mode')
    call addarg('-id', '', '', .false., 'Catchment id')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl = lower(arg_char('resl'))
    modification = arg_int4('-m')
    id = arg_char('-id')
    overwrite = arg_flag('-w')

    call fillDepressions(catmType, resl, modification, id, overwrite)

  ! Main 4
  !-------------------------------------------------------------
  case( 'scaleUp' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl_in', 's', 'Input mesh resolution')
    call addarg('resl_out', 's', 'Output mesh resolution')
    call addarg('-m', '--modified', MODE_TOPOMODIFICATION__AUTO, .false., 'Select modification mode')
    call addarg('-id', '', '', .false., 'Catchment id')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl_src = lower(arg_char('resl_in'))
    resl = lower(arg_char('resl_out'))
    modification = arg_int4('-m')
    id = arg_char('-id')
    overwrite = arg_flag('-w')

    call scaleUp(catmType, resl_src, resl, modification, id, overwrite)

  !
  !-------------------------------------------------------------
  case default
    call errend(ERROR_CODE__VALUE, 'task == '//str(task))
  endselect
end program main
