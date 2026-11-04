program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c2_strnk_const, only: &
    DGT_NWKID
  use mod_remap, only: &
    makeRemappingTables, &
    remap                   
  use mod_mesh, only: &
    calcMeshIntersections, &
    makeHiresNetworkMask , &
    scaleUp              , &
    scaleUpNetworkMask   , &
    trimCatm          
  use mod_topo, only: &
    modifyElv   , &
    modifyFlwDir, &
    calcUpperArea
  implicit none
  character(CLEN_KEY) :: task

  character(CLEN_KEY) :: resl
  character(DGT_NWKID) :: id
  character(CLEN_KEY) :: catmType
  character(CLEN_KEY) :: name_src
  character(CLEN_KEY) :: resl_src
  character(CLEN_KEY) :: var
  character(CLEN_KEY) :: outfmt
  logical :: modified
  logical :: overwrite

  character(CLEN_PROC), parameter :: PRCNAM = 'program make_mesh_data'


! 1-a-1 calcHiresMeshIntersections
!    -2 makeHiresNetworkMask
!    -3 scaleUpNetworkMask
!  -b-1 makeRemappingTables
!  -b-2 remap
! 2     trimCatm
! 3-1   modifyFlwDir
!  -2   modifyElv
!  -3   calcUpperArea
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

  ! Main 1.a.3
  !-------------------------------------------------------------
  case( 'scaleUpNetworkMask' )
    call addarg('resl', '', 'Target resolution')
    call parsearg()

    resl = lower(arg_char('resl'))

    call scaleUpNetworkMask(resl)

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

  ! Main 3.a
  !-------------------------------------------------------------
  case( 'modifyElv' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-id', '', '', .false., 'Catchment id')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl = lower(arg_char('resl'))
    id = arg_char('-id')

    call modifyElv(catmType, resl, id)

  ! Main 3.b
  !-------------------------------------------------------------
  case( 'modifyFlwDir' )
    call addarg('networkType', 's', 'Network type {network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-id', '', '', .false., 'Network id')
    call parsearg()

    catmType = lower(arg_char('networkType'))
    resl = lower(arg_char('resl'))
    id = arg_char('-id')

    call modifyFlwDir(catmType, resl, id)

  ! Main 4
  !-------------------------------------------------------------
  case( 'calcUpperArea' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-m', '--modified', .false., .false., 'Use modified topography')
    call addarg('-id', '', '', .false., 'Catchment id')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl = lower(arg_char('resl'))
    modified = arg_flag('--modified')
    id = arg_char('-id')

    call calcUpperArea(catmType, resl, modified, id)

  ! Main 5
  !-------------------------------------------------------------
  case( 'scaleUp' )
    call addarg('catmType', 's', 'Catchment type {basin,network,networkset}')
    call addarg('resl_in', 's', 'Input mesh resolution')
    call addarg('resl_out', 's', 'Output mesh resolution')
    call addarg('-m', '--modified', .false., .false., 'Use modified topography')
    call addarg('-id', '', '', .false., 'Catchment id')
    call parsearg()

    catmType = lower(arg_char('catmType'))
    resl_src = lower(arg_char('resl_in'))
    resl = lower(arg_char('resl_out'))
    modified = arg_flag('--modified')
    id = arg_char('-id')

    call scaleUp(catmType, resl_src, resl, modified, id)

  !
  !-------------------------------------------------------------
  case default
    call errend(msg_invalid_value('task', task))
  endselect
  !-------------------------------------------------------------
end program main
