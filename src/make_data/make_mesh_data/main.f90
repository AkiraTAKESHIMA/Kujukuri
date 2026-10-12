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
  use mod_intersection, only: &
    calcMeshIntersections
  use mod_mesh, only: &
    makeHiresNetworkMask, &
    scaleUpNetworkMask , &
    trimBasin          
  use mod_topo, only: &
    modifyFlwDir
  implicit none
  character(CLEN_KEY) :: task

  character(CLEN_KEY) :: resl
  character(DGT_NWKID) :: id
  character(CLEN_KEY) :: basinType
  character(CLEN_KEY) :: name_src
  character(CLEN_KEY) :: resl_src
  character(CLEN_KEY) :: var
  character(CLEN_KEY) :: outfmt
  logical :: overwrite

  character(CLEN_PROC), parameter :: PRCNAM = 'program make_mesh_data'


! 1-a-1 calcHiresMeshIntersections
!    -2 makeHiresNetworkMask
!    -3 scaleUpNetworkMask
!  -b-1 makeRemappingTables
!  -b-2 remap
! 2     trimBasin
! 3-1   modifyFlwDir
!  -2   
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
    overwrite = arg_flag('--overwrite')

    call remap(resl, name_src, resl_src, var, overwrite)

  ! Main 2
  !-------------------------------------------------------------
  case( 'trimBasin' )
    call addarg('basinType', 's', 'Type of basin {basin,network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('id', 's', 'Basin id')
    call addarg('var', 's', 'Variable name')
    call addarg('outfmt', 's', 'Output format {default,RRI}')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    basinType = lower(arg_char('basinType'))
    resl = lower(arg_char('resl'))
    id = arg_char('id')
    var = lower(arg_char('var'))
    outfmt = lower(arg_char('outfmt'))
    overwrite = arg_flag('--overwrite')

    call trimBasin(basinType, resl, id, var, outfmt, overwrite)

  ! Main 3
  !-------------------------------------------------------------
  case( 'modifyFlwDir' )
    call addarg('networkType', 's', 'Network type {network,networkset}')
    call addarg('resl', 's', 'Mesh resolution')
    call addarg('-id', '', '', .false., 'Network id')
    call parsearg()

    basinType = lower(arg_char('networkType'))
    resl = lower(arg_char('resl'))
    id = arg_char('-id')

    call modifyFlwDir(basinType, resl, id)

  !
  !-------------------------------------------------------------
  case default
    call errend(msg_invalid_value('task', task))
  endselect
  !-------------------------------------------------------------
end program main
