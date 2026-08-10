program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c2_strnk_const, only: &
        DGT_NWKUID
  use mod_make_mesh_data, only: &
        makeRemappingTables     , &
        remap                   , &
        rasterizeNetworks       , &
        make1secNetworkMask     , &
        make1secNetworkUpperArea, &
        scaleUpNetworkMask      , &
        trimBasin               
  implicit none
  character(CLEN_KEY) :: task

  character(CLEN_KEY) :: resl
  character(DGT_NWKUID) :: uid
  character(CLEN_KEY) :: basinType
  character(CLEN_KEY) :: name_src
  character(CLEN_KEY) :: resl_src
  character(CLEN_KEY) :: var
  logical :: overwrite

  character(CLEN_PROC), parameter :: PRCNAM = 'program make_mesh_data'
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
  case( 'rasterizeNetworks' )
    call addarg('-uid', '', '', .false., 'Network ID')
    call parsearg()

    uid = arg_char('-uid')

    call rasterizeNetworks(uid)

  ! Main 1.a.2
  !-------------------------------------------------------------
  case( 'make1secNetworkMask' )
    call addarg('-uid', '', 'all', .false., 'Network ID')
    call parsearg()

    uid = arg_char('-uid')

    call make1secNetworkMask(uid)

  ! Main 1.a.3
  !-------------------------------------------------------------
  case( 'make1secNetworkUpperArea' )
    call addarg('-uid', '', 'all', .false., 'Network ID')
    call parsearg()

    uid = arg_char('-uid')

    call make1secNetworkUpperArea(uid)

  ! Main 1.a.4
  !-------------------------------------------------------------
  case( 'scaleUpNetworkMask' )
    call addarg('resl', '', 'Target resolution')
    call parsearg()

    resl = arg_char('resl')

    call scaleUpNetworkMask(resl)

  ! Main 1.b.1
  !-------------------------------------------------------------
  case( 'makeRemappingTables' )
    call addarg('resl', 's', 'Resolution of J-FlwDir mesh')
    call addarg('name_src', 's', 'Name of source mesh')
    call addarg('resl_src', 's', 'Resolution of source mesh')
    call addarg('-w', '--overwrite', .false., .false., 'Overwrite existing output files')
    call parsearg()

    resl = arg_char('resl')
    name_src = arg_char('name_src')
    resl_src = arg_char('resl_src')
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

    resl = arg_char('resl')
    name_src = arg_char('name_src')
    resl_src = arg_char('resl_src')
    var = arg_char('var')
    overwrite = arg_flag('--overwrite')

    call remap(resl, name_src, resl_src, var, overwrite)

  ! Main 2
  !-------------------------------------------------------------
  case( 'trimBasin' )
    call addarg('basinType', 's', 'Type of J-FlwDir basin')
    call addarg('resl', 's', 'Resolution of J-FlwDir mesh')
    call addarg('var', 's', 'Variable')
    call addarg('uid', 's', 'Basin/Network/NetworkSet ID')
    call parsearg()

    basinType = arg_char('basinType')
    resl = arg_char('resl')
    var = arg_char('var')
    uid = arg_char('uid')

    call trimBasin(basinType, resl, var, uid)

  !
  !-------------------------------------------------------------
  case default
    call errend(msg_invalid_value('task', task))
  endselect
  !-------------------------------------------------------------
end program main
