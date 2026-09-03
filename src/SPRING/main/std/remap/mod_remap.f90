module mod_remap
  use lib_const
  use lib_base
  use lib_time
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_opt
  use c1_type_gs
  use c1_type_timer
  use c1_timer, only: &
        start_ctimer, &
        stop_ctimer
  use c2_type_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: remap
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_remap'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine remap(s, t, rt)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'remap'
  type(gs_), intent(inout), target :: s, t
  type(rt_), intent(inout), target :: rt

  type(gs_common_)    , pointer :: sc, tc
  type(file_grid_in_) , pointer :: sfg
  type(file_grid_out_), pointer :: tfg
  type(grid_)         , pointer :: sg, tg
  type(file_)         , pointer :: sf, tf
  type(rt_main_)      , pointer :: rtm
  real(8), pointer :: sval(:), sval_2d(:,:)
  real(8), pointer :: tval(:), tval_2d(:,:)
  integer(1), pointer :: tval_mask(:)
  real(8) :: sval_miss
  integer :: iFile
  integer(8) :: sij, tij
  integer(8) :: rtij
  integer(8) :: loc
  logical :: s_is_south_to_north, t_is_south_to_north

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Setup
  !-------------------------------------------------------------
  sc => s%cmn
  tc => t%cmn
  sfg => sc%f_grid_in
  tfg => tc%f_grid_out
  sg => sc%grid
  tg => tc%grid

  rtm => rt%main

  if( sfg%nFiles_val == 0 )then
    call logret(PRCNAM, MODNAM)
    return
  endif

  call get_is_south_to_north(s, s_is_south_to_north)
  call get_is_south_to_north(t, t_is_south_to_north)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( rtm%mode )
  !-------------------------------------------------------------
  ! Case: 1st order conservative
  case( REMAP_MODE_1ST_ORDER_CONSERVATIVE )
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call start_ctimer('buffer')

    nullify(sval, sval_2d, tval, tval_2d, tval_mask)

    call alloc(sc, sfg%lb(:2), sfg%ub(:2), sval, sval_2d)
    call alloc(tc, tfg%lb(:2), tfg%ub(:2), tval, tval_2d)

    allocate(tval_mask(size(tval)))

    call stop_ctimer('buffer')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iFile = 1, sfg%nFiles_val
      sf => sfg%val(iFile)
      tf => tfg%val(iFile)
      call logmsg('In : '//str(fileinfo(sf)))
      call logmsg('Out: '//str(fileinfo(tf)))

      selectcase( sf%dtype )
      case( DTYPE_REAL )
        sval_miss = real(sfg%val_miss,4)  ! dble -> real -> dble
      case default
        sval_miss = real(sfg%val_miss,8)
      endselect
      !---------------------------------------------------------
      ! Read input
      !---------------------------------------------------------
      call start_ctimer('io')

      selectcase( sc%typ )
      case( MESHTYPE__LATLON, MESHTYPE__RASTER )
        call traperr( rbin( &
          sval_2d, sf%path, sf%dtype, sf%endian, sf%rec, &
          sz=sfg%sz(:2), lb=sfg%lb(:2), check_recl=.true. &
        ) )

        if( .not. s_is_south_to_north ) call reverse(sval_2d, 2)

        sval = reshape(sval_2d, (/size(sval)/))
      case( MESHTYPE__POLYGON )
        call traperr( rbin( &
          sval, sf%path, sf%dtype, sf%endian, sf%rec, &
          sz=sfg%sz(2), lb=sfg%lb(2), check_recl=.true. &
        ) )
      case default
        call errend(msg_invalid_value('sc%typ', sc%typ))
      endselect

      call stop_ctimer('io')
      !---------------------------------------------------------
      ! Interpolate
      !---------------------------------------------------------
      call start_ctimer('interpolate')

      tval(:) = 0.d0
      tval_mask(:) = 0_1

      do rtij = 1_8, rtm%nij
        call search(rtm%sidx(rtij), sg%idx, sg%idxarg, loc)
        if( loc == 0_8 )then
          call errend(msg_unexpected_condition()//&
                    '\nIndex '//str(rtm%sidx(rtij))//' of the source mesh is invalid.')
        endif
        sij = sg%idxarg(loc)

        call search(rtm%tidx(rtij), tg%idx, tg%idxarg, loc)
        if( loc == 0_8 )then
          call errend(msg_unexpected_condition()//&
                    '\nIndex '//str(rtm%tidx(rtij))//' of the target mesh is invalid.')
        endif
        tij = tg%idxarg(loc)

        if( sval(sij) == sval_miss ) cycle

        tval(tij) = tval(tij) + sval(sij) * rtm%coef(rtij)
        tval_mask(tij) = 1_1
      enddo  ! rtij/

      do tij = 1_8, tfg%nij
        if( tval_mask(tij) == 0_1 )then
          tval(tij) = tfg%val_miss
        endif
      enddo

      call stop_ctimer('interpolate')
      !---------------------------------------------------------
      ! Write output
      !---------------------------------------------------------
      call start_ctimer('io')

      selectcase( tc%typ )
      case( MESHTYPE__LATLON, MESHTYPE__RASTER )
        tval_2d = reshape(tval,(/tfg%nx,tfg%ny/))

        if( .not. t_is_south_to_north ) call reverse(tval_2d, 2)

        !call traperr( wbin( &
        !  tval_2d, tf%path, tf%dtype, tf%endian, tf%rec, &
        !  sz=tfg%sz(:2), lb=tfg%lb(:2) &
        !) )
        call traperr( wbin( &
          tval_2d, tf%path, tf%dtype, tf%endian, tf%rec &
        ) )

      case( MESHTYPE__POLYGON )
        !call traperr( wbin( &
        !  tval, tf%path, tf%dtype, tf%endian, tf%rec, &
        !  sz=tfg%sz(2), lb=tfg%lb(2) &
        !) )
        call traperr( wbin( &
          tval, tf%path, tf%dtype, tf%endian, tf%rec &
        ) )

      case default
        call errend(msg_invalid_value('tc%typ', tc%typ))
      endselect

      call stop_ctimer('io')
    enddo  ! iFile/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call realloc(sval, 0)
    call realloc(sval_2d, 0)
    call realloc(tval, 0)
    call realloc(tval_2d, 0)
    call realloc(tval_mask, 0)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('rtm%mode', rtm%mode))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine remap
!===============================================================
!
!===============================================================
subroutine get_is_south_to_north(a, is_south_to_north)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_is_south_to_north'
  type(gs_), intent(in) :: a
  logical, intent(out) :: is_south_to_north

  selectcase( a%cmn%typ )
  case( MESHTYPE__LATLON )
    is_south_to_north = a%latlon%is_south_to_north
  case( MESHTYPE__RASTER )
    is_south_to_north = a%raster%is_south_to_north
  case( MESHTYPE__POLYGON )
    is_south_to_north = .true.  ! not used
  case default
    call errend(msg_invalid_value('a%cmn%typ', a%cmn%typ), &
      '', PRCNAM, MODNAM)
  endselect
end subroutine get_is_south_to_north
!===============================================================
!
!===============================================================
subroutine alloc(ac, lb, ub, val, val_2d)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'alloc'
  type(gs_common_), intent(in) :: ac
  integer(8), intent(in) :: lb(:), ub(:)
  real(8), pointer :: val(:), val_2d(:,:)

  selectcase( ac%typ )
  case( MESHTYPE__LATLON, MESHTYPE__RASTER )
    allocate(val_2d(lb(1):ub(1),lb(2):ub(2)))
    allocate(val(size(val_2d)))
  case( MESHTYPE__POLYGON )
    allocate(val(lb(2):ub(2)))
  case default
    call errend(msg_invalid_value('ac%typ', ac%typ), &
      '', PRCNAM, MODNAM)
  endselect
end subroutine alloc
!===============================================================
!
!===============================================================
end module mod_remap
