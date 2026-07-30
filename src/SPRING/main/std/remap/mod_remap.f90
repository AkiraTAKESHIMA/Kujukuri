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

  type(gs_common_)    , pointer :: sgc, tgc
  type(file_grid_in_) , pointer :: sfg
  type(file_grid_out_), pointer :: tfg
  type(grid_)         , pointer :: sg, tg
  type(file_)         , pointer :: sf, tf
  type(rt_main_)      , pointer :: rtm
  real(8), allocatable :: sval(:), tval(:)
  real(8), allocatable :: sval_2d(:,:), tval_2d(:,:)
  integer(1), allocatable :: tval_mask(:)
  integer :: iFile
  integer(8) :: sij, tij
  integer(8) :: rtij
  integer(8) :: loc

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sgc => s%cmn
  tgc => t%cmn
  sfg => sgc%f_grid_in
  tfg => tgc%f_grid_out
  sg => sgc%grid
  tg => tgc%grid

  rtm => rt%main

  if( sfg%nFiles_val == 0 )then
    call logret(PRCNAM, MODNAM)
    return
  endif
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

    allocate(sval(sfg%nij))
    allocate(tval(tfg%nij))
    allocate(tval_mask(tfg%nij))

    allocate(sval_2d(sfg%nx,sfg%ny))
    allocate(tval_2d(tfg%nx,tfg%ny))

    call stop_ctimer('buffer')
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    do iFile = 1, sfg%nFiles_val
      sf => sfg%val(iFile)
      tf => tfg%val(iFile)
      call logmsg('In : '//str(fileinfo(sf)))
      call logmsg('Out: '//str(fileinfo(tf)))
      !---------------------------------------------------------
      ! Read input
      !---------------------------------------------------------
      call start_ctimer('io')

      selectcase( sfg%ny )
      case( 1_8 )
        call traperr( rbin(&
               sval, sf%path, sf%dtype, sf%endian, sf%rec, &
               sz=sfg%sz(1), lb=sfg%lb(1), check_recl=.true.) )
      case( 2_8: )
        call traperr( rbin(&
               sval, sfg%nx, sfg%ny, &
               sf%path, sf%dtype, sf%endian, sf%rec, &
               sz=sfg%sz(:2), lb=sfg%lb(:2), check_recl=.true.) )
      case( :0_8 )
        call errend(msg_invalid_value('sfg%ny', sfg%ny))
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

      tval_2d = reshape(tval,(/tfg%nx,tfg%ny/))

      call traperr( wbin(&
             tval_2d, tf%path, tf%dtype, tf%endian, tf%rec, &
             sz=tf%sz(:2), lb=tf%lb(:2)) )

      call stop_ctimer('io')
    enddo  ! iFile/
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(sval_2d)
    deallocate(tval_2d)

    deallocate(sval)
    deallocate(tval)
    deallocate(tval_mask)
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
end module mod_remap
