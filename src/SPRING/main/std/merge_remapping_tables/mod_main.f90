module mod_main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_opt
  use def_type
  implicit none
  character(CLEN_PROC), parameter :: MODNAM = 'mod_main'
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: merge_rt
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer, parameter :: stat_target_all  = 0
  integer, parameter :: stat_target_part = 1
  integer, parameter :: stat_target_none = -1
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine merge_rt(input, output, opt)
  use c1_file, only: &
        report
  use c2_type_rt
  use c2_rt_main_util, only: &
        merge_elems_same_index, &
        sort_rt
  use c2_rt_main_coef, only: &
        calc_rt_coef_sum_modify_enabled    , &
        calc_rt_coef_sum_modify_not_enabled
  use c2_rt_stats, only: &
        get_rt_main_stats     , &
        report_rt_main_summary
  use mod_utils, only: &
        open_file_grid_im , &
        close_file_grid_im
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'merge_rt'
  type(input_) , intent(inout)         :: input
  type(output_), intent(inout), target :: output
  type(opt_)   , intent(in)            :: opt

  type(f_rt_), pointer :: f_rt
  type(file_), pointer :: f

  type(rt_)     , pointer :: rt
  type(rt_main_), pointer :: rtm
  integer(8), allocatable :: grdidx(:)
  integer(8), allocatable :: grdidxarg(:)
  real(8)   , allocatable :: grdara(:)

  integer    :: iFile_rt
  integer(8) :: nmax_ulim
  integer(8) :: nmax_all, nmax
  integer(8) :: ije
  integer(8) :: idxmin, idxmax

  integer :: un_grid_im

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  rt => output%rt
  rtm => rt%main
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( opt%sys%memory_ulim == 0.d0 )then
    nmax_ulim = 0_8
  else
    call errend(str(msg_not_implemented()))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  ! Case: Memory is not limited
  if( nmax_ulim == 0_8 )then
    !-----------------------------------------------------------
    ! Read remapping tables
    !-----------------------------------------------------------
    call logent('Reading remapping tables', PRCNAM, MODNAM)

    rtm%nij = sum(input%list_f_rt(:)%nij)
    rtm%ijsize = rtm%nij
    call logmsg('Total length: '//str(rtm%nij))
    allocate(rtm%sidx(rtm%ijsize))
    allocate(rtm%tidx(rtm%ijsize))
    allocate(rtm%area(rtm%ijsize))
    nullify(rtm%coef)

    ije = 0_8
    do iFile_rt = 1, input%nFiles_rt
      f_rt => input%list_f_rt(iFile_rt)
      call logent('File '//str(iFile_rt, dgt(input%nFiles_rt))//' / '//&
                  str(input%nFiles_rt))
      call logmsg('Length: '//str(f_rt%nij,dgt(rtm%nij))//&
                  ' ('//str((/ije+1_8,ije+f_rt%nij/),dgt(rtm%nij),' ~ ')//')')

      f => f_rt%f_sidx
      call logmsg('Read '//str(fileinfo(f)))
      call traperr( rbin(rtm%sidx(ije+1_8:ije+f_rt%nij), f%path, f%dtype, f%endian, f%rec) )

      f => f_rt%f_tidx
      call logmsg('Read '//str(fileinfo(f)))
      call traperr( rbin(rtm%tidx(ije+1_8:ije+f_rt%nij), f%path, f%dtype, f%endian, f%rec) )

      f => f_rt%f_area
      call logmsg('Read '//str(fileinfo(f)))
      call traperr( rbin(rtm%area(ije+1_8:ije+f_rt%nij), f%path, f%dtype, f%endian, f%rec) )

      call traperr( get_rt_main_stats(rtm, ije+1_8, ije+f_rt%nij) )

      call add(ije, f_rt%nij)

      call logext()
    enddo  ! iFile_rt

    call logext()
    !-----------------------------------------------------------
    ! Merge the elements that have same pair of indices
    !-----------------------------------------------------------
    call logent('Merging the elements that have same pair of indices', PRCNAM, MODNAM)

    call traperr( merge_elems_same_index(&
           rtm%mesh_sort, rtm%ijsize, rtm%nij, rtm%sidx, rtm%tidx, rtm%area) )
    allocate(rtm%coef(rtm%ijsize))

    call logext()
    !-----------------------------------------------------------
    ! Calc. coef.
    !-----------------------------------------------------------
    call logent('Calculating coef.', PRCNAM, MODNAM)

    selectcase( rtm%mesh_coef )
    !-----------------------------------------------------------
    !
    case( MESH__SOURCE, &
          MESH__TARGET )
      !---------------------------------------------------------
      ! Case: Sum. of coef. is modified
      if( rtm%opt_coef%is_sum_modify_enabled )then
        call logent('Case: coef_sum_modify is enabled', PRCNAM, MODNAM)
        !-------------------------------------------------------
        call logent('Calculating coef.', PRCNAM, MODNAM)

        call traperr( calc_rt_coef_sum_modify_enabled(rtm) )

        call logext()
        !-------------------------------------------------------
        call logext()
      !---------------------------------------------------------
      ! Case: Sum. of coef. is not modified
      else
        call logent('Case: coef_sum_modify is not enabled', PRCNAM, MODNAM)
        !-------------------------------------------------------
        ! Read grid data
        !-------------------------------------------------------
        call logent('Reading grid data', PRCNAM, MODNAM)

        allocate(grdidx(output%f_grid%nmax))
        allocate(grdidxarg(output%f_grid%nmax))
        allocate(grdara(output%f_grid%nmax))

        if( output%f_grid%f_idx%path /= '' )then
          f => output%f_grid%f_idx
          call logmsg('Reading '//str(fileinfo(f)))
          call traperr( rbin(grdidx, f%path, f%dtype, f%endian, f%rec) )

          f => output%f_grid%f_ara
          call logmsg('Reading '//str(fileinfo(f)))
          call traperr( rbin(grdara, f%path, f%dtype, f%endian, f%rec) )
        else
          call open_file_grid_im(output%path_grid_im, action_read, un_grid_im)

          nmax_all = 0_8
          do while( nmax_all < output%f_grid%nmax )
            read(un_grid_im) nmax, idxmin, idxmax
            read(un_grid_im) grdidx(nmax_all+1_8:nmax_all+nmax)
            read(un_grid_im) grdara(nmax_all+1_8:nmax_all+nmax)
            call add(nmax_all, nmax)
          enddo

          call close_file_grid_im()
        endif

        call argsort(grdidx, grdidxarg)

        call logext()
        !-------------------------------------------------------
        ! Calc. coef.
        !-------------------------------------------------------
        call logent('Calculating coef.', PRCNAM, MODNAM)

        call traperr( calc_rt_coef_sum_modify_not_enabled(&
               rtm, grdidx, grdidxarg, grdara) )

        deallocate(grdidx)
        deallocate(grdidxarg)
        deallocate(grdara)

        call logext()
       !-------------------------------------------------------
       call logext()
       !-------------------------------------------------------
     endif
    !-----------------------------------------------------------
    !
    case( MESH__NONE )
      call realloc(rtm%coef, rtm%ijsize, clear=.true.)
      rtm%coef(:) = rtm%area(:)
    !-----------------------------------------------------------
    !
    case default
      call errend(msg_invalid_value('rtm%mesh_coef', rtm%mesh_coef))
    endselect

    call logext()
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call traperr( sort_rt(rtm) )
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call traperr( get_rt_main_stats(rtm) )

    call traperr( report_rt_main_summary(rtm, .true., .true.) )
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call logent('Outputting', PRCNAM, MODNAM)

    f => rtm%f%sidx
    call logmsg('Writing '//str(fileinfo(f)))
    call traperr( wbin(rtm%sidx, f%path, f%dtype, f%endian, f%rec) )

    f => rtm%f%tidx
    call logmsg('Writing '//str(fileinfo(f)))
    call traperr( wbin(rtm%tidx, f%path, f%dtype, f%endian, f%rec) )

    f => rtm%f%area
    call logmsg('Writing '//str(fileinfo(f)))
    call traperr( wbin(rtm%area, f%path, f%dtype, f%endian, f%rec) )

    f => rtm%f%coef
    call logmsg('Writing '//str(fileinfo(f)))
    call traperr( wbin(rtm%coef, f%path, f%dtype, f%endian, f%rec) )

    call logext()
    !-----------------------------------------------------------
    rtm%ijsize = 0_8
    deallocate(rtm%sidx)
    deallocate(rtm%tidx)
    deallocate(rtm%area)
    deallocate(rtm%coef)
  !-------------------------------------------------------------
  ! Case: Memory is limited
  else
    call errend(msg_not_implemented())
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine merge_rt
!===============================================================
!
!===============================================================
end module mod_main
