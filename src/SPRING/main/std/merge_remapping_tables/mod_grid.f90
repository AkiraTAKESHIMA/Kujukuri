module mod_grid
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
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: merge_grid_data
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'mod_grid'

  integer, parameter :: STAT_TARGET_ALL  = 0
  integer, parameter :: STAT_TARGET_PART = 1
  integer, parameter :: STAT_TARGET_NONE = -1
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine merge_grid_data(input, output, opt)
  use mod_utils, only: &
        open_file_grid_im, &
        close_file_grid_im
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'merge_grid_data'
  type(input_) , intent(inout)         :: input
  type(output_), intent(inout), target :: output
  type(opt_)   , intent(in)            :: opt

  type(file_), pointer :: f

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer(8), allocatable :: grdidx_merged(:)
  real(8)   , allocatable :: grdara_merged(:)

  integer(8) :: nmax_ulim
  integer(8) :: nmax, nmax_merged
  integer(8) :: idxmin, idxmax

  integer :: un_grid_im

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Calc. nmax_ulim.
  !-------------------------------------------------------------
  if( opt%sys%memory_ulim == 0.d0  )then
    nmax_ulim = 0_8
  else
    call logent('Calculatin ulim. of length of arrays', PRCNAM, MODNAM)

    call errend(msg_not_implemented())

    call logext()
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( input%nFiles_grid == 0 )then
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( input%opt_idx_dup )
  !-------------------------------------------------------------
  ! Case: Calc. sum
  case( INPUT_OPT_IDX_DUP_SUM )
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call logent('Counting the number of valid grids and get the range of indices', PRCNAM, MODNAM)

    call count_valid_grids(input, nmax, idxmin, idxmax)

    call logmsg('Number of valid grids: '//str(nmax))
    call logmsg('Grid index min: '//str(idxmin)//' max: '//str(idxmax))

    output%f_grid%idxmin = idxmin
    output%f_grid%idxmax = idxmax

    call logext()
    !-----------------------------------------------------------
    ! Merge grid data.
    !-----------------------------------------------------------
    call logent('Merging grid data', PRCNAM, MODNAM)
    !-----------------------------------------------------------
    ! Case: Memory is not limited
    if( nmax_ulim == 0_8 )then
      allocate(grdidx(nmax))
      allocate(grdara(nmax))
      !---------------------------------------------------------
      ! Read all data.
      !---------------------------------------------------------
      call logent('Reading all data', PRCNAM, MODNAM)

      call read_all_data(input, grdidx, grdara)

      call logext()
      !---------------------------------------------------------
      ! Calc. grid area.
      !---------------------------------------------------------
      call logent('Calculating grid area', PRCNAM, MODNAM)

      allocate(grdidx_merged(nmax))
      allocate(grdara_merged(nmax))

      call calc_sum_grid_area(&
             nmax, grdidx, grdara, &
             nmax_merged, grdidx_merged, grdara_merged)

      deallocate(grdidx)
      deallocate(grdara)

      output%f_grid%nmax = nmax_merged

      call logext()
      !---------------------------------------------------------
      ! Output.
      !---------------------------------------------------------
      call logent('Outputting', PRCNAM, MODNAM)

      if( output%f_grid%f_idx%path /= '' )then
        f => output%f_grid%f_idx
        call logmsg('Write '//str(fileinfo(f)))
        call traperr( wbin(grdidx_merged(:nmax_merged), f%path, f%dtype, f%endian, f%rec) )

        f => output%f_grid%f_ara
        call logmsg('Write '//str(fileinfo(f)))
        call traperr( wbin(grdara_merged(:nmax_merged), f%path, f%dtype, f%endian, f%rec) )
      else
        call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)
        write(un_grid_im) nmax_merged, grdidx_merged(1), grdidx_merged(nmax_merged)
        write(un_grid_im) grdidx_merged(:nmax_merged)
        write(un_grid_im) grdara_merged(:nmax_merged)
        call close_file_grid_im()
      endif

      call logext()
      !---------------------------------------------------------
      deallocate(grdidx_merged)
      deallocate(grdara_merged)
    !-----------------------------------------------------------
    ! Case: Memory is limited
    else
      output%f_grid%nmax = 0_8

      call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)

      call calc_grid_area(input, output, nmax_ulim, idxmin, idxmax, un_grid_im)

      call close_file_grid_im()

      if( output%f_grid%f_idx%path /= '' )then
        call make_final_product_from_im(output)
      endif
    !-----------------------------------------------------------
    endif

    call logext()
  !-------------------------------------------------------------
  ! Case: Stop if index was duplicated
  case( INPUT_OPT_IDX_DUP_STOP )
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call logent('Counting the number of valid grids and get the range of indices', PRCNAM, MODNAM)

    call count_valid_grids(input, nmax, idxmin, idxmax)

    call logmsg('Number of valid grids: '//str(nmax))
    call logmsg('Grid index min: '//str(idxmin)//' max: '//str(idxmax))

    output%f_grid%nmax   = nmax
    output%f_grid%idxmin = idxmin
    output%f_grid%idxmax = idxmax

    call logext()
    !-----------------------------------------------------------
    ! Merge grid data.
    !-----------------------------------------------------------
    call logent('Merging grid data', PRCNAM, MODNAM)
    !-----------------------------------------------------------
    ! Case: Memory is not limited
    if( nmax_ulim == 0_8 )then
      allocate(grdidx(nmax))
      allocate(grdara(nmax))
      !---------------------------------------------------------
      ! Read all data.
      !---------------------------------------------------------
      call logent('Reading all data', PRCNAM, MODNAM)

      call read_all_data(input, grdidx, grdara)

      call logext()
      !---------------------------------------------------------
      ! Check duplication of indices.
      !---------------------------------------------------------
      call logent('Checking duplication of indices', PRCNAM, MODNAM)

      call check_index_duplication(nmax, grdidx, grdara)

      call logext()
      !---------------------------------------------------------
      ! Output.
      !---------------------------------------------------------
      call logent('Outputting', PRCNAM, MODNAM)

      if( output%f_grid%f_idx%path /= '' )then
        f => output%f_grid%f_idx
        call logmsg('Write '//str(fileinfo(f)))
        call traperr( wbin(grdidx, f%path, f%dtype, f%endian, f%rec) )

        f => output%f_grid%f_ara
        call logmsg('Write '//str(fileinfo(f)))
        call traperr( wbin(grdara, f%path, f%dtype, f%endian, f%rec) )
      else
        call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)
        write(un_grid_im) nmax, grdidx(1), grdidx(nmax)
        write(un_grid_im) grdidx
        write(un_grid_im) grdara
        call close_file_grid_im()
      endif

      call logext()
      !---------------------------------------------------------
      deallocate(grdidx)
      deallocate(grdara)
    !-----------------------------------------------------------
    ! Case: Memory is limited
    else
      output%f_grid%nmax = 0_8

      call open_file_grid_im(output%path_grid_im, action_write, un_grid_im)

      call merge_grid_data_no_duplication(input, output, nmax_ulim, idxmin, idxmax, un_grid_im)

      call close_file_grid_im()

      if( output%f_grid%f_idx%path /= '' )then
        call make_final_product_from_im(output)
      endif
    !-----------------------------------------------------------
    endif

    call logext()
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('input%opt_idx_dup', input%opt_idx_dup))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine merge_grid_data
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
recursive subroutine calc_grid_area(input, output, nmax_ulim, idxmin, idxmax, un)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_grid_area'
  type(input_) , intent(in)    :: input
  type(output_), intent(inout) :: output
  integer(8)   , intent(in)    :: nmax_ulim
  integer(8)   , intent(in)    :: idxmin, idxmax
  integer      , intent(in)    :: un

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer(8), allocatable :: grdidx_merged(:)
  real(8)   , allocatable :: grdara_merged(:)
  integer   , allocatable :: stat_target(:)
  integer(8) :: nmax, nmax_merged

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('index '//str((/idxmin,idxmax/),' - '))
  !-------------------------------------------------------------
  ! Count the number of target elements
  !-------------------------------------------------------------
  call logent('Counting the number of target elements', PRCNAM, MODNAM)

  allocate(stat_target(input%nFiles_grid))

  call count_target_elements(input, idxmin, idxmax, nmax, stat_target)

  call logmsg('Num. of elements: '//str(nmax))

  call logext()
  !-------------------------------------------------------------
  ! Merge grid data
  !-------------------------------------------------------------
  call logent('Merging grid data', PRCNAM, MODNAM)

  if( nmax > nmax_ulim )then
    call calc_grid_area(input, output, nmax_ulim, idxmin, idxmin+(idxmax-idxmin)/2_8, un)
    call calc_grid_area(input, output, nmax_ulim, idxmin+(idxmax-idxmin)/2_8+1_8, idxmax, un)
  else
    !-----------------------------------------------------------
    ! Extract target elements
    !-----------------------------------------------------------
    call logent('Extracting target elements', PRCNAM, MODNAM)

    allocate(grdidx(nmax))
    allocate(grdara(nmax))

    call extract_target_elements(input, idxmin, idxmax, stat_target, grdidx, grdara)

    call logext()
    !-----------------------------------------------------------
    ! Merge extracted elements
    !-----------------------------------------------------------
    call logent('Merging extracted elements', PRCNAM, MODNAM)

    allocate(grdidx_merged(nmax))
    allocate(grdara_merged(nmax))

    call calc_sum_grid_area(&
           nmax, grdidx, grdara, &
           nmax_merged, grdidx_merged, grdara_merged)

    call logmsg('Num. of elements: '//str(nmax_merged)//&
                ' ('//str((/output%f_grid%nmax+1_8,output%f_grid%nmax+nmax_merged/),' ~ ')//')')

    deallocate(grdidx)
    deallocate(grdara)

    call logext()
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call logent('Outputting', PRCNAM, MODNAM)

    write(un) nmax_merged, idxmin, idxmax
    write(un) grdidx_merged(:nmax_merged)
    write(un) grdara_merged(:nmax_merged)

    call add(output%f_grid%nmax, nmax_merged)

    call logext()
    !-----------------------------------------------------------
    deallocate(grdidx_merged)
    deallocate(grdara_merged)
  endif

  deallocate(stat_target)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calc_grid_area
!===============================================================
!
!===============================================================
subroutine calc_sum_grid_area(&
    nmax, grdidx, grdara, nmax_merged, grdidx_merged, grdara_merged)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_sum_grid_area'
  integer(8), intent(in)    :: nmax
  integer(8), intent(inout) :: grdidx(:)
  real(8)   , intent(inout) :: grdara(:)
  integer(8), intent(out)   :: nmax_merged
  integer(8), intent(out)   :: grdidx_merged(:)
  real(8)   , intent(out)   :: grdara_merged(:)

  integer(8), allocatable :: arg(:)
  integer(8) :: ns, ne

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(arg(nmax))
  call argsort(grdidx, arg)
  call sort(grdidx, arg)
  call sort(grdara, arg)
  deallocate(arg)

  nmax_merged = 0_8
  ne = 0_8
  do while( ne < nmax )
    ns = ne + 1_8
    ne = ne + 1_8
    do while( ne < nmax )
      if( grdidx(ne+1_8) /= grdidx(ns) ) exit
      call add(ne)
    enddo

    nmax_merged = nmax_merged + 1_8
    grdidx_merged(nmax_merged) = grdidx(ns)
    grdara_merged(nmax_merged) = sum(grdara(ns:ne))
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine calc_sum_grid_area
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
recursive subroutine merge_grid_data_no_duplication(&
    input, output, nmax_ulim, idxmin, idxmax, un)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'merge_grid_data_no_duplication'
  type(input_) , intent(in)    :: input
  type(output_), intent(inout) :: output
  integer(8)   , intent(in)    :: nmax_ulim
  integer(8)   , intent(in)    :: idxmin, idxmax
  integer      , intent(in)    :: un

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer   , allocatable :: stat_target(:)
  integer(8) :: nmax

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('index '//str((/idxmin,idxmax/),' - '))
  !-------------------------------------------------------------
  ! Counting the number of target elements
  !-------------------------------------------------------------
  call logent('Counting the number of target elements', PRCNAM, MODNAM)

  allocate(stat_target(input%nFiles_grid))

  call count_target_elements(input, idxmin, idxmax, nmax, stat_target)

  call logmsg('Num. of elements: '//str(nmax)//&
            ' ('//str((/output%f_grid%nmax+1_8,output%f_grid%nmax+nmax/),' ~ ')//')')

  call logext()
  !-------------------------------------------------------------
  ! Merging grid data
  !-------------------------------------------------------------
  call logent('Merging grid data', PRCNAM, MODNAM)

  if( nmax > nmax_ulim )then
    call merge_grid_data_no_duplication(&
           input, output, nmax_ulim, idxmin, idxmin+(idxmax-idxmin)/2_8, un)
    call merge_grid_data_no_duplication(&
           input, output, nmax_ulim, idxmin+(idxmax-idxmin)/2_8+1_8, idxmax, un)
  else
    !-----------------------------------------------------------
    ! Extract target elements
    !-----------------------------------------------------------
    call logent('Extracting target elements', PRCNAM, MODNAM)

    allocate(grdidx(nmax))
    allocate(grdara(nmax))

    call extract_target_elements(input, idxmin, idxmax, stat_target, grdidx, grdara)

    call logext()
    !-----------------------------------------------------------
    ! Check duplication of indices
    !-----------------------------------------------------------
    call logent('Checking duplication of indices', PRCNAM, MODNAM)

    call check_index_duplication(nmax, grdidx, grdara)

    call logext()
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    call logent('Outputting', PRCNAM, MODNAM)

    write(un) nmax, idxmin, idxmax
    write(un) grdidx
    write(un) grdara

    call add(output%f_grid%nmax, nmax)

    call logext()
    !-----------------------------------------------------------
  endif

  deallocate(stat_target)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine merge_grid_data_no_duplication
!===============================================================
!
!===============================================================
subroutine check_index_duplication(nmax, grdidx, grdara)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'check_index_duplication'
  integer(8), intent(in) :: nmax
  integer(8), intent(inout) :: grdidx(:)
  real(8)   , intent(inout) :: grdara(:)

  integer(8), allocatable :: arg(:)
  integer(8) :: ns, ne

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(arg(nmax))
  call argsort(grdidx, arg)
  call sort(grdidx, arg)
  call sort(grdara, arg)
  deallocate(arg)

  ne = 0_8
  do while( ne < nmax )
    ns = ne + 1_8
    ne = ne + 1_8
    do while( ne < nmax )
      if( grdidx(ne+1_8) /= grdidx(ns) ) exit
      call add(ne)
    enddo

    if( ne /= ns )then
      call errend(msg_unexpected_condition()//&
                '\n  Grid index duplicated.')
    endif
  enddo
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine check_index_duplication
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine count_valid_grids(input, nmax_valid, idxmin, idxmax)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'count_valid_grids'
  type(input_), intent(inout) :: input
  integer(8)  , intent(out)   :: nmax_valid
  integer(8)  , intent(out)   :: idxmin, idxmax

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), pointer :: grdidx(:)
  real(8)   , pointer :: grdara(:)

  integer    :: iFile_grid
  integer(8) :: n
  integer    :: stat

  integer    :: dgt_n

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_n = dgt(sum(input%list_f_grid(:)%nmax))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx)
  nullify(grdara)

  idxmin = int8_ulim
  idxmax = int8_llim

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    call logent('File '//str(iFile_grid, dgt(input%nFiles_grid))//&
                ' / '//str(input%nFiles_grid))
    call logmsg('Length: '//str(f_grid%nmax,dgt_n))

    call realloc(grdidx, f_grid%nmax)
    call realloc(grdara, f_grid%nmax)

    f => f_grid%f_idx
    call logmsg('Read '//str(fileinfo(f)))
    call traperr( rbin(grdidx, f%path, f%dtype, f%endian, f%rec) )

    f => f_grid%f_ara
    call logmsg('Read '//str(fileinfo(f)))
    call traperr( rbin(grdara, f%path, f%dtype, f%endian, f%rec) )

    call traperr( get_minmax(&
            grdidx, stat, f_grid%idxmin, f_grid%idxmax, &
            miss=input%idx_miss) )
    if( stat /= 0 )then
      f_grid%nmax_valid = 0_8
      cycle
    endif

    f_grid%nmax_valid = 0_8
    do n = 1_8, f_grid%nmax
      if( grdidx(n) /= input%idx_miss )then
        call add(f_grid%nmax_valid)
      endif
    enddo  ! n/

    call logmsg('Num. of valid elements: '//str(f_grid%nmax_valid))

    idxmin = min(idxmin, f_grid%idxmin)
    idxmax = max(idxmax, f_grid%idxmax)

    call logext()
  enddo  ! iFile_grid/

  nmax_valid = sum(input%list_f_grid(:)%nmax_valid)

  if( nmax_valid == 0_8 )then
    call errend(msg_unexpected_condition()//&
              '\n  Valid index was not found.')
  endif

  deallocate(grdidx)
  deallocate(grdara)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine count_valid_grids
!===============================================================
!
!===============================================================
subroutine read_all_data(input, grdidx_all, grdara_all)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_all_data'
  type(input_), intent(in)  :: input
  integer(8)  , intent(out) :: grdidx_all(:)
  real(8)     , intent(out) :: grdara_all(:)

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), pointer :: grdidx(:)
  real(8)   , pointer :: grdara(:)
  logical(1), pointer :: mask(:)

  integer(8) :: nmax, n
  integer    :: iFile_grid

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(grdidx, grdara, mask)

  nmax = 0_8

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    call realloc(grdidx, f_grid%nmax)
    call realloc(grdara, f_grid%nmax)
    call realloc(mask  , f_grid%nmax)

    f => f_grid%f_idx
    call logmsg('Reading '//str(fileinfo(f)))
    call traperr( rbin(grdidx, f%path, f%dtype, f%endian, f%rec) )

    f => f_grid%f_ara
    call logmsg('Reading '//str(fileinfo(f)))
    call traperr( rbin(grdara, f%path, f%dtype, f%endian, f%rec) )

    if( f_grid%nmax_valid == f_grid%nmax )then
      grdidx_all(nmax+1_8:nmax+f_grid%nmax) = grdidx(:)
      grdara_all(nmax+1_8:nmax+f_grid%nmax) = grdara(:)
      call add(nmax, f_grid%nmax)
      mask(:) = .true.
    else
      mask(:) = .false.
      do n = 1_8, f_grid%nmax
        if( grdidx(n) == input%idx_miss ) cycle
        call add(nmax)
        grdidx_all(nmax) = grdidx(n)
        grdara_all(nmax) = grdara(n)
        mask(n) = .true.
      enddo  ! n/
    endif

    if( .not. any(mask) )then
      call logmsg('No valid grid exists.')
    else
      call logmsg('idx min: '//str(minval(grdidx,mask=mask))//&
                    ', max: '//str(maxval(grdidx,mask=mask))//&
                '\nara min: '//str(minval(grdara,mask=mask))//&
                    ', max: '//str(maxval(grdara,mask=mask)))
    endif
  enddo  ! iFile_grid/

  deallocate(grdidx, grdara, mask)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_all_data
!===============================================================
!
!===============================================================
subroutine count_target_elements(&
    input, idxmin, idxmax, nmax, stat_target)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'count_target_elements'
  type(input_), intent(in)  :: input
  integer(8)  , intent(in)  :: idxmin, idxmax
  integer(8)  , intent(out) :: nmax
  integer     , intent(out) :: stat_target(:)

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), allocatable :: grdidx_this(:)

  integer    :: iFile_grid
  integer(8) :: n

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grdidx_this(maxval(input%list_f_grid(:)%nmax)))

  nmax = 0_8

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)
    !-----------------------------------------------------------
    ! Case: No grid is target.
    if( f_grid%idxmax < idxmin .and. f_grid%idxmin > idxmax )then
      cycle
    !-----------------------------------------------------------
    ! Case: All grids are targets.
    elseif( f_grid%idxmin >= idxmin .and. f_grid%idxmax <= idxmax )then
      call add(nmax, f_grid%nmax)
      stat_target(iFile_grid) = STAT_TARGET_ALL
    !-----------------------------------------------------------
    ! Case: Part of grids can be targets.
    else
      f => f_grid%f_idx
      call traperr( rbin(grdidx_this(:f_grid%nmax), f%path, f%dtype, f%endian, f%rec) )

      do n = 1_8, f_grid%nmax
        if( grdidx_this(n) >= idxmin .and. grdidx_this(n) <= idxmax )then
          stat_target(iFile_grid) = STAT_TARGET_PART
          call add(nmax)
        endif
      enddo  ! n/
    !-----------------------------------------------------------
    endif
  enddo  ! iFile_grid/

  deallocate(grdidx_this)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine count_target_elements
!===============================================================
!
!===============================================================
subroutine extract_target_elements(&
    input, idxmin, idxmax, stat_target, grdidx, grdara)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'extract_target_elements'
  type(input_), intent(in)  :: input
  integer(8)  , intent(in)  :: idxmin, idxmax
  integer     , intent(in)  :: stat_target(:)
  integer(8)  , intent(out) :: grdidx(:)
  real(8)     , intent(out) :: grdara(:)

  type(f_grid_), pointer :: f_grid
  type(file_)  , pointer :: f

  integer(8), allocatable :: grdidx_this(:)
  real(8)   , allocatable :: grdara_this(:)

  integer    :: iFile_grid
  integer(8) :: nmax, n

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grdidx_this(maxval(input%list_f_grid(:)%nmax)))
  allocate(grdara_this(maxval(input%list_f_grid(:)%nmax)))

  nmax = 0_8

  do iFile_grid = 1, input%nFiles_grid
    f_grid => input%list_f_grid(iFile_grid)

    selectcase( stat_target(iFile_grid) )
    !---------------------------------------------------------
    ! Case: No grid is target.
    case( STAT_TARGET_NONE )
      cycle
    !---------------------------------------------------------
    ! Case: All grids are targets.
    case( STAT_TARGET_ALL )
      f => f_grid%f_idx
      call traperr( rbin(grdidx_this, f%path, f%dtype, f%endian, f%rec) )

      f => f_grid%f_ara
      call traperr( rbin(grdara_this, f%path, f%dtype, f%endian, f%rec) )

      grdidx(nmax+1_8:nmax+f_grid%nmax) = grdidx_this(:)
      grdara(nmax+1_8:nmax+f_grid%nmax) = grdara_this(:)
      call add(nmax, f_grid%nmax)
    !---------------------------------------------------------
    ! Case: Part of grids can be targets.
    case( STAT_TARGET_PART )
      f => f_grid%f_idx
      call traperr( rbin(grdidx_this, f%path, f%dtype, f%endian, f%rec) )

      f => f_grid%f_ara
      call traperr( rbin(grdara_this, f%path, f%dtype, f%endian, f%rec) )

      do n = 1_8, f_grid%nmax
        if( grdidx_this(n) >= idxmin .and. grdidx_this(n) <= idxmax )then
          call add(nmax)
          grdidx(nmax) = grdidx_this(n)
          grdara(nmax) = grdara_this(n)
        endif
      enddo  ! n/
    !---------------------------------------------------------
    endselect
  enddo  ! iFile_grid/

  deallocate(grdidx_this)
  deallocate(grdara_this)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine extract_target_elements
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine make_final_product_from_im(output)
  use mod_utils, only: &
        open_file_grid_im, &
        close_file_grid_im
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_final_product_from_im'
  type(output_), intent(in), target :: output

  type(file_), pointer :: f

  integer(8), allocatable :: grdidx(:)
  real(8)   , allocatable :: grdara(:)
  integer(8) :: nmax_all, nmax_max, nmax
  integer(8) :: idxmin, idxmax

  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call open_file_grid_im(output%path_grid_im, action_read, un)

  nmax_all = 0_8
  nmax_max = 0_8
  do while( nmax_all < output%f_grid%nmax )
    read(un) nmax, idxmin, idxmax
    read(un) ! grdidx
    read(un) ! grdara

    call add(nmax_all, nmax)
    nmax_max = max(nmax_max, nmax)
  enddo

  allocate(grdidx(nmax_max))
  allocate(grdara(nmax_max))

  rewind(un)

  nmax_all = 0_8
  do while( nmax_all < output%f_grid%nmax )
    read(un) nmax, idxmin, idxmax
    read(un) grdidx(:nmax)
    read(un) grdara(:nmax)

    f => output%f_grid%f_idx
    call traperr( wbin(grdidx(:nmax), f%path, f%dtype, f%endian, f%rec, &
                       lb=nmax_all+1_8, sz=output%f_grid%nmax) )

    f => output%f_grid%f_ara
    call traperr( wbin(grdara(:nmax), f%path, f%dtype, f%endian, f%rec, &
                       lb=nmax_all+1_8, sz=output%f_grid%nmax) )

    call add(nmax_all, nmax)
  enddo  ! iGroup/

  deallocate(grdidx)
  deallocate(grdara)

  call close_file_grid_im()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine make_final_product_from_im
!===============================================================
!
!===============================================================
end module mod_grid
