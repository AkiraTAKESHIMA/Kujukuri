module lib_math_stats
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_minmax
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface get_minmax
    module procedure get_minmax__int1_1d
    module procedure get_minmax__int1_2d
    module procedure get_minmax__int2_1d
    module procedure get_minmax__int2_2d
    module procedure get_minmax__int4_1d
    module procedure get_minmax__int4_2d
    module procedure get_minmax__int8_1d
    module procedure get_minmax__int8_2d
    module procedure get_minmax__real_1d
    module procedure get_minmax__real_2d
    module procedure get_minmax__dble_1d
    module procedure get_minmax__dble_2d
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'lib_matsh_stats'
  !-------------------------------------------------------------
contains
!===============================================================
! stat = 0: valid value was found
!        1: valid value was not found
!===============================================================
integer(4) function get_minmax__int1_1d(&
    dat, stat, &
    vmin, vmax, vsum, imin, imax, miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int1_1d'
  integer, parameter :: byte = 1
  integer(byte), intent(in)            :: dat(:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  size(mask) /= size(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)
      vmin_ = dat(imin_)
      vmax_ = dat(imax_)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),kind=8)
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)
    vmin_ = dat(imin_)
    vmax_ = dat(imax_)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),kind=8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int1_1d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__int2_1d(&
    dat, stat, &
    vmin, vmax, vsum, imin, imax, miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int2_1d'
  integer, parameter :: byte = 2
  integer(byte), intent(in)            :: dat(:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  size(mask) /= size(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_2
    vmax_ = 0_2
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)
      vmin_ = dat(imin_)
      vmax_ = dat(imax_)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),8)
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)
    vmin_ = dat(imin_)
    vmax_ = dat(imax_)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int2_1d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__int4_1d(&
    dat, stat, &
    vmin, vmax, vsum, imin, imax, miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int4_1d'
  integer, parameter :: byte = 4
  integer(byte), intent(in)            :: dat(:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  size(mask) /= size(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)
      vmin_ = dat(imin_)
      vmax_ = dat(imax_)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),8)
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)
    vmin_ = dat(imin_)
    vmax_ = dat(imax_)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int4_1d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__int8_1d(&
    dat, stat, &
    vmin, vmax, vsum, imin, imax, miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int8_1d'
  integer, parameter :: byte = 8
  integer(byte), intent(in)            :: dat(:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  size(mask) /= size(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)
      vmin_ = dat(imin_)
      vmax_ = dat(imax_)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),8)
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)
    vmin_ = dat(imin_)
    vmax_ = dat(imax_)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),kind=8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int8_1d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__real_1d(&
    dat, stat, &
    vmin, vmax, vsum, imin, imax, miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__real_1d'
  integer, parameter :: byte = 4
  real(byte), intent(in)            :: dat(:)
  integer   , intent(out)           :: stat
  real(byte), intent(out), optional :: vmin, vmax
  real(8)   , intent(out), optional :: vsum
  integer(8), intent(out), optional :: imin, imax
  real(byte), intent(in) , optional :: miss
  logical   , intent(in) , optional :: mask(:)

  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_, imax_
  integer(8) :: nx, ix
  logical    :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  size(mask) /= size(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.0
    vmax_ = 0.0
  endif

  vsum_ = 0.d0

  imin_ = 0_8
  imax_ = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 0
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 0
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)
      vmin_ = dat(imin_)
      vmax_ = dat(imax_)

      if( present(vsum) )then
        vsum_ = 0.d0
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + real(dat(ix),8)
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)
    vmin_ = dat(imin_)
    vmax_ = dat(imax_)

    if( present(vsum) )then
      vsum_ = 0.d0
      do ix = 1_8, nx
        vsum_ = vsum_ + real(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = real(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + real(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__real_1d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__dble_1d(&
    dat, stat, &
    vmin, vmax, vsum, imin, imax, miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__dble_1d'
  integer, parameter :: byte = 8
  real(byte), intent(in)            :: dat(:)
  integer   , intent(out)           :: stat
  real(byte), intent(out), optional :: vmin, vmax
  real(8)   , intent(out), optional :: vsum
  integer(8), intent(out), optional :: imin, imax
  real(byte), intent(in) , optional :: miss
  logical   , intent(in) , optional :: mask(:)

  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_, imax_
  integer(8) :: nx, ix
  logical    :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  size(mask) /= size(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.d0
    vmax_ = 0.d0
  endif

  vsum_ = 0.d0

  imin_ = 0_8
  imax_ = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( found )then
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)
      vmin_ = dat(imin_)
      vmax_ = dat(imax_)
      if( present(vsum) ) vsum_ = sum(dat,mask)
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)
    vmin_ = dat(imin_)
    vmax_ = dat(imax_)
    if( present(vsum) ) vsum_ = sum(dat)
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = real(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + real(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__dble_1d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__int1_2d(&
    dat, stat, &
    vmin, vmax, vsum, imin1, imax1, imin2, imax2, &
    miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int1_2d'
  integer, parameter :: byte = 1
  integer(byte), intent(in)            :: dat(:,:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(mask) /= shape(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
      vmin_ = dat(imin_(1),imin_(2))
      vmax_ = dat(imax_(1),imax_(2))
      if( present(vsum) )then
        vsum_ = 0_8
        do iy = 1, ny
        do ix = 1, nx
          if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
        enddo
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
    vmin_ = dat(imin_(1),imin_(2))
    vmax_ = dat(imax_(1),imax_(2))
    if( present(vsum) )then
      vsum_ = 0_8
      do iy = 1, ny
      do ix = 1, nx
        vsum_ = vsum_ + int(dat(ix,iy),8)
      enddo
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int1_2d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__int2_2d(&
    dat, stat, &
    vmin, vmax, vsum, imin1, imax1, imin2, imax2, &
    miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int2_2d'
  integer, parameter :: byte = 2
  integer(byte), intent(in)            :: dat(:,:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(mask) /= shape(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_2
    vmax_ = 0_2
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
      vmin_ = dat(imin_(1),imin_(2))
      vmax_ = dat(imax_(1),imax_(2))
      if( present(vsum) )then
        vsum_ = 0_8
        do iy = 1, ny
        do ix = 1, nx
          if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
        enddo
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
    vmin_ = dat(imin_(1),imin_(2))
    vmax_ = dat(imax_(1),imax_(2))
    if( present(vsum) )then
      vsum_ = 0_8
      do iy = 1, ny
      do ix = 1, nx
        if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
      enddo
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int2_2d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__int4_2d(&
    dat, stat, &
    vmin, vmax, vsum, imin1, imax1, imin2, imax2, &
    miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int4_2d'
  integer, parameter :: byte = 4
  integer(byte), intent(in)            :: dat(:,:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(mask) /= shape(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_4
    vmax_ = 0_4
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
      vmin_ = dat(imin_(1),imin_(2))
      vmax_ = dat(imax_(1),imax_(2))
      if( present(vsum) )then
        vsum_ = 0_8
        do iy = 1, ny
        do ix = 1, nx
          if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
        enddo
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
    vmin_ = dat(imin_(1),imin_(2))
    vmax_ = dat(imax_(1),imax_(2))
    if( present(vsum) )then
      vsum_ = 0_8
      do iy = 1, ny
      do ix = 1, nx
        if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
      enddo
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int4_2d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__int8_2d(&
    dat, stat, &
    vmin, vmax, vsum, imin1, imax1, imin2, imax2, &
    miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__int8_2d'
  integer, parameter :: byte = 8
  integer(byte), intent(in)            :: dat(:,:)
  integer      , intent(out)           :: stat
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)

  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(mask) /= shape(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_8
    vmax_ = 0_8
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
      vmin_ = dat(imin_(1),imin_(2))
      vmax_ = dat(imax_(1),imax_(2))
      if( present(vsum) )then
        vsum_ = 0_8
        do iy = 1, ny
        do ix = 1, nx
          if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
        enddo
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
    vmin_ = dat(imin_(1),imin_(2))
    vmax_ = dat(imax_(1),imax_(2))
    if( present(vsum) )then
      vsum_ = 0_8
      do iy = 1, ny
      do ix = 1, nx
        if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
      enddo
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__int8_2d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__real_2d(&
    dat, stat, &
    vmin, vmax, vsum, imin1, imax1, imin2, imax2, &
    miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__real_2d'
  integer, parameter :: byte = 4
  real(byte)   , intent(in)            :: dat(:,:)
  integer      , intent(out)           :: stat
  real(byte)   , intent(out), optional :: vmin, vmax
  real(8)      , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)

  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_(2), imax_(2)
  integer(8) :: nx, ix, ny, iy
  logical    :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(mask) /= shape(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.0
    vmax_ = 0.0
  endif

  vsum_ = 0.d0

  imin_(:) = 0_8
  imax_(:) = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
      vmin_ = dat(imin_(1),imin_(2))
      vmax_ = dat(imax_(1),imax_(2))
      if( present(vsum) )then
        vsum_ = 0_8
        do iy = 1, ny
        do ix = 1, nx
          if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
        enddo
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
    vmin_ = dat(imin_(1),imin_(2))
    vmax_ = dat(imax_(1),imax_(2))
    if( present(vsum) )then
      vsum_ = 0_8
      do iy = 1, ny
      do ix = 1, nx
        if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
      enddo
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__real_2d
!===============================================================
!
!===============================================================
integer(4) function get_minmax__dble_2d(&
    dat, stat, &
    vmin, vmax, vsum, imin1, imax1, imin2, imax2, &
    miss, mask &
) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_minmax__dble_2d'
  integer, parameter :: byte = 8
  real(byte)   , intent(in)            :: dat(:,:)
  integer      , intent(out)           :: stat
  real(byte)   , intent(out), optional :: vmin, vmax
  real(8)      , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)

  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_(2), imax_(2)
  integer(8) :: nx, ix, ny, iy
  logical    :: found

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\n  shape(mask) /= shape(dat)')
      return
    endif
  endif

  stat = 0

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.d0
    vmax_ = 0.d0
  endif

  vsum_ = 0.d0

  imin_(:) = 0_8
  imax_(:) = 0_8

  call copy_results()
  !-------------------------------------------------------------
  ! Get statistics
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( any(mask) )then
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
      vmin_ = dat(imin_(1),imin_(2))
      vmax_ = dat(imax_(1),imax_(2))
      if( present(vsum) )then
        vsum_ = 0_8
        do iy = 1, ny
        do ix = 1, nx
          if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
        enddo
        enddo
      endif
    else
      stat = 1
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
    vmin_ = dat(imin_(1),imin_(2))
    vmax_ = dat(imax_(1),imax_(2))
    if( present(vsum) )then
      vsum_ = 0_8
      do iy = 1, ny
      do ix = 1, nx
        if( mask(ix,iy) ) vsum_ = vsum_ + int(dat(ix,iy),8)
      enddo
      enddo
    endif
  endif
  !-------------------------------------------------------------
  call copy_results()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine copy_results()
  implicit none

  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
end subroutine copy_results
!---------------------------------------------------------------
end function get_minmax__dble_2d
!===============================================================
!
!===============================================================
end module lib_math_stats
