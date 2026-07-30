module c2_rt_main_coef
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  use c1_const
  use c1_type_gs
  use c2_type_rt
  use c2_rt_main_util, only: &
        sort_rt
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: calc_rt_coef
  public :: calc_rt_coef_sum_modify_enabled
  public :: calc_rt_coef_sum_modify_not_enabled
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_rt_main_coef'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function calc_rt_coef(&
    rtm, grdidx, grdidxarg, grdara) result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_rt_coef'
  type(rt_main_), intent(inout), target :: rtm
  integer(8), intent(in) :: grdidx(:)
  integer(8), intent(in) :: grdidxarg(:)
  real(8)   , intent(in) :: grdara(:)

  type(gs_common_), pointer :: uc
  integer(8), pointer :: coefidx(:)

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Sort by mesh_coef
  !-------------------------------------------------------------
  selectcase( rtm%mesh_coef )
  case( MESH__SOURCE )
    coefidx => rtm%sidx
  case( MESH__TARGET )
    coefidx => rtm%tidx
  case default
    info = 1
    call errret(msg_invalid_value('rtm%mesh_coef', rtm%mesh_coef))
    return
  endselect
  !-----------------------------------------------------------
  ! Calc. coef.
  !-----------------------------------------------------------
  if( rtm%opt_coef%is_sum_modify_enabled )then
    if( calc_rt_coef_sum_modify_enabled(rtm) /= 0 )then
      info = 1; call errret(); return
    endif
  else
    if( calc_rt_coef_sum_modify_not_enabled(&
          rtm, grdidx, grdidxarg, grdara) /= 0 )then
      info = 1; call errret(); return
    endif
  endif
  !-------------------------------------------------------------
  nullify(uc)
  nullify(coefidx)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_rt_coef
!===============================================================
!
!===============================================================
integer(4) function calc_rt_coef_sum_modify_enabled(rtm) result(info)
  use c2_rt_error, only: &
        raise_error_coef_above_thresh
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_rt_coef_sum_modify_enabled'
  type(rt_main_), intent(inout), target :: rtm

  integer(8), pointer :: coefidx(:)
  integer(8) :: ijs, ije, ij
  real(8)    :: area_sum
  logical    :: updated
  real(8)    :: vmin, vmax, vmax_negative, vmin_positive

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Preparing', PRCNAM, MODNAM, '-p -x2')

  selectcase( rtm%mesh_coef )
  case( MESH__SOURCE )
    coefidx => rtm%sidx
  case( MESH__TARGET )
    coefidx => rtm%tidx
  case default
    info = 1
    call errret(msg_invalid_value('rtm%mesh_coef', rtm%mesh_coef))
    return
  endselect

  if( sort_rt(rtm, rtm%mesh_coef) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call logent('Calculating coef.', PRCNAM, MODNAM, '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    area_sum = sum(rtm%area(ijs:ije))

    if( area_sum == 0.d0 )then
      rtm%coef(ijs:ije) = 0.d0
      cycle
    elseif( area_sum < 0.d0 )then
      info = 1
      call errret(msg_unexpected_condition()//&
                '\nsum(rtm%area(ijs:ije)) < 0'//&
                '\n  ijs: '//str(ijs)//&
                '\n  ije: '//str(ije)//&
                '\n  sum(rtm%area(ijs:ije)): '//str(area_sum))
      return
    endif

    rtm%coef(ijs:ije) = rtm%area(ijs:ije) / area_sum * rtm%opt_coef%sum_modify
  enddo  ! ije/

  call logext()
  !-------------------------------------------------------------
  ! Check the range of coef.
  !-------------------------------------------------------------
  call logent('Checking the range', PRCNAM, MODNAM, '-p -x2')

  call get_ranges_coef(&
         rtm%coef(:rtm%nij), &
         vmin, vmax, vmax_negative, vmin_positive)

  call echo_ranges_coef(&
         vmin, vmax, vmax_negative, vmin_positive)

  call logext()
  !-------------------------------------------------------------
  ! Modify values
  !-------------------------------------------------------------
  call logent('Modifying values', PRCNAM, MODNAM, '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/

    updated = .true.
    do while( updated )
      updated = .false.
      area_sum = sum(rtm%area(ijs:ije))

      if( area_sum == 0.d0 )then
        rtm%coef(ijs:ije) = 0.d0
        exit
      elseif( area_sum < 0.d0 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\nsum(rtm%area(ijs:ije)) < 0'//&
                  '\n  ijs: '//str(ijs)//&
                  '\n  ije: '//str(ije)//&
                  '\n  sum(rtm%area(ijs:ije)): '//str(area_sum))
        return
      endif

      rtm%coef(ijs:ije) = rtm%area(ijs:ije) / area_sum * rtm%opt_coef%sum_modify
      !---------------------------------------------------------
      ! Raise error if coef > 1.0 + error_excess
      !---------------------------------------------------------
      if( rtm%opt_coef%is_error_excess_enabled )then
        do ij = ijs, ije
          if( rtm%coef(ij) > 1.d0 + rtm%opt_coef%error_excess )then
            call raise_error_coef_above_thresh(&
                   ij, rtm%sidx(ij), rtm%tidx(ij), &
                   rtm%coef(ij), rtm%opt_coef%error_excess)
            info = 1; call errret(); return
          endif
        enddo
      endif
      !---------------------------------------------------------
      ! Remove coef. in (0.0, zero_positive)
      !---------------------------------------------------------
      if( rtm%opt_coef%is_zero_positive_enabled )then
        call round_down_coef_lt_zero_positive(&
               rtm%area, rtm%coef, ijs, ije, &
               rtm%opt_coef%zero_positive, updated)
      endif
      !-----------------------------------------------------------
      ! Remove coef. in (zero_negative, 0.0)
      !-----------------------------------------------------------
      if( rtm%opt_coef%is_zero_negative_enabled )then
        call round_down_coef_gt_zero_negative(&
               rtm%area, rtm%coef, ijs, ije, &
               rtm%opt_coef%zero_negative, updated)
      endif
    enddo  ! updated/
    !-----------------------------------------------------------
  enddo  ! ije/

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function calc_rt_coef_sum_modify_enabled
!===============================================================
!
!===============================================================
integer(4) function calc_rt_coef_sum_modify_not_enabled(&
    rtm, grdidx, grdidxarg, grdara) result(info)
  use c2_rt_error, only: &
        raise_error_coef_above_thresh
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_rt_coef_sum_modify_not_enabled'
  type(rt_main_), intent(inout) :: rtm
  integer(8)    , intent(in)    :: grdidx(:)
  integer(8)    , intent(in)    :: grdidxarg(:)
  real(8)       , intent(in)    :: grdara(:)

  integer(8), pointer :: coefidx(:)
  integer(8) :: ijs, ije, ij
  integer(8) :: loc, gij
  real(8)    :: coef_sum
  logical    :: updated
  real(8)    :: vmin, vmax, vmax_negative, vmin_positive

  info = 0
  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Preparing', PRCNAM, MODNAM, '-p -x2')

  selectcase( rtm%mesh_coef )
  case( MESH__SOURCE )
    coefidx => rtm%sidx
  case( MESH__TARGET )
    coefidx => rtm%tidx
  case default
    info = 1
    call errret(msg_invalid_value('rtm%mesh_coef', rtm%mesh_coef))
    return
  endselect

  if( sort_rt(rtm, rtm%mesh_coef) /= 0 )then
    info = 1; call errret(); return
  endif

  call logext()
  !-------------------------------------------------------------
  ! Calc. coef.
  !-------------------------------------------------------------
  call logent('Calculating coef.', PRCNAM, MODNAM, '-p -x2')

  !-------------------------------------------------------------
  ! Case: $grdidx is sorted
  if( size(grdidxarg) == 1 )then
    ije = 0_8
    do while( ije < rtm%nij )
      ijs = ije + 1_8
      ije = ije + 1_8
      do while( ije < rtm%nij )
        if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
        call add(ije)
      enddo  ! ije/

      call search(coefidx(ijs), grdidx, gij)
      if( gij == 0_8 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\ncoefidx(ijs) was not found in grdidx.'//&
                  '\n  ijs: '//str(ijs)//&
                  '\n  coefidx: '//str(coefidx(ijs)))
        return
      endif

      if( calc_coef() /= 0 )then
        info = 1; call errret(); return
      endif
    enddo  ! ije/
  !-------------------------------------------------------------
  ! Case: $grdidx is not sorted
  else
    ije = 0_8
    do while( ije < rtm%nij )
      ijs = ije + 1_8
      ije = ije + 1_8
      do while( ije < rtm%nij )
        if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
        call add(ije)
      enddo  ! ije/

      call search(coefidx(ijs), grdidx, grdidxarg, loc)
      if( loc == 0_8 )then
        info = 1
        call errret(msg_unexpected_condition()//&
                  '\ncoefidx(ijs) was not found in grdidx.'//&
                  '\n  ijs: '//str(ijs)//&
                  '\n  coefidx: '//str(coefidx(ijs)))
        return
      endif

      gij = grdidxarg(loc)

      if( calc_coef() /= 0 )then
        info = 1; call errret(); return
      endif
    enddo  ! ije/
  endif

  call logext()
  !-------------------------------------------------------------
  ! Check the range of coef.
  !-------------------------------------------------------------
  call logent('Checking the range', PRCNAM, MODNAM, '-p -x2')

  call get_ranges_coef(&
         rtm%coef(:rtm%nij), &
         vmin, vmax, vmax_negative, vmin_positive)

  call echo_ranges_coef(&
         vmin, vmax, vmax_negative, vmin_positive)

  call logext()
  !-------------------------------------------------------------
  ! Modify values
  !-------------------------------------------------------------
  call logent('Modifying values', PRCNAM, MODNAM, '-p -x2')

  ije = 0_8
  do while( ije < rtm%nij )
    ijs = ije + 1_8
    ije = ije + 1_8
    do while( ije < rtm%nij )
      if( coefidx(ije+1_8) /= coefidx(ijs) ) exit
      call add(ije)
    enddo  ! ije/
    !-----------------------------------------------------------
    ! Check if value is above thresh.
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_error_excess_enabled )then
      do ij = ijs, ije
        if( rtm%coef(ij) > 1.d0 + rtm%opt_coef%error_excess )then
          call raise_error_coef_above_thresh(&
                 ij, rtm%sidx(ij), rtm%tidx(ij), &
                 rtm%coef(ij), rtm%opt_coef%error_excess)
          info = 1; call errret(); return
        endif
      enddo
    endif
    !-----------------------------------------------------------
    ! Remove coef. in (0.0, zero_positive)
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_zero_positive_enabled )then
      call round_down_coef_lt_zero_positive(&
             rtm%area, rtm%coef, ijs, ije, &
             rtm%opt_coef%zero_positive, updated)
    endif
    !-----------------------------------------------------------
    ! Remove coef. in (zero_negative, 0.0)
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_zero_negative_enabled )then
      call round_down_coef_gt_zero_negative(&
             rtm%area, rtm%coef, ijs, ije, &
             rtm%opt_coef%zero_negative, updated)
    endif
    !-----------------------------------------------------------
    ! Modify sum.
    !-----------------------------------------------------------
    if( rtm%opt_coef%is_sum_modify_ulim_enabled )then
      coef_sum = sum(rtm%coef(ijs:ije))

      do while( coef_sum > rtm%opt_coef%sum_modify_ulim )
        rtm%coef(ijs:ije) = rtm%coef(ijs:ije) / coef_sum * rtm%opt_coef%sum_modify_ulim

        if( rtm%opt_coef%is_zero_positive_enabled )then
          call round_down_coef_lt_zero_positive(&
                 rtm%area, rtm%coef, ijs, ije, &
                 rtm%opt_coef%zero_positive, updated)
        endif

        if( rtm%opt_coef%is_zero_negative_enabled )then
          call round_down_coef_gt_zero_negative(&
                 rtm%area, rtm%coef, ijs, ije, &
                 rtm%opt_coef%zero_negative, updated)
        endif

        coef_sum = sum(rtm%coef(ijs:ije))
      enddo  ! updated/
    endif
    !-----------------------------------------------------------
  enddo  ! ije/

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
integer(4) function calc_coef() result(info)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'calc_coef'

  info = 0

  if( grdara(gij) > 0.d0 )then
    rtm%coef(ijs:ije) = rtm%area(ijs:ije) / grdara(gij)
  else
    if( all(rtm%area(ijs:ije) == 0.d0) )then
      ! TODO: option for allowing zero area grid
      call logwrn(msg_unexpected_condition()//&
                '\ngrdara('//str(gij)//') <= 0.0'//&
                '\n  grdidx: '//str(grdidx(gij))//&
                '\n  grdara: '//str(grdara(gij))//&
                '\n  rtm%area min: '//str(minval(rtm%area(ijs:ije)),'es10.3')//&
                           ', max: '//str(maxval(rtm%area(ijs:ije)),'es10.3'))
      rtm%coef(ijs:ije) = 0.d0
    else
      call logwrn(msg_unexpected_condition()//&
                '\ngrdara('//str(gij)//') <= 0.0'//&
                '\n  grdidx: '//str(grdidx(gij))//&
                '\n  grdara: '//str(grdara(gij))//&
                '\n  rtm%area min: '//str(minval(rtm%area(ijs:ije)),'es10.3')//&
                           ', max: '//str(maxval(rtm%area(ijs:ije)),'es10.3'))
    endif
  endif
end function calc_coef
!---------------------------------------------------------------
end function calc_rt_coef_sum_modify_not_enabled
!===============================================================
!
!===============================================================
subroutine get_ranges_coef(&
    coef, vmin, vmax, vmax_negative, vmin_positive)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_ranges_coef'
  real(8), intent(in)  :: coef(:)
  real(8), intent(out) :: vmin, vmax
  real(8), intent(out) :: vmax_negative, vmin_positive

  integer(8) :: ij

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  vmin = minval(coef)
  vmax = maxval(coef)

  vmin_positive = maxval(coef)
  vmax_negative = minval(coef)

  do ij = 1_8, size(coef)
    if( coef(ij) > 0.d0 )then
      vmin_positive = min(vmin_positive, coef(ij))
    elseif( coef(ij) < 0.d0 )then
      vmax_negative = max(vmax_negative, coef(ij))
    endif
  enddo  ! ij/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine get_ranges_coef
!===============================================================
!
!===============================================================
subroutine echo_ranges_coef(&
    vmin, vmax, vmax_negative, vmin_positive)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'echo_ranges_coef'
  real(8), intent(in) :: vmin, vmax
  real(8), intent(in) :: vmax_negative, vmin_positive

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg('min: '//str(vmin))
  call logmsg('max: '//str(vmax))

  if( vmin < 0.d0 )then
    call logmsg('max negative: '//str(vmax_negative))
  endif

  if( vmax > 0.d0 )then
    call logmsg('min positive: '//str(vmin_positive))
  endif
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine echo_ranges_coef
!===============================================================
!
!===============================================================
subroutine round_down_coef_lt_zero_positive(&
    area, coef, ijs, ije, zero_positive, updated)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'round_down_coef_lt_zero_positive'
  real(8)   , intent(inout) :: area(:), coef(:)
  integer(8), intent(in)    :: ijs, ije
  real(8)   , intent(in)    :: zero_positive
  logical   , intent(inout) :: updated

  integer(8) :: ij
  logical    :: updated_this

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  updated_this = .true.
  do while( updated_this )
    updated_this = .false.
    do ij = ijs, ije
      if( coef(ij) > 0.d0 .and. coef(ij) < zero_positive )then
        area(ij) = 0.d0
        coef(ij) = 0.d0
        updated_this = .true.
        updated = .true.
      endif
    enddo  ! ij/
  enddo  ! updated/
  !-------------------------------------------------------------
   call logret(PRCNAM, MODNAM)
end subroutine round_down_coef_lt_zero_positive
!===============================================================
!
!===============================================================
subroutine round_down_coef_gt_zero_negative(&
    area, coef, ijs, ije, zero_negative, updated)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'round_down_coef_gt_zero_negative'
  real(8)   , intent(inout) :: area(:), coef(:)
  integer(8), intent(in)    :: ijs, ije
  real(8)   , intent(in)    :: zero_negative
  logical   , intent(inout) :: updated

  integer(8) :: ij
  logical    :: updated_this

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  updated_this = .true.
  do while( updated_this )
    updated_this = .false.
    do ij = ijs, ije
      if( coef(ij) < 0.d0 .and. coef(ij) > zero_negative )then
        area(ij) = 0.d0
        coef(ij) = 0.d0
        updated_this = .true.
        updated = .true.
      endif
    enddo  ! ij/
  enddo  ! updated/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine round_down_coef_gt_zero_negative
!===============================================================
!
!===============================================================
end module c2_rt_main_coef
