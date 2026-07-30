module c3_rt_latlon_raster
  use lib_const
  use lib_base
  use lib_log
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
  public :: make_rt_latlon_raster
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_rt_latlon_raster'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function make_rt_latlon_raster(&
    s, t, rt &
) result(info)
  use c1_opt_ctrl, only: &
        get_opt_earth
  use c2_rt1d, only: &
        init_rt1d   , &
        clear_rt1d  , &
        reshape_rt1d
  use c3_rt_llbnds, only: &
        calc_relations_llbnds
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'make_rt_latlon_raster'
  type(gs_), intent(in)   , target :: s
  type(gs_), intent(in)   , target :: t
  type(rt_), intent(inout), target :: rt

  type(gs_)         , pointer :: a  ! latlon
  type(gs_)         , pointer :: b  ! raster
  type(gs_latlon_)  , pointer :: al
  type(gs_raster_)  , pointer :: br
  type(hrel_)       , pointer :: ahr
  type(vrel_)       , pointer :: avr
  type(raster_zone_), pointer :: brz
  type(rt_main_)    , pointer :: rtm

  type(opt_earth_) :: earth
  type(rt1d_), pointer :: rt1d(:), rt1
  integer(8) :: iah, iav
  integer(8) :: ibz, ibr, ibh, ibv, iibv, iibh_this
  integer(8), allocatable :: iibh(:)
  integer(8), allocatable :: ij_prev(:)
  integer(8), allocatable :: bidx_prev(:)
  integer(8) :: aidx, aij
  integer(8) :: bidx
  integer(8) :: loc
  integer(8) :: ij, ij1, ij2
  integer(8), parameter :: IJSIZE_INIT = 16_8
  real(8) :: lapara
  real(8) :: lapara_sum_b
  integer :: case_wgtmap

  info = 0
  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  ! Set pointers
  !-------------------------------------------------------------
  if( s%typ == MESHTYPE__LATLON .and. &
      t%typ == MESHTYPE__RASTER )then
    a => s
    b => t
  elseif( s%typ == MESHTYPE__RASTER .and. &
          t%typ == MESHTYPE__LATLON )then
    a => t
    b => s
  else
    info = 1
    call errret(msg_invalid_value()//&
              '\n  s%typ: '//str(s%typ)//&
              '\n  t%typ: '//str(t%typ))
    return
  endif

  al => a%latlon
  br => b%raster

  rtm => rt%main

  call get_opt_earth(earth)
  !-------------------------------------------------------------
  ! Calc. relations of grid bounds.
  !-------------------------------------------------------------
  call start_ctimer('bounds_relation')

  if( calc_relations_llbnds(al, br) /= 0 )then
    info = 1; call errret(); return
  endif
  if( calc_relations_llbnds(br, al) /= 0 )then
    info = 1; call errret(); return
  endif

  call stop_ctimer('bounds_relation')
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  call start_ctimer('buffer')

  allocate(iibh(br%hi-1_8:br%hf+1_8))

  allocate(rt1d(al%nij))
  if( init_rt1d(rt1d) /= 0 )then
    info = 1; call errret(); return
  endif
  do aij = 1_8, al%nij
    rt1 => rt1d(aij)
    rt1%ijsize = IJSIZE_INIT
    allocate(rt1%idx(rt1%ijsize))
    allocate(rt1%ara(rt1%ijsize))
  enddo

  allocate(bidx_prev(al%nij))
  bidx_prev(:) = br%idx_miss

  allocate(ij_prev(al%nij))

  if( br%status_wgtmap == GRID_STATUS__PREPARED )then
    case_wgtmap = 1
  elseif( br%status_wgtmap == GRID_STATUS__NOT_USED )then
    case_wgtmap = 2
  else
    info = 1
    call errret(msg_unexpected_condition()//&
              '\nbr%status_wgtmap == '//str(br%status_wgtmap))
    return
  endif

  call stop_ctimer('buffer')
  !-------------------------------------------------------------
  ! Make a remapping table
  !-------------------------------------------------------------
  call logent('Making a remapping table')

  call start_ctimer('intersection')

  rt1%mij = 0_8

  if( br%debug )then
    lapara_sum_b = 0.d0
  endif

  do ibz = 1, br%nZone
    if( br%nZone>1 )&
    call logent('("'//str(br%nam)//'") zone '//str(ibz)//' / '//str(br%nZone), PRCNAM, MODNAM)

    brz => br%zone(ibz)
    if( .not. brz%is_valid )then
      if( br%nZone>1 ) call logext()
      cycle
    endif

    do iav = al%vi, al%vf
      avr => al%vrel(iav)
      if( avr%vi == 0_8 ) cycle

      do iah = al%hi, al%hf
        if( .not. al%mskmap(iah,iav) ) cycle

        ahr => al%hrel(iah)
        if( ahr%nr == 0 ) cycle
        if( all(ahr%hf(:ahr%nr) < brz%hi .or. brz%hf < ahr%hi(:ahr%nr)) ) cycle
        !-------------------------------------------------------
        ! Set $rt1
        !-------------------------------------------------------
        aidx = al%idxmap(iah,iav)
        call search(aidx, al%grid%idx, al%grid%idxarg, loc)
        if( loc == 0_8 )then
          info = 1
          call errret(msg_unexpected_condition()//&
                    '\nIndex of $a, '//str(aidx)//' was not found.')
          return
        endif
        aij = al%grid%idxarg(loc)
        rt1 => rt1d(aij)
        rt1%idx_self = al%idxmap(iah,iav)
        !-------------------------------------------------------
        ! Calc. ranges of $ibh
        !-------------------------------------------------------
        iibh_this = 0_8
        do ibr = 1, ahr%nr
          do ibh = ahr%hi(ibr), ahr%hf(ibr)
            call add(iibh_this)
            iibh(ibh) = iibh_this
          enddo
        enddo
        !-------------------------------------------------------
        ! Loop for $b
        !-------------------------------------------------------
        selectcase( case_wgtmap )
        !-------------------------------------------------------
        ! Case: $b have weight data
        case( 1 )
          do ibv = max(brz%vi,avr%vi), min(brz%vf,avr%vf)
            iibv = ibv - avr%vi + 1_8
            do ibr = 1, ahr%nr
              do ibh = max(brz%hi,ahr%hi(ibr)), min(brz%hf,ahr%hf(ibr))
                if( .not. brz%mskmap(ibh,ibv) ) cycle

                lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                           * al%wgtmap(iah,iav) * brz%wgtmap(ibh,ibv)

                call update_rt1d()
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        ! Case: $b does not have weight data
        case( 2 )
          do ibv = max(brz%vi,avr%vi), min(brz%vf,avr%vf)
            iibv = ibv - avr%vi + 1_8
            do ibr = 1, ahr%nr
              do ibh = max(brz%hi,ahr%hi(ibr)), min(brz%hf,ahr%hf(ibr))
                if( .not. brz%mskmap(ibh,ibv) ) cycle

                lapara = avr%lapara_1rad(iibv) * ahr%lonwidth(iibh(ibh)) &
                           * al%wgtmap(iah,iav)

                call update_rt1d()
              enddo  ! ibh/
            enddo  ! ibr/
          enddo  ! ibv/
        !-------------------------------------------------------
        endselect

      enddo  ! iav/
    enddo  ! iah/
    if( br%nZone>1 ) call logext()
  enddo  ! ibz/
  !-------------------------------------------------------------
  if( br%debug )then
    call logmsg('lapara_sum_b: '//str(lapara_sum_b,'es20.13'))
  endif

  call stop_ctimer('intersection')

  ! Reshape $rt1d and output intermediates
  !-------------------------------------------------------------
  call start_ctimer('rt_post')

  if( reshape_rt1d(rt1d, a%is_source, rtm) /= 0 )then
    info = 1; call errret(); return
  endif

  call stop_ctimer('rt_post')

  call logext()
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  call start_ctimer('buffer')

  nullify(rt1)
  if( clear_rt1d(rt1d) /= 0 )then
    info = 1; call errret(); return
  endif

  nullify(rtm)

  deallocate(bidx_prev)
  deallocate(ij_prev)

  deallocate(iibh)

  nullify(ahr, avr)
  deallocate(al%hrel, al%vrel)
  deallocate(br%hrel, br%vrel)
  nullify(al, br)
  nullify(a, b)

  call stop_ctimer('buffer')
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine update_rt1d()
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'update_rt1d'

  bidx = brz%idxmap(ibh,ibv)

  if( bidx == bidx_prev(aij) )then
    call add(rt1%ara(ij_prev(aij)), lapara)
  elseif( rt1%mij == 0_8 )then
    rt1%mij = 1_8
    rt1%idx(1) = bidx
    rt1%ara(1) = lapara
    bidx_prev(aij) = bidx
    ij_prev(aij) = 1_8
  else
    call search_nearest(bidx, rt1%idx(:rt1%mij), ij1, ij2)
    if( ij1 == ij2 )then
      call add(rt1%ara(ij2), lapara)
    else
      if( rt1%mij == rt1%ijsize )then
        call mul(rt1%ijsize, 2)
        call realloc(rt1%idx, rt1%ijsize, clear=.false.)
        call realloc(rt1%ara, rt1%ijsize, clear=.false.)
      endif
      do ij = rt1%mij, ij2, -1_8
        rt1%idx(ij+1_8) = rt1%idx(ij)
        rt1%ara(ij+1_8) = rt1%ara(ij)
      enddo
      rt1%idx(ij2) = bidx
      rt1%ara(ij2) = lapara
      call add(rt1%mij)
    endif
    bidx_prev(aij) = bidx
    ij_prev(aij) = ij2
  endif
end subroutine update_rt1d
!---------------------------------------------------------------
end function make_rt_latlon_raster
!===============================================================
!
!===============================================================
end module c3_rt_latlon_raster
