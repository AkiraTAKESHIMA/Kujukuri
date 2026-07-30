module mod_make_basin
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use lib_array
  use lib_math
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: findRiverEnds
  public :: setBasinIds
  public :: getBasinInTile
  public :: getUpperBasin
  public :: updateUpperBasin
  public :: mergeAllBasins
  public :: makeTiledBasinMaps
  public :: checkBasins
  public :: renewBasinList
  public :: makeNewBasinMaps
  public :: makeTiledBasinLists
  public :: makeBasinRangeList
  public :: makeLowresBasinMaps

  public :: makeBasinTopoMap
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  integer, parameter :: STAT_SAME_BASIN = -1
  integer, parameter :: STAT_NEW_BASIN  = 0
  integer, parameter :: STAT_MERGED     = 1
  integer, parameter :: STAT_OTHER_TILE = 2

  integer, parameter :: RATIO_LRES = 9

  character(CLEN_VAR), parameter :: MODNAM = 'mod_make_basin'
  !-------------------------------------------------------------
  type upper_
    integer :: n
    integer, pointer :: tx(:), ty(:), id(:)
  end type

  type lst_bsn_
    integer :: id
    integer :: id_fix
    type(upper_) :: upper
    logical :: done_upper_update
  end type

  type tile_
    integer :: nBsn, nBsn_fix, nBsn_tmp
    integer :: bsnId0
    type(lst_bsn_), pointer :: lst_bsn(:)
    logical :: done_upper_update
  end type
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface 
    integer function access(path,mode)
      character(*), intent(in) :: path
      character(*), intent(in) :: mode
    end function
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
! IN:
!   ${DIR_ORG}/dir/${tilename(tx,ty)}_dir.bin'
! OUT:
!   ${DIR_TILED}/rivend/${tilename(tx,ty)}.txt'
!===============================================================
subroutine findRiverEnds(tx, ty)
  use c1_const
  use c2_jflw_const
  use c2_jflw_grid, only: &
        get_nextxy
  use c2_jflw_io, only: &
        get_f_map_tile, &
        get_f_lst_tile
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'findRiverEnds'
  integer, intent(in) :: tx, ty

  integer(1), allocatable :: fdr(:,:)
  integer :: ix, iy
  integer :: xx, yy
  character(CLEN_PATH) :: f_fdr
  character(CLEN_PATH) :: f_rivend
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(tileinfo(tx,ty))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(fdr(NX,NY))
  f_fdr = get_f_map_tile(RESOLUTION_1SEC, 'dir', tx, ty)
  if( access(f_fdr,' ') /= 0 )then
    call logmsg('File not found: '//str(f_fdr))
    call logret(PRCNAM, MODNAM)
    return
  endif
  call logmsg('Reading '//str(f_fdr))
  call traperr( rbin(fdr, f_fdr) )

  f_rivend = get_f_lst_tile(RESOLUTION_1SEC, 'rivend', tx, ty)
  call traperr( mkdir(dirname(f_rivend)) )
  call logmsg('Writing '//str(f_rivend))
  open(newunit=un, file=f_rivend, status='replace')
  write(un,"(1x,a)") 'x y next('//str(XY_RIVERMOUTH)//'=rivermouth'//&
                     ','//str(XY_INLAND)//'=inland)'
  do iy = 1, NY
  do ix = 1, NX
    if( fdr(ix,iy) == FDR_MISS ) cycle
    call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
    selectcase( xx )
    case( XY_RIVERMOUTH, &
          XY_INLAND )
      write(un,"(2(1x,i4),1x,i2)") ix, iy, xx
    endselect
  enddo
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine findRiverEnds
!===============================================================
! SUBROUTINE findRiverEnds must be run for all tiles beforehand.
!
! IN:
!   ${DIR_TILED}/id_all.txt
!   ${DIR_ORG}/dir/${tilename(tx,ty)}_dir.bin'
!   ${DIR_TILED}/rivend/${tilename(tx,ty)}.txt'
! OUT:
!   ${DIR_TILED}/rivend_id/${tilename(tx,ty)}.txt'
!===============================================================
subroutine setBasinIds()
  use c1_const
  use c2_jflw_const
  use c2_jflw_io, only: &
        get_f_map_tile, &
        get_f_lst_tile
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'setBasinIds'

  integer :: nBsn, nBsn_tile, iBsn
  integer :: nRivermouth, nInland
  integer :: ix, iy, xx
  integer :: itx, ity
  character(CLEN_PATH) :: f_fdr
  character(CLEN_PATH) :: f_rivend
  character(CLEN_PATH) :: f_rivend_id
  character(CLEN_PATH) :: f_id_all
  integer :: un, un_id, un_all
  integer :: ios

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_id_all = get_f_lst_tile(RESOLUTION_1SEC, 'id_all')
  call logmsg('Writing '//str(f_id_all))
  open(newunit=un_all, file=f_id_all, status='replace')
  write(un_all,"(1x,a)") 'bsnId tx ty x y next'

  nBsn = 0
  nRivermouth = 0
  nInland = 0
  do ity = 1, nty
  do itx = 1, ntx
    f_fdr = get_f_map_tile(RESOLUTION_1SEC, 'dir', itx, ity)
    if( access(f_fdr,' ') /= 0 ) cycle
    call logent(tileinfo(itx, ity))

    f_rivend = get_f_lst_tile(RESOLUTION_1SEC, 'rivend', itx, ity)
    call logmsg('Reading '//str(f_rivend))
    open(newunit=un, file=f_rivend, status='old')
    read(un,*)
    nBsn_tile = 0
    do
      read(un,*,iostat=ios) ix, iy, xx
      selectcase( ios )
      case( 0 )
        nBsn_tile = nBsn_tile + 1
        selectcase( xx )
        case( XY_RIVERMOUTH )
          nRivermouth = nRivermouth + 1
        case( XY_INLAND )
          nInland = nInland + 1
        case default
          call errend(msg_invalid_value('xx', xx))
        endselect
      case( -1 )
        exit
      case default
        call errend(msg_io_error(f=f_rivend))
      endselect
    enddo

    f_rivend_id = get_f_lst_tile(RESOLUTION_1SEC, 'rivend_id', itx, ity)
    call traperr( mkdir(dirname(f_rivend_id)) )
    call logmsg('Writing '//str(f_rivend_id))
    un_id = unit_number()
    open(un_id, file=f_rivend_id, status='replace')
    write(un_id,"(1x,a)") 'i bsnId x y next'
    rewind(un)
    read(un,*)
    do iBsn = 1, nBsn_tile
      read(un,*) ix, iy, xx
      write(un_id,"(2(1x,i8),2(1x,i4),1x,i2)") iBsn, nBsn+iBsn, ix, iy, xx

      write(un_all,"(1x,i8,2(1x,i2),2(1x,i4),1x,i2)") nBsn+iBsn, itx, ity, ix, iy, xx
    enddo

    close(un)
    close(un_id)

    call logmsg('nBsn_tile: '//str(nBsn_tile))
    nBsn = nBsn + nBsn_tile

    call logext()
  enddo  ! ix/
  enddo  ! iy/

  close(un_all)

  call logmsg('Saved '//str(f_id_all))
  call logmsg('nBsn: '//str(nBsn))
  call logmsg('nRivermouth: '//str(nRivermouth))
  call logmsg('nInland    : '//str(nInland))
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine setBasinIds
!===============================================================
! SUBROUTINE setBasinIds must be run beforehand.
!
! IN:
!   ${DIR_ORG}/dir/${tilename(tx,ty)}_dir.bin
!   ${DIR_ORG}/upg/${tilename(tx,ty)}_upg.bin
!   ${DIR_TILED}/rivend_id/${tilename(tx,ty)}.txt
! OUT:
!   ${DIR_TILED}/bsn_parts/${tilename(tx,ty)}.bin
!   ${DIR_TILED}/source/${tilename(tx,ty)}.txt
!===============================================================
subroutine getBasinInTile(tx, ty)
  use c1_const
  use c2_jflw_const
  use c2_jflw_grid, only: &
        get_nextxy
  use c2_jflw_io, only: &
        get_f_map_tile, &
        get_f_lst_tile
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'getBasinInTile'
  integer, intent(in) :: tx, ty

  integer(1), allocatable :: fdr(:,:)
  integer, allocatable :: upg(:,:)
  integer, allocatable :: bsn(:,:)
  integer :: ix, iy
  integer :: xx, yy
  integer :: x, y, next
  integer :: ix0, iy0
  integer :: bsnId
  integer :: iBsn, iBsn_tmp
  integer :: stat
  integer :: i_

  character(CLEN_PATH) :: f_fdr
  character(CLEN_PATH) :: f_upg
  character(CLEN_PATH) :: f_bsn
  character(CLEN_PATH) :: f_rivend_id
  character(CLEN_PATH) :: f_source
  integer :: un
  integer :: ios

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(tileinfo(tx,ty))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(fdr(NX,NY))
  allocate(upg(NX,NY))

  f_fdr = get_f_map_tile(RESOLUTION_1SEC, 'dir', tx, ty)
  f_upg = get_f_map_tile(RESOLUTION_1SEC, 'upg', tx, ty)
  if( access(f_fdr,' ') /= 0 )then
    call logmsg('File not found: '//str(f_fdr))
    call logret(PRCNAM, MODNAM)
    return
  endif

  call logmsg('Reading '//str(f_fdr))
  call traperr( rbin(fdr, f_fdr) )
  call logmsg('Reading '//str(f_upg))
  call traperr( rbin(upg, f_upg) )

  f_bsn = get_f_map_tile(RESOLUTION_1SEC, 'bsn_parts', tx, ty)
  f_source = get_f_lst_tile(RESOLUTION_1SEC, 'source', tx, ty)
  !-------------------------------------------------------------
  ! Get shape of basins
  !-------------------------------------------------------------
  call logent('Getting shape of basins')

  allocate(bsn(NX,NY))
  bsn(:,:) = BSN_MISS

  f_rivend_id = get_f_lst_tile(RESOLUTION_1SEC, 'rivend_id', tx, ty)
  call logmsg('Reading '//str(f_rivend_id))
  open(newunit=un, file=f_rivend_id, status='old')
  read(un,*)
  do
    read(un,*,iostat=ios) i_, bsnId, x, y, next
    selectcase( ios )
    case( 0 )
      bsn(x,y) = bsnId
    case( -1 )
      exit
    case default
      call errend(msg_io_error(f=f_rivend_id))
    endselect
  enddo
  close(un)

  iBsn = 0
  iBsn_tmp = 0
  do iy0 = 1, NY
  do ix0 = 1, NX
    if( bsn(ix0,iy0) /= BSN_MISS ) cycle
    if( fdr(ix0,iy0) == FDR_MISS ) cycle

    ix = ix0
    iy = iy0
    do
      call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
      if( xx < 1 .or. xx > NX .or. yy < 1 .or. yy > NY )then
        if( xx == XY_RIVERMOUTH .or. &
            xx == XY_INLAND )then
          call errend(msg_unexpected_condition()//&
                      ' xx='//str(xx)//', yy='//str(yy))
        elseif( xx == 0 .or. xx == NX+1 .or. &
                yy == 0 .or. yy == NY+1 )then
          stat = STAT_OTHER_TILE
        else
          call errend(msg_unexpected_condition()//&
                      'xx='//str(xx)//', yy='//str(yy))
        endif
        exit
      endif

      ix = xx
      iy = yy
      if( bsn(ix,iy) /= BSN_MISS )then
        stat = STAT_MERGED
        exit
      endif
    enddo

    selectcase( stat )
    !-----------------------------------------------------------
    ! Case: Flow into other tile
    case( STAT_OTHER_TILE )
      iBsn_tmp = iBsn_tmp + 1
      bsnId = -iBsn_tmp
    !-----------------------------------------------------------
    ! Case: Merged
    !       Overwrite basin id
    case( STAT_MERGED )
      bsnId = bsn(ix,iy)
      ix = ix0
      iy = iy0
      !call logmsg('('//str((/ix0,iy0/),4,',')//') bsn '//str(bsnId,10))
      do
        bsn(ix,iy) = bsnId
        call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
        if( xx < 1 .or. xx > NX .or. yy < 1 .or. yy > NY )then
          call errend(msg_unexpected_condition())
        endif
        ix = xx
        iy = yy
        if( bsn(ix,iy) == bsnId ) exit
      enddo
      cycle
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call errend(msg_invalid_value('stat', stat))
    endselect
    !-----------------------------------------------------------
    ! Put basin id on the map
    !-----------------------------------------------------------
    ix = ix0
    iy = iy0
    do
      bsn(ix,iy) = bsnId
      call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
      if( xx < 1 .or. xx > NX .or. yy < 1 .or. yy > NY ) exit
      ix = xx
      iy = yy
    enddo
  enddo  ! ix0/
  enddo  ! iy0/

  call logmsg('Num. of tmp. basins: '//str(iBsn_tmp))

  call logmsg('Writing '//str(f_bsn))
  call traperr( wbin(bsn, f_bsn, replace=.true.) )

  call logext()
  !-------------------------------------------------------------
  ! Find sources of each basin in this tile
  !-------------------------------------------------------------
  call logent('Finding sources of each basin in this tile')

  call logmsg('Writing '//str(f_source))
  open(newunit=un, file=f_source, status='replace')

  write(un,"(a)") 'ID ix iy'

  do iy = 1, NY
  do ix = 1, NX
    if( upg(ix,iy) == 1 )then
      write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
    endif
  enddo
  enddo

  ix = 1
  iy = 1
  if( upg(ix,iy) > 1 )then
    if( .not. (fdr(ix+1,iy  ) == FDR_WEST      .or. &
               fdr(ix+1,iy+1) == FDR_NORTHWEST .or. &
               fdr(ix  ,iy+1) == FDR_NORTH) )then
      write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
    endif
  endif

  ix = 1
  iy = NY
  if( upg(ix,iy) > 1 )then
    if( .not. (fdr(ix+1,iy  ) == FDR_WEST      .or. &
               fdr(ix+1,iy-1) == FDR_SOUTHWEST .or. &
               fdr(ix  ,iy-1) == FDR_SOUTH) )then
      write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
    endif
  endif

  ix = NX
  iy = 1
  if( upg(ix,iy) > 1 )then
    if( .not. (fdr(ix-1,iy  ) == FDR_EAST      .or. &
               fdr(ix-1,iy+1) == FDR_NORTHEAST .or. &
               fdr(ix  ,iy+1) == FDR_NORTH) )then
      write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
    endif
  endif

  ix = NX
  iy = NY
  if( upg(ix,iy) > 1 )then
    if( .not. (fdr(ix-1,iy  ) == FDR_EAST      .or. &
               fdr(ix-1,iy-1) == FDR_SOUTHEAST .or. &
               fdr(ix  ,iy-1) == FDR_SOUTH) )then
      write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
    endif
  endif

  ix = 1
  do iy = 2, NY-1
    if( upg(ix,iy) > 1 )then
      if( .not. (fdr(ix  ,iy-1) == FDR_SOUTH     .or. &
                 fdr(ix  ,iy+1) == FDR_NORTH     .or. &
                 fdr(ix+1,iy-1) == FDR_SOUTHWEST .or. &
                 fdr(ix+1,iy  ) == FDR_WEST      .or. &
                 fdr(ix+1,iy+1) == FDR_NORTHWEST) )then
        write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
      endif
    endif
  enddo

  ix = NX
  do iy = 2, NY-1
    if( upg(ix,iy) > 1 )then
      if( .not. (fdr(ix  ,iy-1) == FDR_SOUTH     .or. &
                 fdr(ix  ,iy+1) == FDR_NORTH     .or. &
                 fdr(ix-1,iy-1) == FDR_SOUTHEAST .or. &
                 fdr(ix-1,iy  ) == FDR_EAST      .or. &
                 fdr(ix-1,iy+1) == FDR_NORTHEAST ) )then
        write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
      endif
    endif
  enddo

  iy = 1
  do ix = 2, NX-1
    if( upg(ix,iy) > 1 )then
      if( .not. (fdr(ix-1,iy  ) == FDR_EAST      .or. &
                 fdr(ix+1,iy  ) == FDR_WEST      .or. &
                 fdr(ix-1,iy+1) == FDR_NORTHEAST .or. &
                 fdr(ix  ,iy+1) == FDR_NORTH     .or. &
                 fdr(ix+1,iy+1) == FDR_NORTHWEST) )then
        write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
      endif
    endif
  enddo

  iy = NY
  do ix = 2, NX-1
    if( upg(ix,iy) > 1 )then
      if( .not. (fdr(ix-1,iy  ) == FDR_EAST      .or. &
                 fdr(ix+1,iy  ) == FDR_WEST      .or. &
                 fdr(ix-1,iy-1) == FDR_SOUTHEAST .or. &
                 fdr(ix  ,iy-1) == FDR_SOUTH     .or. &
                 fdr(ix+1,iy-1) == FDR_SOUTHWEST) )then
        write(un,"(1x,i10,2(1x,i4))") bsn(ix,iy), ix, iy
      endif
    endif
  enddo

  close(un)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine getBasinInTile
!===============================================================
! SUBROUTINE getBasinInTile must be run for all the surrounding 
! tiles beforehand.
!
! IN:
!   ${DIR_ORG}/dir/${tilename(itx,ity)}_dir.bin
!   ${DIR_TILED}/bsn_parts/${tilename(itx,ity)}.bin
!     for itx in (itx-1,itx+1), ity in (ity-1,ity+1)
! OUT:
!   ${DIR_TILED}/upper/${tilename(tx,ty)}.txt
!===============================================================
subroutine getUpperBasin(tx, ty)
  use c1_const
  use c2_jflw_const
  use mod_io, only: &
        read_map_tile_margin, &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'getUpperBasin'
  integer, intent(in) :: tx, ty

  type(tile_) :: tile
  type(lst_bsn_), allocatable, target :: lst_bsn(:)
  type(lst_bsn_), pointer :: lbsn
  type(upper_), pointer :: upper
  integer(1), allocatable :: fdr(:,:)
  integer   , allocatable :: bsn(:,:)
  integer :: nBsn, nBsn_fix, nBsn_tmp, iBsn
  integer :: bsnId, bsnId0, bsnId_max
  integer :: ix, iy
  integer :: is, ie, is2, ie2, is3, ie3
  integer :: n_upper
  integer, pointer :: tmplst_tx(:), tmplst_ty(:), tmplst_id(:)
  integer, allocatable :: arg(:)
  integer :: info

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(tileinfo(tx,ty))
  !-------------------------------------------------------------
  ! Read data
  !-------------------------------------------------------------
  allocate(fdr(0:NX+1,0:NY+1))
  allocate(bsn(0:NX+1,0:NY+1))

  call read_map_tile_margin(fdr, FDR_MISS, 'dir', tx, ty, info)
  if( info /= 0 )then
    call logmsg('File not found for `dir`.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  
  call read_map_tile_margin(bsn, BSN_MISS, 'bsn_parts', tx, ty, info)
  if( info /= 0 )then
    call logmsg('File not found for `bsn_parts`.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Preparing params. and list of basins')

  if( any(bsn(1:NX,1:NY) > 0) )then
    bsnId0 = minval(bsn(1:NX,1:NY),mask=bsn(1:NX,1:NY)>0) - 1
    bsnId_max = maxval(bsn(1:NX,1:NY))
    nBsn_fix = bsnId_to_idx(maxval(bsn(1:NX,1:NY)))
    call logmsg('bsnId >0 min: '//str(bsnId0+1)//', max: '//str(bsnId_max))
  else
    bsnId0 = 0
    bsnId_max = 0
    nBsn_fix = 0
    call logmsg('bsnId >0 null')
  endif
  if( any(bsn(1:NX,1:NY) < 0 .and. bsn(1:NX,1:NY) /= BSN_MISS) )then
    nBsn_tmp = -minval(bsn(1:NX,1:NY),mask=bsn(1:NX,1:NY)/=BSN_MISS)
    call logmsg('bsnId <0 min: '//str(-nBsn_tmp))
  else
    nBsn_tmp = 0
    call logmsg('bsnId <0 null')
  endif
  call logmsg('nBsn fix: '//str(nBsn_fix)//', tmp: '//str(nBsn_tmp))
  nBsn = nBsn_fix + nBsn_tmp

  allocate(lst_bsn(nBsn))
  do iBsn = 1, nBsn
    lst_bsn(iBsn)%upper%n = 0
    lst_bsn(iBsn)%id = 0
    lst_bsn(iBsn)%done_upper_update = .false.
  enddo
  do bsnId = bsnId0+1, bsnId_max
    lst_bsn(bsnId_to_idx(bsnId))%id = bsnId
    lst_bsn(bsnId_to_idx(bsnId))%id_fix = bsnId
  enddo
  do bsnId = -nBsn_tmp, -1
    lst_bsn(bsnId_to_idx(bsnId))%id = bsnId
    lst_bsn(bsnId_to_idx(bsnId))%id_fix = 0
  enddo
  do iBsn = 1, nBsn
    if( lst_bsn(iBsn)%id == 0 )then
      call errend(msg_unexpected_condition()//&
                  ' lst_bsn('//str(iBsn)//')%id == 0')
    endif
  enddo

  ix = 1
  do iy = 1, NY
    if( bsn(ix,iy) == BSN_MISS ) cycle
    call add(lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper%n)
  enddo

  ix = NX
  do iy = 1, NY
    if( bsn(ix,iy) == BSN_MISS ) cycle
    call add(lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper%n)
  enddo

  iy = 1
  do ix = 2, NX-1
    if( bsn(ix,iy) == BSN_MISS ) cycle
    call add(lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper%n)
  enddo

  iy = NY
  do ix = 2, NX-1
    if( bsn(ix,iy) == BSN_MISS ) cycle
    call add(lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper%n)
  enddo

  do iBsn = 1, nBsn
    upper => lst_bsn(iBsn)%upper
    upper%n = upper%n * 5
    if( upper%n == 0 ) cycle
    allocate(upper%tx(upper%n))
    allocate(upper%ty(upper%n))
    allocate(upper%id(upper%n))
    upper%tx(:) = 0
    upper%ty(:) = 0
    upper%id(:) = 0
  enddo

  call logext()
  !-------------------------------------------------------------
  ! Store info. of upper pixels of edge pixels
  !-------------------------------------------------------------
  call logent('Storing info. of upper pixels of edge pixels')

  do iBsn = 1, nBsn
    lst_bsn(iBsn)%upper%n = 0
  enddo

  ix = 1
  do iy = 2, NY-1
    if( bsn(ix,iy) == BSN_MISS ) cycle
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix-1,iy-1) == FDR_SOUTHEAST )then
      call update_upper(upper, tx-1, ty, bsn(ix-1,iy-1))
    endif
    if( fdr(ix-1,iy  ) == FDR_EAST      )then
      call update_upper(upper, tx-1, ty, bsn(ix-1,iy))
    endif
    if( fdr(ix-1,iy+1) == FDR_NORTHEAST )then
      call update_upper(upper, tx-1, ty, bsn(ix-1,iy+1))
    endif
  enddo

  ix = NX
  do iy = 2, NY-1
    if( bsn(ix,iy) == BSN_MISS ) cycle
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix+1,iy-1) == FDR_SOUTHWEST )then
      call update_upper(upper, tx+1, ty, bsn(ix+1,iy-1))
    endif
    if( fdr(ix+1,iy) == FDR_WEST )then
      call update_upper(upper, tx+1, ty, bsn(ix+1,iy))
    endif
    if( fdr(ix+1,iy+1) == FDR_NORTHWEST )then
      call update_upper(upper, tx+1, ty, bsn(ix+1,iy+1))
    endif
  enddo

  iy = 1
  do ix = 2, NX-1
    if( bsn(ix,iy) == BSN_MISS ) cycle
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix-1,iy-1) == FDR_SOUTHEAST )then
      call update_upper(upper, tx, ty-1, bsn(ix-1,iy-1))
    endif
    if( fdr(ix,iy-1) == FDR_SOUTH )then
      call update_upper(upper, tx, ty-1, bsn(ix,iy-1))
    endif
    if( fdr(ix+1,iy-1) == FDR_SOUTHWEST )then
      call update_upper(upper, tx, ty-1, bsn(ix+1,iy-1))
    endif
  enddo

  iy = NY
  do ix = 2, NX-1
    if( bsn(ix,iy) == BSN_MISS ) cycle
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix-1,iy+1) == FDR_NORTHEAST )then
      call update_upper(upper, tx, ty+1, bsn(ix-1,iy+1))
    endif
    if( fdr(ix,iy+1) == FDR_NORTH )then
      call update_upper(upper, tx, ty+1, bsn(ix,iy+1))
    endif
    if( fdr(ix+1,iy+1) == FDR_NORTHWEST )then
      call update_upper(upper, tx, ty+1, bsn(ix+1,iy+1))
    endif
  enddo

  ix = 1
  iy = 1
  if( bsn(ix,iy) /= BSN_MISS )then
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix-1,iy-1) == FDR_SOUTHEAST )then
      call update_upper(upper, tx-1, ty-1, bsn(ix-1,iy-1))
    endif
    if( fdr(ix-1,iy) == FDR_EAST )then
      call update_upper(upper, tx-1, ty, bsn(ix-1,iy))
    endif
    if( fdr(ix-1,iy+1) == FDR_NORTHEAST )then
      call update_upper(upper, tx-1, ty, bsn(ix-1,iy+1))
    endif
    if( fdr(ix,iy-1) == FDR_SOUTH )then
      call update_upper(upper, tx, ty-1, bsn(ix,iy-1))
    endif
    if( fdr(ix+1,iy-1) == FDR_SOUTHWEST )then
      call update_upper(upper, tx, ty-1, bsn(ix+1,iy-1))
    endif
  endif

  ix = 1
  iy = NY
  if( bsn(ix,iy) /= BSN_MISS )then
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix-1,iy+1) == FDR_NORTHEAST )then
      call update_upper(upper, tx-1, ty+1, bsn(ix-1,iy+1))
    endif
    if( fdr(ix-1,iy) == FDR_EAST )then
      call update_upper(upper, tx-1, ty, bsn(ix-1,iy))
    endif
    if( fdr(ix-1,iy-1) == FDR_SOUTHEAST )then
      call update_upper(upper, tx-1, ty, bsn(ix-1,iy-1))
    endif
    if( fdr(ix,iy+1) == FDR_NORTH )then
      call update_upper(upper, tx, ty+1, bsn(ix,iy+1))
    endif
    if( fdr(ix+1,iy+1) == FDR_NORTHWEST )then
      call update_upper(upper, tx, ty+1, bsn(ix+1,iy+1))
    endif
  endif

  ix = NX
  iy = 1
  if( bsn(ix,iy) /= BSN_MISS )then
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix+1,iy-1) == FDR_SOUTHWEST )then
      call update_upper(upper, tx+1, ty-1, bsn(ix+1,iy-1))
    endif
    if( fdr(ix+1,iy) == FDR_WEST )then
      call update_upper(upper, tx+1, ty, bsn(ix+1,iy))
    endif
    if( fdr(ix+1,iy+1) == FDR_NORTHWEST )then
      call update_upper(upper, tx+1, ty, bsn(ix+1,iy+1))
    endif
    if( fdr(ix,iy-1) == FDR_SOUTH )then
      call update_upper(upper, tx, ty-1, bsn(ix,iy-1))
    endif
    if( fdr(ix-1,iy-1) == FDR_SOUTHEAST )then
      call update_upper(upper, tx, ty-1, bsn(ix-1,iy-1))
    endif
  endif

  ix = NX
  iy = NY
  if( bsn(ix,iy) /= BSN_MISS )then
    upper => lst_bsn(bsnId_to_idx(bsn(ix,iy)))%upper
    if( fdr(ix+1,iy+1) == FDR_NORTHWEST )then
      call update_upper(upper, tx+1, ty+1, bsn(ix+1,iy+1))
    endif
    if( fdr(ix+1,iy) == FDR_WEST )then
      call update_upper(upper, tx+1, ty, bsn(ix+1,iy))
    endif
    if( fdr(ix+1,iy-1) == FDR_SOUTHWEST )then
      call update_upper(upper, tx+1, ty, bsn(ix+1,iy-1))
    endif
    if( fdr(ix,iy+1) == FDR_NORTH )then
      call update_upper(upper, tx, ty+1, bsn(ix,iy+1))
    endif
    if( fdr(ix-1,iy+1) == FDR_NORTHEAST )then
      call update_upper(upper, tx, ty+1, bsn(ix-1,iy+1))
    endif
  endif

  call logext()
  !-------------------------------------------------------------
  ! Get IDs of upper basins
  !-------------------------------------------------------------
  call logent('Getting IDs of upper basins')

  tile%nBsn = nBsn
  tile%nBsn_fix = nBsn_fix
  tile%nBsn_tmp = nBsn_tmp
  tile%bsnId0 = bsnId0
  tile%done_upper_update = .false.
  tile%lst_bsn => lst_bsn

  n_upper = 0
  do iBsn = 1, nBsn
    n_upper = max(lst_bsn(iBsn)%upper%n, n_upper)
  enddo
  allocate(tmplst_tx(n_upper))
  allocate(tmplst_ty(n_upper))
  allocate(tmplst_id(n_upper))
  allocate(arg(n_upper))

  do iBsn = 1, nBsn
    lbsn => lst_bsn(iBsn)
    upper => lbsn%upper
    if( upper%n == 0 )then
      lbsn%done_upper_update = .true.
      cycle
    endif

    lbsn%done_upper_update = .false.

    ! Remove overwrapping components
    n_upper = 0

    !-- Sort by %tx
    call argsort(upper%tx(:upper%n), arg(:upper%n))
    call sort(upper%tx(:upper%n), arg(:upper%n))
    call sort(upper%ty(:upper%n), arg(:upper%n))
    call sort(upper%id(:upper%n), arg(:upper%n))

    ie = 0
    do while( ie < upper%n )
      !-- Sort by %ty in same (%tx)
      is = ie + 1
      ie = is
      do while( ie < upper%n )
        if( upper%tx(ie+1) /= upper%tx(is) ) exit
        ie = ie + 1
      enddo
      call argsort(upper%ty(is:ie), arg(is:ie))
      call sort(upper%ty(is:ie), arg(is:ie))
      call sort(upper%id(is:ie), arg(is:ie))

      !-- Sort by %id in same (%tx,%ty)
      ie2 = is-1
      do while( ie2 < ie )
        is2 = ie2 + 1
        ie2 = is2
        do while( ie2 < ie )
          if( upper%ty(ie2+1) /= upper%ty(is2) ) exit
          ie2 = ie2 + 1
        enddo
        call sort(upper%id(is2:ie2))

        ie3 = is2-1
        do while( ie3 < ie2 )
          is3 = ie3 + 1
          ie3 = is3
          do while( ie3 < ie2 )
            if( upper%id(ie3+1) /= upper%id(is3) ) exit
            ie3 = ie3 + 1
          enddo
          call add(n_upper)
          tmplst_tx(n_upper) = upper%tx(is3)
          tmplst_ty(n_upper) = upper%ty(is3)
          tmplst_id(n_upper) = upper%id(is3)
        enddo
      enddo  ! while ( ie2 < ie )
    enddo  ! while( ie < upper%n )

    upper%n = n_upper
    call realloc(upper%tx, upper%n, clear=.true.)
    call realloc(upper%ty, upper%n, clear=.true.)
    call realloc(upper%id, upper%n, clear=.true.)
    upper%tx = tmplst_tx(:n_upper)
    upper%ty = tmplst_ty(:n_upper)
    upper%id = tmplst_id(:n_upper)
  enddo  ! iBsn/

  deallocate(tmplst_tx)
  deallocate(tmplst_ty)
  deallocate(tmplst_id)
  deallocate(arg)

  call write_f_upper(tx, ty, tile)

  call logext()
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
integer function bsnId_to_idx(bsnId) result(iBsn)
  implicit none
  integer, intent(in) :: bsnId

  selectcase( bsnId )
  case( 1: )
    iBsn = bsnId - bsnId0
  case( :-1 )
    iBsn = bsnId_max-bsnId0 + (-bsnId)
  case( 0 )
    iBsn = 0
  endselect
end function bsnId_to_idx
!---------------------------------------------------------------
subroutine update_upper(upper, itx, ity, bsnId)
  implicit none
  type(upper_), intent(inout) :: upper
  integer, intent(in) :: itx, ity
  integer, intent(in) :: bsnId

  call add(upper%n)
  upper%tx(upper%n) = itx
  upper%ty(upper%n) = ity
  upper%id(upper%n) = bsnId
end subroutine update_upper
!---------------------------------------------------------------
end subroutine getUpperBasin
!===============================================================
! SUBROUTINE getUpperBasin must be run for all the surrounding 
! tiles. This procedures works without it, but the results will 
! not be perfect.
!
! IN:
!   ${DIR_TILED}/upper/${tilename(tx,ty)}.txt
! OUT (overwrite):
!   ${DIR_TILED}/upper/${tilename(tx,ty)}.txt
!===============================================================
subroutine updateUpperBasin(tx, ty, updated_any)
  use c1_const
  use c2_jflw_const
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'updateUpperBasin'
  integer, intent(in) :: tx, ty
  logical, intent(out), optional :: updated_any

  logical :: updated_any_

  type(tile_), target :: tile(tx-1:tx+1,ty-1:ty+1)
  type(tile_), pointer :: atile, uptile
  type(lst_bsn_), pointer :: lbsn
  type(upper_), pointer :: upper
  integer :: iBsn
  integer :: iupBsn
  integer :: itx, ity
  logical :: is_updated(tx-1:tx+1,ty-1:ty+1)
  integer :: i
  integer :: ios

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(tileinfo(tx,ty))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  updated_any_ = .false.
  atile => tile(tx,ty)

  if( read_f_upper(tx, ty, atile) /= 0 )then
    if( present(updated_any) ) updated_any = updated_any_
    call logret(PRCNAM, MODNAM)
    return
  endif

  if( atile%done_upper_update )then
    call logmsg('Upper basins have already been updated.')
    if( present(updated_any) ) updated_any = updated_any_
    call logret(PRCNAM, MODNAM)
    return
  endif

  do ity = ty-1, ty+1
  do itx = tx-1, tx+1
    if( itx == tx .and. ity == ty ) cycle
    ios = read_f_upper(itx, ity, tile(itx,ity))
  enddo
  enddo

  is_updated(:,:) = .false.
  do iBsn = 1, atile%nBsn
    lbsn => atile%lst_bsn(iBsn)
    if( lbsn%id_fix == 0 )then
      cycle
    elseif( lbsn%done_upper_update )then
      cycle
    endif
    lbsn%done_upper_update = .true.

    upper => lbsn%upper
    do i = 1, upper%n
      is_updated(upper%tx(i),upper%ty(i)) = .true.
      uptile => tile(upper%tx(i),upper%ty(i))
      uptile%done_upper_update = .false.
      iupBsn = bsnId_to_idx(upper%id(i), uptile%bsnId0, uptile%nBsn_fix, uptile%nBsn_tmp)
      call logmsg('tile('//str((/upper%tx(i),upper%ty(i)/),4,',')//')%bsn('//str(iupBsn,6)//&
                ')%id: '//str(uptile%lst_bsn(iupBsn)%id)//' -> '//str(lbsn%id_fix))
      uptile%lst_bsn(iupBsn)%id_fix = lbsn%id_fix
    enddo  ! i/
  enddo  ! iBsn/
  !-------------------------------------------------------------
  ! Update files
  !-------------------------------------------------------------
  atile%done_upper_update = .true.
  call logmsg('Overwriting:')
  call write_f_upper(tx, ty, atile)

  do ity = ty-1, ty+1
  do itx = tx-1, tx+1
    if( itx == tx .and. ity == ty ) cycle
    if( .not. is_updated(itx,ity) ) cycle
    updated_any_ = .true.
    call logmsg('Overwriting:')
    call write_f_upper(itx, ity, tile(itx,ity))
  enddo
  enddo
  !-------------------------------------------------------------
  call logmsg('updated_any: '//str(updated_any_))
  if( present(updated_any) ) updated_any = updated_any_
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
integer function bsnId_to_idx(bsnId, bsnId0, nBsn_fix, nBsn_tmp) result(iBsn)
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = '__IP__bsnId_to_idx'
  integer, intent(in) :: bsnId
  integer, intent(in) :: bsnId0
  integer, intent(in) :: nBsn_fix
  integer, intent(in) :: nBsn_tmp

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  selectcase( bsnId )
  case( 1: )
    if( bsnId > bsnId0+nBsn_fix )then
      call errend(msg_unexpected_condition()//&
                '\n  bsnId > bsnId0+nBsn_fix'//&
                '\n  bsnId          : '//str(bsnId)//&
                '\n  bsnId0+nBsn_fix: '//str(bsnId0+nBsn_fix))
    endif
    iBsn = bsnId - bsnId0
  case( :-1 )
    if( bsnId < -nBsn_tmp )then
      call errend(msg_unexpected_condition()//&
                '\n  bsnId < -nBsn_tmp'//&
                '\n  bsnId    : '//str(bsnId)//&
                '\n  -nBsn_tmp: '//str(-nBsn_tmp))
    endif
    iBsn = nBsn_fix + (-bsnId)
  case( 0 )
    iBsn = 0
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function bsnId_to_idx
!---------------------------------------------------------------
end subroutine updateUpperBasin
!===============================================================
! [NOTE] SUBROUTINE getUpperBasin must be run for all tiles 
!        beforehand.
!
! INOUT:
!   ${DIR_TILED}/upper/${tilename(tx,ty)}.txt
!     for all tiles (tx,ty)
!===============================================================
subroutine mergeAllBasins()
  use c2_jflw_const
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'mergeAllBasins'
  logical :: updated_any
  integer :: itx, ity
  logical :: updated(ntx,nty)

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  updated_any = .true.
  do while( updated_any )
    updated_any = .false.
    do ity = 1, NTY
    do itx = 1, NTX
      call updateUpperBasin(itx, ity, updated(itx,ity))
      updated_any = updated_any .or. updated(itx,ity)
    enddo  ! itx/
    enddo  ! ity/
  enddo  !while( update_any )/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine mergeAllBasins
!===============================================================
! SUBROUTINE mergeAllBasins must be run beforehand.
!
! IN:
!   ${DIR_ORG}/dir/${tilename(tx,ty)}_dir.bin
!   ${DIR_TILED}/source/${tilename(tx,ty)}.txt
! OUT:
!   ${DIR_TILED}/bsn_tmp/${tilename(tx,ty)}.bin
!===============================================================
subroutine makeTiledBasinMaps(tx, ty)
  use c1_const
  use c2_jflw_const
  use c2_jflw_grid, only: &
        get_nextxy
  use c2_jflw_io, only: &
        get_f_map_tile, &
        get_f_lst_tile
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeTiledBasinMaps'
  integer, intent(in) :: tx, ty

  type(tile_) :: atile
  integer, allocatable :: lst_bsnId(:)
  integer, allocatable :: lst_bsnId_fix(:)
  integer, allocatable :: arg_bsnId(:)
  integer :: loc
  integer :: bsnId, bsnId_prev, bsnId_fix
  integer :: ix, iy, xx, yy, x, y
  integer   , allocatable :: bsn(:,:)
  integer(1), allocatable :: fdr(:,:)

  character(CLEN_PATH) :: f_bsn
  character(CLEN_PATH) :: f_fdr
  character(CLEN_PATH) :: f_source
  integer :: un
  integer :: ios

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(tileinfo(tx,ty))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( read_f_upper(tx, ty, atile) /= 0 )then
    call logmsg('File not found')
    call logret(PRCNAM, MODNAM)
    return
  endif

  allocate(lst_bsnId(atile%nBsn))
  allocate(lst_bsnId_fix(atile%nBsn))
  allocate(arg_bsnId(atile%nBsn))
  lst_bsnId(:) = atile%lst_bsn(:)%id
  lst_bsnId_fix(:) = atile%lst_bsn(:)%id_fix
  call argsort(lst_bsnId, arg_bsnId)

  allocate(bsn(NX,NY))
  allocate(fdr(NX,NY))
  f_fdr = get_f_map_tile(RESOLUTION_1SEC, 'dir', tx, ty)
  call traperr( rbin(fdr, f_fdr) )
  !$omp parallel do
  do iy = 1, NY
  do ix = 1, NX
    if( fdr(ix,iy) == FDR_MISS )then
      bsn(ix,iy) = BSN_MISS
    else
      bsn(ix,iy) = BSN_UNDEF
    endif
  enddo
  enddo
  !$omp endparallel do

  f_source = get_f_lst_tile(RESOLUTION_1SEC, 'source', tx, ty)
  call logmsg('Reading '//str(f_source))
  open(newunit=un, file=f_source, status='old')
  read(un,*)

  bsnId_prev = BSN_MISS
  do
    read(un,*,iostat=ios) bsnId, x, y
    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      exit
    case default
      call errend(msg_io_error(f=f_source))
    endselect

    if( bsnId == 0 )then
      call errend(msg_unexpected_condition()//&
                '\n  bsnId == 0')
    endif

    if( bsnId > 0 )then
      bsnId_fix = bsnId
    else
      if( bsnId /= bsnId_prev )then
        call search(bsnId, lst_bsnId, arg_bsnId, loc)
        bsnId_fix = lst_bsnId_fix(arg_bsnId(loc))
        !call logmsg('bsnId '//str(bsnId)//' -> '//str(bsnId_fix))
      endif
    endif
    bsnId_prev = bsnId

    ix = x
    iy = y
    do while( bsn(ix,iy) <= 0 )
      bsn(ix,iy) = bsnId_fix
      call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
      if( xx < 1 .or. xx > NX .or. yy < 1 .or. yy > NY ) exit
      ix = xx
      iy = yy
    enddo
  enddo  ! EOF/
  close(un)

  do iy = 1, NY
  do ix = 1, NX
    if( fdr(ix,iy) == FDR_MISS )then
      if( bsn(ix,iy) /= BSN_MISS )then
        call errend(msg_unexpected_condition()//&
                  '\n  bsn('//str((/ix,iy/),4,',')//') /= BSN_MISS and fdr == FDR_MISS')
      endif
    else
      if( bsn(ix,iy) <= 0 )then
        call errend(msg_unexpected_condition()//&
                  '\n  bsn('//str((/ix,iy/),4,',')//') <= 0 and fdr /= FDR_MISS')
      endif
    endif
  enddo
  enddo

  f_bsn = get_f_map_tile(RESOLUTION_1SEC, 'bsn_tmp', tx, ty)
  call traperr( mkdir(dirname(f_bsn)) )
  call logmsg('Writing '//str(f_bsn))
  call traperr( wbin(bsn, f_bsn) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeTiledBasinMaps
!===============================================================
! SUBROUTINE makeTiledBasinMaps must be run for all the tiles 
! beforehand.
!
! IN:
!   ${DIR_ORG}/dir/${tilename(itx,ity)}_dir.bin
!   ${DIR_TILED}/bsn_tmp/${tilename(itx,ity)}.bin
!     for itx in (itx-1,itx+1), ity in (ity-1,ity+1)
!===============================================================
subroutine checkBasins(tx, ty)
  use c1_const
  use c2_jflw_const
  use c2_jflw_grid, only: &
        get_nextxy
  use mod_io, only: &
        read_map_tile_margin, &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'checkBasins'
  integer, intent(in) :: tx, ty

  integer(1), allocatable :: fdr(:,:)
  integer   , allocatable :: bsn(:,:)
  integer :: ix, iy, xx, yy
  integer :: info

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(tileinfo(tx,ty))
  !-------------------------------------------------------------
  ! Read data
  !-------------------------------------------------------------
  allocate(fdr(0:NX+1,0:NY+1))
  allocate(bsn(0:NX+1,0:NY+1))

  call read_map_tile_margin(fdr, FDR_MISS, 'dir', tx, ty, info)
  if( info /= 0 )then
    call logmsg('No data.')
    call logret(PRCNAM, MODNAM)
    return
  endif

  call read_map_tile_margin(bsn, BSN_MISS, 'bsn_tmp', tx, ty, info)
  if( info /= 0 )then
    call logmsg('No data.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  !-------------------------------------------------------------
  ! Check data
  !-------------------------------------------------------------
  ix = 1
  iy = 1
  do iy = 1, NY
  do ix = 1, NX
    if( fdr(ix,iy) == FDR_MISS )then
      if( bsn(ix,iy) /= BSN_MISS )then
        call errend(msg_unexpected_condition()//&
                  '\n  @('//str((/ix,iy/),4,',')//') '//&
                    'fdr == FDR_MISS and bsn /= BSN_MISS')
      endif
      cycle
    endif
    call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
    if( xx == XY_RIVERMOUTH .or. xx == XY_INLAND ) cycle
    if( bsn(xx,yy) /= bsn(ix,iy) )then
      call errend(msg_unexpected_condition()//&
                '\n  @('//str((/ix,iy/),4,',')//') '//&
                  'Basin id mismatch')
    endif
  enddo  ! ix/
  enddo  ! iy/
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine checkBasins
!===============================================================
! REQUIREMENTS:
!   SUBROUTINE makeTiledBasinMaps must be run for all the tiles 
!   beforehand.
!
! IN:
!   ${DIR_TILED}/id_all.txt
!   ${DIR_TILED}/bsn_tmp/${tilename(tx,ty)}.bin
!     for all tiles (tx,ty)
! OUT:
!   ${DIR_TILED}/id_area.txt
!===============================================================
subroutine renewBasinList()
  use c1_const
  use c2_jflw_const
  use c2_jflw_io, only: &
        tilename      , &
        get_f_map_tile, &
        get_f_lst_tile
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'renewBasinList'

  integer, allocatable :: bsn(:,:)
  real(8), allocatable :: bsn1d_ara(:)
  integer, allocatable :: bsn1d_kxmin(:), bsn1d_kxmax(:)
  integer, allocatable :: bsn1d_kymin(:), bsn1d_kymax(:)
  integer, allocatable :: arg(:)
  real(8), allocatable :: lat(:)
  real(8), allocatable :: rstara(:,:)
  integer :: nBsn, iBsn, iiBsn
  integer :: bsnId
  integer :: itx, ity
  integer :: ix, iy
  integer :: kx, ky
  real(8) :: lat_north

  character(CLEN_PATH) :: f_id_all
  character(CLEN_PATH) :: f_bsn
  character(CLEN_PATH) :: f_lst_id_area
  integer :: un
  integer :: ios
  integer :: dgt_id

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_id_all = get_f_lst_tile(RESOLUTION_1SEC, 'id_all')
  open(newunit=un, file=f_id_all, status='old')
  read(un,*)

  nBsn = 0
  do
    read(un,*,iostat=ios)
    if( ios == -1 ) exit
    nBsn = nBsn + 1
  enddo
  close(un)

  call logmsg('nBsn: '//str(nBsn))

  dgt_id = dgt(nBsn)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Calculating basin area')

  allocate(rstara(NY,NTY))
  allocate(lat(0:NY))
  do ity = 1, NTY
    lat_north = REGION_NORTH - TILESIZE_LAT*(ity-1)
    call logmsg('ity='//str(ity)//' lat: '//str(int((/lat_north,lat_north-1/)),' - '))
    do iy = 0, NY
      lat(iy) = lat_north - TILESIZE_LAT*iy/NY
    enddo
    call showarr(lat, 'lat')
    rstara(:,ity) = area_sphere_rect(lat(0:NY-1)*d2r, lat(1:NY)*d2r) &
                     !* (rad_360deg/3.6d2/NX) * (EARTH_R*1d-3)**2
                     * (GRIDSIZE_LON*d2r) * EARTH_R**2
    call showarr(rstara(:,ity), 'rstara')
  enddo
  deallocate(lat)

  allocate(bsn(NX,NY))

  allocate(bsn1d_ara(nBsn))
  bsn1d_ara(:) = 0.d0

  allocate(bsn1d_kxmin(nBsn))
  allocate(bsn1d_kxmax(nBsn))
  allocate(bsn1d_kymin(nBsn))
  allocate(bsn1d_kymax(nBsn))
  bsn1d_kxmin(:) = ntx*NX
  bsn1d_kxmax(:) = 1
  bsn1d_kymin(:) = nty*NY
  bsn1d_kymax(:) = 1

  do ity = 1, nty
  do itx = 1, ntx
    f_bsn = get_f_map_tile(RESOLUTION_1SEC, 'bsn_tmp', itx, ity)
    if( access(f_bsn,' ') /= 0 ) cycle
    call logmsg('Tile '//tilename(itx,ity)//' ('//str((/itx,ity/),2)//')')
    call traperr( rbin(bsn, f_bsn) )

    do iy = 1, NY
    do ix = 1, NX
      bsnId = bsn(ix,iy)
      if( bsnId == BSN_MISS ) cycle
      call add(bsn1d_ara(bsnId), rstara(iy,ity))
      kx = NX*(itx-1)+ix
      ky = NY*(ity-1)+iy
      bsn1d_kxmin(bsnId) = min(bsn1d_kxmin(bsnId), kx)
      bsn1d_kxmax(bsnId) = max(bsn1d_kxmax(bsnId), kx)
      bsn1d_kymin(bsnId) = min(bsn1d_kymin(bsnId), ky)
      bsn1d_kymax(bsnId) = max(bsn1d_kymax(bsnId), ky)
    enddo
    enddo
  enddo
  enddo

  deallocate(rstara)

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logent('Setting new ids')

  allocate(arg(nBsn))
  call argsort(bsn1d_ara, arg)
  call reverse(arg)

  do iiBsn = 1, 15
    iBsn = arg(iiBsn)
    call logmsg('id '//str(iBsn,dgt_id)//' -> '//str(iiBsn,dgt_id)//&
              ' area '//str(bsn1d_ara(iBsn),'es12.5'))
  enddo
  call logmsg('...')
  do iiBsn = nBsn-14, nBsn
    iBsn = arg(iiBsn)
    call logmsg('id '//str(iBsn,dgt_id)//' -> '//str(iiBsn,dgt_id)//&
                ' area '//str(bsn1d_ara(iBsn),'es12.5'))
  enddo

  call logext()
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_lst_id_area = get_f_lst_tile(RESOLUTION_1SEC, 'id_area')
  call logmsg('Writing '//str(f_lst_id_area))
  open(newunit=un, file=f_lst_id_area, status='replace')
  write(un,"(1x,a,1x,i0)") 'nBsn', nBsn
  write(un,"(1x,a)") 'id_old id_new area(m2) kxmin kxmax kymin kymax'
  do iiBsn = 1, nBsn
    iBsn = arg(iiBsn)
    write(un,"(2(1x,i"//str(dgt_id)//"),1x,es12.5,4(1x,i8))") &
          iBsn, iiBsn, bsn1d_ara(iBsn), &
          bsn1d_kxmin(iBsn), bsn1d_kxmax(iBsn), &
          bsn1d_kymin(iBsn), bsn1d_kymax(iBsn)
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine renewBasinList
!===============================================================
! REQUIREMENTS:
!   SUBROUTINE renewBasinList must be run beforehand.
!
! IN:
!   ${DIR_ORG}/dir/${tilename(tx,ty)}_dir.bin
!   ${DIR_TILED}/bsn_tmp/${tilename(nx,ny)}.bin
!   ${DIR_TILED}/id_area.txt
!   ${DIR_TILED}/source/${tilename(tx,ty)}.txt
! OUT:
!   ${DIR_TILED}/bsn/${tilename(tx,ty)}.bin
!===============================================================
subroutine makeNewBasinMaps(tx, ty)
  use c1_const
  use c2_jflw_const
  use c2_jflw_grid, only: &
        get_nextxy
  use c2_jflw_io, only: &
        get_f_map_tile, &
        get_f_lst_tile
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeNewBasinMaps'
  integer, intent(in) :: tx, ty

  integer   , allocatable :: bsn(:,:)
  integer(1), allocatable :: fdr(:,:)
  integer, allocatable :: bsn1d_id_old(:)
  integer, allocatable :: bsn1d_id_new(:)
  integer, allocatable :: arg(:)
  integer :: nBsn, iBsn
  integer :: bsnId_new
  integer :: ix, iy, xx, yy
  integer :: i_
  real(8) :: r_
  character(1) :: c_

  character(CLEN_PATH) :: f_lst_id_area
  character(CLEN_PATH) :: f_source
  character(CLEN_PATH) :: f_fdr
  character(CLEN_PATH) :: f_bsn_old
  character(CLEN_PATH) :: f_bsn_new
  integer :: un
  integer :: ios

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call logmsg(tileinfo(tx,ty))
  !-------------------------------------------------------------
  ! Update basin ids
  !-------------------------------------------------------------
  f_fdr = get_f_map_tile(RESOLUTION_1SEC, 'dir', tx, ty)
  if( access(f_fdr,' ') /= 0 )then
    call logmsg('File not found.')
    call logret(PRCNAM, MODNAM)
    return
  endif
  allocate(fdr(NX,NY))
  call traperr( rbin(fdr, f_fdr) )

  f_bsn_old = get_f_map_tile(RESOLUTION_1SEC, 'bsn_tmp', tx, ty)
  allocate(bsn(NX,NY))
  call traperr( rbin(bsn, f_bsn_old) )

  f_lst_id_area = get_f_lst_tile(RESOLUTION_1SEC, 'id_area')
  call logmsg('Reading '//str(f_lst_id_area))
  open(newunit=un, file=f_lst_id_area, status='old')
  read(un,*) c_, nBsn
  allocate(bsn1d_id_old(nBsn))
  allocate(bsn1d_id_new(nBsn))
  read(un,*)
  do iBsn = 1, nBsn
    read(un,*) bsn1d_id_old(iBsn), bsn1d_id_new(iBsn), r_
  enddo
  close(un)

  allocate(arg(nBsn))
  call argsort(bsn1d_id_old, arg)
  call sort(bsn1d_id_old, arg)
  call sort(bsn1d_id_new, arg)
  deallocate(arg)
  call showarr(bsn1d_id_old,'id_old')
  call showarr(bsn1d_id_new,'id_new')

  f_source = get_f_lst_tile(RESOLUTION_1SEC, 'source', tx, ty)
  open(newunit=un, file=f_source, status='old')
  read(un,*)
  do
    read(un,*,iostat=ios) i_, ix, iy
    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      exit
    case default
      call errend(msg_io_error(f=f_source))
    endselect

    iBsn = bsn(ix,iy)
    bsnId_new = bsn1d_id_new(iBsn)
    do while( bsn(ix,iy) /= bsnId_new )
      bsn(ix,iy) = bsnId_new
      call get_nextxy(ix, iy, fdr(ix,iy), xx, yy)
      if( xx < 1 .or. xx > NX .or. yy < 1 .or. yy > NY ) exit
      ix = xx
      iy = yy
    enddo
  enddo  ! EOF/
  close(un)

  f_bsn_new = get_f_map_tile(RESOLUTION_1SEC, 'bsn', tx, ty)
  call logmsg('Writing '//str(f_bsn_new))
  call traperr( wbin(bsn, f_bsn_new) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeNewBasinMaps
!===============================================================
! FUNCTION:
! - To make a list of id and range of basins in each tile.
! REQUIREMENTS:
! - 
! IN:
! - ${DIR_TILED}/id_all.txt
! - ${DIR_TILED}/basin_final/${tilename(tx,ty)}.txt
! OUT:
! - ${DIR_TILED}/basin_range/${tilename(tx,ty)}.txt
! - ${DIR_TILED}/basin_range/all.txt
!===============================================================
subroutine makeTiledBasinLists()
  use c1_const
  use c2_jflw_const
  use c2_jflw_grid, only: &
        xy_to_gxy  , &
        west_of_gx , &
        east_of_gx , &
        south_of_gy, &
        north_of_gy
  use c2_jflw_io, only: &
        get_f_map_tile, &
        get_f_lst_tile
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeTiledBasinLists'

  integer(4), allocatable :: bsn(:,:)
  integer(1), allocatable :: exist_bsnId(:)
  integer, allocatable :: lst_bsnId(:)
  integer :: bsnId_max, &
             bsnId_min_this, bsnId_max_this, bsnId
  integer :: nBsn, iBsn
  integer :: bsnId_prev
  integer :: itx, ity
  integer :: ix, iy
  integer :: gx, gy
  integer :: i

  type basin_
    integer :: n
    integer, pointer :: tx(:), ty(:)
    integer, pointer :: xi(:), xf(:), yi(:), yf(:)
    integer :: nx, ny
    integer :: gxi, gxf, gyi, gyf
  end type

  type(basin_), allocatable, target :: lst_basin(:)
  type(basin_), pointer :: basin

  character(CLEN_PATH) :: f_id_all
  character(CLEN_PATH) :: f_bsn
  character(CLEN_PATH) :: f_basin_range_tile, f_basin_range
  integer :: un
  integer :: ios

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_id_all = trim(DIR_TILED)//'/id_all.txt'
  open(newunit=un, file=f_id_all, status='old')
  read(un,*)
  do
    read(un,*,iostat=ios) bsnId_max
    selectcase( ios )
    case( 0 )
      continue
    case( -1 )
      exit
    case default
      call errend(msg_io_error(f=f_id_all))
    endselect
  enddo
  close(un)

  call logmsg('bsnId max: '//str(bsnId_max))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(lst_basin(bsnId_max))
  do bsnId = 1, bsnId_max
    basin => lst_basin(bsnId)
    basin%n = 0
    allocate(basin%tx(1))
    allocate(basin%ty(1))
    allocate(basin%xi(1))
    allocate(basin%xf(1))
    allocate(basin%yi(1))
    allocate(basin%yf(1))
  enddo

  allocate(bsn(NX,NY))

  do ity = 1, NTY
  do itx = 1, NTX
    call logent(tileinfo(itx,ity))
    !-----------------------------------------------------------
    ! Read basin id map
    !-----------------------------------------------------------
    f_bsn = get_f_map_tile(RESOLUTION_1SEC, 'bsn', itx, ity)

    if( access(f_bsn,' ') /= 0 )then
      call logmsg('File not found: '//str(f_bsn))
      call logext()
      cycle
    endif

    call logmsg('Reading '//str(f_bsn))
    call traperr( rbin(bsn, f_bsn) )

    bsnId_min_this = minval(bsn,mask=bsn/=BSN_MISS)
    bsnId_max_this = maxval(bsn,mask=bsn/=BSN_MISS)
    call logmsg('bsnId: '//str(bsnId_min_this)//' - '//str(bsnId_max_this))
    !-----------------------------------------------------------
    ! Make a list of basin id
    !-----------------------------------------------------------
    allocate(exist_bsnId(bsnId_min_this:bsnId_max_this))
    exist_bsnId(:) = 0_1

    do iy = 1, NY
    do ix = 1, NX
      if( bsn(ix,iy) == BSN_MISS ) cycle
      exist_bsnId(bsn(ix,iy)) = 1_1
    enddo
    enddo

    nBsn = 0
    do bsnId = bsnId_min_this, bsnId_max_this
      if( exist_bsnId(bsnId) == 1_1 ) call add(nBsn)
    enddo

    allocate(lst_bsnId(nBsn))
    iBsn = 0
    do bsnId = bsnId_min_this, bsnId_max_this
      if( exist_bsnId(bsnId) == 1_1 )then
        call add(iBsn)
        lst_bsnId(iBsn) = bsnId
      endif
    enddo
    !-----------------------------------------------------------
    ! Get ranges of the basins
    !-----------------------------------------------------------
    do iBsn = 1, nBsn
      basin => lst_basin(lst_bsnId(iBsn))
      call add(basin%n)
      if( basin%n > size(basin%tx) )then
        call realloc(basin%tx, basin%n*2, clear=.false.)
        call realloc(basin%ty, basin%n*2, clear=.false.)
        call realloc(basin%xi, basin%n*2, clear=.false.)
        call realloc(basin%xf, basin%n*2, clear=.false.)
        call realloc(basin%yi, basin%n*2, clear=.false.)
        call realloc(basin%yf, basin%n*2, clear=.false.)
      endif
      basin%tx(basin%n) = itx
      basin%ty(basin%n) = ity
      basin%xi(basin%n) = NX
      basin%xf(basin%n) = 1
      basin%yi(basin%n) = NY
      basin%yf(basin%n) = 1
    enddo

    bsnId_prev = BSN_MISS
    do iy = 1, NY
    do ix = 1, NX
      if( bsn(ix,iy) == BSN_MISS ) cycle
      basin => lst_basin(bsn(ix,iy))
      basin%xi(basin%n) = min(basin%xi(basin%n),ix)
      basin%xf(basin%n) = max(basin%xf(basin%n),ix)
      basin%yi(basin%n) = min(basin%yi(basin%n),iy)
      basin%yf(basin%n) = max(basin%yf(basin%n),iy)
    enddo  ! ix/
    enddo  ! iy/
    !-----------------------------------------------------------
    ! Output
    !-----------------------------------------------------------
    f_basin_range_tile = get_f_lst_tile(RESOLUTION_1SEC, 'basin_range', itx, ity)
    call logmsg('Writing '//str(f_basin_range_tile))
    open(newunit=un, file=f_basin_range_tile, status='replace')
    write(un,"(3(1x,i0))") nBsn, bsnId_min_this, bsnId_max_this
    do iBsn = 1, nBsn
      basin => lst_basin(lst_bsnId(iBsn))
      write(un,"(1x,i10,4(1x,i4))") &
            lst_bsnId(iBsn), &
            basin%xi(basin%n), basin%xf(basin%n), &
            basin%yi(basin%n), basin%yf(basin%n)
    enddo
    close(un)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    deallocate(lst_bsnId)
    deallocate(exist_bsnId)
    !-----------------------------------------------------------
    call logext()
  enddo  ! itx/
  enddo  ! ity/

  deallocate(bsn)
  !-------------------------------------------------------------
  ! Output
  !-------------------------------------------------------------
  f_basin_range = get_f_lst_tile(RESOLUTION_1SEC, 'basin_range')
  call logmsg('Writing '//str(f_basin_range))
  open(newunit=un, file=f_basin_range, status='replace')
  write(un,"(1x,i10)") bsnId_max
  do bsnId = 1, bsnId_max
    basin => lst_basin(bsnId)
    basin%gxi = NTX*NX
    basin%gxf = 1
    basin%gyi = NTY*NY
    basin%gyf = 1
    do i = 1, basin%n
      call xy_to_gxy(basin%tx(i), basin%ty(i), basin%xi(i), basin%yi(i), gx, gy)
      basin%gxi = min(basin%gxi, gx)
      basin%gyi = min(basin%gyi, gy)
      call xy_to_gxy(basin%tx(i), basin%ty(i), basin%xf(i), basin%yf(i), gx, gy)
      basin%gxf = max(basin%gxf, gx)
      basin%gyf = max(basin%gyf, gy)
    enddo
    basin%nx = basin%gxf - basin%gxi + 1
    basin%ny = basin%gyf - basin%gyi + 1

    write(un,"(1x,i10,1x,i4,2(1x,i6),4(1x,i6))") &
          bsnId, basin%n, &
          basin%nx, basin%ny, &
          basin%gxi, basin%gxf, basin%gyi, basin%gyf
    write(un,"(1x,4(1x,f12.7))") &
          west_of_gx(basin%gxi), east_of_gx(basin%gxf), &
          south_of_gy(basin%gyf), north_of_gy(basin%gyi)
    do i = 1, basin%n
      write(un,"(1x,i10,2(1x,i2),4(1x,i4))") &
            i, basin%tx(i), basin%ty(i), &
            basin%xi(i), basin%xf(i), &
            basin%yi(i), basin%yf(i)
    enddo
  enddo  ! bsnId/
  close(un)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(lst_basin)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeTiledBasinLists
!===============================================================
! IN:
! - ${DIR_TILED}/basin_range/all.txt
! OUT:
! - ${DIR_BASIN}/1sec/range/${bsnId}.txt
!     for bsnId in (bsnId_min,bsnId_max)
!===============================================================
subroutine makeBasinRangeList(bsnId_min, bsnId_max)
  use c2_jflw_const
  use c2_jflw_io, only: &
        get_f_lst_tile , &
        get_f_dat_basin
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeBasinRangeList'
  integer, intent(in) :: bsnId_min, bsnId_max

  integer :: bsnId
  integer :: n, i
  integer :: mx, my
  integer :: gxi, gxf, gyi, gyf
  integer :: tx, ty
  integer :: xi, xf, yi, yf
  real(8) :: west, east, south, north
  integer :: i_
  integer :: un, un_out
  character(CLEN_PATH) :: f, f_range

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f = get_f_lst_tile(RESOLUTION_1SEC, 'basin_range')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*)
  do bsnId = 1, bsnId_min-1
    read(un,*) i_, n
    read(un,*)
    do i = 1, n
      read(un,*)
    enddo
  enddo

  do bsnId = bsnId_min, bsnId_max
    f_range = get_f_dat_basin(RESOLUTION_1SEC, 'range', bsnId)
    if( bsnId <= bsnId_min+1 .or. bsnId >= bsnId_max-1 )then
      call logmsg('Writing '//str(f_range))
    elseif( bsnId == bsnId_min+2 )then
      call logmsg('...')
    endif
    open(newunit=un_out, file=f_range, status='replace')
    read(un,*) i_, n, mx, my, gxi, gxf, gyi, gyf
    read(un,*) west, east, south, north
    write(un_out,"(a)") 'nx '//str(mx,DGT_GXY)
    write(un_out,"(a)") 'ny '//str(my,DGT_GXY)
    write(un_out,"(a)") 'gx '//str((/gxi,gxf/),DGT_GXY)
    write(un_out,"(a)") 'gy '//str((/gyi,gyf/),DGT_GXY)
    write(un_out,"(a)") 'west  '//str(west,'f20.15')
    write(un_out,"(a)") 'east  '//str(east,'f20.15')
    write(un_out,"(a)") 'south '//str(south,'f20.15')
    write(un_out,"(a)") 'north '//str(north,'f20.15')
    write(un_out,"(1x,i0)") n
    do i = 1, n
      read(un,*) i_, tx, ty, xi, xf, yi, yf
      write(un_out,"(2(1x,i2),4(1x,i4))") tx, ty, xi, xf, yi, yf
    enddo
    close(un_out)
  enddo

  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeBasinRangeList
!===============================================================
! IN:
! - ${DIR_TILED}/bsn/${tilename(tx,ty)}.bin
! OUT:
! - ${DIR_TILED}/bsn_lres/${tilename(tx,ty)}.bin
!===============================================================
subroutine makeLowresBasinMaps(tx, ty)
  use c1_const
  use c2_jflw_const
  use c2_jflw_io, only: &
        get_f_map_tile, &
        get_f_lst_tile
  use mod_io, only: &
        tileinfo
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeLowresBasinMaps'
  integer, intent(in) :: tx, ty

  integer, allocatable :: bsn(:,:)
  integer, allocatable :: bsn_lres(:,:)
  integer :: nId, iId
  integer :: ixx, iyy
  integer :: xi, xf, yi, yf
  integer :: ix, iy
  integer, allocatable :: lst_id(:)
  integer, allocatable :: lst_nGrid(:)
  integer :: nGrid_max
  integer :: id_nGrid_max
  character(CLEN_PATH) :: f_bsn, f_bsn_lres

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_bsn = get_f_map_tile(RESOLUTION_1SEC, 'bsn', tx, ty)
  if( access(f_bsn,' ') /= 0 )then
    call logmsg('File not found.')
    call logret(PRCNAM, MODNAM)
    return
  endif

  allocate(bsn(NX,NY))
  allocate(bsn_lres(NX/RATIO_LRES,NY/RATIO_LRES))
  call traperr( rbin(bsn, f_bsn) )

  allocate(lst_id(RATIO_LRES**2))
  allocate(lst_nGrid(RATIO_LRES**2))

  bsn_lres(:,:) = BSN_MISS

  do iyy = 1, NY/RATIO_LRES
    yi = (iyy-1)*RATIO_LRES+1
    yf = iyy*RATIO_LRES
    do ixx = 1, NX/RATIO_LRES
      xi = (ixx-1)*RATIO_LRES+1
      xf = ixx*RATIO_LRES

      nId = 0
      lst_nGrid(:) = 0
      do iy = yi, yf
        do ix = xi, xf
          if( bsn(ix,iy) == BSN_MISS ) cycle
          iId = 1
          do while( iId <= nId )
            if( lst_id(iId) == bsn(ix,iy) ) exit
            call add(iId)
          enddo
          if( iId > nId )then
            call add(nId)
            lst_id(nId) = bsn(ix,iy)
          endif
          call add(lst_nGrid(iId),1)
        enddo  ! ix/
      enddo  ! iy/

      if( nId == 0 ) cycle

      if( sum(lst_nGrid(:nId)) < RATIO_LRES**2/2.0 ) cycle

      nGrid_max = maxval(lst_nGrid(:nId))
      id_nGrid_max = maxval(lst_id(:nId))
      do iId = 1, nId
        if( lst_nGrid(iId) == nGrid_max )then
          id_nGrid_max = min(id_nGrid_max,lst_id(iId))
        endif
      enddo

      bsn_lres(ixx,iyy) = id_nGrid_max
    enddo  ! ixx/
  enddo  ! iyy/

  f_bsn_lres = get_f_map_tile(RESOLUTION_1SEC, 'bsn_lres', tx, ty)
  call traperr( mkdir(dirname(f_bsn_lres)) )
  call logmsg('Writing '//str(f_bsn_lres))
  call traperr( wbin(bsn_lres, f_bsn_lres) )
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeLowresBasinMaps
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
subroutine write_f_upper(tx, ty, tile)
  use c2_jflw_const
  use c2_jflw_io, only: &
        get_f_lst_tile
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'write_f_upper'
  integer, intent(in) :: tx, ty
  type(tile_), intent(in) :: tile

  type(lst_bsn_), pointer :: lbsn
  integer :: iBsn
  character(CLEN_PATH) :: f_upper
  integer :: un

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_upper = get_f_lst_tile(RESOLUTION_1SEC, 'upper', tx, ty)
  call logmsg('Writing '//trim(f_upper))
  open(newunit=un, file=f_upper, status='replace')
  write(un,"(1(1x,a,1x,i0),1x,a,1x,l1)") &
        'nBasin', tile%nBsn, 'done_upper_updated', tile%done_upper_update
  write(un,"(3(1x,a,1x,i0))") &
        'bsnId0', tile%bsnId0, 'nBsn_fix', tile%nBsn_fix, 'nBsn_tmp', tile%nBsn_tmp
  do iBsn = 1, tile%nBsn
    lbsn => tile%lst_bsn(iBsn)
    write(un,"(3(1x,a,1x,i8),1x,a,1x,l1)") &
          'id_self', lbsn%id, 'id_fix', lbsn%id_fix, 'n_upper', lbsn%upper%n, &
          'done_upper_update', lbsn%done_upper_update
    if( lbsn%upper%n > 0 )then
      write(un,"(1x,a,"//str(lbsn%upper%n)//"(1x,i4))") 'tx', lbsn%upper%tx(:lbsn%upper%n)
      write(un,"(1x,a,"//str(lbsn%upper%n)//"(1x,i4))") 'ty', lbsn%upper%ty(:lbsn%upper%n)
      write(un,"(1x,a,"//str(lbsn%upper%n)//"(1x,i8))") 'id', lbsn%upper%id(:lbsn%upper%n)
    endif
  enddo  ! iupBsn/
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine write_f_upper
!===============================================================
!
!===============================================================
integer function read_f_upper(tx, ty, tile) result(iret)
  use c2_jflw_const
  use c2_jflw_io, only: &
        get_f_lst_tile
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'read_f_upper'
  integer, intent(in) :: tx, ty
  type(tile_), intent(out) :: tile

  type(lst_bsn_), pointer :: lbsn
  integer :: iBsn
  character(1) :: c_
  character(CLEN_PATH) :: f_upper
  integer :: un

  call logbgn(PRCNAM, MODNAM, '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  iret = 0

  f_upper = get_f_lst_tile(RESOLUTION_1SEC, 'upper', tx, ty)
  call logmsg('Reading '//str(f_upper))
  if( access(f_upper,' ') /= 0 )then
    iret = -1
    call logret(PRCNAM, MODNAM)
    return
  endif

  open(newunit=un, file=f_upper, status='old')
  read(un,*) c_, tile%nBsn, c_, tile%done_upper_update
  read(un,*) c_, tile%bsnId0, c_, tile%nBsn_fix, c_, tile%nBsn_tmp
  allocate(tile%lst_bsn(tile%nBsn))
  do iBsn = 1, tile%nBsn
    lbsn => tile%lst_bsn(iBsn)
    read(un,*) c_, lbsn%id, c_, lbsn%id_fix, c_, lbsn%upper%n, c_, lbsn%done_upper_update
    if( lbsn%upper%n == 0 ) cycle
    allocate(lbsn%upper%tx(lbsn%upper%n))
    allocate(lbsn%upper%ty(lbsn%upper%n))
    allocate(lbsn%upper%id(lbsn%upper%n))
    read(un,*) c_, lbsn%upper%tx(:)
    read(un,*) c_, lbsn%upper%ty(:)
    read(un,*) c_, lbsn%upper%id(:)
  enddo
  close(un)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function read_f_upper
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! IN:
! - ${DIR_BASIN}/1sec/range/${bsnId}.txt
! - ${DIR_TILED}/bsn/${tilename(tx,ty)}.bin
!     for the sets of (tx,ty) that overlap with bbox of the basin
! OUT:
! - ${DIR_BASIN}/1sec/${var}/${id}.bin
!===============================================================
subroutine makeBasinTopoMap(var, bsnId)
  use c2_jflw_const
  use c2_jflw_io, only: &
        read_basin_range_from_each, &
        read_basin_map_from_tile  , &
        get_f_map_basin         
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'makeBasinTopoMap'
  character(*), intent(in) :: var
  integer     , intent(in) :: bsnId

  integer(4), allocatable :: bsn(:,:)
  integer(1), allocatable :: dati1(:,:)
  integer(4), allocatable :: dati4(:,:)
  real(4)   , allocatable :: datr4(:,:)
  integer(1) :: missi1
  integer(4) :: missi4
  real(4)    :: missr4
  integer :: gxi, gxf, gyi, gyf, igx, igy
  real(8) :: west, east, south, north
  character(CLEN_PATH) :: f

  call logbgn(PRCNAM, MODNAM)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_basin_range_from_each(&
         RESOLUTION_1SEC, bsnId, &
         gxi, gxf, gyi, gyf, &
         west, east, south, north)

  allocate(bsn(gxi:gxf,gyi:gyf))
  call read_basin_map_from_tile(&
         RESOLUTION_1SEC, bsnId, 'bsn', bsn, DTYPE_INT4, gxi, gyi, BSN_MISS)
  do igy = gyi, gyf
  do igx = gxi, gxf
    if( bsn(igx,igy) /= bsnId ) bsn(igx,igy) = BSN_MISS
  enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( var )
  !-------------------------------------------------------------
  ! Case: Int1 (dir)
  case( 'dir' )
    missi1 = FDR_MISS

    allocate(dati1(gxi:gxf,gyi:gyf))
    call read_basin_map_from_tile(&
           RESOLUTION_1SEC, bsnId, var, dati1, DTYPE_INT1, gxi, gyi, missi1, bsn)
    f = get_f_map_basin(RESOLUTION_1SEC, var, bsnId)
    call logmsg('Writing '//str(f))
    call traperr( mkdir(dirname(f)) )
    call traperr( wbin(dati1, f) )
    deallocate(dati1)
  !-------------------------------------------------------------
  ! Case: Int4 (upg, bsn)
  case( 'upg', 'bsn' )
    selectcase( var )
    case( 'upg' )
      missi4 = UPG_MISS
    case( 'bsn' )
      missi4 = BSN_MISS
    case default
      call errend(msg_invalid_value('var', var))
    endselect

    allocate(dati4(gxi:gxf,gyi:gyf))
    call read_basin_map_from_tile(&
           RESOLUTION_1SEC, bsnId, var, dati4, DTYPE_INT4, gxi, gyi, missi4, bsn)
    f = get_f_map_basin(RESOLUTION_1SEC, var, bsnId)
    call traperr( mkdir(dirname(f)) )
    call logmsg('Writing '//str(f))
    call traperr( wbin(dati4, f) )
    deallocate(dati4)
  !-------------------------------------------------------------
  ! Case: Real (elv, upa, wth)
  case( 'elv', 'upa', 'wth' )
    selectcase( var )
    case( 'elv' )
      missr4 = ELV_MISS
    case( 'upa' )
      missr4 = UPA_MISS
    case( 'wth' )
      missr4 = WTH_MISS
    case default
      call errend(msg_invalid_value('var', var))
    endselect

    allocate(datr4(gxi:gxf,gyi:gyf))
    call read_basin_map_from_tile(&
           RESOLUTION_1SEC, bsnId, var, datr4, DTYPE_REAL, gxi, gyi, missr4, bsn)
    f = get_f_map_basin(RESOLUTION_1SEC, var, bsnId)
    call traperr( mkdir(dirname(f)) )
    call logmsg('Writing '//str(f))
    call traperr( wbin(datr4, f) )
    deallocate(datr4)
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call errend(msg_invalid_value('var', var))
  endselect

  deallocate(bsn)
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine makeBasinTopoMap
!===============================================================
!
!===============================================================
end module mod_make_basin
