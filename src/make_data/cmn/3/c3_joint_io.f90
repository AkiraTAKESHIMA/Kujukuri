module c3_joint_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c3_joint_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  ! General
  public :: get_f_map_tile

  ! J-FlwDir and RRI
  public :: varname_jflw2rri
  public :: read_basin_range
  public :: read_topo_as_jflw

  ! J-FlwDir and NLNI
  public :: get_topdir_rt_nlni2jflw_wsCode
  public :: get_dir_rt_nlni2jflw_wsCode
  public :: get_dir_rt_nlni2jflw_wsCode_merged

  public :: get_dir_rt_nlni2jflw

  ! J-FlwDir and StrRank
  public :: get_dir_nwk
  public :: get_f_nwk_chn_dirstat
  public :: get_f_nwk_info
  public :: get_f_nwk_map
  public :: get_f_nwk_map_rri
  public :: get_f_chn_rri
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface read_topo_as_jflw
    module procedure read_topo_as_jflw__int1
    module procedure read_topo_as_jflw__int4
    module procedure read_topo_as_jflw__real
  end interface
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c3_joint_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_map_tile(&
    resl, var, tx, ty) result(f)
  use c2_jflw_io, only: &
        tilename
  implicit none
  character(CLEN_PROC), parameter :: PRCNAM = 'get_f_map_tile'
  character(*), intent(in) :: resl
  character(*), intent(in) :: var
  integer     , intent(in), optional :: tx, ty

  f = joined(DIR_JOINT, &
        trim(resl)//'/'//trim(var)//'/'//tilename(tx,ty)//'.bin')
end function get_f_map_tile
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
character(3) function varname_jflw2rri(varname) result(res)
  implicit none
  character(*), intent(in) :: varname

  character(CLEN_PROC), parameter :: PRCNAM = 'varname_jflw2rri'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'dir' )
    res = 'dir'
  case( 'elv' )
    res = 'dem'
  case( 'upg' )
    res = 'acc'
  case( 'upa' )
    res = 'upa'
  case default
    call errend(msg_invalid_value(varname, varname))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end function varname_jflw2rri
!===============================================================
!
!===============================================================
subroutine read_basin_range(&
    resolution, id, &
    gxi, gxf, gyi, gyf, west, east, south, north)
  use c2_jflw_io, only: &
        jflw_read_basin_range => read_basin_range_from_each
  use c2_rri_io, only: &
        rri_read_basin_range => read_basin_range
  implicit none
  character(*), intent(in)  :: resolution
  integer     , intent(in)  :: id
  integer     , intent(out) :: gxi, gxf, gyi, gyf
  real(8)     , intent(out) :: west, east, south, north

  character(CLEN_PROC), parameter :: PRCNAM = 'read_basin_range'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( resolution )
  case( '1sec' )
    call jflw_read_basin_range(&
           resolution, id, &
           gxi, gxf, gyi, gyf, &
           west, east, south, north)
  case( '3sec' )
    call rri_read_basin_range(&
           resolution, id, &
           gxi, gxf, gyi, gyf, &
           west, east, south, north)
  case default
    call errend(msg_invalid_value('resolution', resolution))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_basin_range
!===============================================================
!
!===============================================================
subroutine read_topo_as_jflw__int1(&
    dat, resolution, varname, bsnId)
  use c1_const
  use c2_jflw_util, only: &
        jflw_get_miss_int1 => get_miss_int1
  use c2_jflw_io, only: &
        jflw_strid           => strid
        !jflw_get_f_map_basin => get_f_map_basin
  use c2_rri_const, only: &
        RRI_FDR_MISS => FDR_MISS
  use c2_rri_io, only: &
        rri_read_topo => read_topo
  use c3_jflw_const
  use c3_joint_grid, only: &
        conv_fdr_rri2jflw
  implicit none
  integer(1)  , intent(out) :: dat(:,:)
  character(*), intent(in)  :: resolution
  character(*), intent(in)  :: varname
  integer     , intent(in)  :: bsnId

  integer(4), allocatable :: dat_rri(:,:)
  integer(1) :: miss
  integer(4) :: miss_rri

  character(CLEN_PATH) :: f

  character(CLEN_PROC), parameter :: PRCNAM = 'read_topo_as_jflw__int1'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  miss = jflw_get_miss_int1(varname)

  selectcase( resolution )
  case( RESOLUTION_1SEC )
    !call traperr( rbin(&
    !       dat, jflw_get_f_map_basin(resolution, varname, bsnId)) )
    f = trim(JFLW_DIR_BASIN)//'/'//trim(resolution)//'/'//&
        trim(varname)//'/'//jflw_strid(bsnId)//'.bin'
    call traperr( rbin(dat, f) )
           
  case( RESOLUTION_3SEC )
    allocate(dat_rri(size(dat,1),size(dat,2)))

    call rri_read_topo(&
           dat_rri, miss_rri,                          & ! out
           resolution, varname_jflw2rri(varname), bsnId) ! in
    where( dat_rri == miss_rri )
      dat_rri = miss
    endwhere
    call conv_fdr_rri2jflw(dat_rri, dat)

    deallocate(dat_rri)
  case default
    call errend(msg_invalid_value('resolution', resolution))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_topo_as_jflw__int1
!===============================================================
! varname: "upg"
!===============================================================
subroutine read_topo_as_jflw__int4(&
    dat, resolution, varname, bsnId)
  use c1_const
  use c2_jflw_util, only: &
        jflw_get_miss_int4 => get_miss_int4
  use c2_jflw_io, only: &
        jflw_strid           => strid
        !jflw_get_f_map_basin => get_f_map_basin
  use c2_rri_io, only: &
        rri_read_topo => read_topo
  use c3_jflw_const
  implicit none
  integer(4)  , intent(out) :: dat(:,:)
  character(*), intent(in)  :: resolution
  character(*), intent(in)  :: varname
  integer     , intent(in)  :: bsnId

  integer(4) :: miss
  integer(4) :: miss_rri

  character(CLEN_PATH) :: f

  character(CLEN_PROC), parameter :: PRCNAM = 'read_topo_as_jflw__int4'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  miss = jflw_get_miss_int4(varname)

  selectcase( resolution )
  case( RESOLUTION_1SEC )
    !call traperr( rbin(&
    !       dat, jflw_get_f_map_basin(resolution, varname, bsnId)) )
    f = trim(JFLW_DIR_BASIN)//'/'//trim(resolution)//'/'//&
        trim(varname)//'/'//jflw_strid(bsnId)//'.bin'
    call traperr( rbin(dat, f) )
  case( RESOLUTION_3SEC )
    call rri_read_topo(&
           dat, miss_rri,                              & ! out
           resolution, varname_jflw2rri(varname), bsnId) ! in
    where( dat == miss_rri )
      dat = miss
    endwhere
  case default
    call errend(msg_invalid_value('resolution', resolution))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_topo_as_jflw__int4
!===============================================================
!
!===============================================================
subroutine read_topo_as_jflw__real(&
    dat, resolution, varname, bsnId)
  use c1_const
  use c2_jflw_util, only: &
        jflw_get_miss_real => get_miss_real
  use c2_jflw_io, only: &
        jflw_strid           => strid
        !jflw_get_f_map_basin => get_f_map_basin
  use c2_rri_io, only: &
        rri_read_topo => read_topo
  use c3_jflw_const
  implicit none
  real(4)     , intent(out) :: dat(:,:)
  character(*), intent(in)  :: resolution
  character(*), intent(in)  :: varname
  integer     , intent(in)  :: bsnId

  real(4) :: miss
  real(4) :: miss_rri

  character(CLEN_PATH) :: f

  character(CLEN_PROC), parameter :: PRCNAM = 'read_topo_as_jflw__real'

  call logbgn(PRCNAM, MODNAM, '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  miss = jflw_get_miss_real(varname)

  selectcase( resolution )
  case( RESOLUTION_1SEC )
    !call traperr( rbin(&
    !       dat, jflw_get_f_map_basin(resolution, varname, bsnId)) )
    f = trim(JFLW_DIR_BASIN)//'/'//trim(resolution)//'/'//&
        trim(varname)//'/'//jflw_strid(bsnId)//'.bin'
    call traperr( rbin(dat, f) )
  case( RESOLUTION_3SEC )
    call rri_read_topo(&
           dat, miss_rri,                             &  ! out
           resolution, varname_jflw2rri(varname), bsnId) ! in
    where( dat == miss_rri )
      dat = miss
    endwhere
  case default
    call errend(msg_invalid_value('resolution', resolution))
  endselect
  !-------------------------------------------------------------
  call logret(PRCNAM, MODNAM)
end subroutine read_topo_as_jflw__real
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! NLNI 流域メッシュデータ
! 3次メッシュ1/10細分区画(100mメッシュ)
! https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W07.html
!===============================================================
character(CLEN_PATH) function get_topdir_rt_nlni2jflw_wsCode() result(res)
  use c3_jflw_const
  implicit none

  res = joined(JFLW_DIR_TILED, 'rt_nlni-wsCode_to_jflw-bsnId')
end function get_topdir_rt_nlni2jflw_wsCode
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_dir_rt_nlni2jflw_wsCode(&
    tx, ty, tx_nlni, ty_nlni) result(res)
  use c3_jflw_io, only: &
        jflw_tilename
  use c3_nlni_io, only: &
        nlni_tilename
  implicit none
  integer, intent(in) :: tx, ty
  integer, intent(in) :: tx_nlni, ty_nlni

  res = joined(get_topdir_rt_nlni2jflw_wsCode(), &
      nlni_tilename(tx_nlni,ty_nlni)//'_to_'//jflw_tilename(tx,ty))
end function get_dir_rt_nlni2jflw_wsCode
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_dir_rt_nlni2jflw_wsCode_merged() result(res)
  implicit none

  res = joined(get_topdir_rt_nlni2jflw_wsCode(), 'merged')
end function get_dir_rt_nlni2jflw_wsCode_merged
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
character(CLEN_PATH) function get_dir_rt_nlni2jflw(&
    resl_nlni, resl_jflw, &
    tx_nlni, ty_nlni, tx_jflw, ty_jflw) result(res)
  use c3_jflw_const
  use c3_jflw_io, only: &
        jflw_tilename
  use c3_nlni_io, only: &
        nlni_tilename
  implicit none
  character(*), intent(in) :: resl_nlni
  character(*), intent(in) :: resl_jflw
  integer, intent(in) :: tx_nlni, ty_nlni
  integer, intent(in) :: tx_jflw, ty_jflw

  res = joined(JFLW_DIR_TILED, &
          trim(resl_jflw)//'/rt_from_nlni-'//str(resl_nlni)//'/'//&
          nlni_tilename(tx_nlni,ty_nlni)//'_to_'//jflw_tilename(tx_jflw,ty_jflw))
end function get_dir_rt_nlni2jflw
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
character(CLEN_PATH) function get_dir_nwk(&
    resl, uid) result(dir)
  implicit none
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid

  dir = trim(DIR_JOINT)//'/network/'//trim(resl)//'/'//trim(uid)

  call traperr( mkdir(dir) )
end function get_dir_nwk
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_nwk_chn_dirstat(&
    uid) result(f)
  implicit none
  character(*), intent(in) :: uid

  f = joined(get_dir_nwk('cmn', uid), 'dirstat.txt')
end function get_f_nwk_chn_dirstat
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_nwk_info(&
    resl, uid) result(f)
  implicit none
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid

  f = joined(get_dir_nwk(resl, uid), 'info.txt')
end function get_f_nwk_info
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_nwk_map(&
    resl, uid, var) result(f)
  implicit none
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid
  character(*), intent(in) :: var

  f = joined(get_dir_nwk(resl, uid), trim(var)//'.bin')
end function get_f_nwk_map
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_nwk_map_rri(&
    resl, uid, var) result(f)
  implicit none
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid
  character(*), intent(in) :: var

  f = joined(get_dir_nwk(resl, uid), trim(var)//'.txt')
end function get_f_nwk_map_rri
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_chn_rri(&
    resl, uid) result(f)
  character(*), intent(in) :: resl
  character(*), intent(in) :: uid

  f = joined(get_dir_nwk(resl, uid), 'chn_rri.txt')
end function get_f_chn_rri
!===============================================================
!
!===============================================================
end module c3_joint_io
