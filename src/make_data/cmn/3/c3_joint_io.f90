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
