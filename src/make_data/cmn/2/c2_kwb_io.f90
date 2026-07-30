module c2_kwb_io
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c1_const
  use c2_kwb_const
  implicit none
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_f_lst_rsys
  public :: get_f_lst_obsId
  public :: get_f_lst_obsId_rsys
  public :: get_f_json_obsinfo
  !-------------------------------------------------------------
  ! Private module variables
  !-------------------------------------------------------------
  character(CLEN_PROC), parameter :: MODNAM = 'c2_kwb_io'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_rsys(var) result(res)
  implicit none
  character(*), intent(in) :: var

  res = joined(DIR_OBSINFO, str(var)//'/rsys.lst')
end function get_f_lst_rsys
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_obsId(var) result(res)
  implicit none
  character(*), intent(in) :: var

  res = joined(DIR_OBSINFO, str(var)//'/all/id.lst')
end function get_f_lst_obsId
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_lst_obsId_rsys(&
    var, rsysCd) result(res)
  implicit none
  character(*), intent(in) :: var
  character(*), intent(in) :: rsysCd

  res = joined(DIR_OBSINFO, str(var)//'/'//str(rsysCd)//'/id.lst')
end function get_f_lst_obsId_rsys
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_f_json_obsinfo(&
    var, obsId) result(res)
  implicit none
  character(*), intent(in) :: var
  character(*), intent(in) :: obsId

  res = joined(DIR_OBSINFO, str(var)//'/all/'//str(obsId)//'.json')
end function get_f_json_obsinfo
!===============================================================
!
!===============================================================
end module c2_kwb_io
