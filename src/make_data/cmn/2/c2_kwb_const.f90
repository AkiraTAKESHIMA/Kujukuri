module c2_kwb_const
  use lib_const
  use c1_const
  implicit none
  public
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  character(CLEN_PATH), parameter :: DIR_KWB     = trim(DIR_DAT)//'/kawabou'
  character(CLEN_PATH), parameter :: DIR_OBSINFO = trim(DIR_KWB)//'/dl/info/obsinfo'
end module c2_kwb_const
