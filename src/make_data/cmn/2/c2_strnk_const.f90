module c2_strnk_const
  use lib_const
  use lib_base
  use lib_log
  use c1_const
  use c2_nlni_const, only: &
        DGT_WSCODE
  implicit none
  public
  !-------------------------------------------------------------
  ! Public module variables
  !-------------------------------------------------------------
  character(CLEN_PATH), parameter :: DIR_STRRANK = trim(DIR_DAT)//'/StrRank'
  character(CLEN_PATH), parameter :: DIR_ORG     = trim(DIR_STRRANK)//'/dl'
  character(CLEN_PATH), parameter :: DIR_PRD     = trim(DIR_STRRANK)//'/dat'
  character(CLEN_PATH), parameter :: DIR_STR     = trim(DIR_PRD)//'/StrRank'

  ! For original data
  integer, parameter :: NREGION = 5
  character(CLEN_VAR), parameter :: REGION_HOKKAIDO = 'Hokkaido'
  character(CLEN_VAR), parameter :: REGION_HONSHU   = 'Honshu'
  character(CLEN_VAR), parameter :: REGION_SHIKOKU  = 'Shikoku'
  character(CLEN_VAR), parameter :: REGION_KYUSHU   = 'Kyushu'
  character(CLEN_VAR), parameter :: REGION_OKINAWA  = 'Okinawa'
  character(CLEN_VAR), parameter :: REGION_ALL      = 'all'

  integer, parameter :: CLEN_NODEID = 12

  integer, parameter :: NODETYPE__INTERMEDIATE = 0
  integer, parameter :: NODETYPE__SOURCE       = 1
  integer, parameter :: NODETYPE__OUTLET       = 2
  integer, parameter :: NODETYPE__NOTFOUND     = -1
  integer, parameter :: NODETYPE__NOTNODE      = -9
  integer, parameter :: NODETYPE__UNDEF        = -99

  integer, parameter :: IDX_FIELD_RIVERNODE__NODEID   = 1
  integer, parameter :: IDX_FIELD_RIVERNODE__WSCODE   = 2
  integer, parameter :: IDX_FIELD_RIVERNODE__ELV      = 3
  integer, parameter :: IDX_FIELD_RIVERNODE__ELVSRC   = 4
  integer, parameter :: IDX_FIELD_RIVERNODE__DISTANCE = 5
  integer, parameter :: IDX_FIELD_RIVERNODE__MAXRANK  = 6
  integer, parameter :: IDX_FIELD_RIVERNODE__ENDPOINT = 7

  integer, parameter :: IDX_FIELD_STREAM__WSCODE    = 1
  integer, parameter :: IDX_FIELD_STREAM__RIVCODE   = 2
  integer, parameter :: IDX_FIELD_STREAM__RIVNAME   = 3
  integer, parameter :: IDX_FIELD_STREAM__SECTTYPE  = 4
  integer, parameter :: IDX_FIELD_STREAM__STRRANK   = 5
  integer, parameter :: IDX_FIELD_STREAM__STRLENG   = 6
  integer, parameter :: IDX_FIELD_STREAM__STRDZ     = 7
  integer, parameter :: IDX_FIELD_STREAM__STRSLOPE  = 8
  integer, parameter :: IDX_FIELD_STREAM__MAXDIST   = 9
  integer, parameter :: IDX_FIELD_STREAM__NDUPSTR   = 10
  integer, parameter :: IDX_FIELD_STREAM__NDDOWNSTR = 11

  ! For modified data
  integer, parameter :: NODETYPE_NEW__INTER     = 0
  integer, parameter :: NODETYPE_NEW__SRC       = 1
  integer, parameter :: NODETYPE_NEW__OUT       = 2
  integer, parameter :: NODETYPE_NEW__SRC_INTER = 11
  integer, parameter :: NODETYPE_NEW__OUT_INTER = 12
  integer, parameter :: NODETYPE_NEW__SRC_OUT   = 13
  integer, parameter :: NODETYPE_NEW__ALLMIXED  = 14
  integer, parameter :: NODETYPE_NEW__UNKNOWN   = 90
  integer, parameter :: NODETYPE_NEW__NEWNODE   = 91
  integer, parameter :: NODETYPE_NEW__UNDEF     = -99

  integer, parameter :: DGT_NWK_SAME_WSCODE = 3
  integer, parameter :: DGT_NWKUID = 1 + DGT_WSCODE + DGT_NWK_SAME_WSCODE
end module c2_strnk_const
