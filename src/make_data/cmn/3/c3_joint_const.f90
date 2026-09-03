module c3_joint_const
  use lib_const
  use c1_const
  implicit none

  character(CLEN_PATH), parameter :: DIR_JOINT = trim(DIR_DAT)//'/joint'

  real(8), parameter :: JACCARD_MISS = -1d20
  real(8), parameter :: JACCARD_NO_BASIN = -9999.d0
  real(8), parameter :: JACCARD_NO_MATCH = -999.d0

  character(CLEN_VAR), parameter :: DATANAME__JFLW = 'j-flwdir'
  character(CLEN_VAR), parameter :: DATANAME__STRNK = 'strrank'
  character(CLEN_VAR), parameter :: DATANAME__RRI = 'rri'

  character(CLEN_VAR), parameter :: BASINTYPE__BASIN = 'basin'
  character(CLEN_VAR), parameter :: BASINTYPE__NETWORK = 'network'
  character(CLEN_VAR), parameter :: BASINTYPE__NETWORKSET = 'networkset'
end module c3_joint_const
