module wrf_instance

use seq_comm_mct, only: seq_comm_suffix, seq_comm_inst, seq_comm_name

implicit none
private
save

public :: wrf_instance_init

integer,           public :: inst_index
character(len=16), public :: inst_name
character(len=16), public :: inst_suffix

!===============================================================================
CONTAINS
!===============================================================================

subroutine wrf_instance_init(atm_id)

   integer, intent(in) :: atm_id

   inst_name   = seq_comm_name(atm_id)
   inst_index  = seq_comm_inst(atm_id)
   inst_suffix = seq_comm_suffix(atm_id)

end subroutine wrf_instance_init

end module wrf_instance
