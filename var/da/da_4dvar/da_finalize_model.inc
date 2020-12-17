subroutine da_finalize_model

   !-----------------------------------------------------------------------
   ! Purpose: finalize model
   !    Author: Xin Zhang,  MMM/ESSL/NCAR,  Date: 94/24/2010
   !-----------------------------------------------------------------------

   implicit none
   
   if (trace_use_dull) call da_trace_entry("da_finalize_model")

   ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.
   CALL wrf_finalize ( .true. )
	
   if (trace_use_dull) call da_trace_exit("da_finalize_model")

end subroutine da_finalize_model
