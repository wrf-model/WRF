subroutine da_free_unit(unit)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
 
   implicit none

   integer, intent(in) :: unit

   ! Do not use as called in trace
   ! if (trace_use) call da_trace_entry("da_free_unit")

   unit_used(unit) = .false.

   ! if (trace_use) call da_trace_exit("da_free_unit")

end subroutine da_free_unit


