subroutine da_get_unit(unit)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
 
   implicit none

   integer, intent(out) :: unit

   integer :: i

   ! Do not trace as called by tracing
   ! if (trace_use_dull) call da_trace_entry("da_get_unit")

   unit = -1

   do i = unit_start, unit_end
      if (.NOT. unit_used(i)) then
         unit=i
         unit_used(i) = .true.
         exit
      end if
   end do

   if (unit == -1) then
      call da_error(__FILE__,__LINE__,(/"No free units"/))

   ! if (trace_use_dull) call da_trace_exit("da_get_unit")

   end if

end subroutine da_get_unit


