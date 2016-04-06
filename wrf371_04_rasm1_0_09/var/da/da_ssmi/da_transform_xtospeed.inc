subroutine da_transform_xtospeed(Speed,U,V)

   !----------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

   implicit none

   real, intent (out) :: Speed
   real, intent (in)  :: U, V

   if (trace_use) call da_trace_entry("da_transform_xtospeed")

   Speed = sqrt(U*U + V*V)

   if (trace_use) call da_trace_exit("da_transform_xtospeed")

end subroutine da_transform_xtospeed


