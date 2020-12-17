subroutine da_transform_xtospeed_adj(ADJ_speed,U,V,ADJ_u,ADJ_v)

   !----------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

   implicit none

   real, intent (in)    :: ADJ_speed
   real, intent (in)    :: U, V
   real, intent (inout) :: ADJ_u, ADJ_v

   real :: speed

   if (trace_use) call da_trace_entry("da_transform_xtospeed_adj")

   speed = sqrt(U*U+V*V+ 1.0e-6)

   ADJ_u = U*ADJ_speed/speed + ADJ_u
   ADJ_v = V*ADJ_speed/speed + ADJ_v

   if (trace_use) call da_trace_exit("da_transform_xtospeed_adj")

end subroutine da_transform_xtospeed_adj


