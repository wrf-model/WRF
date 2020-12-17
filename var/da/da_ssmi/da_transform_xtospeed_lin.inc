subroutine da_transform_xtospeed_lin(TGL_speed,U,V,TGL_u,TGL_v)

   !----------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

   implicit none

   real, intent(out) :: TGL_speed
   real, intent(in)  :: U,V
   real, intent(in)  :: TGL_u,TGL_v

   real :: speed

   if (trace_use) call da_trace_entry("da_transform_xtospeed_lin")

   speed = sqrt(U*U + V*V + 1.0e-6)

   TGL_speed = (U*TGL_u + V*TGL_v)/speed

   if (trace_use) call da_trace_exit("da_transform_xtospeed_lin")

end subroutine da_transform_xtospeed_lin


