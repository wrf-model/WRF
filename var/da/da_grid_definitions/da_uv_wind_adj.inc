subroutine da_uv_wind_adj(sp,dir,u,v,ub,vb)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  ::  sp, dir
   real, intent(inout)  ::  ub, vb
   real, intent(inout) ::  u, v 
 
   if (trace_use_dull) call da_trace_entry("da_uv_wind_adj")

   if (abs(ub - 0.0) <= 0.1) ub = (ub/abs(ub))*0.1
   if (abs(vb - 0.0) <= 0.1) vb = (vb/abs(vb))*0.1

   u = 0.0
   v = 0.0

   u = u + ub/sqrt(ub*ub+vb*vb)*sp + vb/(ub*ub+vb*vb)*dir*180.0/pi
   v = v + vb/sqrt(ub*ub+vb*vb)*sp - ub/(ub*ub+vb*vb)*dir*180.0/pi

   if (trace_use_dull) call da_trace_exit("da_uv_wind_adj")

end subroutine da_uv_wind_adj
