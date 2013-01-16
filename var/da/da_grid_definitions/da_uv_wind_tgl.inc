subroutine da_uv_wind_tgl(sp,dir,u,v,ub,vb)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)     :: u, v
   real, intent(inout)   :: ub, vb
   real, intent(out)    :: sp, dir
   
   if (trace_use_dull) call da_trace_entry("da_uv_wind_tgl")

   if (abs(ub - 0.0) <= 0.1) ub = (ub/abs(ub))*0.1
   if (abs(vb - 0.0) <= 0.1) vb = (vb/abs(vb))*0.1

   sp = (ub*u+vb*v)/sqrt(ub*ub+vb*vb)
   dir = (vb*u-ub*v)/(ub*ub+vb*vb) * 180.0/pi

   if (trace_use_dull) call da_trace_exit("da_uv_wind_tgl")

end subroutine da_uv_wind_tgl
