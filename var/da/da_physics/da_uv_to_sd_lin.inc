subroutine da_uv_to_sd_lin(spd,dir,u,v,ub,vb)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)     :: u, v
   real, intent(inout)  :: ub, vb
   real, intent(out)    :: spd, dir
   
   if (trace_use_dull) call da_trace_entry("da_uv_to_sd_lin")

   if ( (ub*ub+vb*vb) == 0.0 ) then      ! Avoid division by zero
      spd = 0.0
      dir = 0.0
      return
      if (trace_use_dull) call da_trace_exit("da_uv_to_sd_lin")
   end if

   if (abs(ub - 0.0) <= 0.1) ub = (ub/abs(ub))*0.1
   if (abs(vb - 0.0) <= 0.1) vb = (vb/abs(vb))*0.1

   spd = (ub*u+vb*v)/sqrt(ub*ub+vb*vb)
   dir = (vb*u-ub*v)/(ub*ub+vb*vb) * 180.0/pi

   if (trace_use_dull) call da_trace_exit("da_uv_to_sd_lin")

end subroutine da_uv_to_sd_lin 
