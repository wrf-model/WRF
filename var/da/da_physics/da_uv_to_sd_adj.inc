subroutine da_uv_to_sd_adj(spd,dir,u,v,ub,vb)
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)     ::  spd, dir
   real, intent(inout)  ::  ub, vb
   real, intent(out)    ::  u, v 
 
   if (trace_use_dull) call da_trace_entry("da_uv_to_sd_adj")

   u = 0.0
   v = 0.0

   if( (ub*ub+vb*vb) == 0.0 ) then      ! Avoid division by zero
      return
      if (trace_use_dull) call da_trace_exit("da_uv_to_sd_adj")
   end if

   if (abs(ub - 0.0) <= 0.1) ub = (ub/abs(ub))*0.1
   if (abs(vb - 0.0) <= 0.1) vb = (vb/abs(vb))*0.1


   u = ub/sqrt(ub*ub+vb*vb)*spd + vb/(ub*ub+vb*vb)*dir*180.0/pi
   v = vb/sqrt(ub*ub+vb*vb)*spd - ub/(ub*ub+vb*vb)*dir*180.0/pi

   if (trace_use_dull) call da_trace_exit("da_uv_to_sd_adj")

end subroutine da_uv_to_sd_adj 
