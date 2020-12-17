subroutine da_lcl(p0, z0, t0, q0, plcl, zlcl, tlcl, qlcl)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)   :: p0, z0, t0, q0
   real, intent(out)  :: plcl, zlcl, tlcl, qlcl

   real   :: dp, qs, eps

   if (trace_use) call da_trace_entry("da_lcl")

   dp=5.0
   plcl=300.0

   do
      tlcl=t0*((plcl/p0)**0.286)

      call da_qfrmrh(plcl, tlcl, 100.0, qs)

      eps=qs-q0

      if (eps >= 0.0) then
         zlcl=(1004.0/gravity)*(t0-tlcl)+z0
         qlcl=qs
         if (trace_use) call da_trace_exit("da_lcl")
         return
      else
         plcl=plcl+dp

         if (plcl >= p0) then
            zlcl=z0
            qlcl=q0
            plcl=p0
            if (trace_use) call da_trace_exit("da_lcl")
            return
        end if
      end if
   end do

   if (trace_use) call da_trace_exit("da_lcl")

end subroutine da_lcl


