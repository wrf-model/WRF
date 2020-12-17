subroutine da_ref_height(pres, ref_height)

   !---------------------------------------------------------------------------
   ! Purpose: To calculate the height from the reference pressure
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: pres
   real, intent(out) :: ref_height

   real              :: aa, bb, cc
   real              :: p0iso, ziso

   real, parameter   :: rovg = gas_constant/gravity

   if (trace_use) call da_trace_entry("da_ref_height")

   aa = 0.5 * rovg * base_lapse
   bb = rovg * t0

   cc = log(pres/base_pres)
   ref_height = -(bb + aa * cc) * cc

   if (base_temp > 0.0) then
      p0iso=base_pres*exp((base_temp-t0)/base_lapse)
      cc=log(p0iso/base_pres)
      ziso=-(aa*cc+bb)*cc

      if (ref_height > ziso) then
         ref_height=ziso+rovg*base_temp*log(p0iso/pres)
      end if
   end if

   if (trace_use) call da_trace_exit("da_ref_height")

end subroutine da_ref_height


