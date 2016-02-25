subroutine da_thickness(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,pre1,pre2,thk)

   !-----------------------------------------------------------------------
   ! Purpose: calculates the thickness between two layers 
   ! using vertical integration of virtual temperatures.
   ! pre1 and pre2 are two pressures for the two layers
   !-----------------------------------------------------------------------

   implicit none

   integer,intent(in)  :: layer1,layer2         ! two layers
   real,   intent(in)  :: tv1,tv2               ! virtual temp.
   real,   intent(in)  :: pre1,pre2             ! pressure

   integer,intent(in)  :: ks,ke
   real,   intent(in)  :: pre_ma(ks-1:ke+1)
   real,   intent(in)  :: tv_ma(ks-1:ke+1)
   real,   intent(out) :: thk                   ! thickness

   integer :: k
   real    :: p_tmp(ks-1:ke+1)

   if (trace_use_dull) call da_trace_entry("da_thickness")

   ! Thickness at the top and bottom parts of the layer.

   thk = 0.5 * gas_constant/gravity * (tv1*log(pre_ma(layer1-1)/pre1) +  &
      tv2*log(pre2/pre_ma(layer2)) )

   ! Temporary pressure

   p_tmp(layer1) = pre1
   p_tmp(layer2-1) = pre2
   do k = layer2, layer1-1
      p_tmp(k) = pre_ma(k)
   end do

   ! Vertical integration of the virtual temperature

   do k=layer2,layer1-1
      thk = thk + 0.5 * gas_constant/gravity * tv_ma(k) * log(p_tmp(k-1)/p_tmp(k+1))
   end do

   if (trace_use_dull) call da_trace_exit("da_thickness")

end subroutine da_thickness


