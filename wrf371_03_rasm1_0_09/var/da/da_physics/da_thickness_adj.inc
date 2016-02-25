subroutine da_thickness_adj(pre_ma,tv_ma,ks,ke,tv1,tv2,layer1,layer2,pre1,pre2,   &
   ADJ_pre_ma,ADJ_tv_ma,ADJ_tv1,ADJ_tv2,ADJ_thk)

   !-----------------------------------------------------------------------
   ! Purpose: adjoint routine for thickness
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: layer1,layer2
   integer, intent(in)    :: ks,ke
   real,    intent(in)    :: pre_ma(ks-1:ke+1)
   real,    intent(in)    :: tv_ma(ks-1:ke+1)
   real,    intent(inout) :: ADJ_pre_ma(ks-1:ke+1)
   real,    intent(inout) :: ADJ_tv_ma(ks-1:ke+1)
   real,    intent(in)    :: tv1,tv2
   real,    intent(inout) :: ADJ_tv1,ADJ_tv2
   real,    intent(in)    :: pre1,pre2
   real,    intent(inout) :: ADJ_thk

   integer :: k
   real    :: p_tmp(ks-1:ke+1)
   real    :: ADJ_p_tmp(ks-1:ke+1)

   if (trace_use_dull) call da_trace_entry("da_thickness_adj")

   ! p_tmp and ADJ_p_tmp are temporary (local) variables

   ADJ_p_tmp(:)=0.0

   p_tmp(layer1) = pre1
   p_tmp(layer2-1) = pre2
   do k=layer2,layer1-1
      p_tmp(k) = pre_ma(k)
   end do

   do k=layer2,layer1-1
      ADJ_p_tmp(k+1)  = ADJ_p_tmp(k+1) - 0.5*gas_constant/gravity *     &
                        ADJ_thk*tv_ma(k)/p_tmp(k+1)
      ADJ_p_tmp(k-1)  = ADJ_p_tmp(k-1) + 0.5*gas_constant/gravity *     &
                        ADJ_thk*tv_ma(k)/p_tmp(k-1)
      ADJ_tv_ma(k)    = ADJ_tv_ma(k)   + 0.5*gas_constant/gravity *     &
                        ADJ_thk*log(p_tmp(k-1)/p_tmp(k+1))
   end do

   do k=layer2,layer1-1
      ADJ_pre_ma(k) = ADJ_pre_ma(k) + ADJ_p_tmp(k)
   end do

   ADJ_thk = 0.5 * gas_constant/gravity * ADJ_thk
   ADJ_pre_ma(layer2) = ADJ_pre_ma(layer2) - ADJ_thk*tv2/pre_ma(layer2)
   ADJ_tv2 = ADJ_tv2 + ADJ_thk*log(pre2/pre_ma(layer2))
   ADJ_pre_ma(layer1-1) = ADJ_pre_ma(layer1-1) +              &
                          ADJ_thk*tv1/pre_ma(layer1-1)
   ADJ_tv1 = ADJ_tv1 + ADJ_thk*log(pre_ma(layer1-1)/pre1)

   if (trace_use_dull) call da_trace_exit("da_thickness_adj")

end subroutine da_thickness_adj


