subroutine da_find_layer_adj (layer,tv,pre,pre_ma,tv_ma,ks,ke,ADJ_tv,ADJ_pre_ma,ADJ_tv_ma)

   !-----------------------------------------------------------------------
   ! Purpose: adjoint routine for da_find_layer
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ks, ke
   integer, intent(out)   :: layer
   real,    intent(in)    :: pre_ma(ks-1:ke+1)
   real,    intent(in)    :: tv_ma(ks-1:ke+1)
   real,    intent(inout) :: ADJ_pre_ma(ks-1:ke+1)
   real,    intent(inout) :: ADJ_tv_ma(ks-1:ke+1)
   real,    intent(in)    :: pre, tv
   real,    intent(inout) :: ADJ_tv

   integer :: k
   real    :: alpha, coef1, coef2
   real    :: ADJ_alpha

   if (trace_use_frequent) call da_trace_entry("da_find_layer_adj")

   if (pre >= pre_ma(ks)) then
      layer = ks
      coef1=log(pre/pre_ma(ks+1))/(pre_ma(ks)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      coef2=log(pre_ma(ks)/pre)/(pre_ma(ks+1)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      alpha = log(pre_ma(ks)/pre)/log(pre_ma(ks)/pre_ma(ks+1))

      ADJ_pre_ma(ks-1)= 0.0
      ADJ_tv_ma(ks)   = ADJ_tv_ma(ks) + (1.0-alpha)*ADJ_tv
      ADJ_alpha        = (tv_ma(ks+1)-tv_ma(ks))*ADJ_tv
      ADJ_tv_ma(ks+1) = ADJ_tv_ma(ks+1) + alpha*ADJ_tv

      ADJ_pre_ma(ks)    = ADJ_pre_ma(ks) + coef1 * ADJ_alpha
      ADJ_pre_ma(ks+1)  = ADJ_pre_ma(ks+1) + coef2 * ADJ_alpha
   else if (pre <= pre_ma(ke)) then
      layer = ke+1
      coef1=log(pre/pre_ma(ke))/(pre_ma(ke-1)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      coef2=log(pre_ma(ke-1)/pre)/(pre_ma(ke)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      alpha = log(pre_ma(ke-1)/pre)/log(pre_ma(ke-1)/pre_ma(ke))

      ADJ_pre_ma(ke+1)    = 0.0
      ADJ_tv_ma(ke-1)     = ADJ_tv_ma(ke-1) + (1.0-alpha)*ADJ_tv
      ADJ_alpha        = (tv_ma(ke)-tv_ma(ke-1))*ADJ_tv
      ADJ_tv_ma(ke)     = ADJ_tv_ma(ke) + alpha*ADJ_tv

      ADJ_pre_ma(ke-1) = ADJ_pre_ma(ke-1) + coef1 * ADJ_alpha
      ADJ_pre_ma(ke) = ADJ_pre_ma(ke) + coef2 * ADJ_alpha
   else
      do k=ks,ke-1
         if (pre>=pre_ma(k+1) .and. pre<pre_ma(k)) then
            layer = k+1
            coef1=log(pre/pre_ma(k+1))/(pre_ma(k)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            coef2=log(pre_ma(k)/pre)/(pre_ma(k+1)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            alpha = log(pre_ma(k)/pre)/log(pre_ma(k)/pre_ma(k+1))

            ADJ_tv_ma(k)     = ADJ_tv_ma(k) + (1.0-alpha)*ADJ_tv
            ADJ_alpha        = (tv_ma(k+1)-tv_ma(k))*ADJ_tv
            ADJ_tv_ma(k+1)   = ADJ_tv_ma(k+1) + alpha * ADJ_tv

            ADJ_pre_ma(k)   = ADJ_pre_ma(k) + coef1 * ADJ_alpha
            ADJ_pre_ma(k+1) = ADJ_pre_ma(k+1) + coef2 * ADJ_alpha
            exit
         end if
      end do
   end if

   ADJ_tv           = 0.0

   if (trace_use_frequent) call da_trace_exit("da_find_layer_adj")
 
end subroutine da_find_layer_adj


