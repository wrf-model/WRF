subroutine da_find_layer_tl(layer,tv,pre,pre_ma,tv_ma,ks,ke,TGL_tv,TGL_pre_ma,TGL_tv_ma)

   !-----------------------------------------------------------------------
   ! Purpose: tangent-linear routine for da_find_layer
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ks, ke
   integer, intent(out)   :: layer
   real,    intent(in)    :: tv_ma(ks-1:ke+1)
   real,    intent(inout) :: pre_ma(ks-1:ke+1)
   real,    intent(in)    :: TGL_tv_ma(ks-1:ke+1)
   real,    intent(inout) :: TGL_pre_ma(ks-1:ke+1)
   real,    intent(in)    :: pre
   real,    intent(out)   :: tv
   real,    intent(out)   :: TGL_tv

   real    :: TGL_alpha
   integer :: k
   real    :: alpha, coef1, coef2

   if (trace_use_frequent) call da_trace_entry("da_find_layer_tl")

   ! coef1, coef2 are temporarily used in this routine

   if (pre >= pre_ma(ks)) then
      ! Below model bottom
      layer = ks
      coef1=log(pre/pre_ma(ks+1))/(pre_ma(ks)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      coef2=log(pre_ma(ks)/pre)/(pre_ma(ks+1)*     &
            (log(pre_ma(ks)/pre_ma(ks+1)))**2)
      TGL_alpha = coef1 * TGL_pre_ma(ks) + coef2 * TGL_pre_ma(ks+1)
      alpha = log(pre_ma(ks)/pre)/log(pre_ma(ks)/pre_ma(ks+1))

      TGL_tv = (1.0-alpha)*TGL_tv_ma(ks) +               &
               (tv_ma(ks+1)-tv_ma(ks))*TGL_alpha +     &
               alpha*TGL_tv_ma(ks+1)
      TGL_pre_ma(ks-1) = 0.0
      tv = tv_ma(ks) * (1.0-alpha) + tv_ma(ks+1) * alpha
      pre_ma(ks-1) = pre
   else if (pre <= pre_ma(ke)) then
      ! Above model top
      layer = ke+1
      coef1=log(pre/pre_ma(ke))/(pre_ma(ke-1)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      coef2=log(pre_ma(ke-1)/pre)/(pre_ma(ke)*           &
            (log(pre_ma(ke-1)/pre_ma(ke)))**2)
      TGL_alpha = coef1 * TGL_pre_ma(ke-1) + coef2 * TGL_pre_ma(ke)
      alpha = log(pre_ma(ke-1)/pre)/log(pre_ma(ke-1)/pre_ma(ke))

      TGL_tv = (1.0-alpha)*TGL_tv_ma(ke-1) +                 &
               (tv_ma(ke)-tv_ma(ke-1))*TGL_alpha +           &
               alpha*TGL_tv_ma(ke)
      TGL_pre_ma(ke+1) = 0.0
      tv = tv_ma(ke-1) * (1.0-alpha) + tv_ma(ke) * alpha
      pre_ma(ke+1) = pre
   else
      ! Between model layers
      do k=ks,ke-1
         if (pre>=pre_ma(k+1) .and. pre<pre_ma(k)) then
            layer = k+1
            coef1=log(pre/pre_ma(k+1))/(pre_ma(k)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            coef2=log(pre_ma(k)/pre)/(pre_ma(k+1)*   &
                  (log(pre_ma(k)/pre_ma(k+1)))**2)
            TGL_alpha = coef1 * TGL_pre_ma(k) + coef2 * TGL_pre_ma(k+1)
            alpha = log(pre_ma(k)/pre)/log(pre_ma(k)/pre_ma(k+1))
            TGL_tv = (1.0-alpha)*TGL_tv_ma(k) +                 &
                     (tv_ma(k+1)-tv_ma(k))*TGL_alpha +         &
                      alpha*TGL_tv_ma(k+1)
            tv = tv_ma(k) * (1.0-alpha) + tv_ma(k+1) * alpha
            exit
         end if
      end do
   end if

   if (trace_use_frequent) call da_trace_exit("da_find_layer_tl")
 
end subroutine da_find_layer_tl


