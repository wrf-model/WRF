subroutine da_find_layer(layer,tv,pre,pre_ma,tv_ma,ks,ke)

   !-----------------------------------------------------------------------
   ! Purpose: find the vertical location in the Tv profile given
   ! a specific pressure and vertically interpolate Tv to that height.
   ! pre_ma,tv_ma give vertical profile of virtual temperature
   ! pre is a given pressure, alpha is the percentage of pre in the layer.
   ! layer,tv are calculated vertical layer and interpolated virtual temp.
   !-----------------------------------------------------------------------
 
   implicit none

   integer, intent(in)    :: ks, ke
   integer, intent(inout) :: layer
   real,    intent(inout) :: pre_ma(ks-1:ke+1)
   real,    intent(in)    :: tv_ma(ks-1:ke+1)
   real,    intent(in)    :: pre
   real,    intent(inout) :: tv

   integer :: k
   real    :: alpha

   if (trace_use_frequent) call da_trace_entry("da_find_layer")

   if (pre >= pre_ma(ks)) then
      ! Below model bottom
      layer = ks
      alpha = log(pre_ma(ks)/pre)/log(pre_ma(ks)/pre_ma(ks+1))
      tv = tv_ma(ks) * (1.0-alpha) + tv_ma(ks+1) * alpha
      pre_ma(ks-1)=pre
   else if (pre <= pre_ma(ke)) then
      ! Above model top
      layer = ke+1
      alpha = log(pre_ma(ke-1)/pre)/log(pre_ma(ke-1)/pre_ma(ke))
      tv = tv_ma(ke-1) * (1.0-alpha) + tv_ma(ke) * alpha
      pre_ma(ke+1) = pre
   else
      ! Between model layers 
      do k=ks,ke-1
         if (pre>=pre_ma(k+1) .and. pre<pre_ma(k)) then
            layer = k+1
            alpha = log(pre_ma(k)/pre)/log(pre_ma(k)/pre_ma(k+1))
            tv = tv_ma(k) * (1.0-alpha) + tv_ma(k+1) * alpha
            exit
         end if
      end do
   end if

   if (trace_use_frequent) call da_trace_exit("da_find_layer")
 
end subroutine da_find_layer


