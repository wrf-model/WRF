subroutine da_transform_xtoy_satem_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.
   integer :: num_levs ! obs vertical levels

   integer :: k
   real    :: pre_ma(kts-1:kte+1)
   real    :: tv_ma(kts-1:kte+1)
   integer :: layer1,layer2
   real    :: tv1,tv2,pres2

   real    :: ADJ_pre_ma(kts-1:kte+1)
   real    :: ADJ_tv_ma(kts-1:kte+1)
   real    :: ADJ_tv1,ADJ_tv2

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_satem_adj")

   ADJ_pre_ma(:) = 0.0
   ADJ_tv_ma(:)  = 0.0
   ADJ_tv1 = 0.0
   ADJ_tv2 = 0.0

   do n=iv%info(satem)%n1,iv%info(satem)%n2
      num_levs = iv%info(satem)%levels(n)

      ! [2.0] Virtual temperature at obs pt.

      call da_tv_profile(grid,iv%info(satem),n,pre_ma,tv_ma)

      ! [3.0] Find model vertical position of pressure and do interp.

      call da_find_layer(layer2,tv2,iv%satem(n)%ref_p,pre_ma,tv_ma,kts,kte)
      pres2 = iv%satem(n)%ref_p

      ! [4.0] Adjoint calculation of satem thickness

      do k=1, num_levs
         if (ABS(iv % satem(n) %p (k) - missing_r) > 1.0) then
            call da_find_layer(layer1,tv1,iv%satem(n)%p(k),pre_ma,tv_ma, &
               kts,kte)

            call da_thickness_adj(pre_ma,tv_ma,kts,kte,tv1,tv2,layer1, &
               layer2, iv%satem(n)%p(k),pres2,ADJ_pre_ma,ADJ_tv_ma, &
               ADJ_tv1,ADJ_tv2,jo_grad_y%satem(n)%thickness(k))

            call da_find_layer_adj(layer1,tv1,iv%satem(n)%p(k),         &
               pre_ma,tv_ma,kts,kte,ADJ_tv1,ADJ_pre_ma,ADJ_tv_ma)

            pres2  = iv%satem(n)%p(k)
            layer2 = layer1
            tv2    = tv1
         end if
      end do

      ! [5.0] Adjoint of layer-finding and vertical interpolation

      call da_find_layer_adj(layer2,tv2,iv%satem(n)%ref_p,              &
         pre_ma,tv_ma,kts,kte,ADJ_tv2,ADJ_pre_ma,ADJ_tv_ma)

      ! [6.0] Adjoint of horizontal interpolation
      call da_tv_profile_adj(grid,jo_grad_x,iv%info(satem),n,         &
         pre_ma,tv_ma,ADJ_pre_ma,ADJ_tv_ma)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_satem_adj")

end subroutine da_transform_xtoy_satem_adj


