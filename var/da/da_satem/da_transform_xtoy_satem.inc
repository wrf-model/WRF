subroutine da_transform_xtoy_satem (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! Loop counter.
   integer :: i, j     ! Index dimension.
   real    :: dx, dxm  !
   real    :: dy, dym  !
   integer :: num_levs ! obs vertical levels

   integer :: k
   real    :: pre_ma(kts-1:kte+1)
   real    :: tv_ma(kts-1:kte+1)
   integer :: layer1,layer2
   real    :: tv1,tv2,pres2

   real    :: TGL_pre_ma(kts-1:kte+1)
   real    :: TGL_tv_ma(kts-1:kte+1)
   real    :: TGL_tv1,TGL_tv2

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_satem")

   do n=iv%info(satem)%n1,iv%info(satem)%n2

      num_levs = iv%info(satem)%levels(n)

      ! [1.0] Get horizontal interpolation weights:

      i   = iv%info(satem)%i(1,n)
      dy  = iv%info(satem)%dy(1,n)
      dym = iv%info(satem)%dym(1,n)
      j   = iv%info(satem)%j(1,n)
      dx  = iv%info(satem)%dx(1,n)
      dxm = iv%info(satem)%dxm(1,n)

      ! [2.0] Virtual temperature at obs pt.

      call da_tv_profile_tl(grid,i,j,dx,dxm,dy,dym,                &
         pre_ma,tv_ma,TGL_pre_ma,TGL_tv_ma)

      ! [3.0] Find model vertical position of pressure and do interp.

      call da_find_layer_tl(layer2,tv2,iv%satem(n)%ref_p,              &
         pre_ma,tv_ma,kts,kte,TGL_tv2,TGL_pre_ma,TGL_tv_ma)
      pres2 = iv%satem(n)%ref_p

      ! [4.0] Thickness calculation

      do k=1, num_levs
         if (ABS(iv % satem(n) %p (k) - missing_r) > 1.0) then
            call da_find_layer_tl(layer1,tv1,iv%satem(n)%p(k),            &
               pre_ma,tv_ma,kts,kte,TGL_tv1,TGL_pre_ma,TGL_tv_ma)
            call da_thickness_tl(pre_ma,tv_ma,kts,kte,tv1,tv2,layer1,layer2,&
               iv%satem(n)%p(k),pres2,TGL_pre_ma,TGL_tv_ma,           &
               TGL_tv1,TGL_tv2,y%satem(n)%thickness(k))

            pres2 = iv%satem(n)%p(k)
            layer2 = layer1
            tv2 = tv1
         end if
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_satem")

end subroutine da_transform_xtoy_satem


