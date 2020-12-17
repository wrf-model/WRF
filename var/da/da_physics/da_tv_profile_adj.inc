subroutine da_tv_profile_adj(grid,jo_grad_x,info, n,pre_ma,tv_ma,ADJ_pre_ma, ADJ_tv_ma)

   !-----------------------------------------------------------------------
   ! Purpose: adjoint routine for da_tv_profile
   !-----------------------------------------------------------------------

   implicit none

   type (x_type),         intent(inout) :: jo_grad_x ! grad_x(jo)
   type (infa_type),      intent(in)    :: info
   integer,               intent(in)    :: n
   type (domain),         intent(in)    :: grid
   real,                  intent(in)    :: pre_ma(kts-1:kte+1)
   real,                  intent(in)    :: tv_ma(kts-1:kte+1)
   real,                  intent(inout) :: ADJ_pre_ma(kts-1:kte+1)
   real,                  intent(inout) :: ADJ_tv_ma(kts-1:kte+1)

   integer :: ii,jj
   real    :: ADJ_tv_m(2,2,kts:kte)
   integer :: i, j      ! OBS location
   real    :: dx, dxm   ! interpolation weights.
   real    :: dy, dym   ! interpolation weights.

   if (trace_use_dull) call da_trace_entry("da_tv_profile_adj")

   i   = info%i(1,n)
   j   = info%j(1,n)
   dx  = info%dx(1,n)
   dy  = info%dy(1,n)
   dxm = info%dxm(1,n)
   dym = info%dym(1,n)

   ADJ_tv_m(1,1,kts:kte) = dym*dxm * ADJ_tv_ma (kts:kte)
   ADJ_tv_m(2,1,kts:kte) = dym*dx *  ADJ_tv_ma (kts:kte)
   ADJ_tv_m(1,2,kts:kte) = dy*dxm*   ADJ_tv_ma (kts:kte)
   ADJ_tv_m(2,2,kts:kte) = dy*dx*    ADJ_tv_ma (kts:kte)

   jo_grad_x%p(i,j,kts:kte)     = jo_grad_x%p(i,j,kts:kte) + dym*dxm  * ADJ_pre_ma(kts:kte)
   jo_grad_x%p(i+1,j,kts:kte)   = jo_grad_x%p(i+1,j,kts:kte) + dym*dx * ADJ_pre_ma(kts:kte)
   jo_grad_x%p(i,j+1,kts:kte)   = jo_grad_x%p(i,j+1,kts:kte) + dy*dxm * ADJ_pre_ma(kts:kte)
   jo_grad_x%p(i+1,j+1,kts:kte) = jo_grad_x%p(i+1,j+1,kts:kte) + dy*dx* ADJ_pre_ma(kts:kte)

   ADJ_tv_ma (kts:kte) = 0.0
   ADJ_pre_ma(kts:kte) = 0.0

   do ii=i,i+1
      do jj=j,j+1
         jo_grad_x%t(ii,jj,kts:kte) = jo_grad_x%t(ii,jj,kts:kte) + &
            ADJ_tv_m(ii-i+1,jj-j+1,kts:kte)*(1.0+0.61*grid%xb%q(ii,jj,kts:kte))
         jo_grad_x%q(ii,jj,kts:kte) = jo_grad_x%q(ii,jj,kts:kte) + &
            0.61*grid%xb%t(ii,jj,kts:kte)*ADJ_tv_m(ii-i+1,jj-j+1,kts:kte)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_tv_profile_adj")

end subroutine da_tv_profile_adj


