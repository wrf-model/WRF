subroutine da_div_profile_adj(grid,jo_grad_x, info, n, k, ADJ_div)

   !--------------------------------------------------------------------------
   ! Purpose: Calculates divergence (div) on each level at the observed location (i,j). 
   ! dx, dxm, dy, dym are horizontal interpolation weighting.
   !                        d   U      d   V
   !           Div = m^2 *[---(---) + ---(---) ] 
   !                        dx  m      dy  M
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)
   !--------------------------------------------------------------------------

   implicit none

   type (x_type),         intent(inout) :: jo_grad_x ! grad_x(jo)
   type (domain),         intent(in)    :: grid
   type (infa_type),      intent(in)    :: info
   integer,               intent(in)    :: n, k
   real,                  intent(out)   :: ADJ_div
                          
   integer :: ii, jj    ! index dimension.
  
   integer :: i, j      ! OBS location
   real    :: dx, dxm   ! interpolation weights.
   real    :: dy, dym   ! interpolation weights.
   real    :: coeff  
   real    :: ADJ_div_m(2,2)
   
   if (trace_use_dull) call da_trace_entry ("da_div_profile_adj")

   i   = info%i(1,n)
   j   = info%j(1,n)
   dx  = info%dx(1,n)
   dy  = info%dy(1,n)
   dxm = info%dxm(1,n)
   dym = info%dym(1,n)

! avoid the boundary mistake   
   if(i == its) i = its + 1
   if(i == ite) i = ite - 1
   if(j == jts) j = jts + 1
   if(j == jte) j = jte - 1

   ADJ_div_m(1,1) = dym*dxm * ADJ_div
   ADJ_div_m(2,1) = dym*dx *  ADJ_div
   ADJ_div_m(1,2) = dy*dxm*   ADJ_div
   ADJ_div_m(2,2) = dy*dx*    ADJ_div
   ADJ_div        = 0.0

   do ii = i, i+1
      do jj = j, j+1
         coeff = grid%xb%map_factor(ii,jj) * grid%xb%map_factor(ii,jj)*0.5/grid%xb%ds
	 
         jo_grad_x%u(ii+1,jj,k) = jo_grad_x%u(ii+1,jj,k) + ADJ_div_m(ii-i+1,jj-j+1)/grid%xb%map_factor(ii+1,jj)*coeff
								  
         jo_grad_x%u(ii-1,jj,k) = jo_grad_x%u(ii-1,jj,k) - ADJ_div_m(ii-i+1,jj-j+1)/grid%xb%map_factor(ii-1,jj)*coeff
								  
         jo_grad_x%v(ii,jj+1,k) = jo_grad_x%v(ii,jj+1,k) + ADJ_div_m(ii-i+1,jj-j+1)/grid%xb%map_factor(ii,jj+1)*coeff
								  
         jo_grad_x%v(ii,jj-1,k) = jo_grad_x%v(ii,jj-1,k) - ADJ_div_m(ii-i+1,jj-j+1)/grid%xb%map_factor(ii,jj-1)*coeff
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_div_profile_adj")

end subroutine da_div_profile_adj
