subroutine da_tv_profile_tl(grid,i,j,dx,dxm,dy,dym,pre_ma,tv_ma,TGL_pre_ma,TGL_tv_ma)

   !--------------------------------------------------------------------------
   ! Purpose: tangent-linear routine for da_tv_profile
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)  :: grid
   integer,        intent(in)  :: i, j     ! OBS location
   real,           intent(in)  :: dx, dxm  ! interpolation weights.
   real,           intent(in)  :: dy, dym  ! interpolation weights.
   real,           intent(out) :: TGL_pre_ma(kts-1:kte+1)
   real,           intent(out) :: TGL_tv_ma(kts-1:kte+1)
   real,           intent(out) :: pre_ma(kts-1:kte+1)
   real,           intent(out) :: tv_ma(kts-1:kte+1)

   integer :: ii,jj
   real    :: tv_m(2,2,kts:kte)
   real    :: TGL_tv_m(2,2,kts:kte)

   if (trace_use_dull) call da_trace_entry("da_tv_profile_tl")

   do ii=i,i+1
      do jj=j,j+1
         TGL_tv_m(ii-i+1,jj-j+1,kts:kte) = grid%xa%t(ii,jj,kts:kte)*(1.0+0.61*grid%xb%q(ii,jj,kts:kte)) +  &
                                      0.61*grid%xb%t(ii,jj,kts:kte)*grid%xa%q(ii,jj,kts:kte)
         tv_m(ii-i+1,jj-j+1,kts:kte)     = grid%xb%t(ii,jj,kts:kte)*(1.0+0.61*grid%xb%q(ii,jj,kts:kte))
      end do
   end do
 
   TGL_pre_ma(kts:kte) = dym* ( dxm * grid%xa%p(i,j,kts:kte) + dx * grid%xa%p(i+1,j,kts:kte) ) + &
                       dy * ( dxm * grid%xa%p(i,j+1,kts:kte) + dx * grid%xa%p(i+1,j+1,kts:kte) )
   TGL_tv_ma (kts:kte) = dym* ( dxm * TGL_tv_m(1,1,kts:kte) + dx * TGL_tv_m(2,1,kts:kte) ) + &
                       dy * ( dxm * TGL_tv_m(1,2,kts:kte) + dx * TGL_tv_m(2,2,kts:kte) )
   pre_ma(kts:kte) = dym* ( dxm * grid%xb%p(i,j,kts:kte) + dx * grid%xb%p(i+1,j,kts:kte) ) + &
                   dy * ( dxm * grid%xb%p(i,j+1,kts:kte) + dx * grid%xb%p(i+1,j+1,kts:kte) )
   tv_ma (kts:kte) = dym* ( dxm * tv_m (1,1,kts:kte) + dx * tv_m (2,1,kts:kte) ) + &
                   dy * ( dxm * tv_m (1,2,kts:kte) + dx * tv_m (2,2,kts:kte) )

   if (trace_use_dull) call da_trace_exit("da_tv_profile_tl")
 
end subroutine da_tv_profile_tl


