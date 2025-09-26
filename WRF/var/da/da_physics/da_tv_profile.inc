subroutine da_tv_profile(grid, info, n, pre_ma, tv_ma)

   !--------------------------------------------------------------------------
   ! Purpose: Calculates virtual temperature (tv_ma) on each level
   ! (pre_ma, pressure at the level) at the observed location (i,j). 
   ! dx, dxm, dy, dym are horizontal interpolation weighting.
   !--------------------------------------------------------------------------

   implicit none

   type (domain),         intent(in)  :: grid
   type (infa_type),      intent(in)  :: info
   integer,               intent(in)  :: n
   real,                  intent(out) :: pre_ma(kts-1:kte+1)
   real,                  intent(out) :: tv_ma(kts-1:kte+1)
                          
   integer :: ii,jj    ! index dimension.
   real    :: tv_m(2,2,kts:kte)     ! Virtual temperatures

   integer :: i, j      ! OBS location
   real    :: dx, dxm   ! interpolation weights.
   real    :: dy, dym   ! interpolation weights.

   if (trace_use_dull) call da_trace_entry("da_tv_profile")

   i   = info%i(1,n)
   j   = info%j(1,n)
   dx  = info%dx(1,n)
   dy  = info%dy(1,n)
   dxm = info%dxm(1,n)
   dym = info%dym(1,n)

   ! Virtual temperature

   do ii=i,i+1
      do jj=j,j+1
         tv_m(ii-i+1,jj-j+1,kts:kte) = grid%xb%t(ii,jj,kts:kte) * &
            (1.0 + 0.61*grid%xb%q(ii,jj,kts:kte))
      end do
   end do

   ! Horizontal interpolation to the obs. pt.

   pre_ma(kts:kte) = dym* ( dxm * grid%xb%p(i,j,kts:kte) + dx * grid%xb%p(i+1,j,kts:kte) ) + &
      dy * ( dxm * grid%xb%p(i,j+1,kts:kte) + dx * grid%xb%p(i+1,j+1,kts:kte) )

   tv_ma (kts:kte) = dym* ( dxm * tv_m (1,1,kts:kte) + dx * tv_m (2,1,kts:kte) ) + &
      dy * ( dxm * tv_m (1,2,kts:kte) + dx * tv_m (2,2,kts:kte) )

   if (trace_use_dull) call da_trace_exit("da_tv_profile")

end subroutine da_tv_profile


