subroutine da_check_rh_simple (grid)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout)  :: grid

   integer :: i, j, k ! Loop counters.
   real    :: q_new   ! Modified rh.
   real    :: es      ! Dummy output.
   real    :: t_new, p_new
   real    :: ramax,ramin,dqmax,dqmin    
   real    :: t_tropp(ims:ime,jms:jme)
   integer :: k_tropp(ims:ime,jms:jme)

   if (trace_use_dull) call da_trace_entry("da_check_rh_simple")        

   ! ramax=maximum_rh/100.0
   ! ramin=minimum_rh/100.0
   ramax = 1.0
   ramin = 1.0e-6

   ! find the k index of tropopause
   t_tropp = 999.0  ! initialize
   k_tropp = kte    ! initialize
   do k = kte, kts, -1
      do j = jts, jte
         do i = its, ite
            if ( grid%xb%t(i,j,k) < t_tropp(i,j) .and.  &
                 grid%xb%p(i,j,k) > 5000.0 ) then  ! tropopause should not
                                                   ! be higher than 50 hPa
               t_tropp(i,j) = grid%xb%t(i,j,k)
               k_tropp(i,j) = k
            end if
         end do
      end do
   end do

   do k = kts, kte
      do j = jts, jte
         do i = its, ite
            p_new  = grid%xb % p(i,j,k) + grid%xa %  p(i,j,k)
            t_new  = grid%xb % t(i,j,k) + grid%xa %  t(i,j,k)

            if ( k > k_tropp(i,j) ) then  ! limit q incement above tropopause
               grid%xa % q(i,j,k) = 0.0
            else
               call da_tp_to_qs(t_new, p_new, es, q_new)
               dqmax=q_new*ramax - grid%xb % q(i,j,k)        
               dqmin=q_new*ramin - grid%xb % q(i,j,k)        
               grid%xa % q(i,j,k)=min(max(grid%xa % q(i,j,k),dqmin),dqmax)
            end if
         end do
      end do
   end do  

   if (trace_use_dull) call da_trace_exit("da_check_rh_simple") 

end subroutine da_check_rh_simple


