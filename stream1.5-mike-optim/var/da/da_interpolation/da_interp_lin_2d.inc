subroutine da_interp_lin_2d(fm2d, info, k, fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(in)  :: fm2d(ims:ime,jms:jme) ! Input variable
   type(infa_type), intent(in)  :: info
   integer,         intent(in)  :: k                     ! level
   real,            intent(out) :: fo2d(info%n1:info%n2)           ! Output variable 
   
   integer :: n

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_2d")

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (n) 
   do n=info%n1,info%n2
      fo2d(n) = info%dym(k,n)*(info%dxm(k,n)*fm2d(info%i(k,n),info%j(k,n))   + info%dx(k,n)*fm2d(info%i(k,n)+1,info%j(k,n))) &
              + info%dy(k,n) *(info%dxm(k,n)*fm2d(info%i(k,n),info%j(k,n)+1) + info%dx(k,n)*fm2d(info%i(k,n)+1,info%j(k,n)+1))
   end do
   !$OMP END PARALLEL DO

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_2d")

end subroutine da_interp_lin_2d


