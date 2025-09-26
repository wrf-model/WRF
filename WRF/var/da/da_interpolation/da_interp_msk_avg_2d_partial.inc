subroutine da_interp_msk_avg_2d_partial(fm2d, info, k, n1, n2, fo2d, mask2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(in)  :: fm2d(ims:ime,jms:jme)  ! Input variable
   type(infa_type), intent(in)  :: info
   integer,         intent(in)  :: k                      ! level
   integer,         intent(in)  :: n1,n2                  ! Range of obs
   real,            intent(out) :: fo2d(n1:n2)            ! Output variable 
   integer, optional, intent(in):: mask2d(ims:ime,jms:jme)! Input variable mask (1=valid, 0=invalid)
   
   integer :: n

   if (trace_use_frequent) call da_trace_entry("da_interp_msk_avg_2d_partial")

   do n=n1,n2
      fo2d(n) = info%dym(k,n)*(info%dxm(k,n)*fm2d(info%i(k,n),info%j(k,n))   + info%dx(k,n)*fm2d(info%i(k,n)+1,info%j(k,n))) &
              + info%dy(k,n) *(info%dxm(k,n)*fm2d(info%i(k,n),info%j(k,n)+1) + info%dx(k,n)*fm2d(info%i(k,n)+1,info%j(k,n)+1))
   end do

   if (trace_use_frequent) call da_trace_exit("da_interp_msk_avg_2d_partial")

end subroutine da_interp_msk_avg_2d_partial


