subroutine da_interp_lin_2d_adj_partial(fm2d, info, k1,k2, fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(inout) :: fm2d(ims:ime,jms:jme,k1:k2) ! 'Input' variable modified by adjoint
   type(infa_type), intent(in)    :: info
   integer,         intent(in)    :: k1,k2                 ! Range of levels
   real,            intent(in)    :: fo2d(k1:k2,info%n1:info%n2)           ! 'Output' variable unchanged by adjoint

   integer :: n,k

   if (trace_use) call da_trace_entry("da_interp_lin_2d_adj_partial")

   do n=info%n1,info%n2
      do k=k1,k2
         fm2d(info%i(k,n)  ,info%j(k,n),k)   = info%dym(k,n) * info%dxm(k,n) * fo2d(k,n) + fm2d(info%i(k,n)  ,info%j(k,n),k)
         fm2d(info%i(k,n)+1,info%j(k,n),k)   = info%dym(k,n) * info%dx(k,n)  * fo2d(k,n) + fm2d(info%i(k,n)+1,info%j(k,n),k)
         fm2d(info%i(k,n)  ,info%j(k,n)+1,k) = info%dy(k,n)  * info%dxm(k,n) * fo2d(k,n) + fm2d(info%i(k,n)  ,info%j(k,n)+1,k)
         fm2d(info%i(k,n)+1,info%j(k,n)+1,k) = info%dy(k,n)  * info%dx(k,n)  * fo2d(k,n) + fm2d(info%i(k,n)+1,info%j(k,n)+1,k)
      end do
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_2d_adj_partial")

end subroutine da_interp_lin_2d_adj_partial


