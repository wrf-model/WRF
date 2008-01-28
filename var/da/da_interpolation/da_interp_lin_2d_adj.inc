subroutine da_interp_lin_2d_adj(fm2d, info, k, fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(inout) :: fm2d(ims:ime,jms:jme) ! 'Input' variable modified by adjoint
   type(infa_type), intent(in)    :: info
   integer,         intent(in)    :: k                     ! level
   real,            intent(in)    :: fo2d(info%n1:info%n2)           ! 'Output' variable unchanged by adjoint

   integer :: n

   if (trace_use) call da_trace_entry("da_interp_lin_2d_adj")

   do n=info%n1,info%n2
      fm2d(info%i(k,n)  ,info%j(k,n))   = info%dym(k,n) * info%dxm(k,n) * fo2d(n) + fm2d(info%i(k,n)  ,info%j(k,n))
      fm2d(info%i(k,n)+1,info%j(k,n))   = info%dym(k,n) * info%dx(k,n)  * fo2d(n) + fm2d(info%i(k,n)+1,info%j(k,n))
      fm2d(info%i(k,n)  ,info%j(k,n)+1) = info%dy(k,n)  * info%dxm(k,n) * fo2d(n) + fm2d(info%i(k,n)  ,info%j(k,n)+1)
      fm2d(info%i(k,n)+1,info%j(k,n)+1) = info%dy(k,n)  * info%dx(k,n)  * fo2d(n) + fm2d(info%i(k,n)+1,info%j(k,n)+1)
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_2d_adj")

end subroutine da_interp_lin_2d_adj


