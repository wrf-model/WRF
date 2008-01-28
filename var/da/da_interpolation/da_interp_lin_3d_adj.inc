subroutine da_interp_lin_3d_adj(fm3d, info, fo3d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(inout) :: fm3d(ims:ime,jms:jme,kms:kme)
   type(infa_type), intent(in)    :: info
   real,            intent(in)    :: fo3d(1:info%max_lev,info%n1:info%n2) 

   integer :: n,k
   real    :: fmz(kms:kme)

   if (trace_use) call da_trace_entry("da_interp_lin_3d_adj")

   do n=info%n1,info%n2
      fmz = 0.0
      do k = 1, info%levels(n)
         if (info%k(k,n) > 0) then
            fmz(info%k(k,n))   = info%dzm(k,n) * fo3d(k,n) + fmz(info%k(k,n))
            fmz(info%k(k,n)+1) = info%dz (k,n) * fo3d(k,n) + fmz(info%k(k,n)+1)
         end if
      end do
  
      do k=kts,kte
         fm3d(info%i(k,n)  ,info%j(k,n),k)   = info%dym(k,n)*info%dxm(k,n)*fmz(k) + fm3d(info%i(k,n)  ,info%j(k,n)  ,k)
         fm3d(info%i(k,n)+1,info%j(k,n),k)   = info%dym(k,n)*info%dx (k,n)*fmz(k) + fm3d(info%i(k,n)+1,info%j(k,n)  ,k)
         fm3d(info%i(k,n)  ,info%j(k,n)+1,k) = info%dy (k,n)*info%dxm(k,n)*fmz(k) + fm3d(info%i(k,n)  ,info%j(k,n)+1,k)
         fm3d(info%i(k,n)+1,info%j(k,n)+1,k) = info%dy (k,n)*info%dx (k,n)*fmz(k) + fm3d(info%i(k,n)+1,info%j(k,n)+1,k)
      end do
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d_adj")

end subroutine da_interp_lin_3d_adj


