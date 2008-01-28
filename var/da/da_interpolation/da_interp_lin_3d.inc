subroutine da_interp_lin_3d(fm3d, info, fo3d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(in)    :: fm3d(ims:ime,jms:jme,kms:kme) ! Input variable 
   type(infa_type), intent(in)    :: info       
   real,            intent(inout) :: fo3d(1:info%max_lev,info%n1:info%n2)           ! Output variable 

   integer :: n, k
   real    :: fmz(kms:kme)

   if (trace_use) call da_trace_entry("da_interp_lin_3d")

   fo3d(:,:) = 0.0

   do n=info%n1,info%n2
      fmz(:)=0.0

      do k = kts,kte
         fmz(k) = &
              info%dym(k,n) * (info%dxm(k,n)*fm3d(info%i(k,n), info%j(k,n), k) &
            + info%dx (k,n) * fm3d(info%i(k,n)+1,info%j(k,n), k)) &
            + info%dy (k,n) * (info%dxm(k,n)*fm3d(info%i(k,n), info%j(k,n)+1, k) &
            + info%dx (k,n) * fm3d(info%i(k,n)+1, info%j(k,n)+1, k))
      end do
      do k = 1, info%levels(n)
         if (info%k(k,n) > 0) then
            fo3d(k,n) = info%dzm(k,n)*fmz(info%k(k,n)) + info%dz(k,n)*fmz(info%k(k,n)+1)
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d")

end subroutine da_interp_lin_3d


