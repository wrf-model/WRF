#ifdef A2C
subroutine da_interp_lin_3d(fm3d, info, fo3d,string)
#else
subroutine da_interp_lin_3d(fm3d, info, fo3d)
#endif

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(in)    :: fm3d(ims:ime,jms:jme,kms:kme) ! Input variable 
   type(infa_type), intent(in)    :: info       
   real,            intent(inout) :: fo3d(1:info%max_lev,info%n1:info%n2)           ! Output variable 
#ifdef A2C
   character*1, optional, intent(in) :: string
#endif

   integer :: n, k
   real    :: fmz(kms:kme)
#ifdef A2C
   integer :: ii,jj                 
   real    :: dx,dy,dxm,dym         
#endif

   if (trace_use) call da_trace_entry("da_interp_lin_3d")

   fo3d(:,:) = 0.0

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( n, fmz, k )
   do n=info%n1,info%n2
      fmz(:)=0.0

      do k = kts,kte
#ifdef A2C
        ii =  info%i  (k,n) 
        jj =  info%j  (k,n) 
        dx =  info%dx (k,n) 
        dy =  info%dy (k,n) 
        dxm=  info%dxm(k,n) 
        dym=  info%dym(k,n) 

        if(present(string) .and. string == 'u' ) then
         if( dx <= 0.5 ) then
           dx  = dx + 0.5
           dxm = 1.0 - dx
         else 
          dx  = dx - 0.5
          dxm = 1.0 - dx
           ii = ii + 1
         end if 
        endif
        if(present(string) .and. string == 'v' ) then
         if( dy <= 0.5 ) then
           dy  = dy + 0.5
           dym = 1.0 - dy
         else  
          dy  = dy - 0.5
          dym = 1.0 - dy
          jj = jj + 1
         end if 
        endif
         fmz(k) = dym * (dxm*fm3d(ii,  jj, k) + dx * fm3d(ii+1, jj  , k)) &
                 +dy  * (dxm*fm3d(ii,jj+1, k) + dx * fm3d(ii+1, jj+1, k))
#else
         fmz(k) = &
              info%dym(k,n) * (info%dxm(k,n)*fm3d(info%i(k,n), info%j(k,n), k) &
            + info%dx (k,n) * fm3d(info%i(k,n)+1,info%j(k,n), k)) &
            + info%dy (k,n) * (info%dxm(k,n)*fm3d(info%i(k,n), info%j(k,n)+1, k) &
            + info%dx (k,n) * fm3d(info%i(k,n)+1, info%j(k,n)+1, k))
#endif
      end do
      do k = 1, info%levels(n)
         if (info%k(k,n) > 0) then
            fo3d(k,n) = info%dzm(k,n)*fmz(info%k(k,n)) + info%dz(k,n)*fmz(info%k(k,n)+1)
         end if
      end do
   end do
   !$OMP END PARALLEL DO

   if (trace_use) call da_trace_exit("da_interp_lin_3d")

end subroutine da_interp_lin_3d
