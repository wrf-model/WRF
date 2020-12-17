#ifdef A2C
subroutine da_interp_lin_3d_adj(fm3d, info, fo3d,string)
#else
subroutine da_interp_lin_3d_adj(fm3d, info, fo3d)
#endif

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(inout) :: fm3d(ims:ime,jms:jme,kms:kme)
   type(infa_type), intent(in)    :: info
   real,            intent(in)    :: fo3d(1:info%max_lev,info%n1:info%n2) 
#ifdef A2C
   character*1, optional, intent(in) :: string
#endif

   integer :: n,k
   real    :: fmz(kms:kme)
#ifdef A2C
   integer :: ii,jj
   real    :: dx,dy,dxm,dym
#endif

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
          jj = jj  + 1
         end if
        endif
         fm3d(ii   ,jj  ,k)   = dym*dxm*fmz(k) + fm3d(ii ,jj ,k)
         fm3d(ii+1 ,jj  ,k)   = dym*dx *fmz(k) + fm3d(ii+1,jj  ,k)
         fm3d(ii   ,jj+1,k)   = dy *dxm*fmz(k) + fm3d(ii ,jj+1,k)
         fm3d(ii+1 ,jj+1,k)   = dy *dx *fmz(k) + fm3d(ii+1,jj+1,k)
#else
        fm3d(info%i(k,n)  ,info%j(k,n),k)   = info%dym(k,n)*info%dxm(k,n)*fmz(k) + fm3d(info%i(k,n)  ,info%j(k,n)  ,k)
         fm3d(info%i(k,n)+1,info%j(k,n),k)   = info%dym(k,n)*info%dx (k,n)*fmz(k) + fm3d(info%i(k,n)+1,info%j(k,n)  ,k)
         fm3d(info%i(k,n)  ,info%j(k,n)+1,k) = info%dy (k,n)*info%dxm(k,n)*fmz(k) + fm3d(info%i(k,n)  ,info%j(k,n)+1,k)
         fm3d(info%i(k,n)+1,info%j(k,n)+1,k) = info%dy (k,n)*info%dx (k,n)*fmz(k) + fm3d(info%i(k,n)+1,info%j(k,n)+1,k)
#endif
      end do
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_3d_adj")

end subroutine da_interp_lin_3d_adj
