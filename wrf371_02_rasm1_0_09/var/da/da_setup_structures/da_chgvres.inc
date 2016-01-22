subroutine da_chgvres(nlath,nsig,kz,sigmah,sigma_avn,&
   corz_avn,cord_avn,corh_avn,corq_avn,hwll_avn,vztdq_avn,agv_avn,bv_avn,wgv_avn,&
   corz_kz, cord_kz, corh_kz, corq_kz, hwll_kz, vztdq_kz, agv_kz, bv_kz, wgv_kz)

   !---------------------------------------------------------------------------
   ! Purpose: Change vertical resolution of background stats for cv_options=3
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: nlath,nsig,kz
   real, intent(in)    ::  sigmah(kz),sigma_avn(1:nsig)

   real, intent(out) :: corz_kz(1:2*nlath+1,1:kz),cord_kz(1:2*nlath+1,1:kz) 
   real, intent(out) :: corh_kz(1:2*nlath+1,1:kz),corq_kz(1:2*nlath+1,1:kz)
   real, intent(out) :: hwll_kz(0:nlath*2+1,1:kz,1:4)
   real, intent(out) :: vztdq_kz(1:kz,0:nlath*2+1,1:4)
   real, intent(out) :: agv_kz(0:nlath*2+1,1:kz,1:kz)
   real, intent(out) :: bv_kz(0:nlath*2+1,1:kz),wgv_kz(0:nlath*2+1,1:kz)

   real, intent(in) :: corz_avn(1:2*nlath+1,1:nsig),cord_avn(1:2*nlath+1,1:nsig)
   real, intent(in) :: corh_avn(1:2*nlath+1,1:nsig),corq_avn(1:2*nlath+1,1:nsig)
   real, intent(in) :: hwll_avn(0:nlath*2+1,1:nsig,1:4)
   real, intent(in) :: vztdq_avn(1:nsig,0:nlath*2+1,1:4)
   real, intent(in) :: agv_avn(0:nlath*2+1,1:nsig,1:nsig)
   real, intent(in) :: bv_avn(0:nlath*2+1,1:nsig),wgv_avn(0:nlath*2+1,1:nsig)

   integer :: i,j,k,m,l,l1,m1,n
   real    :: rsigo(nsig),rsig(kz)
   real    :: coef1(kz),coef2(kz)
   integer :: lsig(kz)

   if (trace_use) call da_trace_entry("da_chgvres")

   if (kz==nsig) then
      corz_kz=corz_avn
      cord_kz=cord_avn
      corh_kz=corh_avn
      corq_kz=corq_avn
      hwll_kz=hwll_avn
      vztdq_kz=vztdq_avn
      agv_kz=agv_avn
      bv_kz=bv_avn
      wgv_kz=wgv_avn
      if (trace_use) call da_trace_exit("da_chgvres")
      return
   end if

   do k=1,kz
      rsig(k)=log(sigmah(k))
   end do
   do k=1,nsig
      rsigo(k)=log(sigma_avn(k))
   end do

   do k=1,kz
      if (rsig(k).ge.rsigo(1)) then
        m=1
        m1=2
        lsig(k)=1
        coef1(k)=1.0
           coef2(k)=0.0
      else if (rsig(k).lt.rsigo(nsig)) then
         m=nsig-1
         m1=nsig
         lsig(k)=nsig-1
         coef1(k)=0.0
         coef2(k)=1.0
      else
         do m=1,nsig
            m1=m+1
            if ((rsig(k).le.rsigo(m))   .and.  &
                (rsig(k).gt.rsigo(m1))    )then
               lsig(k)=m
               go to 2345
            end if
         end do
2345     continue
         coef1(k)=(rsigo(m1)-rsig(k))/(rsigo(m1)-rsigo(m))
         coef2(k)=1.0-coef1(k)
         if (lsig(k)==nsig) then
            lsig(k)=nsig-1
            coef2(k)=1.0
            coef1(k)=0.0
         end if
      end if
   end do

   ! agv wgv bv
   do k=1,kz
      m=lsig(k)
      m1=m+1
      do i=1,nlath*2
         wgv_kz(i,k)=wgv_avn(i,m)*coef1(k)+wgv_avn(i,m1)*coef2(k)
         bv_kz(i,k)=bv_avn(i,m)*coef1(k)+bv_avn(i,m1)*coef2(k)
      end do

      do j=1,kz
         l=lsig(j)
         l1=l+1
         do i=1,nlath*2
            agv_kz(i,j,k)=(agv_avn(i,l,m)*coef1(j)+agv_avn(i,l1,m)*coef2(j))*coef1(k) &
                    +(agv_avn(i,l,m1)*coef1(j)+agv_avn(i,l1,m1)*coef2(j))*coef2(k)
         end do
      end do
   end do

   agv_kz(0,:,:)=agv_kz(1,:,:)
   wgv_kz(0,:)=wgv_kz(1,:)
   bv_kz(0,:)=bv_kz(1,:)
   agv_kz(nlath*2+1,:,:)=agv_kz(nlath*2,:,:)
   wgv_kz(nlath*2+1,:)=wgv_kz(nlath*2,:)
   bv_kz(nlath*2+1,:)=bv_kz(nlath*2,:)

   do k=1,kz
      m=lsig(k)
      m1=m+1

      ! corz,cord,corh,corq
      do i=1,nlath*2
         corz_kz(i,k)=corz_avn(i,m)*coef1(k)+corz_avn(i,m1)*coef2(k)
         cord_kz(i,k)=cord_avn(i,m)*coef1(k)+cord_avn(i,m1)*coef2(k)
         corh_kz(i,k)=corh_avn(i,m)*coef1(k)+corh_avn(i,m1)*coef2(k)
         corq_kz(i,k)=corq_avn(i,m)*coef1(k)+corq_avn(i,m1)*coef2(k)
      end do

      do n=1,4 
         do i=1,nlath*2
            ! hwll
            hwll_kz(i,k,n)=hwll_avn(i,m,n)*coef1(k)+hwll_avn(i,m1,n)*coef2(k)
            ! vztdq
            vztdq_kz(k,i,n)=vztdq_avn(m,i,n)*coef1(k)+vztdq_avn(m1,i,n)*coef2(k)
          end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_chgvres")

end subroutine da_chgvres


