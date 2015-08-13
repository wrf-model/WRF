subroutine da_cumulus (zcb, tcb, qcb, pcb, pk, te, z, t, q, lcb, lct, pct, zct, kts, kte)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: kts, kte
   real,    intent(inout) :: zcb, tcb, qcb, pcb
   real,    intent(in)    :: pk(kts:kte)
   real,    intent(in)    :: te(kts:kte)
   real,    intent(out)   :: z(kts:kte)
   real,    intent(out)   :: t(kts:kte)
   real,    intent(out)   :: q(kts:kte)
   integer, intent(out)   :: lcb, lct
   real,    intent(out)   :: pct, zct

   integer   :: k, ia, l, ncb
   real      :: cp, r, hl, em, et, p
   real      :: tll, qll, pll, zll, tbar, pbar, qbar
   real      :: dp, dz, ddt, dt 

   if (trace_use) call da_trace_entry("da_cumulus")    

   cp=1004.0
   r=2000.0/7.0
   hl=2.49e06
   dt=0.1
   ia=1000

   do k = kts, kte
      z(k) = 0.0
      t(k) = 0.0
      q(k) = 0.0
   end do

   em=gravity*zcb+cp*tcb+hl*qcb

   ncb=kts

   if (pk(kte) > pcb) then
      ncb=kte
   end if

   do l=kte-1,kts,-1
      if (pk(l) > pcb) then
         ncb=l+1
         exit
      end if
   end do

   do l=ncb,kte
      p=pk(l)
      do k=1,ia
         if (l == ncb) then
            tll=tcb
            qll=qcb
            pll=pcb
            zll=zcb
         else
            tll=t(l-1)
            qll=q(l-1)
            pll=pk(l-1)
            zll=z(l-1)
         end if

         t(l)=tll-(k*dt)

         call da_qfrmrh(p, t(l), 100.0, q(l))

         tbar=0.5*(t(l)+tll)
         qbar=0.5*(q(l)+qll)
         pbar=0.5*(p+pll)
         dp=pll-p
         dz=(r*tbar*(1.0+0.61*qbar)*dp)/(gravity*pbar)
         z(l)=zll+dz
         et=gravity*z(l)+cp*t(l)+hl*q(l)
         if ((et-em) <= 0.0) exit
      end do
   end do

   lct=ncb

   do k=kte,ncb+1,-1
      ddt=t(k)-te(k)

      if (ddt >= 0.0) then
         lct=k
         exit
      end if
   end do

   lcb=lct

   do k=ncb,kte
      ddt=t(k)-te(k)
      if (ddt >= 0.0) then
         lcb=k
         exit
      end if
   end do

   pct=pk(lct)
   zct=z(lct)
   pcb=pk(lcb)
   zcb=z(lcb)

   if (trace_use) call da_trace_exit("da_cumulus")    

end subroutine da_cumulus


