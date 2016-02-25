subroutine da_cloud_model (TB, PB, QB, QCWB, QRNB, ZB, ZFB, DT, kts, kte)

   !-----------------------------------------------------------------
   ! Purpose: Calculate DT (=dz/w) using cumulus parameterization 
   !          of a one-dimensional cloud model.
   !-----------------------------------------------------------------

   ! Calculate DT

   implicit none

   integer, intent(in)                     :: kts, kte
   real, intent(in),  dimension(kts:kte)   :: TB, PB, QB, QCWB, QRNB, ZB
   real, intent(in),  dimension(kts:kte+1) :: ZFB
   real, intent(out), dimension(kts:kte)   :: DT

   integer                    :: k
   real                       :: P0, Z0, T0, Q0
   real                       :: PLCL, ZLCL, TLCL, QLCL
   integer                    :: KCB, KCT
   real                       :: PCT, ZCT
   real, dimension(kts:kte)   :: ZC, TC, QC, PP, QT
   real, dimension(kts:kte)   :: TCV, TBV, B
   real                       :: ALPHA, RC, MU, XX, YY
   real, dimension(kts:kte+1) :: W0, W

   if (trace_use) call da_trace_entry("da_cloud_model")

   ALPHA=0.5
   RC=100.0
   MU=0.183/RC

   do k = kts, kte+1
      W0(k)=0.0  
      W(k)=0.0  
   end do

   do k = kts, kte
      PP(k)=PB(k)/100.0
      DT(k)=0.0
   end do

   P0 = PP(kts)
   Z0 = ZB(kts)
   T0 = MAX(TB(kts),303.0)

   call da_qfrmrh (P0, T0, 95.0, Q0)

   call da_lcl (P0, Z0, T0, Q0, PLCL, ZLCL, TLCL, QLCL)

   call da_qfrmrh (PLCL, TLCL, 95.0, QLCL)

   call da_cumulus (ZLCL, TLCL, QLCL, PLCL, PP, TB,            &
                  ZC, TC, QC, KCB, KCT, PCT, ZCT, kts, kte)

   do k = KCB, KCT
      TCV(k) = TC(k) * (1.0 + 0.608 * QC(k))
      TBV(k) = TB(k) * (1.0 + 0.608 * QB(k))
   
      B(k) = (TCV(k)-TBV(k)) / TBV(k)

      QT(k) = QC(k) + QCWB(k) + QRNB(k)
   end do

   W0(KCB) = 0.0
   do k = KCB+1, KCT+1
      XX = 1.0+2.0*MU*(ZFB(k)-ZFB(k-1))
      YY = 2.0*gravity*(B(k-1)/(1.0+ALPHA) - QT(k-1)) * (ZFB(k)-ZFB(k-1))
      W0(k) =  (W0(k-1)+YY) / XX
   end do
     
   do k = KCB, KCT+1
      if (W0(k) >= 0.0) then
         W(k) = sqrt(W0(k))
      end if
   end do


   do k = KCT, KCB+1, -1
      if (W(k) >= 0.01) then
         DT(k) = (ZB(k)-ZB(k-1))/W(k)
      else
         DT(k) = 0.0
      end if
   end do

   if (trace_use) call da_trace_exit("da_cloud_model")

end subroutine da_cloud_model


