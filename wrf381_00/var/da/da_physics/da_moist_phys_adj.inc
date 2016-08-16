subroutine da_moist_phys_adj(grid)

   !---------------------------------------------------------------------------
   !  Purpose: Partition of the hydrometeors via the moist egrid%xplicit scheme.
   !           A warm rain process is used in this subroutine. 
   !           This is the adjoint code of the scheme.
   !
   !  Method: The warm rain process is according to Hsie and Anthes (1984)
   !          and Dudhia (1989)
   !
   !  Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

   real, dimension(ims:ime,jms:jme,kms:kme) :: T_OLD,T_NEW
   real, dimension(ims:ime,jms:jme,kms:kme) :: Q_OLD,Q_NEW
   real, dimension(ims:ime,jms:jme,kms:kme) :: QCW_OLD,QCW_NEW
   real, dimension(ims:ime,jms:jme,kms:kme) :: QRN_OLD,QRN_NEW

   real, dimension(kms:kme)                 :: EES, QVSS
   real, dimension(kms:kme)                 :: EES9, QVSS9

   real, dimension(its:ite,jts:jte,kms:kme) :: DT
   real, dimension(kms:kme)                   :: QVT,QCT,QRT,TTT
   real, dimension(kms:kme)                   :: QVT9,QCT9,QRT9,TTT9
   real, dimension(kms:kme) :: SCR2,SCR3,SCR4,SCR5,SCR6
   real, dimension(kms:kme) :: DUM31
   real, dimension(kms:kme) :: PRA,PRC,PRD,PRE
   real, dimension(kms:kme) :: SCR31,SCR42,SCR71
   real, dimension(kms:kme) :: DUM112,DUM113,DUM211,DUM411
   real, dimension(kms:kme) :: PRA2,PRC2

   real, dimension(kms:kme) :: SCR29,SCR39,SCR49,SCR59,SCR69
   real, dimension(kms:kme) :: DUM319
   real, dimension(kms:kme) :: PRA9,PRC9,PRD9,PRE9
   real, dimension(kms:kme) :: SCR319,SCR429,SCR719
   real, dimension(kms:kme) :: DUM1129,DUM1139,DUM2119,DUM4119
   real, dimension(kms:kme) :: TMP

   real, parameter :: QCTH  = 0.5E-3 
   real, parameter :: QRTH  = 1.0e-6  
   real, parameter :: alpha = 0.001
   real, parameter :: beta  = 0.0486
   real, parameter :: gamma = 0.002

   integer :: i, j, k
   real    :: tmp1, tmp2
   real    :: qrth1

   if (trace_use) call da_trace_entry("da_moist_phys_adj")

   qrth1 = (QRTH*1.0e3)**0.875

   QRN_NEW(its:ite,jts:jte,kts:kte) = grid%xa % qrn (its:ite,jts:jte,kts:kte) 
   QRN_OLD(its:ite,jts:jte,kts:kte) = grid%xa % qrn (its:ite,jts:jte,kts:kte)
   QCW_NEW(its:ite,jts:jte,kts:kte) = grid%xa % qcw (its:ite,jts:jte,kts:kte)
   QCW_OLD(its:ite,jts:jte,kts:kte) = grid%xa % qcw (its:ite,jts:jte,kts:kte)
   Q_NEW(its:ite,jts:jte,kts:kte) = grid%xa % q (its:ite,jts:jte,kts:kte)
   Q_OLD(its:ite,jts:jte,kts:kte) = grid%xa % q (its:ite,jts:jte,kts:kte)
   T_NEW(its:ite,jts:jte,kts:kte) = grid%xa % t (its:ite,jts:jte,kts:kte)
   T_OLD(its:ite,jts:jte,kts:kte) = grid%xa % t (its:ite,jts:jte,kts:kte)

   call da_filter_adj(grid,t_new)
   call da_filter_adj(grid,q_new)
   call da_filter_adj(grid,qcw_new)
   call da_filter_adj(grid,qrn_new)

   grid%xa % qrn (its:ite,jts:jte,kts:kte) = QRN_NEW(its:ite,jts:jte,kts:kte)
   QRN_OLD(its:ite,jts:jte,kts:kte) = QRN_OLD(its:ite,jts:jte,kts:kte) - QRN_NEW(its:ite,jts:jte,kts:kte)
   grid%xa % qcw (its:ite,jts:jte,kts:kte) = QCW_NEW(its:ite,jts:jte,kts:kte)
   QCW_OLD(its:ite,jts:jte,kts:kte) = QCW_OLD(its:ite,jts:jte,kts:kte) - QCW_NEW(its:ite,jts:jte,kts:kte)
   grid%xa % q (its:ite,jts:jte,kts:kte) = Q_NEW(its:ite,jts:jte,kts:kte)
   Q_OLD(its:ite,jts:jte,kts:kte) = Q_OLD(its:ite,jts:jte,kts:kte) - Q_NEW(its:ite,jts:jte,kts:kte)
   grid%xa % t (its:ite,jts:jte,kts:kte) = T_NEW(its:ite,jts:jte,kts:kte)
   T_OLD(its:ite,jts:jte,kts:kte) = T_OLD(its:ite,jts:jte,kts:kte) - T_NEW(its:ite,jts:jte,kts:kte)

   DT(its:ite,jts:jte,kts:kte) = grid%xb%delt(its:ite,jts:jte,kts:kte)

   do j=jts,jte
      do i=its,ite
         do K=kts,kte

            if (DT(i,j,k) <= 0.0) cycle

            if( grid%xb%t(I,J,K) > TO )then
               EES(K)=SVP1*EXP(SVP2*(grid%xb%t(I,J,K)-SVPT0)/(grid%xb%t(I,J,K)-SVP3))
            else
               EES(K)=.611*EXP(22.514-6.15E3/grid%xb%t(I,J,K))
            end if

            QVSS(K)=622.0*EES(K)/(grid%xb%p(I,J,K)-EES(K))

            SCR4(K)=grid%xb%q(I,J,K)/QVSS(K)

            if(grid%xb%qcw(I,J,K) > 0.0) then
               SCR2(K)=grid%xb%qcw(I,J,K)
            else
               SCR2(K)=0.0
            end if
            if(grid%xb%qrn(I,J,K) > 1.0e-25) then
               SCR3(K)=grid%xb%qrn(I,J,K)
            else
               SCR3(K)=1.0e-25
            end if
            SCR5(K)=grid%xb%q(I,J,K)/SCR4(K)

            SCR6(K)=grid%xb%p(I,J,K)/(gas_constant*grid%xb%t(I,J,K))

            DUM31(K)=3.1484E6-XLV1*grid%xb%t(I,J,K)

            ! autoc

            if ( SCR2(k) >= QCTH ) then
               PRC(k) = alpha * ( SCR2(k) - QCTH )
            else
               PRC(k) = 0.0
            end if

            PRC2(K)=PRC(K)

            ! accre

            if ( SCR2(k) > 0.0 .and. SCR3(k) > QRTH ) then
               PRA(k) = gamma * SCR2(k) * (SCR3(k)*1.0e3)**0.875
            else if ( SCR2(k) > 0.0 .and. SCR3(k) <= QRTH ) then
               PRA(k) = gamma * SCR2(k) * (QRTH*1.0e3)**0.875 
            else
               PRA(k) = 0.0
            end if

            PRA2(K)=PRA(K)

            ! evapo

            if (scr3(k) > qrth .and. grid%xb%q(I,J,k) < scr5(k)) then
               pre(k) = beta * (grid%xb%q(I,J,k)-scr5(k) ) * (scr6(k)*scr3(k))**0.65
            else if (scr3(k) <= qrth .and. scr3(k) > 0.0 .and. grid%xb%q(I,J,k) < scr5(k)) then
               pre(k) = beta * (grid%xb%q(I,J,k)-scr5(k)) * (scr6(k)*qrth)**0.65
            else
               pre(k) = 0.0
            end if

            if ( pre(k) < -scr3(k)/dt(i,j,k) ) then
               pre(k) = -scr3(k) / dt(i,j,k)
            end if

            !  Readjust

            DUM112(K)=(PRC(k)+PRA(k))*DT(i,j,k)
            if (DUM112(K) > SCR2(k)) then
               PRC(k)=SCR2(K)*PRC(K)/DUM112(K)
               PRA(k)=SCR2(K)*PRA(K)/DUM112(K)
            end if
            QVT(K)=-PRE(K)
            QCT(K)=-PRC(K)-PRA(K)
            QRT(K)=PRC(K)+PRA(K)+PRE(K)
            if(grid%xb%t(I,J,K).GT.TO)then
               DUM411(K)=DUM31(K)
            else
               DUM411(K)=XLS
            end if
            PRD(K)=cp*(1.0+0.887*grid%xb%q(I,J,K))
            TTT(K)=-DUM411(K)*QVT(K)/PRD(K)

            DUM113(K)=grid%xb%q(I,J,K)+DT(i,j,k)*QVT(K)
            if(DUM113(K) > 1.0E-12 ) then
               SCR42(K)=DUM113(K)
            else
               SCR42(K)=1.0E-12
            end if
            DUM211(K)=grid%xb%qcw(I,J,K)+QCT(K)*DT(i,j,k)
            if(DUM211(K) > 0.0) then
               SCR31(K)=DUM211(K)
            else
               SCR31(K)=0.0
            end if
            SCR71(K)=grid%xb%t(I,J,K)+TTT(K)*DT(i,j,k)
         end do

         call da_condens_adj(DT(i,j,:),SCR31,SCR42,SCR71,DUM31,PRD,       &
                             QVT,QCT,QRT,TTT,                      &
                             grid%xb%p(I,J,:),grid%xb%t(I,J,:),grid%xb%q(I,J,:),  &
                             grid%xb%qcw(I,J,:),grid%xb%qrn(I,J,:),          &
                             SCR319,SCR429,SCR719,DUM319,PRD9,     &
                             QVT9,QCT9,QRT9,TTT9,                  &
                             grid%xa%p(I,J,:),grid%xa%t(I,J,:),grid%xa%q(I,J,:),  &
                             grid%xa%qcw(I,J,:),grid%xa%qrn(I,J,:),kts,kte)

         do K=kts, kte
            if (DT(i,j,k) <= 0.0) cycle

            !  Readjust

            grid%xa%t(I,J,K)=grid%xa%t(I,J,K)+SCR719(K)
            TTT9(K)=TTT9(K)+DT(i,j,k)*SCR719(K)
            DUM2119(K)=0.0
            if(DUM211(K) > 0.0) then
               DUM2119(K)=SCR319(K)
            end if
            grid%xa%qcw(I,J,K)=grid%xa%qcw(I,J,K)+DUM2119(K)
            QCT9(K)=QCT9(K)+DT(i,j,k)*DUM2119(K)
            DUM1139(K)=0.0
            if(DUM113(K) > 1.0e-12 ) then
               DUM1139(K)=SCR429(K)
            end if
            grid%xa%q(I,J,K)=grid%xa%q(I,J,K)+DUM1139(K)
            QVT9(K)=QVT9(K)+DT(i,j,k)*DUM1139(K)
            PRD9(K)=PRD9(K)+DUM411(K)*QVT(K)/(PRD(K)*PRD(K))*TTT9(K)
            QVT9(K)=QVT9(K)-DUM411(K)/PRD(K)*TTT9(K)
            DUM4119(K)=-QVT(K)/PRD(K)*TTT9(K)
            grid%xa%q(I,J,K)=grid%xa%q(I,J,K)+cp*0.887*PRD9(K)

            if(grid%xb%t(I,J,K).GT.TO)then
               DUM319(K)=DUM319(K)+DUM4119(K)
            end if
            PRC9(K)=QRT9(K)
            PRA9(K)=QRT9(K)
            PRE9(K)=QRT9(K)
            PRC9(K)=PRC9(K)-QCT9(K)
            PRA9(K)=PRA9(K)-QCT9(K)
            PRE9(K)=PRE9(K)-QVT9(K)

            if (DUM112(K) > SCR2(k)) then
               DUM1129(K)=-SCR2(K)*PRA2(K)/(DUM112(K)*DUM112(K))*PRA9(K)
               SCR29(K)=PRA2(K)/DUM112(K)*PRA9(K)
               PRA9(K)=PRA9(K)*SCR2(K)/DUM112(K)
               DUM1129(K)=DUM1129(K)-SCR2(K)*PRC2(K)/(DUM112(K)*DUM112(K))*PRC9(K)
               SCR29(K)=SCR29(K)+PRC2(K)/DUM112(K)*PRC9(K)
               PRC9(K)=PRC9(K)*SCR2(K)/DUM112(K)
            else
               SCR29(K)=0.0
               DUM1129(K)=0.0        
            end if
            PRC9(K)=PRC9(K)+DT(i,j,k)*DUM1129(K)
            PRA9(K)=PRA9(K)+DT(i,j,k)*DUM1129(K)

            ! evapo

            if ( SCR3(K) > QRTH .and. grid%xb%q(I,J,k) < SCR5(k) ) then
               PRE(k) = beta * ( grid%xb%q(I,J,k)-SCR5(K) ) * ( SCR6(k)*SCR3(K) )**0.65
            else if ( SCR3(K) <= QRTH .and. SCR3(k) > 0.0 .and. grid%xb%q(I,J,k) < SCR5(k) ) then
               PRE(k) = beta * ( grid%xb%q(I,J,k)-SCR5(K) ) * ( SCR6(k)*QRTH )**0.65
            else
               PRE(k) = 0.0
            end if

            SCR39(k) = 0.0
            if ( PRE(k) < -SCR3(k)/DT(i,j,k) ) then
               SCR39(k) = -PRE9(k) / DT(i,j,k)
               PRE9(k)  = 0.0
            end if

            SCR59(k) = 0.0
            SCR69(k) = 0.0
            if ( SCR3(k) > QRTH .and. grid%xb%q(I,J,k) < SCR5(k) ) then
               TMP1  = beta * ( grid%xb%q(I,J,k)-SCR5(k) ) * 0.65 * ( SCR6(k)*SCR3(k) )**(-0.35)
               TMP2 = beta * ( SCR6(k)*SCR3(k) )**0.65

               grid%xa%q(I,J,k) = grid%xa%q(I,J,k) + TMP2 * PRE9(k)
               SCR59(k) = -TMP2 * PRE9(k)
               SCR39(k) = SCR39(k) + TMP1 * SCR6(k) * PRE9(k)
               SCR69(k) = TMP1 * SCR3(k) * PRE9(k)
            else if (SCR3(k) <= QRTH .and. SCR3(k) > 0.0 .and. grid%xb%q(I,J,k) < SCR5(k) ) then
               TMP1  = beta * ( grid%xb%q(I,J,k)-SCR5(k) ) * 0.65 * ( SCR6(k)*QRTH )**(-0.35)
               TMP2 = beta * ( SCR6(k)*QRTH )**0.65

               grid%xa%q(I,J,k) = grid%xa%q(I,J,k) + TMP2 * PRE9(k)
               SCR59(k) = -TMP2 * PRE9(k)
               SCR69(k) = TMP1 * QRTH * PRE9(k)
            end if
            ! accre

            if ( SCR2(k) > 0.0 .and. SCR3(k) > QRTH ) then
               SCR39(K) = SCR39(K) + gamma * 0.875 * SCR2(k) * (SCR3(K)*1.0e3)**(-0.125 ) * 1.0e3 * PRA9(K)
               SCR29(K) = SCR29(K) + gamma * (SCR3(K)*1.0e3)**0.875 * PRA9(K)
            else if (SCR2(k) > 0.0 .and. SCR3(k) <= QRTH ) then
               SCR29(k) = SCR29(k) + gamma * (QRTH1 * PRA9(k))
            end if
      
            ! autoc

            if ( scr2(k) >= qcth ) then
               scr29(k) = scr29(k) + alpha * prc9(k)
            end if

            grid%xa%t(I,J,K)=grid%xa%t(I,J,K)-XLV1*DUM319(K)

            grid%xa%p(I,J,K)=grid%xa%p(I,J,K)+SCR69(K)/(gas_constant*grid%xb%t(I,J,K))
            grid%xa%t(I,J,K)=grid%xa%t(I,J,K)-grid%xb%p(I,J,K)/  &
                        (gas_constant*grid%xb%t(I,J,K)**2)*SCR69(K)
            grid%xa%q(I,J,K)=grid%xa%q(I,J,K)+SCR59(K)/SCR4(K)
            SCR49(K)=-grid%xb%q(I,J,K)/SCR4(K)**2*SCR59(K)

            if(grid%xb%qrn(I,J,K) > 1.0e-25) then
               grid%xa%qrn(I,J,K)=grid%xa%qrn(I,J,K)+SCR39(K)
            end if
            if(grid%xb%qcw(I,J,K) > 0.0) then
               grid%xa%qcw(I,J,K)=grid%xa%qcw(I,J,K)+SCR29(K)
            end if

            grid%xa%q(I,J,K)=grid%xa%q(I,J,K)+SCR49(K)/QVSS(K)
            QVSS9(K)=-grid%xb%q(I,J,K)/QVSS(K)**2*SCR49(K)
            TMP(K)=622.0/((grid%xb%p(I,J,K)-EES(K))**2)
            EES9(K)=TMP(K)*grid%xb%p(I,J,K)*QVSS9(K)
            grid%xa%p(I,J,K)=grid%xa%p(I,J,K)-TMP(K)*EES(K)*QVSS9(K)
            if( grid%xb%t(I,J,K) > TO )then
               grid%xa%t(I,J,K)=grid%xa%t(I,J,K)+EES(K)*SVP2*(SVPT0-SVP3)/ ((grid%xb%t(I,J,K)-SVP3)*(grid%xb%t(I,J,K)-SVP3))*EES9(K)
            else
               grid%xa%t(I,J,K)=grid%xa%t(I,J,K)+EES(K)*6.15E3/(grid%xb%t(I,J,K)* grid%xb%t(I,J,K))*EES9(K)
            end if

         end do
      end do
   end do

   grid%xa % qt (its:ite,jts:jte,kts:kte) = grid%xa % qt (its:ite,jts:jte,kts:kte) + grid%xa % q(its:ite,jts:jte,kts:kte)
   grid%xa % qcw(its:ite,jts:jte,kts:kte) = grid%xa % qcw(its:ite,jts:jte,kts:kte) - grid%xa % q(its:ite,jts:jte,kts:kte)
   grid%xa % qrn(its:ite,jts:jte,kts:kte) = grid%xa % qrn(its:ite,jts:jte,kts:kte) - grid%xa % q(its:ite,jts:jte,kts:kte)

   grid%xa % qrn (its:ite,jts:jte,kts:kte) = grid%xa % qrn (its:ite,jts:jte,kts:kte) + QRN_OLD(its:ite,jts:jte,kts:kte)
   grid%xa % qcw (its:ite,jts:jte,kts:kte) = grid%xa % qcw (its:ite,jts:jte,kts:kte) + QCW_OLD(its:ite,jts:jte,kts:kte)
   grid%xa % q   (its:ite,jts:jte,kts:kte) = grid%xa % q (its:ite,jts:jte,kts:kte)   + Q_OLD(its:ite,jts:jte,kts:kte)
   grid%xa % t   (its:ite,jts:jte,kts:kte) = grid%xa % t (its:ite,jts:jte,kts:kte)   + T_OLD(its:ite,jts:jte,kts:kte)

   if (trace_use) call da_trace_exit("da_moist_phys_adj")

end subroutine da_moist_phys_adj


