subroutine da_condens_adj(DT,SCR31,SCR42,SCR71,DUM31,PRD,  &
                   QVT,QCT,QRT,TTT,P_B,T_B,QV_B,QCW_B,QRN_B,  &
                   SCR319,SCR429,SCR719,DUM319,PRD9,  &
                   QVT9,QCT9,QRT9,TTT9,P_A,T_A,QV_A,QCW_A,QRN_A,kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: Condensation
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                     :: kts, kte
   real, dimension(kts:kte), intent(in)    :: DT,SCR31,SCR42,SCR71,PRD,DUM31
   real, dimension(kts:kte), intent(in)    :: P_B,T_B,QV_B,QCW_B,QRN_B
   real, dimension(kts:kte), intent(inout) :: SCR319,SCR429,SCR719,PRD9
   real, dimension(kts:kte), intent(inout) :: P_A,T_A,QV_A,QCW_A,QRN_A,DUM319

   real, dimension(kts:kte), intent(in)    :: QVT,QCT,QRT,TTT
   real, dimension(kts:kte), intent(inout) :: QRT9,QCT9,QVT9,TTT9


   real, dimension(kts:kte)  :: DUM2139
   real, dimension(kts:kte)  :: TMP,DUM114,DUM2129,SCR89,DUM212,DUM115
   real, dimension(kts:kte)  :: PRC5,PRC59,DUM1149,SCR61,SCR8,DUM213
   real, dimension(kts:kte)  :: SCR619
   integer                   :: k

   !  initilization

   do K=kts,kte
      DUM2129(K) = 0.0
      SCR89 (K) = 0.0
      PRC59 (K) = 0.0
   end do

   do K=kts, kte

   if (DT(k) <= 0.0) cycle

      DUM114(K)=1.0e3*SVP1*EXP(SVP2*(SCR71(K)-SVPT0)/(SCR71(K)-SVP3))

      if(SCR71(K) > TO) then
         DUM212(K)=DUM31(K)*DUM31(K)/(gas_constant_v*PRD(K))
      else
         DUM212(K)=XLS*DUM31(K)/(gas_constant_v*PRD(K))
      end if
      PRC5(K)=.622*DUM114(K)/(P_B(K)-DUM114(K))

      if(SCR42(K) < PRC5(K) .AND. SCR71(K) < TO) then
         SCR61(K)=0.0
      else
         SCR8(K)=(SCR42(K)-PRC5(K))/(1.0+DUM212(K)*PRC5(K)/  &
                 (SCR71(K)*SCR71(K)))

         DUM115(K)=SCR31(K)+SCR8(K)
         if (DUM115(K) >= 0.0)then
            SCR61(K)=SCR8(K)/DT(k)
         else
            SCR61(K)=-SCR31(K)/DT(k)
         end if
      end if
      if(SCR71(K) > TO)then
         DUM213(K)=DUM31(K)/PRD(K)
      else
         DUM213(K)=XLS/PRD(K)
      end if

      TTT9(K)=DT(K)*T_A(K)
      SCR619(K)=DT(K)*DUM213(K)*T_A(K)
      DUM2139(K)=DT(K)*SCR61(K)*T_A(K)
      if(QRN_B(K) < 1.0e-25) QRN_A(K)=0.0
      QRT9(K)=DT(K)*QRN_A(K)

      DUM319(K)=0.0
      if(SCR71(K) > TO)then
         DUM319(K)=DUM2139(K)/PRD(K)
         PRD9(K)=-DUM31(K)/(PRD(K)*PRD(K))*DUM2139(K)
      else
         PRD9(K)=-XLS/(PRD(K)*PRD(K))*DUM2139(K)
      end if
      if(QCW_B(K) < 1.0e-25) QCW_A(K)=0.0
      QCT9(K)=DT(K)*QCW_A(K)
      SCR619(K)=SCR619(K)+DT(K)*QCW_A(K)
      if(QV_B(K) < 1.0e-25) QV_A(K)=0.0
      QVT9(K)=DT(K)*QV_A(K)
      SCR619(K)=SCR619(K)-DT(K)*QV_A(K)

      SCR319(K)=0.0
      SCR429(K)=0.0
      SCR719(K)=0.0
      if(SCR42(K) >= PRC5(K) .OR. SCR71(K) >= TO) then
         if(DUM115(K) >= 0.0)then
            SCR89(K)=SCR89(K)+SCR619(K)/DT(k)
         else
            SCR319(K)=-SCR619(K)/DT(k)
         end if

         TMP(K)=1.0/(1.0+DUM212(K)*PRC5(K)/(SCR71(K)*SCR71(K)))
         SCR719(K)=TMP(K)*TMP(K)*2.0*DUM212(K)*PRC5(K)  &
                   *(SCR42(K)-PRC5(K))/(SCR71(K)*SCR71(K)*SCR71(K))*SCR89(K)
         DUM2129(K)=DUM2129(K)-TMP(K)*TMP(K)*(SCR42(K)-PRC5(K))*PRC5(K)/  &
                     (SCR71(K)*SCR71(K))*SCR89(K)
         PRC59(K)=PRC59(K)-TMP(K)*(1.0+(SCR42(K)-PRC5(K))*DUM212(K)/  &
                     (SCR71(K)*SCR71(K))*TMP(K))*SCR89(K)
         SCR429(K)=TMP(K)*SCR89(K)
      end if

      TMP(K)=.622/(P_B(K)-DUM114(K))**2
      DUM1149(K)=TMP(K)*P_B(K)*PRC59(K)
      P_A(K)=P_A(K)-TMP(K)*DUM114(K)*PRC59(K)
      if(SCR71(K) > TO) then
         PRD9(K)=PRD9(K)-DUM31(K)*DUM31(K)/   &
                 (gas_constant_v*PRD(K)*PRD(K))*DUM2129(K)
         DUM319(K)=DUM319(K)+2.0*DUM31(K)/(gas_constant_v*PRD(K))*DUM2129(K)
      else
         PRD9(K)=PRD9(K)-XLS*DUM31(K)/(gas_constant_v*PRD(K)*PRD(K))*DUM2129(K)
         DUM319(K)=DUM319(K)+XLS/(gas_constant_v*PRD(K))*DUM2129(K)
      end if
      DUM114(K)=1.0e3*SVP1*EXP(SVP2*(SCR71(K)-SVPT0)/(SCR71(K)-SVP3))
      SCR719(K)=SCR719(K)+DUM114(K)*SVP2*(SVPT0-SVP3)/  &
                (SCR71(K)-SVP3)**2*DUM1149(K)

   end do

end subroutine da_condens_adj
