!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!      File:           cmf5kf.f                                         
!                                                                       
!      Library:        FFTPACK 5.0                                      
!                                                                       
!      Authors:        Paul N. Swarztrauber and Richard A. Valent       
!                      National Center for Atmospheric Research         
!                      PO 3000, Boulder, Colorado                       
!                                                                       
!      Date:           Wed Mar 29 18:31:13 MST 1995                     
!                                                                       
!      Description:    Lower-level auxiliary routine                    
!                                                                       
      SUBROUTINE CMF5KF (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA) 
      REAL  CC(2,IN1,L1,IDO,5),CH(2,IN2,L1,5,IDO),WA(IDO,4,2) 
      DATA TR11,TI11,TR12,TI12 /.3090169943749474,-.9510565162951536,   &
     &-.8090169943749474,-.5877852522924731/                            
!                                                                       
! FFTPACK 5.0 auxiliary routine                                         
!                                                                       
      M1D = (LOT-1)*IM1+1 
      M2S = 1-IM2 
      IF (IDO .GT. 1) GO TO 102 
      SN = 1./REAL(5*L1) 
      IF (NA .EQ. 1) GO TO 106 
      DO 101 K=1,L1 
         DO 101 M1=1,M1D,IM1 
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5) 
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5) 
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4) 
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4) 
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5) 
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5) 
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4) 
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4) 
         CHOLD1 = SN*(CC(1,M1,K,1,1)+TR2+TR3) 
         CHOLD2 = SN*(CC(2,M1,K,1,1)+TI2+TI3) 
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3 
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3 
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3 
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3 
         CC(1,M1,K,1,1) = CHOLD1 
         CC(2,M1,K,1,1) = CHOLD2 
         CR5 = TI11*TR5+TI12*TR4 
         CI5 = TI11*TI5+TI12*TI4 
         CR4 = TI12*TR5-TI11*TR4 
         CI4 = TI12*TI5-TI11*TI4 
         CC(1,M1,K,1,2) = SN*(CR2-CI5) 
         CC(1,M1,K,1,5) = SN*(CR2+CI5) 
         CC(2,M1,K,1,2) = SN*(CI2+CR5) 
         CC(2,M1,K,1,3) = SN*(CI3+CR4) 
         CC(1,M1,K,1,3) = SN*(CR3-CI4) 
         CC(1,M1,K,1,4) = SN*(CR3+CI4) 
         CC(2,M1,K,1,4) = SN*(CI3-CR4) 
         CC(2,M1,K,1,5) = SN*(CI2-CR5) 
  101 CONTINUE 
      RETURN 
  106 DO 107 K=1,L1 
         M2 = M2S 
         DO 107 M1=1,M1D,IM1 
         M2 = M2+IM2 
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5) 
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5) 
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4) 
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4) 
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5) 
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5) 
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4) 
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4) 
         CH(1,M2,K,1,1) = SN*(CC(1,M1,K,1,1)+TR2+TR3) 
         CH(2,M2,K,1,1) = SN*(CC(2,M1,K,1,1)+TI2+TI3) 
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3 
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3 
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3 
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3 
         CR5 = TI11*TR5+TI12*TR4 
         CI5 = TI11*TI5+TI12*TI4 
         CR4 = TI12*TR5-TI11*TR4 
         CI4 = TI12*TI5-TI11*TI4 
         CH(1,M2,K,2,1) = SN*(CR2-CI5) 
         CH(1,M2,K,5,1) = SN*(CR2+CI5) 
         CH(2,M2,K,2,1) = SN*(CI2+CR5) 
         CH(2,M2,K,3,1) = SN*(CI3+CR4) 
         CH(1,M2,K,3,1) = SN*(CR3-CI4) 
         CH(1,M2,K,4,1) = SN*(CR3+CI4) 
         CH(2,M2,K,4,1) = SN*(CI3-CR4) 
         CH(2,M2,K,5,1) = SN*(CI2-CR5) 
  107 CONTINUE 
      RETURN 
  102 DO 103 K=1,L1 
         M2 = M2S 
         DO 103 M1=1,M1D,IM1 
         M2 = M2+IM2 
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5) 
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5) 
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4) 
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4) 
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5) 
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5) 
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4) 
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4) 
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+TR2+TR3 
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+TI2+TI3 
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3 
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3 
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3 
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3 
         CR5 = TI11*TR5+TI12*TR4 
         CI5 = TI11*TI5+TI12*TI4 
         CR4 = TI12*TR5-TI11*TR4 
         CI4 = TI12*TI5-TI11*TI4 
         CH(1,M2,K,2,1) = CR2-CI5 
         CH(1,M2,K,5,1) = CR2+CI5 
         CH(2,M2,K,2,1) = CI2+CR5 
         CH(2,M2,K,3,1) = CI3+CR4 
         CH(1,M2,K,3,1) = CR3-CI4 
         CH(1,M2,K,4,1) = CR3+CI4 
         CH(2,M2,K,4,1) = CI3-CR4 
         CH(2,M2,K,5,1) = CI2-CR5 
  103 CONTINUE 
      DO 105 I=2,IDO 
         DO 104 K=1,L1 
         M2 = M2S 
         DO 104 M1=1,M1D,IM1 
         M2 = M2+IM2 
            TI5 = CC(2,M1,K,I,2)-CC(2,M1,K,I,5) 
            TI2 = CC(2,M1,K,I,2)+CC(2,M1,K,I,5) 
            TI4 = CC(2,M1,K,I,3)-CC(2,M1,K,I,4) 
            TI3 = CC(2,M1,K,I,3)+CC(2,M1,K,I,4) 
            TR5 = CC(1,M1,K,I,2)-CC(1,M1,K,I,5) 
            TR2 = CC(1,M1,K,I,2)+CC(1,M1,K,I,5) 
            TR4 = CC(1,M1,K,I,3)-CC(1,M1,K,I,4) 
            TR3 = CC(1,M1,K,I,3)+CC(1,M1,K,I,4) 
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+TR2+TR3 
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+TI2+TI3 
            CR2 = CC(1,M1,K,I,1)+TR11*TR2+TR12*TR3 
            CI2 = CC(2,M1,K,I,1)+TR11*TI2+TR12*TI3 
            CR3 = CC(1,M1,K,I,1)+TR12*TR2+TR11*TR3 
            CI3 = CC(2,M1,K,I,1)+TR12*TI2+TR11*TI3 
            CR5 = TI11*TR5+TI12*TR4 
            CI5 = TI11*TI5+TI12*TI4 
            CR4 = TI12*TR5-TI11*TR4 
            CI4 = TI12*TI5-TI11*TI4 
            DR3 = CR3-CI4 
            DR4 = CR3+CI4 
            DI3 = CI3+CR4 
            DI4 = CI3-CR4 
            DR5 = CR2+CI5 
            DR2 = CR2-CI5 
            DI5 = CI2-CR5 
            DI2 = CI2+CR5 
            CH(1,M2,K,2,I) = WA(I,1,1)*DR2+WA(I,1,2)*DI2 
            CH(2,M2,K,2,I) = WA(I,1,1)*DI2-WA(I,1,2)*DR2 
            CH(1,M2,K,3,I) = WA(I,2,1)*DR3+WA(I,2,2)*DI3 
            CH(2,M2,K,3,I) = WA(I,2,1)*DI3-WA(I,2,2)*DR3 
            CH(1,M2,K,4,I) = WA(I,3,1)*DR4+WA(I,3,2)*DI4 
            CH(2,M2,K,4,I) = WA(I,3,1)*DI4-WA(I,3,2)*DR4 
            CH(1,M2,K,5,I) = WA(I,4,1)*DR5+WA(I,4,2)*DI5 
            CH(2,M2,K,5,I) = WA(I,4,1)*DI5-WA(I,4,2)*DR5 
  104    CONTINUE 
  105 END DO 
      RETURN 
      END                                           
