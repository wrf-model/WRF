!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cmf3kb.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CMF3KB (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA) 
      REAL  CC(2,IN1,L1,IDO,3),CH(2,IN2,L1,3,IDO),WA(IDO,2,2) 
      DATA TAUR,TAUI /-.5,.866025403784439/ 
!                                                                       
      M1D = (LOT-1)*IM1+1 
      M2S = 1-IM2 
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102 
      DO 101 K=1,L1 
         DO 101 M1=1,M1D,IM1 
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,3) 
         CR2 = CC(1,M1,K,1,1)+TAUR*TR2 
         CC(1,M1,K,1,1) = CC(1,M1,K,1,1)+TR2 
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,3) 
         CI2 = CC(2,M1,K,1,1)+TAUR*TI2 
         CC(2,M1,K,1,1) = CC(2,M1,K,1,1)+TI2 
         CR3 = TAUI*(CC(1,M1,K,1,2)-CC(1,M1,K,1,3)) 
         CI3 = TAUI*(CC(2,M1,K,1,2)-CC(2,M1,K,1,3)) 
         CC(1,M1,K,1,2) = CR2-CI3 
         CC(1,M1,K,1,3) = CR2+CI3 
         CC(2,M1,K,1,2) = CI2+CR3 
         CC(2,M1,K,1,3) = CI2-CR3 
  101 CONTINUE 
      RETURN 
  102 DO 103 K=1,L1 
         M2 = M2S 
         DO 103 M1=1,M1D,IM1 
         M2 = M2+IM2 
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,3) 
         CR2 = CC(1,M1,K,1,1)+TAUR*TR2 
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+TR2 
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,3) 
         CI2 = CC(2,M1,K,1,1)+TAUR*TI2 
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+TI2 
         CR3 = TAUI*(CC(1,M1,K,1,2)-CC(1,M1,K,1,3)) 
         CI3 = TAUI*(CC(2,M1,K,1,2)-CC(2,M1,K,1,3)) 
         CH(1,M2,K,2,1) = CR2-CI3 
         CH(1,M2,K,3,1) = CR2+CI3 
         CH(2,M2,K,2,1) = CI2+CR3 
         CH(2,M2,K,3,1) = CI2-CR3 
  103 CONTINUE 
      IF (IDO .EQ. 1) RETURN 
      DO 105 I=2,IDO 
        DO 104 K=1,L1 
         M2 = M2S 
         DO 104 M1=1,M1D,IM1 
         M2 = M2+IM2 
            TR2 = CC(1,M1,K,I,2)+CC(1,M1,K,I,3) 
            CR2 = CC(1,M1,K,I,1)+TAUR*TR2 
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+TR2 
            TI2 = CC(2,M1,K,I,2)+CC(2,M1,K,I,3) 
            CI2 = CC(2,M1,K,I,1)+TAUR*TI2 
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+TI2 
            CR3 = TAUI*(CC(1,M1,K,I,2)-CC(1,M1,K,I,3)) 
            CI3 = TAUI*(CC(2,M1,K,I,2)-CC(2,M1,K,I,3)) 
            DR2 = CR2-CI3 
            DR3 = CR2+CI3 
            DI2 = CI2+CR3 
            DI3 = CI2-CR3 
            CH(2,M2,K,2,I) = WA(I,1,1)*DI2+WA(I,1,2)*DR2 
            CH(1,M2,K,2,I) = WA(I,1,1)*DR2-WA(I,1,2)*DI2 
            CH(2,M2,K,3,I) = WA(I,2,1)*DI3+WA(I,2,2)*DR3 
            CH(1,M2,K,3,I) = WA(I,2,1)*DR3-WA(I,2,2)*DI3 
  104    CONTINUE 
  105 END DO 
      RETURN 
      END                                           
