!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cmf4kb.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CMF4KB (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA) 
      REAL CC(2,IN1,L1,IDO,4),CH(2,IN2,L1,4,IDO),WA(IDO,3,2) 
!                                                                       
! FFTPACK 5.0 auxiliary routine                                         
!                                                                       
      M1D = (LOT-1)*IM1+1 
      M2S = 1-IM2 
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102 
      DO 101 K=1,L1 
         DO 101 M1=1,M1D,IM1 
         TI1 = CC(2,M1,K,1,1)-CC(2,M1,K,1,3) 
         TI2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,3) 
         TR4 = CC(2,M1,K,1,4)-CC(2,M1,K,1,2) 
         TI3 = CC(2,M1,K,1,2)+CC(2,M1,K,1,4) 
         TR1 = CC(1,M1,K,1,1)-CC(1,M1,K,1,3) 
         TR2 = CC(1,M1,K,1,1)+CC(1,M1,K,1,3) 
         TI4 = CC(1,M1,K,1,2)-CC(1,M1,K,1,4) 
         TR3 = CC(1,M1,K,1,2)+CC(1,M1,K,1,4) 
         CC(1,M1,K,1,1) = TR2+TR3 
         CC(1,M1,K,1,3) = TR2-TR3 
         CC(2,M1,K,1,1) = TI2+TI3 
         CC(2,M1,K,1,3) = TI2-TI3 
         CC(1,M1,K,1,2) = TR1+TR4 
         CC(1,M1,K,1,4) = TR1-TR4 
         CC(2,M1,K,1,2) = TI1+TI4 
         CC(2,M1,K,1,4) = TI1-TI4 
  101 CONTINUE 
      RETURN 
  102 DO 103 K=1,L1 
         M2 = M2S 
         DO 103 M1=1,M1D,IM1 
         M2 = M2+IM2 
         TI1 = CC(2,M1,K,1,1)-CC(2,M1,K,1,3) 
         TI2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,3) 
         TR4 = CC(2,M1,K,1,4)-CC(2,M1,K,1,2) 
         TI3 = CC(2,M1,K,1,2)+CC(2,M1,K,1,4) 
         TR1 = CC(1,M1,K,1,1)-CC(1,M1,K,1,3) 
         TR2 = CC(1,M1,K,1,1)+CC(1,M1,K,1,3) 
         TI4 = CC(1,M1,K,1,2)-CC(1,M1,K,1,4) 
         TR3 = CC(1,M1,K,1,2)+CC(1,M1,K,1,4) 
         CH(1,M2,K,1,1) = TR2+TR3 
         CH(1,M2,K,3,1) = TR2-TR3 
         CH(2,M2,K,1,1) = TI2+TI3 
         CH(2,M2,K,3,1) = TI2-TI3 
         CH(1,M2,K,2,1) = TR1+TR4 
         CH(1,M2,K,4,1) = TR1-TR4 
         CH(2,M2,K,2,1) = TI1+TI4 
         CH(2,M2,K,4,1) = TI1-TI4 
  103 CONTINUE 
      IF(IDO .EQ. 1) RETURN 
      DO 105 I=2,IDO 
         DO 104 K=1,L1 
         M2 = M2S 
         DO 104 M1=1,M1D,IM1 
         M2 = M2+IM2 
            TI1 = CC(2,M1,K,I,1)-CC(2,M1,K,I,3) 
            TI2 = CC(2,M1,K,I,1)+CC(2,M1,K,I,3) 
            TI3 = CC(2,M1,K,I,2)+CC(2,M1,K,I,4) 
            TR4 = CC(2,M1,K,I,4)-CC(2,M1,K,I,2) 
            TR1 = CC(1,M1,K,I,1)-CC(1,M1,K,I,3) 
            TR2 = CC(1,M1,K,I,1)+CC(1,M1,K,I,3) 
            TI4 = CC(1,M1,K,I,2)-CC(1,M1,K,I,4) 
            TR3 = CC(1,M1,K,I,2)+CC(1,M1,K,I,4) 
            CH(1,M2,K,1,I) = TR2+TR3 
            CR3 = TR2-TR3 
            CH(2,M2,K,1,I) = TI2+TI3 
            CI3 = TI2-TI3 
            CR2 = TR1+TR4 
            CR4 = TR1-TR4 
            CI2 = TI1+TI4 
            CI4 = TI1-TI4 
            CH(1,M2,K,2,I) = WA(I,1,1)*CR2-WA(I,1,2)*CI2 
            CH(2,M2,K,2,I) = WA(I,1,1)*CI2+WA(I,1,2)*CR2 
            CH(1,M2,K,3,I) = WA(I,2,1)*CR3-WA(I,2,2)*CI3 
            CH(2,M2,K,3,I) = WA(I,2,1)*CI3+WA(I,2,2)*CR3 
            CH(1,M2,K,4,I) = WA(I,3,1)*CR4-WA(I,3,2)*CI4 
            CH(2,M2,K,4,I) = WA(I,3,1)*CI4+WA(I,3,2)*CR4 
  104    CONTINUE 
  105 END DO 
      RETURN 
      END                                           
