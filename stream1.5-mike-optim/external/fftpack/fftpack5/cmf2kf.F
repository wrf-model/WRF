!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cmf2kf.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CMF2KF (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA) 
      REAL  CC(2,IN1,L1,IDO,2),CH(2,IN2,L1,2,IDO),WA(IDO,1,2) 
!                                                                       
      M1D = (LOT-1)*IM1+1 
      M2S = 1-IM2 
      IF (IDO .GT. 1) GO TO 102 
      SN = 1./REAL(2*L1) 
      IF (NA .EQ. 1) GO TO 106 
      DO 101 K=1,L1 
         DO 101 M1=1,M1D,IM1 
         CHOLD1 = SN*(CC(1,M1,K,1,1)+CC(1,M1,K,1,2)) 
         CC(1,M1,K,1,2) = SN*(CC(1,M1,K,1,1)-CC(1,M1,K,1,2)) 
         CC(1,M1,K,1,1) = CHOLD1 
         CHOLD2 = SN*(CC(2,M1,K,1,1)+CC(2,M1,K,1,2)) 
         CC(2,M1,K,1,2) = SN*(CC(2,M1,K,1,1)-CC(2,M1,K,1,2)) 
         CC(2,M1,K,1,1) = CHOLD2 
  101 CONTINUE 
      RETURN 
  106 DO 107 K=1,L1 
         M2 = M2S 
         DO 107 M1=1,M1D,IM1 
         M2 = M2+IM2 
         CH(1,M2,K,1,1) = SN*(CC(1,M1,K,1,1)+CC(1,M1,K,1,2)) 
         CH(1,M2,K,2,1) = SN*(CC(1,M1,K,1,1)-CC(1,M1,K,1,2)) 
         CH(2,M2,K,1,1) = SN*(CC(2,M1,K,1,1)+CC(2,M1,K,1,2)) 
         CH(2,M2,K,2,1) = SN*(CC(2,M1,K,1,1)-CC(2,M1,K,1,2)) 
  107 CONTINUE 
      RETURN 
  102 DO 103 K=1,L1 
         M2 = M2S 
         DO 103 M1=1,M1D,IM1 
         M2 = M2+IM2 
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+CC(1,M1,K,1,2) 
         CH(1,M2,K,2,1) = CC(1,M1,K,1,1)-CC(1,M1,K,1,2) 
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+CC(2,M1,K,1,2) 
         CH(2,M2,K,2,1) = CC(2,M1,K,1,1)-CC(2,M1,K,1,2) 
  103 CONTINUE 
      DO 105 I=2,IDO 
         DO 104 K=1,L1 
         M2 = M2S 
         DO 104 M1=1,M1D,IM1 
         M2 = M2+IM2 
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+CC(1,M1,K,I,2) 
            TR2 = CC(1,M1,K,I,1)-CC(1,M1,K,I,2) 
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+CC(2,M1,K,I,2) 
            TI2 = CC(2,M1,K,I,1)-CC(2,M1,K,I,2) 
            CH(2,M2,K,2,I) = WA(I,1,1)*TI2-WA(I,1,2)*TR2 
            CH(1,M2,K,2,I) = WA(I,1,1)*TR2+WA(I,1,2)*TI2 
  104    CONTINUE 
  105 END DO 
      RETURN 
      END                                           
