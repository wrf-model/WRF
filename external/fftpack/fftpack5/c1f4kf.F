!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: c1f4kf.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE C1F4KF (IDO,L1,NA,CC,IN1,CH,IN2,WA) 
      REAL CC(IN1,L1,IDO,4),CH(IN2,L1,4,IDO),WA(IDO,3,2) 
!                                                                       
! FFTPACK 5.0 auxiliary routine                                         
!                                                                       
      IF (IDO .GT. 1) GO TO 102 
      SN = 1./REAL(4*L1) 
      IF (NA .EQ. 1) GO TO 106 
      DO 101 K=1,L1 
         TI1 = CC(2,K,1,1)-CC(2,K,1,3) 
         TI2 = CC(2,K,1,1)+CC(2,K,1,3) 
         TR4 = CC(2,K,1,2)-CC(2,K,1,4) 
         TI3 = CC(2,K,1,2)+CC(2,K,1,4) 
         TR1 = CC(1,K,1,1)-CC(1,K,1,3) 
         TR2 = CC(1,K,1,1)+CC(1,K,1,3) 
         TI4 = CC(1,K,1,4)-CC(1,K,1,2) 
         TR3 = CC(1,K,1,2)+CC(1,K,1,4) 
         CC(1,K,1,1) = SN*(TR2+TR3) 
         CC(1,K,1,3) = SN*(TR2-TR3) 
         CC(2,K,1,1) = SN*(TI2+TI3) 
         CC(2,K,1,3) = SN*(TI2-TI3) 
         CC(1,K,1,2) = SN*(TR1+TR4) 
         CC(1,K,1,4) = SN*(TR1-TR4) 
         CC(2,K,1,2) = SN*(TI1+TI4) 
         CC(2,K,1,4) = SN*(TI1-TI4) 
  101 END DO 
      RETURN 
  106 DO 107 K=1,L1 
         TI1 = CC(2,K,1,1)-CC(2,K,1,3) 
         TI2 = CC(2,K,1,1)+CC(2,K,1,3) 
         TR4 = CC(2,K,1,2)-CC(2,K,1,4) 
         TI3 = CC(2,K,1,2)+CC(2,K,1,4) 
         TR1 = CC(1,K,1,1)-CC(1,K,1,3) 
         TR2 = CC(1,K,1,1)+CC(1,K,1,3) 
         TI4 = CC(1,K,1,4)-CC(1,K,1,2) 
         TR3 = CC(1,K,1,2)+CC(1,K,1,4) 
         CH(1,K,1,1) = SN*(TR2+TR3) 
         CH(1,K,3,1) = SN*(TR2-TR3) 
         CH(2,K,1,1) = SN*(TI2+TI3) 
         CH(2,K,3,1) = SN*(TI2-TI3) 
         CH(1,K,2,1) = SN*(TR1+TR4) 
         CH(1,K,4,1) = SN*(TR1-TR4) 
         CH(2,K,2,1) = SN*(TI1+TI4) 
         CH(2,K,4,1) = SN*(TI1-TI4) 
  107 END DO 
      RETURN 
  102 DO 103 K=1,L1 
         TI1 = CC(2,K,1,1)-CC(2,K,1,3) 
         TI2 = CC(2,K,1,1)+CC(2,K,1,3) 
         TR4 = CC(2,K,1,2)-CC(2,K,1,4) 
         TI3 = CC(2,K,1,2)+CC(2,K,1,4) 
         TR1 = CC(1,K,1,1)-CC(1,K,1,3) 
         TR2 = CC(1,K,1,1)+CC(1,K,1,3) 
         TI4 = CC(1,K,1,4)-CC(1,K,1,2) 
         TR3 = CC(1,K,1,2)+CC(1,K,1,4) 
         CH(1,K,1,1) = TR2+TR3 
         CH(1,K,3,1) = TR2-TR3 
         CH(2,K,1,1) = TI2+TI3 
         CH(2,K,3,1) = TI2-TI3 
         CH(1,K,2,1) = TR1+TR4 
         CH(1,K,4,1) = TR1-TR4 
         CH(2,K,2,1) = TI1+TI4 
         CH(2,K,4,1) = TI1-TI4 
  103 END DO 
      DO 105 I=2,IDO 
         DO 104 K=1,L1 
            TI1 = CC(2,K,I,1)-CC(2,K,I,3) 
            TI2 = CC(2,K,I,1)+CC(2,K,I,3) 
            TI3 = CC(2,K,I,2)+CC(2,K,I,4) 
            TR4 = CC(2,K,I,2)-CC(2,K,I,4) 
            TR1 = CC(1,K,I,1)-CC(1,K,I,3) 
            TR2 = CC(1,K,I,1)+CC(1,K,I,3) 
            TI4 = CC(1,K,I,4)-CC(1,K,I,2) 
            TR3 = CC(1,K,I,2)+CC(1,K,I,4) 
            CH(1,K,1,I) = TR2+TR3 
            CR3 = TR2-TR3 
            CH(2,K,1,I) = TI2+TI3 
            CI3 = TI2-TI3 
            CR2 = TR1+TR4 
            CR4 = TR1-TR4 
            CI2 = TI1+TI4 
            CI4 = TI1-TI4 
            CH(1,K,2,I) = WA(I,1,1)*CR2+WA(I,1,2)*CI2 
            CH(2,K,2,I) = WA(I,1,1)*CI2-WA(I,1,2)*CR2 
            CH(1,K,3,I) = WA(I,2,1)*CR3+WA(I,2,2)*CI3 
            CH(2,K,3,I) = WA(I,2,1)*CI3-WA(I,2,2)*CR3 
            CH(1,K,4,I) = WA(I,3,1)*CR4+WA(I,3,2)*CI4 
            CH(2,K,4,I) = WA(I,3,1)*CI4-WA(I,3,2)*CR4 
  104    CONTINUE 
  105 END DO 
      RETURN 
      END                                           
