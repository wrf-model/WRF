!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: mradf4.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE MRADF4 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1,WA2,WA3) 
      REAL       CC(IN1,IDO,L1,4)   ,CH(IN2,IDO,4,L1)     ,             &
     &           WA1(IDO)           ,WA2(IDO)     ,WA3(IDO)             
!                                                                       
      HSQT2=SQRT(2.)/2. 
      M1D = (M-1)*IM1+1 
      M2S = 1-IM2 
      DO 101 K=1,L1 
         M2 = M2S 
         DO 1001 M1=1,M1D,IM1 
         M2 = M2+IM2 
         CH(M2,1,1,K) = (CC(M1,1,K,2)+CC(M1,1,K,4))                     &
     &      +(CC(M1,1,K,1)+CC(M1,1,K,3))                                
         CH(M2,IDO,4,K) = (CC(M1,1,K,1)+CC(M1,1,K,3))                   &
     &      -(CC(M1,1,K,2)+CC(M1,1,K,4))                                
         CH(M2,IDO,2,K) = CC(M1,1,K,1)-CC(M1,1,K,3) 
         CH(M2,1,3,K) = CC(M1,1,K,4)-CC(M1,1,K,2) 
 1001    CONTINUE 
  101 END DO 
      IF (IDO-2) 107,105,102 
  102 IDP2 = IDO+2 
      DO 104 K=1,L1 
         DO 103 I=3,IDO,2 
            IC = IDP2-I 
            M2 = M2S 
            DO 1003 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH(M2,I-1,1,K) = ((WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*        &
     &       CC(M1,I,K,2))+(WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*           &
     &       CC(M1,I,K,4)))+(CC(M1,I-1,K,1)+(WA2(I-2)*CC(M1,I-1,K,3)+   &
     &       WA2(I-1)*CC(M1,I,K,3)))                                    
            CH(M2,IC-1,4,K) = (CC(M1,I-1,K,1)+(WA2(I-2)*CC(M1,I-1,K,3)+ &
     &       WA2(I-1)*CC(M1,I,K,3)))-((WA1(I-2)*CC(M1,I-1,K,2)+         &
     &       WA1(I-1)*CC(M1,I,K,2))+(WA3(I-2)*CC(M1,I-1,K,4)+           &
     &       WA3(I-1)*CC(M1,I,K,4)))                                    
            CH(M2,I,1,K) = ((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*            &
     &       CC(M1,I-1,K,2))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*           &
     &       CC(M1,I-1,K,4)))+(CC(M1,I,K,1)+(WA2(I-2)*CC(M1,I,K,3)-     &
     &       WA2(I-1)*CC(M1,I-1,K,3)))                                  
            CH(M2,IC,4,K) = ((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*           &
     &       CC(M1,I-1,K,2))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*           &
     &       CC(M1,I-1,K,4)))-(CC(M1,I,K,1)+(WA2(I-2)*CC(M1,I,K,3)-     &
     &       WA2(I-1)*CC(M1,I-1,K,3)))                                  
            CH(M2,I-1,3,K) = ((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*          &
     &       CC(M1,I-1,K,2))-(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*           &
     &       CC(M1,I-1,K,4)))+(CC(M1,I-1,K,1)-(WA2(I-2)*CC(M1,I-1,K,3)+ &
     &       WA2(I-1)*CC(M1,I,K,3)))                                    
            CH(M2,IC-1,2,K) = (CC(M1,I-1,K,1)-(WA2(I-2)*CC(M1,I-1,K,3)+ &
     &       WA2(I-1)*CC(M1,I,K,3)))-((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*  &
     &       CC(M1,I-1,K,2))-(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*           &
     &       CC(M1,I-1,K,4)))                                           
            CH(M2,I,3,K) = ((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*          &
     &       CC(M1,I,K,4))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*           &
     &       CC(M1,I,K,2)))+(CC(M1,I,K,1)-(WA2(I-2)*CC(M1,I,K,3)-       &
     &       WA2(I-1)*CC(M1,I-1,K,3)))                                  
            CH(M2,IC,2,K) = ((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*         &
     &       CC(M1,I,K,4))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*           &
     &       CC(M1,I,K,2)))-(CC(M1,I,K,1)-(WA2(I-2)*CC(M1,I,K,3)-       &
     &       WA2(I-1)*CC(M1,I-1,K,3)))                                  
 1003       CONTINUE 
  103    CONTINUE 
  104 END DO 
      IF (MOD(IDO,2) .EQ. 1) RETURN 
  105 CONTINUE 
      DO 106 K=1,L1 
         M2 = M2S 
         DO 1006 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH(M2,IDO,1,K) = (HSQT2*(CC(M1,IDO,K,2)-CC(M1,IDO,K,4)))+   &
     &       CC(M1,IDO,K,1)                                             
            CH(M2,IDO,3,K) = CC(M1,IDO,K,1)-(HSQT2*(CC(M1,IDO,K,2)-     &
     &       CC(M1,IDO,K,4)))                                           
            CH(M2,1,2,K) = (-HSQT2*(CC(M1,IDO,K,2)+CC(M1,IDO,K,4)))-    &
     &       CC(M1,IDO,K,3)                                             
            CH(M2,1,4,K) = (-HSQT2*(CC(M1,IDO,K,2)+CC(M1,IDO,K,4)))+    &
     &       CC(M1,IDO,K,3)                                             
 1006    CONTINUE 
  106 END DO 
  107 RETURN 
      END                                           
