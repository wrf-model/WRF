!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: r1f3kf.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE R1F3KF (IDO,L1,CC,IN1,CH,IN2,WA1,WA2) 
      REAL       CH(IN2,IDO,3,L1)  ,CC(IN1,IDO,L1,3)     ,              &
     &                WA1(IDO)     ,WA2(IDO)                            
!                                                                       
      ARG=2.*4.*ATAN(1.0)/3. 
      TAUR=COS(ARG) 
      TAUI=SIN(ARG) 
      DO 101 K=1,L1 
         CH(1,1,1,K) = CC(1,1,K,1)+(CC(1,1,K,2)+CC(1,1,K,3)) 
         CH(1,1,3,K) = TAUI*(CC(1,1,K,3)-CC(1,1,K,2)) 
         CH(1,IDO,2,K) = CC(1,1,K,1)+TAUR*                              &
     &      (CC(1,1,K,2)+CC(1,1,K,3))                                   
  101 END DO 
      IF (IDO .EQ. 1) RETURN 
      IDP2 = IDO+2 
      DO 103 K=1,L1 
         DO 102 I=3,IDO,2 
            IC = IDP2-I 
            CH(1,I-1,1,K) = CC(1,I-1,K,1)+((WA1(I-2)*CC(1,I-1,K,2)+     &
     &       WA1(I-1)*CC(1,I,K,2))+(WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*    &
     &       CC(1,I,K,3)))                                              
            CH(1,I,1,K) = CC(1,I,K,1)+((WA1(I-2)*CC(1,I,K,2)-           &
     &       WA1(I-1)*CC(1,I-1,K,2))+(WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*    &
     &       CC(1,I-1,K,3)))                                            
            CH(1,I-1,3,K) = (CC(1,I-1,K,1)+TAUR*((WA1(I-2)*             &
     &       CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2))+(WA2(I-2)*             &
     &       CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3))))+(TAUI*((WA1(I-2)*    &
     &       CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2))-(WA2(I-2)*             &
     &       CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3))))                      
            CH(1,IC-1,2,K) = (CC(1,I-1,K,1)+TAUR*((WA1(I-2)*            &
     &       CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2))+(WA2(I-2)*             &
     &       CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3))))-(TAUI*((WA1(I-2)*    &
     &       CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2))-(WA2(I-2)*             &
     &       CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3))))                      
            CH(1,I,3,K) = (CC(1,I,K,1)+TAUR*((WA1(I-2)*CC(1,I,K,2)-     &
     &       WA1(I-1)*CC(1,I-1,K,2))+(WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*    &
     &       CC(1,I-1,K,3))))+(TAUI*((WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*  &
     &       CC(1,I,K,3))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*             &
     &       CC(1,I,K,2))))                                             
            CH(1,IC,2,K) = (TAUI*((WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*     &
     &       CC(1,I,K,3))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*             &
     &       CC(1,I,K,2))))-(CC(1,I,K,1)+TAUR*((WA1(I-2)*CC(1,I,K,2)-   &
     &       WA1(I-1)*CC(1,I-1,K,2))+(WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*    &
     &       CC(1,I-1,K,3))))                                           
  102    CONTINUE 
  103 END DO 
      RETURN 
      END                                           
