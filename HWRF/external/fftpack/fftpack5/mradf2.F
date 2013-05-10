!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: mradf2.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE MRADF2 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1) 
      REAL       CH(IN2,IDO,2,L1) ,CC(IN1,IDO,L1,2) , WA1(IDO) 
!                                                                       
      M1D = (M-1)*IM1+1 
      M2S = 1-IM2 
      DO 101 K=1,L1 
         M2 = M2S 
         DO 1001 M1=1,M1D,IM1 
         M2 = M2+IM2 
         CH(M2,1,1,K) = CC(M1,1,K,1)+CC(M1,1,K,2) 
         CH(M2,IDO,2,K) = CC(M1,1,K,1)-CC(M1,1,K,2) 
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
            CH(M2,I,1,K) = CC(M1,I,K,1)+(WA1(I-2)*CC(M1,I,K,2)-         &
     &       WA1(I-1)*CC(M1,I-1,K,2))                                   
            CH(M2,IC,2,K) = (WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*            &
     &       CC(M1,I-1,K,2))-CC(M1,I,K,1)                               
            CH(M2,I-1,1,K) = CC(M1,I-1,K,1)+(WA1(I-2)*CC(M1,I-1,K,2)+   &
     &       WA1(I-1)*CC(M1,I,K,2))                                     
            CH(M2,IC-1,2,K) = CC(M1,I-1,K,1)-(WA1(I-2)*CC(M1,I-1,K,2)+  &
     &       WA1(I-1)*CC(M1,I,K,2))                                     
 1003       CONTINUE 
  103    CONTINUE 
  104 END DO 
      IF (MOD(IDO,2) .EQ. 1) RETURN 
  105 DO 106 K=1,L1 
         M2 = M2S 
         DO 1006 M1=1,M1D,IM1 
         M2 = M2+IM2 
         CH(M2,1,2,K) = -CC(M1,IDO,K,2) 
         CH(M2,IDO,1,K) = CC(M1,IDO,K,1) 
 1006    CONTINUE 
  106 END DO 
  107 RETURN 
      END                                           
