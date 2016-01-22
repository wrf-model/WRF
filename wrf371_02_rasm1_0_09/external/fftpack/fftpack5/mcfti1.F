!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: mcfti1.f,v 1.2 2004/06/15 21:29:19 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE MCFTI1 (N,WA,FNF,FAC) 
      REAL            WA(*),FAC(*) 
!                                                                       
      CALL FACTOR (N,NF,FAC) 
      FNF = NF 
      IW = 1 
      L1 = 1 
      DO 110 K1=1,NF 
         IP = FAC(K1) 
         L2 = L1*IP 
         IDO = N/L2 
         CALL TABLES (IDO,IP,WA(IW)) 
         IW = IW+(IP-1)*(IDO+IDO) 
         L1 = L2 
  110 END DO 
      RETURN 
      END                                           
