!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cmfm1b.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CMFM1B (LOT,JUMP,N,INC,C,CH,WA,FNF,FAC) 
      COMPLEX       C(*) 
      REAL       CH(*),     WA(*),     FAC(*) 
!                                                                       
! FFTPACK 5.0 auxiliary routine                                         
!                                                                       
      NF = FNF 
      NA = 0 
      L1 = 1 
      IW = 1 
      DO 125 K1=1,NF 
         IP = FAC(K1) 
         L2 = IP*L1 
         IDO = N/L2 
         LID = L1*IDO 
         NBR = 1+NA+2*MIN(IP-2,4) 
         GO TO (52,62,53,63,54,64,55,65,56,66),NBR 
   52    CALL CMF2KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW)) 
         GO TO 120 
   62    CALL CMF2KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW)) 
         GO TO 120 
   53    CALL CMF3KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW)) 
         GO TO 120 
   63    CALL CMF3KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW)) 
         GO TO 120 
   54    CALL CMF4KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW)) 
         GO TO 120 
   64    CALL CMF4KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW)) 
         GO TO 120 
   55    CALL CMF5KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW)) 
         GO TO 120 
   65    CALL CMF5KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW)) 
         GO TO 120 
   56    CALL CMFGKB (LOT,IDO,IP,L1,LID,NA,C,C,JUMP,INC,CH,CH,1,        &
     &     LOT,WA(IW))                                                  
         GO TO 120 
   66    CALL CMFGKB (LOT,IDO,IP,L1,LID,NA,CH,CH,1,LOT,C,C,             &
     &     JUMP,INC,WA(IW))                                             
  120    L1 = L2 
         IW = IW+(IP-1)*(IDO+IDO) 
         IF(IP .LE. 5) NA = 1-NA 
  125 END DO 
      RETURN 
      END                                           
