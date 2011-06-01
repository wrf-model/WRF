!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: msntb1.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE MSNTB1(LOT,JUMP,N,INC,X,WSAVE,DSUM,XH,WORK,IER) 
      REAL       X(INC,*)       ,WSAVE(*)   ,XH(LOT,*) 
      DOUBLE PRECISION           DSUM(*) 
      IER = 0 
      LJ = (LOT-1)*JUMP+1 
      IF (N-2) 200,102,103 
  102 SRT3S2 = SQRT(3.)/2. 
      DO 112 M=1,LJ,JUMP 
         XHOLD = SRT3S2*(X(M,1)+X(M,2)) 
         X(M,2) = SRT3S2*(X(M,1)-X(M,2)) 
         X(M,1) = XHOLD 
  112 END DO 
      GO TO 200 
  103 NP1 = N+1 
      NS2 = N/2 
      DO 104 K=1,NS2 
         KC = NP1-K 
         M1 = 0 
         DO 114 M=1,LJ,JUMP 
         M1 = M1+1 
         T1 = X(M,K)-X(M,KC) 
         T2 = WSAVE(K)*(X(M,K)+X(M,KC)) 
         XH(M1,K+1) = T1+T2 
         XH(M1,KC+1) = T2-T1 
  114    CONTINUE 
  104 END DO 
      MODN = MOD(N,2) 
      IF (MODN .EQ. 0) GO TO 124 
      M1 = 0 
      DO 123 M=1,LJ,JUMP 
         M1 = M1+1 
         XH(M1,NS2+2) = 4.*X(M,NS2+1) 
  123 END DO 
  124 DO 127 M=1,LOT 
         XH(M,1) = 0. 
  127 END DO 
      LNXH = LOT-1 + LOT*(NP1-1) + 1 
      LNSV = NP1 + INT(LOG(REAL(NP1))) + 4 
      LNWK = LOT*NP1 
!                                                                       
      CALL RFFTMF(LOT,1,NP1,LOT,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,         &
     &            LNWK,IER1)                                            
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('MSNTB1',-5) 
        GO TO 200 
      ENDIF 
!                                                                       
      IF(MOD(NP1,2) .NE. 0) GO TO 30 
      DO 20 M=1,LOT 
      XH(M,NP1) = XH(M,NP1)+XH(M,NP1) 
   20 END DO 
   30 FNP1S4 = FLOAT(NP1)/4. 
      M1 = 0 
      DO 125 M=1,LJ,JUMP 
         M1 = M1+1 
         X(M,1) = FNP1S4*XH(M1,1) 
         DSUM(M1) = X(M,1) 
  125 END DO 
      DO 105 I=3,N,2 
         M1 = 0 
         DO 115 M=1,LJ,JUMP 
            M1 = M1+1 
            X(M,I-1) = FNP1S4*XH(M1,I) 
            DSUM(M1) = DSUM(M1)+FNP1S4*XH(M1,I-1) 
            X(M,I) = DSUM(M1) 
  115    CONTINUE 
  105 END DO 
      IF (MODN .NE. 0) GO TO 200 
      M1 = 0 
      DO 116 M=1,LJ,JUMP 
         M1 = M1+1 
         X(M,N) = FNP1S4*XH(M1,N+1) 
  116 END DO 
!                                                                       
  200 CONTINUE 
      RETURN 
      END                                           
