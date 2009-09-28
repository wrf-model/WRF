!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: rfftf1.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE RFFTF1 (N,IN,C,CH,WA,FAC) 
      REAL       CH(*) ,C(IN,*)  ,WA(N)   ,FAC(15) 
!                                                                       
      NF = FAC(2) 
      NA = 1 
      L2 = N 
      IW = N 
      DO 111 K1=1,NF 
         KH = NF-K1 
         IP = FAC(KH+3) 
         L1 = L2/IP 
         IDO = N/L2 
         IDL1 = IDO*L1 
         IW = IW-(IP-1)*IDO 
         NA = 1-NA 
         IF (IP .NE. 4) GO TO 102 
         IX2 = IW+IDO 
         IX3 = IX2+IDO 
         IF (NA .NE. 0) GO TO 101 
         CALL R1F4KF (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),WA(IX3)) 
         GO TO 110 
  101    CALL R1F4KF (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),WA(IX3)) 
         GO TO 110 
  102    IF (IP .NE. 2) GO TO 104 
         IF (NA .NE. 0) GO TO 103 
         CALL R1F2KF (IDO,L1,C,IN,CH,1,WA(IW)) 
         GO TO 110 
  103    CALL R1F2KF (IDO,L1,CH,1,C,IN,WA(IW)) 
         GO TO 110 
  104    IF (IP .NE. 3) GO TO 106 
         IX2 = IW+IDO 
         IF (NA .NE. 0) GO TO 105 
         CALL R1F3KF (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2)) 
         GO TO 110 
  105    CALL R1F3KF (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2)) 
         GO TO 110 
  106    IF (IP .NE. 5) GO TO 108 
         IX2 = IW+IDO 
         IX3 = IX2+IDO 
         IX4 = IX3+IDO 
         IF (NA .NE. 0) GO TO 107 
         CALL R1F5KF (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),                  &
     &                      WA(IX3),WA(IX4))                            
         GO TO 110 
  107    CALL R1F5KF (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),                  &
     &                      WA(IX3),WA(IX4))                            
         GO TO 110 
  108    IF (IDO .EQ. 1) NA = 1-NA 
         IF (NA .NE. 0) GO TO 109 
         CALL R1FGKF (IDO,IP,L1,IDL1,C,C,C,IN,CH,CH,1,WA(IW)) 
         NA = 1 
         GO TO 110 
  109    CALL R1FGKF (IDO,IP,L1,IDL1,CH,CH,CH,1,C,C,IN,WA(IW)) 
         NA = 0 
  110    L2 = L1 
  111 END DO 
      SN = 1./N 
      TSN = 2./N 
      TSNM = -TSN 
      MODN = MOD(N,2) 
      NL = N-2 
      IF(MODN .NE. 0) NL = N-1 
      IF (NA .NE. 0) GO TO 120 
      C(1,1) = SN*CH(1) 
      DO 118 J=2,NL,2 
         C(1,J) = TSN*CH(J) 
         C(1,J+1) = TSNM*CH(J+1) 
  118 END DO 
      IF(MODN .NE. 0) RETURN 
      C(1,N) = SN*CH(N) 
      RETURN 
  120 C(1,1) = SN*C(1,1) 
      DO 122 J=2,NL,2 
         C(1,J) = TSN*C(1,J) 
         C(1,J+1) = TSNM*C(1,J+1) 
  122 END DO 
      IF(MODN .NE. 0) RETURN 
      C(1,N) = SN*C(1,N) 
      RETURN 
      END                                           
