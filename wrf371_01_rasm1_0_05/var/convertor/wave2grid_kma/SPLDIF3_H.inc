      SUBROUTINE SPLDIF3_H( ZOUT, POUT, LMXOUT, ZIN, PIN, LMXIN, IJMAX,
     W SM, H, AL, AM, AP, C )
      DIMENSION ZOUT(IJMAX,LMXOUT), POUT(IJMAX,LMXOUT),
     1          ZIN (IJMAX,LMXIN),  PIN (IJMAX,LMXIN)
      DIMENSION SM(IJMAX,LMXIN), H(IJMAX,LMXIN), AL(IJMAX,LMXIN),
     1          AM(IJMAX,LMXIN), AP(IJMAX,LMXIN), C(IJMAX,LMXIN)

C     INPUT / ZIN (L), PIN (L), LMXIN  : INPUT.DATA, PRES(LOG), NUMBER
C     OUTPUT/ ZOUT(L), POUT(L), LMXOUT : OUTPUT-VAL, PRES(LOG), NUMBER

      LM1 = LMXIN-1
      GR = -gravity/gas_constant

      DO 110 L = 2, LMXIN
      DO 110 I = 1, IJMAX
        H(I,L) = PIN(I,L)-PIN(I,L-1)
  110 CONTINUE

      DO 120 L = 2, LM1
      DO 120 I = 1, IJMAX
        AL(I,L) = 0.5*H(I,L+1)/(H(I,L)+H(I,L+1))
        AM(I,L) = 0.5-AL(I,L)
  120 CONTINUE

C     ( LAPSE RATE IS CONSTANT )
C     SM(1) = SM(2) ; SM(LMXIN-1) = SM(LMXIN) : SECOND DERIVATIVE
      DO 125 I = 1, IJMAX
        AL(I,1)     = -1.0	! ORG
        AM(I,LMXIN) =  0.0  ! UPPER BOUNDARY
CORG    AM(I,LMXIN) = -1.0	! ORG
        AL(I,LMXIN) =  0.0	! ORG
CHOON   AL(I,LMXIN) = -1.0  ! KMA
  125 CONTINUE

      DO 130 L = 2, LMXIN
      DO 130 I = 1, IJMAX
C
        AP(I,L) = 1.0/(1.0-AL(I,L-1)*AM(I,L))
        AL(I,L) = AL(I,L)*AP(I,L)
  130 CONTINUE

      DO 155 I = 1, IJMAX
      C(I,1)     = 0.0
      C(I,LMXIN) = 0.0
  155 CONTINUE
C
      DO 160 L = 2, LM1
      DO 160 I = 1, IJMAX
        C(I,L) = 3.0*((ZIN(I,L+1)-ZIN(I,L))/H(I,L+1)
     1         - (ZIN(I,L)-ZIN(I,L-1))/H(I,L))
     2            /(H(I,L)+H(I,L+1))
  160 CONTINUE

C     FORWARD SUBSTITUTION

      DO 200 L = 2, LMXIN
      DO 200 I = 1, IJMAX
        C(I,L) = (C(I,L)-C(I,L-1)*AM(I,L))*AP(I,L)
  200 CONTINUE
      DO 205 I = 1, IJMAX
      SM(I,LMXIN) = C(I,LMXIN)
  205 CONTINUE

C     BACKWARD SUBSTUTUTION

      DO 220 K = 1, LM1
      DO 220 I = 1, IJMAX
        L = LMXIN-K
        SM(I,L) = C(I,L)-AL(I,L)*SM(I,L+1)
  220 CONTINUE

C     INTERPOLATION

C     LB = 2

      DO 500 LOUT = 1, LMXOUT
      DO 500 I = 1, IJMAX

        X = POUT(I,LOUT)
        L = LOUT ! FOR ONLY PIN.EQ.POUT
        IF( L.LT.2 ) L=2
CM      DO 300 L = LB, LMXIN
CM        IF( X.GE.PIN(L) ) GO TO 310
CM300   CONTINUE
CM      L = LMXIN
CM310   LB = L
 
        ZOUT(I,LOUT) = SM(I,L-1)*(  -(PIN(I,L)-X)**2
     1             /(2.0*H(I,L))+H(I,L)/6.0 )
     2             + SM(I,L)  *( (X-PIN(I,L-1))**2
     3             /(2.0*H(I,L))-H(I,L)/6.0 )
     \             + ( ZIN(I,L)-ZIN(I,L-1) )/H(I,L)
        ZOUT(I,LOUT) = ZOUT(I,LOUT)*GR

  500 CONTINUE

      RETURN
      END SUBROUTINE SPLDIF3_H
