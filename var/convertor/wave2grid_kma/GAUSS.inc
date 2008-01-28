      SUBROUTINE GAUSS(A,W,K)
C
C  A; COSINE OF COLATITUDE
C  W; GAUSSIAN WEIGHT
C  K; ORDER OF LEGENDRE FUNCTIONS
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION A(K),W(K)
C
      ESP=1.E-14
      C=(1.E0-(2.E0/pi)**2)*0.25E0
      FK=K
      KK=K/2
      CALL BSSLZ1(A,KK)
      DO 30 IS=1,KK
      XZ=COS(A(IS)/SQRT((FK+0.5E0)**2+C))
      ITER=0
   10 PKM2=1.0
      PKM1=XZ
      ITER=ITER+1
      IF(ITER.GT.10) GO TO 70
      DO 20 N=2,K
      FN=N
      PK=((2.E0*FN-1.E0)*XZ*PKM1-(FN-1.0)*PKM2)/FN
      PKM2=PKM1
   20 PKM1=PK
      PKM1=PKM2
      PKMRK=(FK*(PKM1-XZ*PK))/(1.E0-XZ**2)
      SP=PK/PKMRK
      XZ=XZ-SP
      AVSP=ABS(SP)
      IF(AVSP.GT.ESP) GO TO 10
      A(IS)=XZ
      W(IS)=(2.E0*(1.E0-XZ**2))/(FK*PKM1)**2
   30 CONTINUE
      IF(K.EQ.KK*2) GO TO 50
      A(KK+1)=0.E0
      PK=2.E0/FK**2
      DO 40 N=2,K,2
      FN=N
   40 PK=PK*FN**2/(FN-1.E0)**2
      W(KK+1)=PK
   50 CONTINUE
      DO 60 N=1,KK
      L=K+1-N
      A(L)=-A(N)
   60 W(L)=W(N)
      RETURN
   70 WRITE(96,6000)
 6000 FORMAT(//5X,14HERROR IN GAUAW//)
C
      STOP
      END SUBROUTINE GAUSS
