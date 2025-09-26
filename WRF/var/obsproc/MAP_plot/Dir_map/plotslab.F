      SUBROUTINE PLOTSLAB(N,MMI,MMJ,KX,SIG,WLAT,WLON,IOFF,JOFF,
     >                    XX,VAR,ND,FF,AA,SPVAL)
C
#     include "param1.incl"
C
      DIMENSION XX(KX,N), SIG(KX)
      DIMENSION FF(IIMAX*JJMAX), AA(IIMAX*JJMAX), DATA(N)
      CHARACTER*(*) VAR
      CHARACTER*16 NAME
C
      PRINT 10,VAR
10    FORMAT(//'PLOTTING VARIABLE == ',A4)
C
      LVL   = 1
      ICS   = 0             ! dot/cross
      INDEX = 1             ! 0 -- no contours
      NDOT  = -682          ! negative contour pattern
      LW    = 4000          ! width of lines
      FINC = 1.0
      IF (VAR(1:2).EQ.'RH') FINC = 10.

      DO K = 1,KX
C
        WRITE(NAME,'(A3,2X,I2,2X,F6.3)') VAR(1:3),K,SIG(K)
        PRINT 11,NAME
11      FORMAT(/'PLOTTING ==> ',A16) 
        DO I = 1,N
          DATA(I) = XX(K,I)
          IF (ABS(DATA(I)-SPVAL).LT.1.E-3) GO TO 20
          IF (VAR(1:2).EQ.'QQ') DATA(I)=DATA(I)*1000.
20        CONTINUE
        END DO
C
        CALL PLHMP(LVL,MMI,MMJ,IOFF,JOFF,ICS,
     >             INDEX,FINC,NDOT,LW,SPVAL,
     >             N,wLAT,wLON,Data,NAME,NOBS,ND,FF,AA)
      END DO
C
      RETURN
      END
C 
