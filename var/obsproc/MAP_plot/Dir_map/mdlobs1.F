      SUBROUTINE MDLOBS1(NS,NNB,NNE,FOBS,PSOBS,XI,YJ,FMDL,PSA,IX,JX,KX,
     -                                                    ICROSS,NAME)
C  
      DIMENSION FOBS(NS,KX),PSOBS(NS),XI(NS),YJ(NS),
     -          FMDL(IX,JX,KX),PSA(IX,JX),PSD(IX,JX),
     *          FF(JX-ICROSS,IX-ICROSS)
      CHARACTER NAME*4,FVAL*8,SID*2
C     
      IF (ICROSS.EQ.0) CALL P1P2(PSD,PSA,IX,JX)
C
      DO K = 1,KX
C        PRINT *,'* LEVEL K =',K,'  VARIABLE: ',NAME,'  NS=',NS
C .. decoupled
        DO I=1,IX-ICROSS
        DO J=1,JX-ICROSS
          IF (ICROSS.EQ.0) THEN
            PSFC = PSD(I,J)
          ELSE
            PSFC = PSA(I,J)
          ENDIF
          FMDL(I,J,K) = FMDL(I,J,K)/PSFC
        END DO
        END DO
C
        DO N = NNB,NNE
C .. interpolation
        CALL BINT(FOBS(N,K),XI(N),YJ(N),FMDL(1,1,K),IX,JX,ICROSS)
C
C .. coupled
        FOBS(N,K)=FOBS(N,K)*PSOBS(N)   
C        PRINT 10, N,K,XI(N),YJ(N),NAME,FOBS(N,K)
 10     format('N=',I3,' K=',I2,' IX=',F8.2,' JX=',F8.2,2X,A4,'=',F10.3)
        END DO
C .. coupled
        DO I=1,IX-ICROSS
        DO J=1,JX-ICROSS
          IF (ICROSS.EQ.0) THEN
            PSFC = PSD(I,J)
          ELSE
            PSFC = PSA(I,J)
          ENDIF
          FMDL(I,J,K) = FMDL(I,J,K)*PSFC
        END DO
        END DO
C
      END DO
C
      RETURN
      END
C 
