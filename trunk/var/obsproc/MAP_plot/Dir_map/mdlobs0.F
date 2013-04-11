      SUBROUTINE MDLOBS0(NS,FOBS,XI,YJ,FMDL,IX,JX,KX,ICROSS,NAME)
C  
      DIMENSION FOBS(NS,KX),XI(NS),YJ(NS),FMDL(IX,JX,KX),
     *          FF(JX-ICROSS,IX-ICROSS)
      CHARACTER NAME*4,FVAL*8,SID*2
C
      DO K = 1,KX
C        PRINT *,'* LEVEL K =',K,'  VARIABLE: ',NAME,'  NS=',NS
C
        DO N = 1,NS
        CALL BINT(FOBS(N,K),XI(N),YJ(N),FMDL(1,1,K),IX,JX,ICROSS)
C        PRINT 10, N,XI(N),YJ(N),NAME,FOBS(N,K)
 10     format(' N=',I3,'  IX=',F8.2,' JX=',F8.2,2X,A4,'=',F8.2)
        END DO
C
      END DO
C
      RETURN
      END
C 
















