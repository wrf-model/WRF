C***********************************************************************
      SUBROUTINE RFTF3M (N,INC,LOT, C, WA,IFAC, CH)
      DIMENSION       C(INC,N)   ,CH(INC,N)  ,WA(N)    ,IFAC(*)
        CALL RFTF9M (N,INC,LOT, NA,C, WA, IFAC, CH)
C
           CF = 1.0/FLOAT(N)
           N4=(N/4)*4
      IF (NA .EQ. 1) THEN
        IF (N4 .GE. 4) THEN
          DO 117 K=1,N4,4
          DO 117 L=1,LOT
            C(L,K  ) = CF*C(L,K  )
            C(L,K+1) = CF*C(L,K+1)
            C(L,K+2) = CF*C(L,K+2)
            C(L,K+3) = CF*C(L,K+3)
  117     CONTINUE
        ENDIF
        IF (N4 .NE. N) THEN
          DO 118 K=N4+1,N
          DO 118 L=1,LOT
            C(L,K) = CF*C(L,K)
  118     CONTINUE
        ENDIF
      ELSE
        IF (N4 .GE. 4) THEN
          DO 119 K=1,N4,4
          DO 119 L=1,LOT
            C(L,K  ) = CF*CH(L,K  )
            C(L,K+1) = CF*CH(L,K+1)
            C(L,K+2) = CF*CH(L,K+2)
            C(L,K+3) = CF*CH(L,K+3)
  119     CONTINUE
        ENDIF
        IF (N4 .NE. N) THEN
          DO 120 K=N4+1,N
          DO 120 L=1,LOT
            C(L,K) = CF*CH(L,K)
  120     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE RFTF3M

