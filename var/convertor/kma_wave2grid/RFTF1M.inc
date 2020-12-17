C***********************************************************************
      SUBROUTINE RFTF1M (N,INC,LOT, C, WA,IFAC, CH)
      DIMENSION       C(INC,N)   ,CH(INC,N)  ,WA(N)    ,IFAC(*)
        CALL RFTF9M (N,INC,LOT, NA,C, WA, IFAC, CH)
      IF (NA .EQ. 1) RETURN
           N4=(N/4)*4
        IF (N4 .GE. 4) THEN
          DO 117 K=1,N4,4
          DO 117 L=1,LOT
            C(L,K  ) = CH(L,K  )
            C(L,K+1) = CH(L,K+1)
            C(L,K+2) = CH(L,K+2)
            C(L,K+3) = CH(L,K+3)
  117     CONTINUE
        ENDIF
        IF (N4 .NE. N) THEN
          DO 118 K=N4+1,N
          DO 118 L=1,LOT
            C(L,K) = CH(L,K)
  118     CONTINUE
        ENDIF
      RETURN
      END SUBROUTINE RFTF1M

