C
C
C
C***********************************************************************
      SUBROUTINE RFFTIM (N, WA,IFAC)
      DIMENSION       WA(N), IFAC(*)
      IF (N .EQ. 1) RETURN
      CALL RFTI1M (N, WA,IFAC)
      RETURN
      END SUBROUTINE RFFTIM

