C***********************************************************************
      SUBROUTINE RFFTBM (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
      DIMENSION       R(*)       ,WSAVE(*)   ,WA(N)    ,IFAC(*)
cch?     REAL*8 CTIME1,CTIME2
cch?     CALL XCLOCK(CTIME1,5)
C
      IF (N .EQ. 1) RETURN
C
      IF (JUMP .NE. 1) THEN
        CALL RFTB2M (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
      ELSE
        CALL RFTB1M (N,INC,     LOT, R, WA,IFAC, WSAVE)
      ENDIF
cch?     CALL XCLOCK(CTIME2,5)
cch?     CTIME1=CTIME2-CTIME1
cch?     CTIME2=1000.0*CTIME1/FLOAT(N*LOT)
cch?     WRITE(6,500) INC,JUMP,N,LOT, CTIME1,CTIME2
cch? 500 FORMAT(1H0,' RFFTBM: INC,JUMP,N,LOT =',4I6,'  TOTAL CPU =',F13.6
cch?    *      ,' (SEC) ;  UNIT CPU =',F13.6,' (MSEC)')
      RETURN
      END SUBROUTINE RFFTBM

