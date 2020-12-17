      SUBROUTINE MONTWO(DATA1,DATA2,CLV1,CLV2,IJMAX,KMAX)
C----------------------------------------------------------------------         
      DIMENSION DATA1(IJMAX,KMAX),DATA2(IJMAX,KMAX),
     1          DIFF (IJMAX,KMAX)
	  CHARACTER*11 CLV2,CLV1
	  DIMENSION VMX1(KMAX),VMN1(KMAX),VMX2(KMAX),VMN2(KMAX),
     1          DMX (KMAX),DMN (KMAX)
C
	  DIFF=DATA1-DATA2
	  VMX1=MAXVAL(DATA1,DIM=1)
	  VMN1=MINVAL(DATA1,DIM=1)
	  VMX2=MAXVAL(DATA2,DIM=1)
	  VMN2=MINVAL(DATA2,DIM=1)
	  DMX =MAXVAL(DIFF ,DIM=1)
	  DMN =MINVAL(DIFF ,DIM=1)
C
	  WRITE(6,*)'           ',CLV1,'        ',CLV2,
     1                            '        DIFF. OF 1 - 2'
	  WRITE(6,*)'LEVEL    MIN       MAX       MIN       MAX       MIN
     1       MAX'
	  DO K=1,KMAX
		WRITE(6,'(I5,6F10.3)')K,VMN1(K),VMX1(K),VMN2(K),VMX2(K),
     1                          DMN (k),DMX (K)
	  END DO
C
	  RETURN
	  END SUBROUTINE MONTWO
