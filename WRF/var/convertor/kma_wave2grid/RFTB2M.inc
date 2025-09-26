C***********************************************************************
      SUBROUTINE RFTB2M (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
      DIMENSION       R(*)       ,WSAVE(*)   ,WA(N)    ,IFAC(*)
C
        IF (JUMP .GT. INC) THEN
            INCN = (LOT*JUMP)/N
        ELSE
            INCN = INC
        ENDIF
          IF(MOD(INCN,16) .EQ. 0) INCN = INCN-1
          INCN = MAX(INCN,LOT)
C
            N4 = (N/4)*4
        IF (N4 .GE. 4) THEN
            IABASE = 1
            IBBASE = 1+INC
            ICBASE = 1+INC+INC
            IDBASE = 1+INC+INC+INC
            JABASE = 1
            JBBASE = 1+INCN
            JCBASE = 1+INCN+INCN
            JDBASE = 1+INCN+INCN+INCN
            INQ  = 4*INC
            INQN = 4*INCN
*VOPTION NOFVAL
        DO 102 K=1,N4,4
            IA = IABASE
            IB = IBBASE
            IC = ICBASE
            ID = IDBASE
            JA = JABASE
            JB = JBBASE
            JC = JCBASE
            JD = JDBASE
*VOPTION VEC,NOFVAL
*vdir nodep
          DO 101 L=1,LOT
            WSAVE(JA) = R(IA)
            WSAVE(JB) = R(IB)
            WSAVE(JC) = R(IC)
            WSAVE(JD) = R(ID)
            IA = IA+JUMP
            IB = IB+JUMP
            IC = IC+JUMP
            ID = ID+JUMP
            JA = JA+1
            JB = JB+1
            JC = JC+1
            JD = JD+1
  101     CONTINUE
            IABASE = IABASE+INQ
            IBBASE = IBBASE+INQ
            ICBASE = ICBASE+INQ
            IDBASE = IDBASE+INQ
            JABASE = JABASE+INQN
            JBBASE = JBBASE+INQN
            JCBASE = JCBASE+INQN
            JDBASE = JDBASE+INQN
  102   CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
            IABASE = 1+N4*INC
            JABASE = 1+N4*INCN
*VOPTION NOFVAL
        DO 104 K=N4+1,N
            IA = IABASE
            JA = JABASE
*VOPTION VEC,NOFVAL
*vdir nodep
          DO 103 L=1,LOT
            WSAVE(JA) = R(IA)
            IA = IA+JUMP
            JA = JA+1
  103     CONTINUE
            IABASE = IABASE+INC
            JABASE = JABASE+INCN
  104   CONTINUE
      ENDIF
C
        CALL RFTB1M (N,INCN,LOT, WSAVE, WA,IFAC, R)
C
        IF (N4 .GE. 4) THEN
            IABASE = 1
            IBBASE = 1+INC
            ICBASE = 1+INC+INC
            IDBASE = 1+INC+INC+INC
            JABASE = 1
            JBBASE = 1+INCN
            JCBASE = 1+INCN+INCN
            JDBASE = 1+INCN+INCN+INCN
            INQ  = 4*INC
            INQN = 4*INCN
*VOPTION NOFVAL
        DO 112 K=1,N4,4
            IA = IABASE
            IB = IBBASE
            IC = ICBASE
            ID = IDBASE
            JA = JABASE
            JB = JBBASE
            JC = JCBASE
            JD = JDBASE
*VOPTION VEC,NOFVAL
*vdir nodep
          DO 111 L=1,LOT
            R(IA) = WSAVE(JA)
            R(IB) = WSAVE(JB)
            R(IC) = WSAVE(JC)
            R(ID) = WSAVE(JD)
            IA = IA+JUMP
            IB = IB+JUMP
            IC = IC+JUMP
            ID = ID+JUMP
            JA = JA+1
            JB = JB+1
            JC = JC+1
            JD = JD+1
  111     CONTINUE
            IABASE = IABASE+INQ
            IBBASE = IBBASE+INQ
            ICBASE = ICBASE+INQ
            IDBASE = IDBASE+INQ
            JABASE = JABASE+INQN
            JBBASE = JBBASE+INQN
            JCBASE = JCBASE+INQN
            JDBASE = JDBASE+INQN
  112   CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
            IABASE = 1+N4*INC
            JABASE = 1+N4*INCN
*VOPTION NOFVAL
        DO 114 K=N4+1,N
            IA = IABASE
            JA = JABASE
*VOPTION VEC,NOFVAL
*vdir nodep
          DO 113 L=1,LOT
            R(IA) = WSAVE(JA)
            IA = IA+JUMP
            JA = JA+1
  113     CONTINUE
            IABASE = IABASE+INC
            JABASE = JABASE+INCN
  114   CONTINUE
      ENDIF
      RETURN
      END SUBROUTINE RFTB2M

