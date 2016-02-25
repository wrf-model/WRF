C***********************************************************************
      SUBROUTINE RFTI1M (N, WA,IFAC)
      DIMENSION       WA(N)      ,IFAC(*)    ,NTRYH(4)
      REAL*8 ARG, ARGLD, ARGH, TPI
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4) /4,2,3,5/
      DATA TPI /6.283185307179590/
          NL = N
          NF = 0
          J = 0
  101     J = J+1
      IF (J .LE. 4) THEN
          NTRY = NTRYH(J)
      ELSE
          NTRY = NTRY+2
      ENDIF
  104     NQ = NL/NTRY
          NR = NL-NTRY*NQ
      IF (NR .NE. 0) GO TO 101
          NF = NF+1
          IFAC(NF+2) = NTRY
          NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF   .EQ. 1) GO TO 107
*VOPTION NOFVAL
        DO 106 I=2,NF
          IB = NF-I+2
          IFAC(IB+2) = IFAC(IB+1)
  106   CONTINUE
          IFAC(3) = 2
  107 IF (NL .NE. 1) GO TO 104
          IFAC(1) = N
          IFAC(2) = NF
          ARGH = TPI/N
          IS = 0
          NFM1 = NF-1
          L1 = 1
      IF (NFM1 .EQ. 0) RETURN
      DO 110 K1=1,NFM1
          IP = IFAC(K1+2)
          LD = 0
          L2 = L1*IP
          IDO = N/L2
          IPM = IP-1
        DO 109 J=1,IPM
            LD = LD+L1
            ARGLD = LD*ARGH
            IDOX = (IDO-1)/2
*VOPTION NOFVAL
          DO 108 IFI=1,IDOX
               ARG = IFI*ARGLD
               WA(2*IFI+IS-1) = COS(ARG)
               WA(2*IFI+IS  ) = SIN(ARG)
  108     CONTINUE
            IS = IS+IDO
  109   CONTINUE
          L1 = L2
  110 CONTINUE
      RETURN
      END SUBROUTINE RFTI1M

