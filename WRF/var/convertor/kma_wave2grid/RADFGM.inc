C***********************************************************************
      SUBROUTINE RADFGM (INC,LOT,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(INC,IDO,L1,IP)        ,CC(INC,IDO,IP,L1)
     1               ,C1(INC,IDO,L1,IP)        ,C2(INC,IDL1,IP)
     2               ,CH2(INC,IDL1,IP)         ,WA(*)
      REAL*8 ARG,DCP,DSP,AR1,AI1,AR1H,DC2,DS2,AR2,AI2,AR2H,TPI
      DATA TPI /6.283185307179590/
C
      ARG = TPI/IP
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP+2
C     IPPHC = IPP2-IPPH
C     IPPH2= 2*IPPH
      IDP2 = IDO+2
C
      IF (IDO .EQ. 1) GO TO 1000
      DO 101 IK=1,IDL1
          DO 101 L=1,LOT
  101         CH2(L,IK,1) = C2(L,IK,1)
      DO 104 J=2,IP
      DO 104 K=1,L1
          DO 103 L=1,LOT
  103         CH(L,1,K,J) = C1(L,1,K,J)
  104 CONTINUE
          IS = -IDO
*VOPTION NOFVAL
      DO 116 J=2,IP
            IS = IS+IDO
        DO 115 K=1,L1
*VOPTION NOFVAL
          DO 114 I=3,IDO,2
              IDIJ = IS+I-1
            DO 113 L=1,LOT
              CH(L,I-1,K,J) =
     *                WA(IDIJ-1)*C1(L,I-1,K,J)+WA(IDIJ)*C1(L,I  ,K,J)
              CH(L,I  ,K,J) =
     *                WA(IDIJ-1)*C1(L,I  ,K,J)-WA(IDIJ)*C1(L,I-1,K,J)
  113       CONTINUE
  114     CONTINUE
  115   CONTINUE
  116 CONTINUE
*VOPTION NOFVAL
      DO 120 J=2,IPPH
            JC = IPP2-J
        DO 119 K=1,L1
        DO 119 I=3,IDO,2
            DO 118 L=1,LOT
              C1(L,I-1,K,J ) = CH(L,I-1,K,J )+CH(L,I-1,K,JC)
              C1(L,I  ,K,J ) = CH(L,I  ,K,J )+CH(L,I  ,K,JC)
              C1(L,I-1,K,JC) = CH(L,I  ,K,J )-CH(L,I  ,K,JC)
              C1(L,I  ,K,JC) = CH(L,I-1,K,JC)-CH(L,I-1,K,J )
  118       CONTINUE
  119   CONTINUE
  120 CONTINUE
      GO TO 2000
C
 1000 CONTINUE
      DO 122 IK=1,IDL1
        DO 122 L=1,LOT
  122    C2(L,IK,1) = CH2(L,IK,1)
C
 2000 CONTINUE
*VOPTION NOFVAL
      DO 125 J=2,IPPH
              JC = IPP2-J
        DO 124 K=1,L1
            DO 124 L=1,LOT
              C1(L,1,K,J ) = CH(L,1,K,J )+CH(L,1,K,JC)
              C1(L,1,K,JC) = CH(L,1,K,JC)-CH(L,1,K,J )
  124       CONTINUE
  125 CONTINUE
        AR1 = 1.0
        AI1 = 0.0
*VOPTION NOFVAL
      DO 134 M=2,IPPH
            MC = IPP2-M
            AR1H = DCP*AR1-DSP*AI1
            AI1  = DCP*AI1+DSP*AR1
            AR1  = AR1H
        DO 131 IK=1,IDL1
          DO 131 L=1,LOT
            CH2(L,IK,M ) = C2(L,IK,1)+AR1*C2(L,IK, 2)
            CH2(L,IK,MC) =            AI1*C2(L,IK,IP)
  131     CONTINUE
            DC2 = AR1
            DS2 = AI1
            AR2 = AR1
            AI2 = AI1
*VOPTION NOFVAL
        DO 133 J=3,IPPH
              JC = IPP2-J
              AR2H = DC2*AR2-DS2*AI2
              AI2  = DC2*AI2+DS2*AR2
              AR2  = AR2H
          DO 132 IK=1,IDL1
            DO 132 L=1,LOT
              CH2(L,IK,M ) = CH2(L,IK,M )+AR2*C2(L,IK,J )
              CH2(L,IK,MC) = CH2(L,IK,MC)+AI2*C2(L,IK,JC)
  132       CONTINUE
  133   CONTINUE
  134 CONTINUE
      DO 137 J=2,IPPH
        DO 137 IK=1,IDL1
            DO 136 L=1,LOT
  136         CH2(L,IK,1) = CH2(L,IK,1)+C2(L,IK,J)
  137     CONTINUE
      DO 148 K=1,L1
        DO 148 I=1,IDO
          DO 147 L=1,LOT
  147        CC(L,I,1,K) = CH(L,I,K,1)
  148 CONTINUE
*VOPTION NOFVAL
      DO 159 J=2,IPPH
              JC = IPP2-J
              J2 = J+J
        DO 158 K=1,L1
            DO 158 L=1,LOT
              CC(L,IDO,J2-2,K) = CH(L,1,K,J )
              CC(L,  1,J2-1,K) = CH(L,1,K,JC)
  158       CONTINUE
  159 CONTINUE
      IF (IDO .EQ. 1) RETURN
*VOPTION NOFVAL
      DO 167 J=2,IPPH
            JC = IPP2-J
            J2 = J+J
        DO 166 K=1,L1
*VOPTION NOFVAL
          DO 165 I=3,IDO,2
              IC = IDP2-I
            DO 164 L=1,LOT
              CC(L,I -1,J2-1,K) = CH(L,I-1,K,J )+CH(L,I-1,K,JC)
              CC(L,I   ,J2-1,K) = CH(L,I  ,K,J )+CH(L,I  ,K,JC)
              CC(L,IC-1,J2-2,K) = CH(L,I-1,K,J )-CH(L,I-1,K,JC)
              CC(L,IC  ,J2-2,K) = CH(L,I  ,K,JC)-CH(L,I  ,K,J )
  164       CONTINUE
  165     CONTINUE
  166   CONTINUE
  167 CONTINUE
      RETURN
      END SUBROUTINE RADFGM

