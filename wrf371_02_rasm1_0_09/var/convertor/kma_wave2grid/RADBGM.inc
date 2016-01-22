C***********************************************************************
      SUBROUTINE RADBGM (INC,LOT,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(INC,IDO,L1,IP)      ,CC(INC,IDO,IP,L1)
     1               ,C1(INC,IDO,L1,IP)      ,C2(INC,IDL1,IP)
     2               ,CH2(INC,IDL1, IP)      ,WA(*)
      REAL*8 ARG,DCP,DSP,AR1,AI1,AR1H,DC2,DS2,AR2,AI2,AR2H,TPI
      DATA TPI /6.283185307179590/
C
      ARG = TPI/IP
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IDP2 = IDO+2
      IPP2 = IP+2
      IPPH = (IP+1)/2
C     IPPHC = IPP2-IPPH
C     IPPH2 = 2*IPPH
C
      DO 102 K=1,L1
      DO 102 I=1,IDO
            DO 101 L=1,LOT
  101         CH(L,I,K,1) = CC(L,I,1,K)
  102 CONTINUE
*VOPTION NOFVAL
      DO 113 J=2,IPPH
              JC = IPP2-J
              J2 = J+J
      DO 113 K=1,L1
            DO 112 L=1,LOT
              CH(L,1,K,J ) = CC(L,IDO,J2-2,K)+CC(L,IDO,J2-2,K)
              CH(L,1,K,JC) = CC(L,  1,J2-1,K)+CC(L,  1,J2-1,K)
  112       CONTINUE
  113 CONTINUE
      IF (IDO .NE. 1) THEN
*VOPTION NOFVAL
        DO 121 J=2,IPPH
            JC = IPP2-J
          DO 120 K=1,L1
*VOPTION NOFVAL
            DO 119 I=3,IDO,2
                IC = IDP2-I
              DO 118 L=1,LOT
                CH(L,I-1,K,J ) = CC(L,I-1,2*J-1,K)+CC(L,IC-1,2*J-2,K)
                CH(L,I  ,K,J ) = CC(L,I  ,2*J-1,K)-CC(L,IC  ,2*J-2,K)
                CH(L,I-1,K,JC) = CC(L,I-1,2*J-1,K)-CC(L,IC-1,2*J-2,K)
                CH(L,I  ,K,JC) = CC(L,I  ,2*J-1,K)+CC(L,IC  ,2*J-2,K)
  118         CONTINUE
  119       CONTINUE
  120     CONTINUE
  121   CONTINUE
      ENDIF
            AR1 = 1.0
            AI1 = 0.0
*VOPTION NOFVAL
      DO 126 M=2,IPPH
            MC = IPP2-M
            AR1H = DCP*AR1-DSP*AI1
            AI1  = DCP*AI1+DSP*AR1
            AR1  = AR1H
        DO 123 IK=1,IDL1
          DO 123 L=1,LOT
            C2(L,IK,M ) = CH2(L,IK,1)+AR1*CH2(L,IK, 2)
            C2(L,IK,MC) =             AI1*CH2(L,IK,IP)
  123     CONTINUE
            DC2 = AR1
            DS2 = AI1
            AR2 = AR1
            AI2 = AI1
*VOPTION NOFVAL
        DO 125 J=3,IPPH
              JC = IPP2-J
              AR2H = DC2*AR2-DS2*AI2
              AI2  = DC2*AI2+DS2*AR2
              AR2  = AR2H
          DO 124 IK=1,IDL1
            DO 124 L=1,LOT
              C2(L,IK,M ) = C2(L,IK,M )+AR2*CH2(L,IK,J )
              C2(L,IK,MC) = C2(L,IK,MC)+AI2*CH2(L,IK,JC)
  124       CONTINUE
  125   CONTINUE
  126 CONTINUE
      DO 129 J=2,IPPH
      DO 129 IK=1,IDL1
          DO 128 L=1,LOT
  128       CH2(L,IK,1) = CH2(L,IK,1)+CH2(L,IK,J)
  129 CONTINUE
*VOPTION NOFVAL
      DO 141 J=2,IPPH
              JC = IPP2-J
      DO 141 K=1,L1
            DO 140 L=1,LOT
              CH(L,1,K,J ) = C1(L,1,K,J)-C1(L,1,K,JC)
              CH(L,1,K,JC) = C1(L,1,K,J)+C1(L,1,K,JC)
  140       CONTINUE
  141 CONTINUE
      IF (IDO .EQ. 1) RETURN
*VOPTION NOFVAL
      DO 150 J=2,IPPH
            JC = IPP2-J
        DO 149 K=1,L1
        DO 149 I=3,IDO,2
            DO 148 L=1,LOT
              CH(L,I-1,K,J ) = C1(L,I-1,K,J)-C1(L,I  ,K,JC)
              CH(L,I  ,K,J ) = C1(L,I  ,K,J)+C1(L,I-1,K,JC)
              CH(L,I-1,K,JC) = C1(L,I-1,K,J)+C1(L,I  ,K,JC)
              CH(L,I  ,K,JC) = C1(L,I  ,K,J)-C1(L,I-1,K,JC)
  148       CONTINUE
  149   CONTINUE
  150 CONTINUE
      DO 152 IK=1,IDL1
        DO 152 L=1,LOT
  152      C2(L,IK,1) = CH2(L,IK,1)
      DO 155 J=2,IP
        DO 155 K=1,L1
          DO 154 L=1,LOT
  154         C1(L,1,K,J) = CH(L,1,K,J)
  155     CONTINUE
          IS = -IDO
*VOPTION NOFVAL
      DO 169 J=2,IP
            IS = IS+IDO
        DO 168 K=1,L1
*VOPTION NOFVAL
          DO 167 I=3,IDO,2
              IDIJ = IS+I-1
            DO 166 L=1,LOT
              C1(L,I-1,K,J) =
     *               WA(IDIJ-1)*CH(L,I-1,K,J)-WA(IDIJ)*CH(L,I  ,K,J)
              C1(L,I  ,K,J) =
     *               WA(IDIJ-1)*CH(L,I  ,K,J)+WA(IDIJ)*CH(L,I-1,K,J)
  166       CONTINUE
  167     CONTINUE
  168   CONTINUE
  169 CONTINUE
      RETURN
      END SUBROUTINE RADBGM

