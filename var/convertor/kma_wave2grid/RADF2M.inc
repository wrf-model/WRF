C***********************************************************************
      SUBROUTINE RADF2M (INC,LOT,IDO,L1,CC,CH,WA1)
      DIMENSION       CH(INC,IDO,2,L1)  ,CC(INC,IDO,L1,2)  ,WA1(*)
      DO 101 K=1,L1
        DO 101 L=1,LOT
          CH(L,  1,1,K) = CC(L,1,K,1)+CC(L,1,K,2)
          CH(L,IDO,2,K) = CC(L,1,K,1)-CC(L,1,K,2)
  101   CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
        DO 102 K=1,L1
          DO 102 L=1,LOT
            CH(L,  1,2,K) = -CC(L,IDO,K,2)
            CH(L,IDO,1,K) =  CC(L,IDO,K,1)
  102     CONTINUE
      END IF
      IF (IDO .GT. 2) THEN
          IDP2 = IDO+2
        DO 104 K=1,L1
*VOPTION NOFVAL
        DO 104 I=3,IDO,2
            IC = IDP2-I
*VOPTION NOFVAL
          DO 103 L=1,LOT
            TR2 = WA1(I-2)*CC(L,I-1,K,2)+WA1(I-1)*CC(L,I  ,K,2)
            TI2 = WA1(I-2)*CC(L,I  ,K,2)-WA1(I-1)*CC(L,I-1,K,2)
            CH(L,I -1,1,K) = CC(L,I-1,K,1)+TR2
            CH(L,I   ,1,K) = TI2          +CC(L,I,K,1)
            CH(L,IC-1,2,K) = CC(L,I-1,K,1)-TR2
            CH(L,IC  ,2,K) = TI2          -CC(L,I,K,1)
  103     CONTINUE
  104   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADF2M

