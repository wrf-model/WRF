C***********************************************************************
      SUBROUTINE RADB2M (INC,LOT,IDO,L1,CC,CH,WA1)
      DIMENSION       CC(INC,IDO,2,L1)  ,CH(INC,IDO,L1,2)  ,WA1(*)
C
      DO 101 K=1,L1
        DO 101 L=1,LOT
          CH(L,1,K,1) = CC(L,1,1,K)+CC(L,IDO,2,K)
          CH(L,1,K,2) = CC(L,1,1,K)-CC(L,IDO,2,K)
  101   CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
        DO 102 K=1,L1
          DO 102 L=1,LOT
            CH(L,IDO,K,1) =   CC(L,IDO,1,K)+CC(L,IDO,1,K)
            CH(L,IDO,K,2) = -(CC(L,  1,2,K)+CC(L,  1,2,K))
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
            CH(L,I-1,K,1) = CC(L,I-1,1,K)+CC(L,IC-1,2,K)
            CH(L,I  ,K,1) = CC(L,I  ,1,K)-CC(L,IC  ,2,K)
            TR2           = CC(L,I-1,1,K)-CC(L,IC-1,2,K)
            TI2           = CC(L,I  ,1,K)+CC(L,IC  ,2,K)
            CH(L,I-1,K,2) = WA1(I-2)*TR2 -WA1(I-1)*TI2
            CH(L,I  ,K,2) = WA1(I-2)*TI2 +WA1(I-1)*TR2
  103     CONTINUE
  104   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADB2M

