C***********************************************************************
      SUBROUTINE RADB4M (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(INC,IDO,4,L1)       ,CH(INC,IDO,L1,4)
     1               ,WA1(*)     ,WA2(*)     ,WA3(*)
      DATA SQRT2 /1.4142135623730950/
C
      DO 101 K=1,L1
*VOPTION NOFVAL
        DO 101 L=1,LOT
          TR1         = CC(L,  1,1,K)-CC(L,IDO,4,K)
          TR2         = CC(L,  1,1,K)+CC(L,IDO,4,K)
          TR3         = CC(L,IDO,2,K)+CC(L,IDO,2,K)
          TR4         = CC(L,  1,3,K)+CC(L,  1,3,K)
          CH(L,1,K,1) = TR2+TR3
          CH(L,1,K,2) = TR1-TR4
          CH(L,1,K,3) = TR2-TR3
          CH(L,1,K,4) = TR1+TR4
  101   CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
        DO 102 K=1,L1
*VOPTION NOFVAL
          DO 102 L=1,LOT
            TR1           = CC(L,IDO,1,K)-CC(L,IDO,3,K)
            TI1           = CC(L,  1,2,K)+CC(L,  1,4,K)
            TR2           = CC(L,IDO,1,K)+CC(L,IDO,3,K)
            TI2           = CC(L,  1,4,K)-CC(L,  1,2,K)
            CH(L,IDO,K,1) =         TR2+TR2
            CH(L,IDO,K,2) =  SQRT2*(TR1-TI1)
            CH(L,IDO,K,3) =         TI2+TI2
            CH(L,IDO,K,4) = -SQRT2*(TR1+TI1)
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
            TR1           = CC(L,I-1,1,K)-CC(L,IC-1,4,K)
            TI1           = CC(L,I  ,1,K)+CC(L,IC  ,4,K)
            TR2           = CC(L,I-1,1,K)+CC(L,IC-1,4,K)
            TI2           = CC(L,I  ,1,K)-CC(L,IC  ,4,K)
            TR3           = CC(L,I-1,3,K)+CC(L,IC-1,2,K)
            TI3           = CC(L,I  ,3,K)-CC(L,IC  ,2,K)
            TI4           = CC(L,I-1,3,K)-CC(L,IC-1,2,K)
            TR4           = CC(L,I  ,3,K)+CC(L,IC  ,2,K)
            CH(L,I-1,K,1) = TR2+TR3
            CH(L,I  ,K,1) = TI2+TI3
            CR3           = TR2-TR3
            CI3           = TI2-TI3
            CR2           = TR1-TR4
            CI2           = TI1+TI4
            CR4           = TR1+TR4
            CI4           = TI1-TI4
            CH(L,I-1,K,2) = WA1(I-2)*CR2-WA1(I-1)*CI2
            CH(L,I  ,K,2) = WA1(I-2)*CI2+WA1(I-1)*CR2
            CH(L,I-1,K,3) = WA2(I-2)*CR3-WA2(I-1)*CI3
            CH(L,I  ,K,3) = WA2(I-2)*CI3+WA2(I-1)*CR3
            CH(L,I-1,K,4) = WA3(I-2)*CR4-WA3(I-1)*CI4
            CH(L,I  ,K,4) = WA3(I-2)*CI4+WA3(I-1)*CR4
  103     CONTINUE
  104   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADB4M

