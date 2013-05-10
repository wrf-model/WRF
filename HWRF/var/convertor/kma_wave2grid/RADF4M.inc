C***********************************************************************
      SUBROUTINE RADF4M (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(INC,IDO,L1,4)       ,CH(INC,IDO,4,L1)
     1               ,WA1(*)     ,WA2(*)     ,WA3(*)
      DATA HSQT2 /.70710678118654750/
      DO 101 K=1,L1
*VOPTION NOFVAL
        DO 101 L=1,LOT
          TR1           = CC(L,1,K,2)+CC(L,1,K,4)
          TR2           = CC(L,1,K,1)+CC(L,1,K,3)
          CH(L,  1,3,K) = CC(L,1,K,4)-CC(L,1,K,2)
          CH(L,  1,1,K) = TR1+TR2
          CH(L,IDO,2,K) = CC(L,1,K,1)-CC(L,1,K,3)
          CH(L,IDO,4,K) = TR2-TR1
  101   CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
        DO 102 K=1,L1
*VOPTION NOFVAL
          DO 102 L=1,LOT
            TR1           =  HSQT2*(CC(L,IDO,K,2)-CC(L,IDO,K,4))
            TI1           = -HSQT2*(CC(L,IDO,K,2)+CC(L,IDO,K,4))
            CH(L,IDO,1,K) = CC(L,IDO,K,1)+TR1
            CH(L,  1,2,K) = TI1          -CC(L,IDO,K,3)
            CH(L,IDO,3,K) = CC(L,IDO,K,1)-TR1
            CH(L,  1,4,K) = TI1          +CC(L,IDO,K,3)
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
            CR2 = WA1(I-2)*CC(L,I-1,K,2)+WA1(I-1)*CC(L,I  ,K,2)
            CI2 = WA1(I-2)*CC(L,I  ,K,2)-WA1(I-1)*CC(L,I-1,K,2)
            CR3 = WA2(I-2)*CC(L,I-1,K,3)+WA2(I-1)*CC(L,I  ,K,3)
            CI3 = WA2(I-2)*CC(L,I  ,K,3)-WA2(I-1)*CC(L,I-1,K,3)
            CR4 = WA3(I-2)*CC(L,I-1,K,4)+WA3(I-1)*CC(L,I  ,K,4)
            CI4 = WA3(I-2)*CC(L,I  ,K,4)-WA3(I-1)*CC(L,I-1,K,4)
            TR2            = CC(L,I-1,K,1)+CR3
            TI2            = CC(L,I  ,K,1)+CI3
            TR1            = CR2+CR4
            TI1            = CI2+CI4
            TR3            = CC(L,I-1,K,1)-CR3
            TI3            = CC(L,I  ,K,1)-CI3
            TR4            = CR4-CR2
            TI4            = CI2-CI4
            CH(L,I -1,1,K) = TR1+TR2
            CH(L,I   ,1,K) = TI1+TI2
            CH(L,IC-1,2,K) = TR3-TI4
            CH(L,IC  ,2,K) = TR4-TI3
            CH(L,I -1,3,K) = TI4+TR3
            CH(L,I   ,3,K) = TR4+TI3
            CH(L,IC-1,4,K) = TR2-TR1
            CH(L,IC  ,4,K) = TI1-TI2
  103     CONTINUE
  104   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADF4M

