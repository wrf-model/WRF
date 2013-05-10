C***********************************************************************
      SUBROUTINE RADF5M (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(INC,IDO,L1,5)       ,CH(INC,IDO,5,L1)
     1               ,WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.3090169943749470,.9510565162951540,
     1                         -.8090169943749470,.5877852522924730/
      DO 101 K=1,L1
*VOPTION NOFVAL
        DO 101 L=1,LOT
          CR2           = CC(L,1,K,5)+CC(L,1,K,2)
          CR3           = CC(L,1,K,4)+CC(L,1,K,3)
          CI5           = CC(L,1,K,5)-CC(L,1,K,2)
          CI4           = CC(L,1,K,4)-CC(L,1,K,3)
          CH(L,  1,1,K) = CC(L,1,K,1)+     CR2+     CR3
          CH(L,  1,3,K) =             TI11*CI5+TI12*CI4
          CH(L,IDO,2,K) = CC(L,1,K,1)+TR11*CR2+TR12*CR3
          CH(L,  1,5,K) =             TI12*CI5-TI11*CI4
          CH(L,IDO,4,K) = CC(L,1,K,1)+TR12*CR2+TR11*CR3
  101   CONTINUE
      IF (IDO .GT. 1) THEN
          IDP2 = IDO+2
        DO 103 K=1,L1
*VOPTION NOFVAL
        DO 103 I=3,IDO,2
            IC = IDP2-I
*VOPTION NOFVAL
          DO 102 L=1,LOT
            CA2            = WA1(I-2)*CC(L,I-1,K,2)
            CU2            = WA1(I-2)*CC(L,I  ,K,2)
            CA3            = WA2(I-2)*CC(L,I-1,K,3)
            CU3            = WA2(I-2)*CC(L,I  ,K,3)
            CA4            = WA3(I-2)*CC(L,I-1,K,4)
            CU4            = WA3(I-2)*CC(L,I  ,K,4)
            CA5            = WA4(I-2)*CC(L,I-1,K,5)
            CU5            = WA4(I-2)*CC(L,I  ,K,5)
            CB2            = WA1(I-1)*CC(L,I-1,K,2)
            CV2            = WA1(I-1)*CC(L,I  ,K,2)
            CB3            = WA2(I-1)*CC(L,I-1,K,3)
            CV3            = WA2(I-1)*CC(L,I  ,K,3)
            CB4            = WA3(I-1)*CC(L,I-1,K,4)
            CV4            = WA3(I-1)*CC(L,I  ,K,4)
            CB5            = WA4(I-1)*CC(L,I-1,K,5)
            CV5            = WA4(I-1)*CC(L,I  ,K,5)
            CH(L,IC-1,2,K) = CA2+CV2
            CH(L,IC  ,2,K) = CU2-CB2
            CH(L,I -1,3,K) = CA3+CV3
            CH(L,I   ,3,K) = CU3-CB3
            CH(L,IC-1,4,K) = CA4+CV4
            CH(L,IC  ,4,K) = CU4-CB4
            CH(L,I -1,5,K) = CA5+CV5
            CH(L,I   ,5,K) = CU5-CB5
  102     CONTINUE
  103   CONTINUE
        DO 105 K=1,L1
*VOPTION NOFVAL
        DO 105 I=3,IDO,2
             IC = IDP2-I
          DO 104 L=1,LOT
            CC(L,I-1,K,2) = CH(L,IC-1,2,K)+CH(L,I -1,5,K)
            CC(L,I  ,K,2) = CH(L,IC  ,2,K)+CH(L,I   ,5,K)
            CC(L,I-1,K,3) = CH(L,I -1,3,K)+CH(L,IC-1,4,K)
            CC(L,I  ,K,3) = CH(L,I   ,3,K)+CH(L,IC  ,4,K)
            CC(L,I-1,K,4) = CH(L,I   ,3,K)-CH(L,IC  ,4,K)
            CC(L,I  ,K,4) = CH(L,IC-1,4,K)-CH(L,I -1,3,K)
            CC(L,I-1,K,5) = CH(L,IC  ,2,K)-CH(L,I   ,5,K)
            CC(L,I  ,K,5) = CH(L,I -1,5,K)-CH(L,IC-1,2,K)
  104     CONTINUE
  105   CONTINUE
        DO 107 K=1,L1
        DO 107 I=3,IDO,2
             IC = IDP2-I
*VOPTION NOFVAL
          DO 106 L=1,LOT
            CTR2           = TR11*CC(L,I-1,K,2)+TR12*CC(L,I-1,K,3)
            CTI2           = TR11*CC(L,I  ,K,2)+TR12*CC(L,I  ,K,3)
            CTR3           = TR12*CC(L,I-1,K,2)+TR11*CC(L,I-1,K,3)
            CTI3           = TR12*CC(L,I  ,K,2)+TR11*CC(L,I  ,K,3)
            TR2            = CTR2+CC(L,I-1,K,1)
            TI2            = CTI2+CC(L,I  ,K,1)
            TR3            = CTR3+CC(L,I-1,K,1)
            TI3            = CTI3+CC(L,I  ,K,1)
            TR4            = TI12*CC(L,I-1,K,5)-TI11*CC(L,I-1,K,4)
            TI4            = TI12*CC(L,I  ,K,5)-TI11*CC(L,I  ,K,4)
            SR23           = CC(L,I-1,K,2)+CC(L,I-1,K,3)
            SI23           = CC(L,I  ,K,2)+CC(L,I  ,K,3)
            TR5            = TI11*CC(L,I-1,K,5)+TI12*CC(L,I-1,K,4)
            TI5            = TI11*CC(L,I  ,K,5)+TI12*CC(L,I  ,K,4)
            CH(L,I -1,1,K) = CC(L,I-1,K,1)+SR23
            CH(L,I   ,1,K) = CC(L,I  ,K,1)+SI23
            CH(L,IC-1,2,K) = TR2-TR5
            CH(L,IC  ,2,K) = TI5-TI2
            CH(L,I -1,3,K) = TR2+TR5
            CH(L,I   ,3,K) = TI2+TI5
            CH(L,IC-1,4,K) = TR3-TR4
            CH(L,IC  ,4,K) = TI4-TI3
            CH(L,I -1,5,K) = TR3+TR4
            CH(L,I   ,5,K) = TI3+TI4
  106     CONTINUE
  107   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADF5M

