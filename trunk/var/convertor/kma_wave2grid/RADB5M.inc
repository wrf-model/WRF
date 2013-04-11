C***********************************************************************
      SUBROUTINE RADB5M (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(INC,IDO,5,L1)       ,CH(INC,IDO,L1,5)
     1               ,WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.3090169943749470,.9510565162951540,
     1                         -.8090169943749470,.5877852522924730/
      DO 101 K=1,L1
*VOPTION NOFVAL
        DO 101 L=1,LOT
          TR2         = CC(L,IDO,2,K)+CC(L,IDO,2,K)
          TR3         = CC(L,IDO,4,K)+CC(L,IDO,4,K)
          TI5         = CC(L,  1,3,K)+CC(L,  1,3,K)
          TI4         = CC(L,  1,5,K)+CC(L,  1,5,K)
          CR2         = CC(L,  1,1,K)+TR11*TR2+TR12*TR3
          CI5         =               TI11*TI5+TI12*TI4
          CR3         = CC(L,  1,1,K)+TR12*TR2+TR11*TR3
          CI4         =               TI12*TI5-TI11*TI4
          CH(L,1,K,1) = CC(L,  1,1,K)+TR2+TR3
          CH(L,1,K,2) = CR2-CI5
          CH(L,1,K,3) = CR3-CI4
          CH(L,1,K,4) = CR3+CI4
          CH(L,1,K,5) = CR2+CI5
  101   CONTINUE
      IF (IDO .GT. 1) THEN
          IDP2 = IDO+2
        DO 103 K=1,L1
*VOPTION NOFVAL
        DO 103 I=3,IDO,2
            IC = IDP2-I
*VOPTION NOFVAL
          DO 102 L=1,LOT
            TR2           = CC(L,I-1,3,K)+CC(L,IC-1,2,K)
            TI2           = CC(L,I  ,3,K)-CC(L,IC  ,2,K)
            TR3           = CC(L,I-1,5,K)+CC(L,IC-1,4,K)
            TI3           = CC(L,I  ,5,K)-CC(L,IC  ,4,K)
            TR4           = CC(L,I-1,5,K)-CC(L,IC-1,4,K)
            TI4           = CC(L,I  ,5,K)+CC(L,IC  ,4,K)
            TR5           = CC(L,I-1,3,K)-CC(L,IC-1,2,K)
            TI5           = CC(L,I  ,3,K)+CC(L,IC  ,2,K)
            CR2           = CC(L,I-1,1,K)+TR11*TR2+TR12*TR3
            CI2           = CC(L,I  ,1,K)+TR11*TI2+TR12*TI3
            CR4           =               TI12*TR5-TI11*TR4
            CI4           =               TI12*TI5-TI11*TI4
            CR3           = CC(L,I-1,1,K)+TR12*TR2+TR11*TR3
            CI3           = CC(L,I  ,1,K)+TR12*TI2+TR11*TI3
            CR5           =               TI11*TR5+TI12*TR4
            CI5           =               TI11*TI5+TI12*TI4
            CH(L,I-1,K,1) = CC(L,I-1,1,K)+     TR2+     TR3
            CH(L,I  ,K,1) = CC(L,I  ,1,K)+     TI2+     TI3
            DR2           = CR2-CI5
            DI2           = CI2+CR5
            DR3           = CR3-CI4
            DI3           = CI3+CR4
            DR4           = CR3+CI4
            DI4           = CI3-CR4
            DR5           = CR2+CI5
            DI5           = CI2-CR5
            CH(L,I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(L,I  ,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(L,I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(L,I  ,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
            CH(L,I-1,K,4) = WA3(I-2)*DR4-WA3(I-1)*DI4
            CH(L,I  ,K,4) = WA3(I-2)*DI4+WA3(I-1)*DR4
            CH(L,I-1,K,5) = WA4(I-2)*DR5-WA4(I-1)*DI5
            CH(L,I  ,K,5) = WA4(I-2)*DI5+WA4(I-1)*DR5
  102     CONTINUE
  103   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADB5M

