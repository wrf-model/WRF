C***********************************************************************
      SUBROUTINE RADB3M (INC,LOT,IDO,L1,CC,CH,WA1,WA2)
      DIMENSION  CC(INC,IDO,3,L1)  ,CH(INC,IDO,L1,3)  ,WA1(*)  ,WA2(*)
      DATA TAUR,TAUI /-.5,.8660254037844390/
C
      DO 101 K=1,L1
*VOPTION NOFVAL
        DO 101 L=1,LOT
          TR2         = CC(L,IDO,2,K)+CC(L,IDO,2,K)
          CR2         = CC(L,  1,1,K)+TAUR*TR2
          CI3         = TAUI*(CC(L,1,3,K)+CC(L,1,3,K))
          CH(L,1,K,1) = CC(L,  1,1,K)+TR2
          CH(L,1,K,2) = CR2-CI3
          CH(L,1,K,3) = CR2+CI3
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
            CR2           = CC(L,I-1,1,K)+TAUR*TR2
            CI2           = CC(L,I  ,1,K)+TAUR*TI2
            CR3           = TAUI*(CC(L,I-1,3,K)-CC(L,IC-1,2,K))
            CI3           = TAUI*(CC(L,I  ,3,K)+CC(L,IC  ,2,K))
            CH(L,I-1,K,1) = CC(L,I-1,1,K)+TR2
            CH(L,I  ,K,1) = CC(L,I  ,1,K)+TI2
            DR2           = CR2-CI3
            DI2           = CI2+CR3
            DR3           = CR2+CI3
            DI3           = CI2-CR3
            CH(L,I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(L,I  ,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(L,I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(L,I  ,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
  102     CONTINUE
  103   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADB3M

