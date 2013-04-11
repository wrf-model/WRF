C***********************************************************************
      SUBROUTINE RADF3M (INC,LOT,IDO,L1,CC,CH,WA1,WA2)
      DIMENSION  CH(INC,IDO,3,L1)  ,CC(INC,IDO,L1,3)  ,WA1(*)  ,WA2(*)
      DATA TAUR,TAUI /-.5,.8660254037844390/
      DO 101 K=1,L1
*VOPTION NOFVAL
        DO 101 L=1,LOT
          CR2           =       CC(L,1,K,2)+CC(L,1,K,3)
          CH(L,  1,1,K) =       CC(L,1,K,1)+     CR2
          CH(L,  1,3,K) = TAUI*(CC(L,1,K,3)-CC(L,1,K,2))
          CH(L,IDO,2,K) =       CC(L,1,K,1)+TAUR*CR2
  101   CONTINUE
      IF (IDO .GT. 1) THEN
          IDP2 = IDO+2
        DO 103 K=1,L1
*VOPTION NOFVAL
        DO 103 I=3,IDO,2
            IC = IDP2-I
*VOPTION NOFVAL
          DO 102 L=1,LOT
            DR2 = WA1(I-2)*CC(L,I-1,K,2)+WA1(I-1)*CC(L,I  ,K,2)
            DI2 = WA1(I-2)*CC(L,I  ,K,2)-WA1(I-1)*CC(L,I-1,K,2)
            DR3 = WA2(I-2)*CC(L,I-1,K,3)+WA2(I-1)*CC(L,I  ,K,3)
            DI3 = WA2(I-2)*CC(L,I  ,K,3)-WA2(I-1)*CC(L,I-1,K,3)
            CR2            = DR2+DR3
            CI2            = DI2+DI3
            TR2            = CC(L,I-1,K,1)+TAUR*CR2
            TI2            = CC(L,I  ,K,1)+TAUR*CI2
            TR3            = TAUI*(DI2-DI3)
            TI3            = TAUI*(DR3-DR2)
            CH(L,I -1,1,K) = CC(L,I-1,K,1)+     CR2
            CH(L,I   ,1,K) = CC(L,I  ,K,1)+     CI2
            CH(L,IC-1,2,K) = TR2-TR3
            CH(L,IC  ,2,K) = TI3-TI2
            CH(L,I -1,3,K) = TR2+TR3
            CH(L,I   ,3,K) = TI2+TI3
  102     CONTINUE
  103   CONTINUE
      END IF
      RETURN
      END SUBROUTINE RADF3M

