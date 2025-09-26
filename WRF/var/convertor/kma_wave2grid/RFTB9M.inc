C***********************************************************************
      SUBROUTINE RFTB9M (N,INC,LOT, NA,C, WA,IFAC, CH)
      DIMENSION       C(INC,N)   ,CH(INC,N)  ,WA(N)    ,IFAC(*)
          NF = IFAC(2)
          NA = 1
          L1 = 1
          IW = 1
      DO 116 K1=1,NF
            IP = IFAC(K1+2)
            L2 = IP*L1
            IDO = N/L2
            IDL1 = IDO*L1
            NA = 1-NA
        IF (IP .EQ. 4) THEN
            IX2 = IW +IDO
            IX3 = IX2+IDO
          IF (NA .EQ. 0) THEN
            CALL RADB4M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
          ELSE
            CALL RADB4M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
          ENDIF
        ELSE IF (IP .EQ. 2) THEN
          IF (NA .EQ. 0) THEN
            CALL RADB2M (INC,LOT,IDO,L1,C,CH,WA(IW))
          ELSE
            CALL RADB2M (INC,LOT,IDO,L1,CH,C,WA(IW))
          ENDIF
        ELSE IF (IP .EQ. 3) THEN
            IX2 = IW +IDO
          IF (NA .EQ. 0) THEN
            CALL RADB3M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2))
          ELSE
            CALL RADB3M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2))
          ENDIF
        ELSE IF (IP .EQ. 5) THEN
            IX2 = IW +IDO
            IX3 = IX2+IDO
            IX4 = IX3+IDO
          IF (NA .EQ. 0) THEN
            CALL RADB5M (INC,LOT,IDO,L1,C,CH
     *                  ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
          ELSE
            CALL RADB5M (INC,LOT,IDO,L1,CH,C
     *                  ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
          ENDIF
        ELSE
  112    IF (NA .EQ. 0) THEN
           CALL RADBGM (INC,LOT,IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
           NA = 1
         ELSE
           CALL RADBGM (INC,LOT,IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
           NA = 0
         ENDIF
         IF (IDO .EQ. 1) NA = 1-NA
       ENDIF
           L1 = L2
           IW = IW+(IP-1)*IDO
  116 CONTINUE
      RETURN
      END SUBROUTINE RFTB9M

