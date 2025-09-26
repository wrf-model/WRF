C***********************************************************************
      SUBROUTINE RFTF9M (N,INC,LOT, NA,C, WA,IFAC, CH)
      DIMENSION       C(INC,N)   ,CH(INC,N)  ,WA(N)    ,IFAC(*)
          NF = IFAC(2)
          NA = 1
          L2 = N
          IW = N
      DO 111 K1=1,NF
          KH = NF-K1
          IP = IFAC(KH+3)
          L1 = L2/IP
          IDO = N/L2
          IDL1 = IDO*L1
          IW = IW-(IP-1)*IDO
          NA = 1-NA
       IF (IP .EQ. 4) THEN
           IX2 = IW +IDO
           IX3 = IX2+IDO
         IF (NA .EQ. 0) THEN
           CALL RADF4M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         ELSE
           CALL RADF4M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
         ENDIF
       ELSE IF (IP .EQ. 2) THEN
         IF (NA .EQ. 0) THEN
           CALL RADF2M (INC,LOT,IDO,L1,C,CH,WA(IW))
         ELSE
           CALL RADF2M (INC,LOT,IDO,L1,CH,C,WA(IW))
         ENDIF
       ELSE IF (IP .EQ. 3) THEN
           IX2 = IW +IDO
         IF (NA .EQ. 0) THEN
           CALL RADF3M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2))
         ELSE
           CALL RADF3M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2))
         ENDIF
       ELSE IF (IP .EQ. 5) THEN
           IX2 = IW +IDO
           IX3 = IX2+IDO
           IX4 = IX3+IDO
         IF (NA .EQ. 0) THEN
           CALL RADF5M (INC,LOT,IDO,L1,C,CH
     *                 ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         ELSE
           CALL RADF5M (INC,LOT,IDO,L1,CH,C
     *                 ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         ENDIF
       ELSE
         IF (IDO .EQ. 1) NA = 1-NA
         IF (NA .EQ. 0) THEN
           CALL RADFGM (INC,LOT,IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
           NA = 1
         ELSE
           CALL RADFGM (INC,LOT,IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
           NA = 0
         ENDIF
       ENDIF
           L2 = L1
  111 CONTINUE
      RETURN
      END SUBROUTINE RFTF9M

