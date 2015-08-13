      SUBROUTINE REDDAT_ASCII                                                         
     I(NFL   , IMAXE , JMAXE , KMAX  , PSE    ,                                               
     O GTE   , GUE   , GVE   , GQE   )                          
C                                                                               
      REAL*8 PSE(IMAXE,JMAXE)
      REAL*8 GUE(IMAXE,JMAXE,KMAX),GVE(IMAXE,JMAXE,KMAX),
     &     GTE(IMAXE,JMAXE,KMAX),GQE(IMAXE,JMAXE,KMAX)                                                
      REAL*8 DUM1(IMAXE,JMAXE),DUM2(IMAXE,JMAXE)
      REAL*8 DUM3(IMAXE,JMAXE,KMAX),DUM4(IMAXE,JMAXE,KMAX),
     &     DUM5(IMAXE,JMAXE,KMAX),DUM6(IMAXE,JMAXE,KMAX)                                                
      
C                                                                               
C     IVAR = 6
C                                                                               
C     DO 1000 L = 1, IVAR

C      IF(L.EQ.1) THEN
        DO J = 1, JMAXE
          READ(NFL,FMT='(10F12.5,1x)') (DUM1(I,J),I=1,IMAXE)
        ENDDO
C      ENDIF

C      IF(L.EQ.2) THEN
        DO J = 1, JMAXE
          READ(NFL,FMT='(10F12.5,1x)') (DUM2(I,J),I=1,IMAXE)
        ENDDO
          PSE = DUM2
C      ENDIF

C      IF(L.EQ.3) THEN
        DO 100 K = 1, KMAX
        DO 100 J = 1, JMAXE
          READ(NFL,FMT='(10F12.5,1x)') (DUM3(I,J,K),I=1,IMAXE)
100     CONTINUE
          GTE= DUM3
C      ENDIF

C      IF(L.EQ.4) THEN
        DO 200 K = 1, KMAX
        DO 200 J = 1, JMAXE
          READ(NFL,FMT='(10F12.5,1x)') (DUM4(I,J,K),I=1,IMAXE)
200     CONTINUE
          GUE= DUM4
C      ENDIF

C      IF(L.EQ.5) THEN
        DO 300 K = 1, KMAX
        DO 300 J = 1, JMAXE
          READ(NFL,FMT='(10F12.5,1x)') (DUM5(I,J,K),I=1,IMAXE)
300     CONTINUE
          GVE= DUM5
C      ENDIF
       
C      IF(L.EQ.6) THEN
        DO 400 K = 1, KMAX
        DO 400 J = 1, JMAXE
          READ(NFL,FMT='(10F12.5,1x)') (DUM6(I,J,K),I=1,IMAXE)
400     CONTINUE
          GQE= DUM6
C      ENDIF

1000  CONTINUE

      RETURN                                                                    
C                                                                               
      END SUBROUTINE REDDAT_ASCII                                                         
