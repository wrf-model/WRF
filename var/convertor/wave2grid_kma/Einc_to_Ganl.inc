         SUBROUTINE Einc_to_Ganl        !shc start
     1   (DPSE,DUE,DVE,DTE,DQE,    
     2     PSB, UB, VB, TB, QB,
     3     PSG, UG, VG, TG, QG,
     4     IMAX,JMAX,IMAXE,JMAXE,KMAX,MAXJZ)
      INTEGER IDATE(5), IDGES(5), IDSST(5)                                      
      CHARACTER*8 FILE, MODEL, RESL                                             
      CHARACTER*80 CINF(10)                                                     
      CHARACTER*4 TYPE, EXPR, KTUNIT, NPROD, NPROM, VCODD, VCODM                
      CHARACTER*4 LEVEL, ELEM                                                   
      CHARACTER*32 TITLE                                                        
      CHARACTER*16 UNIT                                                         
      CHARACTER*8 MDLINF(4)                                                     
      REAL        DTHPRO(7)                                                     
      INTEGER ITYP(2)                                                           
      CHARACTER*48 LABEL                                                        
      INTEGER JTINF(2)                                                          
	  CHARACTER*10	FROMUNPACK
	  INTEGER		IUNPACK
C                                                                               
      DIMENSION GPHIS(IMAX*JMAX)                                                
      REAL, DIMENSION(IMAX,JMAX)    :: GAU

      DIMENSION DPSE (IMAXE,JMAXE), 
     1       DUE (IMAXE,JMAXE,KMAX), DVE  (IMAXE,JMAXE,KMAX),
     2       DTE (IMAXE,JMAXE,KMAX), DQE  (IMAXE,JMAXE,KMAX) 
      DIMENSION DPSG (IMAX,JMAX), 
     1       DUG (IMAX,JMAX,KMAX), DVG  (IMAX,JMAX,KMAX),
     2       DTG (IMAX,JMAX,KMAX), DQG  (IMAX,JMAX,KMAX) 
      DIMENSION PSB (IMAX,JMAX), 
     1       UB (IMAX,JMAX,KMAX), VB  (IMAX,JMAX,KMAX),
     2       TB (IMAX,JMAX,KMAX), QB  (IMAX,JMAX,KMAX) 
      DIMENSION PSG (IMAX,JMAX), 
     1       UG (IMAX,JMAX,KMAX), VG  (IMAX,JMAX,KMAX),
     2       TG (IMAX,JMAX,KMAX), QG  (IMAX,JMAX,KMAX) 

      DIMENSION COLRAD(JMAX), DY(JMAX), LY(JMAX)                                
      REAL*8    GAUL(JMAX),GAUW(JMAX),COCOT(JMAX)                               
      DIMENSION RLAT(MAXJZ), ZDAT(MAXJZ,KMAX)                                   
C   =================================================================           
C   >>>   GENERATE GAUSSIAN LATITUDES                             <<<           
C   =================================================================           
      CALL GAUSS(GAUL,GAUW,JMAX)                                                
      DO 800 J=1,JMAX                                                           
      COLRAD(J)=ACOS(GAUL(J))                                                   
  800 CONTINUE                                                                  
      CALL ZMNLAT( RLAT, MAXJZ, COLRAD, JMAX )                                  
C                                                                               
C---------------------------------------------------------------------
C +++ CONVERT LAT/LON to GAUSS
C---------------------------------------------------------------------
        CALL LT2GAU (DPSE,IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,DPSG,DY,LY)
       DO K = 1, KMAX
        CALL LT2GAU (DTE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,DTG(:,:,K),DY,LY)
        CALL LT2GAU (DUE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,DUG(:,:,K),DY,LY)
        CALL LT2GAU (DVE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,DVG(:,:,K),DY,LY)
        CALL LT2GAU (DQE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,DQG(:,:,K),DY,LY)
       ENDDO
C---------------------------------------------------------------------
C +++ ANAL = INCR + BACKG
C---------------------------------------------------------------------
       DO I=1,IMAX
       DO J=1,JMAX
        PSG(I,J)=DPSG(I,J)+PSB(I,J)    
       DO K=1,KMAX
        UG(I,J,K)=DUG(I,J,K)+UB(I,J,K)
        VG(I,J,K)=DVG(I,J,K)+VB(I,J,K)
        TG(I,J,K)=DTG(I,J,K)+TB(I,J,K)
        QG(I,J,K)=DQG(I,J,K)+QB(I,J,K)
       ENDDO
       ENDDO
       ENDDO
C                                                                               
      END SUBROUTINE Einc_to_Ganl       !shc end
