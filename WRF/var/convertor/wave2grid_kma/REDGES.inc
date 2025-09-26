      SUBROUTINE REDGES                                                         
     I(NGSFL ,IMAX  ,JMAX  ,KMAX  ,KTLAG ,IDATE ,IDCHCK,                        
     O IDGES ,AGD   ,BGD   ,AGM   ,BGM   ,GCWC  ,GCVR  ,GUMB  ,                 
     W IDA   ,IDSST )                                                           
C                                                                               
      INTEGER IDGES(5), IDATE(5), IDSST(5)                                      
      DIMENSION GCWC(IMAX*JMAX,KMAX), GCVR(IMAX*JMAX,KMAX),                     
     1          GUMB(IMAX*JMAX,KMAX)                                            
      DIMENSION AGD(KMAX+1), BGD(KMAX+1), AGM(KMAX+1), BGM(KMAX+1)              
c     INTEGER*2 IDA(IMAX*JMAX)    !shc-rizvi
      INTEGER   IDA(IMAX*JMAX/2)    !shc-rizvi
C                                                                               
      CHARACTER*4 TYPE, EXPR, KTUNIT, NPROD, NPROM, VCODD, VCODM                
      CHARACTER*8 FILE, MODEL, RESL                                             
      CHARACTER*80 CINF(10)                                                     
      CHARACTER*4 LEVEL, ELEM                                                   
      CHARACTER*32 TITLE                                                        
      CHARACTER*16 UNIT                                                         
C                                                                               
C   =================================================================           
C   >>>   DATA CHECK                                              <<<           
C   =================================================================           
      READ(NGSFL,ERR=1,END=1)                                                   
      GOTO 2                                                                    
    1 WRITE(6,*) '## I CANNOT READ FCST FILE'                                   
      CALL RESET( GCWC, IMAX*JMAX*KMAX )                                        
      CALL RESET( GCVR, IMAX*JMAX*KMAX )                                        
      CALL RESET( GUMB, IMAX*JMAX*KMAX )                                        
      RETURN                                                                    
    2 REWIND NGSFL                                                              
C                                                                               
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL REDHED                                                               
     I(NGSFL ,                                                                  
     O TYPE  ,IDGES ,FILE  ,MODEL ,RESL  ,EXPR  ,KTUNIT,IDTYPE,                 
     O IBACK ,NNSP  ,                                                           
     O IMD   ,JMD   ,NPROD ,FLATID,FLONID,                                      
     O XID   ,XJD   ,XLATD ,XLOND ,                                             
     O VCODD ,KMD   ,AGD   ,BGD   ,                                             
     O IMM   ,JMM   ,NPROM ,FLATIM,FLONIM,                                      
     O XIM   ,XJM   ,XLATM ,XLONM ,                                             
     O VCODM ,KMM   ,AGM   ,BGM   ,                                             
     O CINF  )                                                                  
      WRITE(6,*)'GUESS FILE ',IDGES, FILE, MODEL, RESL, EXPR                    
C                                                                               
C   =================================================================           
C   >>>   DATE CHECK                                              <<<           
C   =================================================================           
      IF( IDCHCK.EQ.1 ) THEN                                                    
        CALL CVDATE( IDSST, IDGES, KTLAG )                                      
        IF( IDATE(1).NE.IDSST(1).OR.IDATE(2).NE.IDSST(2).OR.                    
     1      IDATE(3).NE.IDSST(3) ) THEN                                         
          WRITE(6,*) 'GFEG : DATE CHECK ERROR'                                  
          STOP 999                                                              
        ENDIF                                                                   
      ENDIF                                                                     
C                                                                               
C   =================================================================           
C   >>>   SPECIAL                                                 <<<           
C   =================================================================           
      DO 10 I=1,NNSP                                                            
        READ(NGSFL)                                                             
   10 CONTINUE                                                                  
C                                                                               
C   =================================================================           
C   >>>   CWC, CVR                                                <<<           
C   =================================================================           
      DO 110 K=1,KMAX                                                           
  100 CALL REDDAT                                                               
     I(NGSFL ,                                                                  
     O IDGES , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GCWC(1,K), IRTN  ,                                                       
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(KT.NE.KTLAG.OR.LEVEL.EQ.'SURF'.OR.ELEM.NE.'CWC ') GOTO 100             
  110 CONTINUE                                                                  
C                                                                               
      DO 210 K=1,KMAX                                                           
  200 CALL REDDAT                                                               
     I(NGSFL ,                                                                  
     O IDGES , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GCVR(1,K), IRTN  ,                                                       
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(KT.NE.KTLAG.OR.LEVEL.EQ.'SURF'.OR.ELEM.NE.'CVR ') GOTO 200             
  210 CONTINUE                                                                  
C                                                                               
C   =================================================================           
C   >>>   UMB                                                     <<<           
C   =================================================================           
      DO 310 K=1,KMAX                                                           
  300 CALL REDDAT                                                               
     I(NGSFL ,                                                                  
     O IDGES , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GUMB(1,K), IRTN  ,                                                       
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(KT.NE.KTLAG.OR.LEVEL.EQ.'SURF'.OR.ELEM.NE.'UMB ') GOTO 300             
  310 CONTINUE                                                                  
C                                                                               
      WRITE(6,*) '## READ FCST-ETA NORMAL END'                                  
      RETURN                                                                    
      END SUBROUTINE REDGES                                                         
