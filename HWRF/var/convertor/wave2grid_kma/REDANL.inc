      SUBROUTINE REDANL                                                         
     I(NALFL ,IMAX  ,JMAX  ,KMAX  ,                                             
     O IDATE ,AAD   ,BBD   ,AAM   ,BBM   ,CINF   , 
     O PS    ,GZ    ,GU    ,GV    ,GQ    ,GT     ,
     O LARHM ,                                                                  
     W IDA   ,TYPE  ,EXPR  ,KTUNIT,NPROD ,NPROM ,VCODD ,VCODM ,                 
     W FILE  ,MODEL ,RESL  ,LEVEL ,ELEM  ,TITLE ,UNIT  )                        
C                                                                               
      INTEGER IDATE(5)                                                          
      DIMENSION PS(IMAX*JMAX)                                                   
      DIMENSION GZ(IMAX*JMAX,KMAX), GU(IMAX*JMAX,KMAX),                         
     1          GV(IMAX*JMAX,KMAX), GQ(IMAX*JMAX,KMAX),
     2          GT(IMAX*JMAX,KMAX)
      CHARACTER*80 CINF(10)                                                     
      DIMENSION AAD(KMAX+1), AAM(KMAX+1), BBD(KMAX+1), BBM(KMAX+1)              
c     INTEGER*2 IDA(IMAX*JMAX)    !shc-rizvi
      INTEGER   IDA(IMAX*JMAX/2)    !shc-rizvi
C                                                                               
      CHARACTER*4 TYPE, EXPR, KTUNIT, NPROD, NPROM, VCODD, VCODM                
      CHARACTER*8 FILE, MODEL, RESL                                             
      CHARACTER*4 LEVEL, ELEM                                                   
      CHARACTER*32 TITLE                                                        
      CHARACTER*16 UNIT                                                         
C
C                                                                               
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL REDHED                                                               
     I(NALFL ,                                                                  
     O TYPE  ,IDATE ,FILE  ,MODEL ,RESL  ,EXPR  ,KTUNIT,IDTYPE,                 
     O IBACK ,NNSP  ,                                                           
     O IMD   ,JMD   ,NPROD ,FLATID,FLONID,                                      
     O XID   ,XJD   ,XLATD ,XLOND ,                                             
     O VCODD ,KMD   ,AAD   ,BBD   ,                                             
     O IMM   ,JMM   ,NPROM ,FLATIM,FLONIM,                                      
     O XIM   ,XJM   ,XLATM ,XLONM ,                                             
     O VCODM ,KMM   ,AAM   ,BBM   ,                                             
     O CINF  )                                                                  
      WRITE(6,*)'ANAL FILE ',IDATE, FILE, MODEL, RESL, EXPR                     
      IF( FILE.NE.'ANALETA ' ) THEN                                             
        WRITE(6,*) 'FILE ERROR! THIS IS NOT ANAL DATA'                          
        STOP 999                                                                
      ENDIF                                                                     
C                                                                               
C   =================================================================           
C   >>>   SPECIAL                                                 <<<           
C   =================================================================           
      DO 10 I=1,NNSP                                                            
        READ(NALFL)                                                             
   10 CONTINUE                                                                  
C                                                                               
C   =================================================================           
C   >>>   PS                                                      <<<           
C   =================================================================           
   30 CALL REDDAT                                                               
     I(NALFL ,                                                                  
     O IDATE , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O PS    , IRTN  ,                                                          
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(.NOT.(LEVEL.EQ.'SURF'.AND.ELEM.EQ.'P   ')) GOTO 30                     
C                                                                               
C   =================================================================           
C   >>>   U                                                       <<<           
C   =================================================================           
      DO 110 K=1,KMAX                                                           
  100 CALL REDDAT                                                               
     I(NALFL ,                                                                  
     O IDATE , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GU(1,K), IRTN  ,                                                         
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(LEVEL.EQ.'SURF'.OR.ELEM.NE.'U   ') GOTO 100                            
  110 CONTINUE                                                                  
C   =================================================================           
C   >>>   V                                                       <<<           
C   =================================================================           
      DO 210 K=1,KMAX                                                           
  200 CALL REDDAT                                                               
     I(NALFL ,                                                                  
     O IDATE , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GV(1,K), IRTN  ,                                                         
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(LEVEL.EQ.'SURF'.OR.ELEM.NE.'V   ') GOTO 200                            
  210 CONTINUE                                                                  
C   =================================================================           
C   >>>   Z                                                       <<<           
C   =================================================================           
      DO 310 K=1,KMAX                                                           
  300 CALL REDDAT                                                               
     I(NALFL ,                                                                  
     O IDATE , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GZ(1,K), IRTN  ,                                                         
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(LEVEL.EQ.'SURF'.OR.ELEM.NE.'Z   ') GOTO 300                            
  310 CONTINUE                                                                  
C   =================================================================           
C   >>>   T                                                       <<<           
C   =================================================================           
      DO 320 K=1,KMAX                                                           
  330 CALL REDDAT                                                               
     I(NALFL ,                                                                  
     O IDATE , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GT(1,K), IRTN  ,                                                         
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(LEVEL.EQ.'SURF'.OR.ELEM.NE.'T   ') GOTO 330                            
  320 CONTINUE                                                                  
C   =================================================================           
C   >>>   RH, Q                                                   <<<           
C   =================================================================           
      LARHM=1                                                                   
      DO 410 K=1,KMAX                                                           
  400 CALL REDDAT                                                               
     I(NALFL ,                                                                  
     O IDATE , KT    , LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,          
     O GQ(1,K), IRTN  ,                                                         
     I IMD   , JMD   , 1     ,                                                  
     W BASE  , AMP   , IDA   )                                                  
      IF(LEVEL.EQ.'SURF'.OR.(ELEM.NE.'RH  '.AND.ELEM.NE.'Q   '))                
     1  GOTO 400                                                                
      IF(ELEM.EQ.'RH  ') LARHM=LARHM+1                                          
  410 CONTINUE                                                                  
C                                                                               
      WRITE(6,*) '## READ ANAL-ETA NORMAL END'                                  
      RETURN                                                                    
      END SUBROUTINE REDANL                                                         
