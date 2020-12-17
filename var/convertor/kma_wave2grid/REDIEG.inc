C======================================================================         
C   >>>   REDIEG   <<<   READ INITIAL DATA(GRID) AND CONVERT TO WAVE            
C======================================================================         
      SUBROUTINE REDIEG                                                         
     I(NIEGFL,ITPGFL,IMAX,JMAX,KMAX,IMX ,MEND1,NEND1,JEND1,          
     I MNWAV ,JMAXHF,KMX2  ,KQDMAX,KTSTAR,LAG   ,ITOPOG,MWVORG,                 
     I PNM   ,IFAX  ,TRIG  ,GW    ,SINCLT,COSCLT,ALP   ,DALP  ,                 
     O QDATA ,QPHIS ,IDATE ,ISTP  ,KTM   ,KT0   ,FSECM ,FSEC0 ,                 
#if   (defined CW)
     O PA    ,PB    ,CWCM  ,CWCP  ,CVRM  ,CVRP  ,XMB   ,CINF  ,                 
#else
     O PA    ,PB    ,CWCM  ,             CVRP  ,        CINF  ,                 
#endif
     W RAA   ,RBB   ,IDA   ,DATA  ,EDAT1 ,EDAT2 ,EDAT3 ,WDATA )
CMM  W PNMGC ,DPNMGC)                                                           
C                                                                               
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z,\)                                     
C                                                                               
C...FILE                                                                        
      CHARACTER*8 FILE, MODEL, RESL                                             
      CHARACTER*4 TYPE, EXPR, KTUNIT                                            
      INTEGER IMD, JMD, IMM, JMM                                                
      CHARACTER*4 NPROD, NPROM                                                  
      CHARACTER*4 VCODD, VCODM                                                  
      INTEGER KMD, KMM                                                          
      REAL RAA(KMAX+1), RBB(KMAX+1)                                             
      CHARACTER*80 CINF2(10)                                                    
C...OUTPUT                                                                      
      DIMENSION QDATA(KQDMAX,MNWAV), QPHIS(2,MNWAV)                             
      INTEGER IDATE(5)                                                          
      DIMENSION PA(KMAX+1), PB(KMAX+1)                                          
      DIMENSION CWCM(IMAX*JMAX*KMAX), CVRM(IMAX*JMAX*KMAX)                      
#if   (defined CW)
      DIMENSION CWCP(IMAX*JMAX*KMAX), CVRP(IMAX*JMAX*KMAX)                      
      DIMENSION XMB(IMAX*JMAX*KMAX)                                             
#endif
      CHARACTER*80 CINF                                                         
C...WORK                                                                        
      INTEGER*2 IDA(IMAX*JMAX)                                                  
      REAL      DATA(IMAX*JMAX)                                                 
      CHARACTER* 4 LEVEL, ELEM                                                  
      CHARACTER*32 TITLE                                                        
      CHARACTER*16 UNIT                                                         
      DIMENSION EDAT1(IMX*JMAX*KMAX)                                            
      DIMENSION EDAT2(IMX*JMAX*KMAX)                                            
      DIMENSION EDAT3(IMX*JMAX*KMAX)                                            
      DIMENSION WDATA(KMX2,MNWAV,2)                                             
CMM   DIMENSION WDATA(KMX2*MNWAV*2)                                             
CMM   DIMENSION PNMGC(MNWAV*JMAXHF), DPNMGC(MNWAV*JMAXHF)                       
C...INPUT                                                                       
      DIMENSION PNM(MNWAV,JMAXHF), IFAX(10), TRIG(IMAX), GW(JMAX)               
      DIMENSION SINCLT(JMAX), COSCLT(JMAX)                                      
      INTEGER   LAG(MEND1,NEND1)                                                
      DIMENSION ALP(MNWAV), DALP(MNWAV)                                         
      CHARACTER*4 MWVORG,IWORG,INOUT                                            
      DATA IWORG,INOUT/'CLMN','IN  '/                                           
      DATA ER/6371.D3/                                                          
C                                                                               
      COMMON/COMPTR/KQA  ,KQB ,KQF  ,KQP  ,KQE ,KQZ ,                           
     1              KQTMP,KQWV,KQROT,KQDIV,KQU ,KQV ,KQPS,KDROT,KDWV,           
     2              MQTMP,MQWV,MQROT,MQDIV          ,MQPS                       
C                                                                               
C   =================================================================           
C   >>>   INPUT TOPOGRAPHY FILE                                   <<<           
C   =================================================================           
      READ(ITPGFL) MDIM                                                         
      IF(MDIM.NE.MEND1) THEN                                                    
        WRITE(6,*)'TOPOGRAPHY FILE IRRELEVANT MEND1,MDIM=',MEND1,MDIM           
        STOP 9999                                                               
      END IF                                                                    
CMM   READ(ITPGFL)  ((WDATA(K,L),K=3,   4),L=1,MNWAV)                           
C                                                                               
C   *****************************************************************           
C   >>>   INPUT INITIAL DATA                                      <<<           
C   *****************************************************************           
      CALL REDHED                                                               
     I(NIEGFL,                                                                  
     O TYPE  ,IDATE ,FILE  ,MODEL ,RESL  ,EXPR  ,KTUNIT,IDTYPE,                 
     O IBACK ,NNSP  ,                                                           
     O IMD   ,JMD   ,NPROD ,FLONID, FLATID,                                     
     O XID   ,XJD   ,XLATD ,XLOND ,                                             
     O VCODD ,KMD   ,RAA   ,RBB   ,                                             
     O IMM   ,JMM   ,NPROM ,FLONIM, FLATIM,                                     
     O XIM   ,XJM   ,XLATM ,XLONM ,                                             
     O VCODM ,KMM   ,EDAT1 ,EDAT2 ,                                             
     O CINF2 )                                                                  
      WRITE(6,*) IDATE, FILE,MODEL,RESL,EXPR                                    
      WRITE(CINF(1:80),'(A80)') CINF2(1) ! FOR LONG FORECAST DIVISION           
C     IF( FILE.NE.'INITETA ' ) THEN                                             
C       WRITE(6,*) 'FILE ERROR! THIS IS NOT INITIAL DATA'                       
C       STOP 999                                                                
C     ENDIF                                                                     
      IF( IMAX.NE.IMD.OR.JMAX.NE.JMD.OR.KMAX.NE.KMD ) THEN                      
        WRITE(6,*) 'DIMENSION ERROR'                                            
        STOP 999                                                                
      ENDIF                                                                     
C                                                                               
      DO 10 K=1,KMD                                                             
        PA(K)=RAA(K)                                                            
        PB(K)=RBB(K)                                                            
   10 CONTINUE                                                                  
C                                                                               
      KT0=-1                                                                    
      ISTP=0                                                                    
      FSEC0=0.0                                                                 
C                                                                               
C   =================================================================           
C   >>>   PS                                                      <<<           
C   =================================================================           
1100  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( IRTN.EQ.-1 ) THEN                                                     
        WRITE(6,*) '*** I CANNOT FIND INITIAL DATA KT=', KTSTAR                 
        STOP 999                                                                
      ENDIF                                                                     

C	  write(*,*)'REDIEG  : KT=',KT,KTSTAR,LEVEL,' ',ELEM

      IF( KT.NE.KTSTAR ) GOTO 1100                                              
      IF( LEVEL.NE.'SURF'.OR.ELEM.NE.'P   ' ) GOTO 1100                         
C	  WRITE(6,*) 'REDIEG : LEVEL=',LEVEL, 'ELEM=',ELEM, DATA(10*10)                                       
      CALL RESET(EDAT1,IMAX*JMAX*KMAX)                                          
      CALL MOVERD(DATA,EDAT1,IMD*JMD)                                           
      CALL MNMX(EDAT1,IMAX*JMAX,'QPS ')                                         
      CALL RESET(WDATA,KMX2*MNWAV)                                              
      CALL G2W                                                                  
     I(MEND1,NEND1,JEND1,MNWAV,IMAX,JMAX,IMX ,JMAXHF,   1,                      
     I PNM  ,EDAT1,IFAX ,TRIG ,GW  ,                                            
     O WDATA(1,1,2),                                                            
CMM  O WDATA(KMX2*MNWAV+1),                                                     
     W EDAT2)                                                                   
      CALL REOWAV (WDATA(1,1,2),WDATA,MEND1,NEND1,JEND1,MNWAV,                  
CMM   CALL REOWAV (WDATA(KMX2*MNWAV+1),WDATA,MEND1,NEND1,JEND1,MNWAV,           
     1                2,  KMX2,0,   0,   2,LAG,IWORG,INOUT)                     
      CALL WAVMAG (WDATA,MNWAV,KMAX,'QPS ')                                     
C                                                                               
      READ(ITPGFL)  ((WDATA(K,L,1),K=3,4),L=1,MNWAV)                            
      CALL REOWAV (WDATA,QDATA,MEND1,NEND1,JEND1,MNWAV,                         
     1             KMX2,KQDMAX,0,KQPS,   2,LAG,IWORG,INOUT)                     
C                                                                               
      REWIND ITPGFL                                                             
      IF(ITOPOG.EQ.1) THEN                                                      
        CALL REOWAV (WDATA,QPHIS,MEND1,NEND1,JEND1,MNWAV,                       
     1               KMX2,     2,2,    0,   2,LAG,MWVORG,INOUT)                 
      ELSE                                                                      
        DO 100 K=1,2                                                            
          DO 100 L=1,MNWAV                                                      
            QPHIS(K,L)=0.0                                                      
  100   CONTINUE                                                                
      END IF                                                                    
C                                                                               
C   =================================================================           
C   >>>   U, V -> ROT, DIV                                        <<<           
C   =================================================================           
      DO 1200 K=1,KMAX                                                          
1210  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( LEVEL.EQ.'SURF'.OR.ELEM.NE.'U   ' ) GOTO 1210                         
      CALL MOVERD(DATA,EDAT1(IMD*JMD*(K-1)+1),IMD*JMD)                          
1200  CONTINUE                                                                  
      DO 1300 K=1,KMAX                                                          
1310  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( LEVEL.EQ.'SURF'.OR.ELEM.NE.'V   ' ) GOTO 1310                         
      CALL MOVERD(DATA,EDAT2(IMD*JMD*(K-1)+1),IMD*JMD)                          
1300  CONTINUE                                                                  
C                                                                               
      DO 1320 J=1,JMAXHF                                                        
      CALL LGNDR1(COSCLT(J),MEND1,ALP,DALP)                                     
      DO 1330 MN=1,MNWAV                                                        
       CWCM (MN+(J-1)*MNWAV)= ALP(MN)*GW(J)/(1.0-COSCLT(J)**2)                  
       CVRM (MN+(J-1)*MNWAV)=DALP(MN)*GW(J)/(1.0-COSCLT(J)**2)                  
CMM    PNMGC(MN+(J-1)*MNWAV)= ALP(MN)*GW(J)/(1.0-COSCLT(J)**2)                  
CMM   DPNMGC(MN+(J-1)*MNWAV)=DALP(MN)*GW(J)/(1.0-COSCLT(J)**2)                  
 1330 CONTINUE                                                                  
 1320 CONTINUE                                                                  
      CALL G2WDZ                                                                
     I(MEND1, NEND1 , JEND1, MNWAV, IMAX, JMAX  , IMX , JMAXHF, KMAX,           
     I CWCM , CVRM  , EDAT1, EDAT2, ER  , SINCLT, IFAX, TRIG  ,                 
CMM  I PNMGC, DPNMGC, EDAT1, EDAT2, ER  , SINCLT, IFAX, TRIG  ,                 
     O WDATA, WDATA(1,1,2),                                                     
CMM  O WDATA, WDATA(KMX2*MNWAV+1),                                              
     W EDAT3)                                                                   
      CALL WAVMAG (WDATA,MNWAV,KMAX,'QROT')                                     
      CALL REOWAV (WDATA,QDATA,MEND1,NEND1,JEND1,MNWAV,                         
     1             KMX2,KQDMAX,0,KQROT,KMX2,LAG,IWORG,INOUT)                    
      CALL WAVMAG (WDATA(1,1,2),MNWAV,KMAX,'QDIV')                              
CMM   CALL WAVMAG (WDATA(KMX2*MNWAV+1),MNWAV,KMAX,'QDIV')                       
      CALL REOWAV (WDATA(1,1,2),QDATA,MEND1,NEND1,JEND1,MNWAV,                  
CMM   CALL REOWAV (WDATA(KMX2*MNWAV+1),QDATA,MEND1,NEND1,JEND1,MNWAV,           
     1             KMX2,KQDMAX,0,KQDIV,KMX2,LAG,IWORG,INOUT)                    
C                                                                               
C   =================================================================           
C   >>>   T                                                       <<<           
C   =================================================================           
      DO 1400 K=1,KMAX                                                          
1410  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( LEVEL.EQ.'SURF'.OR.ELEM.NE.'T   ' ) GOTO 1410                         
      CALL MOVERD(DATA,EDAT1(IMD*JMD*(K-1)+1),IMD*JMD)                          
1400  CONTINUE                                                                  
      CALL G2W                                                                  
     I(MEND1,NEND1,JEND1,MNWAV,IMAX,JMAX,IMX ,JMAXHF,KMAX,                      
     I PNM  ,EDAT1,IFAX ,TRIG ,GW  ,                                            
     O WDATA,                                                                   
     W EDAT2)                                                                   
      CALL WAVMAG (WDATA,MNWAV,KMAX,'QTMP')                                     
      CALL REOWAV (WDATA,QDATA,MEND1,NEND1,JEND1,MNWAV,                         
     1             KMX2,KQDMAX,0,KQTMP,KMX2,LAG,IWORG,INOUT)                    
C                                                                               
C   =================================================================           
C   >>>   Q                                                       <<<           
C   =================================================================           
      DO 1500 K=1,KMAX                                                          
1510  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( LEVEL.EQ.'SURF'.OR.ELEM.NE.'Q   ' ) GOTO 1510                         
      CALL MOVERD(DATA,EDAT1(IMD*JMD*(K-1)+1),IMD*JMD)                          
C                                                                               
      DO 1520 I=1,IMD*JMD                                                       
        IF(EDAT1(IMD*JMD*(K-1)+I).LT.0.0)                                       
     1    EDAT1(IMD*JMD*(K-1)+I)=0.0                                            
1520  CONTINUE                                                                  
1500  CONTINUE                                                                  
      CALL G2W                                                                  
     I(MEND1,NEND1,JEND1,MNWAV,IMAX,JMAX,IMX ,JMAXHF,KMAX,                      
     I PNM  ,EDAT1,IFAX ,TRIG ,GW  ,                                            
     O WDATA,                                                                   
     W EDAT2)                                                                   
      CALL WAVMAG (WDATA,MNWAV,KMAX,'QWV ')                                     
      CALL REOWAV (WDATA,QDATA,MEND1,NEND1,JEND1,MNWAV,                         
     1             KMX2,KQDMAX,0,KQWV ,KMX2,LAG,IWORG,INOUT)                    
#if   (defined CW)
C   =================================================================           
C   >>>   CWC, CVR, XMB                                           <<<           
C   =================================================================           
      DO 1600 K=1,KMAX                                                          
1610  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( LEVEL.EQ.'SURF'.OR.ELEM.NE.'CWC ' ) GOTO 1610                         
      CALL MOVERD(DATA,CWCP(IMD*JMD*(K-1)+1),IMD*JMD)                           
      CALL MOVERD(DATA,CWCM(IMD*JMD*(K-1)+1),IMD*JMD)                           
C                                                                               
      DO 1620 I=1,IMD*JMD                                                       
        IF(CWCP(IMD*JMD*(K-1)+I).LT.0.0)                                        
     1    CWCP(IMD*JMD*(K-1)+I)=0.0                                             
        IF(CWCM(IMD*JMD*(K-1)+I).LT.0.0)                                        
     1    CWCM(IMD*JMD*(K-1)+I)=0.0                                             
1620  CONTINUE                                                                  
1600  CONTINUE                                                                  
      DO 1700 K=1,KMAX                                                          
1710  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( LEVEL.EQ.'SURF'.OR.ELEM.NE.'CVR ' ) GOTO 1710                         
      CALL MOVERD(DATA,CVRP(IMD*JMD*(K-1)+1),IMD*JMD)                           
      CALL MOVERD(DATA,CVRM(IMD*JMD*(K-1)+1),IMD*JMD)                           
C                                                                               
      DO 1720 I=1,IMD*JMD                                                       
        IF(CVRP(IMD*JMD*(K-1)+I).LT.0.0)                                        
     1    CVRP(IMD*JMD*(K-1)+I)=0.0                                             
        IF(CVRM(IMD*JMD*(K-1)+I).LT.0.0)                                        
     1    CVRM(IMD*JMD*(K-1)+I)=0.0                                             
1720  CONTINUE                                                                  
1700  CONTINUE                                                                  
      DO 1800 K=1,KMAX                                                          
1810  CALL REDDAT                                                               
     I(NIEGFL,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IMD   , JMD   , KMD   ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
      IF( LEVEL.EQ.'SURF'.OR.ELEM.NE.'UMB ' ) GOTO 1810                         
      CALL MOVERD(DATA,XMB(IMD*JMD*(K-1)+1),IMD*JMD)                            
C                                                                               
      DO 1820 I=1,IMD*JMD                                                       
        IF(XMB(IMD*JMD*(K-1)+I).LT.0.0)                                         
     1    XMB(IMD*JMD*(K-1)+I)=0.0                                              
1820  CONTINUE                                                                  
1800  CONTINUE                                                                  
#endif
C                                                                               
C   *****************************************************************           
C   >>>   ( T - DELT T )                                         <<<           
C   *****************************************************************           
C                                                                               
CC    IF(KTSTAR.LE.0) THEN                                                      
        DO 2000 K=1,KMX2                                                        
*VOPTION INDEP                                                                  
*vdir nodep                                                                  
        DO 2000 L=1,MNWAV                                                       
         QDATA(MQTMP+K,L)=QDATA(KQTMP+K,L)                                      
         QDATA(MQROT+K,L)=QDATA(KQROT+K,L)                                      
         QDATA(MQDIV+K,L)=QDATA(KQDIV+K,L)                                      
         QDATA(MQWV +K,L)=QDATA(KQWV +K,L)                                      
 2000   CONTINUE                                                                
        DO 2010 K=1,   2                                                        
*VOPTION INDEP                                                                  
*vdir nodep                                                                  
        DO 2010 L=1,MNWAV                                                       
         QDATA(MQPS +K,L)=QDATA(KQPS +K,L)                                      
 2010   CONTINUE                                                                
CC    ENDIF                                                                     
C                                                                               
      RETURN                                                                    
      END SUBROUTINE REDIEG

