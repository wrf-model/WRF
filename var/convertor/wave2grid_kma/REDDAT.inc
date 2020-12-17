      SUBROUTINE REDDAT                                                         
     I(NFL   ,                                                                  
     O IDATE , KT    ,                                                          
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     O DATA  , IRTN  ,                                                          
     I IDIM  , JDIM  , KMAX  ,                                                  
     W BASE  , AMP   ,IDA   )                                                   
C                                                                               
      INTEGER IDATE(5)                                                          
      CHARACTER* 4 LEVEL, ELEM                                                  
      CHARACTER*32 TITLE                                                        
      CHARACTER*16 UNIT                                                         
      INTEGER KTSD, KTSA                                                        
      DIMENSION DATA(IDIM*JDIM)                                                 
c     INTEGER*2 IDA(IDIM*JDIM)    !shc-rizvi
      INTEGER   IDA(IDIM*JDIM/2)  !shc-rizvi
C                                                                               
      INTEGER IDNEXT(5)                                                         
C                                                                               
      READ (NFL,END=999,ERR=999) IDATE, KT, NELM, LEVEL, ELEM                   
      IF( NELM.GT.1 ) THEN                                                      
        WRITE(96,*) 'REDDAT:NELM.GT.1'                                           
        WRITE(96,*) 'REDDAT:NELM=', NELM                                         
        STOP 999                                                                
      ELSE IF( NELM.EQ.0 ) THEN                                                 
C     ---------------------------------------------------------------           
C     >>>   DD ˜AŒ‹‘Î‰ž‚Ì‚Â‚à‚è                                   <<<       
C     ---------------------------------------------------------------           
        READ(NFL,END=998,ERR=998)                                               
     1           NNSP, IDNEXT, NNSP, NNSP, NNSP, NNSP, NNSP, NNSP,              
     2           NNSP, NNSP, NNSP, NNSP, NNSP                                   
        IF(IDATE(1).NE.IDNEXT(1).OR.IDATE(2).NE.IDNEXT(2)                       
     1   .OR.IDATE(3).NE.IDNEXT(3).OR.IDATE(4).NE.IDNEXT(4)) THEN               
          WRITE(96,*) '## INVALID DD CONNECTION'                                 
          WRITE(96,*) IDATE                                                      
          WRITE(96,*) IDNEXT                                                     
          GOTO 998                                                              
        ENDIF                                                                   
          WRITE(96,*) '## VALID DD CONNECTION'                                   
        READ(NFL)                                                               
        READ(NFL)                                                               
        READ(NFL)                                                               
        READ(NFL)                                                               
        READ(NFL)                                                               
        DO 10 I=1,NNSP                                                          
          READ(NFL)                                                             
   10   CONTINUE                                                                
        READ (NFL,END=999,ERR=999) IDATE, KT, NELM, LEVEL, ELEM                 
      ENDIF                                                                     
C                                                                               
    1 READ (NFL,END=999,ERR=999)                                                
     1  LEVEL, ELEM, TITLE, UNIT,                                               
     2  KTSD, KTSA, BASE, AMP,                                                  
     3  (IDA(I),I=1,IDIM*JDIM)                                                  
      CALL CI2R4V(DATA,BASE,AMP,IDA,IDIM*JDIM)                                  
CC    DO 10 I=1,IDIM*JDIM                                                       
CC      DATA(I)=BASE + AMP*IDA(I)                                               
CC 10 CONTINUE                                                                  
C                                                                               
      IRTN=0                                                                    
      RETURN                                                                    
C                                                                               
  998 IRTN=-1                                                                   
      RETURN                                                                    
C                                                                               
  999 WRITE(96,*) 'REDDAT: READ ERROR IN FILE',NFL                               
      STOP 999                                                                  
C                                                                               
      END SUBROUTINE REDDAT

