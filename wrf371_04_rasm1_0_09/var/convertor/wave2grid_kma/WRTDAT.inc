      SUBROUTINE WRTDAT                                                         
     I(NFL   ,                                                                  
     I IDATE , KT    ,                                                          
     I LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,                          
     I DATA  , IDIM  , JDIM  ,                                                  
     W IDA   )                                                                  
C                                                                               
      INTEGER IDATE(5)                                                          
      CHARACTER* 4 LEVEL, ELEM                                                  
      CHARACTER*32 TITLE                                                        
      CHARACTER*16 UNIT                                                         
      REAL*8    DATA(IDIM*JDIM) ! <<<--- WARNING!!! THIS IS REAL*8              
c     INTEGER*2 IDA(IDIM*JDIM)    !shc-rizvi
      INTEGER   IDA(IDIM*JDIM/2)    !shc-rizvi
C                                                                               
      NELM=1                                                                    
      WRITE(NFL) IDATE, KT, NELM, LEVEL, ELEM                                   
      CALL CR8I2V(DATA,BASE,AMP,IDA,IDIM*JDIM)                                  
      WRITE(NFL) LEVEL, ELEM, TITLE, UNIT, KTSD, KTSA, BASE, AMP, IDA           
C                                                                               
      RETURN                                                                    
      END SUBROUTINE WRTDAT                                                         
