      SUBROUTINE LT2GAU                                          
     &(EDATA,IMAXE,JMAXE,IMAXG,JMAXG,COLRAD,                    
     & GDATA,DY,LY)                                            
C                                                             
      DIMENSION EDATA(IMAXE,JMAXE),GDATA(IMAXG,JMAXG)        
      DIMENSION DY(JMAXG),LY(JMAXG)                         
      DIMENSION COLRAD(*)                                  
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE
CCH      LOGICAL FIRST/.TRUE./                            
C                                                        
      IF(FIRST) THEN                                    
      FIRST =.FALSE.                                   
      PAI2  =8.0*ATAN(1.0)                            
      DLONE =PAI2/FLOAT(IMAXE)                       
      DLONG =PAI2/FLOAT(IMAXG)                     
      DLONEI=1.0/DLONE                            
      CNVG2E=DLONG*DLONEI                       
C                                              
      DO 100 JG=1,JMAXG                                               
      DO 100 JE=1,JMAXE                                              
      FLATE=FLOAT(JE-1)*DLONE                                       
      IF(COLRAD(JG).GE.FLATE) THEN                                 
      LY(JG)=JE                                                  
      DY(JG)=(COLRAD(JG)-FLATE)*DLONEI                          
      END IF                                                   
  100 CONTINUE                                                
C                                                            
      END IF                                                
C                                                          
      DO 200 JG=1,JMAXG                                   
      JE=LY(JG)                                                        
      FY=DY(JG)                                                       
      DO 200 IG=1,IMAXG                                              
      FLONG =FLOAT(IG-1)*CNVG2E                                     
      IE    =INT(FLONG)+1                                          
      IEP1  =IE+1                                                 
      IF(IE.EQ.IMAXE) IEP1=1                                     
      FX    =FLONG-FLOAT(IE-1)                                  
      GDATA(IG,JG)=(1.0-FX)*(1.0-FY)*EDATA(IE  ,JE  )          
     &            +     FX *(1.0-FY)*EDATA(IEP1,JE  )         
     &            +     FX *     FY *EDATA(IEP1,JE+1)        
     &            +(1.0-FX)*     FY *EDATA(IE  ,JE+1)       
  200 CONTINUE                                             
C                                                         
C                                                        
      RETURN                                           
      END SUBROUTINE LT2GAU                                          
