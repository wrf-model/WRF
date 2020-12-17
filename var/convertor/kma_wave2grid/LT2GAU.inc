      SUBROUTINE LT2GAU                                                 
     &(EDATA,IMAXE,JMAXE,IMAXG,JMAXG,COLRAD,                       
     & GDATA,DY,LY)                                                     
C                                                                       
      DIMENSION EDATA(IMAXE,JMAXE)
      DIMENSION GDATA(IMAXG,JMAXG)      
      DIMENSION DY(JMAXG),LY(JMAXG)                                     
      DIMENSION COLRAD(JMAXG) 

      PAI  =4.0*ATAN(1.0)  
      DLONE =PAI/FLOAT(JMAXE)                             
      DLONG =PAI/FLOAT(JMAXG)                              
      DLONEI=1.0/DLONE                                       
      CNVG2E=DLONG*DLONEI                                     
c      print*,' PAI = ',PAI
c      print*,' IMAXG= ',IMAXG,' JMAXG= ',JMAXG
c      print*,' IMAXE= ',IMAXE,' JMAXE= ',JMAXE
c      print*,' DLONE = ',DLONE,' DLONG= ',DLONG,
c     + ' CNVG2E= ',CNVG2E
C
      DO 200 JG=1,JMAXG                                        
      JE=LY(JG)                                                 
      FY=DY(JG)                                                  
      DO 200 IG=1,IMAXG  
      GDATA(IG,JG)=(1.0-FY)*EDATA(IG,JE)+ FY*EDATA(IG,JE+1)               
c      print*,IG,JG,JE,(1.0-FY),FY,' d1 & 2 res ',
c     + EDATA(IG,JE),EDATA(IG,JE+1),GDATA(IG,JG)
  200 CONTINUE                                                        
C                                                                    
C                                                                   
      RETURN                                                       
      END SUBROUTINE LT2GAU

