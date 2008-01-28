      SUBROUTINE TETEN(ICE)                                                     
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z,\)                                     
      REAL*8 XB,XC,X,TABLE,DTABLE                                               
      REAL*8 B,C,BI,CI,TSAT,TSATI                                               
      REAL*8 RTABLE                                                 !##         
      COMMON/CTETEN /TABLE (25000)                                              
      COMMON/DTETEN /DTABLE(25000)                                              
      COMMON/CLATENT/CTABLE(25000),DCL,TEMP0,TEMPI                              
      COMMON/RLQIC  /RTABLE(25000),TLI1,TLI2                        !##         
      COMMON/COMPHC/ CP,HL,GASR,ER,G,STB,SOLCON,TWOMG                           
      COMMON/COMEVP/CEV(4001),DFW,RDFW                                          
      DATA XB,XC/21.18123D0,5418.0D0/                                           
      DATA B,C,BI,CI,TSAT,TSATI/19.480254D0,4304.412D0,                         
     &                          23.684812D0,5803.3203D0,                        
     &                          29.55D0,7.85D0/                                 
      TEMP0 = t_kelvin
CBBK      TEMPI = 233.15                                                            
      TEMPI = 258.15                                                            
      DTEMP = TEMP0-TEMPI                                                       
      TLI1  = TEMPI                                                 !##         
      TLI2  = TEMP0                                                 !##         
      IF(ICE.EQ.1) THEN                                                         
      DICE = 3.33E5                                                             
      ELSE                                                                      
      DICE = 0.0                                                                
      ENDIF                                                                     
      HICE = HL + DICE                                                          
C  DL/DT                                                                        
      DCL = -DICE/DTEMP                                                         
CX    CLBYCP = HL/CP                                                            
CX    CLBYCPI = HICE/CP                                                         
      IF(ICE.EQ.0) THEN                                                         
      DO 10 I = 1,25000                                                         
      X = 123.2D0 + 0.01D0*I                                                    
! WRFVAR compiles at double precision by default, so DEXP is overkill
!      TABLE(I) = 0.622*DEXP(B-C/(X-TSAT))                                         
      TABLE(I) = 0.622*EXP(B-C/(X-TSAT))                                     
      DTABLE(I) = TABLE(I)*C/(X-TSAT)**2                                        
      CTABLE(I) = HL                                                            
      RTABLE(I) = 1.                                                !##         
 10   CONTINUE                                                                  
      ELSE                                                                      
      DO 20 I = 1,25000                                                         
      X = 123.2D0 + 0.01D0*I                                                    
      IF(X.GE.TEMP0) THEN                                                       
!      TABLE(I) = 0.622*DEXP(B-C/(X-TSAT))                                        
      TABLE(I) = 0.622*EXP(B-C/(X-TSAT))                                       
      DTABLE(I) = TABLE(I)*C/(X-TSAT)**2                                        
      CTABLE(I) = HL                                                            
      RTABLE(I) = 1.                                                !##         
      ELSEIF(X.LE.TEMPI) THEN                                                   
!      TABLE(I) = 0.622*DEXP(BI-CI/(X-TSATI))                                   
      TABLE(I) = 0.622*EXP(BI-CI/(X-TSATI))                                    
      DTABLE(I) = TABLE(I)*CI/(X-TSATI)**2                                      
      CTABLE(I) = HICE                                                          
      RTABLE(I) = 0.                                                !##         
      ELSE                                                                      
      RR = (TEMP0-X)/DTEMP                                                      
      CTABLE(I) = HL*(1.0-RR) + HICE*RR                                         
!      TBL1 = 0.622*DEXP(B-C/(X-TSAT))                                             
      TBL1 = 0.622*EXP(B-C/(X-TSAT))                                         
      DTBL1 = TBL1*C/(X-TSAT)**2     
!      TBL2 = 0.622*DEXP(BI-CI/(X-TSATI))                                          
      TBL2 = 0.622*EXP(BI-CI/(X-TSATI))                                      
      DTBL2 = TBL2*CI/(X-TSATI)**2                                              
      TABLE(I)  = TBL1*(1.D0-RR)+TBL2*RR                                        
      DTABLE(I) = DTBL1*(1.D0-RR)+DTBL2*RR+(TBL1-TBL2)/DTEMP                    
      RTABLE(I) = 1.D0-RR                                           !##         
C ##                         3-JI KANSUU : RTABLE(I) = TT*TT*(3.-2.*TT)         
C ##                                       WHERE   TT = 1.D0-RR                 
      ENDIF                                                                     
 20   CONTINUE                                                                  
      ENDIF                                                                     
      FWMX = 5.0                                                                
      FWMN = 0.0                                                                
      IFWM = 4001                                                               
      DFW = (FWMX-FWMN)/(IFWM-1.)                                               
      RDFW=1./DFW                                                               
*VOPTION NOFVAL                                                                 
      DO 30 I = 1,IFWM                                                          
      FW = FWMN + DFW*(I-1.)                                                    
      CEV(I) = 8.*GASR*(1.6+23.2*(FW)**0.167)*(FW)**0.467                       
   30 CONTINUE                                                                  
      CEV(1) = 0.0                                                              
C ##           CALL MNTRLQIC (RTABLE,TLI1,TLI2)                                 
      RETURN                                                                    
      END SUBROUTINE TETEN
