      SUBROUTINE VPRM( NVPFL, A, B, AAM, BBM, KMAX )                            
      DIMENSION A(KMAX+1), B(KMAX+1), AAM(KMAX+1), BBM(KMAX+1)                  
C                                                                               
      READ( NVPFL,END=9999,ERR=9999 ) KMX,(A(K),K=1,KMX),(B(K),K=1,KMX)         
      IF( KMX.NE.KMAX ) THEN                                                    
        WRITE(6,*) 'DIMENSION ERROR IN VPRM', KMX, KMAX                         
        STOP 999                                                                
      ENDIF                                                                     
      A(KMAX+1) = 0.0                                                           
      B(KMAX+1) = 0.0                                                           
C                                                                               
      WRITE(6,*) '<< MODEL LEVEL >> AMDL, AANL, BMDL, BANL'                     
      DO 10 K=1,KMAX+1                                                          
        IF(ABS(A(K)-AAM(K)).GT.ABS(A(K)*10E-3)                                  
     1 .OR.ABS(B(K)-BBM(K)).GT.ABS(B(K)*10E-3)) THEN                            
        WRITE(6,*) '*', A(K), AAM(K), B(K), BBM(K)                              
        ELSE                                                                    
        WRITE(6,*) ' ', A(K), AAM(K), B(K), BBM(K)                              
        ENDIF                                                                   
   10 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
C                                                                               
 9999 WRITE(6,*) '## READ ERROR IN VPRM'                                        
      STOP 999                                                                  
      END SUBROUTINE VPRM
