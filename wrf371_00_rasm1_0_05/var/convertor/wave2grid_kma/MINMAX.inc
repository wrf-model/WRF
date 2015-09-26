      SUBROUTINE MINMAX(DATA,IDIM,JDIM,LDIM,ITTL)                               
      DIMENSION DATA(IDIM*JDIM,LDIM)                                            
      CHARACTER*4 ITTL                                                          
C                                                                               
      DO 100 L=1,LDIM                                                           
      FMAX=DATA(1,L)                                                            
      FMIN=DATA(1,L)                                                            
      IJMAX=1                                                                   
      IJMIN=1                                                                   
      DO 200 IJ=1,IDIM*JDIM                                                     
      IF(DATA(IJ,L).GT.FMAX) THEN                                               
      FMAX=DATA(IJ,L)                                                           
      IJMAX=IJ                                                                  
      END IF                                                                    
      IF(DATA(IJ,L).LT.FMIN) THEN                                               
      FMIN=DATA(IJ,L)                                                           
      IJMIN=IJ                                                                  
      END IF                                                                    
  200 CONTINUE                                                                  
      JMIN=IJMIN/IDIM+1                                                         
      IMIN=IJMIN-IDIM*(JMIN-1)                                                  
Crizvi      JMAX=IJMAX/IDIM+1                                                         
Crizvi      IMAX=IJMAX-IDIM*(J1MAX-1)                                                  
Crizvi      WRITE(6,300) ITTL,L,IMAX,JMAX,FMAX,IMIN,JMIN,FMIN                         
      J1MAX=IJMAX/IDIM+1                                                         
      I1MAX=IJMAX-IDIM*(J1MAX-1)                                                  
      WRITE(6,300) ITTL,L,I1MAX,J1MAX,FMAX,IMIN,JMIN,FMIN                         
  300 FORMAT(1H ,A4,'  LEVL=',I2,'  MAX(I,J)=',2I4,E12.5,                       
     1                           '  MIN(I,J)=',2I4,E12.5)                       
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE MINMAX
