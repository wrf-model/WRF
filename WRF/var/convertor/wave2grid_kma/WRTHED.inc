      SUBROUTINE WRTHED                                                         
     I(NFL   ,                                                                  
     I TYPE  ,IDATE ,FILE  ,MODEL ,RESL  ,EXPR  ,KTUNIT,IDTYPE,                 
     I IBACK ,NNSP  ,                                                           
     I IMD   ,JMD   ,NPROD ,FLONID,FLATID,                                      
     I XID   ,XJD   ,XLATD ,XLOND ,                                             
     I VCODD ,KMD   ,AAD   ,BBD   ,                                             
     I IMM   ,JMM   ,NPROM ,FLONIM,FLATIM,                                      
     I XIM   ,XJM   ,XLATM ,XLONM ,                                             
     I VCODM ,KMM   ,AAM   ,BBM   ,                                             
     I CINF  )                                                                  
CC    IMPLICIT DOUBLE PRECISION (A-H,O-Z,\)                                     
C                                                                               
      INTEGER IDATE(5)                                                          
      CHARACTER*8 FILE, MODEL, RESL                                             
      CHARACTER*4 TYPE, EXPR, KTUNIT                                            
C                                                                               
      INTEGER IMD, JMD, IMM, JMM                                                
      CHARACTER*4 NPROD, NPROM                                                  
C                                                                               
      CHARACTER*4 VCODD, VCODM                                                  
      INTEGER KMD, KMM                                                          
      DIMENSION AAD(KMD+1), BBD(KMD+1), AAM(KMM+1), BBM(KMM+1)                  
C                                                                               
      CHARACTER*80 CINF(10)                                                     
C                                                                               
      WRITE(NFL) TYPE, IDATE, FILE, MODEL, RESL, EXPR, KTUNIT, IDTYPE,          
     1           IBACK, NNSP                                                    
      WRITE(NFL) IMD, JMD, NPROD,                                               
     1           REAL(FLONID), REAL(FLATID),                                    
     2           REAL(XID), REAL(XJD), REAL(XLATD), REAL(XLOND)                 
      WRITE(NFL) VCODD, KMD,                                                    
     1           (REAL(AAD(I)),I=1,KMD+1), (REAL(BBD(I)),I=1,KMD+1)             
      WRITE(NFL) IMM, JMM, NPROM,                                               
     1           REAL(FLONIM), REAL(FLATIM),                                    
     2           REAL(XIM), REAL(XJM), REAL(XLATM), REAL(XLONM)                 
      WRITE(NFL) VCODM, KMM,                                                    
     1           (REAL(AAM(I)),I=1,KMM+1), (REAL(BBM(I)),I=1,KMM+1)             
      WRITE(NFL) CINF                                                           
C                                                                               
      WRITE(6,*) 'WRTHED:FILE,NFL ', FILE,'   ',NFL                             
C                                                                               
      RETURN                                                                    
      END SUBROUTINE WRTHED                                                         
