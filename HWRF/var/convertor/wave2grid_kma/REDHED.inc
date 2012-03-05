      SUBROUTINE REDHED                                                         
     I(NFL   ,                                                                  
     O TYPE  ,IDATE ,FILE  ,MODEL ,RESL  ,EXPR  ,KTUNIT,IDTYPE,                 
     O IBACK ,NNSP  ,                                                           
     O IMD   ,JMD   ,NPROD ,FLONID, FLATID,                                     
     O XID   ,XJD   ,XLATD ,XLOND ,                                             
     O VCODD ,KMD   ,AAD   ,BBD   ,                                             
     O IMM   ,JMM   ,NPROM ,FLONIM, FLATIM,                                     
     O XIM   ,XJM   ,XLATM ,XLONM ,                                             
     O VCODM ,KMM   ,AAM   ,BBM   ,                                             
     O CINF  )                                                                  
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
      DIMENSION AAD(*), BBD(*), AAM(*), BBM(*)                                  
CM    DIMENSION AAD(KMD+1), BBD(KMD+1), AAM(KMM+1), BBM(KMM+1)                  
C                                                                               
      CHARACTER*80 CINF(10)                                                     
C                                                                               
      READ (NFL,IOSTAT=IOS)                                                
     1           TYPE, IDATE, FILE, MODEL, RESL, EXPR, KTUNIT, IDTYPE,          
     2           IBACK, NNSP                                                    
	  IF (IOS /= 0) THEN
		WRITE(96,*) 'REDHED: READ ERROR IN FILE:ERROR=',IOS
		STOP 9991
	  END IF
      READ (NFL,IOSTAT=IOS) IMD, JMD, NPROD,                               
     1           FLONID, FLATID, XID, XJD, XLATD, XLOND                         
	  IF (IOS /= 0) THEN
		WRITE(96,*) 'REDHED: READ ERROR IN FILE:ERROR=',IOS
		STOP 9992
	  END IF
      READ (NFL,IOSTAT=IOS) VCODD, KMD,                                    
     1           (AAD(I),I=1,KMD+1), (BBD(I),I=1,KMD+1)                         
	  IF (IOS /= 0) THEN
		WRITE(96,*) 'REDHED: READ ERROR IN FILE:ERROR=',IOS
		STOP 9993
	  END IF
      READ (NFL,IOSTAT=IOS) IMM, JMM, NPROM,                               
     1           FLONIM, FLATIM, XIM, XJM, XLATM, XLONM                         
	  IF (IOS /= 0) THEN
		WRITE(96,*) 'REDHED: READ ERROR IN FILE:ERROR=',IOS
		STOP 9994
	  END IF
      READ (NFL,IOSTAT=IOS) VCODM, KMM,                                    
     1           (AAM(I),I=1,KMM+1), (BBM(I),I=1,KMM+1)                         
	  IF (IOS /= 0) THEN
		WRITE(96,*) 'REDHED: READ ERROR IN FILE:ERROR=',IOS
		STOP 9994
	  END IF
      READ (NFL,IOSTAT=IOS) CINF                                           
	  IF (IOS /= 0) THEN
		WRITE(96,*) 'REDHED: READ ERROR IN FILE:ERROR=',IOS
		STOP 9995
	  END IF
C                                                                               
      WRITE(96,*) 'REDHED:FILE,NFL ', FILE,'   ',NFL                             
C                                                                               
      RETURN                                                                    
C                                                                               
      END SUBROUTINE REDHED                                                         
