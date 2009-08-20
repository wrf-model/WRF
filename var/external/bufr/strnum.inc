      SUBROUTINE STRNUM (STR, NUM) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    STRNUM                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE DECODES AN INTEGER FROM A CHARACTER STRING. 
!   THE INPUT STRING SHOULD CONTAIN ONLY DIGITS AND (OPTIONAL) TRAILING 
!   BLANKS AND SHOULD NOT CONTAIN ANY SIGN CHARACTERS (E.G. '+', '-')   
!   NOR LEADING BLANKS NOR EMBEDDED BLANKS.                             
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION                               
!                                                                       
! USAGE:    CALL STRNUM (STR, NUM)                                      
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING CONTAINING ENCODED INTEGER VALUE 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NUM      - INTEGER: DECODED VALUE                                 
!                      -1 = decode was unsuccessful                     
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        STRSUC                                  
!    THIS ROUTINE IS CALLED BY: JSTNUM   PARUTG   SEQSDX                
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(20) STR2 
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      NUM = 0 
      K = 0 
                                                                        
!     Note that, in the following call to subroutine STRSUC, the output 
!     string STR2 is not used anywhere else in this routine.  In fact,  
!     the only reason that subroutine STRSUC is being called here is to 
!     determine NUM, which, owing to the fact that the input string STR 
!     cannot contain any leading blanks, is equal to the number of      
!     digits to be decoded from the beginning of STR.                   
!  .... DK: Should we have a contingency for NUM returned as -1? (guess 
!           it can't ever happen, right)                                
                                                                        
      CALL STRSUC (STR, STR2, NUM) 
                                                                        
      DO I = 1, NUM 
      READ (STR (I:I) , '(I1)', ERR = 99) J 
      IF (J.EQ.0.AND.STR (I:I) .NE.'0') GOTO 99 
      K = K * 10 + J 
      ENDDO 
                                                                        
      NUM = K 
      GOTO 100 
                                                                        
!     Note that NUM = -1 unambiguously indicates a bad decode since     
!     the input string cannot contain sign characters; thus, NUM is     
!     always positive if the decode is successful.                      
                                                                        
   99 NUM = - 1 
      IF (IPRT.GE.0) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * , 'BUFRLIB: STRNUM - BAD DECODE -  RETURN WITH NUM = -1' 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      ENDIF 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE STRNUM                         
