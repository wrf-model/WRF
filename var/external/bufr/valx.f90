      FUNCTION VALX (STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    VALX                                                   
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION DECODES A REAL NUMBER FROM A CHARACTER        
!   STRING.  IF THE DECODE FAILS, THEN THE VALUE BMISS (10E10) IS       
!   RETURNED.  NOTE THAT, UNLIKE FOR SUBROUTINE STRNUM, THE INPUT       
!   STRING MAY CONTAIN A LEADING SIGN CHARACTER (E.G. '+', '-').        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- RENAMED THIS FUNCTION FROM "VAL$" TO "VALX" 
!                           TO REMOVE THE POSSIBILITY OF THE "$" SYMBOL 
!                           CAUSING PROBLEMS ON OTHER PLATFORMS         
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2 
!                                                                       
! USAGE:    VALX (STR)                                                  
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING CONTAINING ENCODED REAL VALUE    
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     VALX     - REAL: DECODED VALUE                                    
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT2    RJUST                          
!    THIS ROUTINE IS CALLED BY: GETTBH   NEMTBB   UPFTBV                
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER(99) BSTR 
      CHARACTER(8) FMT 
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      LENS = LEN (STR) 
      IF (LENS.GT.99) GOTO 900 
      BSTR (1:LENS) = STR 
      RJ = RJUST (BSTR (1:LENS) ) 
      WRITE (FMT, '(''(F'',I2,''.0)'')') LENS 
      VALX = BMISS 
      READ (BSTR, FMT, ERR = 800) VAL 
      VALX = VAL 
      GOTO 100 
  800 IF (IPRT.GE.0) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: VALX - ERROR READING STRING ', BSTR (1:LENS) 
      PRINT * , '                RETURN WITH VALX = MISSING (10E10)' 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR1, '("STRING IS: ",A)') STR 
      WRITE (BORT_STR2, '("BUFRLIB: VALX - STRING LENGTH EXCEEDS LIMIT '&
     &//' OF 99 CHARACTERS")')                                          
      CALL BORT2 (BORT_STR1, BORT_STR2) 
      END FUNCTION VALX                             
