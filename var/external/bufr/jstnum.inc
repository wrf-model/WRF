      SUBROUTINE JSTNUM (STR, SIGN, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    JSTNUM                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE REMOVES ALL LEADING BLANKS FROM A CHARACTER 
!   STRING CONTAINING AN ENCODED INTEGER VALUE.  IF THE VALUE HAS A     
!   LEADING SIGN CHARACTER ('+' OR '-'), THEN THIS CHARACTER IS ALSO    
!   REMOVED AND IS RETURNED SEPARATELY WITHIN SIGN.  IF THE RESULTANT   
!   STRING CONTAINS ANY NON-NUMERIC CHARACTERS, THAN AN APPROPRIATE     
!   CALL IS MADE TO TO BUFR ARCHIVE LIBRARY SUBROUTINE BORT.            
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN JSTIFY)     
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT" (IN PARENT ROUTINE JSTIFY)   
! 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE     
!                           PORTABILITY TO OTHER PLATFORMS (JSTIFY WAS  
!                           THEN REMOVED BECAUSE IT WAS JUST A DUMMY    
!                           ROUTINE WITH ENTRIES)                       
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY OR UNUSUAL THINGS HAPPEN         
!                                                                       
! USAGE:    CALL JSTNUM (STR, SIGN, IRET)                               
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING CONTAINING ENCODED INTEGER VALUE 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR      - CHARACTER*(*): COPY OF INPUT STR WITH LEADING BLANKS   
!                AND SIGN CHARACTER REMOVED                             
!     SIGN     - CHARACTER*1: SIGN OF ENCODED INTEGER VALUE:            
!                     '+' = positive value                              
!                     '-' = negative value                              
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = encoded value within STR was not an integer 
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     STRNUM                         
!    THIS ROUTINE IS CALLED BY: ELEMDX                                  
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(1) SIGN 
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
      IRET = 0 
                                                                        
      IF (STR.EQ.' ') GOTO 900 
                                                                        
      LSTR = LEN (STR) 
    2 IF (STR (1:1) .EQ.' ') THEN 
      STR = STR (2:LSTR) 
      GOTO 2 
      ENDIF 
      IF (STR (1:1) .EQ.'+') THEN 
      STR = STR (2:LSTR) 
      SIGN = '+' 
      ELSEIF (STR (1:1) .EQ.'-') THEN 
      STR = STR (2:LSTR) 
      SIGN = '-' 
      ELSE 
      SIGN = '+' 
      ENDIF 
                                                                        
      CALL STRNUM (STR, NUM) 
      IF (NUM.LT.0) THEN 
      IF (IPRT.GE.0) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: JSTNUM: ENCODED VALUE WITHIN RESULTANT ',     &
      'CHARACTER STRING (', STR, ') IS NOT AN INTEGER - RETURN WITH ',  &
      'IRET = -1'                                                       
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * 
      ENDIF 
      IRET = - 1 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: JSTNUM - INPUT BLANK CHARACTER STRING NOT '//&
      'ALLOWED')                                                        
      END SUBROUTINE JSTNUM                         
