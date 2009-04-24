      SUBROUTINE JSTCHR (STR, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    JSTCHR                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE LEFT-JUSTIFIES (I.E. REMOVES ALL LEADING    
!   BLANKS FROM) A CHARACTER STRING.                                    
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
!                           ABNORMALLY                                  
! 2007-01-19  J. ATOR    -- RESTRUCTURED AND ADDED IRET ARGUMENT        
!                                                                       
! USAGE:    CALL JSTCHR (STR, IRET)                                     
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING                                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR      - CHARACTER*(*): COPY OF INPUT STR WITH LEADING BLANKS   
!                REMOVED                                                
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = input string was empty (i.e. all blanks)    
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: ELEMDX   IGETFXY  SNTBBE                
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (STR.EQ.' ') THEN 
      IRET = - 1 
      ELSE 
      IRET = 0 
      LSTR = LEN (STR) 
      DO WHILE (STR (1:1) .EQ.' ') 
      STR = STR (2:LSTR) 
      ENDDO 
      ENDIF 
                                                                        
      RETURN 
      END SUBROUTINE JSTCHR                         
