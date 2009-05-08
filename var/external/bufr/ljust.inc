      FUNCTION LJUST (STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    LJUST                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1998-07-08           
!                                                                       
! ABSTRACT: THIS FUNCTION REMOVES ALL LEADING BLANKS FROM A CHARACTER   
!   STRING (LEFT JUSTIFIES).  IT IS CONSIDERED OBSOLETE AND MAY BE      
!   REMOVED FROM THE BUFR ARCHIVE LIBRARY IN A FUTURE VERSION.  USERS   
!   SHOULD INSTEAD MIGRATE TO THE USE OF SUBROUTINE JSTCHR.             
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1998-07-08  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
! 2007-01-19  J. ATOR    -- MARKED AS OBSOLETE AND ADDED PRINT          
!                           NOTIFICATION                                
!                                                                       
! USAGE:    LJUST (STR)                                                 
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING TO BE LEFT-JUSTIFED              
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR      - CHARACTER*(*): LEFT-JUSTIFED STRING (UNLESS INPUT      
!                VALUE CONTAINED ALL BLANKS - THEN INPUT VALUE IS       
!                RETURNED WITH NO CHANGES)                              
!     LJUST    - INTEGER: ALWAYS RETURNED AS 0 (DUMMY)                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: None at this time                       
!                               Normally not called by any application  
!                               Programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
      DATA IFIRST / 0 / 
                                                                        
      SAVE IFIRST 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (IFIRST.EQ.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT 101 
  101 FORMAT(' BUFRLIB: LJUST - THIS FUNCTION IS NOW OBSOLETE; ',       &
     & 'USE SUBROUTINE JSTCHR INSTEAD')                                 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT * 
         ENDIF 
         IFIRST = 1 
      ENDIF 
                                                                        
      LJUST = 0 
                                                                        
      IF (STR.EQ.' ') GOTO 100 
                                                                        
      LSTR = LEN (STR) 
      DO I = 1, LSTR 
      DO WHILE (STR (I:I) .EQ.' '.AND.STR (I + 1:LSTR) .NE.' ') 
      STR (I:LSTR) = STR (I + 1:LSTR) 
      ENDDO 
      ENDDO 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END FUNCTION LJUST                            
