      SUBROUTINE READFT (LUNIT, SUBSET, JDATE, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    READFT                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1996-10-09           
!                                                                       
! ABSTRACT:  THIS SUBROUTINE CALLS BUFR ARCHIVE LIBRARY SUBROUTINE      
!   READMG IN FAULT-TOLERANT MODE.  IT IS CONSIDERED OBSOLETE AND MAY   
!   BE REMOVED FROM THE BUFR ARCHIVE LIBRARY IN A FUTURE VERSION.       
!   USERS SHOULD MIGRATE TO THE DIRECT USE OF READMG (AS SHOWN BELOW).  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1996-10-09  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K        
!                           COMPLIANT                                   
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI); MODIFIED WITH    
!                           SEMANTIC ADJUSTMENTS TO AMELIORATE COMPILER 
!                           COMPLAINTS FROM LINUX BOXES (INCREASES      
!                           PORTABILITY)                                
! 2000-09-19  J. WOOLLEN -- REMOVED MESSAGE DECODING LOGIC THAT HAD     
!                           BEEN REPLICATED IN THIS AND OTHER READ      
!                           ROUTINES AND CONSOLIDATED IT INTO A NEW     
!                           ROUTINE CKTABA, CALLED HERE, WHICH IS       
!                           ENHANCED TO ALLOW COMPRESSED AND STANDARD   
!                           BUFR MESSAGES TO BE READ; MAXIMUM MESSAGE   
!                           LENGTH INCREASED FROM 10,000 TO 20,000      
!                           BYTES                                       
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- MARKED AS OBSOLETE AND ADDED PRINT          
!                           NOTIFICATION                                
!                                                                       
! USAGE:    CALL READFT (LUNIT, SUBSET, JDATE, IRET)                    
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING READ                                             
!     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR    
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = there are no more BUFR messages in LUNIT or 
!                           I/O error reading BUFR message              
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        READMG                                  
!    THIS ROUTINE IS CALLED BY: IREADFT                                 
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER(8) SUBSET 
                                                                        
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
  101 FORMAT(' BUFRLIB: READFT - THIS SUBROUTINE IS NOW OBSOLETE; ',    &
     & 'USE SUBROUTINE READMG INSTEAD')                                 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT * 
         ENDIF 
         IFIRST = 1 
      ENDIF 
                                                                        
      CALL READMG (LUNIT * ( - 1), SUBSET, JDATE, IRET) 
                                                                        
      RETURN 
      END SUBROUTINE READFT                         
