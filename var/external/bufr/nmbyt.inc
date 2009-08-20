      FUNCTION NMBYT (LUNIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NMBYT                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2000-09-19           
!                                                                       
! ABSTRACT: THIS FUNCTION CALLS BUFR ARCHIVE LIBRARY SUBROUTINE IUPVS01 
!   TO GET THE LENGTH (IN BYTES) OF THE BUFR MESSAGE OPEN FOR INPUT VIA 
!   THE MOST RECENT CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READMG OR   
!   EQUIVALENT.  IT IS CONSIDERED OBSOLETE AND MAY BE REMOVED FROM THE  
!   BUFR ARCHIVE LIBRARY IN A FUTURE VERSION.  USERS SHOULD MIGRATE TO  
!   THE DIRECT USE OF IUPVS01 (AS SHOWN BELOW).                         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2000-09-19  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-08-18  J. ATOR    -- IMPROVED DOCUMENTATION; MAXIMUM MESSAGE     
!                           LENGTH INCREASED FROM 20,000 TO 50,000      
!                           BYTES                                       
! 2005-11-29  J. ATOR    -- USE IUPVS01 AND MARK AS OBSOLETE            
!                                                                       
! USAGE:    NMBYT (LUNIT)                                               
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NMBYT    - INTEGER: BUFR MESSAGE LENGTH (BYTES)                   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        IUPVS01                                 
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
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
  101 FORMAT(' BUFRLIB: NMBYT - THIS FUNCTION IS NOW OBSOLETE; ',       &
     & 'USE FUNCTION IUPVS01 INSTEAD')                                  
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT * 
         ENDIF 
         IFIRST = 1 
      ENDIF 
                                                                        
      NMBYT = IUPVS01 (LUNIT, 'LENM') 
                                                                        
      RETURN 
      END FUNCTION NMBYT                            
