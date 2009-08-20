      SUBROUTINE MESGBF (LUNIT, MESGTYP) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    MESGBF                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS AND EXAMINES SECTION 1 OF MESSAGES    
!  IN A BUFR FILE IN SEQUENCE UNTIL IT FINDS THE FIRST MESSAGE THAT     
!  IS NOT A BUFR TABLE (DICTIONARY) (I.E., NOT MESSAGE TYPE 11).  IT    
!  THEN RETURNS THE MESSAGE TYPE FOR THIS FIRST NON-DICTIONARY MESSAGE. 
!  THE BUFR FILE SHOULD NOT BE OPEN VIA BUFR ARCHIVE LIBRARY SUBROUTINE 
!  OPENBF PRIOR TO CALLING THIS SUBROUTINE HOWEVER THE BUFR FILE MUST   
!  BE CONNECTED TO UNIT LUNIT.  THIS SUBROUTINE IS IDENTICAL TO BUFR    
!  ARCHIVE LIBRARY SUBROUTINE MESGBC EXCEPT MESGBC RETURNS THE MESSAGE  
!  TYPE FOR THE FIRST NON-DICTIONARY MESSAGE THAT ACTUALLY CONTAINS     
!  REPORT DATA (WHEREAS MESGBF WOULD RETURN THE REPORT TYPE OF A DUMMY  
!  MESSAGE CONTAINING THE CENTER TIME FOR DUMP FILES), AND MESGBC ALSO  
!  INDICATES WHETHER OR NOT THE FIRST REPORT DATA MESSAGE CONTAINS      
!  REPORT DATA IS BUFR COMPRESSED.  (MESGBC ALSO HAS AN OPTION TO       
!  OPERATE ON THE CURRENT BUFR STORED IN MEMORY, SOMETHING MESGBF       
!  CANNOT DO.)                                                          
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- USE IUPBS01 AND RDMSGW                      
!                                                                       
! USAGE:    CALL MESGBF (LUNIT, MESGTYP)                                
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     MESGTYP  - INTEGER: BUFR MESSAGE TYPE FOR FIRST NON-DICTIONARY    
!                MESSAGE                                                
!                      -1 = no messages read or error                   
!                      11 = if only BUFR table messages in BUFR file    
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        IUPBS01  RDMSGW   WRDLEN                
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      DIMENSION MBAY (MXMSGLD4) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      MESGTYP = - 1 
                                                                        
!  SINCE OPENBF HAS NOT YET BEEN CALLED, MUST CALL WRDLEN TO GET        
!  MACHINE INFO NEEDED LATER                                            
!  -------------------------------------------------------------        
                                                                        
      CALL WRDLEN 
                                                                        
!  READ PAST ANY BUFR TABLES AND RETURN THE FIRST MESSAGE TYPE FOUND    
!  -----------------------------------------------------------------    
                                                                        
      REWIND LUNIT 
                                                                        
    1 CALL RDMSGW (LUNIT, MBAY, IER) 
      IF (IER.LT.0) GOTO 100 
                                                                        
      MESGTYP = IUPBS01 (MBAY, 'MTYP') 
      IF (MESGTYP.EQ.11) GOTO 1 
                                                                        
      REWIND LUNIT 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE MESGBF                         
