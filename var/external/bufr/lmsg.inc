      FUNCTION LMSG (SEC0) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    LMSG                                                   
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: GIVEN A CHARACTER STRING CONTAINING SECTION ZERO FROM A     
!   BUFR MESSAGE, THIS FUNCTION DETERMINES A COUNT OF MACHINE WORDS     
!   (I.E. INTEGER ARRAY MEMBERS) THAT WILL HOLD THE ENTIRE MESSAGE.     
!   NOTE THAT THIS COUNT MAY BE GREATER THAN THE MINIMUM NUMBER         
!   OF WORDS REQUIRED TO HOLD THE MESSAGE.                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
! 2004-08-18  J. ATOR    -- IMPROVED DOCUMENTATION                      
! 2005-11-29  J. ATOR    -- USE NMWRD                                   
!                                                                       
! USAGE:    LMSG (SEC0)                                                 
!   INPUT ARGUMENT LIST:                                                
!     SEC0     - CHARACTER*8: PACKED BUFR MESSAGE SECTION ZERO          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     LMSG     - INTEGER: BUFR MESSAGE LENGTH (IN MACHINE WORDS)        
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        NMWRD                                   
!    THIS ROUTINE IS CALLED BY: RDMSGB   RDMSGW   READERME              
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER(8) SEC0, CSEC0 
      DIMENSION MSEC0 (2) 
                                                                        
      EQUIVALENCE (MSEC0, CSEC0) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      CSEC0 = SEC0 
      LMSG = NMWRD (MSEC0) 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
      RETURN 
      END FUNCTION LMSG                             
