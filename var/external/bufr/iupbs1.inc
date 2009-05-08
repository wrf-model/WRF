      FUNCTION IUPBS1 (MBAY, NBYT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IUPBS1                                                 
!   PRGMMR: ATOR             ORG: NP12       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS FUNCTION UNPACKS AND RETURNS THE BINARY INTEGER WORD   
!   CONTAINED WITHIN BYTE NBYT OF SECTION 1 (OR BYTE 8 OF SECTION 0,    
!   IF NBYT = 0) OF THE BUFR MESSAGE STORED IN ARRAY MBAY.  THE START   
!   OF THE BUFR MESSAGE (I.E., THE STRING "BUFR") MUST BE ALIGNED ON    
!   THE FIRST FOUR BYTES OF MBAY.  NOTE THAT THIS FUNCTION IS CONSIDERED
!   OBSOLETE AND MAY BE REMOVED FROM THE BUFR ARCHIVE LIBRARY IN A      
!   FUTURE VERSION; USERS SHOULD INSTEAD MIGRATE TO THE USE OF BUFR     
!   ARCHIVE LIBRARY FUNCTION IUPBS01.                                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. ATOR    -- ORIGINAL AUTHOR (WAS IN DECODER VERSION)    
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF                    
! 2004-08-18  J. ATOR    -- REMOVED IFIRST CHECK, SINCE WRDLEN NOW      
!                           KEEPS TRACK OF WHETHER IT HAS BEEN CALLED   
! 2005-11-29  J. ATOR    -- MARKED AS OBSOLETE                          
!                                                                       
! USAGE:    IUPBS1 (MBAY, NBYT)                                         
!   INPUT ARGUMENT LIST:                                                
!     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR    
!                MESSAGE                                                
!     NBYT     - INTEGER: BYTE TO UNPACK WITHIN SECTION 1 OF BUFR MSG   
!                       0 = UNPACK BYTE 8 OF SECTION 0                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IUPBS1   - INTEGER: UNPACKED INTEGER WORD                         
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        IUPB     WRDLEN                         
!    THIS ROUTINE IS CALLED BY: IUPVS1                                  
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      DIMENSION MBAY ( * ) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!     CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION   
!     ABOUT THE LOCAL MACHINE, JUST IN CASE SUBROUTINE OPENBF HASN'T    
!     BEEN CALLED YET.                                                  
                                                                        
      CALL WRDLEN 
                                                                        
!     NOTE THAT THERE ARE 8 BYTES WITHIN SECTION 0 THAT MUST BE SKIPPED.
                                                                        
      IUPBS1 = IUPB (MBAY, NBYT + 8, 8) 
                                                                        
      RETURN 
      END FUNCTION IUPBS1                           
