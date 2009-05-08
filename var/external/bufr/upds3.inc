      SUBROUTINE UPDS3 (MBAY, CDS3, NDS3) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UPDS3                                                  
!   PRGMMR: ATOR             ORG: NP12       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE UNPACKS AND RETURNS THE DESCRIPTORS         
!   CONTAINED WITHIN SECTION 3 OF A BUFR MESSAGE STORED IN ARRAY MBAY.  
!   THE START OF THE BUFR MESSAGE (I.E. THE STRING "BUFR") MUST BE      
!   ALIGNED ON THE FIRST FOUR BYTES OF MBAY.  NOTE ALSO THAT THIS       
!   SUBROUTINE DOES NOT RECURSIVELY RESOLVE SEQUENCE DESCRIPTORS THAT   
!   APPEAR WITHIN SECTION 3; RATHER, WHAT IS RETURNED IS THE EXACT LIST 
!   OF DESCRIPTORS AS IT APPEARS WITHIN SECTION 3.                      
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. ATOR    -- ORIGINAL AUTHOR (WAS IN DECODER VERSION)    
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF                    
! 2004-08-18  J. ATOR    -- REMOVED IFIRST CHECK, SINCE WRDLEN NOW      
!                           KEEPS TRACK OF WHETHER IT HAS BEEN CALLED   
! 2005-11-29  J. ATOR    -- USE GETLENS                                 
!                                                                       
! USAGE:    CALL UPDS3 (MBAY, CDS3, NDS3)                               
!   INPUT ARGUMENT LIST:                                                
!     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR    
!                MESSAGE                                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     CDS3     - CHARACTER*6: *-WORD ARRAY CONTAINING UNPACKED LIST OF  
!                DESCRIPTORS (FIRST NDS3 WORDS FILLED)                  
!     NDS3     - INTEGER: NUMBER OF DESCRIPTORS RETURNED                
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        ADN30    IUPB     GETLENS  WRDLEN       
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      DIMENSION MBAY ( * ) 
                                                                        
      CHARACTER(6) CDS3 ( * ), ADN30 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!     CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION   
!     ABOUT THE LOCAL MACHINE, JUST IN CASE SUBROUTINE OPENBF HASN'T    
!     BEEN CALLED YET.                                                  
                                                                        
      CALL WRDLEN 
                                                                        
!     SKIP TO THE BEGINNING OF SECTION 3.                               
                                                                        
      CALL GETLENS (MBAY, 3, LEN0, LEN1, LEN2, LEN3, L4, L5) 
      IPT = LEN0 + LEN1 + LEN2 
                                                                        
!     UNPACK THE SECTION 3 DESCRIPTORS.                                 
                                                                        
      NDS3 = 0 
      DO JJ = 8, (LEN3 - 1), 2 
      NDS3 = NDS3 + 1 
      CDS3 (NDS3) = ADN30 (IUPB (MBAY, IPT + JJ, 16), 6) 
      ENDDO 
                                                                        
      RETURN 
      END SUBROUTINE UPDS3                          
