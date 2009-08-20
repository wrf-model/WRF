      FUNCTION IUPVS01 (LUNIT, S01MNEM) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IUPVS01                                                
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29           
!                                                                       
! ABSTRACT: THIS FUNCTION UNPACKS AND RETURNS A SPECIFIED INTEGER VALUE 
!   FROM SECTION 0 OR SECTION 1 OF THE LAST BUFR MESSAGE THAT WAS READ  
!   FROM LOGICAL UNIT NUMBER LUNIT VIA BUFR ARCHIVE LIBRARY SUBROUTINE  
!   READMG, READERME OR EQUIVALENT.  IT WILL WORK ON ANY MESSAGE ENCODED
!   USING BUFR EDITION 2, 3 OR 4, AND THE VALUE TO BE UNPACKED IS       
!   SPECIFIED VIA THE MNEMONIC S01MNEM (SEE THE DOCBLOCK OF BUFR ARCHIVE
!   LIBRARY FUNCTION IUPBS01 FOR A LISTING OF POSSIBLE VALUES FOR       
!   S01MNEM).  NOTE THAT THIS FUNCTION IS SIMILAR TO BUFR ARCHIVE       
!   LIBRARY FUNCTION IUPBS01 EXCEPT THAT IT OPERATES ON A BUFR MESSAGE  
!   THAT HAS ALREADY BEEN READ INTO THE INTERNAL BUFR ARCHIVE LIBRARY   
!   ARRAYS (VIA A PREVIOUS CALL TO READMG, READERME, ETC.) RATHER THAN  
!   ON A BUFR MESSAGE PASSED DIRECTLY INTO THE FUNCTION VIA A MEMORY    
!   ARRAY.                                                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    IUPVS01 (LUNIT, S01MNEM)                                    
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     S01MNEM  - CHARACTER*(*): MNEMONIC SPECIFYING VALUE TO BE         
!                UNPACKED FROM SECTION 0 OR SECTION 1 OF BUFR MESSAGE   
!                  (SEE DOCBLOCK OF FUNCTION IUPBS01 FOR LISTING OF     
!                  POSSIBLE VALUES)                                     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IUPVS01   - INTEGER: UNPACKED INTEGER VALUE                       
!                  -1 = THE INPUT S01MNEM MNEMONIC WAS INVALID          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IUPBS01  STATUS                
!    THIS ROUTINE IS CALLED BY: NMBYT                                   
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
                                                                        
      CHARACTER ( * ) S01MNEM 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE FILE STATUS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUNIT, LUN, ILST, IMST) 
      IF (ILST.EQ.0) GOTO 900 
      IF (ILST.GT.0) GOTO 901 
      IF (IMST.EQ.0) GOTO 902 
                                                                        
!  UNPACK THE REQUESTED VALUE                                           
!  --------------------------                                           
                                                                        
      IUPVS01 = IUPBS01 (MBAY (1, LUN), S01MNEM) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS OPEN FOR '//    &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: IUPVS01 - A MESSAGE MUST BE OPEN IN INPUT '//&
      'BUFR FILE, NONE ARE')                                            
      END FUNCTION IUPVS01                          
