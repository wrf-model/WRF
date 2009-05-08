      FUNCTION IUPVS1 (LUNIT, IL) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IUPVS1                                                 
!   PRGMMR: ATOR             ORG: NP12       DATE: 2004-08-18           
!                                                                       
! ABSTRACT: THIS FUNCTION UNPACKS AND RETURNS THE BINARY INTEGER WORD   
!   CONTAINED WITHIN BYTE IL OF SECTION 1 (OR BYTE 8 OF SECTION 0, IF   
!   IL = 0) OF THE LAST BUFR MESSAGE THAT WAS READ FROM LOGICAL UNIT    
!   NUMBER LUNIT VIA BUFR ARCHIVE LIBRARY SUBROUTINE READMG, READERME,  
!   READIBM OR EQUIVALENT.  THIS FUNCTION IS SIMILAR TO BUFR ARCHIVE    
!   LIBRARY FUNCTION IUPBS1 EXCEPT THAT IT OPERATES ON A BUFR MESSAGE   
!   THAT HAS ALREADY BEEN READ INTO THE INTERNAL BUFR ARCHIVE LIBRARY   
!   ARRAYS (VIA A PREVIOUS CALL TO READMG, READERME, READIBM, ETC.)     
!   RATHER THAN ON A BUFR MESSAGE PASSED DIRECTLY INTO THE FUNCTION     
!   VIA A MEMORY ARRAY.  NOTE THAT THIS FUNCTION IS CONSIDERED          
!   OBSOLETE AND MAY BE REMOVED FROM THE BUFR ARCHIVE LIBRARY IN A      
!   FUTURE VERSION; USERS SHOULD INSTEAD MIGRATE TO THE USE OF BUFR     
!   ARCHIVE LIBRARY FUNCTION IUPVS01.                                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2004-08-18  J. ATOR    -- ORIGINAL AUTHOR                             
! 2005-11-29  J. ATOR    -- MARKED AS OBSOLETE                          
!                                                                       
! USAGE:    IUPVS1 (LUNIT, IL)                                          
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     IL       - INTEGER: BYTE TO UNPACK WITHIN SECTION 1 OF BUFR MSG   
!                       0 = UNPACK BYTE 8 OF SECTION 0                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IUPVS1   - INTEGER: UNPACKED INTEGER WORD                         
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IUPBS1   STATUS                
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
                                                                        
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE FILE STATUS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUNIT, LUN, ILST, IMST) 
      IF (ILST.EQ.0) GOTO 900 
      IF (ILST.GT.0) GOTO 901 
      IF (IMST.EQ.0) GOTO 902 
                                                                        
!  UNPACK THE REQUESTED BYTE                                            
!  -------------------------                                            
                                                                        
      IUPVS1 = IUPBS1 (MBAY (1, LUN), IL) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: IUPVS1 - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: IUPVS1 - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: IUPVS1 - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
      END FUNCTION IUPVS1                           
