      FUNCTION NWORDS (N, LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NWORDS (docblock incomplete)                           
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1996-10-09           
!                                                                       
! ABSTRACT: THIS FUNCTION ....                                          
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1996-10-09  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY) (INCOMPLETE)                       
!                                                                       
! USAGE:    NWORDS (N, LUN)                                             
!   INPUT ARGUMENT LIST:                                                
!     N        - INTEGER: BYTE COUNT INDEX FOR BUFR MESSAGE             
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NWORDS   - INTEGER: ....                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: INVMRG                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
!     REAL(8) VAL 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      NWORDS = 0 
                                                                        
      DO K = 1, NINT (VAL (N, LUN) ) 
      NWORDS = NWORDS + NINT (VAL (NWORDS + N + 1, LUN) ) 
      ENDDO 
                                                                        
      RETURN 
      END FUNCTION NWORDS                           
