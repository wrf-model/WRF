      SUBROUTINE TRYBUMP (LUNIT, LUN, USR, I1, I2, IO, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    TRYBUMP (docblock incomplete)                          
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE CHECKS THE FIRST NODE ASSOCIATED WITH A     
!   CHARACTER STRING (PARSED INTO ARRAYS IN COMMON BLOCK /USRSTR/) IN   
!   ORDER TO DETERMINE IF IT REPRESENTS A DELAYED REPLICATION SEQUENCE. 
!   IF SO, THEN THE DELAYED REPLICATION SEQUENCE IS INITIALIZED, IT IS  
!   "BUMPED" TO THE VALUE OF INPUT ARGUMENT I2, AND AN ATTEMPT IS MADE  
!   TO READ OR WRITE SPECIFIED VALUES TO THE CURRENT BUFR DATA SET IN   
!   UNIT LUNIT.                                                         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- IMPROVED MACHINE PORTABILITY                
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
!                           HISTORY) (INCOMPLETE); OUTPUTS MORE         
!                           COMPLETE DIAGNOSTIC INFO WHEN ROUTINE       
!                           TERMINATES ABNORMALLY                       
!                                                                       
! USAGE:    CALL TRYBUMP (LUNIT, LUN, USR, I1, I2, IO, IRET)            
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                (SEE REMARKS)                                          
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT) 
!     USR      - REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES READ   
!                FROM OR WRITTEN TO DATA SUBSET                         
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR              
!     I2       - INTEGER: IF READING, LENGTH OF SECOND DIMENSION OF     
!                USR; IF WRITING, NUMBER OF "LEVELS" OF DATA VALUES TO  
!                BE WRITTEN TO DATA SUBSET                              
!     IO       - INTEGER: STATUS INDICATOR FOR BUFR FILE ASSOCIATED     
!                WITH LUNIT:                                            
!                       0 = input file                                  
!                       1 = output file                                 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IRET     - INTEGER: NUMBER OF "LEVELS" OF DATA VALUES READ FROM   
!                OR WRITTEN TO DATA SUBSET (IF READING SHOULD BE NO     
!                LARGER THAN I2, IF WRITING SHOULD BE SAME AS I2)       
!                      -1 = ....                                        
!                                                                       
! REMARKS:                                                              
!    ARGUMENT LUNIT IS NOT REFERENCED IN THIS SUBROUTINE.  IT IS LEFT   
!    HERE IN CASE AN APPLICATION PROGRAM CALLS THIS SUBROUTINE.         
!                                                                       
!    THIS ROUTINE CALLS:        BORT     INVWIN   LSTJPB   UFBRW        
!                               USRTPL                                  
!    THIS ROUTINE IS CALLED BY: UFBINT   UFBOVR                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
      NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), IVLS (10),    &
      KONS (10)                                                         
                                                                        
      REAL(8) USR (I1, I2), VAL 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  SEE IF THERE IS A DELAYED REPLICATION GROUP INVOLVED                 
!  ----------------------------------------------------                 
                                                                        
      NDRP = LSTJPB (NODS (1) , LUN, 'DRP') 
      IF (NDRP.LE.0) GOTO 100 
                                                                        
!  IF SO, CLEAN IT OUT AND BUMP IT TO I2                                
!  -------------------------------------                                
                                                                        
      INVN = INVWIN (NDRP, LUN, 1, NVAL (LUN) ) 
      VAL (INVN, LUN) = 0 
      JNVN = INVN + 1 
      DO WHILE (NINT (VAL (JNVN, LUN) ) .GT.0) 
      JNVN = JNVN + NINT (VAL (JNVN, LUN) ) 
      ENDDO 
      DO KNVN = 1, NVAL (LUN) - JNVN + 1 
      INV (INVN + KNVN, LUN) = INV (JNVN + KNVN - 1, LUN) 
      VAL (INVN + KNVN, LUN) = VAL (JNVN + KNVN - 1, LUN) 
      ENDDO 
      NVAL (LUN) = NVAL (LUN) - (JNVN - INVN - 1) 
      CALL USRTPL (LUN, INVN, I2) 
                                                                        
!  FINALLY, CALL THE MNEMONIC READER/WRITER                             
!  ----------------------------------------                             
                                                                        
      CALL UFBRW (LUN, USR, I1, I2, IO, IRET) 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE TRYBUMP                        
