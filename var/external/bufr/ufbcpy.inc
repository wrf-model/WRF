      SUBROUTINE UFBCPY (LUBIN, LUBOT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBCPY                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES A COMPLETE SUBSET BUFFER, UNPACKED   
!   INTO INTERNAL MEMORY FROM LOGICAL UNIT LUBIN BY A PREVIOUS CALL     
!   TO BUFR ARCHIVE LIBRARY SUBROUTINE READSB OR READNS, TO             
!   LOGICAL UNIT LUBOT.  BUFR ARCHIVE LIBRARY SUBROUTINE OPENMG OR      
!   OPENMB MUST HAVE BEEN PREVIOUSLY CALLED TO OPEN AND INITIALIZE A    
!   BUFR MESSAGE WITHIN MEMORY FOR LOGICAL UNIT LUBOU.  BOTH FILES MUST 
!   HAVE BEEN OPENED TO THE INTERFACE (VIA A CALL TO BUFR ARCHIVE       
!   LIBRARY SUBROUTINE OPENBF) WITH IDENTICAL BUFR TABLES.              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
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
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
!                                                                       
! USAGE:    CALL UFBCPY (LUBIN, LUBOT)                                  
!   INPUT ARGUMENT LIST:                                                
!     LUBIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR INPUT BUFR    
!                FILE                                                   
!     LUBOT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR OUTPUT BUFR   
!                FILE                                                   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     STATUS                         
!    THIS ROUTINE IS CALLED BY: COPYSB                                  
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
!     REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
!  CHECK THE FILE STATUSES AND I-NODE                                   
!  ----------------------------------                                   
                                                                        
      CALL STATUS (LUBIN, LUI, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (INODE (LUI) .NE.INV (1, LUI) ) GOTO 903 
                                                                        
      CALL STATUS (LUBOT, LUO, IL, IM) 
      IF (IL.EQ.0) GOTO 904 
      IF (IL.LT.0) GOTO 905 
      IF (IM.EQ.0) GOTO 906 
                                                                        
      IF (INODE (LUI) .NE.INODE (LUO) ) GOTO 907 
                                                                        
!  EVERYTHING OKAY COPY USER ARRAY FROM LUI TO LUO                      
!  -----------------------------------------------                      
                                                                        
      NVAL (LUO) = NVAL (LUI) 
                                                                        
      DO N = 1, NVAL (LUI) 
      INV (N, LUO) = INV (N, LUI) 
      VAL (N, LUO) = VAL (N, LUI) 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: UFBCPY - INPUT BUFR FILE IS CLOSED, IT MUST'/&
     &/' BE OPEN FOR INPUT')                                            
  901 CALL BORT ('BUFRLIB: UFBCPY - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: UFBCPY - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: UFBCPY - LOCATION OF INTERNAL TABLE FOR '//  &
      'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//     &
      'INTERNAL SUBSET ARRAY')                                          
  904 CALL BORT ('BUFRLIB: UFBCPY - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  905 CALL BORT ('BUFRLIB: UFBCPY - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  906 CALL BORT ('BUFRLIB: UFBCPY - A MESSAGE MUST BE OPEN IN OUTPUT '//&
      'BUFR FILE, NONE ARE')                                            
  907 CALL BORT ('BUFRLIB: UFBCPY - INPUT AND OUTPUT BUFR FILES MUST '//&
      'HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')         
      END SUBROUTINE UFBCPY                         
