      SUBROUTINE COPYSB (LUNIN, LUNOT, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    COPYSB                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2005-09-16           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES A PACKED DATA SUBSET, INTACT, FROM   
!   LOGICAL UNIT LUNIN, OPENED FOR INPUT VIA A PREVIOUS CALL TO BUFR    
!   ARCHIVE LIBRARY SUBROUTINE OPENBF, TO LOGICAL UNIT LUNOT, OPENED    
!   FOR OUTPUT VIA A PREVIOUS CALL TO OPENBF.  THE BUFR MESSAGE MUST    
!   HAVE BEEN PREVIOUSLY READ FROM UNIT LUNIT USING BUFR ARCHIVE        
!   LIBRARY SUBROUTINE READMG OR READERME AND MAY BE EITHER COMPRESSED  
!   OR UNCOMPRESSED.  ALSO, BUFR ARCHIVE LIBRARY SUBROUTINE OPENMG OR   
!   OPENMB MUST HAVE BEEN PREVIOUSLY CALLED TO OPEN AND INITIALIZE A    
!   BUFR MESSAGE WITHIN MEMORY FOR UNIT LUNOT.  EACH CALL TO COPYSB     
!   ADVANCES THE POINTER TO THE BEGINNING OF THE NEXT SUBSET IN BOTH    
!   THE INPUT AND OUTPUT FILES, UNLESS INPUT PARAMETER LUNOT IS .LE.    
!   ZERO, IN WHICH CASE THE OUTPUT POINTER IS NOT ADVANCED. THE         
!   COMPRESSION STATUS OF THE OUTPUT SUBSET/BUFR MESSAGE WILL ALWAYS    
!   MATCH THAT OF THE INPUT SUBSET/BUFR MESSAGE {I.E., IF INPUT MESSAGE 
!   IS UNCOMPRESSED(COMPRESSED) OUTPUT MESSAGE WILL BE UNCOMPRESSED     
!   (COMPRESSED)}.                                                      
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
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-09-16  J. WOOLLEN -- NOW WRITES OUT COMPRESSED SUBSET/MESSAGE IF 
!                           INPUT SUBSET/MESSAGE IS COMPRESSED (BEFORE  
!                           COULD ONLY WRITE OUT UNCOMPRESSED SUBSET/   
!                           MESSAGE REGARDLESS OF COMPRESSION STATUS OF 
!                           INPUT SUBSET/MESSAGE)                       
!                                                                       
! USAGE:    CALL COPYSB  ( LUNIN, LUNOT, IRET )                         
!   INPUT ARGUMENT LIST:                                                
!     LUNIN    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR INPUT BUFR    
!                FILE                                                   
!     LUNOT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR OUTPUT BUFR   
!                FILE                                                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = there are no more subsets in the input      
!                           BUFR message                                
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CMPMSG   CPYUPD   MESGBC       
!                               READSB   STATUS   UFBCPY   UPB          
!                               WRITSB                                  
!    THIS ROUTINE IS CALLED BY: ICOPYSB                                 
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
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
                                                                        
      CHARACTER(128) BORT_STR 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IRET = 0 
                                                                        
!  CHECK THE FILE STATUSES                                              
!  -----------------------                                              
                                                                        
      CALL STATUS (LUNIN, LIN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
      IF (LUNOT.GT.0) THEN 
         CALL STATUS (LUNOT, LOT, IL, IM) 
         IF (IL.EQ.0) GOTO 903 
         IF (IL.LT.0) GOTO 904 
         IF (IM.EQ.0) GOTO 905 
         IF (INODE (LIN) .NE.INODE (LOT) ) GOTO 906 
      ENDIF 
                                                                        
!  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE                        
!  ---------------------------------------------                        
                                                                        
      IF (NSUB (LIN) .EQ.MSUB (LIN) ) THEN 
         IRET = - 1 
         GOTO 100 
      ENDIF 
                                                                        
!  CHECK COMPRESSION STATUS OF INPUT MESSAGE, OUTPUT MESSAGE WILL MATCH 
!  -------------------------------------------------------------------- 
                                                                        
      CALL MESGBC ( - LUNIN, MEST, ICMP) 
                                                                        
      IF (ICMP.EQ.1) THEN 
                                                                        
!  -------------------------------------------------------              
!  THIS BRANCH IS FOR COMPRESSED INPUT/OUTPUT MESSAGES                  
!  -------------------------------------------------------              
!  READ IN AND UNCOMPRESS SUBSET, THEN COPY IT TO COMPRESSED OUTPUT MSG 
!  -------------------------------------------------------------------- 
                                                                        
         CALL READSB (LUNIN, IRET) 
         IF (LUNOT.GT.0) THEN 
            CALL UFBCPY (LUNIN, LUNOT) 
            CALL CMPMSG ('Y') 
            CALL WRITSB (LUNOT) 
            CALL CMPMSG ('N') 
         ENDIF 
         GOTO 100 
      ELSEIF (ICMP.EQ.0) THEN 
                                                                        
!  -------------------------------------------------------              
!  THIS BRANCH IS FOR UNCOMPRESSED INPUT/OUTPUT MESSAGES                
!  -------------------------------------------------------              
!  COPY THE SUBSET TO THE OUTPUT MESSAGE AND/OR RESET THE POINTERS      
!  ---------------------------------------------------------------      
                                                                        
         IBIT = (MBYT (LIN) ) * 8 
         CALL UPB (NBYT, 16, MBAY (1, LIN), IBIT) 
         IF (LUNOT.GT.0) CALL CPYUPD (LUNOT, LIN, LOT, NBYT) 
         MBYT (LIN) = MBYT (LIN) + NBYT 
         NSUB (LIN) = NSUB (LIN) + 1 
      ELSE 
         GOTO 907 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: COPYSB - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: COPYSB - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  904 CALL BORT ('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  905 CALL BORT ('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN OUTPUT '//&
      'BUFR FILE, NONE ARE')                                            
  906 CALL BORT ('BUFRLIB: COPYSB - INPUT AND OUTPUT BUFR FILES MUST '//&
      'HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')         
  907 WRITE (BORT_STR, '("BUFRLIB: COPYSB - INVALID COMPRESSION '//     &
      'INDICATOR (ICMP=",I3," RETURNED FROM BUFR ARCHIVE LIBRARY '//    &
      'ROUTINE MESGBF")') ICMP                                          
      CALL BORT (BORT_STR) 
      END SUBROUTINE COPYSB                         
