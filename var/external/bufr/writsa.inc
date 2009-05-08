      SUBROUTINE WRITSA (LUNXX, MSGT, MSGL) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    WRITSA                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE SHOULD ONLY BE CALLED WHEN LOGICAL UNIT     
!   ABS(LUNXX) HAS BEEN OPENED FOR OUTPUT OPERATIONS.                   
!                                                                       
!   WHEN LUNXX IS GREATER THAN ZERO, IT PACKS UP THE CURRENT SUBSET     
!   WITHIN MEMORY AND THEN TRIES TO ADD IT TO THE BUFR MESSAGE THAT IS  
!   CURRENTLY OPEN WITHIN MEMORY FOR ABS(LUNXX).  THE DETERMINATION AS  
!   TO WHETHER OR NOT THE SUBSET CAN BE ADDED TO THE MESSAGE IS MADE    
!   VIA AN INTERNAL CALL TO ONE OF THE BUFR ARCHIVE LIBRARY SUBROUTINES 
!   WRCMPS OR MSGUPD, DEPENDING UPON WHETHER OR NOT THE MESSAGE IS      
!   COMPRESSED.  IF IT TURNS OUT THAT THE SUBSET CANNOT BE ADDED TO THE 
!   CURRENTLY OPEN MESSAGE, THEN THAT MESSAGE IS FLUSHED TO ABS(LUNXX)  
!   AND A NEW ONE IS CREATED IN ORDER TO HOLD THE SUBSET.  AS LONG AS   
!   LUNXX IS GREATER THAN ZERO, WRITSA FUNCTIONS EXACTLY LIKE BUFR      
!   ARCHIVE LIBRARY SUBROUTINE WRITSB, EXCEPT THAT WRITSA ALSO RETURNS  
!   A COPY OF EACH COMPLETED BUFR MESSAGE TO THE APPLICATION PROGRAM    
!   IN THE FIRST MSGL WORDS OF ARRAY MSGT.                              
!                                                                       
!   ALTERNATIVELY, WHEN LUNXX IS LESS THAN ZERO, THIS IS A SIGNAL TO    
!   FORCE ANY CURRENT MESSAGE IN MEMORY TO BE FLUSHED TO ABS(LUNXX) AND 
!   RETURNED IN ARRAY MSGT.  IN SUCH CASES, ANY CURRENT SUBSET IN MEMORY
!   IS IGNORED.  THIS OPTION IS NECESSARY BECAUSE ANY MESSAGE RETURNED  
!   IN MSGT FROM A CALL TO THIS ROUTINE NEVER CONTAINS THE ACTUAL SUBSET
!   THAT WAS PACKED UP AND STORED DURING THE SAME CALL TO THIS ROUTINE. 
!   THEREFORE, THE ONLY WAY TO ENSURE THAT EVERY LAST BUFR SUBSET IS    
!   RETURNED WITHIN A BUFR MESSAGE IN MSGT BEFORE, E.G., EXITING THE    
!   APPLICATION PROGRAM, IS TO DO ONE FINAL CALL TO THIS ROUTINE WITH   
!   LUNXX LESS THAN ZERO IN ORDER TO FORCIBLY FLUSH OUT AND RETURN ONE  
!   FINAL BUFR MESSAGE.                                                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-08-18  J. ATOR    -- ADD POST-MSGUPD CHECK FOR AND RETURN OF     
!                           MESSAGE WITHIN MSGT IN ORDER TO PREVENT     
!                           LOSS OF MESSAGE IN CERTAIN SITUATIONS;      
!                           MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-03-09  J. ATOR    -- ADDED CAPABILITY FOR COMPRESSED MESSAGES    
!                                                                       
! USAGE:    CALL WRITSA (LUNXX, MSGT, MSGL)                             
!   INPUT ARGUMENT LIST:                                                
!     LUNXX    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER 
!                FOR BUFR FILE {IF LUNXX IS LESS THAN ZERO, THEN ANY    
!                CURRENT MESSAGE IN MEMORY WILL BE FORCIBLY FLUSHED TO  
!                ABS(LUNXX) AND TO ARRAY MSGT}                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     MSGT     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR    
!                MESSAGE (FIRST MSGL WORDS FILLED)                      
!     MSGL     - INTEGER: NUMBER OF WORDS FILLED IN MSGT                
!                       0 = no message was returned                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CLOSMG   MSGUPD   STATUS       
!                               WRCMPS   WRTREE                         
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
                                                                        
      COMMON / BUFRMG / MSGLEN, MSGTXT (MXMSGLD4) 
      COMMON / MSGCMP / CCMF 
                                                                        
      CHARACTER(1) CCMF 
                                                                        
      DIMENSION MSGT ( * ) 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      LUNIT = ABS (LUNXX) 
                                                                        
!  CHECK THE FILE STATUS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.LT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
!  IF LUNXX < 0, FORCE MEMORY MSG TO BE WRITTEN (W/O ANY CURRENT SUBSET)
!  ---------------------------------------------------------------------
                                                                        
      IF (LUNXX.LT.0) CALL CLOSMG (LUNIT) 
                                                                        
!  IS THERE A COMPLETED BUFR MESSAGE TO BE RETURNED?                    
!  -------------------------------------------------                    
                                                                        
      IF (MSGLEN.GT.0) THEN 
         MSGL = MSGLEN 
         DO N = 1, MSGL 
         MSGT (N) = MSGTXT (N) 
         ENDDO 
         MSGLEN = 0 
      ELSE 
         MSGL = 0 
      ENDIF 
                                                                        
      IF (LUNXX.LT.0) GOTO 100 
                                                                        
!  PACK UP THE SUBSET AND PUT IT INTO THE MESSAGE                       
!  ----------------------------------------------                       
                                                                        
      CALL WRTREE (LUN) 
      IF (CCMF.EQ.'Y') THEN 
         CALL WRCMPS (LUNIT) 
      ELSE 
         CALL MSGUPD (LUNIT, LUN) 
      ENDIF 
                                                                        
!  IF THE JUST-COMPLETED CALL TO WRCMPS OR MSGUPD FOR THIS SUBSET CAUSED
!  A PREVIOUS MESSAGE TO BE FLUSHED TO ABS(LUNXX), THEN RETRIEVE AND    
!  RETURN THAT MESSAGE NOW.  OTHERWISE, WE RUN THE RISK THAT THE NEXT   
!  CALL TO OPENMB OR OPENMG MIGHT CAUSE A NEWER MESSAGE (WHICH WOULD    
!  CONTAIN THE CURRENT SUBSET!) TO BE FLUSHED AND THUS OVERWRITE THE    
!  PREVIOUS MESSAGE WITHIN ARRAY MSGTXT BEFORE WE HAD THE CHANCE TO     
!  RETRIEVE IT DURING THE NEXT CALL TO WRITSA!                          
                                                                        
!  NOTE ALSO THAT, IF THE MOST RECENT CALL TO OPENMB OR OPENMG HAD      
!  CAUSED A MESSAGE TO BE FLUSHED, IT WOULD HAVE DONE SO IN ORDER TO    
!  CREATE A NEW MESSAGE TO HOLD THE CURRENT SUBSET.  THUS, IN SUCH      
!  CASES, IT SHOULD NOT BE POSSIBLE THAT THE JUST-COMPLETED CALL TO     
!  WRCMPS OR MSGUPD (FOR THIS SAME SUBSET!) WOULD HAVE ALSO CAUSED A    
!  MESSAGE TO BE FLUSHED, AND THUS IT SHOULD NOT BE POSSIBLE TO HAVE    
!  TWO (2) SEPARATE BUFR MESSAGES RETURNED FROM ONE (1) CALL TO WRITSA! 
                                                                        
      IF (MSGLEN.GT.0) THEN 
         IF (MSGL.NE.0) GOTO 903 
         MSGL = MSGLEN 
         DO N = 1, MSGL 
         MSGT (N) = MSGTXT (N) 
         ENDDO 
         MSGLEN = 0 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  901 CALL BORT ('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  902 CALL BORT ('BUFRLIB: WRITSA - A MESSAGE MUST BE OPEN IN OUTPUT '//&
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: WRITSA - TWO BUFR MESSAGES WERE RETRIEVED '//&
      'BY ONE CALL TO THIS ROUTINE')                                    
      END SUBROUTINE WRITSA                         
