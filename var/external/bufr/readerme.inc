      SUBROUTINE READERME (MESG, LUNIT, SUBSET, JDATE, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    READERME                                               
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1995-06-28           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS INFORMATION FROM A BUFR DATA MESSAGE  
!   ALREADY IN MEMORY, PASSED IN AS AN INPUT ARGUMENT.  IT IS SIMILAR   
!   TO BUFR ARCHIVE LIBRARY SUBROUTINE READMG EXCEPT, INSTEAD OF        
!   READING BUFR MESSAGES DIRECTLY FROM A BUFR FILE THAT IS PHYSICALLY  
!   STORED ON THE LOCAL SYSTEM AND INTERFACED TO THE SOFTWARE VIA A     
!   LOGICAL UNIT NUMBER, IT READS BUFR MESSAGES DIRECTLY FROM A MEMORY  
!   ARRAY WITHIN THE APPLICATION PROGRAM ITSELF.  THIS PROVIDES USERS   
!   WITH GREATER FLEXIBILITY FROM AN INPUT/OUTPUT PERSPECTIVE.          
!   READERME CAN BE USED IN ANY CONTEXT IN WHICH READMG MIGHT OTHERWISE 
!   BE USED.  IF THIS MESSAGE IS NOT A BUFR MESSAGE, THEN AN            
!   APPROPRIATE CALL IS MADE TO BUFR ARCHIVE LIBRARY SUBROUTINE BORT.   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1995-06-28  J. WOOLLEN -- ORIGINAL AUTHOR (FOR ERS DATA)              
! 1997-07-29  J. WOOLLEN -- MODIFIED TO PROCESS GOES SOUNDINGS FROM     
!                           NESDIS                                      
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K        
!                           COMPLIANT; IMPROVED MACHINE PORTABILITY     
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI); INCREASED THE    
!                           MAXIMUM NUMBER OF POSSIBLE DESCRIPTORS IN A 
!                           SUBSET FROM 1000 TO 3000                    
! 2000-09-19  J. WOOLLEN -- REMOVED MESSAGE DECODING LOGIC THAT HAD     
!                           BEEN REPLICATED IN THIS AND OTHER READ      
!                           ROUTINES AND CONSOLIDATED IT INTO A NEW     
!                           ROUTINE CKTABA, CALLED HERE, WHICH IS       
!                           ENHANCED TO ALLOW COMPRESSED AND STANDARD   
!                           BUFR MESSAGES TO BE READ (ROUTINE UNCMPS,   
!                           WHICH HAD BEEN CALLED BY THIS AND OTHER     
!                           ROUTINES IS NOW OBSOLETE AND HAS BEEN       
!                           REMOVED FROM THE BUFRLIB; MAXIMUM MESSAGE   
!                           LENGTH INCREASED FROM 10,000 TO 20,000      
!                           BYTES                                       
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-08-18  J. ATOR    -- MODIFIED 'BUFR' STRING TEST FOR PORTABILITY 
!                           TO EBCDIC MACHINES; MAXIMUM MESSAGE LENGTH  
!                           INCREASED FROM 20,000 TO 50,000 BYTES       
! 2005-11-29  J. ATOR    -- USE ICHKSTR                                 
!                                                                       
! USAGE:    CALL READERME (MESG, LUNIT, SUBSET, JDATE, IRET)            
!   INPUT ARGUMENT LIST:                                                
!     MESG     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR    
!                MESSAGE                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING READ                                             
!     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR    
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = unrecognized Table A message type           
!                      11 = this is a BUFR table (dictionary) message   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CKTABA   ICHKSTR  LMSG         
!                               STATUS   WTSTAT                         
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
                                                                        
      CHARACTER(8) SUBSET, SEC0 
      CHARACTER(1) CEC0 (8) 
      DIMENSION MESG ( * ), IEC0 (2) 
      EQUIVALENCE (SEC0, IEC0, CEC0) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IRET = 0 
                                                                        
!  CHECK THE FILE STATUS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      CALL WTSTAT (LUNIT, LUN, IL, 1) 
                                                                        
!  COPY THE INPUT MESSAGE INTO THE INTERNAL MESSAGE BUFFER              
!  -------------------------------------------------------              
                                                                        
      IEC0 (1) = MESG (1) 
      IEC0 (2) = MESG (2) 
      DO I = 1, LMSG (SEC0) 
      MBAY (I, LUN) = MESG (I) 
      ENDDO 
                                                                        
!     Confirm that the first 4 bytes of SEC0 contain 'BUFR' encoded in  
!     CCITT IA5 (i.e. ASCII).                                           
                                                                        
      IF (ICHKSTR ('BUFR', CEC0, 4) .NE.0) GOTO 902 
                                                                        
!  PARSE THE MESSAGE SECTION CONTENTS                                   
!  ----------------------------------                                   
                                                                        
      CALL CKTABA (LUN, SUBSET, JDATE, IRET) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: READERME - INPUT BUFR FILE IS CLOSED, IT '// &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: READERME - INPUT BUFR FILE IS OPEN FOR '//   &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: READERME - FIRST 4 BYTES READ FROM RECORD'// &
      ' NOT "BUFR", DOES NOT CONTAIN BUFR DATA')                        
      END SUBROUTINE READERME                       
