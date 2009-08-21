      SUBROUTINE RDMEMM (IMSG, SUBSET, JDATE, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDMEMM                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS A PARTICULAR BUFR MESSAGE FROM        
!   INTERNAL MEMORY (ARRAY MSGS IN COMMON BLOCK /MSGMEM/) INTO A        
!   MESSAGE BUFFER (ARRAY MBAY IN COMMON BLOCK /BITBUF/).  IT IS        
!   IDENTICAL TO BUFR ARCHIVE LIBRARY SUBROUTINE READMM EXCEPT IT DOES  
!   NOT ADVANCE THE VALUE OF IMSG PRIOR TO RETURNING TO CALLING         
!   PROGRAM.                                                            
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K        
!                           COMPLIANT                                   
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI); THE MAXIMUM      
!                           NUMBER OF BYTES REQUIRED TO STORE ALL       
!                           MESSAGES INTERNALLY WAS INCREASED FROM 4    
!                           MBYTES TO 8 MBYTES                          
! 2000-09-19  J. WOOLLEN -- REMOVED MESSAGE DECODING LOGIC THAT HAD     
!                           BEEN REPLICATED IN THIS AND OTHER READ      
!                           ROUTINES AND CONSOLIDATED IT INTO A NEW     
!                           ROUTINE CKTABA, CALLED HERE, WHICH IS       
!                           ENHANCED TO ALLOW COMPRESSED AND STANDARD   
!                           BUFR MESSAGES TO BE READ; MAXIMUM MESSAGE   
!                           LENGTH INCREASED FROM 10,000 TO 20,000      
!                           BYTES                                       
! 2001-08-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 8 MBYTES TO  
!                           16 MBYTES                                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- PARAMETER MAXMSG (THE MAXIMUM NUMBER OF     
!                           BUFR MESSAGES WHICH CAN BE STORED           
!                           INTERNALLY) INCREASED FROM 50000 TO 200000; 
!                           UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY OR UNUSUAL THINGS     
!                           HAPPEN                                      
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2004-11-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 16 MBYTES TO 
!                           50 MBYTES                                   
!                                                                       
! USAGE:    CALL RDMEMM (IMSG, SUBSET, JDATE, IRET)                     
!   INPUT ARGUMENT LIST:                                                
!     IMSG     - INTEGER: POINTER TO BUFR MESSAGE NUMBER (RECORD) IN    
!                         STORAGE                                       
!                       0 = resets the memory file                      
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING READ                                             
!     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR    
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = IMSG is either zero or greater than the     
!                           number of messages in memory                
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    NOTE THAT UFBMEM IS CALLED PRIOR TO THIS TO STORE THE BUFR         
!    MESSAGES INTO INTERNAL MEMORY.                                     
!                                                                       
!    THIS ROUTINE CALLS:        BORT     CKTABA   STATUS   WTSTAT       
!    THIS ROUTINE IS CALLED BY: UFBMMS   UFBMNS   UFBRMS   UFBTAM       
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
!     COMMON / MSGMEM / MUNIT, MLAST, MSGP (0:MAXMSG), MSGS (MAXMEM) 
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER(8) SUBSET 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE MESSAGE REQUEST AND FILE STATUS                            
!  -----------------------------------------                            
                                                                        
      CALL STATUS (MUNIT, LUN, IL, IM) 
      CALL WTSTAT (MUNIT, LUN, IL, 1) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IRET = 0 
                                                                        
      IF (IMSG.EQ.0.OR.IMSG.GT.MSGP (0) ) THEN 
         CALL WTSTAT (MUNIT, LUN, IL, 0) 
         IF (IPRT.GE.1) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            IF (IMSG.EQ.0) THEN 
               PRINT * , 'BUFRLIB: RDMEMM - REQUESTED MEMORY MESSAGE ', &
               'NUMBER {FIRST (INPUT) ARGUMENT} IS 0, RETURN WITH ',    &
               'IRET = -1'                                              
            ELSE 
               PRINT * , 'BUFRLIB: RDMEMM - REQ. MEMORY MESSAGE NO. {', &
               IMSG, ' - {1ST (INPUT) ARG.} > NO. OF MESSAGES IN ',     &
               'MEMORY (', MSGP (0) , '), RETURN WITH IRET = -1'        
            ENDIF 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         IRET = - 1 
         GOTO 100 
      ENDIF 
                                                                        
!  READ MEMORY MESSAGE NUMBER IMSG INTO A MESSAGE BUFFER                
!  -----------------------------------------------------                
                                                                        
      IPTR = MSGP (IMSG) 
      IF (IMSG.LT.MSGP (0) ) LPTR = MSGP (IMSG + 1) - IPTR 
      IF (IMSG.EQ.MSGP (0) ) LPTR = MLAST - IPTR + 1 
      IPTR = IPTR - 1 
                                                                        
      DO I = 1, LPTR 
      MBAY (I, LUN) = MSGS (IPTR + I) 
      ENDDO 
                                                                        
!  PARSE THE MESSAGE SECTION CONTENTS                                   
!  ----------------------------------                                   
                                                                        
      CALL CKTABA (LUN, SUBSET, JDATE, JRET) 
      NMSG (LUN) = IMSG 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: RDMEMM - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: RDMEMM - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
      END SUBROUTINE RDMEMM                         
