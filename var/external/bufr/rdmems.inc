      SUBROUTINE RDMEMS (ISUB, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDMEMS                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS A PARTICULAR SUBSET FROM A BUFR       
!   MESSAGE IN INTERNAL MEMORY (ARRAY MBAY IN COMMON BLOCK /BITBUF/)    
!   INTO INTERNAL SUBSET ARRAYS BASED ON THE SUBSET NUMBER IN THE       
!   MESSAGE.                                                            
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-  
!                           LINING CODE WITH FPP DIRECTIVES             
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
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
! USAGE:    CALL RDMEMS (ISUB, IRET)                                    
!   INPUT ARGUMENT LIST:                                                
!     ISUB     - INTEGER: POINTER TO SUBSET NUMBER TO READ IN BUFR      
!                MESSAGE                                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = ISUB is greater than the number of subsets  
!                           in memory                                   
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IUPB     READSB   STATUS       
!    THIS ROUTINE IS CALLED BY: UFBMMS   UFBMNS   UFBRMS                
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      CHARACTER(128) BORT_STR 
                                                                        
!     COMMON / MSGMEM / MUNIT, MLAST, MSGP (0:MAXMSG), MSGS (MAXMEM) 
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
      COMMON / UNPTYP / MSGUNP (NFILES) 
      COMMON / QUIET / IPRT 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE MESSAGE REQUEST AND FILE STATUS                            
!  -----------------------------------------                            
                                                                        
      CALL STATUS (MUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (NSUB (LUN) .NE.0) GOTO 903 
                                                                        
      IF (ISUB.GT.MSUB (LUN) ) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: RDMEMS - REQ. SUBSET NO. {', ISUB, ' - FIRST '&
     &, '(INPUT) ARGUMENT} > NO. OF SUBSETS IN MEMORY MESSAGE (', MSUB (&
     &LUN) , '), RETURN WITH IRET = -1'                                 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         IRET = - 1 
         GOTO 100 
      ENDIF 
                                                                        
      MBYM = MBYT (LUN) 
      NBYT = 0 
                                                                        
!  POSITION TO SUBSET NUMBER ISUB IN MEMORY MESSAGE                     
!  ------------------------------------------------                     
                                                                        
      IF (MSGUNP (LUN) .EQ.0) THEN 
         NSUB (LUN) = ISUB - 1 
         DO I = 1, ISUB - 1 
         MBYT (LUN) = MBYT (LUN) + IUPB (MBAY (1, LUN), MBYT (LUN)      &
         + 1, 16)                                                       
         ENDDO 
      ELSEIF (MSGUNP (LUN) .EQ.1) THEN 
!  .... message with "standard" Section 3                               
         DO I = 1, ISUB - 1 
         CALL READSB (MUNIT, IRET) 
         ENDDO 
      ELSEIF (MSGUNP (LUN) .EQ.2) THEN 
!  .... compressed message                                              
         NSUB (LUN) = ISUB - 1 
      ENDIF 
                                                                        
!  NOW READ SUBSET NUMBER ISUB FROM MEMORY MESSAGE                      
!  -----------------------------------------------                      
                                                                        
      CALL READSB (MUNIT, IRET) 
!  .... This should have already been accounted for with stmt. 902 or   
!       IRET = -1 above                                                 
      IF (IRET.NE.0) GOTO 904 
                                                                        
!  RESET SUBSET POINTER BACK TO ZERO (BEGINNING OF MESSAGE) AND RETURN  
!  -------------------------------------------------------------------  
                                                                        
      MBYT (LUN) = MBYM 
      NSUB (LUN) = 0 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: RDMEMS - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: RDMEMS - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: RDMEMS - A MEMORY MESSAGE MUST BE OPEN IN '//&
      'INPUT BUFR FILE, NONE ARE')                                      
  903 WRITE (BORT_STR, '("BUFRLIB: RDMEMS - UPON ENTRY, SUBSET POINTER '&
     &//'IN MEMORY MESSAGE IS NOT AT BEGINNING (",I3," SUBSETS HAVE '//'&
     &BEEN READ, SHOULD BE 0)")') NSUB (LUN)                            
      CALL BORT (BORT_STR) 
  904 CALL BORT ('BUFRLIB: RDMEMS - CALL TO ROUTINE READSB RETURNED '// &
      'WITH IRET = -1 (EITHER MEMORY MESSAGE NOT OPEN OR ALL '//        &
      'SUBSETS IN MESSAGE READ')                                        
      END SUBROUTINE RDMEMS                         
