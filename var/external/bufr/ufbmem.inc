      SUBROUTINE UFBMEM (LUNIT, INEW, IRET, IUNIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBMEM                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE OPENS A BUFR FILE FOR INPUT, READS EACH     
!   MESSAGE AND TRANSFERS THEM ONE-BY-ONE TO INTERNAL MEMORY (ARRAY     
!   MSGS IN COMMON BLOCK /MSGMEM/).  IF MESSAGES ARE APPENDED TO        
!   EXISTING MESSAGES IN INTERNAL MEMORY, THE BUFR FILE READ HERE IS    
!   CLOSED PRIOR TO RETURNING TO THE CALLING PROGRAM.                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE MAXIMUM NUMBER OF BYTES REQUIRED TO     
!                           STORE ALL MESSAGES INTERNALLY WAS INCREASED 
!                           FROM 4 MBYTES TO 8 MBYTES                   
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
!                           TERMINATES ABNORMALLY                       
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2004-11-15  D. KEYSER  -- MODIFIED TO NOT ABORT WHEN THERE ARE EITHER 
!                           TOO MANY MESSAGES READ IN (I.E., .GT.       
!                           MAXMSG) OR TOO MANY BYTES READ IN (I.E.,    
!                           .GT. MAXMEM), BUT RATHER JUST STORE MAXMSG  
!                           MESSAGES OR MAXMEM BYTES AND PRINT A        
!                           DIAGNOSTIC; PARAMETER MAXMEM (THE MAXIMUM   
!                           NUMBER OF BYTES REQUIRED TO STORE ALL       
!                           MESSAGES INTERNALLY) WAS INCREASED FROM 16  
!                           MBYTES TO 50 MBYTES                         
! 2005-11-29  J. ATOR    -- USE RDMSGW AND NMWRD                        
!                                                                       
! USAGE:    CALL UFBMEM (LUNIT, INEW, IRET, IUNIT)                      
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     INEW     - INTEGER: SWITCH:                                       
!                       0 = initialize internal arrays prior to         
!                           transferring messages here                  
!                    else = append the messages transferred here to     
!                           internal memory arrays                      
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IRET     - INTEGER: NUMBER OF MESSAGES TRANSFERRED                
!     IUNIT    - INTEGER: RETURN CODE:                                  
!                       0 = no messages were read from LUNIT, file is   
!                           empty                                       
!                   LUNIT = INEW input as 0                             
!                    else = FORTRAN logical unit for BUFR file          
!                           associated with initial message transferred 
!                           to internal memory                          
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    NOTE THAT IREADMM, RDMEMM, READMM, UFBMMS, UFBMNS, UFBRMS, UFBTAB  
!    OR UFBTAM CAN BE CALLED AFTER THIS TO READ SPECIFIC BUFR MESSAGES  
!    FROM INTERNAL MEMORY.                                              
!                                                                       
!    THIS ROUTINE CALLS:        BORT     CLOSBF   NMWRD    OPENBF       
!                               RDMSGW                                  
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
                                                                        
!     COMMON / MSGMEM / MUNIT, MLAST, MSGP (0:MAXMSG), MSGS (MAXMEM) 
                                                                        
      CHARACTER(128) BORT_STR 
      DIMENSION MBAY (MXMSGLD4) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  TRY TO OPEN BUFR FILE AND SET TO INITIALIZE OR CONCATENATE           
!  ----------------------------------------------------------           
                                                                        
      CALL OPENBF (LUNIT, 'IN', LUNIT) 
                                                                        
      IF ( .NOT. ALLOCATED (MSGS) ) ALLOCATE ( MSGS(MAXMEM) )
      IF ( .NOT. ALLOCATED (MSGP) ) ALLOCATE ( MSGP(MAXMSG) )

      IF (INEW.EQ.0) THEN 
         MSGP (0) = 0 
         MUNIT = 0 
         MLAST = 0 
      ENDIF 
                                                                        
      NMSG = MSGP (0) 
      IRET = 0 
      IFLG = 0 
      ITIM = 0 
                                                                        
!  TRANSFER MESSAGES FROM FILE TO MEMORY - SET MESSAGE POINTERS         
!  ------------------------------------------------------------         
                                                                        
    1 CALL RDMSGW (LUNIT, MBAY, IER) 
      IF (IER.EQ. - 1) GOTO 100 
      IF (IER.EQ. - 2) GOTO 900 
      NMSG = NMSG + 1 
      IF (NMSG.GT.MAXMSG) IFLG = 1 
      LMEM = NMWRD (MBAY) 
      IF (LMEM + MLAST.GT.MAXMEM) IFLG = 2 
                                                                        
      IF (IFLG.EQ.0) THEN 
         IRET = IRET + 1 
         DO I = 1, LMEM 
         MSGS (MLAST + I) = MBAY (I) 
         ENDDO 
         MSGP (0) = NMSG 
         MSGP (NMSG) = MLAST + 1 
      ELSE 
         IF (ITIM.EQ.0) THEN 
            MLAST0 = MLAST 
            ITIM = 1 
         ENDIF 
      ENDIF 
      MLAST = MLAST + LMEM 
      GOTO 1 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 IF (IFLG.EQ.1) THEN 
                                                                        
!  EMERGENCY ROOM TREATMENT FOR MAXMSG ARRAY OVERFLOW                   
!  --------------------------------------------------                   
                                                                        
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBMEM - THE NO. OF MESSAGES REQUIRED TO STORE&
     &', ' ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMSG, ') - ', '&
     &INCOMPLETE READ'                                                  
      PRINT * , '>>>UFBMEM STORED ', MSGP (0) , ' MESSAGES OUT OF ', NMS&
     &G, '<<<'                                                          
            PRINT * , '>>>UFBMEM STORED ', MLAST0, ' BYTES OUT OF ',    &
            MLAST, '<<<'                                                
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         MLAST = MLAST0 
      ENDIF 
                                                                        
      IF (IFLG.EQ.2) THEN 
                                                                        
!  EMERGENCY ROOM TREATMENT FOR MAXMEM ARRAY OVERFLOW                   
!  --------------------------------------------------                   
                                                                        
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBMEM - THE NO. OF BYTES REQUIRED TO STORE', &
     &' ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMEM, ') - ', 'INC&
     &OMPLETE READ'                                                     
            PRINT * , '>>>UFBMEM STORED ', MLAST0, ' BYTES OUT OF ',    &
            MLAST, '<<<'                                                
      PRINT * , '>>>UFBMEM STORED ', MSGP (0) , ' MESSAGES OUT OF ', NMS&
     &G, '<<<'                                                          
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         MLAST = MLAST0 
      ENDIF 
                                                                        
      IF (IRET.EQ.0) THEN 
         CALL CLOSBF (LUNIT) 
      ELSE 
         IF (MUNIT.NE.0) CALL CLOSBF (LUNIT) 
         IF (MUNIT.EQ.0) MUNIT = LUNIT 
      ENDIF 
      IUNIT = MUNIT 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: UFBMEM - ERROR READING MESSAGE '//   &
      'NUMBER",I5," INTO MEMORY FROM UNIT",I3)') NMSG + 1, LUNIT        
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBMEM                         
