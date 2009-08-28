      SUBROUTINE COPYMG (LUNIN, LUNOT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    COPYMG                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES A BUFR MESSAGE, INTACT, FROM LOGICAL 
!   UNIT LUNIN, OPENED FOR INPUT VIA A PREVIOUS CALL TO BUFR ARCHIVE    
!   LIBRARY SUBROUTINE OPENBF, TO LOGICAL UNIT LUNOT, OPENED FOR OUTPUT 
!   VIA A PREVIOUS CALL TO OPENBF.  THE MESSAGE COPIED FROM LOGICAL     
!   UNIT LUNIN WILL BE THE ONE MOST RECENTLY READ USING BUFR ARCHIVE    
!   LIBRARY SUBROUTINE READMG.  THE OUTPUT FILE MUST HAVE NO CURRENTLY  
!   OPEN MESSAGES.  ALSO, BOTH FILES MUST HAVE BEEN OPENED TO THE BUFR  
!   INTERFACE WITH IDENTICAL BUFR TABLES.                               
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
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- USE IUPBS01                                 
!                                                                       
! USAGE:    CALL COPYMG (LUNIN, LUNOT)                                  
!   INPUT ARGUMENT LIST:                                                
!     LUNIN    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR INPUT BUFR    
!                FILE                                                   
!     LUNOT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR OUTPUT BUFR   
!                FILE                                                   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IUPBS01  MSGWRT   NEMTBA       
!                               STATUS                                  
!    THIS ROUTINE IS CALLED BY: None.                                   
!                               Normally called only by application     
!                               programs.                               
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
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
                                                                        
!     CHARACTER(10) TAG 
      CHARACTER(8) SUBSET 
!     CHARACTER(3) TYP 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE FILE STATUSES                                              
!  -----------------------                                              
                                                                        
      CALL STATUS (LUNIN, LIN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
      CALL STATUS (LUNOT, LOT, IL, IM) 
      IF (IL.EQ.0) GOTO 903 
      IF (IL.LT.0) GOTO 904 
      IF (IM.NE.0) GOTO 905 
                                                                        
!  MAKE SURE BOTH FILES HAVE THE SAME TABLES                            
!  -----------------------------------------                            
                                                                        
      SUBSET = TAG (INODE (LIN) ) 
!  .... Given SUBSET, returns MSGT,MSTB,INOD                            
      CALL NEMTBA (LOT, SUBSET, MSGT, MSTB, INOD) 
      IF (INODE (LIN) .NE.INOD) GOTO 906 
                                                                        
!  EVERYTHING OKAY, COPY A MESSAGE                                      
!  -------------------------------                                      
                                                                        
      MBYM = IUPBS01 (MBAY (1, LIN) , 'LENM') 
      CALL MSGWRT (LUNOT, MBAY (1, LIN), MBYM) 
                                                                        
!  SET THE MESSAGE CONTROL WORDS FOR PARTITION ASSOCIATED WITH LUNOT    
!  -----------------------------------------------------------------    
                                                                        
      NMSG (LOT) = NMSG (LOT) + 1 
      NSUB (LOT) = MSUB (LIN) 
      IDATE (LOT) = IDATE (LIN) 
      INODE (LOT) = INODE (LIN) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: COPYMG - INPUT BUFR FILE IS CLOSED, IT MUST'/&
     &/' BE OPEN FOR INPUT')                                            
  901 CALL BORT ('BUFRLIB: COPYMG - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: COPYMG - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  904 CALL BORT ('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  905 CALL BORT ('BUFRLIB: COPYMG - ALL MESSAGES MUST BE CLOSED IN '//  &
      'OUTPUT BUFR FILE, A MESSAGE IS OPEN')                            
  906 CALL BORT ('BUFRLIB: COPYMG - INPUT AND OUTPUT BUFR FILES MUST '//&
      'HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')         
      END SUBROUTINE COPYMG                         
