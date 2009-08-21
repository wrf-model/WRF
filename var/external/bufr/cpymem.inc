      SUBROUTINE CPYMEM (LUNOT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CPYMEM                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES A BUFR MESSAGE, INTACT, FROM         
!   INTERNAL MEMORY, STORED VIA A PREVIOUS CALL TO BUFR ARCHIVE LIBRARY 
!   SUBROUTINE UFBMEM, TO LOGICAL UNIT LUNOT, OPENED FOR OUTPUT VIA A   
!   PREVIOUS CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF.  THE       
!   MESSAGE COPIED FROM INTERNAL MEMORY WILL BE THE ONE MOST RECENTLY   
!   READ INTO THE MESSAGE BUFFER (ARRAY MBAY IN COMMON BLOCK /BITBUF/)  
!   USING BUFR ARCHIVE LIBRARY SUBROUTINE RDMEMM OR READMM.  THE OUTPUT 
!   FILE MUST HAVE NO CURENTLY OPEN MESSAGES.  ALSO, THE INTERNAL BUFR  
!   TABLES ASSOCIATED WITH THE INPUT MESSAGE MUST BE IDENTICAL TO THE   
!   BUFR TABLES USED TO OPEN LUNOT TO THE BUFR INTERFACE.  THIS         
!   SUBROUTINE IS SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE COPYMG     
!   EXCEPT THE INPUT MESSAGE IS FROM INTERNAL MEMORY NOT FROM A         
!   PHYSICAL BUFR FILE.                                                 
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
! 2001-08-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 8 MBYTES TO  
!                           16 MBYTES                                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- PARAMETER MAXMSG (THE MAXIMUM NUMBER OF     
!                           BUFR MESSAGES WHICH CAN BE STORED           
!                           INTERNALLY) INCREASED FROM 50000 TO 200000; 
!                           MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2004-11-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 16 MBYTES TO 
!                           50 MBYTES                                   
! 2005-11-29  J. ATOR    -- USE IUPBS01                                 
!                                                                       
! USAGE:    CALL CPYMEM (LUNOT)                                         
!   INPUT ARGUMENT LIST:                                                
!     LUNOT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IUPBS01  MSGWRT   NEMTBA       
!                               STATUS                                  
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
                                                                        
      CALL STATUS (MUNIT, LIN, IL, IM) 
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
!  .... Given SUBSET, returns MTYP,MSTB,INOD                            
      CALL NEMTBA (LOT, SUBSET, MTYP, MSBT, INOD) 
      IF (INODE (LIN) .NE.INOD) GOTO 906 
                                                                        
!  EVERYTHING OKAY, COPY A MESSAGE                                      
!  -------------------------------                                      
                                                                        
      MBYM = IUPBS01 (MBAY (1, LIN) , 'LENM') 
      CALL MSGWRT (LUNOT, MBAY (1, LIN), MBYM) 
                                                                        
!  SET THE MESSAGE CONTROL WORDS FOR PARTITION ASSOCIATED WITH LUNOT    
!  -----------------------------------------------------------------    
                                                                        
      NMSG (LOT) = NMSG (LOT) + 1 
      NSUB (LOT) = MSUB (LIN) 
      MSUB (LOT) = MSUB (LIN) 
      IDATE (LOT) = IDATE (LIN) 
      INODE (LOT) = INODE (LIN) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: CPYMEM - LOGICAL UNIT NO. ASSOC. WITH INPUT'/&
     &/' BUFR MESSAGES IN INTERNAL MEMORY IS CLOSED, IT MUST BE OPEN '//&
     &'FOR INPUT')                                                      
  901 CALL BORT ('BUFRLIB: CPYMEM - LOGICAL UNIT NO. ASSOC. WITH INPUT'/&
     &/' BUFR MESSAGES IN INTERNAL MEMORY OPEN FOR OUTPUT, MUST BE '//' &
     &OPEN FOR INPUT')                                                  
  902 CALL BORT ('BUFRLIB: CPYMEM - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR MESSAGES IN INTERNAL MEMORY, NONE ARE')                     
  903 CALL BORT ('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  904 CALL BORT ('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  905 CALL BORT ('BUFRLIB: CPYMEM - ALL MESSAGES MUST BE CLOSED IN '//  &
      'OUTPUT BUFR FILE, A MESSAGE IS OPEN')                            
  906 CALL BORT ('BUFRLIB: CPYMEM - INPUT BUFR MESSAGES IN INTERNAL '//'&
     &MEMORY AND OUTPUT BUFR FILE MUST HAVE SAME INTERNAL TABLES '//'(DI&
     &FFERENT HERE)')                                                   
                                                                        
      END SUBROUTINE CPYMEM                         
