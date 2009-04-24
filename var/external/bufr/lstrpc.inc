      FUNCTION LSTRPC (NODE, LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    LSTRPC                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION RETURNS THE PREVIOUS REPLICATION FACTOR NODE, 
!   GIVEN A NODE IN THE INTERNAL JUMP/LINK INDEX TABLE FOR A PARTICULAR 
!   BUFR FILE.  IF THE INPUT NODE IS A REPLICATION FACTOR NODE, THEN IT 
!   IS SIMPLY RETUNED AS LSTRPC.                                        
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
! USAGE:    LSTRPC (NODE, LUN)                                          
!   INPUT ARGUMENT LIST:                                                
!     NODE     - INTEGER: POSITIONAL INDEX IN INTERNAL JUMP/LINK TABLE  
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     LSTRPC   - INTEGER: POSITIONAL INDEX IN INTERNAL JUMP/LINK TABLE  
!                FOR PREVIOUS REPLICATION FACTOR NODE                   
!                    NODE = input node is a replication factor          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT                                    
!    THIS ROUTINE IS CALLED BY: GETWIN   NEWWIN   NXTWIN   PARUSR       
!                               Normally not called by any application  
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
      COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
      MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
      IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
      ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(10) TAG 
      CHARACTER(3) TYP 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IF (NODE.LT.INODE (LUN) ) GOTO 900 
      IF (NODE.GT.ISC (INODE (LUN) ) ) GOTO 901 
                                                                        
      NOD = NODE 
                                                                        
!  FIND THIS OR THE PREVIOUS REPLICATION FACTOR (RPC) NODE              
!  -------------------------------------------------------              
                                                                        
   10 IF (TYP (NOD) .NE.'RPC') THEN 
      NOD = JMPB (NOD) 
      IF (NOD.NE.0) GOTO 10 
      ENDIF 
                                                                        
      LSTRPC = NOD 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: LSTRPC - TABLE NODE (",I7,") IS OUT '&
     &//'OF BOUNDS, < LOWER BOUNDS (",I7,"); TAG IS ",A10)') NODE, INODE&
     & (LUN) , TAG (NODE)                                               
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: LSTRPC - TABLE NODE (",I7,") IS OUT '&
     &//'OF BOUNDS, > UPPER BOUNDS (",I7,"); TAG IS ",A10)') NODE, ISC (&
     &INODE (LUN) ) , TAG (NODE)                                        
      CALL BORT (BORT_STR) 
      END FUNCTION LSTRPC                           
