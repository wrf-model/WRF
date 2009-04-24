      SUBROUTINE CHEKSTAB (LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CHEKSTAB                                               
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE CHECKS THAT AN INTERNAL BUFR TABLE          
!   REPRESENTATION IS SELF-CONSISTENT AND FULLY DEFINED.  IF ANY ERRORS 
!   ARE FOUND, THEN AN APPROPRIATE CALL IS MADE TO BUFR ARCHIVE LIBRARY 
!   SUBROUTINE BORT.                                                    
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE   
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES      
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
!                                                                       
! USAGE:    CALL CHEKSTAB (LUN)                                         
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     NEMTAB   NEMTBB   NEMTBD       
!    THIS ROUTINE IS CALLED BY: MAKESTAB                                
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / TABABD / NTBA (0:NFILES), NTBB (0:NFILES), NTBD (0:      &
      NFILES), MTAB (MAXTBA, NFILES), IDNA (MAXTBA, NFILES, 2), IDNB (  &
      MAXTBB, NFILES), IDND (MAXTBD, NFILES), TABA (MAXTBA, NFILES),    &
      TABB (MAXTBB, NFILES), TABD (MAXTBD, NFILES)                      
                                                                        
      CHARACTER(600) TABD 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(128) BORT_STR 
      CHARACTER(24) UNIT 
      CHARACTER(8) NEMO, NEMS (MAXCD) 
      CHARACTER(1) TAB 
      DIMENSION IRPS (MAXCD), KNTS (MAXCD) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  THERE MUST BE ENTRIES IN TABLES A, B, AND D                          
!  -------------------------------------------                          
                                                                        
      IF (NTBA (LUN) .EQ.0) GOTO 900 
      IF (NTBB (LUN) .EQ.0) GOTO 901 
      IF (NTBD (LUN) .EQ.0) GOTO 902 
                                                                        
!  MAKE SURE EACH TABLE A ENTRY DEFINED AS A SEQUENCE                   
!  --------------------------------------------------                   
                                                                        
      DO I = 1, NTBA (LUN) 
      NEMO = TABA (I, LUN) (4:11) 
      CALL NEMTAB (LUN, NEMO, IDN, TAB, IRET) 
      IF (TAB.NE.'D') GOTO 903 
      ENDDO 
                                                                        
!  CHECK TABLE B CONTENTS                                               
!  ----------------------                                               
                                                                        
      DO ITAB = 1, NTBB (LUN) 
      CALL NEMTBB (LUN, ITAB, UNIT, ISCL, IREF, IBIT) 
      ENDDO 
                                                                        
!  CHECK TABLE D CONTNETS                                               
!  ----------------------                                               
                                                                        
      DO ITAB = 1, NTBD (LUN) 
      CALL NEMTBD (LUN, ITAB, NSEQ, NEMS, IRPS, KNTS) 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: CHEKSTAB - EMPTY TABLE A IN INTERNAL BUFR TAB&
     &LES')                                                             
  901 CALL BORT ('BUFRLIB: CHEKSTAB - EMPTY TABLE B IN INTERNAL BUFR TAB&
     &LES')                                                             
  902 CALL BORT ('BUFRLIB: CHEKSTAB - EMPTY TABLE D IN INTERNAL BUFR TAB&
     &LES')                                                             
  903 WRITE (BORT_STR, '("BUFRLIB: CHEKSTAB - TABLE A ENTRY: ",A," NOT '&
     &//'DEFINED AS A SEQUENCE")') NEMO                                 
      CALL BORT (BORT_STR) 
      END SUBROUTINE CHEKSTAB                       
