      SUBROUTINE NUMTBD (LUN, IDN, NEMO, TAB, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NUMTBD                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2002-05-14           
!                                                                       
! ABSTRACT: THIS SUBROUTINE SEARCHES FOR AN INTEGER IDN, CONTAINING THE 
!   BIT-WISE REPRESENTATION OF A DESCRIPTOR (FXY) VALUE, WITHIN THE     
!   INTERNAL BUFR TABLE D AND B ARRAYS IN COMMON BLOCK /TABABD/.  IF    
!   FOUND, IT RETURNS THE CORRESPONDING MNEMONIC AND OTHER INFORMATION  
!   FROM WITHIN THESE ARRAYS.  IF IDN IS NOT FOUND, IT RETURNS WITH     
!   IRET=0.  THIS SUBROUTINE IS IDENTICAL TO BUFR ARCHIVE LIBRARY       
!   SUBROUTINE NUMTAB EXCEPT NUMTAB ALSO SEARCHS FOR IDN WITHIN         
!   INTERNAL BUFR REPLICATION ARRAYS AND BUFR TABLE C.                  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                                                                       
! USAGE:    CALL NUMTBD (LUN, IDN, NEMO, TAB, IRET)                     
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     IDN      - INTEGER: BIT-WISE REPRESENTATION OF DESCRIPTOR (FXY)   
!                VALUE                                                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NEMO     - CHARACTER*(*): MNEMONIC CORRESPONDING TO IDN           
!     TAB      - CHARACTER*1: TYPE OF FXY VALUE THAT IS BIT-WISE        
!                REPRESENTED BY IDN:                                    
!                     'B' = BUFR Table B descriptor                     
!                     'D' = BUFR Table D descriptor                     
!     IRET     - INTEGER: RETURN VALUE (SEE REMARKS)                    
!                                                                       
! REMARKS:                                                              
!    THE INTERPRETATION OF THE RETURN VALUE IRET DEPENDS UPON THE       
!    RETURN VALUE OF TAB, AS FOLLOWS:                                   
!                                                                       
!    IF ( TAB = 'B' ) THEN                                              
!       IRET = positional index of IDN within internal BUFR Table B     
!              array                                                    
!    ELSE IF ( TAB = 'D') THEN                                          
!       IRET = positional index of IDN within internal BUFR Table D     
!              array                                                    
!    ELSE IF ( IRET = 0 ) THEN                                          
!       IDN was not found in internal BUFR Table B or D                 
!    END IF                                                             
!                                                                       
!                                                                       
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: RESTD                                   
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
                                                                        
      CHARACTER ( * ) NEMO 
      CHARACTER(600) TABD 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(1) TAB 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      NEMO = ' ' 
      IRET = 0 
      TAB = ' ' 
                                                                        
!  LOOK FOR IDN IN TABLE D                                              
!  -----------------------                                              
                                                                        
      DO I = 1, NTBD (LUN) 
      IF (IDN.EQ.IDND (I, LUN) ) THEN 
      NEMO = TABD (I, LUN) (7:14) 
      TAB = 'D' 
      IRET = I 
      GOTO 100 
      ENDIF 
      ENDDO 
                                                                        
!  LOOK FOR IDN IN TABLE B                                              
!  -----------------------                                              
                                                                        
      DO I = 1, NTBB (LUN) 
      IF (IDN.EQ.IDNB (I, LUN) ) THEN 
      NEMO = TABB (I, LUN) (7:14) 
      TAB = 'B' 
      IRET = I 
      GOTO 100 
      ENDIF 
      ENDDO 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE NUMTBD                         
