      SUBROUTINE GETABDB (LUNIT, TABDB, ITAB, JTAB) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    GETABDB                                                
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS INTERNAL TABLE B AND TABLE D        
!   INFORMATION FOR LOGICAL UNIT LUNIT IN A PRE-DEFINED ASCII FORMAT.   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-11-29  J. ATOR    -- ADDED TO BUFR ARCHIVE LIBRARY (WAS IN-LINED 
!                           IN PROGRAM NAMSND)                          
!                                                                       
! USAGE:    CALL GETABDB( LUNIT, TABDB, ITAB, JTAB )                    
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     ITAB     - INTEGER: DIMENSIONED SIZE OF TABDB ARRAY               
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     TABDB    - CHARACTER*128: (JTAB)-WORD ARRAY OF INTERNAL TABLE B   
!                AND TABLE D INFORMATION                                
!     JTAB     - INTEGER: NUMBER OF ENTRIES STORED WITHIN TABDB         
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        NEMTBD   STATUS                         
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
                                                                        
      COMMON / TABABD / NTBA (0:NFILES), NTBB (0:NFILES), NTBD (0:      &
      NFILES), MTAB (MAXTBA, NFILES), IDNA (MAXTBA, NFILES, 2), IDNB (  &
      MAXTBB, NFILES), IDND (MAXTBD, NFILES), TABA (MAXTBA, NFILES),    &
      TABB (MAXTBB, NFILES), TABD (MAXTBD, NFILES)                      
                                                                        
      CHARACTER(600) TABD 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(128) TABDB ( * ) 
      CHARACTER(8) NEMO, NEMS (MAXCD) 
      DIMENSION IRPS (MAXCD), KNTS (MAXCD) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      JTAB = 0 
                                                                        
!  MAKE SURE THE FILE IS OPEN                                           
!  --------------------------                                           
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) RETURN 
                                                                        
!  WRITE OUT THE TABLE D ENTRIES FOR THIS FILE                          
!  -------------------------------------------                          
                                                                        
      DO I = 1, NTBD (LUN) 
      NEMO = TABD (I, LUN) (7:14) 
      CALL NEMTBD (LUN, I, NSEQ, NEMS, IRPS, KNTS) 
      DO J = 1, NSEQ, 10 
      JTAB = JTAB + 1 
      IF (JTAB.LE.ITAB) THEN 
         WRITE (TABDB (JTAB), 1) NEMO, (NEMS (K), K = J, MIN (J + 9,    &
         NSEQ) )                                                        
    1 FORMAT   ('D ',A8,10(1X,A10)) 
      ENDIF 
      ENDDO 
      ENDDO 
                                                                        
!  ADD THE TABLE B ENTRIES                                              
!  -----------------------                                              
                                                                        
      DO I = 1, NTBB (LUN) 
      JTAB = JTAB + 1 
      IF (JTAB.LE.ITAB) THEN 
         WRITE (TABDB (JTAB), 2) TABB (I, LUN) (7:14), TABB (I, LUN)    &
         (71:112)                                                       
    2 FORMAT   ('B ',A8,1X,A42) 
      ENDIF 
      ENDDO 
                                                                        
      RETURN 
      END SUBROUTINE GETABDB                        
