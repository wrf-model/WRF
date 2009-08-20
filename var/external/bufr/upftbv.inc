      SUBROUTINE UPFTBV (LUNIT, NEMO, VAL, MXIB, IBIT, NIB) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UPFTBV                                                 
!   PRGMMR: JATOR            ORG: NP12       DATE: 2005-11-29           
!                                                                       
! ABSTRACT:  GIVEN A MNEMONIC OF TYPE "FLAG TABLE" ALONG WITH ITS       
!   CORRESPONDING VALUE, THIS SUBROUTINE DETERMINES THE BIT SETTINGS    
!   EQUIVALANT TO THAT VALUE.  NOTE THAT THIS SUBROUTINE IS THE         
!   LOGICAL INVERSE OF BUFRLIB SUBROUTINE PKFTBV.                       
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-11-29  J. ATOR    -- ORIGINAL VERSION                            
!                                                                       
! USAGE:    UPFTBV (LUNIT,NEMO,VAL,MXIB,IBIT,NIB)                       
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     NEMO     - CHARACTER*(*): MNEMONIC OF TYPE "FLAG TABLE"           
!     VAL      - REAL*8: VALUE CORRESPONDING TO NEMO                    
!     MXIB     - INTEGER: DIMENSIONED SIZE OF IBIT IN CALLING PROGRAM   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IBIT     - INTEGER(*): BIT NUMBERS WHICH WERE SET TO "ON"         
!                (I.E. SET TO "1") IN VAL                               
!     NIB      - INTEGER: NUMBER OF BIT NUMBERS RETURNED IN IBIT        
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     NEMTAB   STATUS   VALX         
!    THIS ROUTINE IS CALLED BY: UFBDMP   UFDUMP                         
!                               Also called by application programs.    
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
                                                                        
      REAL(8) VAL, R8VAL, R82I 
                                                                        
      INTEGER IBIT ( * ) 
                                                                        
      CHARACTER ( * ) NEMO 
      CHARACTER(600) TABD 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(128) BORT_STR 
      CHARACTER(1) TAB 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
!     Perform some sanity checks.                                       
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
                                                                        
      CALL NEMTAB (LUN, NEMO, IDN, TAB, N) 
      IF (N.EQ.0) GOTO 901 
      IF (TABB (N, LUN) (71:74) .NE.'FLAG') GOTO 902 
                                                                        
!     Figure out which bits are set.                                    
                                                                        
      NIB = 0 
      R8VAL = VAL 
      NBITS = VALX (TABB (N, LUN) (110:112) ) 
      DO I = (NBITS - 1), 0, - 1 
      R82I = (2.) **I 
      IF (ABS (R8VAL - R82I) .LT. (0.005) ) THEN 
         NIB = NIB + 1 
         IF (NIB.GT.MXIB) GOTO 903 
         IBIT (NIB) = NBITS - I 
         RETURN 
      ELSEIF (R82I.LT.R8VAL) THEN 
         NIB = NIB + 1 
         IF (NIB.GT.MXIB) GOTO 903 
         IBIT (NIB) = NBITS - I 
         R8VAL = R8VAL - R82I 
      ENDIF 
      ENDDO 
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: UPFTBV - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 WRITE (BORT_STR, '("BUFRLIB: UPFTBV - MNEMONIC ",A,'//'" NOT FOUND&
     & IN TABLE B")') NEMO                                              
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: UPFTBV - MNEMONIC ",A,'//'" IS NOT A &
     &FLAG TABLE")') NEMO                                               
      CALL BORT (BORT_STR) 
  903 CALL BORT ('BUFRLIB: UPFTBV - IBIT ARRAY OVERFLOW') 
      END SUBROUTINE UPFTBV                         
