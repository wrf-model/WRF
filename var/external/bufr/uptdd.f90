      SUBROUTINE UPTDD (ID, LUN, IENT, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UPTDD                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS THE BIT-WISE REPRESENTATION OF THE  
!   FXY VALUE CORRESPONDING TO, SEQUENTIALLY, A PARTICULAR (IENT'th)    
!   "CHILD" MNEMONIC OF A TABLE D SEQUENCE ("PARENT") MNEMONIC.         
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
! USAGE:    CALL UPTDD (ID, LUN, IENT, IRET)                            
!   INPUT ARGUMENT LIST:                                                
!     ID       - INTEGER: POSITIONAL INDEX OF PARENT MNEMONIC WITHIN    
!                INTERNAL BUFR TABLE D ARRAY TABD                       
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     IENT     - INTEGER: ORDINAL INDICATOR OF CHILD MNEMONIC TO RETURN 
!                FROM WITHIN TABD(ID,LUN) SEQUENCE:                     
!                       0 = return a count of the total number of child 
!                           mnemonics within TABD(ID,LUN)               
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IRET     - INTEGER: RETURN VALUE (SEE REMARKS)                    
!                                                                       
! REMARKS:                                                              
!    THE INTERPRETATION OF THE RETURN VALUE IRET DEPENDS UPON THE INPUT 
!    VALUE IENT, AS FOLLOWS:                                            
!                                                                       
!    IF ( IENT = 0 ) THEN                                               
!       IRET = a count of the total number of child mnemonics within    
!              TABD(ID,LUN)                                             
!    ELSE                                                               
!       IRET = the bit-wise representation of the FXY value             
!              corresponding to the IENT'th child mnemonic of           
!              TABD(ID,LUN)                                             
!    END IF                                                             
!                                                                       
!                                                                       
!    THIS ROUTINE CALLS:        BORT     IUPM                           
!    THIS ROUTINE IS CALLED BY: NEMTBD   RESTD                          
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
      COMMON / DXTAB / MAXDX, IDXV, NXSTR (10), LDXA (10), LDXB (10),   &
      LDXD (10), LD30 (10), DXSTR (10)                                  
                                                                        
      CHARACTER(600) TABD 
      CHARACTER(128) BORT_STR 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(56) DXSTR 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      LDD = LDXD (IDXV + 1) + 1 
                                                                        
!  CHECK IF IENT IS IN BOUNDS                                           
!  --------------------------                                           
                                                                        
      NDSC = IUPM (TABD (ID, LUN) (LDD:LDD), 8) 
                                                                        
      IF (IENT.EQ.0) THEN 
      IRET = NDSC 
      GOTO 100 
      ELSEIF (IENT.LT.0.OR.IENT.GT.NDSC) THEN 
      GOTO 900 
      ENDIF 
                                                                        
!  RETURN THE DESCRIPTOR INDICATED BY IENT                              
!  ---------------------------------------                              
                                                                        
      IDSC = LDD+1 + (IENT - 1) * 2 
      IRET = IUPM (TABD (ID, LUN) (IDSC:IDSC), 16) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: UPTDD - VALUE OF THIRD ARGUMENT IENT'&
     &//' (INPUT) IS OUT OF RANGE (IENT =",I4,")")') IENT               
      CALL BORT (BORT_STR) 
      END SUBROUTINE UPTDD                          
