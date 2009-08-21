      FUNCTION NEVN (NODE, LUN, INV1, INV2, I1, I2, I3, USR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NEVN (docblock incomplete)                             
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS FUNCTION ACCUMULATES ALL DATA EVENTS FOR A PARTICULAR  
!   DATA VALUE AND LEVEL AND RETURNS THEM TO THE CALLING PROGRAM.  THE  
!   VALUE OF THE FUNCTION ITSELF IS THE TOTAL NUMBER OF EVENTS FOUND.   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN VERIFICATION        
!                           VERSION)                                    
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                           (INCOMPLETE); OUTPUTS MORE COMPLETE         
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
!                                                                       
! USAGE:    NEVN (NODE, LUN, INV1, INV2, I1, I2, I3, USR)               
!   INPUT ARGUMENT LIST:                                                
!     NODE     - INTEGER: ....                                          
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     INV1     - INTEGER: ....                                          
!     INV2     - INTEGER: ....                                          
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR              
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF USR             
!     I3       - INTEGER: LENGTH OF THIRD DIMENSION OF USR              
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     USR      - REAL*8:(I1,I2,I3) STARTING ADDRESS OF DATA VALUES READ 
!                FROM DATA SUBSET, EVENTS ARE RETURNED IN THE THIRD     
!                DIMENSION FOR A PARTICULAR DATA VALUE AND LEVEL IN THE 
!                FIRST AND SECOND DIMENSIONS                            
!     NEVN     - INTEGER: NUMBER OF EVENTS IN STACK (MUST BE LESS THAN  
!                OR EQUAL TO I3)                                        
!                                                                       
! REMARKS:                                                              
!    IMPORTANT: THIS ROUTINE SHOULD ONLY BE CALLED BY ROUTINE UFBIN3,   
!               WHICH, ITSELF, IS CALLED ONLY BY VERIFICATION           
!               APPLICATION PROGRAM GRIDTOBS, WHERE IT WAS PREVIOUSLY   
!               AN IN-LINE SUBROUTINE.  IN GENERAL, NEVN DOES NOT WORK  
!               PROPERLY IN OTHER APPLICATION PROGRAMS AT THIS TIME.    
!                                                                       
!    THIS ROUTINE CALLS:        BORT     INVWIN   LSTJPB                
!    THIS ROUTINE IS CALLED BY: UFBIN3                                  
!                               Should NOT be called by any             
!                               application programs!!!                 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
      CHARACTER(128) BORT_STR 
      DIMENSION USR (I1, I2, I3) 
      REAL(8) USR !, VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      NEVN = 0 
                                                                        
!  FIND THE ENCLOSING EVENT STACK DESCRIPTOR                            
!  -----------------------------------------                            
                                                                        
      NDRS = LSTJPB (NODE, LUN, 'DRS') 
      IF (NDRS.LE.0) GOTO 100 
                                                                        
      INVN = INVWIN (NDRS, LUN, INV1, INV2) 
      IF (INVN.EQ.0) GOTO 900 
                                                                        
      NEVN = VAL (INVN, LUN) 
      IF (NEVN.GT.I3) GOTO 901 
                                                                        
!  SEARCH EACH STACK LEVEL FOR THE REQUESTED NODE AND COPY THE VALUE    
!  -----------------------------------------------------------------    
                                                                        
      N2 = INVN + 1 
                                                                        
      DO L = 1, NEVN 
      N1 = N2 
      N2 = N2 + VAL (N1, LUN) 
      DO N = N1, N2 
      IF (INV (N, LUN) .EQ.NODE) USR (1, 1, L) = VAL (N, LUN) 
      ENDDO 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: NEVN - CAN''T FIND THE EVENT STACK!!!!!!') 
  901 WRITE (BORT_STR, '("BUFRLIB: NEVN - THE NO. OF EVENTS FOR THE '// &
      'REQUESTED STACK (",I3,") EXCEEDS THE VALUE OF THE 3RD DIM. OF'// &
      ' THE USR ARRAY (",I3,")")') NEVN, I3                             
      CALL BORT (BORT_STR) 
      END FUNCTION NEVN                             
