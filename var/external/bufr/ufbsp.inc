      SUBROUTINE UFBSP (LUN, USR, I1, I2, IO, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBSP (docblock incomplete)                            
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1999-11-18           
!                                                                       
! ABSTRACT: THIS SUBROUTINE WRITES OR READS                             
!  ....                                                                 
!  THIS ROUTINE IS ONLY CALLED BY UFBSTP AND SHOULD NEVER BE CALLED BY  
!  ANY APPLICATION PROGRAM (APPLICATION PROGRAMS SHOULD ALWAYS CALL     
!  BUFR ARCHIVE LIBRARY SUBROUTINE UFBSTP TO PERFORM THESE FUNCTIONS).  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1999-11-18  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY) (INCOMPLETE)                       
!                                                                       
! USAGE:    CALL UFBSP (LUN, USR, I1, I2, IO, IRET)                     
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     USR      - ONLY IF BUFR FILE OPEN FOR OUTPUT:                     
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES     
!                   WRITTEN TO DATA SUBSET                              
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR              
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF USR             
!     IO       - INTEGER: STATUS INDICATOR FOR BUFR FILE ASSOCIATED     
!                WITH LUN:                                              
!                       0 = input file                                  
!                       1 = output file                                 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     USR      - ONLY IF BUFR FILE OPEN FOR INPUT:                      
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES     
!                   READ FROM DATA SUBSET                               
!     IRET     - INTEGER:                                               
!                  - IF BUFR FILE OPEN FOR INPUT: NUMBER OF "LEVELS" OF 
!                    DATA VALUES READ FROM DATA SUBSET (MUST BE NO      
!                    LARGER THAN I2)                                    
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"   
!                    OF DATA VALUES WRITTEN TO DATA SUBSET (SHOULD BE   
!                    SAME AS I2)                                        
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        INVTAG                                  
!    THIS ROUTINE IS CALLED BY: UFBSTP                                  
!                               Normally not called by any application  
!                               programs (they should call UFBSTP).     
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), IVLS (10),    &
      KONS (10)                                                         
                                                                        
      REAL(8) USR (I1, I2)!, VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IRET = 0 
      INS1 = 0 
      INS2 = 0 
                                                                        
!  FRAME A SECTION OF THE BUFFER - RETURN WHEN NO FRAME                 
!  ----------------------------------------------------                 
                                                                        
    1 IF (INS1 + 1.GT.NVAL (LUN) ) GOTO 100 
      INS1 = INVTAG (NODS (1), LUN, INS1 + 1, NVAL (LUN) ) 
      IF (INS1.EQ.0) GOTO 100 
                                                                        
      INS2 = INVTAG (NODS (1), LUN, INS1 + 1, NVAL (LUN) ) 
      IF (INS2.EQ.0) INS2 = NVAL (LUN) 
      IRET = IRET + 1 
                                                                        
!  READ USER VALUES                                                     
!  ----------------                                                     
                                                                        
      IF (IO.EQ.0.AND.IRET.LE.I2) THEN 
         INVM = INS1 
         DO I = 1, NNOD 
         IF (NODS (I) .GT.0) THEN 
            INVN = INVTAG (NODS (I), LUN, INVM, INS2) 
            IF (INVN.GT.0) USR (I, IRET) = VAL (INVN, LUN) 
            INVM = MAX (INVN, INVM) 
         ENDIF 
         ENDDO 
      ENDIF 
                                                                        
!  WRITE USER VALUES                                                    
!  -----------------                                                    
                                                                        
      IF (IO.EQ.1.AND.IRET.LE.I2) THEN 
         INVM = INS1 
         DO I = 1, NNOD 
         IF (NODS (I) .GT.0) THEN 
            INVN = INVTAG (NODS (I), LUN, INVM, INS2) 
            IF (INVN.GT.0) VAL (INVN, LUN) = USR (I, IRET) 
            INVM = MAX (INVN, INVM) 
         ENDIF 
         ENDDO 
      ENDIF 
                                                                        
!  GO FOR NEXT FRAME                                                    
!  -----------------                                                    
                                                                        
      GOTO 1 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE UFBSP                          
