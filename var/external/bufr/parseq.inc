      SUBROUTINE PARSEQ (STR, TAGS, MTAG, NTAG) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    PARSEQ                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE PARSES A STRING CONTAINING ONE OR MORE      
!   MNEMONICS INTO AN ARRAY OF MNEMONICS.  THE MNEMONICS WITHIN THE     
!   STRING MUST BE SEPARATED BY ONE OR MORE BLANK CHARACTERS.           
!   THIS SUBROUTINE IS CONSIDERED OBSOLETE, AND APPLICATION CODES       
!   SHOULD NOW INSTEAD USE THE NEWER BUFR ARCHIVE LIBRARY SUBROUTINE    
!   PARSTR WHICH IS MORE FLEXIBLE BECAUSE IT ALLOWS FOR OTHER           
!   SEPARATORS BESIDES JUST THE BLANK CHARACTER.                        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2 
! 2007-01-19  J. ATOR    -- MARKED AS OBSOLETE AND ADDED PRINT          
!                           NOTIFICATION                                
!                                                                       
! USAGE:    CALL PARSEQ (STR, TAGS, MTAG, NTAG)                         
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING                                  
!     MTAG     - INTEGER: MAXIMUM NUMBER OF MNEMONICS TO BE PARSED      
!                FROM STRING                                            
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     TAGS     - CHARACTER*(*): MTAG-WORD ARRAY OF MNEMONICS (FIRST     
!                NTAG WORDS FILLED)                                     
!     NTAG     - INTEGER: NUMBER OF MNEMONICS RETURNED                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT2                                   
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR, TAGS (MTAG) 
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER(80) ASTR 
      LOGICAL WORD 
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
      DATA IFIRST / 0 / 
                                                                        
      SAVE IFIRST 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (IFIRST.EQ.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT 101 
  101 FORMAT(' BUFRLIB: PARSEQ - THIS SUBROUTINE IS NOW OBSOLETE; ',    &
     & 'USE SUBROUTINE PARSTR INSTEAD')                                 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT * 
         ENDIF 
         IFIRST = 1 
      ENDIF 
                                                                        
      ASTR = STR 
      LSTR = LEN (STR) 
      LTAG = LEN (TAGS (1) ) 
      IF (LSTR.GT.80) GOTO 900 
      NTAG = 0 
      NCHR = 0 
      WORD = .FALSE. 
                                                                        
      DO I = 1, LSTR 
                                                                        
      IF (.NOT.WORD.AND.STR (I:I) .NE.' ') THEN 
         NTAG = NTAG + 1 
         IF (NTAG.GT.MTAG) GOTO 901 
         TAGS (NTAG) = ' ' 
      ENDIF 
                                                                        
      IF (WORD.AND.STR (I:I) .EQ.' ') NCHR = 0 
      WORD = STR (I:I) .NE.' ' 
                                                                        
      IF (WORD) THEN 
         NCHR = NCHR + 1 
         IF (NCHR.GT.LTAG) GOTO 902 
         TAGS (NTAG) (NCHR:NCHR) = STR (I:I) 
      ENDIF 
                                                                        
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR1, '("BUFRLIB: PARSEQ - INPUT STRING (",A,") HAS ")&
     &') STR                                                            
      WRITE (BORT_STR2, '(18X,"LENGTH (",I4,"), > LIMIT OF 80 CHAR.")') &
      LSTR                                                              
      CALL BORT2 (BORT_STR1, BORT_STR2) 
  901 WRITE (BORT_STR1, '("BUFRLIB: PARSEQ - INPUT STRING (",A,") '//   &
      'CONTAINS",I4)') STR, NTAG                                        
      WRITE (BORT_STR2, '(18X,"MNEMONICS, THIS EXCEEDS THE LIMIT {",I4,'&
     &//'" - THIRD (INPUT) ARGUMENT}")') MTAG                           
      CALL BORT2 (BORT_STR1, BORT_STR2) 
  902 WRITE (BORT_STR1, '("BUFRLIB: PARSEQ - INPUT STRING (",A,") ")')  &
      STR                                                               
      WRITE (BORT_STR2, '(18X,"CONTAINS A PARSED MNEMONIC WITH LENGTH '/&
     &/'EXCEEDING THE MAXIMIM OF",I4," CHARACTERS")') LTAG              
      CALL BORT2 (BORT_STR1, BORT_STR2) 
      END SUBROUTINE PARSEQ                         
