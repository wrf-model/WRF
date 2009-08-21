      SUBROUTINE DRFINI (LUNIT, MDRF, NDRF, DRFTAG) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    DRFINI                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2002-05-14           
!                                                                       
! ABSTRACT:  THIS SUBROUTINE INITIALIZES DELAYED REPLICATION FACTORS    
!   AND EXPLICITLY ALLOCATES A CORRESPONDING AMOUNT OF SPACE IN THE     
!   INTERNAL SUBSET ARRAYS, THEREBY ALLOWING THE SUBSEQUENT USE OF BUFR 
!   ARCHIVE LIBRARY SUBROUTINE UFBSEQ TO WRITE DATA DIRECTLY INTO       
!   DELAYED REPLICATION SEQUENCES.  NOTE THAT THIS SAME TYPE OF         
!   INITIALIZATION IS DONE IMPLICTLY WITHIN BUFR ARCHIVE LIBRARY        
!   SUBROUTINE UFBINT FOR DELAYED REPLICATION SEQUENCES WHICH APPEAR    
!   ONLY ONE TIME WITHIN AN OVERALL SUBSET DEFINITION.  HOWEVER, BY     
!   USING SUBROUTINE DRFINI ALONG WITH A SUBSEQUENT CALL TO SUBROUTINE  
!   UFBSEQ, IT IS ACTUALLY POSSIBLE TO HAVE MULTIPLE OCCURRENCES OF A   
!   PARTICULAR DELAYED REPLICATION SEQUENCE WITHIN A SINGLE OVERALL     
!   SUBSET DEFINITION.                                                  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
! 2005-03-04  J. ATOR    -- UPDATED DOCUMENTATION                       
!                                                                       
! USAGE:    CALL DRFINI (LUNIT, MDRF, NDRF, DRFTAG)                     
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     MDRF     - INTEGER: ARRAY OF DELAYED REPLICATION FACTORS,         
!                IN ONE-TO-ONE CORRESPONDENCE WITH THE NUMBER OF        
!                OCCURRENCES OF DRFTAG WITHIN THE OVERALL SUBSET        
!                DEFINITION, AND EXPLICITLY DEFINING HOW MUCH SPACE     
!                (I.E. HOW MANY REPLICATIONS) TO ALLOCATE WITHIN        
!                EACH SUCCESSIVE OCCURRENCE                             
!     NDRF     - INTEGER: NUMBER OF DELAYED REPLICATION FACTORS         
!                WITHIN MDRF                                            
!     DRFTAG   - CHARACTER*(*): SEQUENCE MNEMONIC, BRACKETED BY         
!                APPROPRIATE DELAYED REPLICATION NOTATION               
!                (E.G. {}, () OR <>)                                    
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     STATUS   USRTPL                
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs                                
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
      CHARACTER ( * ) DRFTAG 
      CHARACTER(128) BORT_STR 
!     CHARACTER(10) TAG 
!     CHARACTER(3) TYP 
!     REAL(8) VAL 
      DIMENSION MDRF (NDRF) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (NDRF.GT.100) GOTO 900 
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
                                                                        
!  COMFORM THE TEMPLATES TO THE DELAYED REPLICATION FACTORS             
!  --------------------------------------------------------             
                                                                        
      M = 0 
      N = 0 
                                                                        
   10 DO N = N + 1, NVAL (LUN) 
      NODE = INV (N, LUN) 
      IF (ITP (NODE) .EQ.1.AND.TAG (NODE) .EQ.DRFTAG) THEN 
         M = M + 1 
         CALL USRTPL (LUN, N, MDRF (M) ) 
         GOTO 10 
      ENDIF 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: DRFINI - THE NUMBER OF DELAYED '//   &
      'REPLICATION FACTORS (",I5,") EXCEEDS THE LIMIT (100)")') NDRF    
      CALL BORT (BORT_STR) 
      END SUBROUTINE DRFINI                         
