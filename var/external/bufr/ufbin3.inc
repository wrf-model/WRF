      SUBROUTINE UFBIN3 (LUNIT, USR, I1, I2, I3, IRET, JRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBIN3                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS SPECIFIED VALUES FROM THE CURRENT     
!   BUFR DATA SUBSET WITHIN INTERNAL ARRAYS.  THE DATA VALUES           
!   CORRESPOND TO MNEMONICS WHICH ARE PART OF A MULTIPLE-REPLICATION    
!   SEQUENCE WITHIN ANOTHER MULTIPLE-REPLICATION SEQUENCE.  THE INNER   
!   SEQUENCE IS USUALLY ASSOCIATED WITH DATA "LEVELS" AND THE OUTER     
!   SEQUENCE WITH DATA "EVENTS".  THE BUFR FILE IN LOGICAL UNIT LUNIT   
!   MUST HAVE BEEN OPENED FOR INPUT VIA A PREVIOUS CALL TO BUFR ARCHIVE 
!   LIBRARY SUBROUTINE OPENBF.  IN ADDITION, THE DATA SUBSET MUST HAVE  
!   SUBSEQUENTLY BEEN READ INTO THE INTERNAL BUFR ARCHIVE LIBRARY       
!   ARRAYS VIA CALLS TO BUFR ARCHIVE LIBRARY SUBROUTINE READMG OR       
!   READERME FOLLOWED BY A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE      
!   READSB (OR VIA A SINGLE CALL TO BUFR ARCHIVE LIBRARY                
!   SUBROUTINE READNS).  THIS SUBROUTINE IS DESIGNED TO READ EVENT      
!   INFORMATION FROM "PREPFITS" TYPE BUFR FILES (BUT NOT FROM           
!   "PREPBUFR" TYPE FILES!!).  PREPFITS FILES HAVE THE FOLLOWING BUFR   
!   TABLE EVENT STRUCTURE (NOTE SIXTEEN CHARACTERS HAVE BEEN REMOVED    
!   FROM THE LAST COLUMN TO ALLOW THE TABLE TO FIT IN THIS DOCBLOCK):   
!                                                                       
!   | ADPUPA   | HEADR  {PLEVL}                                    |    
!   | HEADR    | SID XOB YOB DHR ELV TYP T29 ITP                   |    
!   | PLEVL    | CAT PRC PQM QQM TQM ZQM WQM CDTP_QM [OBLVL]       |    
!   | OBLVL    | SRC FHR <PEVN> <QEVN> <TEVN> <ZEVN> <WEVN> <CEVN> |    
!   | OBLVL    | <CTPEVN>                                          |    
!   | PEVN     | POB  PMO                                          |    
!   | QEVN     | QOB                                               |    
!   | TEVN     | TOB                                               |    
!   | ZEVN     | ZOB                                               |    
!   | WEVN     | UOB  VOB                                          |    
!   | CEVN     | CAPE CINH LI                                      |    
!   | CTPEVN   | CDTP GCDTT TOCC                                   |    
!                                                                       
!   NOTE THAT THE ONE-BIT DELAYED REPLICATED SEQUENCES "<xxxx>" ARE     
!   NESTED INSIDE THE EIGHT-BIT DELAYED REPLIATION EVENT SEQUENCES      
!   "[yyyy]".  THE ANALOGOUS BUFR ARCHIVE LIBRARY SUBROUTINE UFBEVN     
!   DOES NOT WORK PROPERLY ON THIS TYPE OF EVENT STRUCTURE.  IT WORKS   
!   ONLY ON THE EVENT STRUCTURE FOUND IN "PREPBUFR" TYPE BUFR FILES     
!   (SEE UFBEVN FOR MORE DETAILS).  IN TURN, UFBIN3 DOES NOT WORK       
!   PROPERLY ON THE EVENT STRUCTURE FOUND IN PREPBUFR FILES (ALWAYS USE 
!   UFBEVN IN THIS CASE).  ONE OTHER DIFFERENCE BETWEEN UFBIN3 AND      
!   UFBEVN IS THAT UFBIN3 RETURNS THE MAXIMUM NUMBER OF EVENTS FOUND    
!   FOR ALL DATA VALUES SPECIFIED AS AN OUTPUT ARGUMENT (JRET).  UFBEVN 
!   DOES NOT DO THIS, BUT RATHER IT STORES THIS VALUE INTERNALLY IN     
!   COMMON BLOCK /UFBN3C/.                                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN VERIFICATION        
!                           VERSION)                                    
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY OR UNUSUAL THINGS HAPPEN         
!                                                                       
! USAGE:    CALL UFBIN3 (LUNIT, USR, I1, I2, I3, IRET, JRET, STR)       
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE       
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER     
!                MUST BE .GE. LATTER)                                   
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF USR             
!     I3       - INTEGER: LENGTH OF THIRD DIMENSION OF USR (MAXIMUM     
!                VALUE IS 255)                                          
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B       
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST      
!                DIMENSION OF USR                                       
!                  - THERE ARE THREE "GENERIC" MNEMONICS NOT RELATED    
!                     TO TABLE B, THESE RETURN THE FOLLOWING            
!                     INFORMATION IN CORRESPONDING USR LOCATION:        
!                     'NUL'  WHICH ALWAYS RETURNS MISSING (10E10)       
!                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR      
!                            MESSAGE (RECORD) NUMBER IN WHICH THIS      
!                            SUBSET RESIDES                             
!                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET    
!                            NUMBER OF THIS SUBSET WITHIN THE BUFR      
!                            MESSAGE (RECORD) NUMBER 'IREC'             
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     USR      - REAL*8: (I1,I2,I3) STARTING ADDRESS OF DATA VALUES     
!                READ FROM DATA SUBSET                                  
!     IRET     - INTEGER: NUMBER OF "LEVELS" OF DATA VALUES READ FROM   
!                DATA SUBSET (MUST BE NO LARGER THAN I2)                
!     JRET     - INTEGER: MAXIMUM NUMBER OF "EVENTS" FOUND FOR ALL DATA 
!                VALUES SPECIFIED AMONGST ALL LEVELS READ FROM DATA     
!                SUBSET (MUST BE NO LARGER THAN I3)                     
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    IMPORTANT: THIS ROUTINE SHOULD ONLY BE CALLED BY THE VERIFICATION  
!               APPLICATION PROGRAM "GRIDTOBS", WHERE IT WAS PREVIOUSLY 
!               AN IN-LINE SUBROUTINE.  IN GENERAL, UFBIN3 DOES NOT     
!               WORK PROPERLY IN OTHER APPLICATION PROGRAMS (I.E, THOSE 
!               THAT ARE READING PREPBUFR FILES) AT THIS TIME.  ALWAYS  
!               USE UFBEVN INSTEAD!!                                    
!                                                                       
!    THIS ROUTINE CALLS:        BORT     CONWIN   GETWIN   NEVN         
!                               NXTWIN   STATUS   STRING                
!    THIS ROUTINE IS CALLED BY: None                                    
!                               SHOULD NOT BE CALLED BY ANY APPLICATION 
!                               PROGRAMS EXCEPT GRIDTOBS!!              
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), IVLS (10),    &
      KONS (10)                                                         
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER ( * ) STR 
      REAL(8) USR (I1, I2, I3) !, VAL
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IRET = 0 
      JRET = 0 
                                                                        
!  CHECK THE FILE STATUS AND I-NODE                                     
!  --------------------------------                                     
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (INODE (LUN) .NE.INV (1, LUN) ) GOTO 903 
                                                                        
      IF (I1.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBIN3 - THIRD ARGUMENT (INPUT) IS .LE. 0', ' &
     &-  RETURN WITH SIXTH AND SEVENTH ARGUMENTS (IRET, JRET) = 0'      
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ELSEIF (I2.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBIN3 - FOURTH ARGUMENT (INPUT) IS .LE. 0', '&
     & -  RETURN WITH SIXTH AND SEVENTH ARGUMENTS (IRET, JRET) = 0'     
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ELSEIF (I3.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBIN3 - FIFTH ARGUMENT (INPUT) IS .LE. 0', ' &
     &-  RETURN WITH SIXTH AND SEVENTH ARGUMENTS (IRET, JRET) = 0'      
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ENDIF 
                                                                        
!  PARSE OR RECALL THE INPUT STRING                                     
!  --------------------------------                                     
                                                                        
      CALL STRING (STR, LUN, I1, 0) 
                                                                        
!  INITIALIZE USR ARRAY                                                 
!  --------------------                                                 
                                                                        
      DO K = 1, I3 
      DO J = 1, I2 
      DO I = 1, I1 
      USR (I, J, K) = BMISS 
      ENDDO 
      ENDDO 
      ENDDO 
                                                                        
!  LOOP OVER COND WINDOWS                                               
!  ----------------------                                               
                                                                        
      INC1 = 1 
      INC2 = 1 
                                                                        
    1 CALL CONWIN (LUN, INC1, INC2, I2) 
      IF (NNOD.EQ.0) THEN 
         IRET = I2 
         GOTO 100 
      ELSEIF (INC1.EQ.0) THEN 
         GOTO 100 
      ELSE 
         DO I = 1, NNOD 
         IF (NODS (I) .GT.0) THEN 
            INS2 = INC1 
            CALL GETWIN (NODS (I), LUN, INS1, INS2) 
            IF (INS1.EQ.0) GOTO 100 
            GOTO 2 
         ENDIF 
         ENDDO 
         INS1 = INC1 
         INS2 = INC2 
      ENDIF 
                                                                        
!  READ PUSH DOWN STACK DATA INTO 3D ARRAYS                             
!  ----------------------------------------                             
                                                                        
    2 IRET = IRET + 1 
      IF (IRET.LE.I2) THEN 
         DO I = 1, NNOD 
         NNVN = NEVN (NODS (I), LUN, INS1, INS2, I1, I2, I3, USR (I,    &
         IRET, 1) )                                                     
         JRET = MAX (JRET, NNVN) 
         ENDDO 
      ENDIF 
                                                                        
!  DECIDE WHAT TO DO NEXT                                               
!  ----------------------                                               
                                                                        
      CALL NXTWIN (LUN, INS1, INS2) 
      IF (INS1.GT.0.AND.INS1.LT.INC2) GOTO 2 
      IF (NCON.GT.0) GOTO 1 
                                                                        
      IF (IRET.EQ.0.OR.JRET.EQ.0) THEN 
         IF (IPRT.GE.1) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBIN3 - NO SPECIFIED VALUES READ IN - ', 'RET&
     &URN WITH SIXTH ARGUMENT (IRET) = 0 AND/OR SEVENTH ', 'ARGUMENT (JR&
     &ET) = 0'                                                          
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: UFBIN3 - INPUT BUFR FILE IS CLOSED, IT MUST'/&
     &/' BE OPEN FOR INPUT')                                            
  901 CALL BORT ('BUFRLIB: UFBIN3 - INPUT BUFR FILE IS OPEN FOR OUTPUT'/&
     &/', IT MUST BE OPEN FOR INPUT')                                   
  902 CALL BORT ('BUFRLIB: UFBIN3 - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: UFBIN3 - LOCATION OF INTERNAL TABLE FOR '//  &
      'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//     &
      'INTERNAL SUBSET ARRAY')                                          
      END SUBROUTINE UFBIN3                         
