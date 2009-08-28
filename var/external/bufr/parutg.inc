      SUBROUTINE PARUTG (LUN, IO, UTG, NOD, KON, VAL) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    PARUTG                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE PARSES A USER-SPECIFIED TAG (MNEMONIC)      
!   (UTG) THAT REPRESENTS A VALUE EITHER BEING DECODED FROM A BUFR FILE 
!   (IF IT IS BEING READ) OR ENCODED INTO A BUFR FILE (IF IT IS BEING   
!   WRITTEN).  THIS SUBROUTINE FIRST CHECKS TO SEE IF THE TAG CONTAINS  
!   A CONDITION CHARACTER ('=', '!', '<', '>' OR '^').  IF IT DOES NOT, 
!   NOTHING HAPPENS AT THIS POINT.  IF IT DOES, THEN THE TYPE OF        
!   CONDITION CHARACTER IS NOTED AND THE TAG IS STRIPPED OF ALL         
!   CHARACTERS AT AND BEYOND THE CONDITION CHARACTER.  IN EITHER EVENT, 
!   THE RESULTANT TAG IS CHECKED AGAINST THOSE IN THE INTERNAL JUMP/    
!   LINK SUBSET TABLE (IN COMMON BLOCK /TABLES/).  IF FOUND, THE NODE   
!   ASSOCIATED WITH THE TAG IS RETURNED (AND IT IS EITHER A "CONDITION" 
!   NODE OR A "STORE" NODE DEPENDING OF THE PRESENCE OR ABSENCE OF A    
!   CONDITION CHARACTER IN UTG).  OTHERWISE THE NODE IS RETURNED AS     
!   ZERO.  IF THE TAG REPRESENTS A CONDITION NODE, THEN THE CONDITION   
!   VALUE (NUMERIC CHARACTERS BEYOND THE CONDITION CHARACTER IN THE     
!   USER-SPECIFIED TAG INPUT HERE) IS RETURNED.                         
!                                                                       
!   AS AN EXAMPLE OF CONDITION CHARACTER USAGE, CONSIDER THE FOLLOWING  
!   EXAMPLE OF A CALL TO UFBINT:                                        
!                                                                       
!      REAL*8 USR(4,50)                                                 
!             ....                                                      
!             ....                                                      
!      CALL UFBINT(LUNIN,USR,4,50,IRET,'PRLC<50000 TMDB WDIR WSPD')     
!                                                                       
!   ASSUMING THAT LUNIN POINTS TO A BUFR FILE OPEN FOR INPUT (READING), 
!   THEN THE USR ARRAY NOW CONTAINS IRET LEVELS OF DATA (UP TO A MAXIMUM
!   OF 50!) WHERE THE VALUE OF PRLC IS/WAS LESS THAN 50000, ALONG WITH  
!   THE CORRESPONDING VALUES FOR TMDB, WDIR AND WSPD AT THOSE LEVELS.   
!                                                                       
!   VALID CONDITION CODES INCLUDE:                                      
!       '<' - LESS THAN                                                 
!       '>' - GREATER THAN                                              
!       '=' - EQUAL TO                                                  
!       '!' - NOT EQUAL TO                                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY;    
!                           CHANGED CALL FROM BORT TO BORT2 IN SOME     
!                           CASES; REPLACED PREVIOUS "RETURN 1"         
!                           STATEMENT WITH "GOTO 900" (AND CALL TO      
!                           BORT) SINCE THE ONLY ROUTINE THAT CALLS     
!                           THIS ROUTINE, PARUSR, USED THIS ALTERNATE   
!                           RETURN TO GO TO A STATEMENT WHICH CALLED    
!                           BORT                                        
! 2005-04-22  J. ATOR    -- HANDLED SITUATION WHERE INPUT TAG CONTAINS  
!                           1-BIT DELAYED REPLICATION, AND IMPROVED     
!                           DOCUMENTATION                               
!                                                                       
! USAGE:    CALL PARUTG (LUN, IO, UTG, NOD, KON, VAL)                   
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     IO       - INTEGER: STATUS INDICATOR FOR BUFR FILE ASSOCIATED     
!                WITH LUN:                                              
!                       0 = input file                                  
!                       1 = output file                                 
!     UTG      CHARACTER*20: USER-SUPPLIED TAG REPRESENTING A VALUE TO  
!              BE ENCODED/DECODED TO/FROM BUFR FILE                     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NOD      - INTEGER: POSITIONAL INDEX IN INTERNAL JUMP/LINK SUBSET 
!                TABLE FOR TAG                                          
!                       0 = tag not found in table                      
!     KON      - INTEGER: INDICATOR FOR TYPE OF CONDITION CHARACTER     
!                FOUND IN UTG:                                          
!                      0 = no condition character found (NOD is a store 
!                          node)                                        
!                      1 = character '=' found                          
!                      2 = character '!' found                          
!                      3 = character '<' found                          
!                      4 = character '>' found                          
!                      5 = character '^' found                          
!                      (1-5 means NOD is a condition node, and          
!                       specifically 5 is a "bump" node)                
!     VAL      - REAL: CONDITION VALUE ASSOCIATED WITH CONDITION        
!                CHARACTER FOUND IN UTG                                 
!                      0 = UTG does not have a condition character      
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     BORT2    STRNUM                
!    THIS ROUTINE IS CALLED BY: PARUSR                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
      COMMON / UTGPRM / PICKY 
                                                                        
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER(20) UTG, ATAG 
!     CHARACTER(10) TAG 
      CHARACTER(3) ATYP, BTYP !, TYP
      CHARACTER(1) COND (5) 
      DIMENSION BTYP (8), IOK (8) 
      LOGICAL PICKY 
                                                                        
      DATA NCHK / 8 / 
      DATA BTYP / 'SUB', 'SEQ', 'REP', 'RPC', 'RPS', 'DRB', 'DRP',      &
      'DRS' /                                                           
      DATA IOK / - 1, - 1, - 1, - 1, - 1, 0, 0, 0 / 
      DATA LTG / 20 / 
                                                                        
!---------------------------------------------------------------------- 
!     For now, set PICKY (see below) to always be .FALSE.               
      PICKY = .FALSE. 
      COND (1) = '=' 
      COND (2) = '!' 
      COND (3) = '<' 
      COND (4) = '>' 
      COND (5) = '^' 
      NCOND = 5 
!---------------------------------------------------------------------- 
                                                                        
      ATAG = ' ' 
      ATYP = ' ' 
      KON = 0 
      NOD = 0 
      VAL = 0 
                                                                        
!  PARSE UTG, SAVING INTO ATAG ONLY CHARACTERS PRIOR TO CONDITION CHAR. 
!  -------------------------------------------------------------------- 
                                                                        
!     But first, take care of the special case where UTG denotes the    
!     short (i.e. 1-bit) delayed replication of a Table D mnemonic.     
!     This will prevent confusion later on since '<' and '>' are each   
!     also valid as condition characters.                               
                                                                        
      IF ( (UTG (1:1) .EQ.'<') .AND. (INDEX (UTG (3:) , '>') .NE.0) )   &
      THEN                                                              
      ATAG = UTG 
      GOTO 1 
      ENDIF 
                                                                        
      DO I = 1, LTG 
      IF (UTG (I:I) .EQ.' ') GOTO 1 
      DO J = 1, NCOND 
      IF (UTG (I:I) .EQ.COND (J) ) THEN 
      KON = J 
      ICV = I + 1 
      GOTO 1 
      ENDIF 
      ENDDO 
      ATAG (I:I) = UTG (I:I) 
      ENDDO 
                                                                        
!  FIND THE NODE ASSOCIATED WITH ATAG IN THE SUBSET TABLE               
!  ------------------------------------------------------               
                                                                        
    1 INOD = INODE (LUN) 
      DO NOD = INOD, ISC (INOD) 
      IF (ATAG.EQ.TAG (NOD) ) GOTO 2 
      ENDDO 
                                                                        
!  ATAG NOT FOUND IN SUBSET TABLE                                       
!  ------------------------------                                       
                                                                        
!     So what do we want to do?  We could be "picky" and abort right    
!     here, or we could allow for the possibility that, e.g. a user     
!     application has been streamlined to always call UFBINT with the   
!     same STR, even though some of the mnemonics contained within that 
!     STR may not exist within the sequence definition of every         
!     possible type/subtype that is being written by the application.   
!     In such cases, by not being "picky", we could just allow BUFRLIB  
!     to subsequently (and quietly, if IPRT happened to be set to -1    
!     in COMMON /QUIET/!) not actually store the value corresponding    
!     to such mnemonics, rather than loudly complaining and aborting.   
                                                                        
      IF (KON.EQ.0.AND. (IO.EQ.0.OR.ATAG.EQ.'NUL'.OR..NOT.PICKY) ) THEN 
!        i.e. (if this tag does not contain any condition characters)   
!                 .AND.                                                 
!             ((either the file is open for input) .OR.                 
!              (the tag consists of 'NUL') .OR.                         
!              (we aren't being "picky"))                               
      NOD = 0 
      GOTO 100 
      ELSE 
!        abort...                                                       
      GOTO 900 
      ENDIF 
                                                                        
!  ATAG IS FOUND IN SUBSET TABLE, MAKE SURE IT HAS A VALID NODE TYPE    
!  -----------------------------------------------------------------    
                                                                        
    2 IF (KON.EQ.5) THEN 
!  .... Cond. char "^" must be assoc. with a delayed replication        
!       sequence (this is a "bump" node) (Note: This is obsolete but    
!       remains for "old" programs using the BUFR ARCHIVE LIBRARY)      
      IF (TYP (NOD-1) .NE.'DRP'.AND.TYP (NOD-1) .NE.'DRS') GOTO 901 
      ELSE 
!        Allow reading (but not writing) of delayed replication factors.
      ATYP = TYP (NOD) 
      DO I = 1, NCHK 
      IF (ATYP.EQ.BTYP (I) .AND.IO.GT.IOK (I) ) GOTO 902 
      ENDDO 
      ENDIF 
                                                                        
!  IF CONDITION NODE, GET CONDITION VALUE WHICH IS A NUMBER FOLLOWING IT
!  ---------------------------------------------------------------------
                                                                        
      IF (KON.NE.0) THEN 
      CALL STRNUM (UTG (ICV:LTG), NUM) 
      IF (NUM.LT.0) GOTO 903 
      VAL = NUM 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR1, '("BUFRLIB: PARUTG - TRYING TO WRITE A MNEMONIC'&
     &//' (",A,") WHICH DOES NOT EXIST IN SUBSET TABLE")') ATAG         
      WRITE (BORT_STR2, '(18X,"(UPON INPUT, IT CONTAINED THE CONDITION '&
     &//'CHARACTER ",A,")")') UTG (ICV - 1:ICV - 1)                     
      CALL BORT2 (BORT_STR1, BORT_STR2) 
  901 WRITE (BORT_STR1, '("BUFRLIB: PARUTG - BUMP NODE (MNEMONIC ",A,")'&
     &//' MUST REFER TO A DELAYED REPLICATION SEQUENCE, HERE TYPE IS "'/&
     &/',A)') ATAG, TYP (NOD-1)                                         
      CALL BORT (BORT_STR1) 
  902 WRITE (BORT_STR1, '("BUFRLIB: PARUTG - ILLEGAL NODE TYPE: ",A," '/&
     &/'FOR MNEMONIC ",A)') ATYP, ATAG                                  
      CALL BORT (BORT_STR1) 
  903 WRITE (BORT_STR1, '("BUFRLIB: PARUTG - CONDITION VALUE IN '//     &
      'MNEMONIC ",A," ILLEGAL BECAUSE ALL OTHER CHARACTERS IN '//       &
      'MNEMONIC MUST BE NUMERIC")') UTG                                 
      CALL BORT (BORT_STR1) 
      END SUBROUTINE PARUTG                         
