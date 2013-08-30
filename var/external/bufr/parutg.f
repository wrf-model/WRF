      SUBROUTINE PARUTG(LUN,IO,UTG,NOD,KON,VAL)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PARUTG
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE PARSES A USER-SPECIFIED TAG (MNEMONIC)
C   (UTG) THAT REPRESENTS A VALUE EITHER BEING DECODED FROM A BUFR FILE
C   (IF IT IS BEING READ) OR ENCODED INTO A BUFR FILE (IF IT IS BEING
C   WRITTEN).  THIS SUBROUTINE FIRST CHECKS TO SEE IF THE TAG CONTAINS
C   A CONDITION CHARACTER ('=', '!', '<', '>', '^' OR '#').  IF IT DOES
C   NOT, NOTHING HAPPENS AT THIS POINT.  IF IT DOES, THEN THE TYPE OF
C   CONDITION CHARACTER IS NOTED AND THE TAG IS STRIPPED OF ALL
C   CHARACTERS AT AND BEYOND THE CONDITION CHARACTER.  IN EITHER EVENT,
C   THE RESULTANT TAG IS CHECKED AGAINST THOSE IN THE INTERNAL JUMP/
C   LINK SUBSET TABLE (IN COMMON BLOCK /BTABLES/).  IF FOUND, THE NODE
C   ASSOCIATED WITH THE TAG IS RETURNED (AND IT IS EITHER A "CONDITION"
C   NODE OR A "STORE" NODE DEPENDING OF THE PRESENCE OR ABSENCE OF A
C   CONDITION CHARACTER IN UTG).  OTHERWISE THE NODE IS RETURNED AS
C   ZERO.  IF THE TAG REPRESENTS A CONDITION NODE, THEN THE CONDITION
C   VALUE (NUMERIC CHARACTERS BEYOND THE CONDITION CHARACTER IN THE
C   USER-SPECIFIED TAG INPUT HERE) IS RETURNED.
C
C   AS AN EXAMPLE OF CONDITION CHARACTER USAGE, CONSIDER THE FOLLOWING
C   EXAMPLE OF A CALL TO UFBINT:
C
C      REAL*8 USR(4,50)
C             ....
C             ....
C      CALL UFBINT(LUNIN,USR,4,50,IRET,'PRLC<50000 TMDB WDIR WSPD')
C
C   ASSUMING THAT LUNIN POINTS TO A BUFR FILE OPEN FOR INPUT (READING),
C   THEN THE USR ARRAY NOW CONTAINS IRET LEVELS OF DATA (UP TO A MAXIMUM
C   OF 50!) WHERE THE VALUE OF PRLC IS/WAS LESS THAN 50000, ALONG WITH
C   THE CORRESPONDING VALUES FOR TMDB, WDIR AND WSPD AT THOSE LEVELS. 
C
C   AS ANOTHER EXAMPLE, CONSIDER THE FOLLOWING EXAMPLE OF A CALL TO
C   READLC FOR A LONG CHARACTER STRING:
C
C      CHARACTER*200 LCHR
C             ....
C             ....
C      CALL READLC(LUNIN,LCHR,'NUMID#3')
C
C   ASSUMING THAT LUNIN POINTS TO A BUFR FILE OPEN FOR INPUT (READING),
C   THEN THE LCHR STRING NOW CONTAINS THE VALUE CORRESPONDING TO THE 
C   THIRD OCCURRENCE OF NUMID WITHIN THE CURRENT SUBSET.
C
C   VALID CONDITION CODES INCLUDE:
C	'<' - LESS THAN
C       '>' - GREATER THAN
C       '=' - EQUAL TO
C       '!' - NOT EQUAL TO
C       '#' - ORDINAL IDENTIFIER FOR A PARTICULAR OCCURRENCE OF A LONG
C             CHARACTER STRING
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
C                           INFO WHEN ROUTINE TERMINATES ABNORMALLY;
C                           CHANGED CALL FROM BORT TO BORT2 IN SOME
C                           CASES; REPLACED PREVIOUS "RETURN 1"
C                           STATEMENT WITH "GOTO 900" (AND CALL TO
C                           BORT) SINCE THE ONLY ROUTINE THAT CALLS
C                           THIS ROUTINE, PARUSR, USED THIS ALTERNATE
C                           RETURN TO GO TO A STATEMENT WHICH CALLED
C                           BORT
C 2005-04-22  J. ATOR    -- HANDLED SITUATION WHERE INPUT TAG CONTAINS
C                           1-BIT DELAYED REPLICATION, AND IMPROVED
C                           DOCUMENTATION
C 2009-03-23  J. ATOR    -- ADDED '#' CONDITION CODE
C
C USAGE:    CALL PARUTG (LUN, IO, UTG, NOD, KON, VAL)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     IO       - INTEGER: STATUS INDICATOR FOR BUFR FILE ASSOCIATED
C                WITH LUN:
C                       0 = input file
C                       1 = output file
C     UTG      CHARACTER*(*): USER-SUPPLIED TAG REPRESENTING A VALUE TO
C              BE ENCODED/DECODED TO/FROM BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     NOD      - INTEGER: POSITIONAL INDEX IN INTERNAL JUMP/LINK SUBSET
C                TABLE FOR TAG
C                       0 = tag not found in table
C     KON      - INTEGER: INDICATOR FOR TYPE OF CONDITION CHARACTER
C                FOUND IN UTG:
C                      0 = no condition character found (NOD is a store
C                          node)
C                      1 = character '=' found
C                      2 = character '!' found
C                      3 = character '<' found
C                      4 = character '>' found
C                      5 = character '^' found
C                      6 = character '#' found
C                      (1-6 means NOD is a condition node, and
C                       specifically 5 is a "bump" node)
C     VAL      - REAL: CONDITION VALUE ASSOCIATED WITH CONDITION
C                CHARACTER FOUND IN UTG
C                      0 = UTG does not have a condition character
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     BORT2    STRNUM
C    THIS ROUTINE IS CALLED BY: PARUSR   READLC   WRITLC
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES),
     .                INODE(NFILES),IDATE(NFILES)
      COMMON /BTABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /UTGPRM/ PICKY

      CHARACTER*(*) UTG
      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*20  ATAG
      CHARACTER*10  TAG
      CHARACTER*3   TYP,ATYP,BTYP
      CHARACTER*1   COND(6)
      DIMENSION     BTYP(8),IOK(8)
      LOGICAL       PICKY

      DATA NCHK   / 8/
      DATA BTYP   /'SUB','SEQ','REP','RPC','RPS','DRB','DRP','DRS'/
      DATA IOK    /  -1 ,  -1 ,  -1 ,  -1 ,  -1 ,   0 ,   0 ,   0 /

C----------------------------------------------------------------------
C     For now, set PICKY (see below) to always be .FALSE.
      PICKY = .FALSE.
      COND(1) = '='
      COND(2) = '!'
      COND(3) = '<'
      COND(4) = '>'
      COND(5) = '^'
      COND(6) = '#'
      NCOND   = 6
C----------------------------------------------------------------------

      ATAG  = ' '
      ATYP  = ' '
      KON   = 0
      NOD   = 0
      VAL   = 0
      LTG   = MIN(20,LEN(UTG))

C  PARSE UTG, SAVING INTO ATAG ONLY CHARACTERS PRIOR TO CONDITION CHAR.
C  --------------------------------------------------------------------

C     But first, take care of the special case where UTG denotes the
C     short (i.e. 1-bit) delayed replication of a Table D mnemonic.
C     This will prevent confusion later on since '<' and '>' are each
C     also valid as condition characters.

      IF((UTG(1:1).EQ.'<').AND.(INDEX(UTG(3:),'>').NE.0)) THEN
         ATAG = UTG
         GO TO 1
      ENDIF

      DO I=1,LTG
      IF(UTG(I:I).EQ.' ') GOTO 1
      DO J=1,NCOND
      IF(UTG(I:I).EQ.COND(J)) THEN
         KON = J
         ICV = I+1
         GOTO 1
      ENDIF
      ENDDO
      ATAG(I:I) = UTG(I:I)
      ENDDO

C  FIND THE NODE ASSOCIATED WITH ATAG IN THE SUBSET TABLE
C  ------------------------------------------------------

1     INOD = INODE(LUN)
      DO NOD=INOD,ISC(INOD)
      IF(ATAG.EQ.TAG(NOD)) GOTO 2
      ENDDO

C  ATAG NOT FOUND IN SUBSET TABLE
C  ------------------------------

C     So what do we want to do?  We could be "picky" and abort right
C     here, or we could allow for the possibility that, e.g. a user
C     application has been streamlined to always call UFBINT with the
C     same STR, even though some of the mnemonics contained within that
C     STR may not exist within the sequence definition of every
C     possible type/subtype that is being written by the application.
C     In such cases, by not being "picky", we could just allow BUFRLIB
C     to subsequently (and quietly, if IPRT happened to be set to -1
C     in COMMON /QUIET/!) not actually store the value corresponding
C     to such mnemonics, rather than loudly complaining and aborting. 

      IF(KON.EQ.0 .AND. (IO.EQ.0.OR.ATAG.EQ.'NUL'.OR..NOT.PICKY)) THEN
C        i.e. (if this tag does not contain any condition characters)
C                 .AND.
C             ((either the file is open for input) .OR.
C              (the tag consists of 'NUL') .OR.
C              (we aren't being "picky"))
         NOD = 0
         GOTO 100
      ELSE
C        abort...
         GOTO 900
      ENDIF

C  ATAG IS FOUND IN SUBSET TABLE, MAKE SURE IT HAS A VALID NODE TYPE
C  -----------------------------------------------------------------

2     IF(KON.EQ.5) THEN
c  .... Cond. char "^" must be assoc. with a delayed replication
c       sequence (this is a "bump" node) (Note: This is obsolete but
c       remains for "old" programs using the BUFR ARCHIVE LIBRARY)
         IF(TYP(NOD-1).NE.'DRP' .AND. TYP(NOD-1).NE.'DRS') GOTO 901
      ELSEIF(KON.NE.6) THEN
C        Allow reading (but not writing) of delayed replication factors.
         ATYP = TYP(NOD)
         DO I=1,NCHK
           IF(ATYP.EQ.BTYP(I) .AND. IO.GT.IOK(I)) GOTO 902
         ENDDO
      ENDIF

C  IF CONDITION NODE, GET CONDITION VALUE WHICH IS A NUMBER FOLLOWING IT
C  ---------------------------------------------------------------------

      IF(KON.NE.0) THEN
         CALL STRNUM(UTG(ICV:LTG),NUM)
         IF(NUM.LT.0) GOTO 903
         VAL = NUM
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - TRYING TO WRITE A MNEMONIC'//
     . ' (",A,") WHICH DOES NOT EXIST IN SUBSET TABLE")') ATAG
      WRITE(BORT_STR2,'(18X,"(UPON INPUT, IT CONTAINED THE CONDITION '//
     . 'CHARACTER ",A,")")') UTG(ICV-1:ICV-1)
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - BUMP NODE (MNEMONIC ",A,")'//
     . ' MUST REFER TO A DELAYED REPLICATION SEQUENCE, HERE TYPE IS "'//
     . ',A)') ATAG,TYP(NOD-1)
      CALL BORT(BORT_STR1)
902   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - ILLEGAL NODE TYPE: ",A," '//
     . 'FOR MNEMONIC ",A)') ATYP,ATAG
      CALL BORT(BORT_STR1)
903   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - CONDITION VALUE IN '//
     . 'MNEMONIC ",A," ILLEGAL BECAUSE ALL OTHER CHARACTERS IN '//
     . 'MNEMONIC MUST BE NUMERIC")') UTG
      CALL BORT(BORT_STR1)
      END
