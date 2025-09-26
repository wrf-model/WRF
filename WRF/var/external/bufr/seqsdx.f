      SUBROUTINE SEQSDX(CARD,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SEQSDX
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE DECODES THE TABLE D SEQUENCE INFORMATION
C   FROM A MNEMONIC DEFINITION CARD THAT WAS PREVIOUSLY READ FROM A
C   USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT BY
C   BUFR ARCHIVE LIBRARY SUBROUTINE RDUSDX.  THESE ARE THEN ADDED TO
C   THE ALREADY-EXISTING ENTRY FOR THAT MNEMONIC (BUILT IN RDUSDX)
C   WITHIN THE INTERNAL BUFR TABLE D ARRAY TABD(*,LUN) IN COMMON BLOCK
C   /TABABD/.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2
C 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR
C
C USAGE:    CALL SEQSDX (CARD, LUN)
C   INPUT ARGUMENT LIST:
C     CARD     - CHARACTER*80: MNEMONIC DEFINITION CARD THAT WAS READ
C                FROM A USER-SUPPLIED BUFR DICTIONARY TABLE
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    BORT2    NEMOCK   NEMTAB
C                               PARSTR   PKTDD    RSVFVM   STRNUM
C    THIS ROUTINE IS CALLED BY: RDUSDX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)

      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*80  CARD,SEQS
      CHARACTER*12  ATAG,TAGS(250)
      CHARACTER*8   NEMO,NEMA,NEMB
      CHARACTER*6   ADN30,CLEMON
      CHARACTER*3   TYPS
      CHARACTER*1   REPS,TAB

      DATA MAXTGS /250/
      DATA MAXTAG /12/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  FIND THE SEQUENCE TAG IN TABLE D AND PARSE THE SEQUENCE STRING
C  --------------------------------------------------------------

      NEMO = CARD( 3:10)
      SEQS = CARD(14:78)

C     Note that an entry for this mnemonic should already exist within
C     the internal BUFR Table D array TABD(*,LUN); this entry should
C     have been created by subroutine RDUSDX when the mnemonic and its
C     associated FXY value and description were initially defined
C     within a card read from the "Descriptor Definition" section at
C     the top of the user-supplied BUFR dictionary table in character
C     format.  Now, we need to retrieve the positional index for that
C     entry within TABD(*,LUN) so that we can access the entry and then
C     add the decoded sequence information to it.

      CALL NEMTAB(LUN,NEMO,IDN,TAB,ISEQ)
      IF(TAB.NE.'D') GOTO 900
      CALL PARSTR(SEQS,TAGS,MAXTGS,NTAG,' ',.TRUE.)
      IF(NTAG.EQ.0 ) GOTO 901

      DO N=1,NTAG
      ATAG = TAGS(N)
      IREP = 0

C  CHECK FOR REPLICATOR
C  --------------------

      DO I=1,5
      IF(ATAG(1:1).EQ.REPS(I,1)) THEN

C        Note that REPS(*,*), which contains all of the symbols used to
C        denote all of the various replication schemes that are
C        possible within a user-supplied BUFR dictionary table in
C        character format, was previously defined within subroutine
C        BFRINI.

         DO J=2,MAXTAG
         IF(ATAG(J:J).EQ.REPS(I,2)) THEN
            IF(J.EQ.MAXTAG) GOTO 902

C           Note that subroutine STRNUM will return NUMR = 0 if the
C           string passed to it contains all blanks (as *should* be the
C           case whenever I = 2 '(' ')', 3 '{' '}', 4 '[' ']', or
C           5 '<' '>').

C           However, when I = 1 '"' '"', then subroutine STRNUM will
C           return NUMR = (the number of replications for the mnemonic
C           using F=1 "regular" (i.e. non-delayed) replication).

            CALL STRNUM(ATAG(J+1:MAXTAG),NUMR)
            IF(I.EQ.1 .AND. NUMR.LE.0  ) GOTO 903
            IF(I.EQ.1 .AND. NUMR.GT.255) GOTO 904
            IF(I.NE.1 .AND. NUMR.NE.0  ) GOTO 905
            ATAG = ATAG(2:J-1)
            IREP = I
            GOTO 1
         ENDIF
         ENDDO
         GOTO 902
      ENDIF
      ENDDO

C  CHECK FOR VALID TAG
C  -------------------

1     IRET=NEMOCK(ATAG)
      IF(IRET.EQ.-1) GOTO 906
      IF(IRET.EQ.-2) GOTO 907
      CALL NEMTAB(LUN,ATAG,IDN,TAB,IRET)
      IF(IRET.GT.0) THEN

C        Note that the next code line checks that we are not trying to
C        replicate a Table B mnemonic (which is currently not allowed).
C        The logic works because, for replicated mnemonics, IREP = I =
C        (the index within REPS(*,*) of the symbol associated with the
C        type of replication in question (e.g. "{, "<", etc.))

         IF(TAB.EQ.'B' .AND. IREP.NE.0) GOTO 908
         IF(ATAG(1:1).EQ.'.') THEN

C           This mnemonic is a "following value" mnemonic
C           (i.e. it relates to the mnemonic that immediately
C           follows it within the user-supplied character-format BUFR
C           dictionary table sequence), so confirm that it contains, as
C           a substring, this mnemonic that immediately follows it.

            NEMB = TAGS(N+1)
c  .... get NEMA from IDN
            CALL NUMTAB(LUN,IDN,NEMA,TAB,ITAB)
            CALL NEMTAB(LUN,NEMB,JDN,TAB,IRET)
            CALL RSVFVM(NEMA,NEMB)
            IF(NEMA.NE.ATAG) GOTO 909
c  .... DK: I don't think the next test can ever be satisfied
c  ....     should probably be IF(N.EQ.NTAG ) GOTO 910
            IF(N.GT.NTAG ) GOTO 910
            IF(TAB.NE.'B') GOTO 911
         ENDIF
      ELSE
         GOTO 912
      ENDIF

C  WRITE THE DESCRIPTOR STRING INTO TABD ARRAY
C  -------------------------------------------
c  .... first look for a replication descriptor
      IF(IREP.GT.0) CALL PKTDD(ISEQ,LUN,IDNR(IREP,1)+NUMR,IRET)
      IF(IRET.LT.0) GOTO 913
      CALL PKTDD(ISEQ,LUN,IDN,IRET)
      IF(IRET.LT.0) GOTO 914

      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IS NOT A TABLE D ENTRY '//
     . '(UNDEFINED, TAB=",A,")")') NEMO,TAB
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TABLE D SEQUENCE (PARENT) MNEMONIC ",A,'//
     . '" DOES NOT CONTAIN ANY CHILD MNEMONICS")') NEMO
      CALL BORT2(BORT_STR1,BORT_STR2)
902   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TABLE D SEQUENCE (PARENT) MNEMONIC ",A,'//
     . '" CONTAINS A BADLY FORMED CHILD MNEMONIC",A)') NEMO,TAGS(N)
      CALL BORT2(BORT_STR1,BORT_STR2)
903   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(9X,"TBL D MNEM. ",A," CONTAINS REG. REPL. '//
     . 'CHILD MNEM. ",A," W/ INVALID # OF REPLICATIONS (",I3,") AFTER'//
     . ' 2ND QUOTE")') NEMO,TAGS(N),NUMR
      CALL BORT2(BORT_STR1,BORT_STR2)
904   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TBL D MNEM. ",A," CONTAINS REG. REPL. '//
     . 'CHILD MNEM. ",A," W/ # OF REPLICATIONS (",I3,") > LIMIT OF '//
     . '255")') NEMO,TAGS(N),NUMR
      CALL BORT2(BORT_STR1,BORT_STR2)
905   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TBL D MNEM. ",A," CONTAINS DELAYED REPL.'//
     . ' CHILD MNEM. ",A," W/ # OF REPL. (",I3,") SPECIFIED - A NO-'//
     . 'NO")') NEMO,TAGS(N),NUMR
      CALL BORT2(BORT_STR1,BORT_STR2)
906   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TABLE D (PARENT) MNEMONIC ",A," CONTAINS'//
     .' A CHILD MNEMONIC ",A," NOT BETWEEN 1 & 8 CHARACTERS")')
     . NEMO,TAGS(N)
      CALL BORT2(BORT_STR1,BORT_STR2)
907   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TABLE D (PARENT) MNEMONIC ",A," CONTAINS'//
     . ' A CHILD MNEMONIC ",A," WITH INVALID CHARACTERS")') NEMO,TAGS(N)
      CALL BORT2(BORT_STR1,BORT_STR2)
908   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TABLE D (PARENT) MNEMONIC ",A," CONTAINS'//
     . ' A REPLICATED CHILD TABLE B MNEMONIC ",A," - A NO-NO")')
     . NEMO,TAGS(N)
      CALL BORT2(BORT_STR1,BORT_STR2)
909   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TBL D (PARENT) MNEM. ",A," CONTAINS AN '//
     . 'INVALID ''FOLLOWING VALUE'' MNEMONIC ",A,"(SHOULD BE ",A,")")')
     . NEMO,TAGS(N),NEMA
      CALL BORT2(BORT_STR1,BORT_STR2)
910   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TBL D (PARENT) MNEM. ",A," CONTAINS A '//
     . '''FOLLOWING VALUE'' MNEMONIC ",A," WHICH IS LAST IN THE '//
     . 'STRING")') NEMO,NEMA
      CALL BORT2(BORT_STR1,BORT_STR2)
911   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TBL D (PARENT) MNEM. ",A,", THE MNEM. ",'//
     . 'A," FOLLOWING A ''FOLLOWING VALUE'' MNEM. IS NOT A TBL B '//
     . 'ENTRY")') NEMO,NEMB
      CALL BORT2(BORT_STR1,BORT_STR2)
912   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"TABLE D SEQUENCE (PARENT) MNEMONIC ",A,'//
     . '" CONTAINS A CHILD MNEMONIC ",A," NOT FOUND IN ANY TABLE")')
     .  NEMO,TAGS(N)
      CALL BORT2(BORT_STR1,BORT_STR2)
913   CLEMON = ADN30(IDNR(IREP,1)+NUMR,6)
      WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(9X,"TBL D (PARENT) MNEM. ",A," - BAD RETURN '//
     . 'FROM PKTDD TRYING TO STORE REPL. DESC. ",A,", SEE PREV. '//
     . 'WARNING MSG")') NEMO,CLEMON
      CALL BORT2(BORT_STR1,BORT_STR2)
914   WRITE(BORT_STR1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(9X,"TBL D (PARENT) MNEM. ",A," - BAD RETURN '//
     . 'FROM PKTDD TRYING TO STORE CHILD MNEM. ",A,", SEE PREV. '//
     . 'WARNING MSG")') NEMO,TAGS(N)
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
