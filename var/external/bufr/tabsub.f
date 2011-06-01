      SUBROUTINE TABSUB(LUN,NEMO)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TABSUB
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE BUILDS THE ENTIRE JUMP/LINK TREE (I.E.,
C   INCLUDING RECURSIVELY RESOLVING ALL "CHILD" MNEMONICS) FOR A TABLE
C   A MNEMONIC (NEMO) WITHIN THE INTERNAL JUMP/LINK TABLE.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2000-09-19  J. WOOLLEN -- ADDED CAPABILITY TO ENCODE AND DECODE DATA
C                           USING THE OPERATOR DESCRIPTORS (BUFR TABLE
C                           C) FOR CHANGING WIDTH AND CHANGING SCALE
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED HISTORY DOCUMENTATION; OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C 2005-11-29  J. ATOR    -- ADDED SUPPORT FOR 207 AND 208 OPERATORS
C
C USAGE:    CALL TABSUB (LUN, NEMO)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     NEMO     - CHARACTER*8: TABLE A MNEMONIC
C
C REMARKS:
C    EXAMPLE SHOWING CONTENTS OF INTERNAL JUMP/LINK TABLE (WITHIN
C    COMMON /TABLES/):
C
C      INTEGER MAXTAB = maximum number of jump/link table entries
C
C      INTEGER NTAB = actual number of jump/link table entries
C                     currently in use
C
C      For I = 1, NTAB:
C
C      CHARACTER*10 TAG(I) = mnemonic
C
C      CHARACTER*3 TYP(I) = mnemonic type indicator:
C         "SUB" if TAG(I) is a Table A mnemonic
C         "SEQ" if TAG(I) is a Table D mnemonic using either short
C               (i.e. 1-bit) delayed replication, F=1 regular (i.e.
C               non-delayed) replication, or no replication at all
C         "RPC" if TAG(I) is a Table D mnemonic using either medium
C               (i.e. 8-bit) delayed replication or long (i.e. 16-bit)
C               delayed replication
C         "DRB" if TAG(I) denotes the short (i.e. 1-bit) delayed
C               replication of a Table D mnemonic (which would then
C               itself have its own separate entry in the jump/link
C               table with a corresponding TAG value of "SEQ")
C         "DRP" if TAG(I) denotes either the medium (i.e. 8-bit) or
C               long (i.e. 16-bit) delayed replication of a Table D
C               mnemonic (which would then itself have its own separate
C               entry in the jump/link table with a corresponding TAG
C               value of "RPC")
C         "REP" if TAG(I) denotes the F=1 regular (i.e. non-delayed)
C               replication of a Table D mnemonic (which would then
C               itself have its own separate entry in the jump/link
C               table with a corresponding TAG value of "SEQ")
C         "CHR" if TAG(I) is a Table B mnemonic with units "CCITT IA5"
C         "NUM" if TAG(I) is a Table B mnemonic with any units other
C               than "CCITT IA5"
C
C       INTEGER JMPB(I):
C
C       IF ( TYP(I) = "SUB" ) THEN
C          JMPB(I) = 0
C       ELSE IF ( ( TYP(I) = "SEQ" and TAG(I) uses either short (i.e.
C                   1-bit) delayed replication or F=1 regular (i.e.
C                   non-delayed) replication )
C                OR
C                 ( TYP(I) = "RPC" )  ) THEN
C          JMPB(I) = the index of the jump/link table entry denoting
C                    the replication of TAG(I)
C       ELSE
C          JMPB(I) = the index of the jump/link table entry for the
C                    Table A or Table D mnemonic of which TAG(I) is a
C                    child
C       END IF
C
C       INTEGER JUMP(I):
C
C       IF ( ( TYP(I) = "CHR" )  OR  ( TYP(I) = "NUM" ) ) THEN
C          JUMP(I) = 0
C       ELSE IF ( ( TYP(I) = "DRB" ) OR
C                 ( TYP(I) = "DRP" ) OR
C                 ( TYP(I) = "REP" ) ) THEN
C          JUMP(I) = the index of the jump/link table entry for the
C                    Table D mnemonic whose replication is denoted by
C                    TAG(I)
C       ELSE
C          JUMP(I) = the index of the jump/link table entry for the
C                    Table B or Table D mnemonic which, sequentially,
C                    is the first child of TAG(I)
C       END IF
C
C       INTEGER LINK(I):
C
C       IF ( ( TYP(I) = "SEQ" and TAG(I) uses either short (i.e.
C              1-bit) delayed replication or F=1 regular (i.e. non-
C              delayed) replication )
C           OR
C            ( TYP(I) = "SUB" )
C           OR
C            ( TYP(I) = "RPC" ) ) THEN
C              LINK(I) = 0
C       ELSE IF ( TAG(I) is, sequentially, the last child Table B or
C                 Table D mnemonic of the parent Table A or Table D
C                 mnemonic indexed by JMPB(I) ) THEN
C          LINK(I) = 0
C       ELSE
C          LINK(I) = the index of the jump/link table entry for the
C                    Table B or Table D mnemonic which, sequentially,
C                    is the next (i.e. following TAG(I)) child mnemonic
C                    of the parent Table A or Table D mnemonic indexed
C                    by JMPB(I)
C       END IF
C
C       INTEGER IBT(I):
C
C       IF ( ( TYP(I) = "CHR" )  OR  ( TYP(I) = "NUM" ) ) THEN
C          IBT(I) = bit width of Table B mnemonic TAG(I)
C       ELSE IF ( ( TYP(I) = "DRB" )  OR  ( TYP(I) = "DRP" ) ) THEN
C          IBT(I) = bit width of delayed descriptor replication factor
C                   (i.e. 1, 8, or 16, depending on the replication
C                   scheme denoted by TAG(I))
C       ELSE
C          IBT(I) = 0
C       END IF
C
C       INTEGER IRF(I):
C
C       IF ( TYP(I) = "NUM" ) THEN
C          IRF(I) = reference value of Table B mnemonic TAG(I)
C       ELSE IF ( TYP(I) = "REP" ) THEN
C          IRF(I) = number of F=1 regular (i.e. non-delayed)
C                   replications of Table D mnemonic TAG(JUMP(I))
C       ELSE
C          IRF(I) = 0
C       END IF
C
C       INTEGER ISC(I):
C
C       IF ( TYP(I) = "NUM" ) THEN
C          ISC(I) = scale factor of Table B mnemonic TAG(I)
C       ELSE IF ( TYP(I) = "SUB" ) THEN
C          ISC(I) = the index of the jump/link table entry which,
C                   sequentially, constitutes the last element of the
C                   jump/link tree for Table A mnemonic TAG(I)
C       ELSE
C          ISC(I) = 0
C       END IF
C
C
C
C    THIS ROUTINE CALLS:        BORT     INCTAB   NEMTAB   NEMTBD
C                               TABENT
C    THIS ROUTINE IS CALLED BY: MAKESTAB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /TABCCC/ ICDW,ICSC,ICRV,INCW

      CHARACTER*128 BORT_STR
      CHARACTER*10  TAG
      CHARACTER*8   NEMO,NEMS,NEM
      CHARACTER*3   TYP
      CHARACTER*1   TAB
      DIMENSION     NEM(MAXCD,10),IRP(MAXCD,10),KRP(MAXCD,10)
      DIMENSION     DROP(10),JMP0(10),NODL(10),NTAG(10,2)
      LOGICAL       DROP

      DATA MAXLIM /10/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE MNEMONIC
C  ------------------

C     Note that Table A mnemonics, in addition to being stored within
C     internal BUFR Table A array TABA(*,LUN), are also stored as
C     Table D mnemonics within internal BUFR Table D array TABD(*,LUN).
C     Thus, the following test is valid.

      CALL NEMTAB(LUN,NEMO,IDN,TAB,ITAB)
      IF(TAB.NE.'D') GOTO 900

C  STORE A SUBSET NODE AND JUMP/LINK THE TREE
C  ------------------------------------------

      CALL INCTAB(NEMO,'SUB',NODE)
      JUMP(NODE) = NODE+1
      JMPB(NODE) = 0
      LINK(NODE) = 0
      IBT (NODE) = 0
      IRF (NODE) = 0
      ISC (NODE) = 0

      CALL NEMTBD(LUN,ITAB,NSEQ,NEM(1,1),IRP(1,1),KRP(1,1))
      NTAG(1,1) = 1
      NTAG(1,2) = NSEQ
      JMP0(1)   = NODE
      LIMB      = 1

      ICDW = 0
      ICSC = 0
      ICRV = 1
      INCW = 0

C  THIS LOOP RESOLVES ENTITIES IN A SUBSET BY EMULATING RECURSION
C  --------------------------------------------------------------

1     DO N=NTAG(LIMB,1),NTAG(LIMB,2)

      NTAG(LIMB,1) = N+1
      NODL(LIMB)   = NTAB+1
      DROP(LIMB)   = N.EQ.NTAG(LIMB,2)

      CALL NEMTAB(LUN,NEM(N,LIMB),IDN,TAB,ITAB)
      NEMS = NEM(N,LIMB)

C  SPECIAL TREATMENT FOR CERTAIN OPERATOR DESCRIPTORS (TAB=C)
C  ----------------------------------------------------------

      IF(TAB.EQ.'C') THEN
         NODL(LIMB) = NTAB
         READ(NEMS,'(3X,I3)') IYYY
         IF(ITAB.EQ.1) THEN
            IF(IYYY.NE.0) THEN
              IF(ICDW.NE.0) GOTO 907
              ICDW = IYYY-128
            ELSE
              ICDW = 0
            ENDIF
         ELSEIF(ITAB.EQ.2) THEN
            IF(IYYY.NE.0) THEN
              IF(ICSC.NE.0) GOTO 908
              ICSC = IYYY-128
            ELSE
              ICSC = 0
            ENDIF
         ELSEIF(ITAB.EQ.7) THEN
            IF(IYYY.GT.0) THEN
              IF(ICDW.NE.0) GOTO 907
              IF(ICSC.NE.0) GOTO 908
              ICDW = ((10*IYYY)+2)/3
              ICSC = IYYY
              ICRV = 10**IYYY
            ELSE
              ICSC = 0
              ICDW = 0
              ICRV = 1
            ENDIF
         ELSEIF(ITAB.EQ.8) THEN
            INCW = IYYY
         ENDIF
      ELSE
         IREP = IRP(N,LIMB)
         IKNT = KRP(N,LIMB)
         JUM0 = JMP0(LIMB)
         CALL TABENT(LUN,NEMS,TAB,ITAB,IREP,IKNT,JUM0)
      ENDIF

      IF(TAB.EQ.'D') THEN

C        Note here how a new tree "LIMB" is created (and is then
C        immediately recursively resolved) whenever a Table D mnemonic
C        contains another Table D mnemonic as one of its children.

         LIMB = LIMB+1
         IF(LIMB.GT.MAXLIM) GOTO 901
         CALL NEMTBD(LUN,ITAB,NSEQ,NEM(1,LIMB),IRP(1,LIMB),KRP(1,LIMB))
         NTAG(LIMB,1) = 1
         NTAG(LIMB,2) = NSEQ
         JMP0(LIMB)   = NTAB
         GOTO 1
      ELSEIF(DROP(LIMB)) THEN
2        LINK(NODL(LIMB)) = 0
         LIMB = LIMB-1
         IF(LIMB.EQ.0 ) THEN
            IF(ICRV.NE.1) GOTO 904
            IF(ICDW.NE.0) GOTO 902
            IF(ICSC.NE.0) GOTO 903
            IF(INCW.NE.0) GOTO 905
            GOTO 100
         ENDIF
         IF(DROP(LIMB)) GOTO 2
         LINK(NODL(LIMB)) = NTAB+1
         GOTO 1
      ELSEIF(TAB.NE.'C') THEN
         LINK(NODL(LIMB)) = NTAB+1
      ENDIF

      ENDDO

      GOTO 906

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: TABSUB - SUBSET NODE NOT IN TABLE D '//
     . '(TAB=",A,") FOR INPUT MNEMONIC ",A)') TAB,NEMO
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: TABSUB - THERE ARE TOO MANY NESTED '//
     . 'TABLE D SEQUENCES (TREES) WITHIN INPUT MNEMONIC ",A," - THE '//
     . 'LIMIT IS",I4)') NEMO,MAXLIM
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-01-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-02-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-07-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-08-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLIB: TABSUB - ENTITIES WERE NOT '//
     . 'SUCCESSFULLY RESOLVED (BY EMULATING RESURSION) FOR SUBSET '//
     . 'DEFINED BY TBL A MNEM. ",A)') NEMO
      CALL BORT(BORT_STR)
907   WRITE(BORT_STR,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '//
     . 'CHANGE DATA WIDTH OPERATIONS IN THE TREE BUILT FROM INPUT ' //
     . 'MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
908   WRITE(BORT_STR,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '//
     . 'CHANGE DATA SCALE OPERATIONS IN THE TREE BUILT FROM INPUT ' //
     . 'MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
      END
