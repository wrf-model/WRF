      SUBROUTINE MAKESTAB

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MAKESTAB
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE CONSTRUCTS AN INTERNAL JUMP/LINK TABLE
C  WITHIN COMMON BLOCK /TABLES/, USING THE INFORMATION WITHIN THE
C  INTERNAL BUFR TABLE ARRAYS (WITHIN COMMON BLOCK /TABABD/) FOR ALL OF
C  THE LUN (I.E., I/O STREAM INDEX) VALUES THAT ARE CURRENTLY DEFINED TO
C  THE BUFR ARCHIVE LIBRARY SOFTWARE.  NOTE THAT THE ENTIRE JUMP/LINK
C  TABLE WILL ALWAYS BE COMPLETELY RECONSTRUCTED FROM SCRATCH, EVEN IF
C  SOME OF THE INFORMATION WITHIN THE INTERNAL BUFR TABLE ARRAYS
C  ALREADY EXISTED THERE AT THE TIME OF THE PREVIOUS CALL TO THIS
C  SUBROUTINE, BECAUSE THERE MAY HAVE BEEN OTHER EVENTS THAT HAVE TAKEN
C  PLACE SINCE THE PREVIOUS CALL TO THIS SUBROUTINE THAT HAVE NOT YET
C  BEEN REFLECTED WITHIN THE INTERNAL JUMP/LINK TABLE, SUCH AS, E.G.
C  THE UNLINKING OF AN LUN VALUE FROM THE BUFR ARCHIVE LIBRARY SOFTWARE
C  VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE CLOSBF.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED HISTORY DOCUMENTATION; OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY; NOW ALLOWS FOR THE
C                           POSSIBILITY THAT A CONNECTED FILE MAY NOT
C                           CONTAIN ANY DICTIONARY TABLE INFO (E.G.,
C                           AN EMPTY FILE), SUBSEQUENT CONNECTED FILES
C                           WHICH ARE NOT EMPTY WILL NO LONGER GET
C                           TRIPPED UP BY THIS (THIS AVOIDS THE NEED
C                           FOR AN APPLICATION PROGRAM TO DISCONNECT
C                           ANY EMPTY FILES VIA A CALL TO CLOSBF)
C 2009-03-18  J. WOOLLEN -- ADDED LOGIC TO RESPOND TO THE CASES WHERE  
C                           AN INPUT FILE'S TABLES CHANGE IN MIDSTREAM.
C                           THE NEW LOGIC MOSTLY ADDRESSES CASES WHERE
C                           OTHER FILES ARE CONNECTED TO THE TABLES OF
C                           THE FILE WHOSE TABLES HAVE CHANGED.
C 2009-06-25  J. ATOR    -- TWEAK WOOLLEN LOGIC TO HANDLE SPECIAL CASE
C                           WHERE TABLE WAS RE-READ FOR A PARTICULAR
C                           LOGICAL UNIT BUT IS STILL THE SAME ACTUAL
C                           TABLE AS BEFORE AND IS STILL SHARING THAT
C                           TABLE WITH A DIFFERENT LOGICAL UNIT
C 2009-11-17  J. ATOR    -- ADDED CHECK TO PREVENT WRITING OUT OF TABLE
C                           INFORMATION WHEN A TABLE HAS BEEN RE-READ
C                           WITHIN A SHARED LOGICAL UNIT BUT HASN'T
C                           REALLY CHANGED
C
C USAGE:    CALL MAKESTAB
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     CHEKSTAB CLOSMG   CPBFDX
C                               ERRWRT   ICMPDX   ISHRDX   STRCLN
C                               TABSUB   WRDXTB
C    THIS ROUTINE IS CALLED BY: RDBFDX   RDMEMM   RDUSDX   READDX
C                               READERME READS3
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /QUIET/  IPRT
      COMMON /STBFR/  IOLUN(NFILES),IOMSG(NFILES)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXSS,NFILES),VAL(MAXSS,NFILES)
      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .                MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .                IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .                TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .                TABD(MAXTBD,NFILES)
      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /NRV203/ NNRV,INODNRV(MXNRV),NRV(MXNRV),TAGNRV(MXNRV),
     .                ISNRV(MXNRV),IENRV(MXNRV),IBTNRV,IPFNRV
      COMMON /LUSHR/  LUS(NFILES)

      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*10  TAG
      CHARACTER*8   NEMO,TAGNRV
      CHARACTER*3   TYP
      LOGICAL       EXPAND,XTAB(NFILES)
      REAL*8        VAL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  RESET POINTER TABLE AND STRING CACHE
C  ------------------------------------

      NTAB = 0
      NNRV = 0
      CALL STRCLN

C  FIGURE OUT WHICH UNITS SHARE TABLES
C  -----------------------------------

C     The LUS array is static between calls to this subroutine, and it
C     keeps track of which logical units share dictionary table
C     information:
C        if LUS(I) = 0, then IOLUN(I) does not share dictionary table
C                       information with any other logical unit
C        if LUS(I) > 0, then IOLUN(I) shares dictionary table
C                       information with logical unit IOLUN(LUS(I))
C        if LUS(I) < 0, then IOLUN(I) does not now, but at one point in
C                       the past, shared dictionary table information
C                       with logical unit IOLUN(ABS(LUS(I)))

C     The XTAB array is non-static and is recomputed within the below
C     loop during each call to this subroutine:
C        if XTAB(I) = .TRUE., then the dictionary table information
C                             has changed for IOLUN(I) since the last
C                             call to this subroutine
C        if XTAB(I) = .FALSE., then the dictionary table information
C                              has not changed for IOLUN(I) since the
C                              last call to this subroutine

      DO LUN=1,NFILES
        XTAB(LUN) = .FALSE.
        IF(IOLUN(LUN).EQ.0) THEN

C          Logical unit IOLUN(LUN) is not defined to the BUFRLIB.

           LUS(LUN) = 0
        ELSE IF(MTAB(1,LUN).EQ.0) THEN

C          New dictionary table information has been read for logical
C          unit IOLUN(LUN) since the last call to this subroutine.

           XTAB(LUN) = .TRUE.
           IF(LUS(LUN).NE.0) THEN
             IF(IOLUN(ABS(LUS(LUN))).EQ.0) THEN
               LUS(LUN) = 0
             ELSE IF(LUS(LUN).GT.0) THEN

C              IOLUN(LUN) was sharing table information with logical
C              unit IOLUN(LUS(LUN)), so check whether the table
C              information has really changed.  If not, then IOLUN(LUN)
C              just re-read a copy of the exact same table information
C              as before, and therefore it can continue to share with
C              logical unit IOLUN(LUS(LUN)).

               IF(ICMPDX(LUS(LUN),LUN).EQ.1) THEN
                 XTAB(LUN) = .FALSE. 
                 CALL CPBFDX(LUS(LUN),LUN)
               ELSE
                 LUS(LUN) = (-1)*LUS(LUN)
               ENDIF
             ELSE IF(ICMPDX(ABS(LUS(LUN)),LUN).EQ.1) THEN

C              IOLUN(LUN) was not sharing table information with logical
C              unit IOLUN(LUS(LUN)), but it did at one point in the past
C              and now once again has the same table information as that
C              logical unit.  Since the two units shared table
C              information at one point in the past, allow them to do
C              so again.

               XTAB(LUN) = .FALSE. 
               LUS(LUN) = ABS(LUS(LUN))
               CALL CPBFDX(LUS(LUN),LUN)
             ENDIF
           ENDIF
        ELSE IF(LUS(LUN).GT.0) THEN

C          Logical unit IOLUN(LUN) is sharing table information with
C          logical unit IOLUN(LUS(LUN)), so make sure that the latter
C          unit is still defined to the BUFRLIB.

           IF(IOLUN(LUS(LUN)).EQ.0) THEN
             LUS(LUN) = 0
           ELSE IF( XTAB(LUS(LUN)) .AND.
     +             (ICMPDX(LUS(LUN),LUN).EQ.0) ) THEN 

C            The table information for logical unit IOLUN(LUS(LUN))
C            just changed (in midstream).  If IOLUN(LUN) is an output
C            file, then we will have to update it with the new table
C            information later on in this subroutine.  Otherwise,
C            IOLUN(LUN) is an input file and is no longer sharing
C            tables with IOLUN(LUS(LUN)).

             IF(IOLUN(LUN).LT.0) LUS(LUN) = (-1)*LUS(LUN)
           ENDIF
        ELSE

C          Determine whether logical unit IOLUN(LUN) is sharing table
C          information with any other logical units.

           LUM = 1
           DO WHILE ((LUM.LT.LUN).AND.(LUS(LUN).EQ.0))
              IF(ISHRDX(LUM,LUN).EQ.1) THEN
                 LUS(LUN) = LUM
              ELSE
                 LUM = LUM+1
              ENDIF
           ENDDO
        ENDIF
      ENDDO

C  INITIALIZE JUMP/LINK TABLES WITH SUBSETS/SEQUENCES/ELEMENTS
C  -----------------------------------------------------------

      DO LUN=1,NFILES

       IF(IOLUN(LUN).NE.0 .AND. NTBA(LUN).GT.0) THEN

C        Reset any existing inventory pointers.

         IF(IOMSG(LUN).NE.0) THEN
            IF(LUS(LUN).EQ.0) THEN
              INC = (NTAB+1)-MTAB(1,LUN)
            ELSE
              INC = MTAB(1,LUS(LUN))-MTAB(1,LUN)
            ENDIF
            DO N=1,NVAL(LUN)
              INV(N,LUN) = INV(N,LUN)+INC
            ENDDO
         ENDIF

         IF(LUS(LUN).LE.0) THEN   

C           The dictionary table information corresponding to logical
C           unit IOLUN(LUN) has not yet been written into the internal
C           jump/link table, so add it in now.

            CALL CHEKSTAB(LUN)  
            DO ITBA=1,NTBA(LUN) 
              INOD = NTAB+1
              NEMO = TABA(ITBA,LUN)(4:11)
              CALL TABSUB(LUN,NEMO)
              MTAB(ITBA,LUN) = INOD
              ISC(INOD)      = NTAB
            ENDDO
         ELSE IF( XTAB(LUS(LUN)) .AND.
     +           (ICMPDX(LUS(LUN),LUN).EQ.0) ) THEN 

C           Logical unit IOLUN(LUN) is an output file that is sharing
C           table information with logical unit IOLUN(LUS(LUN)) whose
C           table just changed (in midstream).  Flush any existing data
C           messages from IOLUN(LUN), then update the table information
C           for this logical unit with the corresponding new table
C           information from IOLUN(LUS(LUN)), then update IOLUN(LUN)
C           itself with a copy of the new table information.

            LUNIT = ABS(IOLUN(LUN))
            IF(IOMSG(LUN).NE.0) CALL CLOSMG(LUNIT)    
            CALL CPBFDX(LUS(LUN),LUN)
            LUNDX = ABS(IOLUN(LUS(LUN)))
            CALL WRDXTB(LUNDX,LUNIT) 
         ENDIF

       ENDIF

      ENDDO

C  STORE TYPES AND INITIAL VALUES AND COUNTS
C  -----------------------------------------

      DO NODE=1,NTAB
      IF(TYP(NODE).EQ.'SUB') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'SEQ') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'RPC') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 0
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'RPS') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 0
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'REP') THEN
         VALI(NODE) = BMISS
         KNTI(NODE) = IRF(NODE)
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'DRS') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 1
      ELSEIF(TYP(NODE).EQ.'DRP') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 1
      ELSEIF(TYP(NODE).EQ.'DRB') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 0
         ITP (NODE) = 1
      ELSEIF(TYP(NODE).EQ.'NUM') THEN
         VALI(NODE) = BMISS
         KNTI(NODE) = 1
         ITP (NODE) = 2
      ELSEIF(TYP(NODE).EQ.'CHR') THEN
         VALI(NODE) = BMISS
         KNTI(NODE) = 1
         ITP (NODE) = 3
      ELSE
         GOTO 901
      ENDIF
      ENDDO

C  SET UP EXPANSION SEGMENTS FOR TYPE 'SUB', 'DRP', AND 'DRS' NODES
C  ----------------------------------------------------------------

      NEWN = 0

      DO N=1,NTAB
      ISEQ(N,1) = 0
      ISEQ(N,2) = 0
      EXPAND = TYP(N).EQ.'SUB' .OR. TYP(N).EQ.'DRP' .OR. TYP(N).EQ.'DRS'
     .                         .OR. TYP(N).EQ.'REP' .OR. TYP(N).EQ.'DRB'
      IF(EXPAND) THEN
         ISEQ(N,1) = NEWN+1
         NODA = N
         NODE = N+1
         DO K=1,MAXJL
         KNT(K) = 0
         ENDDO
         IF(TYP(NODA).EQ.'REP') KNT(NODE) = KNTI(NODA)
         IF(TYP(NODA).NE.'REP') KNT(NODE) = 1

1        NEWN = NEWN+1
         IF(NEWN.GT.MAXJL) GOTO 902
         JSEQ(NEWN) = NODE
         KNT(NODE) = MAX(KNTI(NODE),KNT(NODE))
2        IF(JUMP(NODE)*KNT(NODE).GT.0) THEN
            NODE = JUMP(NODE)
            GOTO 1
         ELSE IF(LINK(NODE).GT.0) THEN
            NODE = LINK(NODE)
            GOTO 1
         ELSE
            NODE = JMPB(NODE)
            IF(NODE.EQ.NODA) GOTO 3
            IF(NODE.EQ.0   ) GOTO 903
            KNT(NODE) = MAX(KNT(NODE)-1,0)
            GOTO 2
         ENDIF
3        ISEQ(N,2) = NEWN
      ENDIF
      ENDDO

C  PRINT THE SEQUENCE TABLES
C  ------------------------

      IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
         DO I=1,NTAB
           WRITE ( UNIT=ERRSTR, FMT='(A,I5,2X,A10,A5,6I8)' )
     .      'BUFRLIB: MAKESTAB ', I, TAG(I), TYP(I), JMPB(I), JUMP(I),
     .      LINK(I), IBT(I), IRF(I), ISC(I)
           CALL ERRWRT(ERRSTR)
         ENDDO
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - MNEMONIC ",A," IS '//
     . 'DUPLICATED IN SUBSET: ",A)') NEMO,TAG(N1)
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - UNKNOWN TYPE ",A)')TYP(NODE)
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - NUMBER OF JSEQ ENTRIES IN'//
     . ' JUMP/LINK TABLE EXCEEDS THE LIMIT (",I6,")")') MAXJL
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - NODE IS ZERO, FAILED TO '//
     . 'CIRCULATE (TAG IS ",A,")")') TAG(N)
      CALL BORT(BORT_STR)
      END
