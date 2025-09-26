	SUBROUTINE READMT ( IMT, IMTV, IOGCE, IMTVL )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    READMT
C   PRGMMR: ATOR            ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT:  THIS SUBROUTINE OPENS AND READS BUFR MASTER TABLES AS
C   SPECIFIED BY THE INPUT ARGUMENTS AND USING ADDITIONAL INFORMATION
C   AS WAS DEFINED IN THE MOST RECENT CALL TO BUFR ARCHIVE LIBRARY
C   SUBROUTINE MTINFO (OR AS WAS DEFINED WITHIN BUFR ARCHIVE LIBRARY
C   SUBROUTINE BFRINI, IF SUBROUTINE MTINFO WAS NEVER CALLED).
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL READMT ( IMT, IMTV, IOGCE, IMTVL )
C   INPUT ARGUMENT LIST:
C     IMT      - INTEGER: MASTER TABLE NUMBER
C     IMTV     - INTEGER: MASTER TABLE VERSION NUMBER
C     IOGCE    - INTEGER: ORIGINATING CENTER
C     IMTVL    - INTEGER: LOCAL TABLE VERSION NUMBER
C
C   INPUT FILES:
C     UNITS 98,99  - IF SUBROUTINE MTINFO WAS NEVER CALLED, THEN THESE
C                    LOGICAL UNIT NUMBERS ARE USED BY THIS ROUTINE FOR
C                    OPENING AND READING THE BUFR MASTER TABLES.
C                    ALTERNATIVELY, IF SUBROUTINE MTINFO WAS CALLED,
C                    THEN THE LOGICAL UNIT NUMBERS SPECIFIED IN THE
C                    MOST RECENT CALL TO MTINFO (ARGUMENTS LUNMT1 AND
C                    LUNMT2) ARE USED INSTEAD.
C REMARKS:
C    THIS ROUTINE CALLS:        BORT2    ERRWRT   ICVIDX   IGETTDI
C                               RDMTBB   RDMTBD
C    THIS ROUTINE IS CALLED BY: READS3
C                               Not normally called by any application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	INCLUDE 'bufrlib.prm'

	COMMON /QUIET/  IPRT
	COMMON /MSTINF/ LUN1, LUN2, LMTD, MTDIR
	COMMON /MSTABS/ NMTB, IBFXYN(MXMTBB), CBSCL(MXMTBB),
     .                        CBSREF(MXMTBB), CBBW(MXMTBB),
     .                        CBUNIT(MXMTBB), CBMNEM(MXMTBB),
     .                        CBELEM(MXMTBB),
     .                  NMTD, IDFXYN(MXMTBD), CDSEQ(MXMTBD),
     .			      CDMNEM(MXMTBD), NDELEM(MXMTBD),
     .			      IDEFXY(MXMTBD*MAXCD),
     .			      CDELEM(MXMTBD*MAXCD)

	DIMENSION	IMFXYB(MXMTBB), IMFXYD(MXMTBD),
     .			NMELEM(MXMTBD), IEFXYN(MXMTBD,MAXCD)
	CHARACTER*4	CMDSCB(MXMTBB), CMDSCD(MXMTBD),
     .			CBSCL, CMSCL(MXMTBB),
     .			CBBW, CMBW(MXMTBB)
	CHARACTER*8	CBMNEM, CMMNMB(MXMTBB),
     .			CDMNEM, CMMNMD(MXMTBD)
	CHARACTER*12	CBSREF, CMSREF(MXMTBB)
	CHARACTER*14	CBUNIT, CMUNIT(MXMTBB)
	CHARACTER*20	FMTF
	CHARACTER*100	MTDIR
	CHARACTER*120	CBELEM, CMELEM(MXMTBB),
     .			CDSEQ, CMSEQ(MXMTBD),
     .			CDELEM, CEELEM(MXMTBD,MAXCD)
	CHARACTER*128	BORT_STR
	CHARACTER*132	TBLFIL,STDFIL,LOCFIL1,LOCFIL2
	LOGICAL		FOUND

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C*	Reset the scratch table D index for this master table.

	ITMP = IGETTDI ( 0 )

	IF ( IPRT .GE. 2 ) THEN
        CALL ERRWRT(' ')
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	CALL ERRWRT('BUFRLIB: READMT - OPENING/READING MASTER TABLES')
	ENDIF

C*	Locate and open the master Table B files.  There should be one
C*	file of standard descriptors and one file of local descriptors.

C*	First locate and open the file of standard Table B descriptors.

	IF ( ( IMT .EQ. 0 ) .AND. ( IMTV .LE. 13 ) ) THEN

C*	  For master table 0, version 13 is a superset of all earlier
C*	  versions.

	  STDFIL = MTDIR(1:LMTD) // '/' // 'bufrtab.TableB_STD_0_13'
	ELSE
	  WRITE ( FMTF, '(A,I1,A,I1,A)' )
     .	     '(2A,I', ISIZE(IMT), ',A,I', ISIZE(IMTV), ')'
	  WRITE ( STDFIL, FMTF ) MTDIR(1:LMTD), '/bufrtab.TableB_STD_',
     .	     IMT, '_', IMTV
	ENDIF
	TBLFIL = STDFIL
	IF ( IPRT .GE. 2 ) THEN
	  CALL ERRWRT('Standard Table B:')
	  CALL ERRWRT(TBLFIL)
	ENDIF
	INQUIRE ( FILE = TBLFIL, EXIST = FOUND )
	IF ( .NOT. FOUND ) GOTO 900
	OPEN ( UNIT = LUN1, FILE = TBLFIL, IOSTAT = IER )
	IF ( IER .NE. 0 ) GOTO 901

C*	Now locate and open the file of local Table B descriptors.

C*	Use the local table corresponding to the originating center
C*	and local table version number of the current message, if such
C*	a table exists.  Otherwise use the NCEP local table B.

	LOCFIL2 = MTDIR(1:LMTD) // '/' // 'bufrtab.TableB_LOC_0_7_1'
	WRITE ( FMTF, '(A,I1,A,I1,A,I1,A)' )
     .	   '(2A,I', ISIZE(IMT), ',A,I', ISIZE(IOGCE),
     .	   ',A,I',  ISIZE(IMTVL), ')'
	WRITE ( LOCFIL1, FMTF ) MTDIR(1:LMTD), '/bufrtab.TableB_LOC_',
     .	   IMT, '_', IOGCE, '_', IMTVL
	TBLFIL = LOCFIL1
	IF ( IPRT .GE. 2 ) THEN
	  CALL ERRWRT('Local Table B:')
	  CALL ERRWRT(TBLFIL)
	ENDIF
	INQUIRE ( FILE = TBLFIL, EXIST = FOUND )
	IF ( .NOT. FOUND ) THEN

C*	  Use the NCEP local table B.

	  TBLFIL = LOCFIL2
	  IF ( IPRT .GE. 2 ) THEN
	    CALL ERRWRT('Local Table B not found, so using:')
	    CALL ERRWRT(TBLFIL)
	  ENDIF
	  INQUIRE ( FILE = TBLFIL, EXIST = FOUND )
	  IF ( .NOT. FOUND ) GOTO 900
	ENDIF
	OPEN ( UNIT = LUN2, FILE = TBLFIL, IOSTAT = IER )
	IF ( IER .NE. 0 ) GOTO 901

C*	Read the master Table B files.

	CALL RDMTBB ( LUN1, LUN2, MXMTBB,
     .		      IBMT, IBMTV, IBOGCE, IBLTV,
     .		      NMTBB, IMFXYB, CMSCL, CMSREF, CMBW,
     .		      CMUNIT, CMMNMB, CMDSCB, CMELEM )

C*	Save the output into COMMON /MSTABS/.

	NMTB = NMTBB
	DO I = 1, NMTB
	  IBFXYN(I) = IMFXYB(I)
	  CBSCL(I) = CMSCL(I)
	  CBSREF(I) = CMSREF(I)
	  CBBW(I) = CMBW(I)
	  CBUNIT(I) = CMUNIT(I)
	  CBMNEM(I) = CMMNMB(I)
	  CBELEM(I) = CMELEM(I)
	ENDDO

C*	Close the master Table B files.

	CLOSE ( UNIT = LUN1 )
	CLOSE ( UNIT = LUN2 )

C*	Locate and open the master Table D files.  There should be one
C*	file of standard descriptors and one file of local descriptors.

C*	First locate and open the file of standard Table D descriptors.

	TBLFIL = STDFIL
	TBLFIL(LMTD+15:LMTD+15) = 'D'
	IF ( IPRT .GE. 2 ) THEN
	  CALL ERRWRT('Standard Table D:')
	  CALL ERRWRT(TBLFIL)
	ENDIF
	INQUIRE ( FILE = TBLFIL, EXIST = FOUND )
	IF ( .NOT. FOUND ) GOTO 900
	OPEN ( UNIT = LUN1, FILE = TBLFIL, IOSTAT = IER )
	IF ( IER .NE. 0 ) GOTO 901

C*	Now locate and open the file of local Table D descriptors.

C*	Use the local table corresponding to the originating center
C*	and local table version number of the current message, if such
C*	a table exists.  Otherwise use the NCEP local table D.

	TBLFIL = LOCFIL1
	TBLFIL(LMTD+15:LMTD+15) = 'D'
	IF ( IPRT .GE. 2 ) THEN
	  CALL ERRWRT('Local Table D:')
	  CALL ERRWRT(TBLFIL)
	ENDIF
	INQUIRE ( FILE = TBLFIL, EXIST = FOUND )
	IF ( .NOT. FOUND ) THEN

C*	  Use the NCEP local table D.

	  TBLFIL = LOCFIL2
	  TBLFIL(LMTD+15:LMTD+15) = 'D'
	  IF ( IPRT .GE. 2 ) THEN
	    CALL ERRWRT('Local Table D not found, so using:')
	    CALL ERRWRT(TBLFIL)
	  ENDIF
	  INQUIRE ( FILE = TBLFIL, EXIST = FOUND )
	  IF ( .NOT. FOUND ) GOTO 900
	ENDIF
	OPEN ( UNIT = LUN2, FILE = TBLFIL, IOSTAT = IER )
	IF ( IER .NE. 0 ) GOTO 901

C*	Read the master Table D files.

	CALL RDMTBD ( LUN1, LUN2, MXMTBD, MAXCD,
     .		      IDMT, IDMTV, IDOGCE, IDLTV,
     .		      NMTBD, IMFXYD, CMMNMD, CMDSCD, CMSEQ,
     .		      NMELEM, IEFXYN, CEELEM )

C*	Save the output into COMMON /MSTABS/.

	NMTD = NMTBD
	DO I = 1, NMTD
	  IDFXYN(I) = IMFXYD(I)
	  CDMNEM(I) = CMMNMD(I)
	  CDSEQ(I) = CMSEQ(I)
	  NDELEM(I) = NMELEM(I)
	  DO J = 1, NDELEM(I)
	    IDX = ICVIDX ( I-1, J-1, MAXCD ) + 1
	    IDEFXY(IDX) = IEFXYN(I,J)
	    CDELEM(IDX) = CEELEM(I,J)
	  ENDDO
	ENDDO

C*	Close the master Table D files.

	CLOSE ( UNIT = LUN1 )
	CLOSE ( UNIT = LUN2 )

	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF

	RETURN
900	BORT_STR = 'BUFRLIB: READMT - COULD NOT FIND FILE:'
	CALL BORT2(BORT_STR,TBLFIL)
901	BORT_STR = 'BUFRLIB: READMT - COULD NOT OPEN FILE:'
	CALL BORT2(BORT_STR,TBLFIL)
	END
