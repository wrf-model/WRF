      SUBROUTINE GETS1LOC(S1MNEM,IBEN,ISBYT,IWID,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:   GETS1LOC 
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT: THIS SUBROUTINE RETURNS THE LOCATION (I.E. STARTING BYTE
C   AND BIT WIDTH) OF A SPECIFIED VALUE WITHIN SECTION 1 OF A BUFR
C   MESSAGE ENCODED ACCORDING TO A SPECIFIED BUFR EDITION.  IT WILL
C   WORK ON ANY MESSAGE ENCODED USING BUFR EDITION 2, 3 OR 4.  THE
C   VALUE FOR WHICH THE LOCATION IS TO BE DETERMINED IS SPECIFIED VIA
C   THE MNEMONIC S1MNEM, AS EXPLAINED IN FURTHER DETAIL BELOW.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C 2006-04-14  D. KEYSER  -- ADDED OPTIONS FOR 'YCEN' AND 'CENT'
C
C USAGE:    GETS1LOC ( S1MNEM, IBEN, ISBYT, IWID, IRET )
C   INPUT ARGUMENT LIST:
C     S1MNEM   - CHARACTER*(*): MNEMONIC SPECIFYING VALUE WHOSE
C                LOCATION WITHIN SECTION 1 IS TO BE DETERMINED:
C                  'LEN1'  = LENGTH (IN BYTES) OF SECTION 1
C                  'BMT'   = BUFR MASTER TABLE 
C                  'OGCE'  = ORIGINATING CENTER
C                  'GSES'  = ORIGINATING SUBCENTER
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 3 OR 4 MESSAGES!)
C                  'USN'   = UPDATE SEQUENCE NUMBER 
C                  'ISC2'  = FLAG INDICATING ABSENCE/PRESENCE OF
C                            (OPTIONAL) SECTION 2 IN BUFR MESSAGE:
C                              0 = SECTION 2 ABSENT
C                              1 = SECTION 2 PRESENT
C                  'MTYP'  = DATA CATEGORY 
C                  'MSBTI' = DATA SUBCATEGORY (INTERNATIONAL)
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C                  'MSBT'  = DATA SUBCATEGORY (LOCAL)
C                  'MTV'   = VERSION NUMBER OF MASTER TABLE
C                  'MTVL'  = VERSION NUMBER OF LOCAL TABLES
C                  'YCEN'  = YEAR OF CENTURY (1-100)
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 2 AND 3 MESSAGES!)
C                  'CENT'  = CENTURY (I.E., 20 FOR YEARS 1901-2000,
C                                           21 FOR YEARS 2001-2100)
C                              (NOTE: THIS VALUE *MAY* BE PRESENT IN
C                                     BUFR EDITION 2 AND 3 MESSAGES,
C                                     BUT IT IS NEVER PRESENT IN ANY
C                                     BUFR EDITION 4 MESSAGES!)
C                  'YEAR'  = YEAR (4-DIGIT)
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C                  'MNTH'  = MONTH
C                  'DAYS'  = DAY
C                  'HOUR'  = HOUR
C                  'MINU'  = MINUTE
C                  'SECO'  = SECOND
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C     IBEN     - INTEGER: BUFR EDITION NUMBER
C
C
C   OUTPUT ARGUMENT LIST:
C     ISBYT    - INTEGER: NUMBER OF STARTING BYTE WITHIN SECTION 1
C                WHICH CONTAINS VALUE CORRESPONDING TO S1MNEM
C                   (NOTE: ISBYT IS ALWAYS RETURNED AS 18 WHENEVER
C                          S1MNEM = 'CENT' AND IBEN = 2 OR 3; IN SUCH
C                          CASES IT IS THEN UP TO THE CALLING ROUTINE
C                          TO DETERMINE WHETHER THIS LOCATION ACTUALLY
C                          CONTAINS A VALID CENTURY VALUE!)
C     IWID     - INTEGER: WIDTH (IN BITS) OF VALUE CORRESPONDING
C                TO S1MNEM
C     IRET     - INTEGER: RETURN CODE
C                   0 = NORMAL RETURN
C                  -1 = THE INPUT S1MNEM MNEMONIC IS INVALID FOR
C                       BUFR EDITION IBEN
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: CRBMG    IUPBS01  PKBS1
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	CHARACTER*(*)	S1MNEM

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IRET = 0
	IWID = 8

	IF(S1MNEM.EQ.'LEN1') THEN
	    ISBYT = 1 
	    IWID = 24 
	ELSE IF(S1MNEM.EQ.'BMT') THEN
	    ISBYT = 4 
	ELSE IF(S1MNEM.EQ.'OGCE') THEN
	    IF(IBEN.EQ.3) THEN
		ISBYT = 6 
	    ELSE

C               Note that this location is actually the same for both
C               Edition 2 *and* Edition 4 of BUFR!

		ISBYT = 5
		IWID = 16
	    ENDIF
	ELSE IF(S1MNEM.EQ.'GSES') THEN
	    IF(IBEN.EQ.3) THEN
		ISBYT = 5
	    ELSE IF(IBEN.EQ.4) THEN
		ISBYT = 7
		IWID = 16
	    ELSE
		IRET = -1
	    ENDIF
	ELSE IF(S1MNEM.EQ.'USN') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 9
	    ELSE
		ISBYT = 7
	    ENDIF
	ELSE IF(S1MNEM.EQ.'ISC2') THEN
	    IWID = 1
	    IF(IBEN.EQ.4) THEN
		ISBYT = 10
	    ELSE
		ISBYT = 8
	    ENDIF
	ELSE IF(S1MNEM.EQ.'MTYP') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 11
	    ELSE
		ISBYT = 9
	    ENDIF
	ELSE IF(S1MNEM.EQ.'MSBTI') THEN
	    IF(IBEN.EQ.4) THEN
	        ISBYT = 12 
	    ELSE
		IRET = -1
	    ENDIF
	ELSE IF(S1MNEM.EQ.'MSBT') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 13 
	    ELSE
		ISBYT = 10
	    ENDIF
	ELSE IF(S1MNEM.EQ.'MTV') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 14 
	    ELSE
		ISBYT = 11 
	    ENDIF
	ELSE IF(S1MNEM.EQ.'MTVL') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 15 
	    ELSE
		ISBYT = 12 
	    ENDIF
	ELSE IF(S1MNEM.EQ.'YEAR') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 16
		IWID = 16
	    ELSE
		IRET = -1
	    ENDIF
	ELSE IF(S1MNEM.EQ.'YCEN') THEN
	    IF(IBEN.LT.4) THEN
		ISBYT = 13
	    ELSE
		IRET = -1
	    ENDIF
	ELSE IF(S1MNEM.EQ.'CENT') THEN
	    IF(IBEN.LT.4) THEN
		ISBYT = 18
	    ELSE
		IRET = -1
	    ENDIF
	ELSE IF(S1MNEM.EQ.'MNTH') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 18 
	    ELSE
		ISBYT = 14 
	    ENDIF
	ELSE IF(S1MNEM.EQ.'DAYS') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 19 
	    ELSE
		ISBYT = 15 
	    ENDIF
	ELSE IF(S1MNEM.EQ.'HOUR') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 20 
	    ELSE
		ISBYT = 16 
	    ENDIF
	ELSE IF(S1MNEM.EQ.'MINU') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 21 
	    ELSE
		ISBYT = 17 
	    ENDIF
	ELSE IF(S1MNEM.EQ.'SECO') THEN
	    IF(IBEN.EQ.4) THEN
		ISBYT = 22 
	    ELSE
		IRET = -1 
	    ENDIF
	ELSE
	    IRET = -1
	ENDIF

	RETURN
	END
