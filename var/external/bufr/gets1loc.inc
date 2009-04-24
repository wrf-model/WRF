      SUBROUTINE GETS1LOC(S1MNEM,IBEN,ISBYT,IWID,IRET)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:   GETS1LOC 
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
!
! ABSTRACT: THIS SUBROUTINE RETURNS THE LOCATION (I.E. STARTING BYTE
!   AND BIT WIDTH) OF A SPECIFIED VALUE WITHIN SECTION 1 OF A BUFR
!   MESSAGE ENCODED ACCORDING TO A SPECIFIED BUFR EDITION.  IT WILL
!   WORK ON ANY MESSAGE ENCODED USING BUFR EDITION 2, 3 OR 4.  THE
!   VALUE FOR WHICH THE LOCATION IS TO BE DETERMINED IS SPECIFIED VIA
!   THE MNEMONIC S1MNEM, AS EXPLAINED IN FURTHER DETAIL BELOW.
!
! PROGRAM HISTORY LOG:
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
! 2006-04-14  D. KEYSER  -- ADDED OPTIONS FOR 'YCEN' AND 'CENT'
!
! USAGE:    GETS1LOC ( S1MNEM, IBEN, ISBYT, IWID, IRET )
!   INPUT ARGUMENT LIST:
!     S1MNEM   - CHARACTER*(*): MNEMONIC SPECIFYING VALUE WHOSE
!                LOCATION WITHIN SECTION 1 IS TO BE DETERMINED:
!                  'LEN1'  = LENGTH (IN BYTES) OF SECTION 1
!                  'BMT'   = BUFR MASTER TABLE 
!                  'OGCE'  = ORIGINATING CENTER
!                  'GSES'  = ORIGINATING SUBCENTER
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 3 OR 4 MESSAGES!)
!                  'USN'   = UPDATE SEQUENCE NUMBER 
!                  'ISC2'  = FLAG INDICATING ABSENCE/PRESENCE OF
!                            (OPTIONAL) SECTION 2 IN BUFR MESSAGE:
!                              0 = SECTION 2 ABSENT
!                              1 = SECTION 2 PRESENT
!                  'MTYP'  = DATA CATEGORY 
!                  'MSBTI' = DATA SUBCATEGORY (INTERNATIONAL)
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!                  'MSBT'  = DATA SUBCATEGORY (LOCAL)
!                  'MTV'   = VERSION NUMBER OF MASTER TABLE
!                  'MTVL'  = VERSION NUMBER OF LOCAL TABLES
!                  'YCEN'  = YEAR OF CENTURY (1-100)
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 2 AND 3 MESSAGES!)
!                  'CENT'  = CENTURY (I.E., 20 FOR YEARS 1901-2000,
!                                           21 FOR YEARS 2001-2100)
!                              (NOTE: THIS VALUE *MAY* BE PRESENT IN
!                                     BUFR EDITION 2 AND 3 MESSAGES,
!                                     BUT IT IS NEVER PRESENT IN ANY
!                                     BUFR EDITION 4 MESSAGES!)
!                  'YEAR'  = YEAR (4-DIGIT)
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!                  'MNTH'  = MONTH
!                  'DAYS'  = DAY
!                  'HOUR'  = HOUR
!                  'MINU'  = MINUTE
!                  'SECO'  = SECOND
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!     IBEN     - INTEGER: BUFR EDITION NUMBER
!
!
!   OUTPUT ARGUMENT LIST:
!     ISBYT    - INTEGER: NUMBER OF STARTING BYTE WITHIN SECTION 1
!                WHICH CONTAINS VALUE CORRESPONDING TO S1MNEM
!                   (NOTE: ISBYT IS ALWAYS RETURNED AS 18 WHENEVER
!                          S1MNEM = 'CENT' AND IBEN = 2 OR 3; IN SUCH
!                          CASES IT IS THEN UP TO THE CALLING ROUTINE
!                          TO DETERMINE WHETHER THIS LOCATION ACTUALLY
!                          CONTAINS A VALID CENTURY VALUE!)
!     IWID     - INTEGER: WIDTH (IN BITS) OF VALUE CORRESPONDING
!                TO S1MNEM
!     IRET     - INTEGER: RETURN CODE
!                   0 = NORMAL RETURN
!                  -1 = THE INPUT S1MNEM MNEMONIC IS INVALID FOR
!                       BUFR EDITION IBEN
!
! REMARKS:
!    THIS ROUTINE CALLS:        None
!    THIS ROUTINE IS CALLED BY: CRBMG    IUPBS01  PKBS1
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

	CHARACTER*(*)	S1MNEM

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

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

!               Note that this location is actually the same for both
!               Edition 2 *and* Edition 4 of BUFR!

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
	END SUBROUTINE GETS1LOC
