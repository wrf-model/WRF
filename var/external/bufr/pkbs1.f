	SUBROUTINE PKBS1(IVAL,MBAY,S1MNEM)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PKBS1
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT: THIS SUBROUTINE STORES A SPECIFIED INTEGER VALUE INTO A
C   SPECIFIED LOCATION WITHIN SECTION 1 OF THE BUFR MESSAGE STORED IN
C   ARRAY MBAY, OVERWRITING THE VALUE PREVIOUSLY STORED AT THAT
C   LOCATION.  IT WILL WORK ON ANY MESSAGE ENCODED USING BUFR EDITION
C   2, 3 OR 4.  THE START OF THE BUFR MESSAGE (I.E. THE STRING "BUFR")
C   MUST BE ALIGNED ON THE FIRST FOUR BYTES OF MBAY, AND THE LOCATION
C   WITHIN WHICH TO STORE THE VALUE IS SPECIFIED VIA THE MNEMONIC
C   S1MNEM, AS EXPLAINED IN FURTHER DETAIL BELOW.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C 2006-04-14  D. KEYSER  -- ADDED OPTIONS FOR 'MTYP', 'MSBT', 'YEAR',
C                           'MNTH', 'DAYS', 'HOUR', 'YCEN' AND 'CENT'
C
C USAGE:    PKBS1 (IVAL, MBAY, S1MNEM)
C   INPUT ARGUMENT LIST:
C     IVAL     - INTEGER: VALUE TO BE STORED
C     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING
C                BUFR MESSAGE PRIOR TO STORING IVAL
C     S1MNEM   - CHARACTER*(*): MNEMONIC SPECIFYING LOCATION WHERE IVAL
C                IS TO BE STORED WITHIN SECTION 1 OF BUFR MESSAGE:
C                  'BMT'   = BUFR MASTER TABLE
C                  'OGCE'  = ORIGINATING CENTER
C                  'GSES'  = ORIGINATING SUBCENTER
C                              (NOTE: THIS VALUE IS STORED ONLY IN
C                                     BUFR EDITION 3 OR 4 MESSAGES!)
C                  'USN'   = UPDATE SEQUENCE NUMBER
C                  'MTYP'  = DATA CATEGORY
C                  'MSBTI' = DATA SUBCATEGORY (INTERNATIONAL)
C                              (NOTE: THIS VALUE IS STORED ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C                  'MSBT'  = DATA SUBCATEGORY (LOCAL)
C                  'MTV'   = VERSION NUMBER OF MASTER TABLE
C                  'MTVL'  = VERSION NUMBER OF LOCAL TABLES
C                  'YCEN'  = YEAR OF CENTURY (1-100)
C                              (NOTE: THIS VALUE IS STORED ONLY IN
C                                     BUFR EDITION 2 AND 3 MESSAGES!)
C                  'CENT'  = CENTURY (I.E., 20 FOR YEARS 1901-2000,
C                                           21 FOR YEARS 2001-2100)
C                              (NOTE: THIS VALUE IS STORED ONLY IN
C                                     BUFR EDITION 2 AND 3 MESSAGES!)
C                  'YEAR'  = YEAR (4-DIGIT)
C                              (NOTE: THIS VALUE IS STORED ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C                  'MNTH'  = MONTH
C                  'DAYS'  = DAY
C                  'HOUR'  = HOUR
C                  'MINU'  = MINUTE
C                  'SECO'  = SECOND
C                              (NOTE: THIS VALUE IS STORED ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C
C   OUTPUT ARGUMENT LIST:
C     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR
C                MESSAGE WITH IVAL NOW STORED AS REQUESTED
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     GETS1LOC IUPBS01  PKB      
C    THIS ROUTINE IS CALLED BY: MINIMG   MSGWRT
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	DIMENSION	MBAY(*)

	CHARACTER*(*)	S1MNEM

	CHARACTER*128	BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Note that the following call to function IUPBS01 will ensure
C	that subroutine WRDLEN has been called.

	IBEN = IUPBS01(MBAY,'BEN')

C	Determine where to store the value.

	CALL GETS1LOC(S1MNEM,IBEN,ISBYT,IWID,IRET)
	IF ( (IRET.EQ.0) .AND.
     .	     ( (S1MNEM.EQ.'USN') .OR. (S1MNEM.EQ.'BMT')   .OR.
     .	       (S1MNEM.EQ.'OGCE') .OR. (S1MNEM.EQ.'GSES')  .OR.
     .	       (S1MNEM.EQ.'MTYP') .OR. (S1MNEM.EQ.'MSBTI') .OR.
     .	       (S1MNEM.EQ.'MSBT') .OR. (S1MNEM.EQ.'MTV')   .OR.
     .	       (S1MNEM.EQ.'MTVL') .OR. (S1MNEM.EQ.'YCEN')  .OR.
     .	       (S1MNEM.EQ.'CENT') .OR. (S1MNEM.EQ.'YEAR')  .OR.
     .	       (S1MNEM.EQ.'MNTH') .OR. (S1MNEM.EQ.'DAYS')  .OR.
     .	       (S1MNEM.EQ.'HOUR') .OR. (S1MNEM.EQ.'MINU')  .OR.
     .	       (S1MNEM.EQ.'SECO') ) ) THEN

C	    Store the value.

	    IBIT = (IUPBS01(MBAY,'LEN0')+ISBYT-1)*8
	    CALL PKB(IVAL,IWID,MBAY,IBIT)
	ELSE
	    GOTO 900
	ENDIF

	RETURN
900	WRITE(BORT_STR,'("BUFRLIB: PKBS1 - CANNOT OVERWRITE LOCATION '//
     .	    'CORRESPONDING TO MNEMONIC (",A,") WITHIN BUFR EDITION '//
     .	    '(",I1,")")') S1MNEM, IBEN
      	CALL BORT(BORT_STR)
	END
