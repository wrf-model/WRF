	SUBROUTINE PKBS1(IVAL,MBAY,S1MNEM)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    PKBS1
!   PRGMMR: J. ATOR          ORG: NP12       DATE: 2005-11-29
!
! ABSTRACT: THIS SUBROUTINE STORES A SPECIFIED INTEGER VALUE INTO A
!   SPECIFIED LOCATION WITHIN SECTION 1 OF THE BUFR MESSAGE STORED IN
!   ARRAY MBAY, OVERWRITING THE VALUE PREVIOUSLY STORED AT THAT
!   LOCATION.  IT WILL WORK ON ANY MESSAGE ENCODED USING BUFR EDITION
!   2, 3 OR 4.  THE START OF THE BUFR MESSAGE (I.E. THE STRING "BUFR")
!   MUST BE ALIGNED ON THE FIRST FOUR BYTES OF MBAY, AND THE LOCATION
!   WITHIN WHICH TO STORE THE VALUE IS SPECIFIED VIA THE MNEMONIC
!   S1MNEM, AS EXPLAINED IN FURTHER DETAIL BELOW.
!
! PROGRAM HISTORY LOG:
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
! 2006-04-14  D. KEYSER  -- ADDED OPTIONS FOR 'MTYP', 'MSBT', 'YEAR',
!                           'MNTH', 'DAYS', 'HOUR', 'YCEN' AND 'CENT'
!
! USAGE:    PKBS1 (IVAL, MBAY, S1MNEM)
!   INPUT ARGUMENT LIST:
!     IVAL     - INTEGER: VALUE TO BE STORED
!     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING
!                BUFR MESSAGE PRIOR TO STORING IVAL
!     S1MNEM   - CHARACTER*(*): MNEMONIC SPECIFYING LOCATION WHERE IVAL
!                IS TO BE STORED WITHIN SECTION 1 OF BUFR MESSAGE:
!                  'BMT'   = BUFR MASTER TABLE
!                  'OGCE'  = ORIGINATING CENTER
!                  'GSES'  = ORIGINATING SUBCENTER
!                              (NOTE: THIS VALUE IS STORED ONLY IN
!                                     BUFR EDITION 3 OR 4 MESSAGES!)
!                  'USN'   = UPDATE SEQUENCE NUMBER
!                  'MTYP'  = DATA CATEGORY
!                  'MSBTI' = DATA SUBCATEGORY (INTERNATIONAL)
!                              (NOTE: THIS VALUE IS STORED ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!                  'MSBT'  = DATA SUBCATEGORY (LOCAL)
!                  'MTV'   = VERSION NUMBER OF MASTER TABLE
!                  'MTVL'  = VERSION NUMBER OF LOCAL TABLES
!                  'YCEN'  = YEAR OF CENTURY (1-100)
!                              (NOTE: THIS VALUE IS STORED ONLY IN
!                                     BUFR EDITION 2 AND 3 MESSAGES!)
!                  'CENT'  = CENTURY (I.E., 20 FOR YEARS 1901-2000,
!                                           21 FOR YEARS 2001-2100)
!                              (NOTE: THIS VALUE IS STORED ONLY IN
!                                     BUFR EDITION 2 AND 3 MESSAGES!)
!                  'YEAR'  = YEAR (4-DIGIT)
!                              (NOTE: THIS VALUE IS STORED ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!                  'MNTH'  = MONTH
!                  'DAYS'  = DAY
!                  'HOUR'  = HOUR
!                  'MINU'  = MINUTE
!                  'SECO'  = SECOND
!                              (NOTE: THIS VALUE IS STORED ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!
!   OUTPUT ARGUMENT LIST:
!     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR
!                MESSAGE WITH IVAL NOW STORED AS REQUESTED
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT     GETS1LOC IUPBS01  PKB      
!    THIS ROUTINE IS CALLED BY: MINIMG   MSGWRT
!                               Also called by application programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

	DIMENSION	MBAY(*)

	CHARACTER*(*)	S1MNEM

	CHARACTER*128	BORT_STR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!	Note that the following call to function IUPBS01 will ensure
!	that subroutine WRDLEN has been called.

	IBEN = IUPBS01(MBAY,'BEN')

!	Determine where to store the value.

	CALL GETS1LOC(S1MNEM,IBEN,ISBYT,IWID,IRET)
	IF ( (IRET.EQ.0) .AND. &
      	     ( (S1MNEM.EQ.'USN') .OR. (S1MNEM.EQ.'BMT')   .OR. &
      	       (S1MNEM.EQ.'OGCE') .OR. (S1MNEM.EQ.'GSES')  .OR. &
      	       (S1MNEM.EQ.'MTYP') .OR. (S1MNEM.EQ.'MSBTI') .OR. &
      	       (S1MNEM.EQ.'MSBT') .OR. (S1MNEM.EQ.'MTV')   .OR. &
      	       (S1MNEM.EQ.'MTVL') .OR. (S1MNEM.EQ.'YCEN')  .OR. &
      	       (S1MNEM.EQ.'CENT') .OR. (S1MNEM.EQ.'YEAR')  .OR. &
      	       (S1MNEM.EQ.'MNTH') .OR. (S1MNEM.EQ.'DAYS')  .OR. &
      	       (S1MNEM.EQ.'HOUR') .OR. (S1MNEM.EQ.'MINU')  .OR. &
      	       (S1MNEM.EQ.'SECO') ) ) THEN

!	    Store the value.

	    IBIT = (IUPBS01(MBAY,'LEN0')+ISBYT-1)*8
	    CALL PKB(IVAL,IWID,MBAY,IBIT)
	ELSE
	    GOTO 900
	ENDIF

	RETURN
900	WRITE(BORT_STR,'("BUFRLIB: PKBS1 - CANNOT OVERWRITE LOCATION '// &
      	    'CORRESPONDING TO MNEMONIC (",A,") WITHIN BUFR EDITION '// &
      	    '(",I1,")")') S1MNEM, IBEN
      	CALL BORT(BORT_STR)
	END SUBROUTINE PKBS1
