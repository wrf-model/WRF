	SUBROUTINE CNVED4(MSGIN,LMSGOT,MSGOT)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    CNVED4
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
!
! ABSTRACT: THIS SUBROUTINE READS AN INPUT BUFR MESSAGE ENCODED USING
!   BUFR EDITION 3 AND OUTPUTS AN EQUIVALENT BUFR MESSAGE ENCODED USING
!   BUFR EDITION 4.  THE OUTPUT MESSAGE WILL BE SLIGHTLY LONGER THAN THE
!   INPUT MESSAGE, SO THE USER MUST ALLOW FOR ENOUGH SPACE WITHIN THE
!   MSGOT ARRAY.  NOTE THAT MSGIN AND MSGOT MUST BE SEPARATE ARRAYS.
!
! PROGRAM HISTORY LOG:
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
!
! USAGE:    CALL STNDRD (MSGIN, LMSGOT, MSGOT)
!   INPUT ARGUMENT LIST:
!     MSGIN    - INTEGER: *-WORD ARRAY CONTAINING BUFR MESSAGE ENCODED
!                USING BUFR EDITION 3
!     LMSGOT   - INTEGER: DIMENSIONED SIZE (IN INTEGER WORDS) OF MSGOT;
!                USED BY THE SUBROUTINE TO ENSURE THAT IT DOES NOT
!                OVERFLOW THE MSGOT ARRAY
!
!   OUTPUT ARGUMENT LIST:
!     MSGOT    - INTEGER: *-WORD ARRAY CONTAINING INPUT BUFR MESSAGE
!                NOW ENCODED USING BUFR EDITION 4
!
! REMARKS:
!    MSGIN AND MSGOT MUST BE SEPARATE ARRAYS.
!
!    THIS ROUTINE CALLS:        BORT     GETLENS  IUPBS01  MVB
!                               PKB
!    THIS ROUTINE IS CALLED BY: MSGWRT
!                               Also called by application programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

	DIMENSION MSGIN(*), MSGOT(*)

	COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!	Verify that the input message is not already encoded in
!	BUFR edition 4.

	IF(IUPBS01(MSGIN,'BEN').EQ.4) GOTO 900

!	Get some section lengths and addresses from the input message.

	CALL GETLENS(MSGIN,3,LEN0,LEN1,LEN2,LEN3,L4,L5)

	IAD2 = LEN0 + LEN1
	IAD4 = IAD2 + LEN2 + LEN3 

	LENM = IUPBS01(MSGIN,'LENM')

!	Check for overflow of the output array.  Note that the new
!	edition 4 message will be a total of 3 bytes longer than the
!	input message (i.e. 4 more bytes in Section 1, but 1 fewer
!	byte in Section 3).

	LENMOT = LENM + 3
	IF(LENMOT.GT.((LMSGOT*NBYTW)-8)) GOTO 901 

	LEN1OT = LEN1 + 4
	LEN3OT = LEN3 - 1

!	Write Section 0 of the new message into the output array.

	CALL MVB ( MSGIN, 1, MSGOT, 1, 4 )
	IBIT = 32
	CALL PKB ( LENMOT, 24, MSGOT, IBIT )
	CALL PKB ( 4, 8, MSGOT, IBIT )

!	Write Section 1 of the new message into the output array.

	CALL PKB ( LEN1OT, 24, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'BMT'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'OGCE'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'GSES'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'USN'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'ISC2')*128, 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTYP'), 8, MSGOT, IBIT )

!	Set a default of 255 for the international subcategory.

	CALL PKB ( 255, 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MSBT'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTV'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTVL'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'YEAR'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MNTH'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'DAYS'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'HOUR'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MINU'), 8, MSGOT, IBIT )

!	Set a default of 0 for the second.

	CALL PKB ( 0, 8, MSGOT, IBIT )

!	Copy Section 2 (if it exists) through the next-to-last byte
!	of Section 3 from the input array to the output array.

	CALL MVB ( MSGIN, IAD2+1, MSGOT, (IBIT/8)+1, LEN2+LEN3-1 )

!	Store the length of the new Section 3.

	IBIT = ( LEN0 + LEN1OT + LEN2 ) * 8
	CALL PKB ( LEN3OT, 24, MSGOT, IBIT )
	
!	Copy Section 4 and Section 5 from the input array to the
!	output array.

	IBIT = IBIT + ( LEN3OT * 8 ) - 24
	CALL MVB ( MSGIN, IAD4+1, MSGOT, (IBIT/8)+1, LENM-IAD4 )

	RETURN
900	CALL BORT('BUFRLIB: CNVED4 - INPUT MSG IS ALREADY EDITION 4')
901	CALL BORT('BUFRLIB: CNVED4 - OVERFLOW OF OUTPUT (EDITION 4) '// &
          'MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
	END SUBROUTINE CNVED4
