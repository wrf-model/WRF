	INTEGER FUNCTION IPKS(VAL,NODE)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:   IPKS
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2012-03-02
C
C ABSTRACT: THIS FUNCTION PACKS A REAL*8 USER VALUE INTO A BUFR
C   INTEGER BY APPLYING THE PROPER SCALE AND REFERENCE VALUES.
C   NORMALLY THE SCALE AND REFERENCE VALUES ARE OBTAINED FROM INDEX
C   NODE OF THE INTERNAL JUMP/LINK TABLE ARRAYS ISC(*) AND IRF(*);
C   HOWEVER, THE REFERENCE VALUE IN IRF(*) WILL BE OVERRIDDEN IF A
C   2-03 OPERATOR IS IN EFFECT FOR THIS NODE.
C
C PROGRAM HISTORY LOG:
C 2012-03-02  J. ATOR    -- ORIGINAL AUTHOR; ADAPTED FROM INTERNAL
C                           STATEMENT FUNCTION IN WRTREE
C
C USAGE:    IPKS (VAL,NODE)
C   INPUT ARGUMENT LIST:
C     VAL       - REAL*8: USER VALUE
C     NODE      - INTEGER: INDEX INTO INTERNAL JUMP/LINK TABLES
C
C   OUTPUT ARGUMENT LIST:
C     IPKS      - INTEGER: PACKED BUFR VALUE
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: WRTREE
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
     .			JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .			IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .			ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .			ISEQ(MAXJL,2),JSEQ(MAXJL)
	COMMON /NRV203/ NNRV,INODNRV(MXNRV),NRV(MXNRV),TAGNRV(MXNRV),
     .			ISNRV(MXNRV),IENRV(MXNRV),IBTNRV,IPFNRV

	CHARACTER*10  TAG
	CHARACTER*8   TAGNRV
	CHARACTER*3   TYP

	REAL*8	TEN,VAL

	DATA TEN /10./

C-----------------------------------------------------------------------

	IPKS = NINT( VAL * TEN**(ISC(NODE)) ) - IRF(NODE)

	IF ( NNRV .GT. 0 ) THEN

C	  There are redefined reference values in the jump/link table,
C	  so we need to check if this node is affected by any of them.

	  DO JJ = 1, NNRV
	    IF ( NODE .EQ. INODNRV(JJ) ) THEN

C	      This node contains a redefined reference value.
C	      Per the rules of BUFR, negative values should be encoded
C	      as positive integers with the left-most bit set to 1.

	      NRV(JJ) = NINT(VAL)
	      IF ( NRV(JJ) .LT. 0 ) THEN
		IMASK = 2**(IBT(NODE)-1)
		IPKS = IOR(IABS(NRV(JJ)),IMASK)
	      ELSE
		IPKS = NRV(JJ)
	      END IF
	      RETURN
	    ELSE IF ( ( TAG(NODE)(1:8) .EQ. TAGNRV(JJ) ) .AND.	    
     .		      ( NODE .GE. ISNRV(JJ) ) .AND.
     .		      ( NODE .LE. IENRV(JJ) ) ) THEN

C	      The corresponding redefinded reference value needs to
C	      be used when encoding this value.

	      IPKS = NINT( VAL * TEN**(ISC(NODE)) ) - NRV(JJ)
	      RETURN
	    END IF
	  END DO

	END IF

	RETURN
	END
