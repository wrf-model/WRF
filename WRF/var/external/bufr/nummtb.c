/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:   NUMMTB
C   PRGMMR: ATOR             ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT:  THIS ROUTINE SEARCHES FOR AN ENTRY CORRESPONDING TO IDN
C   IN THE BUFR MASTER TABLE (EITHER 'B' OR 'D', DEPENDING ON THE VALUE
C   OF IDN).  THE SEARCH USES BINARY SEARCH LOGIC, SO ALL OF THE ENTRIES
C   IN THE TABLE MUST BE SORTED IN ASCENDING ORDER (BY FXY NUMBER) IN
C   ORDER FOR THIS ROUTINE TO WORK PROPERLY.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL NUMMTB( IDN, TAB, IPT )
C   INPUT ARGUMENT LIST:
C     IDN      - INTEGER:  BIT-WISE REPRESENTATION OF FXY VALUE TO BE
C                SEARCHED FOR
C
C   OUTPUT ARGUMENT LIST:
C     TAB      - CHARACTER: TABLE IN WHICH IDN WAS FOUND ('B' OR 'D')
C     IPT      - INTEGER: INDEX OF ENTRY FOR IDN IN MASTER TABLE TAB
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     CADN30   CMPIA
C    THIS ROUTINE IS CALLED BY: STSEQ
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#define COMMON_MSTABS
#include "bufrlib.h"

void nummtb( f77int *idn, char *tab, f77int *ipt )
{
	f77int *pifxyn, *pbs,  nmt;

	char adn[7], errstr[129];

	if ( *idn >= ifxy( "300000", 6 ) ) {
	    *tab = 'D';
	    pifxyn = &mstabs.idfxyn[0];
	    nmt = mstabs.nmtd;
	}
	else {
	    *tab = 'B';
	    pifxyn = &mstabs.ibfxyn[0];
	    nmt = mstabs.nmtb;
	}

        pbs = ( f77int * ) bsearch( idn, pifxyn, ( size_t ) nmt, sizeof( f77int ),
				( int (*) ( const void *, const void * ) ) cmpia );
        if ( pbs == NULL ) {
	    cadn30( idn, adn, sizeof( adn ) );
	    adn[6] = '\0';
	    sprintf( errstr, "BUFRLIB: NUMMTB - COULD NOT FIND DESCRIPTOR "
			     "%s IN MASTER TABLE %c", adn, *tab );
	    bort( errstr, ( f77int ) strlen( errstr ) );
	}
	*ipt = pbs - pifxyn;

	return;
}
