/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:   CMPIA 
C   PRGMMR: ATOR             ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT:  THIS ROUTINE DEFINES A COMPARISON BETWEEN TWO INTEGERS
C   FOR USE BY THE BINARY SEARCH FUNCTION BSEARCH.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL CMPIA( PF1, PF2 )
C   INPUT ARGUMENT LIST:
C     PF1      - INTEGER: FIRST INTEGER TO BE COMPARED
C     PF2      - INTEGER: SECOND INTEGER TO BE COMPARED
C
C   OUTPUT ARGUMENT LIST:
C     CMPIA    - INTEGER: RESULT OF COMPARISON:
C                      -1 = PF1 is less than PF2
C                       0 = PF1 is equal to PF2
C                       1 = PF1 is greater than PF2
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: NUMMTB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"

int cmpia( const f77int *pf1, const f77int *pf2 )
{
	if ( *pf1 == *pf2 ) return 0;

	return ( *pf1 < *pf2 ? -1 : 1 );
}
