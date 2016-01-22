/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RESTD
C   PRGMMR: ATOR             ORG: NP12       DATE: 2004-08-18
C
C ABSTRACT:  GIVEN THE BIT-WISE REPRESENTATION OF A LOCAL
C   (I.E. NON-STANDARD) TABLE D DESCRIPTOR, THIS ROUTINE RETURNS
C   AN EQUIVALENT LIST OF STANDARDIZED CHILD DESCRIPTORS.  ANY CHILD
C   DESCRIPTORS WHICH ARE THEMSELVES LOCAL TABLE D DESCRIPTORS ARE
C   AUTOMATICALLY RESOLVED VIA A RECURSIVE CALL TO THIS SAME ROUTINE.
C   THE RECURSIVE PROCESS CONTINUES UNTIL ALL CHILD DESCRIPTORS ARE
C   EITHER WMO-STANDARD DESCRIPTORS (I.E. FROM TABLE B, TABLE C, OR
C   TABLE D, OR REPLICATION DESCRIPTORS) OR ELSE ARE LOCAL TABLE B
C   DESCRIPTORS, IN WHICH CASE THEY ARE PRECEDED WITH AN APPROPRIATE
C   206YYY TABLE C OPERATOR IN THE OUTPUT LIST.  IN ANY EVENT, THE
C   FINAL OUTPUT LIST OF EQUIVALENT CHILD DESCRIPTORS IS USABLE BY
C   ANY STANDARD BUFR DECODER PROGRAM IN ORDER TO INTERPRET THE SAME
C   DATA VALUES AS WERE REPRESENTED BY THE INITIAL LOCAL TABLE D
C   DESCRIPTOR THAT WAS INPUT.
C
C PROGRAM HISTORY LOG:
C 2004-08-18  J. ATOR    -- ORIGINAL AUTHOR
C 2012-04-30  J. ATOR    -- USE LONG CAST FOR IBIT IN SPRINTF STMT
C
C USAGE:    CALL RESTD( LUN, TDDESC, NCTDDESC, CTDDESC )
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     TDDESC   - INTEGER: BIT-WISE REPRESENTATION OF FXY VALUE FOR
C		 LOCAL TABLE D DESCRIPTOR
C
C   OUTPUT ARGUMENT LIST:
C     NCTDDESC - INTEGER: NUMBER OF STANDARDIZED CHILD DESCRIPTORS
C		 RETURNED IN CTDDESC
C     CTDDESC  - INTEGER: ARRAY OF STANDARDIZED CHILD DESCRIPTORS
C
C REMARKS:
C    THIS ROUTINE CALLS:        RESTD    NUMTBD   NEMTBB   IFXY
C				CADN30   ISTDESC  WRDESC   UPTDD
C    THIS ROUTINE IS CALLED BY: RESTD    STNDRD 
C                               Normally not called by application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"

void restd( f77int *lun, f77int *tddesc, f77int *nctddesc, f77int ctddesc[] )
{
    f77int i0 = 0;

    f77int desc, ncdesc, cdesc[MAXNC];
    f77int i, j, inum, itbd, ictbd;
    f77int iscl, iref, ibit;

    char tab, nemo[9], adn[7], cunit[25];

/*
**  How many child descriptors does *tddesc have?
*/
    numtbd( lun, tddesc, nemo, &tab, &itbd, 9, 1 );
    uptdd( &itbd, lun, &i0, &inum );

    *nctddesc = 0;
/*
**  Examine each child descriptor one at a time.
*/
    for ( i = 1; i <= inum; i++ ) {
	uptdd( &itbd, lun, &i, &desc ); 
	if (! istdesc( &desc ) ) {
/*
**	    desc is a local descriptor.
*/ 
	    numtbd( lun, &desc, nemo, &tab, &ictbd, 9, 1 );
	    if ( tab == 'D' ) {
/*
**		desc is itself a local Table D descriptor, so resolve
**		it now via a recursive call to this same routine.
*/ 
	        restd( lun, &desc, &ncdesc, cdesc );

	        if ( ( *nctddesc > 0 ) &&
		     ( ctddesc[(*nctddesc)-1] >  ifxy( "101000", 6 ) ) &&
		     ( ctddesc[(*nctddesc)-1] <= ifxy( "101255", 6 ) ) ) {
/*
**		    desc is replicated using fixed replication, so write
**		    the number of child descriptors into the X value of
**		    the replication descriptor ctddesc[(*nctddesc)-1]
*/
		    cadn30( &ctddesc[(*nctddesc)-1], adn, 7 );
		    sprintf( adn, "%c%02ld%c%c%c",
			     adn[0], (long) ncdesc, adn[3], adn[4], adn[5] );
		    ctddesc[(*nctddesc)-1] = ifxy( adn, 7 );
		}
		else if ( ( *nctddesc > 1 ) &&
			  ( ctddesc[(*nctddesc)-2] == ifxy( "101000", 6 ) ) ) {
/*
**		    desc is replicated using delayed replication, so write
**		    the number of child descriptors into the X value of
**		    the replication descriptor ctddesc[(*nctddesc)-2]
*/
		    cadn30( &ctddesc[(*nctddesc)-2], adn, 7 );
		    sprintf( adn, "%c%02ld%c%c%c",
			     adn[0], (long) ncdesc, adn[3], adn[4], adn[5] );
		    ctddesc[(*nctddesc)-2] = ifxy( adn, 7 );
		}
/*
**		Add the child descriptors to the output list.
*/
		for ( j = 0; j < ncdesc; j++ ) {
		    wrdesc( cdesc[j], ctddesc, nctddesc );
		}
		    
	    }
	    else if ( tab == 'B' ) {
/*
**		desc is a local Table B descriptor, so precede it with
**		a 206YYY operator in the output list.
*/ 
		nemtbb( lun, &ictbd, cunit, &iscl, &iref, &ibit, 25 );
		sprintf( adn, "%c%c%c%03ld", '2', '0', '6', (long) ibit );
		wrdesc( ifxy( adn, 7 ), ctddesc, nctddesc );
	        wrdesc( desc, ctddesc, nctddesc );
	    }
        }
	else {
/*
**	    desc is a standard Table B, Table D, operator or replicator
**	    descriptor, so append it "as is" to the output list.
*/ 
	    wrdesc( desc, ctddesc, nctddesc );
	}
    }

    return;
}
