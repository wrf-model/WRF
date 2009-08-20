/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CRBMG
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT:  THIS ROUTINE READS THE NEXT BUFR MESSAGE FROM THE SYSTEM
C   FILE MOST RECENTLY OPENED FOR READING/INPUT VIA BUFR ARCHIVE LIBRARY
C   ROUTINE COBFL.  ANY BUFR EDITION 0 OR EDITION 1 MESSAGES THAT ARE
C   READ ARE AUTOMATICALLY CONVERTED TO BUFR EDITION 2.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL CRBMG( BMG, MXMB, NMB, IRET )
C   INPUT ARGUMENT LIST:
C     MXMB     - INTEGER: DIMENSIONED SIZE (IN BYTES) OF BMG; USED
C                BY THE ROUTINE TO ENSURE THAT IT DOES NOT OVERFLOW
C                THE BMG ARRAY
C
C   OUTPUT ARGUMENT LIST:
C     BMG      - CHARACTER*1: ARRAY CONTAINING BUFR MESSAGE
C     NMB      - INTEGER: SIZE (IN BYTES) OF BUFR MESSAGE IN BMG
C     IRET     - INTEGER: RETURN CODE:
C                  0 = normal return
C                  1 = overflow of BMG array
C                  2 = "7777" indicator not found in expected location 
C                 -1 = end-of-file encountered while reading
C                 -2 = I/O error encountered while reading
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     GETS1LOC ICHKSTR  IPKM
C                               IUPBS01  IUPM     RBYTES
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"

void crbmg( char *bmg, f77int *mxmb, f77int *nmb, f77int *iret )
{
    f77int i1 = 1, i2 = 2, i3 = 3, i4 = 4, i24 = 24;
    f77int wkint[2];
    f77int iben, isbyt, iwid;

    char errstr[129];

    unsigned short i, nsecs; 
    unsigned int lsec;
/*
**  Make sure that a file is open for reading.
*/
    if ( pbf[0] == NULL ) {
	sprintf( errstr, "BUFRLIB: CRBMG - NO FILE IS OPEN FOR READING" );
        bort( errstr, ( f77int ) strlen( errstr ) );
    }
/*
**  Initialize the first 4 characters of the output array to blanks.
*/
    if ( *mxmb < 4 ) {
	*iret = 1;
	return;
    }
    strncpy( bmg, "    ", 4);
/*
**  Look for the start of the next BUFR message.
*/
    while ( ichkstr( "BUFR", bmg, &i4, 4, 4 ) != 0 ) {
        memmove( bmg, &bmg[1], 3 );
	if ( ( *iret = rbytes( bmg, mxmb, 3, 1 ) ) != 0 ) return;
    }
/*
**  Read the next 4 bytes and determine the BUFR edition number that was used
**  to encode the message.
*/
    if ( ( *iret = rbytes( bmg, mxmb, 4, 4 ) ) != 0 ) return;
    memcpy( wkint, bmg, 8 );
    iben = iupbs01( wkint, "BEN", 3 );

    if ( iben >= 2 ) {
/*
**	Get the length of the BUFR message.
*/
        *nmb = iupbs01( wkint, "LENM", 4 );
/*
**	Read the remainder of the BUFR message.
*/
	if ( ( *iret = rbytes( bmg, mxmb, 8, *nmb-8 ) ) != 0 ) return;
    }
    else {
/*
**	Read the remainder of the BUFR message and then convert it to BUFR
**	edition 2.  The message length isn't encoded in Section 0, so we need
**	to compute it by unpacking and summing the lengths of the individual
**	sections.
*/
	lsec = 4;   /* length of Section 0 */
/*
**	Get the length of Section 1 and add it to the total.
*/
	gets1loc( "LEN1", &iben, &isbyt, &iwid, &wkint[0], 4 );
	*nmb = lsec + iupm( &bmg[lsec+isbyt-1], &iwid, 3 );
/*
**	Read up through the end of Section 1.
*/
	if ( ( *iret = rbytes( bmg, mxmb, 8, *nmb-8 ) ) != 0 ) return;
/*
**	Is there a Section 2?
*/
	gets1loc( "ISC2", &iben, &isbyt, &iwid, &wkint[0], 4 );
	nsecs = iupm( &bmg[lsec+isbyt-1], &iwid, 1 ) + 2;
/*
**	Read up through the end of Section 4.
*/
	for ( i = 1; i <= nsecs; i++ ) {
	    if ( ( *iret = rbytes( bmg, mxmb, *nmb, 3 ) ) != 0 ) return;
	    lsec = iupm( &bmg[*nmb], &i24, 3 );
	    if ( ( *iret = rbytes( bmg, mxmb, *nmb+3, lsec-3 ) ) != 0 ) return;
	    *nmb += lsec;
	}
/*
**	Read Section 5.
*/
	if ( ( *iret = rbytes( bmg, mxmb, *nmb, 4 ) ) != 0 ) return;
	*nmb += 4;
/*
**	Expand Section 0 from 4 bytes to 8 bytes, then encode the message length
**	and new edition number (i.e. 2) into the new (expanded) Section 0.
*/
	if ( *nmb + 4 > *mxmb ) {
	    *iret = 1;
	    return;
        }
	memmove( &bmg[8], &bmg[4], *nmb-4 );
	*nmb += 4;
	ipkm( &bmg[4], &i3, nmb, 3 );
	ipkm( &bmg[7], &i1, &i2, 1 );
    }
/*
**  Check that the "7777" is in the expected location.
*/
    *iret = ( ( ichkstr( "7777", &bmg[*nmb-4], &i4, 4, 4 ) == 0 ) ? 0 : 2 );

    return;
}
