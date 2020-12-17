/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CWBMG
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT:  THIS ROUTINE WRITES A SPECIFIED NUMBER OF BYTES TO THE
C   SYSTEM FILE MOST RECENTLY OPENED FOR WRITING/OUTPUT VIA BUFR
C   ARCHIVE LIBRARY ROUTINE COBFL.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL CWBMG( BMG, NMB, IRET )
C   INPUT ARGUMENT LIST:
C     BMG      - CHARACTER*1: ARRAY CONTAINING BYTES TO BE WRITTEN
C     NMB      - INTEGER: NUMBER OF BYTES WITHIN BMG TO BE WRITTEN
C
C   OUTPUT ARGUMENT LIST:
C     IRET     - INTEGER: RETURN CODE:
C                  0 = normal return
C                 -1 = I/O error occurred while writing
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT
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

void cwbmg( char *bmg, f77int *nmb, f77int *iret )
{
    char errstr[129];

/*
**  Make sure that a file is open for writing.
*/
    if ( pbf[1] == NULL ) {
	sprintf( errstr, "BUFRLIB: CWBMG - NO FILE IS OPEN FOR WRITING" );
        bort( errstr, ( f77int ) strlen( errstr ) );
    }
/*
**  Write the BUFR message to the file.
*/
    *iret = ( ( fwrite( bmg, 1, *nmb, pbf[1] ) == *nmb ) ? 0 : -1 );

    return;
}
