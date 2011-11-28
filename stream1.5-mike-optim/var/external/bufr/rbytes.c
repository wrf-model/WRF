/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RBYTES
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT:  THIS FUNCTION READS A SPECIFIED NUMBER OF BYTES FROM
C   THE SYSTEM FILE MOST RECENTLY OPENED FOR READING/INPUT VIA
C   BUFR ARCHIVE LIBRARY ROUTINE COBFL.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    RBYTES( BMG, MXMB, ISLOC, NEWBYTES )
C   INPUT ARGUMENT LIST:
C     MXMB     - INTEGER: DIMENSIONED SIZE (IN BYTES) OF BMG; USED
C                BY THE FUNCTION TO ENSURE THAT IT DOES NOT OVERFLOW
C                THE BMG ARRAY
C     ISLOC    - INTEGER: STARTING BYTE NUMBER WITHIN BMG INTO
C                WHICH TO READ THE NEXT NEWBYTES BYTES
C     NEWBYTES - INTEGER: NUMBER OF BYTES TO READ FROM THE SYSTEM
C                FILE MOST RECENTLY OPENED FOR READING/INPUT VIA
C                BUFR ARCHIVE LIBRARY ROUTINE COBFL
C
C   OUTPUT ARGUMENT LIST:
C     BMG      - CHARACTER*1: ARRAY CONTAINING THE NEWBYTES BYTES
C                THAT WERE READ, BEGINNING AT BYTE NUMBER ISLOC
C     RBYTES   - INTEGER: RETURN CODE:
C                  0 = normal return
C                  1 = overflow of BMG array
C                 -1 = end-of-file encountered while reading
C                 -2 = I/O error encountered while reading
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: CRBMG
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"

f77int rbytes( char *bmg, f77int *mxmb, f77int isloc, f77int newbytes )
{
    short iret;

    if ( ( isloc + newbytes ) > *mxmb ) {
	iret = 1;
    }
    else if ( fread( &bmg[isloc], 1, newbytes, pbf[0] ) != newbytes ) {
	iret = ( feof(pbf[0]) ? -1 : -2 );
    }
    else {
	iret = 0;
    }

    return (f77int) iret;
}
