/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CCBFL
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT:  THIS ROUTINE CLOSES (AND FLUSHES ANY REMAINING OUTPUT TO!)
C   ANY SYSTEM FILES THAT ARE STILL OPEN FROM ANY PREVIOUS CALLS TO BUFR
C   ARCHIVE LIBRARY SUBROUTINE COBFL.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL CCBFL
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
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

void ccbfl( void )
{
    unsigned short i;

    for ( i = 0; i < 2; i++ ) {
        if ( pbf[i] != NULL ) fclose( pbf[i] );
    }
}
