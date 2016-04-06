/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    COBFL
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT:  THIS ROUTINE OPENS A SPECIFIED SYSTEM FILE FOR READING
C   OR WRITING VIA THE BUFR ARCHIVE LIBRARY C I/O INTERFACE.  THERE
C   CAN BE AT MOST TWO SYSTEM FILES OPEN AT ANY GIVEN TIME (ONE FOR
C   READING/INPUT AND ONE FOR WRITING/OUTPUT).  IF A CALL TO THIS
C   ROUTINE IS MADE FOR EITHER READING/INPUT OR WRITING/OUTPUT AND
C   SUCH A FILE IS ALREADY OPEN TO THE BUFR ARCHIVE LIBRARY C I/O
C   INTERFACE, THEN THAT FILE WILL BE CLOSED BEFORE OPENING THE
C   NEW ONE.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL COBFL( BFL, IO )
C   INPUT ARGUMENT LIST:
C     BFL      - CHARACTER*(*): SYSTEM FILE TO BE OPENED. INCLUSION
C                OF DIRECTORY PREFIXES OR OTHER LOCAL FILESYSTEM
C                NOTATION IS ALLOWED UP TO 120 TOTAL CHARACTERS.
C     IO       - CHARACTER: FLAG INDICATING HOW BFL IS TO BE OPENED
C                FOR USE WITH THE C I/O INTERFACE:
C                   'r' = READING (INPUT)
C                   'w' = WRITING (OUTPUT)
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     WRDLEN
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#define BUFRLIB_GLOBAL
#include "bufrlib.h"

#define MXFNLEN 120

void cobfl( char *bfl, char *io )
{
    char lbf[MXFNLEN+1];
    char lio;

    char errstr[129];

    char foparg[3] = " b";  /* 3rd character will automatically
			       initialize to NULL */
    unsigned short i, j;

/*
**  Copy the input arguments into local variables and check them for validity.
**  This is especially important in case either of the arguments was passed in
**  as a string literal by the calling program or else doesn't have a trailing
**  NULL character.
*/
    for ( i = 0; ( ! isspace( bfl[i] ) && ! iscntrl( bfl[i] ) ); i++ ) {
	if ( i == MXFNLEN ) {
	    sprintf( errstr, "BUFRLIB: COBFL - INPUT FILENAME CONTAINS"
			    " MORE THAN %d CHARACTERS", MXFNLEN );
	    bort( errstr, ( f77int ) strlen( errstr ) );
	}
	lbf[i] = bfl[i];
    }
    lbf[i] = '\0';

    lio = io[0];
    if ( ( foparg[0] = (char) tolower( lio ) ) == 'r' ) {
	j = 0;
    }
    else if ( foparg[0] == 'w' ) {
	j = 1;
    }
    else {
	sprintf( errstr, "BUFRLIB: COBFL - SECOND ARGUMENT WAS (%c),"
			" WHICH IS AN ILLEGAL VALUE", lio );
	bort( errstr, ( f77int ) strlen( errstr ) );
    }

/*
**  If a file of this type is already open, then close it before
**  opening the new one.
*/
    if ( pbf[j] != NULL ) fclose( pbf[j] );

/*
**  Open the requested file.
*/
    if ( ( pbf[j] = fopen( lbf, foparg ) ) == NULL ) {
	sprintf( errstr, "BUFRLIB: COBFL - COULD NOT OPEN FILE %s", lbf );
	bort( errstr, ( f77int ) strlen( errstr ) );
    }

/*
**  Call wrdlen to initialize some important information about the
**  local machine, just in case it hasn't already been called.
*/
    wrdlen( );
    
    return;
}
