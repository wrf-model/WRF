/*
  Program Name:
  Author(s)/Contact(s):
  Abstract:
  History Log:
 
  Usage:
  Parameters: <Specify typical arguments passed>
  Input Files:
        <list file names and briefly describe the data they include>
  Output Files:
        <list file names and briefly describe the information they include>
 
  Condition codes:
        <list exit condition or error codes returned >
        If appropriate, descriptive troubleshooting instructions or
        likely causes for failures could be mentioned here with the
        appropriate error code
 
  User controllable options: <if applicable>

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bzlib.h>

FILE *f[255];
BZFILE *b[255];

#define BUFFLEN 10485760

int  ibuf = 0;
int  nbuf;
int rdit;
char buffer[BUFFLEN];

int bz_open_(char *flnm, int *iunit, int *ierr, int *ivrb) {

  /*      Input:  *flnm,  *ivrb   */
  /*      Output: *iunit, *ierr   */
  /* ivrb is a verbose flag */

  int bzerror;
  int i;

  *ierr = 0;
/*   printf("flnm = '%s'\n", flnm); */

  /* Find an iunit number, the first unused *f and *b index */
  /* Use only numbers greater than 99 */
  for (i = 100; i<256; i++) {
    if ( ! f[i] ) break;
  }
  if (i > 255) {
    printf("too many in bz_open_\n");
    exit ( 1 ) ;
  }
  *iunit = i;
  

  f[*iunit] = fopen(flnm, "r");
  if ( !f ) {
    printf("open error: '%s'\n", flnm);
    *ierr = 1;
    return (0);
    exit ( 1 ) ;
  }

  b[*iunit] = BZ2_bzReadOpen (&bzerror, f[*iunit], 0, 1, NULL, 0);
  if (bzerror != BZ_OK) {
    printf("bzopen error: '%s'\n", flnm);
    BZ2_bzReadClose (&bzerror, b[*iunit]);
    *ierr = 2;
    return(0);
    exit ( 1 ) ;
  }

  rdit = 1;
  if (*ivrb > 0) printf("BZOPEN file '%s' as bzunit %i\n", flnm, *iunit);
  return (0);
}

int bz_read_string_(int *iunit, unsigned char *string, int *length, int *ierr){

  int nbuf;
  int bzerror;

  nbuf = BZ2_bzRead(&bzerror, b[*iunit], string, *length);

  *ierr = bzerror;
  return(0);

}

int bz_restart_(int *offset) {
  rdit = 0;
  ibuf += *offset;
  if (ibuf < 0) ibuf = 0;
}

int bz_advance_line_() {
  while ( buffer[ibuf] != '\n') ibuf++;
  ibuf++;
}

int bz_getline_(int *iunit, char *string, int *length, int *ierr) {
  /* 
     Error flags returned:  
            -1:  ?
  */

  int bzerror;
  int istring;

  /* Initialize our string to all blanks. */
  for (istring=0; istring<*length; istring++) {
    string[istring] = ' ';
  }

  /* For  efficiency considerations, read a chunk into a buffer, and 
     take bytes from the buffer as needed.  When the buffer is exhausted,
     read more into the buffer */

  istring = 0;
  bzerror = BZ_OK;

 READMORE:

  if ((ibuf == 0) && ( rdit )) {
    printf("reading to buffer ... ");
    nbuf = BZ2_bzRead(&bzerror, b[*iunit], buffer, BUFFLEN);
    printf("nbuf = %i\n", nbuf);
    switch (bzerror) {
    case BZ_OK:
      /* ok */
      *ierr = 0;
      break;
    case BZ_STREAM_END:
      /* ok */
      *ierr = 0;
      break;
    case BZ_PARAM_ERROR:
      printf("bzerror == BZ_PARAM_ERROR\n");
      *ierr = -1;
      return(0);
      break;
    case BZ_SEQUENCE_ERROR:
      printf("bzerror == BZ_SEQUENCE_ERROR\n");
      *ierr = -1;
      return(0);
      break;
    case BZ_IO_ERROR:
      printf("bzerror == BZ_SEQUENCE_ERROR\n");
      *ierr = -1;
      return(0);
      break;
    case BZ_UNEXPECTED_EOF:
      printf("bzerror == BZ_UNEXPECTED_EOF\n");
      *ierr = -1;
      return(0);
      break;
    case BZ_DATA_ERROR:
      printf("bzerror == BZ_DATA_ERROR\n");
      *ierr = -1;
      return(0);
      break;
    case BZ_DATA_ERROR_MAGIC:
      printf("bzerror == BZ_DATA_ERROR_MAGIC\n");
      *ierr = -1;
      return(0);
      break;
    case BZ_MEM_ERROR:
      printf("bzerror == BZ_MEM_ERROR\n");
      *ierr = -1;
      return(0);
      break;
    default:
      printf("other\n");
      *ierr = -1;
      return(0);
      exit ( -1 );
    }
  }

  /* Copy bytes from the buffer to the string, until we hit a newline
     or we've exhausted the buffer */
/*   printf("ibuf, nbuf = %i %i\n", ibuf, nbuf); */
  while ((ibuf < nbuf) && ( buffer[ibuf] != '\n')) {
    string[istring] = buffer[ibuf];
    istring++;
    ibuf++;
    if (ibuf == nbuf) {
      ibuf = 0;
      goto READMORE;
    }
  }
  if (buffer[ibuf] == '\n') ibuf++;
  if (ibuf == nbuf) ibuf = 0;

  istring--;

  if (istring < 0) {
    printf("Problem!\n");
    exit (1);
  }
  string[istring] = ' ';

/*   printf("string = '%s'\n", string); */
  *ierr = 0;
  return(0);

}

#if defined (__SLOW__)

int bz_getline_(int *iunit, char *string, int *length, int *ierr){
  /* 
     Error flags returned:  
            -1:  ?
  */

  int nbuf;
  int bzerror;
  int istring;

  char h;

  for (istring=0; istring<*length; istring++) {
    string[istring] = ' ';
  }

  /* Read one character at a time */
  /* Probably very inefficient, but what the heck.  It works. */
  istring = 0;
  h = NULL;
  while ( h != '\n') {
    nbuf = BZ2_bzRead(&bzerror, b[*iunit], &h, 1);
    if (bzerror == BZ_OK) {
      string[istring] = h;
      istring++;
    }
    else {
/*       printf("bzerror = %i\n", bzerror); */
/*       printf("h = %i : '%c' \n", h, h); */
      if (bzerror == -1) {
	*ierr = -1;
	return(0);
      }
    }
  }
  istring--;
  if (istring < 0) {
    printf("Problem!\n");
    exit (1);
  }
  string[istring] = ' ';

  *ierr = bzerror;
  return(0);

}
#endif

int bz_close_(int *iunit, int *ierr, int *ivrb) {
  int bzerror;

  if (*ivrb > 0) printf("Attempting to BZCLOSE bzunit %i\n", *iunit);

  if (b[*iunit]) {
    BZ2_bzReadClose (ierr, b[*iunit]);
  }
  if (f[*iunit]) {
    fclose(f[*iunit]);
  }
  b[*iunit] = NULL;
  f[*iunit] = NULL;
  ibuf = 0;
  if (*ivrb > 0) printf("BZCLOSE bzunit %i\n", *iunit);
  return(0);
}
