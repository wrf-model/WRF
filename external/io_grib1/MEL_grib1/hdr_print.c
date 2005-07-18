/* FILENAME:   hdr_print.c */
#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
* 
********************************************************************
* A. FUNCTION:  hdr_print
*      print specified number of bytes from the block provided.
*      does not require Debug flag to be set;
*
*    INTERFACE:
*      void  hdr_print (title, block, bytestoprint)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *title;		Title string to print
*      (I)  unsigned char *block;       Block whose content to print
*      (I)  int bytestoprint;           Number of bytes to print 
*
*    RETURN CODE:   none;
********************************************************************
*/
#if PROTOTYPE_NEEDED
void  hdr_print (char *title, unsigned char *block, int bytestoprint)
#else
void  hdr_print (title, block, bytestoprint)
		char *title; unsigned char *block; int bytestoprint;
#endif
{
int i=0;
/*
*
* A.1       PRINT title string
*/
   fprintf(stdout,"hdr_print %d bytes of '%s'=", bytestoprint, title);

/*
* 
* A.2       WHILE (more bytes to print) DO
*                PRINT byte value
*           ENDDO
*/
   while (i < bytestoprint)
    {
	if (i % 8 == 0) {
	   if (i+7>= bytestoprint-1)
		fprintf(stdout,"\n[%2d-%2d]:  ",i+1, bytestoprint);
      	   else fprintf(stdout,"\n[%2d-%2d]:  ",i+1, i+8);
	}
	fprintf(stdout,"%03u ", block[i++]);
	if (i % 4 == 0) fprintf(stdout, "| ");
    }
   fprintf(stdout,"\n");
/*
*
* A.3      RETURN w/nothing
*/
   fprintf(stdout,"Exiting hdr_print, no return code\n");
/*
*
* END OF FUNCTION
*
*/ 
}
