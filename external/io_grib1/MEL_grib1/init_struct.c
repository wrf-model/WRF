/* FILENAME:     init_struct.c
   DATE:         15 FEB 1997
   PROGRAMMER:   STEVE LOWE, SAIC

   27aug97 Alice Nakajima (ATN):  changed 'size_t size' to 'int' (gcc complains)
*/ 
#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"	/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
*
************************************************************************
* A. FUNCTION:   init_struct
*       initializes structures DATA_INPUT and GEOM_IN
*
*    INTERFACE:
*       void init_struct (generic, size)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (O)  void    *generic;   address of block to be cleared out
*      (I)  int     size;       size of block in bytes
*
*    RETURN CODE:   none
************************************************************************
*
*/
#if PROTOTYPE_NEEDED
void  init_struct ( void *generic, int size)
#else
void  init_struct ( generic, size)
		void *generic; int size;
#endif
{

  DPRINT0 ("Entering init_struct()\n");
/*
*
* A.1       CLEAR elements of Structure
*/
  memset ((void *)generic, '\0', size);

  DPRINT0 ("Exiting init_struct()\n");
/*
*
* A.2        RETURN
*/
  return;

/*
* END OF FUNCTION
*
*/ 
}
