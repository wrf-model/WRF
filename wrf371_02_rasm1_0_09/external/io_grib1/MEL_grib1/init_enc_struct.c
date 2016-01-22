/* FILENAME:     init_enc_struct.c
   DATE:         15 FEB 1996
   PROGRAMMER:   STEVE LOWE, SAIC
   REVISED BY:   ALICE NAKAJIMA, SAIC
 */
#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */
/*
*
************************************************************************
* A. FUNCTION:  init_enc_struct
*       initializes structures DATA_INPUT and GEOM_IN
*
*    INTERFACE:
*       void init_enc_struct (data_input, geom_in, user_input)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (O) DATA_INPUT *data_input;    encoder struct to be initialized
*      (O) GEOM_IN  *geom_in;         encoder struct to be initialized
*      (O) USER_INPUT *user_input;    encoder struct to be initialized
*
*      RETURN CODE:   none
************************************************************************
*/
#if PROTOTYPE_NEEDED
void init_enc_struct ( DATA_INPUT *data_input, GEOM_IN  *geom_in,
			USER_INPUT *user_input)
#else
void init_enc_struct ( data_input, geom_in, user_input)
			DATA_INPUT *data_input; 
			GEOM_IN  *geom_in;
			USER_INPUT *user_input;
#endif
{
  DPRINT0 ("Entering init_enc_struct()\n");
/*
*
* A.1       CLEAR elements of DATA_INPUT Structure
*
* A.2       CLEAR elements of GEOM_IN Structure
*
* A.3       CLEAR elements of USER_INPUT Structure 
*/
  memset ((void *)data_input, '\0', sizeof (DATA_INPUT));
  memset ((void *)geom_in, '\0', sizeof (GEOM_IN));
  memset ((void *)user_input, '\0', sizeof (USER_INPUT));

  DPRINT0 ("Exiting init_enc_struct()\n");
/*
*
* A.4        RETURN
*/
  return;

/*
* END OF FUNCTION
*
*/ 
}
