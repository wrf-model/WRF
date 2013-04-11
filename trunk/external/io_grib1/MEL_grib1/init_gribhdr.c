/*  File:  init_gribhdr.c		Alice T. Nakajima, SAIC, 10/96
    funcs to make storage and free up storage for Grib header struct
*/
#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */
/*
*
****************************************************************************
* A. FUNCTION  init_gribhdr
*       Allocates storage for Grib Header and its entire_msg and initialize
*       every of its attributes.
*
*    INTERFACE:
*       int     init_gribhdr (ppgrib_hdr, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (O)  GRIB_HDR **ppgrib_hdr;
*           Grib Header structure, Null upon entry;  Returns pointing to a 
*           newly created storage.  Its attribute 'entire_msg' will point
*           to a block of size indicated in 'abs_size' (initially set to
*           DEF_MSG_LEN bytes, see grib.h).  'entire_msg' may later be 
*           expanded by other functions if required, but 'abs_size' must
*           be updated to the expanded byte length.
*      (O)  char *errmsg;
*           empty array, returned filled if error occurred;
*
*    RETURNS:  
*     0>  no error; storage for grib header and its entire_msg array created
*         and cleared;  msg_length and all section lengths are set to zero, 
*         all section pointers are Null; abs_size is set to DEF_MSG_LEN;
*         'shuffled' flag is set to zero;
*     1>  failed, see errmsg;
****************************************************************************
*/
#if PROTOTYPE_NEEDED
int  init_gribhdr ( GRIB_HDR **ppgrib_hdr, char *errmsg)
#else
int  init_gribhdr ( ppgrib_hdr, errmsg)
		GRIB_HDR **ppgrib_hdr; char *errmsg;
#endif
{
/*
* A.0       DEFAULT to error status
*/
char *func= "init_gribhdr";
int	stat=1;

   DPRINT1 ("Entering %s\n", func);
/*
*
* A.1       ALLOCATE storage for struct GRIB_HDR
*           IF (fails) THEN
*               RETURN  error
*           ELSE
*               CLEAR out struct GRIB_HDR
*           ENDIF
*/
   *ppgrib_hdr= (GRIB_HDR *)malloc(sizeof(GRIB_HDR));
   if (*ppgrib_hdr == NULL) {

	DPRINT1 ("%s:  failed to create storage for GRIB_HDR\n", func);
	sprintf (errmsg, "%s:  failed to create storage for GRIB_HDR\n", func);
	goto BYE;
	}
   else memset ((void *)*ppgrib_hdr, '\0', sizeof(GRIB_HDR));
   DPRINT2 ("Allocate storage of GRIB_HDR struct, addr=%ld (%ld bytes)\n", 
   *ppgrib_hdr, sizeof(GRIB_HDR));

/*
*
* A.2       ALLOCATE storage for struct GRIB_HDR's Entire_Msg array
*           !size DEF_MSG_LEN bytes as defined in 'grib.h'
*           IF (fails) THEN
*               FREE Grib Header
*               RETURN error
*           ELSE
*               STORE absolute size of Entire_Msg in header's Abs_Size
*               CLEAR out array Entire_Msg of struct
*               SET status to good
*           ENDIF
*/
   
   (*ppgrib_hdr)->entire_msg=  (void *)malloc(DEF_MSG_LEN);
   if ((*ppgrib_hdr)->entire_msg == NULL) {
	DPRINT1 ( "%s:  failed to create storage for GRIB_HDR's Msg\n", func);
	sprintf (errmsg, "%s:  failed to create storage for GRIB_HDR's Msg\n", 
	func);
	free (*ppgrib_hdr);
	}
   else {
	(*ppgrib_hdr)->abs_size = (long)DEF_MSG_LEN;
	memset ((void *)(*ppgrib_hdr)->entire_msg, '\0', DEF_MSG_LEN);
	DPRINT2 (
	"Allocate storage for GRIB_HDR->entire_msg, addr=%ld, sz= %ld bytes \n",
	(*ppgrib_hdr)->entire_msg, (*ppgrib_hdr)->abs_size);
	stat=0;
	}


/*
*
* A.3       RETURN status 
*/
BYE:
   DPRINT2 ("Leaving %s,  stat=%d;\n", func,stat);
   return (stat);

/*
*
* END OF FUNCTION
*
*
*/
}

/*
*
****************************************************************************
* B. FUNCTION:  free_gribhdr
*      to free up storage of Grib Header structure and all its attributes.
*
*    INTERFACE:
*      void    free_gribhdr (ppgrib_hdr)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (O)  GRIB_HDR **ppgrib_hdr;
*           Grib Header structure whose storage is released;
*
*    RETURN CODE:  none;
****************************************************************************
*/
#if PROTOTYPE_NEEDED
void	free_gribhdr ( GRIB_HDR **ppgrib_hdr)
#else
void	free_gribhdr ( ppgrib_hdr)
			GRIB_HDR **ppgrib_hdr;
#endif
{
   char *func="free_gribhdr";
   DPRINT1 ("Entering %s\n", func);
/*
*
* B.1       IF (this struct is not null) {
*               IF (struct's entire_msg is not null)
*                   FREE entire msg array
*               ENDIF
*               FREE struct itself
*               SET it to null
*           ENDIF
*/
   if (*ppgrib_hdr != NULL) {
       	if ((*ppgrib_hdr)->entire_msg != NULL) free((*ppgrib_hdr)->entire_msg);
	free (*ppgrib_hdr);
	*ppgrib_hdr= NULL;
   }
   DPRINT1 ("Leaving %s, no return code\n", func);
/*
*
* END OF FUNCTION
*
*/
}

/*
***********************************************************************
* C. FUNCTION:  Expand_gribhdr
*      to make Grib Header structure 's entire_msg buffer larger
*      than its current abs_size.
*
*    INTERFACE:
*      int   Expand_gribhdr (gh, newsize, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*    (I&O)  GRIB_HDR *gh;
*           Grib Header structure whose buffer is to be expanded;
*      (I)  long newsize;
*           size to expand entire_msg to;
*      (O)  char *errmsg;
*           empty array, returned filled if error occurred;
*
*    RETURN CODE:  
*      0>  newsize is smaller or equal to current size and function
*          with return with GRIB header unchanged;  OR,
*          successful, entire_msg now is larger & abs_size has
*          been updated;  all of the section pointers are also 
*          updated to point to correct location within the new
*          larger block.
*      1>  error occurred, Errmsg filled;
****************************************************************************
*/
#if PROTOTYPE_NEEDED
int	Expand_gribhdr (GRIB_HDR *gh, long newsize, char *errmsg)
#else
int	Expand_gribhdr (gh, newsize, errmsg)
GRIB_HDR	*gh;
long		newsize;
char		*errmsg;
#endif
{
   char *func="Expand_gribhdr";
   unsigned char  *Buff;			/* temp array */

   DPRINT1 ("Entering %s\n", func);
/*
* C.0       IF (grib hdr struct pointer or entire_msg is null) 
*              RETURN with error
*           ENDIF
*/
   if (gh == (GRIB_HDR *)NULL || gh->entire_msg == (unsigned char *)NULL)  {
	sprintf(errmsg,"%s: either GRIB_HDR or Entire_msg is Null\n",
	func);
        DPRINT1 ("Leaving %s, with error (NULL Grib Header)\n", func);
	return (1);
   }

/*
* C.1       IF (new size is smaller than abs_size) THEN
*              PRINT warning
*              RETURN with no errors
*           ENDIF
*/
   if (newsize <= gh->abs_size) {
	fprintf(stdout,
	"%s:  cannot expand to %ld bytes (must be bigger than abs_size= %ld)\n",
	func, newsize, gh->abs_size);
	return (0);
   }

   DPRINT2 ("Require %ld bytes and curr abs_size= %ld\n", 
   newsize, gh->abs_size);

/*
* C.2       ALLOCATE a new block of 'newsize' bytes
*           RETURN on error
*/
   Buff =  (unsigned char *)malloc (newsize);
   if (Buff == NULL) {
	sprintf(errmsg,"%s: failed to create new array (%d bytes)\n",
	func, newsize);
        DPRINT1 ("Leaving %s, with Malloc error\n", func);
	return (1);
   } 

/*
* C.3       CLEAR new array out 
*/
   memset ((void*)Buff, '\0', newsize);

/*
* C.4       COPY content of old buffer into new buffer
*/
   if (gh->msg_length > 0) {
	DPRINT1(
	"Copy %ld bytes of data from old buffer to new one\n",
	gh->msg_length);

	memcpy ((void*)Buff, (void*)gh->entire_msg, gh->msg_length);
   }

/*
* C.6       UPDATE each Section that's present to point to
*           proper location within the new larger buffer
*/
   if (gh->ids_ptr !=NULL) gh->ids_ptr= Buff + (gh->ids_ptr - gh->entire_msg);
   if (gh->pds_ptr !=NULL) gh->pds_ptr= Buff + (gh->pds_ptr - gh->entire_msg);
   if (gh->gds_ptr !=NULL) gh->gds_ptr= Buff + (gh->gds_ptr - gh->entire_msg);
   if (gh->bms_ptr !=NULL) gh->bms_ptr= Buff + (gh->bms_ptr - gh->entire_msg);
   if (gh->bds_ptr !=NULL) gh->bds_ptr= Buff + (gh->bds_ptr - gh->entire_msg);
   if (gh->eds_ptr !=NULL) gh->eds_ptr= Buff + (gh->eds_ptr - gh->entire_msg);

/*
* C.5       FREE the old buffer & assign the new one to GRIB_HDR
*/
   free ((void *) gh->entire_msg);
   gh->entire_msg = (unsigned char *)Buff;
   
/*
* C.6       UPDATE alloc_size of GRIB_HDR
*/
   gh->abs_size = newsize;
   DPRINT1 ("expanded  gh->abs_size = %ld\n", gh->abs_size);

   DPRINT1 ("Leaving %s, no errors\n", func);
   return (0);
/*
* END OF FUNCTION
*/
}
