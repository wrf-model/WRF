#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

/*
******************************************************************************
* A.  FUNCTION:  gribputbds
*       Use the information provided to create a  Binary Data Section of
*       the GRIB format and store it in the GRIB_HDR structure;
*
*    INTERFACE:
*      int   gribputbds (user_input, lgrid_size, sDec_sc_fctr, pfData_Array,
*                         pBDS_Head_Input,  pgrib_hdr, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  USER_INPUT user_input;   
*           Structure containing encoder configuration data
*      (I)  long lgrid_size;         
*           number of datapoints expected for this projection
*      (I)  short sDec_sc_fctr;
*           Decimal Scle Factor used when packing up data 
*    (I&O)  float *pfData_Array;
*           float array to be packed up.  Returned scaled up by Dec Scale Fctr.
*      (O)  BDS_HEAD_INPUT *pBDS_Head_Input  
*           returned filled;
*    (I&O)  GRIB_HDR **pgrib_hdr;    
*           structure to hold encoded BDS and its info
*      (O)  char *errmsg
*           empty array, returned filled if error occurred;
*
*     RETURN CODE:
*      0> no errors; GRIB_HDR now has a valid Binary Data Section;
*         BDS_HEAD_INPUT filled also;
*      1> error occurred, errmsg filled;
*         either GRIB_HDR structure is corrupted, or
*         non-shuffle mode but the Data array is Null, or
*         failed to pack the Data array up, or 
*         failed to expand 'entire_msg' in GRIB_HDR to support encoded BDS;
******************************************************************************
*/
#if PROTOTYPE_NEEDED
int   gribputbds ( USER_INPUT user_input, long lgrid_size,
		short sDec_sc_fctr, float *pfData_Array,
		BDS_HEAD_INPUT *pBDS_Head_Input, GRIB_HDR **pgrib_hdr,
		char *errmsg)
#else
int   gribputbds ( user_input, lgrid_size, sDec_sc_fctr, pfData_Array,
		   pBDS_Head_Input, pgrib_hdr, errmsg)
 
		  USER_INPUT    user_input;             /* input */
		  long          lgrid_size;             /* input */
		  short                 sDec_sc_fctr;   /* input */
		  float                 *pfData_Array;  /* input */
		  BDS_HEAD_INPUT  *pBDS_Head_Input;     /* output */
		  GRIB_HDR      **pgrib_hdr;            /* input & output */
		  char          *errmsg;                /* output */
		 
#endif
{
/*
*
* A.0       DEFAULT to Error Stat
*/
char *func= "gribputbds";
long lBDS_length= 0;    /* Rnd2_len bytes */
void *pvbstr= 0;	/* remains null until after Inp2true_bds */
GRIB_HDR *gh;		/* working var */
long newsize;		/* working var */
void  create_inpBDS();
int  n, stat=1;

    DPRINT1 ("\nEntering %s() ...\n",func);
/*
*
* A.1       ASSIGN the GRIB_HDR pointer to local ptr;
*           IF (it's null OR entire_msg is null) THEN
*              RETURN error !errmsg filled
*           ENDIF
*/
    gh= *pgrib_hdr;
    if (!gh || !gh->entire_msg) {
	DPRINT1( "%s:  Grib Header or its Entire_msg is NULL\n", func);
	sprintf(errmsg,"%s:  Grib Header or its Entire_msg is NULL\n", func);
	goto BYE;
	}

/*
*
* A.2       IF (the floating point array is null) THEN
*/
    if (pfData_Array == NULL) {

/*
* A.2.1         IF (creating all sections mode) 
* A.2.1.a       THEN
*                  RETURN error      !cannot go on w/o float array
*/
 	  if  (! gh->shuffled) {
	       DPRINT1 ("%s:  Float array is Null, cannot proceed;\n",func);
	       sprintf(errmsg,
		"%s:  Float array is Null, cannot proceed;\n",func);
		goto BYE;
	       }
/*
* A.2.1.b       ELSE   /# Create all sections mode #/
*                  !bds must already exist & has non-zero length, else error;
*
*                  IF (bds is null or bdslen <=0) THEN
*                      RETURN error    !errmsg filled
*                  ELSE
*                      RETURN no error !bds already defined & has nonzero len
*                  ENDIF
*/
	  else {  /* create all mode */
		if (gh->bds_ptr== NULL || gh->bds_len<=0) 
		   {
	            DPRINT3 ( "%s:  No FloatData avail and GribHdr "\
		    "has no BDS yet (ptr=%ld len=%ld)\n"
		     ,func,gh->bds_ptr,gh->bds_len); 
	            sprintf(errmsg, 
		    "%s:  No FloatData avail and GribHdr has no BDS yet"\
		    "(ptr=%ld len=%ld)\n",func,gh->bds_ptr,gh->bds_len); 
		    }
		else { 
		    stat= 0;
	            DPRINT2 ("%s: No need to proceed, GribHdr already "\
		    "has a BDS  (len=%ld)\n", func, gh->bds_len);  
		   }
/*
* A.2.1         ENDIF
*/
	       } /* if */

/*
* A.2.2         RETURN with Stat   !not decoding anything
*/
	goto BYE;  /* quit */
/*
*
* A.2       ENDIF    !no float data
*/
      }  /* no flt data */


    DPRINT0 ("Need to pack Float Data & Store in (Char*);\n");

/*
*
* A.3         FILL the BDS Head Input struct;
*/
   pBDS_Head_Input->Bin_sc_fctr = 0;     /* INPUT NOT USED AT THIS TIME */
   pBDS_Head_Input->fReference = 0.0;    /* INPUT NOT USED AT THIS TIME */
   pBDS_Head_Input->usBit_pack_num = user_input.usBit_pack_num;
                                         /* #bits used for packing, 0=default*/
   pBDS_Head_Input->ulGrid_size = (unsigned long) lgrid_size;  /* Grid size */
   pBDS_Head_Input->fPack_null = 1e10;   /* Pack null value */

   DPRINT3 ("\t bds_head_input->usBit_pack_num = %u\n" \
           "\t bds_head_input->ulGrid_size = %u\n" \
          "\t bds_head_input->fPack_null = %f\n",
          pBDS_Head_Input->usBit_pack_num, pBDS_Head_Input->ulGrid_size,
          pBDS_Head_Input->fPack_null );
   
/*
*
* A.4       FUNCTION pack_spatial !packs data into binary bitstream
*           IF (error in pack grid routine)
*           THEN
*               RETURN with error !errmsg filled
*           ENDIF
*/
     if ((n= pack_spatial ( (long *)&(pBDS_Head_Input->ulGrid_size),
                    &(pBDS_Head_Input->usBit_pack_num),
                    &(pBDS_Head_Input->fPack_null),
                    pfData_Array, 
		    (unsigned long **) &pvbstr, 
		    sDec_sc_fctr, &lBDS_length, errmsg)) )
          {
          DPRINT2 ("%s:  Pack Spatial returned err=%d\n", func, n);
          upd_child_errmsg (func, errmsg);
          goto BYE;
          }

/*
*
* A.5       CALCULATE new message length including new BDS
*             (Include 4 bytes for EDS to avoid another realloc)
*/
     newsize=  gh->msg_length + lBDS_length + 4;

/* 
*
* A.6       IF gribhdr's buffer is too small AND
*               FUCTION Expand_gribhdr failed 
*           THEN
*               RETURN with error   !errmsg filled
*           ENDIF
*/
     if (newsize > gh->abs_size 
	&& Expand_gribhdr (gh, newsize, errmsg) !=0) 
	{
	upd_child_errmsg (func, errmsg);
	goto BYE;
	}

/*
*
* A.7       STORE bds & its info into Grib Hdr
*           !copy true BDS struct over into entire message array
*           !update message length also in Grib Hdr
*           !save length of bds into Internal Struct too
*/
    gh->bds_ptr= gh->entire_msg + gh->msg_length;
    memcpy ((void *) gh->bds_ptr, pvbstr, lBDS_length);
    gh->bds_len    = lBDS_length;
    gh->msg_length += gh->bds_len;

    /* Added by Todd Hutchinson, TASC  4/16/99*/
    /* This stops a memory leak */
    free(pvbstr);

    pBDS_Head_Input->length = lBDS_length;  /* update the Input struct too */

    DPRINT2("%s: copied %ld bytes from pvbstr to BDSPTR\n", func, lBDS_length);

/*
*
* A.8       CHANGE status to no errors
*/
    stat = 0;

BYE:
/*
*
* A.9       RETURN w/ Stat
*/
    DPRINT2 ("Leaving %s,  Stat=%d\n", func , stat);
    return (stat);
/*
*
* END OF FUNCTION
*
*
*/
}
