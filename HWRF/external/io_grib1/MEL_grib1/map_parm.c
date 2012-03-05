#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"	  /* for debug printing  */
#include "grib_lookup.h"  /* PARM_DEFN */
#include "gribfuncs.h"		/* prototypes */

/* DB_PARM_TBL is defined in ld_enc_inputs.c:
#  as of 4/9/97 this tbl is the master Parameter Table holding
#  MAX_PARM_TBLS  sets of 256 parameters (previously known as 
#  Tbl 0/A/B/C/D/E).
#  Index that are divisable by 256 are reserved and not used;
#    indices   000-255:  Main parameter tbl (000 is reserved & not used)
#    indices   256-511:  subtable A (256 is reserved & not used)
#    indices   512-767:  subtable B (512 is reserved & not used)
#    indices  768-1023:  subtable C (768 is reserved & not used)
#    indices 1024-1279:  subtable D (1024 is reserved & not used)
#    indices 1080-1535:  subtable E (1080 is reserved & not used)
*/
extern PARM_DEFN    db_parm_tbl[];

/*
************************************************************************
* A. FUNCTION:  map_parm
*      Map the given Parm_name to its appropriate usParm_id and usParm_sub
*      within the Parameter Lookup table, and also return its
*      Scale Factor and Reference which the caller can apply to the
*      float dta at a later time.
*
*    INTERFACE
*      int  map_parm (parm_name, data_input, parm_scl, parm_ref, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *parm_name
*           Name of Parameter to look for in the array of Parameter structs
*      (I&O) DATA_INPUT *data_input
*           attributes (usParm_id, usParm_sub_id, nDec_sc_fctr) are filled;
*      (O)  float *parm_scl       
*           used along with parm_ref to convert data to GRIB unit
*      (O)  float *parm_ref       
*           used along with parm_scl to convert data to GRIB unit
*                      
*    RETURN CODE:
*      0> success, DATA_INPUT, parm_scl and parm_ref filled 
*      1> parameter not found, errmsg filled; 
************************************************************************
*/
#if PROTOTYPE_NEEDED
int     map_parm ( char		*parm_name,
		DATA_INPUT  	*data_input,
		float		*parm_scl, 
		float		*parm_ref,
		char		*errmsg)

#else
int     map_parm ( parm_name, data_input, parm_scl, parm_ref, errmsg)
		char		*parm_name;
		DATA_INPUT  	*data_input;
		float		*parm_scl;
		float		*parm_ref;
		char		*errmsg;
#endif
{
char	*func= "map_parm";
int	indx= 0;		/* index for array */
int	found = 0;		/* set if located parm */
PARM_DEFN   *P;			/* working var */

  DPRINT1 ("Entering %s\n", func);

/*
* A.1       SEARCH the Parameter info table for the given Parm Name
*/
   for (P=db_parm_tbl; indx < NPARM*MAX_PARM_TBLS; P=(++indx +db_parm_tbl)) 
	if (P->db_name[0] && !strcmp (P->db_name, parm_name))  {
		found=1; break; 
	   }
/*
*
* A.2       IF (cannot find it) THEN
*              FILL errmsg with message
*              RETURN 1  ! bad status
*           ENDIF
*/
   if (!found) { 
	DPRINT1 ("No '%s' in db_parm_tbl;\n", parm_name);
	sprintf (errmsg, "%s: no '%s' in db_parm_tbl", func, parm_name);
        DPRINT1 ("Exiting  %s, with errors\n", func);
	return (1); 
	}
/*
*
* A.3       FILL in Parmid, subParmid, nDec_sc_fctr of DATA_INPUT struct
*           FILL in Parm_scl and Parm_ref for caller
*/
   data_input->usParm_id      = P->usParm_id;
   data_input->usParm_sub_id  = P->usParm_sub;
   data_input->nDec_sc_fctr   = P->sDSF;
   *parm_scl		= P->fScale;
   *parm_ref		= P->fOffset;

   DPRINT4 (
   "Found '%s'\nfill Data_Input->Parm_id=%d; \nfill Data_Input->Parm_sub=%d;"\
   "\nfill Data_Input->DSF=%d\n",
   parm_name, data_input->usParm_id, data_input->usParm_sub_id,
   data_input->nDec_sc_fctr);

/*
#*
* A.4       /# comment #/
#* A.4       IF (there is a scl fctr  OR nonzero Offset) THEN
#*               APPLY Scale Fctr and Offset to Float data
#*               for all data points in grid 
#*           ENDIF
##/
#  ... where... data_pts was passed in as an argument...
#
#   if (P->fScale != 1.0 || P->fOffset != 0.0) {
#	DPRINT3 ("Scaling FloatArr[%d pts] w/ Scale=%lf, Off=%lf\n",
#	data_pts,  P->fScale , P->fOffset);
#
#      	for (indx=0; indx < data_pts; ++indx)
#		fbuff[indx]=  fbuff[indx] * P->fScale + P->fOffset;
#	}
#   else { DPRINT2("Not scaling float dta  (Scl=%lf, Offs=%lf)\n",
#   	 P->fScale , P->fOffset); 
#	}
*/
   
/*
*
* A.5       RETURN with no errors
*/
   DPRINT1 ("Exiting %s with no errors\n", func);
   return (0);
/*
*
* END OF FUNCTION
*/
}
