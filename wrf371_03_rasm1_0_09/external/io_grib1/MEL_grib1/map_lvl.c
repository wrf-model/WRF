#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"	  /* for debug printing  */
#include "grib_lookup.h"  /* LVL_DEFN */
#include "gribfuncs.h"		/* prototypes */

extern LVL_DEFN    db_lvl_tbl[NLEV];     /* defined in ld_dec_lookup.c */

/*
************************************************************************
* A. FUNCTION:  map_lvl
*       Map the given Level_type to its appropriate usLevelid, scale up the
*       Level_1 and Level_2 to GRIB unit and also return the Scale Factor, 
*       Reference.
*
*    INTERFACE:
*       int map_lvl (lvl_type, data_input, lvl_scl_fctr, lvl_reference, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)   char *lvl_type;
*            name of Level to look for in the array of Level structures;
*      (I&O) DATA_INPUT *data_input;
*            structure holding data pertaining to current message required by
*            the encoder;  Three of its attributes get filled (usLevel_id, 
*            nLvl_1, nLvl_2);
*      (O)   float *lvl_scl_fctr, float *lvl_reference;
*            numbers needed to scale the Level up to GRIB unit.
*            multiply the level value by the Scale Factor, then add to the
*            Reference to convert to GRIB unit;
*      (O)   char *errmsg;
*            empty array, returned filled if error occurred;
*
*      RETURN CODE:
*        0: success, DATA_INPUT filled, fbuff may have changed;
*        1: parameter not found, errmsg filled; 
************************************************************************
*/
#if PROTOTYPE_NEEDED
int   map_lvl ( char		*lvl_type,
		DATA_INPUT  	*data_input,
		float		*lvl_scl_fctr,
		float		*lvl_reference,
		char		*errmsg)
#else
int   map_lvl ( lvl_type, data_input, lvl_scl_fctr, lvl_reference,errmsg)
		char		*lvl_type;
		DATA_INPUT  	*data_input;
		float		*lvl_scl_fctr;
		float		*lvl_reference;
		char		*errmsg;
#endif
{
char	*func= "map_lvl";
int	indx= 0;		/* index for array */
int	found = 0;		/* set if located level  */
LVL_DEFN   *PL;			/* working var */

 DPRINT1 ("Entering %s\n", func);
/*
* A.1       SEARCH the Level info table for the given Level Type
*/
   for (PL=db_lvl_tbl; indx < NLEV ; PL=(++indx +db_lvl_tbl)) 
	if (PL->db_name[0] && !strcmp (PL->db_name, lvl_type))  {
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
	DPRINT1 ("No '%s' in db_lvl_tbl;\n", lvl_type);
	sprintf (errmsg, "%s:  no '%s' in db_lvl_tbl;", func, lvl_type);
	return (1); 
	}
/*
*
* A.3       SCALE up nLvl_1 and nLvl_2 to GRIB's unit
*/
   data_input->nLvl_1 = (int)(data_input->nLvl_1 * PL->fScale + PL->fOffset);
   data_input->nLvl_2 = (int)(data_input->nLvl_2 * PL->fScale + PL->fOffset);
   
/*
*
* A.4       FILL in Level_id DATA_INPUT struct
*           FILL in caller's Scale factor & Reference
*/
   data_input->usLevel_id       = PL->usLevel_id;
   *lvl_scl_fctr	 	= PL->fScale; 
   *lvl_reference 		= PL->fOffset;

/*
*
* A.5       RETURN with no errors
*/
   DPRINT6 (
    "Found '%s'\nfill Data_Input->usLevel_id=%d; *lvl_scl=%lf, *lvl_ref=%lf\n"\
    "Scaled up Data_Input->nLvl_1= %d\nScaled up Data_Input->Lvl_2= %d\n",
    lvl_type,
    data_input->usLevel_id , *lvl_scl_fctr ,*lvl_reference,
    data_input->nLvl_1, data_input->nLvl_2);
  
   DPRINT1 ("Exiting %s with no errors\n", func);
   return (0);
}
