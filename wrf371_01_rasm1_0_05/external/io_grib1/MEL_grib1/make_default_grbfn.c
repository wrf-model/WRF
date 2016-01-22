#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"
#include "gribfuncs.h"		/* prototypes */
#include "grib_lookup.h"	/* macros */

/*
****************************************************************************
* A.  FUNCTION:   make_default_grbfn
*        build and return default filename for current message to be encoded
*        using the information from structures DATA_INPUT and USER_INPUT.
*
*    INTERFACE:
*      void  make_default_grbfn (DATA_INPUT di, USER_INPUT ui, char *default_fn)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) DATA_INPUT  di;     contains info of msg to be encoded
*      (I) USER_INPUT  ui;     contains the required chCase_id
*      (O) char *default_fn;   empty string atleast 42 characters long
*
*     RETURN CODE: none; default_fn string contains name with format
*       'Mid_Gid_yyyymmddhhtau_PIndx_Lid.lvl1.c.grb';
****************************************************************************
*/
#if PROTOTYPE_NEEDED
void    make_default_grbfn  (DATA_INPUT di, USER_INPUT ui, char *default_fn)
#else
void    make_default_grbfn  (di,ui,default_fn)
		DATA_INPUT di; 
                USER_INPUT ui; 
                char *default_fn;
#endif
{
/*
* A.1   Build the default filename:  MMM_GGG_yyyymmddhhtau_PIndx.lvl1.c.grb
*       where
*         MMM   : 3-dibit model id from DATA_INPUT
*         GGG   : 3-digit geom id from DATA_INPUT
*         yyyy  : 4-digit year of reference date/time from DATA_INPUT
*          mm   : 2-digit month of reference date/time from DATA_INPUT
*          dd   : 2-digit day of reference date/time from DATA_INPUT
*          hh   : 2-digit hour of reference date/time from DATA_INPUT
*         tau   : 3-digit forecast period  from DATA_INPUT
*       PIndx   : 4-digit Parameter Index computed from DATA_INPUT's 
*                 Parmid & ParmSubid
*         Lid   : 3-digit Level id from DATA_INPUT
*        lvl1   : 5-digit Level 1  from DATA_INPUT
*           c   : 1-digit Case id from USER_INPUT
*        .grb   : 4-char string, as is
*/
  sprintf (default_fn, 
        "%03d_%03d_%04d%02d%02d%02d%03d_%04d_%03d.%05d.%c.grb",
        di.usProc_id, di.usGrid_id, di.nYear, di.nMonth, di.nDay, 
        di.nHour, di.usFcst_per1, 
	(int)PARMTBL_INDX (di.usParm_id, di.usParm_sub_id),
	di.usLevel_id, di.nLvl_1, 
        ui.chCase_id);

  DPRINT1("make_default_grb_fn built '%s'\n", default_fn);
}

