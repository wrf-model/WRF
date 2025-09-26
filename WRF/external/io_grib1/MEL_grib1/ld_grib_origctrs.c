#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grib_lookup.h"
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */

CTR_DEFN 	db_ctr_tbl[NCTRS];   /* GLOBVARS */

/*
***************************************************************************
* A. FUNCTION:  ld_grib_origctrs
*      Load Originating Centers information from named file into
*      an array of structures of type CTR_DEFN.  
*
*    INTERFACE:
*      int     ld_grib_origctrs (orig_ctr_fn, pathnm,  errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) char	*orig_ctr_fn;    name of file to load
*      (I) char *pathnm;         path where input file resides
*      (O) char *errmsg;         returned filled if error occurred
*
*    RETURN CODE:
*        0: no errors; db_ctrs_tbl array filled;
*       -1: bad, errmsg is filled;
***************************************************************************
*/
#if PROTOTYPE_NEEDED
int  ld_grib_origctrs ( char *orig_ctr_fn, char *pathnm, char *errmsg)
#else
int  ld_grib_origctrs ( orig_ctr_fn, pathnm, errmsg)
		char *orig_ctr_fn; char *pathnm; char *errmsg;
#endif
{
/*
*
* A.0       DEFAULT to bad Status;
*/
char *func="ld_grib_origctrs", *ptr;
char  strGribCode[200],Line[200], fn[200], mybuff[200];
int ftp_already=0, usGribCode, cnt=0, stat= -1;
FILE  *fLook;

/*
*
* A.1       PREPARE name and path of orig ctr file
*           CLEAR out the lookup arrays
*/
  DPRINT1 ("Entering %s\n", func);
  sprintf (fn, "%s/%s", pathnm, orig_ctr_fn);
  DPRINT1 ("Try to load= %s\n", fn);
  memset ((void*)db_ctr_tbl, '\0', NCTRS * sizeof(CTR_DEFN));

/*
*
* A.2       IF (unable to open OrigCtr file for reading) THEN
*               RETURN with error status;
*           ENDIF
*/
   if ( (fLook= fopen(fn,"r")) == NULL) 
     {
	 sprintf(errmsg,"%s: failed to load '%s';\n", func, orig_ctr_fn);
	 goto BYE;	/* return with error status */
     }


      /*  Now, read:  *** ORIG_CTRS ****
      Sample:

     GRIB Table 0 - Originating Center Definitions (Octet 5 of PDS)
     Code Figure    Model Name
     ===========    ==========
     007            US Weather Service - National Meteorological Center (NMC)
     057            US Air Force - Air Force Global Weather Central
     058            Fleet Numerical Meteorology and Oceanography Center (FNMOC)
     059            NOAA Forecast Systems Laboratory (FSL)
     097            European Space Agency (ESA)
     098            European Centre for Medium Range Weather Forecasts (ECMWF)
     128            Naval Research Laboratory (NRL) Monterey, CA
     129            Center for Air/Sea Technology (CAST)
     */

/*
*
* A.3       SKIP over the comments lines 
*/
     /* Read until last of Header line */
     for (Line[0]='\0'; ! strstr(Line,"====") ; ) 
     {
       fgets(Line, sizeof(Line), fLook); 
       if (feof(fLook) || ferror(fLook)) 
         { sprintf(errmsg,
	   "%s: got EOF/ERROR skipping over Header lines in %s\n", func,fn);
	   goto BYE;
         }
      }

/*
* A.4       WHILE (not end of file yet)
*/
    cnt=0;
    while (!feof(fLook) && !ferror(fLook))
      {
/*
*             READ a line  !stop if fails
*             SKIP line if it doesn't have 2 args or ctr_id out of range
*             STORE center info into db_ctr_tbl array, cell #usGribCode;
*/
		if (fgets(Line, sizeof(Line), fLook) == NULL) break;

	       /* skip additional comments, 
		replace tabs with spaces, newlines with null
	       */
		if (Line[0]=='#') continue;
		while (ptr=strchr(Line,'\t')) *ptr=' ';
		if (ptr=strchr(Line,'\n')) *ptr='\0';

                if (sscanf(Line,"%s%s",strGribCode, mybuff) != 2) continue;

	       /* Make sure Ctr_Id field has a Number */
	       if (strspn (strGribCode, "0123456789") != strlen(strGribCode)) {
		   sprintf(errmsg,"%s:  Invalid Ctr_id '%s', LINE=\n%s\n",
		   func,strGribCode, Line);
		   goto BYE; }

	       	usGribCode = (unsigned short) atoi(strGribCode);
		if (usGribCode<0 || usGribCode>= NOCTR) continue;

                /* copy over to Neon Tbl, descr has more than 1 words */
                strncpy(db_ctr_tbl[usGribCode].ctr_dsc, 
			strstr(Line, mybuff), 
			sizeof(db_ctr_tbl[usGribCode].ctr_dsc) -1);
                ++cnt;
	        DPRINT2 ("(+)  ctr_id=%d, descr=%s\n", usGribCode, 
		db_ctr_tbl[usGribCode].ctr_dsc); 
/*
*           ENDWHILE    !read entries
*/
      } 

/*
*
* A.5       SET Status to no errors
*/
    DPRINT1 ("File 'orig_ctrs' has %d entries\n", cnt);
    stat = 0;

BYE:
/*
*
* A.6       CLOSE "orig_ctrs" if file is opened
*
* A.7       RETURN with status
*/
    if (fLook)  close(fLook);
    DPRINT2 ("Exiting %s, Stat=%d\n", func, stat);
    return (stat);
/*
*
* END OF FUNCTION
*
*/
}
