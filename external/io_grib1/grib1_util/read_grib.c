 /**************************************************************************
 * Todd Hutchinson          4/20/98
 * tahutchinson@tasc.com    (781) 942-2000 x3108
 * TASC 
 * 55 Walkers Brook Drive
 * Reading, MA  01867
 *
 * Functions in this file are used for decoding grib data.  Please see the
 * headers before each function for a full descrption.
 *
 * Routines in this file call functions in the Naval Research Lab's grib 
 * library.  The grib library is freely available from 
 * http://www-mel.nrlmry.navy.mil/cgi-bin/order_grib.  This library should
 * be installed on your system prior to using the routines in this file.
 * Documentation for this library is available from
 *  Master Environmental Grib Library user's manual
 *  http://mel.dmso.mil/docs/grib.pdf
 * Note: the standard NRL grib library does not support 
 * "Little-Endian" platforms such as linux.  There is a version of the NRL 
 * grib library within the WxPredictor project which does support linux.
 *
 * This file references the cfortran.h header file to ease the use of calling 
 * this function from a fortran routine.  cfortran.h is a header file that 
 * allows for simple machine-independent calls between c and fortran.  The 
 * package is available via anonymous ftp at zebra.desy.de.
 *
 * The grib document "A GUIDE TO THE CODE FORM FM 92-IX Ext. GRIB" may be
 * useful to your understanding of this code.  This document is available 
 * via anonymous ftp from nic.fb4.noaa.gov.  Check the readme file in the
 * root directory for further instructions.
 *
 ****************************************************************************/

#define ERRSIZE 2000
#define ALLOCSIZE 30
#define MISSING -999

#define EARTH_RADIUS 6371.229        /* in km */
#define PI           3.141592654
#define PI_OVER_180  PI/180.

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "cfortran.h"
#include "gribfuncs.h"
#include "gribsize.incl"
#include "read_grib.h"

/* Function Declarations */

void remove_element(int array[],int index, int size);
int advance_time(int *century, int year, int month, int day, int hour,
		 int amount, int unit);
char *advance_time_str(char startdatein[], int amount, char enddate[]);
int date_diff(int date1,int century1,int date2,int century2);
int hours_since_1900(int date,int century);
int isLeapYear(int year);
int get_factor2(int unit);
int compare_record(GribInfo *gribinfo, FindGrib *findgrib, int gribnum);

/* 
 *These lines allow fortran routines to call the c routines.  They are
 * used by macros defined in cfortran.h
 */
#define get_pressure_levels_STRV_A1 TERM_CHARS(' ',1)
/*
FCALLSCFUN6(INT, get_pressure_levels,GET_PRESSURE_LEVELS,
	    get_pressure_levels,STRINGV,INTV,INTV,INTV,INT,INT)
#define setup_gribinfo_STRV_A1 TERM_CHARS(' ',1)
FCALLSCFUN2(INT,setup_gribinfo,SETUP_GRIBINFO,setup_gribinfo,STRINGV,INT)
#define get_msl_indices_STRV_A1 TERM_CHARS(' ',1)
FCALLSCFUN9(INT, get_msl_indices,GET_MSL_INDICES,get_msl_indices,
	    STRINGV,INTV,INTV,INTV,INTV,INTV,INT,INTV,INTV)
FCALLSCFUN5(INT, get_index,GET_INDEX,get_index,INT,INT,INT,INT,INT)
#define read_grib_STRV_A1 TERM_CHARS(' ',1)
FCALLSCFUN7(INT,get_dates,GET_DATES,get_dates,INTV,INTV,INTV,INT,INTV,
	    INTV,INTV)
FCALLSCFUN7(INT, read_grib,READ_GRIB,read_grib,
	    STRINGV,INT,INT,INT,INT,FLOATVV,PVOID)
FCALLSCFUN8(INT, get_index_near_date,GET_INDEX_NEAR_DATE,get_index_near_date,
            STRING,INT,INT,INT,INTV,INTV,INTV,INT)
*/
/* The value for usLevel_id for isobaric levels */
#define ISOBARIC_LEVEL_ID 100  

/*************************************************************************
 * This function reads and decodes grib records in a list of input files 
 * and stores information about each grib record in the gribinfo array 
 * structure.  The gribinfo structure can then be accessed by any function 
 * within this file.
 * 
 * Interface:
 *   Input:
 *      gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *      files - a string array containing the names of the files containing
 *              the grib data.  If called from a fortran routine, the
 *              fortran routine must set the character size of the array to
 *              be STRINGSIZE-1. The last filled element in the array should
 *              be "END".
 *      use_fcst - if TRUE, forecast fields will be included in the gribinfo
 *              structure, otherwise, only analysis fields will be included.
 *
 *    Return:
 *       1  - successful call to setup_gribinfo
 *       -1 - call to setup_gribinfo failed
 *
 ***************************************************************************/

int rg_setup_gribinfo(GribInfo *gribinfo, char files[][STRINGSIZE],
		      int use_fcst)
{
  FILE *fp;
  int filenum;
  int nReturn;
  int idx;
  int status;
  int start_elem;
  
  /* Loop through input files */
  filenum = 0;
  while ((strcmp(files[filenum], "end") != 0 ) && 
	 (strcmp(files[filenum], "END") != 0 )) {
    
    /* 
     * This forces gribinfo to be fully initialized.
     */
    if (filenum == 0) 
      {
	gribinfo->num_elements = 0;
      }

    start_elem = gribinfo->num_elements;

    fp = fopen(files[filenum],"r");
    if (fp == NULL)
      {
	fprintf(stderr,"Could not open %s\n",files[filenum]);
	nReturn = -1;
	break;
      }
    
    status = rg_setup_gribinfo_f(gribinfo, fp, use_fcst);
    if (status != 1) 
      {
	fprintf(stderr, 
	    "rg_setup_gribinfo_f returned non-zero status (%d), skipping %s\n",
		status,files[filenum]);
	continue;
      }

    for (idx=start_elem; idx < gribinfo->num_elements; idx++) 
      {
	strcpy(gribinfo->elements[idx].filename,
	       files[filenum]);
      }

    
    filenum++;
    nReturn = 1;
  }

  return nReturn;
}



/*************************************************************************
 *
 * Similar to rg_setup_gribinfo, except, a unix file descriptor is passed in, 
 * rather than a list of files to open.
 *
 *************************************************************************/

int rg_setup_gribinfo_i(GribInfo *gribinfo, int fid, int use_fcst)
{
  FILE *fp;
  int status;
  
  fp = fdopen(fid,"r");
  if (fp == NULL)
    {
      fprintf(stderr,"Could not open file descriptor %d\n",fid);
      status = -1;
      return status;
    }
  
  /* This forces gribinfo to be initialized for the first time */
  gribinfo->num_elements = 0;
  
  status = rg_setup_gribinfo_f(gribinfo, fp, use_fcst);
  if (status != 1) 
    {
      fprintf(stderr, 
	      "rg_setup_gribinfo_f returned non-zero status (%d)\n",
	      status);
    }
  
  return status;
}

/*************************************************************************
 *
 * Similar to rg_setup_gribinfo, except, a file pointer is passed in, rather
 * than a list of files to open.
 * 
 * If gribinfo->num_elements is 0, gribinfo is initialized, otherwise, 
 *    gribinfo is appended to.
 *
 *************************************************************************/

int rg_setup_gribinfo_f(GribInfo *gribinfo, FILE *fp, int use_fcst)
{
  char errmsg[ERRSIZE];
  int nReturn=0;
  long offset;
  int filenum;
  int Rd_Indexfile=0;
  GRIB_HDR *gh1;
  long tmpoffset=0;
  int century;
  int year4d;
  int fcsttime1=0;
  int fcsttime2=0;
  int factor=0;

  /* Set the number of elements to be zero initially */
  if (gribinfo->num_elements <= 0)
    {
      /* Allocate space for gribinfo */
      gribinfo->elements = (Elements *)calloc(ALLOCSIZE,sizeof(Elements));
      if (gribinfo->elements == NULL) {
	sprintf(errmsg,"Could not allocate %d bytes for gribinfo->elements\n",
		ALLOCSIZE*sizeof(Elements));
	goto bail_out;
      }
    }
  
  /* Make storage for Grib Header */
  nReturn = init_gribhdr(&gh1, errmsg);
  /* 
   * The grib library is setup such that, when init_gribhdr == 0, it was 
   * successful.  If it is 1, it failed.
   */
  if (nReturn == 1) goto bail_out;
  
  /* Scan through message */
  for (offset = 0L; nReturn == 0; offset += gh1->msg_length) {
    if ((gribinfo->num_elements > 0) && 
	(gribinfo->num_elements%ALLOCSIZE == 0))
      gribinfo->elements = 
	(Elements *)realloc(gribinfo->elements,
			    (gribinfo->num_elements+ALLOCSIZE)*
			    sizeof(Elements));

    if (gribinfo->elements == NULL) {
      sprintf(errmsg,"Could not allocate %d bytes for gribinfo\n",
	      (gribinfo->num_elements + ALLOCSIZE)*sizeof(Elements));
      goto bail_out;
    }

    /* Setup the File pointer */
    gribinfo->elements[gribinfo->num_elements].fp = fp;

    gribinfo->elements[gribinfo->num_elements].pds = 
      (PDS_INPUT *)malloc(1*sizeof(PDS_INPUT));
    gribinfo->elements[gribinfo->num_elements].gds = 
      (grid_desc_sec *)malloc(1*sizeof(grid_desc_sec));
    gribinfo->elements[gribinfo->num_elements].bms = 
      (BMS_INPUT *)malloc(1*sizeof(BMS_INPUT));
    gribinfo->elements[gribinfo->num_elements].bds_head = 
      (BDS_HEAD_INPUT *)malloc(1*sizeof(BDS_HEAD_INPUT));
    errmsg[0] = '\0';
    nReturn = 
      grib_fseek(fp,&offset, Rd_Indexfile, gh1, errmsg);
    if (nReturn != 0) {
      if (nReturn == 2) break;   /* End of file error */
      else {
	fprintf(stderr, "Grib_fseek returned non zero status (%d)\n",
		nReturn);
	goto bail_out;
      }
    }
    if (errmsg[0] != '\0')
      { /* NO errors but got a Warning msg from seek */
	fprintf(stderr,"%s; Skip Decoding...\n",errmsg);
	errmsg[0] = '\0';   
	gh1->msg_length = 1L;    /* set to 1 to bump offset up */
	continue;
      }
      
    if (gh1->msg_length < 0) {
      fprintf(stderr, "Error:  message returned had bad length (%ld)\n",
	      gh1->msg_length);
      goto bail_out;
    }
    else if (gh1->msg_length == 0) {
      fprintf(stderr, "msg_length is Zero\n");
      gh1->msg_length = 1L;
      continue;
    }
    init_dec_struct(gribinfo->elements[gribinfo->num_elements].pds, 
		    gribinfo->elements[gribinfo->num_elements].gds, 
		    gribinfo->elements[gribinfo->num_elements].bms, 
		    gribinfo->elements[gribinfo->num_elements].bds_head);
    
    /* 
     * gribgetpds is an undocumented function within the grib library.
     * gribgetpds grabs the pds section from the grib message without
     * decoding the entire grib message.  The interface is as follows:
     *     first input param: a pointer to the beginning of the pds 
     *                        section.
     *     second input param: a pointer to a structure which will hold
     *                        the pds information
     *     third param: the error message.
     * 
     * If gribgetpds ever fails, it can be replaced with the following 
     *    nReturn = grib_dec((char *)gh1->entire_msg, &pds, &gds, &bds_head, 
     *              &bms, &grib_data, errmsg);
     * 
     * This will degrade performance since this grib_dec decodes the 
     *    entire grib message.
     */
    
    nReturn = gribgetpds((char*)(gh1->entire_msg + 8),
			 gribinfo->elements[gribinfo->num_elements].pds,
			 errmsg);
    if (nReturn != 0) goto bail_out;

    /* Get gds if present */
    if (gribinfo->elements[gribinfo->num_elements].pds->usGds_bms_id >> 7 
	& 1) {
      nReturn = 
	gribgetgds((char*)
		   (gh1->entire_msg+8+
		    gribinfo->elements[gribinfo->num_elements].pds->uslength),
		   gribinfo->elements[gribinfo->num_elements].gds,errmsg);
      if (nReturn != 0) goto bail_out;
    }

    /* Get bms section if present */
    if (gribinfo->elements[gribinfo->num_elements].pds->usGds_bms_id >> 6 
	& 1) {
      /*
	fprintf(stderr,"grids with bms section not currently supported\n");
	return -1;
      */
    }
    
    gribinfo->elements[gribinfo->num_elements].usGrid_id = 
      gribinfo->elements[gribinfo->num_elements].pds->usGrid_id;
    gribinfo->elements[gribinfo->num_elements].usParm_id = 
      gribinfo->elements[gribinfo->num_elements].pds->usParm_id;
    gribinfo->elements[gribinfo->num_elements].usLevel_id = 
      gribinfo->elements[gribinfo->num_elements].pds->usLevel_id;
    gribinfo->elements[gribinfo->num_elements].usHeight1 = 
      gribinfo->elements[gribinfo->num_elements].pds->usHeight1;
    gribinfo->elements[gribinfo->num_elements].usHeight2 = 
      gribinfo->elements[gribinfo->num_elements].pds->usHeight2;
    gribinfo->elements[gribinfo->num_elements].center_id = 
      gribinfo->elements[gribinfo->num_elements].pds->usCenter_id;
    gribinfo->elements[gribinfo->num_elements].parmtbl = 
      gribinfo->elements[gribinfo->num_elements].pds->usParm_tbl;
    gribinfo->elements[gribinfo->num_elements].proc_id = 
      gribinfo->elements[gribinfo->num_elements].pds->usProc_id;
    gribinfo->elements[gribinfo->num_elements].subcenter_id = 
      gribinfo->elements[gribinfo->num_elements].pds->usCenter_sub;
    gribinfo->elements[gribinfo->num_elements].offset = offset;
    gribinfo->elements[gribinfo->num_elements].end = 
      offset + gh1->msg_length - 1;
  
    if (use_fcst) {
      century = gribinfo->elements[gribinfo->num_elements].pds->usCentury;
      
      if (gribinfo->elements[gribinfo->num_elements].pds->usTime_range == 10)
	{
	  fcsttime1 = gribinfo->elements[gribinfo->num_elements].pds->usP1*256 + 
	    gribinfo->elements[gribinfo->num_elements].pds->usP2;
	  fcsttime2 = 0;
	}
      else if (gribinfo->elements[gribinfo->num_elements].pds->usTime_range 
	       == 203) {
	/* This is the WSI extension to grib.  203 indicates "duration" */
	fcsttime1 = gribinfo->elements[gribinfo->num_elements].pds->usP1;
	fcsttime2 = gribinfo->elements[gribinfo->num_elements].pds->usP1 +
	  gribinfo->elements[gribinfo->num_elements].pds->usP2;
      } else {
	fcsttime1 = gribinfo->elements[gribinfo->num_elements].pds->usP1;
	fcsttime2 = gribinfo->elements[gribinfo->num_elements].pds->usP2;
      }
      
      gribinfo->elements[gribinfo->num_elements].date = 
	advance_time(&century,
		     gribinfo->elements[gribinfo->num_elements].pds->usYear,
		     gribinfo->elements[gribinfo->num_elements].pds->usMonth,
		     gribinfo->elements[gribinfo->num_elements].pds->usDay,
		     gribinfo->elements[gribinfo->num_elements].pds->usHour,
		     fcsttime1,
		     gribinfo->elements[gribinfo->num_elements].pds->usFcst_unit_id);
    } 
    else {
      gribinfo->elements[gribinfo->num_elements].date = 
	gribinfo->elements[gribinfo->num_elements].pds->usHour*1 + 
	gribinfo->elements[gribinfo->num_elements].pds->usDay*100 + 
	gribinfo->elements[gribinfo->num_elements].pds->usMonth*10000 + 
	gribinfo->elements[gribinfo->num_elements].pds->usYear*1000000;
    } 
    gribinfo->elements[gribinfo->num_elements].century = 
      gribinfo->elements[gribinfo->num_elements].pds->usCentury;
    
    year4d = 
	(gribinfo->elements[gribinfo->num_elements].pds->usCentury - 1) * 100
	+ gribinfo->elements[gribinfo->num_elements].pds->usYear;

    sprintf(gribinfo->elements[gribinfo->num_elements].initdate,
	    "%04d%02d%02d%02d%02d%02d",
	    year4d,
	    gribinfo->elements[gribinfo->num_elements].pds->usMonth,
	    gribinfo->elements[gribinfo->num_elements].pds->usDay,
	    gribinfo->elements[gribinfo->num_elements].pds->usHour,
	    gribinfo->elements[gribinfo->num_elements].pds->usMinute,
	    0);	      
    
    factor = 
      get_factor2(gribinfo->elements[gribinfo->num_elements].pds->usFcst_unit_id);
    gribinfo->elements[gribinfo->num_elements].fcsttime1 = 
      fcsttime1 * factor;
    gribinfo->elements[gribinfo->num_elements].fcsttime2 = 
      fcsttime2 * factor;
    
    advance_time_str(gribinfo->elements[gribinfo->num_elements].initdate,
		     gribinfo->elements[gribinfo->num_elements].fcsttime1,
		     gribinfo->elements[gribinfo->num_elements].valid_time);

    gribinfo->num_elements++;
  }
  
  free_gribhdr(&gh1);
  return 1;
  
  /* The error condition */
 bail_out:
  if (errmsg[0] != '\0') fprintf(stderr,"\n***ERROR: %s: %s\n",
				 "setup_grib",errmsg);
  if (gribinfo->elements != NULL) free(gribinfo->elements);
  perror("System Error ");
  return -1;
}

/*****************************************************************************
 *
 * Retrieve pressure levels from grib data.  This function will pass the 
 * pressure levels for which the input parameter is available at all input 
 * times back to the calling routine.  
 *
 * Interface
 *      Input: 
 *         gribinfo - pointer to a previously allocated gribinfo structure.  
 *                 The gribinfo structure is filled in this function.
 *         dates: an array of dates to check for data
 *                format: yymmddhh
 *                If called from a fortran routine, the fortran routine must 
 *                set the character size of the array to be STRINGSIZE-1
 *         centuries: an array holding the centuries for each of the 
 *                dates in the array dates.
 *         parm_id: the input parameter id.  From table 2 of the grib manual.
 *      Output:
 *         finallevels: an array of pressure levels which are contained in 
 *                the grib data at all input times.
 *      Return:
 *         the number of levels in the levels array.  The levels are listing
 *         in descending (by value) order, i.e., the value with the highest
 *         pressure (lowest vertical level) is the first element.
 *
 ****************************************************************************/

int rg_get_pressure_levels(GribInfo *gribinfo, int dates[], int centuries[], 
			int parm_id[], int finallevels[],int min_pres,
			int numparms)
{
  int datenum;
  int gribnum;
  int *levelnum;
  int levelincluded;
  int i,j;
  int contains_level;
  int **tmplevels;
  int numfinallevels = 0;
  int parmnum;
  int tmpval;
  
  /* Allocate space */
  levelnum = (int *)calloc(numparms,sizeof(int));
  tmplevels = (int **)calloc(numparms,sizeof(int *));
  for (j = 0; j < numparms; j++) {
    tmplevels[j] = (int *)calloc(1000,sizeof(int));
    if (tmplevels[j] == NULL) {
      tmplevels = NULL;
      break;
    }
  }
  if ((levelnum == NULL) || (tmplevels == NULL)) {
    fprintf(stderr,
	    "get_pressure_levels: Allocation of space failed, returning\n");
    return -1;
  }
  
  /* Loop through all parameters */
  for (parmnum = 0; parmnum < numparms; parmnum++) {

    levelnum[parmnum] = 0;

  /* Get the array of pressure levels available at the first input time */
    datenum = 0;
    for (gribnum = 0; gribnum < gribinfo->num_elements; gribnum++) {
      if (gribinfo->elements[gribnum].date == dates[datenum]) {
	if (gribinfo->elements[gribnum].century == centuries[datenum]) {
	  if (gribinfo->elements[gribnum].usLevel_id == ISOBARIC_LEVEL_ID) {
	    if (gribinfo->elements[gribnum].usParm_id == parm_id[parmnum]) {
	      if (gribinfo->elements[gribnum].usHeight1 >= min_pres) {
		levelincluded = 0;
		for (j=0; j < levelnum[parmnum]; j++) {
		  if (tmplevels[parmnum][j] == 
		      gribinfo->elements[gribnum].usHeight1) {
		    levelincluded = 1;
		    break;
		  }
		}
		if (levelincluded == 0) {
		  tmplevels[parmnum][levelnum[parmnum]] = 
		    gribinfo->elements[gribnum].usHeight1;
		  levelnum[parmnum]++;
		}
	      }
	    }
	  }
	}
      }
    }
    
    /* Remove levels that are not contained at all subsequent times */
    datenum++;
    while (dates[datenum] != -99){
      for (j = 0; j < levelnum[parmnum]; j++) {
	contains_level = 0;
	for (gribnum = 0; gribnum < gribinfo->num_elements; gribnum++) {
	  if (gribinfo->elements[gribnum].date == dates[datenum]) {
	    if (gribinfo->elements[gribnum].century == centuries[datenum]) {
	      if (gribinfo->elements[gribnum].usLevel_id == ISOBARIC_LEVEL_ID)
		{
		  if (gribinfo->elements[gribnum].usParm_id == 
		      parm_id[parmnum]) {
		    if (tmplevels[parmnum][j] == 
			gribinfo->elements[gribnum].usHeight1)
		      contains_level = 1;
		  }
		}
	    }
	  }
	}
	if (!(contains_level)) {
	  remove_element(tmplevels[parmnum],j,levelnum[parmnum]);
	  levelnum[parmnum]--;
	  j--;
	}
      }
      datenum++;
    }

    /* 
     * Put the values for levels into an array.  Remove any levels that
     * were not found at all other levels
     */
    if (parmnum == 0) {
      for (j = 0; j < levelnum[parmnum]; j++) {
	finallevels[j] = tmplevels[parmnum][j];
	numfinallevels++;
      }
    } else {
      for (i=0; i<numfinallevels; i++) {
	contains_level = 0;
	for (j=0; j<levelnum[parmnum]; j++) {
	  if (finallevels[i] == tmplevels[parmnum][j]) {
	    contains_level = 1;
	    break;
	  }
	}
	if (!contains_level) {
	  remove_element(finallevels,i,numfinallevels);
	  numfinallevels--;
	  i--;
	}
      }
    }

  }

  /* 
   * Sort the numfinallevels array into descending order. Use straight 
   * insertion.
   */
  for (j=1; j<numfinallevels; j++) {
    tmpval = finallevels[j];
    for (i=j-1; i >= 0; i--) {
      if (finallevels[i] >= tmpval) break;
      finallevels[i+1] = finallevels[i];
    }
    finallevels[i+1] = tmpval;
  }

  return numfinallevels;
}

/****************************************************************************
 *
 * Returns an array of grib indices that correspond to particular grib fields
 * to use as sea level pressure.  There will be exactly one element for each
 * input time.  If a field was not found, then this function returns NULL
 *
 * Interface:
 *    Input:
 *      gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *         dates: a string array of dates to check for data.
 *                format: yymmddhh
 *                If called from a fortran routine, the fortran routine must 
 *                set the character size of the array to be STRINGSIZE-1
 *         centuries: an array holding the centuries for each of the 
 *                dates in the array dates.
 *         usParm_id: an array of parameter identifiers that could be
 *                    used as a sea level pressure field (From table 2 of 
 *                    grib documentation)
 *         usLevel_id: the level id that could be used as a sea level pressure
 *                    field (from table 3 of the grib documentation)
 *         usHeight1: the height for the particular parameter and level
 *                    (in units described by the parameter index)
 *         numparms:  the number of parameters in each of the usParm_id, 
 *                    usLevel_id, and usHeight1 arrays.
 *    Output:
 *         grib_index: an array of grib indices to use for the sea level 
 *                    pressure.  The index to grib_index corresponds to 
 *                    the time, i.e., the first array element of grib_index 
 *                    corresponds to the first time, the second element to 
 *                    the second time, etc.
 *         
 *            Note: Values in the input arrays, usParm_id, usLevel_id, and 
 *                    usHeight with the same array index must correspond.
 *
 *    Return:
 *          1 for success
 *          -1 if no field was found.
 ***************************************************************************/

int rg_get_msl_indices(GribInfo *gribinfo, char dates[][STRINGSIZE],
		    int centuries[], int usParm_id[],int usLevel_id[],
		    int usHeight1[],int infactor[],int numparms,
		    int grib_index[],int outfactor[])
{
  int parmindex;
  int datenum = 0;
  int gribnum;
  int foundfield=0;

  for (parmindex = 0; parmindex < numparms; parmindex++) {

    datenum = 0;
    while ((strcmp(dates[datenum], "end") != 0 ) && 
	   (strcmp(dates[datenum], "END") != 0 )) {

      for (gribnum = 0; gribnum < gribinfo->num_elements; gribnum++) {
	if (gribinfo->elements[gribnum].date == atoi(dates[datenum])) {
	  if (gribinfo->elements[gribnum].century == centuries[datenum]) {
	    if ((gribinfo->elements[gribnum].usParm_id == 
		 usParm_id[parmindex]) &&
		(gribinfo->elements[gribnum].usLevel_id == 
		 usLevel_id[parmindex]) &&
		(gribinfo->elements[gribnum].usHeight1 == 
		 usHeight1[parmindex])) {
	      grib_index[datenum] = gribnum;
	      outfactor[datenum] = infactor[parmindex];
	      foundfield++;
	      break;
	    }
	  }
	}
      }

      datenum++;

      /* 
       * Break out of loop and continue on to next parameter if the current 
       * parameter was missing from a date. 
       */

      if (foundfield != datenum) break;
    }

/* 
 * Break out of the parameter loop once we've found a field available at all 
 * dates 
 */
    if (foundfield == datenum) {
      break;
    }

  }

  if (foundfield == datenum)
    return 1;
  else
    return -1;

}


/***************************************************************************
 * 
 * This function takes an index as input and returns a 2d grib data field
 *
 * Interface:
 *    input: 
 *      gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *       index - the index of gribinfo for which data is to be retrieved
 *       scale - the scale factor to multiply data by, i.e., if -2, 
 *               data will be multiplied by 10^-2.
 *    output:
 *       grib_out - the 2 dimensional output grib data
 *               Warning: This 2d array is setup with i being the vertical
 *               dimension and j being the horizontal dimension.  This
 *               is the convention used in mesoscale numerical modeling
 *               (the MM5 in particular), so it is used here.
 *    return:
 *       1 for success
 *      -1 for failure
 ***************************************************************************/

int rg_get_grib(GribInfo *gribinfo, int index,int scale,
	     float **grib_out,int *vect_comp_flag, 
	     GRIB_PROJECTION_INFO_DEF *Proj, BDS_HEAD_INPUT *bds_head)
{
  char errmsg[ERRSIZE];
  int nReturn=0;
  long offset;
  int Rd_Indexfile=0;
  BMS_INPUT bms;
  PDS_INPUT pds;
  BDS_HEAD_INPUT dummy;
  grid_desc_sec gds;
  GRIB_HDR *gh1;
  int i,j;
  int expandlon = 0;
  float *grib_data;

  /* Initialize Variables */
  errmsg[0] = '\0';
  offset = 0L;
  grib_data = (float *)NULL;

  /* Make storage for Grib Header */
  nReturn = init_gribhdr (&gh1, errmsg);
  if (nReturn == 1) goto bail_out;
  
  /* Seek to the position in the grib data */
  offset = gribinfo->elements[index].offset;
  nReturn = grib_fseek(gribinfo->elements[index].fp,&offset,
		      Rd_Indexfile,gh1,errmsg);
  if (nReturn != 1) {
    fprintf(stderr,"Grib_fseek returned error status (%d)\n",nReturn);
    goto bail_out;
  }
  if (errmsg[0] != '\0')
    { /* NO errors but got a Warning msg from seek */ 
      fprintf(stderr,"%s: Skip Decoding...\n",errmsg);
      errmsg[0] = '\0';
    }
  if (gh1->msg_length <= 0) {
    fprintf(stderr,"Error: message returned had bad length (%ld)\n",
	    gh1->msg_length);
    goto bail_out;
  }
  init_dec_struct(&pds, &gds, &bms, &dummy);
 
  nReturn = grib_dec((char *)gh1->entire_msg, &pds, &gds, 
		     bds_head,
		     &bms, &grib_data, errmsg);
 
  if (nReturn != 0) goto bail_out;
  
  if (bms.uslength > 0) {
    nReturn = apply_bitmap(&bms, &grib_data, FILL_VALUE, bds_head, 
			   errmsg);
    if (nReturn != 0) goto bail_out;
  }
 
  switch(gds.head.usData_type) {
  case 0:
  case 4:
    strcpy(Proj->prjnmm,"latlon");
    Proj->colcnt = gds.llg.usNi;
    Proj->rowcnt = gds.llg.usNj;
    Proj->origlat = gds.llg.lLat1/1000.;
    Proj->origlon = gds.llg.lLon1/1000.;
    Proj->xintdis = (gds.llg.iDi/1000.)*EARTH_RADIUS*PI_OVER_180;
    Proj->yintdis = (gds.llg.iDj/1000.)*EARTH_RADIUS*PI_OVER_180;
    Proj->parm1 = 0.;
    Proj->parm2 = 0.;
    if ((gds.llg.usRes_flag >> 3) & 1) *vect_comp_flag = 1;
    else *vect_comp_flag = 0;

    /* If the grid is a global grid, we want to set the expandlon flag
     * so that the number of columns in the array is expanded by one and
     * the first column of data is copied to the last column.  This
     * allows calling routines to interpolate between first and last columns
     * of data.
     */

    if (gds.llg.usNi*gds.llg.iDi/1000. == 360)
      expandlon = 1;
    else
      expandlon = 0;
     
    break;
  case 1: 
    strcpy(Proj->prjnmm,"mercator");
    Proj->colcnt = gds.merc.cols;
    Proj->rowcnt = gds.merc.rows;
    Proj->origlat = gds.merc.first_lat/1000.;
    Proj->origlon = gds.merc.first_lon/1000.;
    Proj->xintdis = gds.merc.lon_inc/1000.;
    Proj->yintdis = gds.merc.lat_inc/1000.;
    Proj->parm1 = gds.merc.latin/1000.;
    Proj->parm2 = (gds.merc.Lo2/1000. - Proj->origlon)/gds.merc.cols;
    if ((gds.merc.usRes_flag >> 3) & 1) *vect_comp_flag = 1;
    else *vect_comp_flag = 0;
    break;
  case 3:
    strcpy(Proj->prjnmm,"lambert");
    Proj->colcnt = gds.lam.iNx;
    Proj->rowcnt = gds.lam.iNy;
    Proj->origlat = gds.lam.lLat1/1000.;
    Proj->origlon = gds.lam.lLon1/1000.;
    Proj->xintdis = gds.lam.ulDx/1000.;
    Proj->yintdis = gds.lam.ulDy/1000.;
    Proj->parm1 = gds.lam.lLat_cut1/1000.;
    Proj->parm2 = gds.lam.lLat_cut2/1000.;
    Proj->parm3 = gds.lam.lLon_orient/1000.;
    if ((gds.lam.usRes_flag >> 3) & 1) *vect_comp_flag = 1;
    else *vect_comp_flag = 0;
    break;
  case 5:
    strcpy(Proj->prjnmm,"polar_stereo");
    Proj->colcnt = gds.pol.usNx;
    Proj->rowcnt = gds.pol.usNy;
    Proj->origlat = gds.pol.lLat1/1000.;
    Proj->origlon = gds.pol.lLon1/1000.;
    Proj->xintdis = gds.pol.ulDx/1000.;
    Proj->yintdis = gds.pol.ulDy/1000.;
    Proj->parm1 = 60.;
    Proj->parm2 = gds.pol.lLon_orient/1000.;   
    if ((gds.pol.usRes_flag >> 3) & 1) *vect_comp_flag = 1;
    else *vect_comp_flag = 0;
    break;
  default:
    fprintf(stderr,"Grid not supported, gds.head.usData_type = %d\n",
	    gds.head.usData_type);
    fprintf(stderr,"Exiting\n");
    exit(-1);
    break;
  }  

  strcpy(Proj->stordsc,"+y_in_+x");
  Proj->origx = 1;
  Proj->origy = 1;

  for (j=0; j< (Proj->rowcnt); j++) {
    for (i=0; i<(Proj->colcnt); i++) {
      grib_out[j][i] = grib_data[i+j*Proj->colcnt]*pow(10,scale);
    }
  }

  if (expandlon) {
    (Proj->colcnt)++;
    for (j = 0; j < Proj->rowcnt; j++) {
      grib_out[j][Proj->colcnt-1] = grib_out[j][0];
    }
  }

  /* 
   * You only reach here when there is no error, so return successfully.
   */
      
  nReturn = 0;

  if (grib_data != NULL) {
    free_gribhdr(&gh1);
    free(grib_data);
  } 

  return 1;
      
  /* The error condition */
 bail_out:
  if (errmsg[0] != '\0') fprintf(stderr,"\n***ERROR: %s %s\n",
				 "get_grib",errmsg);
  if (grib_data != NULL)
    free(grib_data);
  free_gribhdr(&gh1);
  return -1;
}

/***************************************************************************
 * 
 * This function takes an index as input and returns a 2d grib data field
 *
 * Interface:
 *    input: 
 *      gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *       index - the index of gribinfo for which data is to be retrieved
 *    output:
 *       data - the 2 dimensional output grib data
 *               Warning: This 2d array is setup with i being the vertical
 *               dimension and j being the horizontal dimension.  This
 *               is the convention used in mesoscale numerical modeling
 *               (the MM5 in particular), so it is used here.
 *    return:
 *       1 for success
 *      -1 for failure
 ***************************************************************************/

int rg_get_data(GribInfo *gribinfo, int index, float **data)
{
  float *data_1d;
  int i,j;
  int numrows,numcols;
  int status;

  numrows = rg_get_numrows(gribinfo,index);
  numcols = rg_get_numcols(gribinfo,index);
  
  data_1d = (float *)calloc(numrows*numcols,sizeof(float));
  if (data_1d == 0)
    {
      fprintf(stderr,"Allocating space for data_1d failed, index: %d\n",index);
      return -1;
    }

  status = rg_get_data_1d(gribinfo, index, data_1d);
  if (status != 1)
    {
      return status;
    }

  for (j=0; j< numrows; j++) {
    for (i=0; i < numcols; i++) {
      data[j][i] = data_1d[i+j*numcols];
    }
  }

  free(data_1d);

  return 1;
  
}

/***************************************************************************
 * 
 * This function takes an index as input and returns a 1d grib data field
 *
 * Interface:
 *    input: 
 *      gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *       index - the index of gribinfo for which data is to be retrieved
 *    output:
 *       data - 1 dimensional output grib data
 *               Warning: This 2d array is setup with i being the vertical
 *               dimension and j being the horizontal dimension.  This
 *               is the convention used in mesoscale numerical modeling
 *               (the MM5 in particular), so it is used here.
 *    return:
 *       1 for success
 *      -1 for failure
 ***************************************************************************/

int rg_get_data_1d(GribInfo *gribinfo, int index, float *data)
{
  char errmsg[ERRSIZE];
  int nReturn=0;
  long offset;
  int Rd_Indexfile=0;
  BMS_INPUT bms;
  PDS_INPUT pds;
  BDS_HEAD_INPUT bds_head;
  grid_desc_sec gds;
  GRIB_HDR *gh1;
  int i,j;
  int numcols, numrows;
  float *grib_data;

  /* Initialize Variables */
  errmsg[0] = '\0';
  offset = 0L;
  grib_data = (float *)NULL;

  /* Make storage for Grib Header */
  nReturn = init_gribhdr (&gh1, errmsg);
  if (nReturn == 1) goto bail_out;
  
  /* Seek to the position in the grib data */
  offset = gribinfo->elements[index].offset;
  nReturn = grib_fseek(gribinfo->elements[index].fp,&offset,
		      Rd_Indexfile,gh1,errmsg);
  if (nReturn != 0) {
    fprintf(stderr,"Grib_fseek returned non-zero status (%d)\n",nReturn);
    goto bail_out;
  }
  if (errmsg[0] != '\0')
    { /* NO errors but got a Warning msg from seek */ 
      fprintf(stderr,"%s: Skip Decoding...\n",errmsg);
      errmsg[0] = '\0';
    }
  if (gh1->msg_length <= 0) {
    fprintf(stderr,"Error: message returned had bad length (%ld)\n",
	    gh1->msg_length);
    goto bail_out;
  }

  init_dec_struct(&pds, &gds, &bms, &bds_head);
 
  nReturn = grib_dec((char *)gh1->entire_msg, &pds, &gds, 
		     &bds_head,
		     &bms, &grib_data, errmsg);
 
  if (nReturn != 0) goto bail_out;
  
  if (bms.uslength > 0) {
    nReturn = apply_bitmap(&bms, &grib_data, FILL_VALUE, &bds_head, 
			   errmsg);
    if (nReturn != 0) goto bail_out;
  }
 
  /*
   * Copy the data into the permanent array
   */ 
  numcols = rg_get_numcols(gribinfo,index);
  numrows = rg_get_numrows(gribinfo,index);
  memcpy(data,grib_data,numcols*numrows*sizeof(float));

  /* 
   * You only reach here when there is no error, so return successfully.
   */
      
  nReturn = 0;

  if (grib_data != NULL) {
    free_gribhdr(&gh1);
    free(grib_data);
  } 

  return 1;
      
  /* The error condition */
 bail_out:
  if (errmsg[0] != '\0') fprintf(stderr,"\n***ERROR: %s %s\n",
				 "get_grib",errmsg);
  if (grib_data != NULL)
    free(grib_data);
  free_gribhdr(&gh1);
  return -1;
}

/****************************************************************************
 * Returns the index of gribinfo corresponding to the input date, level, 
 * height, and parameter.
 *
 * Interface:
 *   Input: 
 *      gribinfo  - pointer to a previously populated gribinfo structure.  
 *      initdate  -  initialization date in the form yyyymmdd[HHMMSS].  If any
 *                  part of HHMMSS is not specified, it will be set to 0.
 *      parmid    - the parameter id in the grib file
 *      leveltype - the leveltype id from table 3/3a of the grib document.
 *      level1    - First level of the data in units described by leveltype.
 *      level2    - Second level of the data in units described by leveltype.
 *      fcsttime1 - First forecast time in seconds.
 *      fcsttime2 - Second forecast time in seconds.
 *    Note: If an input variable is set set to -INT_MAX, then any value
 *                will be considered a match.
 *    Return:
 *       if >= 0    The index of the gribinfo data that corresponds to the
 *                  input parameters
 *       if < 0     No field corresponding to the input parms was found.
 *
 ***************************************************************************/

int rg_get_index(GribInfo *gribinfo, FindGrib *findgrib)
{
  int gribnum;
  int grib_index=-1;

  for (gribnum = 0; gribnum < gribinfo->num_elements; gribnum++) {
    if (compare_record(gribinfo, findgrib, gribnum) == 1)
      {
	grib_index = gribnum;
	break;
      }
  }
  return grib_index;
}


/****************************************************************************
 * Same as rg_get_index, except that a guess for the record number is given.
 *   This "guess" record is first checked to see if it matches, if so, 
 *      that grib record number is just returned.  If it does not match, 
 *      full searching ensues.
 * Returns the index of gribinfo corresponding to the input date, level, 
 * height, and parameter.
 *
 * Interface:
 *   Input: 
 *      Same is rg_get_index, except:
 *      guess_index  - The index to check first.
 *    Return:
 *      Same as rg_get_index
 *
 ***************************************************************************/

int rg_get_index_guess(GribInfo *gribinfo, FindGrib *findgrib, int guess_index)
{
  int retval;

  if (compare_record(gribinfo, findgrib, guess_index) == 1) {
    retval = guess_index;
  } else {
    retval = rg_get_index(gribinfo, findgrib);
  }

  return retval;
}


/****************************************************************************
 * Sets all values in FindGrib to missing.
 *
 * Interface:
 *   Input: 
 *      findgrib  - pointer to a previously allocated findgrib structure.
 *
 *    Return:
 *       1  for success.
 *       -1 for failure.
 *
 ***************************************************************************/
int rg_init_findgrib(FindGrib *findgrib)
{
  strcpy(findgrib->initdate,"*");
  strcpy(findgrib->validdate,"*");
  findgrib->parmid          = -INT_MAX;
  findgrib->parmid          = -INT_MAX;
  findgrib->leveltype       = -INT_MAX;
  findgrib->level1          = -INT_MAX;
  findgrib->level2          = -INT_MAX;
  findgrib->fcsttime1       = -INT_MAX;
  findgrib->fcsttime2       = -INT_MAX;
  findgrib->center_id       = -INT_MAX;
  findgrib->subcenter_id    = -INT_MAX;
  findgrib->parmtbl_version = -INT_MAX;
  
  return 1;
}

/****************************************************************************
 * Returns the indices of all gribinfo entries that match the input date, 
 * level, height, and parameter.
 *
 * Interface:
 *   Input: 
 *      gribinfo  - pointer to a previously populated gribinfo structure.  
 *      initdate  -  initialization date in the form yyyymmdd[HHMMSS].  If any
 *                  part of HHMMSS is not specified, it will be set to 0.
 *      parmid    - the parameter id in the grib file
 *      leveltype - the leveltype id from table 3/3a of the grib document.
 *      level1    - First level of the data in units described by leveltype.
 *      level2    - Second level of the data in units described by leveltype.
 *      fcsttime1 - First forecast time in seconds.
 *      fcsttime2 - Second forecast time in seconds.
 *      indices   - an array of indices that match
 *      num_indices - the number of matches and output indices
 * 
 *    Note: If an input variable is set set to -INT_MAX, then any value
 *                will be considered a match.
 *    Return:
 *          The number of matching indices.
 *
 ***************************************************************************/

int rg_get_indices(GribInfo *gribinfo, FindGrib *findgrib, int indices[])
{
  int gribnum;
  int matchnum = 0;

  for (gribnum = 0; gribnum < gribinfo->num_elements; gribnum++) {
    if (compare_record(gribinfo, findgrib, gribnum) == 1) {
      indices[matchnum] = gribnum;
      matchnum++;
    }    
  }
  return matchnum;
}

/*************************************************************************
 *
 * Returns an array of dates that correspond to particular input grib fields.
 * The dates will be sorted so that the dates increase as the index increases.
 *
 * Interface:
 *    Input:
 *         gribinfo - pointer to a previously allocated gribinfo structure.  
 *                    The gribinfo structure is filled in this function.
 *         usParm_id: an array of parameter identifiers that could be
 *                    used as a sea level pressure field (From table 2 of 
 *                    grib documentation)
 *         usLevel_id: the level id that could be used as a sea level pressure
 *                    field (from table 3 of the grib documentation)
 *         usHeight1: the height for the particular parameter and level
 *                    (in units described by the parameter index)
 *         numparms:  the number of parameters in each of the usParm_id, 
 *                    usLevel_id, and usHeight1 arrays.
 *    Output:
 *         dates:     the dates for which the input fields are available.
 *         
 *            Note: Values in the input arrays, usParm_id, usLevel_id, and 
 *                    usHeight with the same array index must correspond.
 *
 *    Return:
 *          The number of dates found.
 *************************************************************************/

int rg_get_dates(GribInfo *gribinfo,int usParm_id[],int usLevel_id[],
	      int usHeight1[],int numparms,int dates[],int century[],
	      int indices[])
{
  int datenum=0;
  int gribnum;
  int parmindex;
  int already_included;
  int i,j;
  int tmpval,tmpval2,tmpval3;

  /* Get the dates for the given parameters */

  for (parmindex = 0; parmindex < numparms; parmindex++) {
    for (gribnum = 0; gribnum < gribinfo->num_elements; gribnum++) {
      if ((gribinfo->elements[gribnum].usParm_id == usParm_id[parmindex]) &&
	  (gribinfo->elements[gribnum].usLevel_id == usLevel_id[parmindex]) &&
	  (gribinfo->elements[gribnum].usHeight1 == usHeight1[parmindex])) {
	already_included = 0;
	for (i = 0; i < datenum; i++){
	  if ((dates[datenum] == gribinfo->elements[gribnum].date) && 
	      (century[datenum] == gribinfo->elements[gribnum].century)) {
	    already_included = 1;
	    break;
	  }
	}
	if (!already_included) {
	  dates[datenum] = gribinfo->elements[gribnum].date;
	  century[datenum] = gribinfo->elements[gribnum].century;
	  indices[datenum] = gribnum;
	  datenum++;
	}
      }
    }
  }

  /* Sort the dates into increasing order */
  for (j = 1; j < datenum; j++) {
    tmpval = dates[j];
    tmpval2 = indices[j];
    tmpval3 = century[j];
    for (i=j-1; i >= 0; i--) {
      if (dates[i] <= tmpval) break;
      dates[i+1] = dates[i];
      indices[i+1] = indices[i];
      century[i+1] = century[i];
    }
    dates[i+1] = tmpval;
    indices[i+1] = tmpval2;
    century[i+1] = tmpval3;
  }

  return datenum;
}

/****************************************************************************
 * This function returns the pds, gds, bms, and bms_head section of the
 * grib element
 *
 * Input:
 *   gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *              gribinfo structure is filled in this function.
 *   index - the index of the grib record to access as indexed by 
 *           setup_gribinfo
 *   
 * Output:
 *   *pds - a pointer to a structure holding the pds information
 *   *gds - a pointer to a structure holding the gds information
 *   *bms - a pointer to a structure holding the bms information
 *   *bds_head - a pointer to a structure holding the binary data section
 *             header information
 *
 ***************************************************************************
 */
int rg_get_grib_header(GribInfo *gribinfo, int index, PDS_INPUT *pds, 
		    grid_desc_sec *gds,BMS_INPUT *bms)
{
  int xsize,ysize,j;
  
  memcpy(pds,gribinfo->elements[index].pds,sizeof(PDS_INPUT));
  memcpy(gds,gribinfo->elements[index].gds,sizeof(grid_desc_sec));
  memcpy(bms,gribinfo->elements[index].bms,sizeof(BMS_INPUT));
  
  /* Reset the dimensions for thinned grids */
  if (gribinfo->elements[index].gds->head.thin != NULL) {
    if (gds->head.thin != NULL) {
      if ((gds->head.usData_type == LATLON_PRJ) || 
	  (gds->head.usData_type == GAUSS_PRJ) ||
	  (gds->head.usData_type == ROT_LATLON_PRJ) ||
	  (gds->head.usData_type == ROT_GAUSS_PRJ) ||
	  (gds->head.usData_type == STR_LATLON_PRJ) ||
	  (gds->head.usData_type == STR_GAUSS_PRJ) ||
	  (gds->head.usData_type == STR_ROT_LATLON_PRJ) ||
	  (gds->head.usData_type == STR_ROT_GAUSS_PRJ)) {
	ysize = gds->llg.usNj;
      } else if (gds->head.usData_type == MERC_PRJ) {
	ysize = gds->merc.rows;
      } else if (gds->head.usData_type == POLAR_PRJ) {
	ysize = gds->pol.usNy;
      } else if ((gds->head.usData_type == LAMB_PRJ) ||
		 (gds->head.usData_type == ALBERS_PRJ) ||
		 (gds->head.usData_type == OBLIQ_LAMB_PRJ)) {
	ysize = gds->lam.iNy;
      }

      xsize = 0;
      for (j = 0; j<ysize; j++) {
	if (gds->head.thin[j] > xsize) {
	  xsize = gds->head.thin[j];
	}
      }
      

      if ((gds->head.usData_type == LATLON_PRJ) || 
	  (gds->head.usData_type == GAUSS_PRJ) ||
	  (gds->head.usData_type == ROT_LATLON_PRJ) ||
	  (gds->head.usData_type == ROT_GAUSS_PRJ) ||
	  (gds->head.usData_type == STR_LATLON_PRJ) ||
	  (gds->head.usData_type == STR_GAUSS_PRJ) ||
	  (gds->head.usData_type == STR_ROT_LATLON_PRJ) ||
	  (gds->head.usData_type == STR_ROT_GAUSS_PRJ)) {
	gds->llg.usNi = xsize;
	gds->llg.iDi = abs(gds->llg.lLat2 - gds->llg.lLat1)/(xsize-1);
      } else if (gds->head.usData_type == MERC_PRJ) {
	gds->merc.cols = xsize;
      } else if (gds->head.usData_type == POLAR_PRJ) {
	gds->pol.usNx = xsize;
      } else if ((gds->head.usData_type == LAMB_PRJ) ||
		 (gds->head.usData_type == ALBERS_PRJ) ||
		 (gds->head.usData_type == OBLIQ_LAMB_PRJ)) {
	gds->lam.iNx = xsize;
      }
      
    }
  }
  return 1;
}

/****************************************************************************
 * This returns the index of the gribdata for paramaters which match the input
 * parameters and for the date closest to the input targetdate.  If dates are
 * not found either within hours_before or hours_after the time, then a missing
 * value is returned.
 *
 * Interface:
 *   Input:
 *     gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                  gribinfo structure is filled in this function.
 *     targetdate:  This is the date which dates in the grib data will be 
 *                  compared to.  (format: integer yymmddhh)
 *     hours_before: The maximum difference in time prior to the targetdate
 *                  for which data should be searched for.
 *     hours_after: The maximum difference in time after the targetdate for
 *                  which data should be searched for.
 *     usParm_id:   an array of parameter identifiers that could be
 *                  used as a sea level pressure field (From table 2 of 
 *                  grib documentation)
 *     usLevel_id:  the level id that could be used as a sea level pressure
 *                  field (from table 3 of the grib documentation)
 *     usHeight1:   the height for the particular parameter and level
 *                  (in units described by the parameter index)
 *     numparms:    the number of parameters in each of the usParm_id, 
 *                  usLevel_id, and usHeight1 arrays.
 *   Return:
 *     the index of the gribdata with a time closest to the target date.
 *     -1 if there is no time within the input time limits.
 * 
 ****************************************************************************/
int rg_get_index_near_date(GribInfo *gribinfo,char targetdate[STRINGSIZE],
			int century,int hours_before,int hours_after,
			int usParm_id[],int usLevel_id[],int usHeight1[],
			int numparms)
{
  int dates[500],indices[500],centuries[500];
  int date_before = MISSING;
  int date_after = MISSING;
  int century_before,century_after;
  int date_diff_before = MISSING;
  int date_diff_after = MISSING;
  int index_before,index_after;
  int numdates,datenum;
  int index;
  int itargetdate;

  itargetdate = atoi(targetdate);

  numdates = rg_get_dates(gribinfo,usParm_id,usLevel_id,usHeight1,numparms,
			  dates,centuries,indices);
  if (numdates <= 0) {
    fprintf(stderr,"get_index_near_date: No dates were found\n");
    return -1;
  }
  
  for (datenum = 0; datenum < numdates; datenum++) {
    if ((dates[datenum] > itargetdate) && (centuries[datenum] >= century)) {
      century_after = centuries[datenum];
      date_after = dates[datenum];
      index_after = indices[datenum];
      break;
    } else {
      century_before = centuries[datenum];
      date_before = dates[datenum];
      index_before = indices[datenum];
    }
  }
  
  if (date_after != MISSING)
    date_diff_after = date_diff(date_after,century_after,itargetdate,century);
  if (date_before != MISSING)
    date_diff_before = 
      date_diff(itargetdate,century,date_before,century_before);

  if ((date_after != MISSING) && (date_before != MISSING)) {
    if ((date_diff_after <= hours_after) && 
	(date_diff_before <= hours_before)) {
      if (date_diff_after < date_diff_before)
	index = index_before;
      else
	index = index_after;
    } else if (date_diff_after <= hours_after) {
	index = index_after;
    } else if (date_diff_before <= hours_before) {
	index = index_before;
    } else {
      index = -1;
    }
  } else if (date_after != MISSING) {
    if (date_diff_after <= hours_after)
      index = index_after;
    else 
      index = -1;
  } else if (date_before != MISSING) {
    if (date_diff_before <= hours_before)
      index = index_before;
    else
      index = -1;
  } else {
    index = -1;
  }

  return index;
  
}

/*****************************************************************************
 *
 * returns valid time ( = init time + forecast time)
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *    index    - index number of record to get valid time from
 *
 * Output:
 *    valid_time - yyyymmddhhmmss
 *
 * Return:
 *    0 for success
 *   -1 for error
 *    
 *****************************************************************************/
int rg_get_valid_time(GribInfo *gribinfo, int index, char valid_time[])
{
  strcpy(valid_time, gribinfo->elements[index].valid_time);
  return 0;
}

/*****************************************************************************
 *
 * returns generating center id
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *    index    - index number of record to get valid time from
 *
 * Return:
 *    generating center id
 *   -1 for error
 *    
 *****************************************************************************/
int rg_get_center_id(GribInfo *gribinfo, int index)
{
  return gribinfo->elements[index].center_id;
}

/*****************************************************************************
 *
 * returns parameter table version number
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *    index    - index number of record to get valid time from
 *
 * Return:
 *    parameter table version number
 *   -1 for error
 *    
 *****************************************************************************/
int rg_get_parmtbl(GribInfo *gribinfo, int index)
{
  return gribinfo->elements[index].parmtbl;
}

/*****************************************************************************
 *
 * returns generating process id
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *    index    - index number of record to get valid time from
 *
 * Return:
 *    generating process id
 *   -1 for error
 *    
 *****************************************************************************/
int rg_get_proc_id(GribInfo *gribinfo, int index)
{
  return gribinfo->elements[index].proc_id;
}

/*****************************************************************************
 *
 * returns sub center id
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *    index    - index number of record to get valid time from
 *
 * Return:
 *    sub center id
 *   -1 for error
 *    
 *****************************************************************************/
int rg_get_subcenter_id(GribInfo *gribinfo, int index)
{
  return gribinfo->elements[index].subcenter_id;
}

/**************************************************************************
 *
 * Interpolates grib grid data to a point location.
 *
 * Interface:
 *     input: 
 *       gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                  gribinfo structure is filled in this function.
 *       index    - the index of gribinfo for which data is to be retrieved.
 *                  the first grib record is number 1.
 *       column   - the column of the point in grid coordinates (can be 
 *                  floating point number).  leftmost column is 1.
 *       row      - the row of the point in grid coordinates (can be 
 *                  floating point number).  bottommost row is 1.
 *
 *    return:
 *       on success - the interpolated value at the column,row location.
 *       on failure - -99999
 * 
 ***************************************************************************/

float rg_get_point(GribInfo *gribinfo, int index, float column, float row)
{
  int status;
  GRIB_PROJECTION_INFO_DEF Proj;
  BDS_HEAD_INPUT bds_head;
  int dummy;
  float **grib_out;
  float y1, y2;
  int numrows, numcols;
  int top, left, right, bottom;
  float outval;
  
  numrows = rg_get_numrows(gribinfo, index);
  numcols = rg_get_numcols(gribinfo, index);

  grib_out = (float **)alloc_float_2d(numrows,numcols);
  if (grib_out == NULL) {
    fprintf(stderr,"rg_get_point: Could not allocate space for grib_out\n");
    return -99999;
  }

  status = rg_get_data(gribinfo, index, grib_out);
  if (status < 0) {
    fprintf(stderr,"rg_get_point: rg_get_data failed\n");
    return -99999;
  }

  /* Do the interpolation here */
  bottom = floor(row);
  top = floor(row+1);
  left = floor(column);
  right = floor(column+1);
  
  y1 = (row - bottom) * (grib_out[top][left] - grib_out[bottom][left]) + 
    grib_out[bottom][left];
  y2 = (row - bottom) * (grib_out[top][right] - grib_out[bottom][right]) + 
    grib_out[bottom][right];
  outval = (y2 - y1) * (column - left) + y1;

  free_float_2d(grib_out,numrows,numcols);

  return outval;
  
}

/**************************************************************************
 *
 * Interpolates grib grid data to a point location.
 *
 * Interface:
 *     input: 
 *       gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                  gribinfo structure is filled in this function.
 *       index    - the index of gribinfo for which data is to be retrieved.
 *                  the first grib record is number 1.
 *     input and output:
 *       pointdata- array of pointdata structures.  Only the column and
 *                  row values in the structures need to be filled.  On
 *                  output, the 'value' member of pointdata is filled.
 *     input:
 *       numpoints- number of pointdata structures in the array.
 *
 *    return:
 *       on success - the interpolated value at the column,row location.
 *       on failure - -99999
 * 
 ***************************************************************************/
int rg_get_points(GribInfo *gribinfo, int index, PointData pointdata[], 
		   int numpoints)
{
  int status;
  float **grib_out;
  float y1, y2;
  int numrows, numcols;
  int top, left, right, bottom;
  float column, row;
  int idx;

  numrows = rg_get_numrows(gribinfo, index);
  numcols = rg_get_numcols(gribinfo, index);

  grib_out = (float **)alloc_float_2d(numrows,numcols);
  if (grib_out == NULL) {
    fprintf(stderr,"rg_get_points: Could not allocate space for grib_out\n");
    return -99999;
  }

  status = rg_get_data(gribinfo, index, grib_out);
  if (status < 0) {
    fprintf(stderr,"rg_get_points: rg_get_data failed\n");
    return -99999;
  }

  for (idx = 0; idx < numpoints; idx++) {

    /* Change from 1 based to 0 based col/row */
    row = pointdata[idx].row;
    column = pointdata[idx].column;
    
    /* Do the interpolation here */
    bottom = floor(row);
    top = floor(row+1);
    left = floor(column);
    right = floor(column+1);
    
    y1 = (row - bottom) * (grib_out[top][left] - grib_out[bottom][left]) + 
      grib_out[bottom][left];
    y2 = (row - bottom) * (grib_out[top][right] - grib_out[bottom][right]) + 
      grib_out[bottom][right];
    pointdata[idx].value = (y2 - y1) * (column - left) + y1;

  }

  free_float_2d(grib_out,numrows,numcols);

  return 1;
}

/**************************************************************************
 *
 * Remove an element from an array and decrease, by one, indices of all 
 * elements with an index greater than the index of the element to remove.
 *
 * Interface:
 *     input: 
 *        array - the integer array to manipulate
 *        index - the index of the element to remove
 *        size - the number of elements in the array
 *
 ***************************************************************************/
void remove_element(int array[],int index, int size)
{
  int j;
  
  for (j = index; j < size-1; j++) {
    array[j] = array[j+1];
  }

}

/****************************************************************************
 * Advance the time by the input amount
 *
 * Interface:
 *     Input: 
 *         century - an integer value for the century (20 for 1900's)
 *                   If the century is advanced, this value is advanced
 *                   and output to the calling routine.
 *         year - a 2 digit value for the year.
 *         month - a 2 digit value for the month.
 *         day - the day of the month
 *         hour - the hour of the day
 *         amount - the amount to advance the time by.
 *         unit - the units for the amount.  These are values from table 4
 *                of the grib manual.
 *    return:
 *         a date in the form yymmddhh
 ****************************************************************************/

int advance_time(int *century, int year, int month, int day, int hour,
		 int amount, int unit)
{
  int daysinmonth[] = {31,28,31,30,31,30,31,31,30,31,30,31};
  int date;

  switch(unit) {
  case 0:
    hour += (int)((amount/60.)+0.5);
    break;
  case 1:
    hour += amount;
    break;
  case 2:
    day += amount;
    break;
  case 3:
    month += amount;
    break;
  case 4:
    year += amount;
    break;
  case 5:
    year += 10*amount;
    break;
  case 6:
    year += 30*amount;
    break;
  case 7:
    year += 100*amount;
    break;
  case 10:
    hour += 3*amount;
    break;
  case 11:
    hour += 6*amount;
    break;
  case 12:
    hour += 12*amount;
    break;
  case 50:
    hour += (int)((amount/12.)+0.5);
  case 254:
    hour += (int)((amount/(60.*60.))+0.5);
    break;
  default:
    fprintf(stderr,"WARNING: Could not advance time, incorrect unit: %d\n",
	    unit);
    return -1;
  }

  while (hour >= 24) {
    day++;
    hour -= 24;
  }
  while (month > 12) {
    year++;
    month -= 12;
  }

  /* if it is a leap year, change days in month for Feb now. */ 
  if (isLeapYear(year)) daysinmonth[1] = 29;

  while (day > daysinmonth[month-1]) {
    day -= daysinmonth[month-1];
    month++;
    if (month > 12) {
      year++;
      month -= 12;
      if (isLeapYear(year))
	daysinmonth[1] = 29;
      else
	daysinmonth[1] = 28;
    }
  }

  if (year > 100) {
    (*century)++;
  }

  if (year >= 100) {
    year -= 100;
  }

  date = hour*1 + day*100 + month*10000 + year*1000000;

  return date;

}
/****************************************************************************
 * Advance the time by the input amount
 *
 * Interface:
 *     Input: 
 *         startdate  - initialization date in the form yyyymmdd[HHMMSS].  If any
 *                        part of HHMMSS is not specified, it will be set to 0.
 *         amount     - the amount (in seconds) to advance the time by.
 *
 *    Output:
 *         enddate[] - the time advanced to: yyyymmddHHMMSS format. 
 *
 *    Return:
 *          1 - success
 *         -1 - failure
 *
 ****************************************************************************/
char *advance_time_str(char startdatein[], int amount, char enddate[])
{
  struct tm starttp;
  struct tm endtp;
  char startdate[15];
  time_t time;

  strcpy(startdate,startdatein);
  while (strlen(startdate) < 14) {
    strcpy(startdate+(strlen(startdate)),"0");
  }

  /* This forces all calculations to use GMT time */
  putenv("TZ=GMT0");
  tzset();

  sscanf(startdate,"%4d%2d%2d%2d%2d%2d",&(starttp.tm_year),&(starttp.tm_mon),
	 &(starttp.tm_mday),&(starttp.tm_hour),&(starttp.tm_min),
	 &(starttp.tm_sec));
  starttp.tm_mon -= 1;
  starttp.tm_year -= 1900;
  time = mktime(&starttp);
  time += amount;
 #ifdef _WIN32
   localtime_s(&endtp, &time);
 #else
   localtime_r(&time, &endtp);
 #endif
  strftime(enddate,15,"%Y%m%d%H%M%S",&endtp);
  
  return enddate;
}

/****************************************************************************
 * Returns the difference in time in hours between date1 and date2 
 * (date1-date2). 
 * 
 * Interface:
 *   Input:
 *     date1,date2:  dates in yymmddhh format (integers)
 *     century1,century2: centuries for each date (20 for 1900's).
 *   Return:
 *     the difference in time between the first and second dates in hours.
 ****************************************************************************/
int date_diff(int date1,int century1,int date2,int century2)
{
  return (hours_since_1900(date1,century1) - 
	   hours_since_1900(date2,century2));
}

/****************************************************************************
 * Returns the number of hours since Jan 1, at 00:00 1900.
 * 
 * Interface:
 *   Input:
 *     date:    integer in form yymmddhh
 *     century: 2 digit century (20 for 1900's)
 *   Return:
 *     the number of hours since 00:00 Jan1, 1900.
 *
 ****************************************************************************/
int hours_since_1900(int date,int century)
{
  int daysinmonth[] = {31,28,31,30,31,30,31,31,30,31,30,31};
  int hour,day,month,year;
  int days_since_1900 = 0;
  int i;

  hour = date%100;
  day = (date%10000)/100;
  month = (date%1000000)/10000;
  year = (date%100000000)/1000000;

  days_since_1900 += day;
  
  if (isLeapYear((century-1)*100 + year))
    daysinmonth[1] = 29;
  else 
    daysinmonth[1] = 28;
  
  for (i = 0; i < (month - 1); i++)
    days_since_1900 += daysinmonth[i];

  for (i=0; i < (year + ((century - 20)*100) - 1); i++) {
    if (isLeapYear((century - 1)*100 + year))
      days_since_1900 += 366;
    else
      days_since_1900 += 365;
  }

  return days_since_1900*24 + hour;

}

/****************************************************************************
 *
 * Returns true if the input year is a leap year, otherwise returns false
 *
 ****************************************************************************/
int isLeapYear(int year)
{
  if ( (((year % 4) == 0) && ((year % 100) != 0)) 
       || ((year % 400) == 0) ) 
    return 1;
  else
    return 0;
						  
}

/*****************************************************************************
 *
 * Returns the number of grib elements (gribinfo->num_elements) processsed
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *   the number of elements in the gribinfo structure
 ****************************************************************************/

int rg_num_elements(GribInfo *gribinfo){

  return gribinfo->num_elements;

}

/*****************************************************************************
 *
 * Deallocates the elements in the gribinfo structure and closes the files.
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 *****************************************************************************/
void rg_free_gribinfo_elements(GribInfo *gribinfo)
{
  int i;
  
  for (i=0; i<gribinfo->num_elements; i++) {
    free(gribinfo->elements[i].pds);
    free(gribinfo->elements[i].gds);
    free(gribinfo->elements[i].bms);
    free(gribinfo->elements[i].bds_head);
    fclose(gribinfo->elements[i].fp);
  }
  free(gribinfo->elements);
}

/*****************************************************************************
 *
 * Returns the value for level1 (gribinfo->usHeight1)
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *   value for level1
 ****************************************************************************/

int rg_get_level1(GribInfo *gribinfo, int index)
{
  return gribinfo->elements[index].usHeight1;
}

/*****************************************************************************
 *
 * Returns the value for level2 (gribinfo->usHeight2)
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *   value for level1
 ****************************************************************************/

int rg_get_level2(GribInfo *gribinfo, int index)
{
  return gribinfo->elements[index].usHeight2;
}

/*****************************************************************************
 *
 * returns number of rows in grid
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *    number of rows in grid
 *****************************************************************************/
int rg_get_numrows(GribInfo *gribinfo,int index)
{
  if ((gribinfo->elements[index].gds->head.usData_type == LATLON_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == GAUSS_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == ROT_LATLON_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == ROT_GAUSS_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == STR_LATLON_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == STR_GAUSS_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == STR_ROT_LATLON_PRJ)|| 
      (gribinfo->elements[index].gds->head.usData_type == STR_ROT_GAUSS_PRJ)) 
    {
    return gribinfo->elements[index].gds->llg.usNj;
  } else if (gribinfo->elements[index].gds->head.usData_type == MERC_PRJ) {
    return gribinfo->elements[index].gds->merc.rows;
  } else if (gribinfo->elements[index].gds->head.usData_type == LAMB_PRJ) {
    return gribinfo->elements[index].gds->lam.iNy;
  } else if (gribinfo->elements[index].gds->head.usData_type == POLAR_PRJ) {
    return gribinfo->elements[index].gds->pol.usNy;
  }

}
/*****************************************************************************
 *
 * returns number of columns in grid
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *    number of columns in grid
 *****************************************************************************/
int rg_get_numcols(GribInfo *gribinfo,int index)
{
  if ((gribinfo->elements[index].gds->head.usData_type == LATLON_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == GAUSS_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == ROT_LATLON_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == ROT_GAUSS_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == STR_LATLON_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == STR_GAUSS_PRJ) || 
      (gribinfo->elements[index].gds->head.usData_type == STR_ROT_LATLON_PRJ)|| 
      (gribinfo->elements[index].gds->head.usData_type == STR_ROT_GAUSS_PRJ)) 
    {
      return gribinfo->elements[index].gds->llg.usNi;
  } else if (gribinfo->elements[index].gds->head.usData_type == MERC_PRJ) {
    return gribinfo->elements[index].gds->merc.cols;
  } else if (gribinfo->elements[index].gds->head.usData_type == LAMB_PRJ) {
    return gribinfo->elements[index].gds->lam.iNx;
  } else if (gribinfo->elements[index].gds->head.usData_type == POLAR_PRJ) {
    return gribinfo->elements[index].gds->pol.usNx;
  }

}
/*****************************************************************************
 *
 * returns the offset (in bytes) from the beginning of the file.
 *
 * Input:
 *    gribinfo - pointer to a filled gribinfo structure.
 *
 * Return:
 *    offset (in bytes) from beginning of file
 *****************************************************************************/
int rg_get_offset(GribInfo *gribinfo,int index)
{
  return gribinfo->elements[index].offset;
}
/*****************************************************************************
 *
 * returns the grib record ending position (in bytes) from the beginning of 
 *    the file.
 *
 * Input:
 *    gribinfo - pointer to a filled gribinfo structure.
 *
 * Return:
 *    position (in bytes) of the end of the grib record within the file.
 *****************************************************************************/
int rg_get_end(GribInfo *gribinfo,int index)
{
  return gribinfo->elements[index].end;
}
/*****************************************************************************
 *
 * returns grib id of input grid
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *    grib id of input grid
 *****************************************************************************/
int rg_get_gridnum(GribInfo *gribinfo,int index)
{
  return gribinfo->elements[index].pds->usGrid_id;
}
/*****************************************************************************
 *
 * returns date
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *    date (yymmddhh) in integer type
 *****************************************************************************/
int rg_get_date(GribInfo *gribinfo,int index)
{
  return gribinfo->elements[index].date;
}
/*****************************************************************************
 *
 * returns century
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *    century in integer type
 *****************************************************************************/
int rg_get_century(GribInfo *gribinfo,int index)
{
  return gribinfo->elements[index].century;
}
/*****************************************************************************
 *
 * returns forecast time
 *
 * Input:
 *    gribinfo - pointer to a previously allocated gribinfo structure.  The 
 *                 gribinfo structure is filled in this function.
 *
 * Return:
 *    forecast time in units described by usFcst_unit_id
 *****************************************************************************/
int rg_get_forecast_time(GribInfo *gribinfo,int index)
{
  return gribinfo->elements[index].pds->usP1;
}

/*****************************************************************************
 *
 * reads the gribmap file, and stores the information in the GribParameters
 *    structure.
 *
 * Input:
 *   gribmap - pointer to a previously allocated GribParameters structure.  
 *              The gribmap structure is filled in this function.
 *   file - the name of the gribmap file to read.
 *
 *   Return:
 *      1  - successful call to setup_gribinfo
 *     -1 - call to setup_gribinfo failed
 *
 *****************************************************************************/
int rg_setup_gribmap(GribParameters *gribmap, char filename[])
{
  FILE *fid;
  char line[500];
  int id, center, subcenter, table;
  int idx;
  
  fid = fopen(filename,"r");
  if (fid == NULL)
    {
      fprintf(stderr,"Could not open %s\n",filename);
      return -1;
    }

  gribmap->parms = (GribTableEntry *)malloc(sizeof(GribTableEntry));

  idx = 0;
  while (fgets(line,500,fid) != NULL)
    {
      /* Skip over comments at begining of gribmap file */
      if (line[0] == '#') continue;

      sscanf(line,"%d:",&id);
      if (id == -1) 
	{
	  sscanf(line,"%d:%d:%d:%d",&id,&center,&subcenter,&table);
	} 
      else
	{
	  gribmap->parms = 
	    (GribTableEntry *)realloc(gribmap->parms,
					(idx+1)*sizeof(GribTableEntry));
	  gribmap->parms[idx].center = center;
	  gribmap->parms[idx].subcenter = subcenter;
	  gribmap->parms[idx].table = table;
	  sscanf(line,"%d:%[^:]:%[^:]",&(gribmap->parms[idx].parmid),
		 gribmap->parms[idx].name,
		 gribmap->parms[idx].comment);
	  idx++;
	}
    }


  gribmap->num_entries = idx;

  close(fid);
  return 1;
}

/*****************************************************************************
 *
 * finds the gribmap entry described by  the gribmap file, and stores the information in the GribParameters
 *    structure.
 *
 * Input:
 *   gribmap - pointer to structure that was filled by a call to 
 *             rg_setup_gribmap
 *   table   - if set to -1, the first table the valid name will be used.
 *             Otherwise, the table id must match as well.
 *   name    - name of the parameter to find.
 * Output
 *   gribmap_parms - pointer to GribTableEntry structure containing 
 *             information about the parameter that was found.
 *
 *   Return:
 *      1  - successful call to setup_gribinfo
 *     -1 - call to setup_gribinfo failed
 *
 *****************************************************************************/
int rg_gribmap_parameter(GribParameters *gribmap, char name[], int table,
			 GribTableEntry *gribmap_parms)
{
  int idx;
  int found;

  found = 0;
  for (idx = 0; idx < gribmap->num_entries; idx++) 
    {
      
      if (strcmp(gribmap->parms[idx].name,name) == 0)
	{
	  if ((table == -1) || (table == gribmap->parms[idx].table))
	    {
	      /* We found a match! */
	      found = 1;
	      break;
	    }
	}
    }
  
  if (found) 
    {
      memcpy(gribmap_parms,&(gribmap->parms[idx]),sizeof(GribTableEntry));
      return 1;
    }
  else
    {
      return -1;
    }
}

/*****************************************************************************
 *
 * Deallocates the elements in the gribmap structure.
 *
 * Input:
 *    gribmap - pointer to a previously allocated gribmap structure.  The 
 *                 gribmap structure is filled in this function.
 *
 *****************************************************************************/
void rg_free_gribmap_elements(GribParameters *gribmap)
{
  free(gribmap->parms);
}

/*****************************************************************************
 *
 * Compares the elements in a findgrib structure with the elements in the
 *   gribinfo structure for the input index.  If they match, returns 1, 
 *   otherwise, returns 0.
 *
 * Input:
 *    gribinfo
 *    findgrib
 *    index    - the index of the grib record in gribinfo to compare to.
 *
 *****************************************************************************/
int compare_record(GribInfo *gribinfo, FindGrib *findgrib, int gribnum)
{

  /* 
   *  Note (6/20/05): This searching is very inefficient. We may need to 
   *   improve this, since, for WRF, when searching through boundary data, 
   *   each search is slower that the previous, since the record to be 
   *   found turns out to be farther into the list.
   */
  
  int retval = 0;

  if ((findgrib->center_id == -INT_MAX) || 
      findgrib->center_id == gribinfo->elements[gribnum].center_id) {
    if ((findgrib->subcenter_id == -INT_MAX) ||
	findgrib->subcenter_id == gribinfo->elements[gribnum].subcenter_id) {
      if ((findgrib->parmtbl_version == -INT_MAX) || 
	  findgrib->parmtbl_version == gribinfo->elements[gribnum].parmtbl) {
	if ((strcmp(findgrib->initdate,"*") == 0) || 
	    (strncmp(gribinfo->elements[gribnum].initdate,findgrib->initdate,
		     strlen(findgrib->initdate)) == 0)) {
	  if ((strcmp(findgrib->validdate,"*") == 0) || 
	      (strncmp(gribinfo->elements[gribnum].valid_time,
		       findgrib->validdate,
		       strlen(findgrib->validdate)) == 0)) {
	    if ((findgrib->parmid == -INT_MAX) || 
		(findgrib->parmid == 
		 gribinfo->elements[gribnum].usParm_id)) {
	      if ((findgrib->leveltype == -INT_MAX) || 
		  (findgrib->leveltype == 
		   gribinfo->elements[gribnum].usLevel_id)) {
		if ((findgrib->level1 == -INT_MAX) || 
		    (findgrib->level1 == 
		     gribinfo->elements[gribnum].usHeight1)) {
		  if ((findgrib->level2 == -INT_MAX) || 
		      (findgrib->level2 == 
		       gribinfo->elements[gribnum].usHeight2)) {
		    if ((findgrib->fcsttime1 == -INT_MAX) || 
			(findgrib->fcsttime1 == 
			 gribinfo->elements[gribnum].fcsttime1)) {
		      if ((findgrib->fcsttime2 == -INT_MAX) || 
			  (findgrib->fcsttime2 == 
			   gribinfo->elements[gribnum].fcsttime2)) {
			retval = 1;
		      }
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }

  return retval;
}


/*****************************************************************************
 *
 * returns the multiplication factor to convert grib forecast times to 
 *   seconds.
 *
 * Input: 
 *    unit_id  - grib forecast unit id, from Table 4.
 *
 * Return:
 *    conversion factor
 *****************************************************************************/
int get_factor2(int unit)
{
  int factor;
  
  switch (unit) {
  case 0:
    factor = 60;
    break;
  case 1:
    factor = 60*60;
    break;
  case 2:
    factor = 60*60*24;
    break;
  case 10:
    factor = 60*60*3;
    break; 
  case 11:
    factor = 60*60*3;
    break;
  case 12:
    factor = 60*60*12;
    break;
  case 50:
    /* This is a WSI (non-standard) time unit of 5 minutes */
    factor = 5*60;
    break;
  case 254:
    factor = 1;
    break;
  default:
    fprintf(stderr,"Invalid unit for forecast time: %d\n",unit);
    factor = 0;
  }
  return factor;
}
