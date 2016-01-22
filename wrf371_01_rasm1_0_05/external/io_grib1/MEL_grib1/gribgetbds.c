#include <stdio.h>
#include <stdlib.h>
#include <math.h> 
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */
/*
  REVISION/MODIFICATION HISTORY:
       03/07/94 written by Mugur Georgescu CSC, Monterey CA
       02/01/96 modified by Steve Lowe SAIC, Monterey CA
       04/17/96 modified by Alice Nakajima SAIC, Monterey CA
       06/19/96 add hdrprint;/nakajima
* 
* ********************************************************************
* A.  FUNCTION:  gribgetbds 
*       decodes the Binary Data Section of the GRIB message 
*       and filling grib_data float array.
*
*    INTERFACE:
*       int gribgetbds (curr_ptr, deci_scale, bms, gds, 
*                       ppgrib_data, bds_head, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *curr_ptr;
*           points to first Octet of the BDS to be decoded;
*      (I)  short  deci_scale;
*           decimal scaling factor to be applied to data;
*      (I)  BMS_INPUT *bms;
*           points to the decoded internal Bit Map Section Struct 
*      (I/O) grid_desc_sec *gds
*           points to decoded internal grid definition struct
*      (O)  float **ppgrib_data;
*           double pointer to array of float, null upon entry;
*           upon successful exit, holds the unpacked and restored float data
*           in a newly malloced array;
*      (O)  BDS_HEAD_INPUT *bds_head; 
*           points to Binary Data Sect hdr struct, empty upon entry;
*           to be filled with decoded BDS info;
*      (O)  char  *errmsg;
*           Only returned filled when error occurred;
*
*      RETURN CODE:
*        0>   no errors
*        1>   unrecognized packing algorithm
*        2>   number of points does not match bitmap
*        3>   number of points does not match grid size in GDS
*        4>   malloc error
* ********************************************************************
*/

#if PROTOTYPE_NEEDED
int gribgetbds ( char *curr_ptr, short  deci_scale, BMS_INPUT *bms,
        grid_desc_sec *gds, float **ppgrib_data,
        BDS_HEAD_INPUT *bds_head, char  *errmsg)
#else
int gribgetbds (curr_ptr, deci_scale, bms, gds, ppgrib_data, 
		bds_head, errmsg)
		char *curr_ptr; 
		short  deci_scale; 
		BMS_INPUT *bms;
		grid_desc_sec *gds;
		float **ppgrib_data; 
		BDS_HEAD_INPUT *bds_head; 
		char  *errmsg;
#endif
{
char *func="gribgetbds";
char *in = curr_ptr;      /* pointer to beginning of BDS */
long length;              /* size of the Binary Data Section */
long scale;               /* scaling factor */
float ref_val;            /* reference value (minimum value) */
unsigned long something;  /* generic value from message */
long data_width;          /* number of bits that data occupies */
int halfBYTE4;            /* the first 4 bits in 4-th byte */
int status=0;
int sign;                 /* sign + or - */
float dscale;             /* 10 to the decimal scaling power */
float bscale;             /* 2 to the binary scaling power   */
unsigned long skip=0;              /* number of bits to be skipped */
long c_ristic;            /* characteristic for float representation */
long mantissa;            /* mantissa for float representation */
long numpts;              /* number of bits left at end of bitstream */
unsigned long data_pts;   /* number of data points in bitstream */
unsigned long num_calc;	  /* temp work var */
float *grib_data=0;       /* local work array for grid data */
float fdata=0;            /* data value stored in reference */
int	i,j;         /* array counter */
int xsize, ysize; 
float *outdata;

  DPRINT1 ("Entering %s()\n", func);
/*
*
* A.1       FUNCTION gbyte !get bds length
*/

  gbyte(in,(unsigned long *) &length,&skip,24);
  DPRINT0 ("bds_head->length\n");
  bds_head->length = (unsigned long) length;

/*
*
* A.2       FUNCTION gbyte !get BDS flag 
*/
  gbyte(in, (unsigned long *) &halfBYTE4, &skip, 4);
  DPRINT0 ("bds_head->usBDS_flag\n");
  bds_head->usBDS_flag = (short) halfBYTE4;  /* get BDS Flag (Table 11) */

/*
*
* A.3       IF (unsupported packing algorithm)  THEN
*               RETURN 1
*           ENDIF
*/
  /* need to check on packing algorithm */
  if (halfBYTE4)  /* unrecognized packing algorithm */
    {
     DPRINT1 ("%s:  error, unrecognized packing algorithm\n", func);
     sprintf(errmsg, "%s:  unrecognized packing algorithm\n", func);
     status= (1);
     goto BYE;
    }

/*
*
* A.4       FUNCTION gbyte !get number of unused bits
*/
  gbyte(in,(unsigned long *) &numpts,&skip,4); /* get #bits at end of BDS */
  DPRINT0 ("numpts\n");

/*
*
* A.5       FUNCTION gbyte !get Binary Scale Factor
*/
  gbyte(in,&something,&skip,16);
  DPRINT0 ("Sign & bds_head->Bin_sc_fctr\n");
  sign = (int)(something >> 15) & 1;  /* get sign for scale */
  scale = (int)(something) & 32767;   /* get scale */
  if(sign)                            /* scale negative */
     scale = -scale;                  /* multiply scale by -1 */
  bds_head->Bin_sc_fctr = (int) scale;  /* get binary scale factor */
  DPRINT1 ("Binary Scale Factor = %d\n", scale);

/*
*
* A.6       CALCULATE Reference value from IBM representation
*             !FUNCTION gbyte !get the sign of reference
*             !FUNCTION gbyte !get charateristic
*             !FUNCTION gbyte !get the mantissa
*/
  gbyte(in,&something,&skip,8);
  DPRINT0 ("Sign & Reference)\n");
  sign = (int)(something >> 7) & 1; /* get the sign for reference value */

  skip -= 7;
  gbyte(in,(unsigned long *)&c_ristic,&skip,7); /*characteristic for the float*/
  DPRINT0 ("c_ristic\n");

  gbyte(in,(unsigned long*)&mantissa,&skip,24); /*mantissa for the float */
  DPRINT0 ("mantissa\n");
  c_ristic -= 64;                   /* subtract 64 from characteristic */
  ref_val = (float) mantissa * (float)(pow(16.0,(double)c_ristic)) * 
	    (pow(2.0,-24.0));
  if(sign)                          /* negative reference value */
     ref_val = -ref_val;            /* multiply ref_val by -1 */
  bds_head->fReference = (float)ref_val;
  DPRINT1 ("Reference = %f\n", ref_val);

/*
*
* A.7       FUNCTION gbyte !get data width
*/
  gbyte(in, (unsigned long*) &data_width,&skip,8);       /* get data width */
  DPRINT0 ("bds_head->usBit_pack_num\n");
  bds_head->usBit_pack_num = (short)data_width;

/*
*
* A.8       SET Binary and Decimal Scale Factors
*/
  /* set binary scale */
  bscale = (float)pow (2.0,(double) scale);

  /* set decimal scale */
  dscale = (float)pow (10.0, (double) deci_scale);

  DPRINT2 ("Scaled-up BSF= (2**%d) = %f\n", scale, bscale);
  DPRINT2 ("Scaled-up DSF= (10**%d) = %f\n", deci_scale,dscale);

/*
*
* A.9       IF (data_width is zero) THEN
*               ! grid contains a constant value
*               IF expected grid size is invalid THEN
*                  FORCE grid size of 1
*               ENDIF
*               ALLOCATE array for grid of expected grid size
*               FILL grid with the constant value
*               RETURN 0 !success
*           ENDIF
*/
  if (!data_width)   /* grid contains constant value, success, all done */
  {
/* Used to send back array of 1 element:
#     bds_head->ulGrid_size = 1;
#     fdata = (float) (ref_val / dscale);
#     *ppgrib_data = (float *) malloc(sizeof(float));
#     **ppgrib_data = fdata;
*/

     fdata = (float) (ref_val / dscale);
     if (bds_head->ulGrid_size <= 0) {
	fprintf(stdout,
	"WARNING:  gribgetbds detects bad ulGrid_size (%ld); "\
	"Set to 1 to hold constant value %lf\n", bds_head->ulGrid_size, fdata);
	bds_head->ulGrid_size = 1;
	}
    
     grib_data =(float *) malloc(bds_head->ulGrid_size * sizeof(float));
     if (!grib_data) {
	sprintf(errmsg,
	"%s: failed to create array[%d] for grid to hold Constant data", 
	func, bds_head->ulGrid_size);
	goto BYE;
	}

     *ppgrib_data = grib_data;		/* store address */
     for (i=0; i < bds_head->ulGrid_size ; ) 
	grib_data[i++] = fdata;		/* fill grid with constant val */

     DPRINT3("%s:  grid[%ld] contains convant value %lf\n",
	func, bds_head->ulGrid_size, fdata);

     status = (0);  /* no errors */
     goto BYE;
  }

  /* fill the data array with values from message */
  /*     - Assume that GDS may not be included so that
   *         the number of grid points may not be defined.       
   *     - Compute space to malloc based on BDS length,
   *         data_width, and numpts.
   *     - if grid_size from GDS is zero, use
   *         computed number of points.
   */

/*
*
* A.10      CALCULATE number of data points actually in BDS
*/
  num_calc = ((length - 11)*8 - numpts) / data_width;

  /* Check the number of points computed against info in the BMS
     or GDS, if they are available */

/*
*
* A.11      IF (BMS is present and has included bitmap) THEN
*               IF (#calculated not same as #bits set in BMS) THEN
*                   RETURN 2
*               ENDIF
*/

  if (bms->uslength > 6)
  {
      if (bms->ulbits_set != num_calc) {
	DPRINT3 ("%s:  BMS present, #datapts calculated (%d) " \
	"not same as BMS's set bits (%d)\n",func, num_calc, bms->ulbits_set);

	sprintf(errmsg,"%s:  BMS present, #datapts calculated (%d) " \
	"not same as BMS's set bits (%d)\n",func, num_calc, bms->ulbits_set);
	status= (2); goto BYE;
	}
  }
/*
* A.11.1    ELSE  !no bms
*               IF (GDS is present AND
*                   #calculated not same as GDS's grid size)
*               THEN
*                   RETURN 3
*               ENDIF
*/
  
  else
    
  {    
      if (bds_head->ulGrid_size && bds_head->ulGrid_size != num_calc) {
	DPRINT0("Averting failure of grid size test\n");
	/*
	DPRINT3 ( "%s:  GDS present, #datapts calculated (%d) " \
	   "not same as GDS's grid size (%d)\n",
	   func,num_calc,bds_head->ulGrid_size);

	   sprintf(errmsg,"%s:  GDS present, #datapts calculated (%d) " \
	   "not same as GDS's grid size (%d)\n",
	   func,num_calc,bds_head->ulGrid_size);
	   status=(3); goto BYE;
	*/
	 }          
  }
    
  
/*
* A.11      ENDIF (BMS present)
*/

  /* Only reach this point if number of points in BDS matches info
     in BMS or GDS.  This number is unchecked if no BMS or GDS. */

  /* Make sure number of points in BDS is value used for extracting */
/*
*
* A.12      SET #datapoints
*/
  bds_head->ulGrid_size = num_calc;
  data_pts= num_calc;

/*
*
* A.13      ALLOCATE storage for float array size
*           IF (error) THEN
*               RETURN 4
*           ENDIF
*/
  grib_data =(float *) malloc(data_pts * sizeof(float));
  if (grib_data==NULL) { 
	DPRINT1 ("%s: failed to malloc Grib_Data\n",func);
	sprintf(errmsg,"%s: failed to malloc Grib_Data\n",func);
	status=(4); goto BYE; }

/*
*
* A.14      SET data array pointer to local data array
*/
  *ppgrib_data = grib_data;

/*
*
* A.15      FOR (each data point) DO
*               FUNCTION gbyte_quiet   !get data_width bits
*               INCREMENT skip by data_width
*               COMPUTE and STORE value in float array
*           ENDDO
*/

  DPRINT3 ( "Restore float data by = (float)(%f + X * %f)) / %f;\n",
  ref_val, bscale, dscale);

  for(i=0;i < data_pts ;i++)  
     {
     gbyte_quiet(in,&something,&skip,data_width);
     grib_data[i]= (float)(ref_val + (something * bscale))/dscale;
     }

/*
 * Unthin grid if it is thinned
 */
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
    } else {
       DPRINT2 ("%s:  unknown datatype=%d\n",func, gds->head.usData_type);
       sprintf(errmsg,"%s:  unknown datatype=%d\n",func, gds->head.usData_type);
       status=1;         /* set status to failure */
    }

    xsize = 0;
    for (j = 0; j<ysize; j++) {
      if (gds->head.thin[j] > xsize) {
	xsize = gds->head.thin[j];
      }
    }
    outdata = (float *)malloc(ysize*xsize*sizeof(float));
    grib_unthin(grib_data,outdata,gds->head.thin,ysize,
	  &xsize);
    free(grib_data);
    grib_data = (float *)malloc(sizeof(float)*ysize*xsize);
    *ppgrib_data = grib_data;
    memcpy(grib_data,outdata,sizeof(float)*ysize*xsize);
    free(outdata);
    free(gds->head.thin);
    gds->head.thin = NULL;
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
    } else {
       DPRINT2 ("%s:  unknown datatype=%d\n",func, gds->head.usData_type);
       sprintf(errmsg,"%s:  unknown datatype=%d\n",func, gds->head.usData_type);
       status=1;         /* set status to failure */
    }
  }

  DPRINT0 ("Sample of first 30 unpacked & restored datapoints=\n");
  for (i=0; i < 30; i+=5)
     DPRINT6 ("%03d:  %f %f %f %f %f\n",
	i, grib_data[i], grib_data[i+1],grib_data[i+2],
	grib_data[i+3], grib_data[i+4] );
  
BYE:
/*
*
* A.16      RETURN Status;
*/
  DPRINT2 ("Exiting %s, status=%d\n", func, status);
  return(status); 
/* 
* END OF FUNCTION
*
*/
}    
