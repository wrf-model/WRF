#include <stdio.h>              /* standard I/O header file          */
#include <stdlib.h>
#include <string.h>
#include "dprints.h"		/* for dprints & func prototype*/
#include "gribfuncs.h"		/* prototypes */

/* PROGRAMMER : Steve Lowe and Todd Kienitz, SAIC Monterey
   DATE       : February 7, 1996
                Oct. 1996 by Alice Nakajima, SAIC Monterey
*
*********************************************************************
* A.  FUNCTION:  grib_dec
*     decode a Gridded Binary (GRIB edition 1) format message
*
*    INTERFACE:
*      int grib_dec (curr_ptr, pds, gds, bds_head, bms, ppgrib_data, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *curr_ptr;
*           pointer to block containing GRIB message to decode;
*      (O)  PDS_INPUT  *pds ;
*           to be filled with decoded Product Defn Section info;
*      (O)  grid_desc_sec  *gds;
*           to be filled with decoded Binary Data Section info;
*      (O)  BDS_HEAD_INPUT *bds_head;
*           to be filled with decoded Binary Data Section info;
*      (O)  BMS_INPUT *bms;
*           to be filled with decoded Bitmap Section info;
*      (O)  float **ppgrib_data;
*           points to NULL upon entry; upon successful exit, points to newly
*           malloced Float array filled with unpacked and restored data;
*      (O)  char *errmsg;
*           Empty array, Returned filled if error occurred;
*
*     RETURN CODE:
*        0> Success, **ppgrib_data now points to a block containing
*           the unpacked & restored data (float);
*        1> Fail: first 4 bytes of curr_ptr is not 'GRIB'
*        2> Fail: last 4 bytes of curr_ptr is not '7777'
*        3> Fail: not Grib Edition 1
*        4> Fail: unknown projection type;
***********************************************************************
*/

#if PROTOTYPE_NEEDED
int   grib_dec (char *curr_ptr, PDS_INPUT  *pds, grid_desc_sec  *gds,
        	BDS_HEAD_INPUT *bds_head, BMS_INPUT *bms, float **ppgrib_data,
        	char *errmsg)
#else
int grib_dec (curr_ptr, pds, gds, bds_head, bms, ppgrib_data, errmsg)
  char          *curr_ptr; /*input= ptr to 1st byte of GRIB message block*/
  PDS_INPUT     *pds;      /* output=ptr to Internal PDS struct*/
  grid_desc_sec *gds;      /* output=ptr to Internal GDS struct*/
  BDS_HEAD_INPUT*bds_head; /*out=ptr to Internal BDS header struct*/
  BMS_INPUT     *bms;      /*output=ptr to Internal bitmap section struct*/
  float         **ppgrib_data; /*outp=ptr to nothing upon entry; upon exit, */
                           /* points to a newly malloced array of floats;  */
  char          *errmsg;   /* output= empty unless Error happens */
#endif
{
  char *func="grib_dec";
  unsigned long lMessageSize;       /* message and section size */
  long edition;                     /* GRIB edition number */
  int flag;                         /* tests if a condition has happened */
  int gds_flag;			    /* set if Gds present */
  int nReturn = 0;
  unsigned long skip;
  float *outdata;
  int xsize;
  int j;

/*
*
* A.0     DEBUG printing
*/
 DPRINT1 ("Entering %s\n", func);
 DPRINT6 (
  "curr_ptr=%ld, pds=%ld, gds=%ld\nbds_head=%ld, bms=%ld, ppgrib_data=%ld\n",
  curr_ptr, pds, gds, bds_head, bms, ppgrib_data);
/*
*
* A.1     IF (incoming pointer is not at 'GRIB') 
*            RETURN 1  !errmsg filled
*         ENDIF
*/
if(strncmp(curr_ptr,"GRIB",4) != 0) {
  sprintf (errmsg,"%s:  no 'GRIB' at beg. of this msg\n", func);
  nReturn= (1);   /* GRIB not found */
 }

/*
*
* A.2     FUNCTION gbyte   !get total message length from IDS 
*/
skip=32;
gbyte(curr_ptr,&lMessageSize,&skip,24);
DPRINT0 ("lMessageSize\n");

/*
*
* A.3     IF (Message does not end with '7777') 
*            RETURN 2  !errmsg filled
*         ENDIF
*/
if(strncmp((curr_ptr + lMessageSize - 4),"7777",4)!=0) {
  DPRINT1 ("%s:  no '7777' at end of this msg\n", func);
  sprintf (errmsg,"%s:  no '7777' at end of this msg\n", func);
  nReturn= 2; goto BYE;
  }

/*
*
* A.4     EXTRACT the  GRIB edition out of Section 0
*         IF (not GRIB edition 1)
*            RETURN 3  !errmsg filled
*         ENDIF
*/
edition = (long) curr_ptr[7];        /* get edition */
pds->usEd_num = (unsigned short) edition;
if(edition != 1) {
  DPRINT1 ("%s:  error, not Grib Edition 1 \n", func);
  sprintf (errmsg,"%s:  not Grib Edition 1 \n", func);
  nReturn=(3);   goto BYE;
  }

/*
*
* A.5     MOVE pointer to the Product Definition section
*/
curr_ptr = curr_ptr + 8;

/* 
*
* A.6     FUNCTION gribgetpds  !decode the PDS 
*         RETURN error code if fails  !errmsg filled
*/
if( nReturn= gribgetpds(curr_ptr, pds, errmsg)) {
   DPRINT2 ("%s:  error=%d  in grib get pds;\n", func, nReturn);
   upd_child_errmsg (func, errmsg);
   goto BYE;
  }

/* 
*
* A.7     MOVE pointer to the end of PDS
*/
curr_ptr += pds->uslength;

/*
*
* A.8     IF (GDS is present)
*/
gds_flag = pds->usGds_bms_id >> 7 & 1;
if(gds_flag)  /* grid description section present */
  {
/*
* A.8.1      FUNCTION gribgetgds   !decode GDS
*            RETURN error code if fails  !errmsg filled
*/
   if( nReturn=gribgetgds(curr_ptr, gds, errmsg)) {
      DPRINT2 ("%s:  error=%d  in grib get GDS;\n", func, nReturn);
      upd_child_errmsg (func, errmsg);
      goto BYE;
       }

/* 
* A.8.2      MOVE the cursor to the next section (either BMS/BDS)
*/
   curr_ptr += gds->head.uslength;
/* 
* A.8.3      SET the number of data points depending on Projection
*/
   switch(gds->head.usData_type)
  {
     case LATLON_PRJ:    	/* Lat/Lon Grid */
     case GAUSS_PRJ:    	/* Gaussian Latitude/Longitude grid */
     case ROT_LATLON_PRJ:   	/* Rotated Lat/Lon */
     case ROT_GAUSS_PRJ:   	/* Rotated Gaussian */
     case STR_LATLON_PRJ:   	/* Stretched Lat/Lon */
     case STR_GAUSS_PRJ :   	/* Stretched Gaussian */
     case STR_ROT_LATLON_PRJ :  /* Stretched and Rotated Lat/Lon */
     case STR_ROT_GAUSS_PRJ :   /* Stretched and Rotated Gaussian */
       	bds_head->ulGrid_size = gds->llg.usNi * gds->llg.usNj;       
	break;

     case MERC_PRJ:  		/* Mercator Grid */
       	bds_head->ulGrid_size = gds->merc.cols * gds->merc.rows; 
	break;

     case LAMB_PRJ:  		/* Lambert Conformal */
     case ALBERS_PRJ:  		/* Albers equal-area */
     case OBLIQ_LAMB_PRJ: 	/* Oblique Lambert Conformal */
       	bds_head->ulGrid_size = gds->lam.iNx * gds->lam.iNy; 
	break;

     case POLAR_PRJ:  		/* Polar Stereographic */
       	bds_head->ulGrid_size = gds->pol.usNx * gds->pol.usNy; 
	break;

     default: /* unknown */
       DPRINT2 ("%s: unknown usData_type=%d\n",func,gds->head.usData_type);
       sprintf(errmsg,"%s: unknown usData_type=%d\n", 
	func, gds->head.usData_type);
       nReturn= (4); goto BYE;
     }
  }
/*
* A.8     ENDIF (GDS is present)
*/

/*
*
* A.9     IF (bitmap Section is present)
*/
flag = pds->usGds_bms_id >> 6 & 1;
if(flag)  /* bit map section present */
  {
/*
* A.9.1      FUNCTION gribgetbms   !decode BMS
*            RETURN error code if fails  !errmsg filled
*/
   if( nReturn=gribgetbms(curr_ptr,bms,gds_flag, bds_head->ulGrid_size,errmsg)) 
   { 
     DPRINT2 ("%s:  error=%d  in grib get BMS;\n",func,nReturn);
     upd_child_errmsg (func, errmsg);
     goto BYE;
   } 

/* 
* A.9.2      MOVE the cursor to beginning of Binary Data Section
*/
     curr_ptr += bms->uslength;

  } /* Bms present */
/*
* A.9     ENDIF  !bms present
*/


/*
*
* A.10    FUNCTION  gribgetbds()
*         RETURN error code if failed  !errmsg filled
*/
  if(nReturn=gribgetbds(curr_ptr, pds->sDec_sc_fctr, bms, gds, ppgrib_data, 
		bds_head, errmsg)) 
   { 
     DPRINT2 ("%s:  error=%d  in grib get BDS;\n",func,nReturn);
     upd_child_errmsg (func, errmsg);
     goto BYE;
    }
 
/*
* 
* A.11    SET return code to 0  !no errors
*/
  nReturn = 0;

/*
*
* A.12    RETURN return code;
*/
BYE:
  DPRINT2  ("Exit %s, Stat=%d\n", func, nReturn);
  return(nReturn);
/*
*
* END OF FUNCTION
*
*
*/
} 
