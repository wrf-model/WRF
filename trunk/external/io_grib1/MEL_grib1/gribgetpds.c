#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */
/* REVISION/MODIFICATION HISTORY:
       03/07/94 written by Mugur Georgescu CSC, Monterey CA
       02/01/96 modified by Steve Lowe SAIC, Monterey CA
       06/18/96 modified by Alice T. Nakajima (ATN), SAIC, Monterey CA 
       01/22/98 ATN, MRY SAIC
       04/22/98 ATN change requirement for using extensions.
*
************************************************************************
* A.  FUNCTION  gribgetpds
*       Decode the Product Definition Section (PDS) from the provided 
*       pointer location and store the info in the internal PDS structure.
*
*    INTERFACE:
*       int gribgetpds (curr_ptr, pds, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *curr_ptr;    pointer to first octet of PDS
*      (O)  PDS_INPUT *pds;    empty PDS structure to be filled
*      (O)  char *errmsg;      returned filled if error occurred
* 
*     RETURN CODE:
*     0>  Always,  PDS info stored in Pds structure;
************************************************************************
*/
int get_factor(int unit);
#if PROTOTYPE_NEEDED
int gribgetpds ( char *curr_ptr, PDS_INPUT *pds, char *errmsg)
#else

int gribgetpds ( curr_ptr, pds, errmsg)
		char *curr_ptr; 
		PDS_INPUT *pds; 
		char *errmsg;
#endif
{
char *in = curr_ptr;      /* pointer to the message */
unsigned long skip=0;              /* bits to be skipped */
unsigned long something;  /* value extracted from message */
int sign;                 /* sign + or - */
 int unit;
 int P1, P2;

 DPRINT0 ("Entering gribgetpds()\n");
/*
*
* A.1       FUNCTION gbyte !3-byte PDS length
*/
 gbyte(in,&something,&skip,24); 
 DPRINT0 ("pds->uslength\n");
 pds->uslength = (unsigned short) something;       

/*
*
* A.2       FUNCTION gbyte !parameter table version
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usParm_tbl\n");
 pds->usParm_tbl = (unsigned short) something;     

/*
*
* A.3       FUNCTION gbyte !center identification
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usCenter_id\n");
 pds->usCenter_id = (unsigned short) something;    

/*
*
* A.4       FUNCTION gbyte !generating process id
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usProc_id\n");
 pds->usProc_id = (unsigned short) something;      

/*
*
* A.5       FUNCTION gbyte !grid identification
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usGrid_id\n");
 pds->usGrid_id = (unsigned short) something;      

/*
*
* A.6       FUNCTION gbyte !flag of GDS, BMS presence
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usGds_bms_id\n");
 pds->usGds_bms_id = (unsigned short) something;   

/*
*
* A.7       FUNCTION gbyte !parameter indicator and units 
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usParm_id\n");
 pds->usParm_id = (unsigned short) something;      

/*
*
* A.8       FUNCTION gbyte !level type indicator
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usLevel_id\n");
 pds->usLevel_id = (unsigned short) something;  

 /* switch on Level_id to determine if level or layer */
/*
*
* A.9       SWITCH (level_id)
*/
 switch(pds->usLevel_id)
    {
    case 101: /* layer between two isobaric surfaces */
    case 104: /* layer between two specified altitudes */
    case 106: /* layer between two specified height levels above ground */
    case 108: /* layer between two sigma levels */
    case 110: /* layer between two hybrid levels */
    case 112: /* layer between two depths below land surface */
    case 114: /* layer between two isentropic levels */
    case 121: /* layer between two isobaric surfaces (high precision) */
    case 128: /* layer between two sigma levels (high precision) */
    case 141: /* layer between two isobaric surfaces (mixed precision) */
/*
*              layer:
*                 FUNCTION gbyte !top of layer
*                 FUNCTION gbyte !bottom of layer
*/
       gbyte(in,&something,&skip,8);
       DPRINT0 ("pds->usHeight1\n");
       pds->usHeight1 = (unsigned short) something;  /* top layer */
       gbyte(in,&something,&skip,8);
       DPRINT0 ("pds->usHeight2\n");
       pds->usHeight2 = (unsigned short) something;  /* bottom layer */
       break;

    default:  /* all others (levels) */
/*
*              default:  !assume a level
*                 FUNCTION gbyte !level value
*                 SET Height2 to ZERO
*/
       gbyte(in,&something,&skip,16);
      DPRINT0 ("pds->usHeight1\n");
       pds->usHeight1 = (unsigned short) something;
       pds->usHeight2 = 0.0;                
       break;
    }
/*
* A.9       ENDSWITCH
*/

/*
*
* A.10      FUNCTION gbyte !year of Reference Data/Time
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usYear\n");
 pds->usYear = (unsigned short) something;   

/*
*
* A.11      FUNCTION gbyte !month of Reference Data/Time
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usMonth\n");
 pds->usMonth = (unsigned short) something;   

/*
*
* A.12      FUNCTION gbyte !day of Reference Data/Time
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usDay\n");
 pds->usDay = (unsigned short) something;      

/*
*
* A.13      FUNCTION gbyte !hour of Reference Data/Time
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usHour\n");
 pds->usHour = (unsigned short) something;      

/*
*
* A.14      FUNCTION gbyte !minute of Reference Data/Time
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usMinute\n");
 pds->usMinute = (unsigned short) something;     

/*
*
* A.15      FUNCTION gbyte !forecast time unit
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usFcst_unit_id\n");
 pds->usFcst_unit_id = (unsigned short) something;

/*
*
* A.16      FUNCTION gbyte !forecast period 1
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usP1\n");
 pds->usP1 = (unsigned short) something;         

/*
*
* A.17      FUNCTION gbyte !forecast period 2
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usP2\n");
 pds->usP2 = (unsigned short) something;          

/*
*
* A.18      FUNCTION gbyte !time range indicator
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usTime_range\n");
 pds->usTime_range = (unsigned short) something;   

/*
*
* A.19      FUNCTION gbyte !#included in average
*/
 gbyte(in,&something,&skip,16); 
 DPRINT0 ("pds->usTime_range_avg\n");
 pds->usTime_range_avg = (unsigned short) something;

/*
*
* A.20      FUNCTION gbyte !#missing from average
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usTime_range_mis\n");
 pds->usTime_range_mis = (unsigned short) something;

/*
*
* A.21      FUNCTION gbyte !century of Reference Data/Time
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usCentury\n");
 pds->usCentury = (unsigned short) something;  

/*
*
* A.22      FUNCTION gbyte !originating Sub-Center  (Oct 26)
*/
 gbyte(in,&something,&skip,8); 
 DPRINT0 ("pds->usCenter_sub (Oct 26)\n");
 pds->usCenter_sub = 	(unsigned short) something;

/*
*
* A.23      FUNCTION gbyte !decimal scale factor
*/
 gbyte(in,&something,&skip,16); 
      DPRINT0 ("Sign & pds->sDec_sc_fctr\n");
 sign = (int)(something >> 15) & 1;                /* sign bit*/
 pds->sDec_sc_fctr = (short) (something) & 32767;  /* Decimal sclfctr D */
 if(sign)                                          /* negative Dec. sclfctr*/
    pds->sDec_sc_fctr = - pds->sDec_sc_fctr;       /* multiply by -1 */

 /*
  * This is the WSI extension for forecast time unit
  */

 if (pds->usTime_range == 255)
   {

     /* Skip ahead to byte 41 */
     skip += 96;
     
     /* Get forecast time unit for P1 from byte 41 */
     gbyte(in,&something,&skip,8);
     unit = (unsigned short)something;
     
     /* Get P1 */
     gbyte(in,&something,&skip,32);
     P1 = (unsigned int)something;
     pds->usP1 = get_factor(unit)*P1;

     /* Get forecast time unit for P2 from byte 46 */
     gbyte(in,&something,&skip,8);
     unit = (unsigned short)something;

     /* Get P2 */
     gbyte(in,&something,&skip,32);
     P2 = (unsigned int)something;
     pds->usP2 = get_factor(unit)*P2;

     /* Get Time Range Indicator */
     gbyte(in,&something,&skip,8);
     pds->usTime_range = (unsigned short)something;

     /* 
      * Set forecast time unit to seconds, since we've converted usP1 and usP2
      *   to seconds.
      */
     pds->usFcst_unit_id = 254;
   }

/*
* A.26      DEBUG Print
*/
  DPRINT0 ("Exiting gribgetpds(), status=0\n");

/*
* 
* A.27      RETURN 0  !success
*/
return(0);
/*
* END OF FUNCTION
*
*
*/
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
int get_factor(int unit)
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
