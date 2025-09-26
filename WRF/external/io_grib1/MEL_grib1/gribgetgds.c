#include <stdio.h>
#include <stdlib.h>
#include "dprints.h"		/* for dprints & func prototypes  */
#include "gribfuncs.h"		/* prototypes */
/*
  REVISION/MODIFICATION HISTORY:
       03/07/94 written by Mugur Georgescu CSC, Monterey CA
       02/01/96 modified by Steve Lowe SAIC, Monterey CA
       06/19/96 modified by Alice Nakajima SAIC, Monterey CA
       10/15/97/ATN init usData_type in Projection struct too
       02/18/98/atn replace projection ids with constants
*
******************************************************************* 
* A.  FUNCTION  gribgetgds 
*       Decode the Grid Description Section (GDS) from the provided 
*       pointer location and store its the in the internal GDS structure;
*
*    INTERFACE:
*       int gribgetgds (curr_ptr, gds, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  char *curr_ptr;        points to 1st octet of GDS
*      (O)  grid_desc_sec *gds;    internal GDS structure to be filled
*      (O)  char *errmsg;          returned filled if error occurred
*
*    RETURN CODE:
*       0>  success, struct grid_desc_sec filled;
*       1>  unsupported projection number, errmsg filled;
*       2>  section length too short, errmsg filled;
******************************************************************* 
*/

#if PROTOTYPE_NEEDED
int gribgetgds ( char *curr_ptr, grid_desc_sec *gds, char *errmsg)
#else
int gribgetgds ( curr_ptr, gds, errmsg)
		char *curr_ptr; 
		grid_desc_sec *gds; 
		char *errmsg;
#endif
{
char *func= "gribgetgds";
char *in = curr_ptr;      /* pointer to the message */
unsigned long skip;             /* bits to be skipped */
unsigned long something;  /* value extracted from message */
int sign;                 /* sign + or - */
int status;
FILE *fp;
int i;
 
 DPRINT1 ("Entering %s\n", func);
/*
*
* A.0      INIT status to good, skip to 0
*/
 status=0;  skip=0;
/*
*
* A.1      FUNCTION gbyte  !GDS length 
*          IF (length < 32 bytes) THEN
*             SET error status of 2   !length too short
*             CONTINUE to load as much info as possible into
*             structure 'grid_desc_sec' but will return with error
*          ENDIF
*/
 gbyte(in,&something,&skip,24); DPRINT0 ("gds->head.uslength\n");
 gds->head.uslength = (unsigned short) something;  
 if (gds->head.uslength < 32) {
    sprintf(errmsg,
    "GDS length too short (%ld).  Will attempt to load struct grid_desc_sec\n",
    gds->head.uslength);
    status = 2;		/* corrupt length */
   }

/* 
*
* A.2      FUNCTION gbyte  !parm_nv
*/
 gbyte(in,&something,&skip,8); DPRINT0 ("gds->head.usNum_v\n");
 gds->head.usNum_v =(short) something;             
/* 
*
* A.3      FUNCTION gbyte  !parm_pv_pl
*/
 gbyte(in,&something,&skip,8); DPRINT0 ("gds->head.usPl_Pv\n");
 gds->head.usPl_Pv = (short) something;            

/* 
*
* A.4      FUNCTION gbyte  !data representation type
*/
 gbyte(in,&something,&skip,8); DPRINT0 ("gds->head.usData_type\n");
 gds->head.usData_type = (short) something;        

/* Remainder of GDS is projection dependent */
/*
*
* A.5      SWITCH (data type)
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
/*
*           case latlon: 
*           case gaussian_latlon:
*           case rotated gaussian:
*           case stretched latlon:
*           case stretched gaussian:
*           case stretched & rotated latlon:
*           case stretched & rotated gaussian:
*                 Mark the Projection type 
*                 FUNCTION gbyte !get Number of Columns
*/
       gds->llg.usData_type = gds->head.usData_type;

       gbyte(in, &something, &skip, 16); 
       DPRINT0 ("gds->llg.usNi\n");
       gds->llg.usNi = (int) something;                   /* get Ni */

/*
*                 FUNCTION gbyte !get Number of Rows
*/
       gbyte(in, &something, &skip, 16);
       DPRINT0 ("gds->llg.usNj\n");
       gds->llg.usNj = (int) something;                   /* get Nj */

/*
*                 FUNCTION gbyte !get Latitude of First point
*/
       gbyte(in,&something,&skip,24);
       DPRINT0 ("Sign & gds->llg.lLat1 \n");
       sign = (int)(something >> 23) & 1;                 /* get sign */
       gds->llg.lLat1 = (long) (something) & 8388607;     /* get La1 */
       if(sign)                                           /* negative value */
          gds->llg.lLat1 = - gds->llg.lLat1;              /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Longitude of First point
*/
       gbyte(in,&something,&skip,24);
       DPRINT0 ("Sign & gds->llg.lLon1 \n");
       sign = (int)(something >> 23) & 1;                 /* get sign */
       gds->llg.lLon1 = (long) (something) & 8388607;     /* get Lo1 */
       if(sign)                                           /* negative value */
           gds->llg.lLon1 = - gds->llg.lLon1;             /* multiply by -1 */

/*
*                 FUNCTION gbyte !get resolution & comp flags
*/
       gbyte(in,&something,&skip,8);
       DPRINT0 ("gds->llg.usRes_flag\n");
       gds->llg.usRes_flag = (short) something;           /* get resolution & comp flags */

       gbyte(in,&something,&skip,24);
       DPRINT0 ("Sign & gds->llg.lLat2 \n");
/*
*                 FUNCTION gbyte !get Latitude of Last point
*/
       sign = (int)(something >> 23) & 1;                 /* get sign */
       gds->llg.lLat2 = (long) (something) & 8388607;     /* get La2 */
       if(sign)                                           /* negative value */
          gds->llg.lLat2 = - gds->llg.lLat2;              /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Longitude of Last point
*/
       gbyte(in,&something,&skip,24);
       DPRINT0 ("Sign & gds->llg.lLon2 \n");
       sign = (int)(something >> 23) & 1;                 /* get sign */
       gds->llg.lLon2 = (long) (something) & 8388607;     /* get Lo2 */
       if(sign)                                           /* negative value */
          gds->llg.lLon2 = - gds->llg.lLon2;              /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Longitudinal Increment
*/
       gbyte(in,&something,&skip,16);
       DPRINT0 ("gds->llg.iDi\n");
       gds->llg.iDi = (int) something;			  /* get Di */

/*
*                 FUNCTION gbyte !get Latitudinal Increment
*/
       gbyte(in,&something,&skip,16);
       DPRINT0 ("gds->llg.iDj\n");
       gds->llg.iDj = (int) something;			  /* get Dj */

/*
*                 FUNCTION gbyte !get scanning mode
*/
       gbyte(in,&something,&skip,8);
       DPRINT0 ("gds->llg.usScan_mode\n");
       gds->llg.usScan_mode = (short) something;    /* get scaning mode flag */

/*
*                 FUNCTION gbyte !get reserved octets 29-32
*/
       gbyte(in,&something,&skip,32);
       DPRINT0 ("gds->llg.usZero\n");
       gds->llg.usZero = (long) something; /* get reserved octets 29 - 32 */

       if (gds->head.usNum_v > 0) {
/*
*                 FUNCTION gbyte !get south pole lat
*/
	 gbyte(in,&something,&skip,24);
	 DPRINT0 ("Sign & gds->llg.lLat_southpole \n");
	 sign = (int)(something >> 23) & 1;                      /* get sign */
	 gds->llg.lLat_southpole = (long)(something) & 8388607; /* southpole lat*/
	 if(sign)                                            /* negative value */
	   gds->llg.lLat_southpole = - gds->llg.lLat_southpole; /* multiply -1 */
	 
/*
*                 FUNCTION gbyte !get south pole lon
*/
	 gbyte(in,&something,&skip,24);
	 DPRINT0 ("Sign & gds->llg.lLon_southpole \n");
	 sign = (int)(something >> 23) & 1;                      /* get sign */
	 gds->llg.lLon_southpole =(long)(something) & 8388607; /* southpole lon*/
	 if(sign)                 /* negative value , multiply by -1 */
	   gds->llg.lLon_southpole = - gds->llg.lLon_southpole; 
	 
/*
*                 FUNCTION gbyte !angle of rotation
*/
	 gbyte(in,&something,&skip,24);
	 DPRINT0 ("gds->llg.lRotate\n");
	 gds->llg.lRotate = (long) something;          /* get angle of rotation */
	 
/*
*                 FUNCTION gbyte !get lat pole stretching
*/
	 gbyte(in,&something,&skip,24);
	 DPRINT0 ("Sign & gds->llg.lPole_lat \n");
	 sign = (int)(something >> 23) & 1;                 /* get sign */
	 gds->llg.lPole_lat = (long)something & 8388607; /* lat pole stretching */
	 if(sign)                                           /* negative value */
	   gds->llg.lPole_lat = - gds->llg.lPole_lat;      /* multiply by -1 */

/*
*                 FUNCTION gbyte !get lon pole stretching
*/
	 gbyte(in,&something,&skip,24);
	 DPRINT0 ("Sign & gds->llg.lPole_lon \n");
	 sign = (int)(something >> 23) & 1;                 /* get sign */
	 gds->llg.lPole_lon= (long)(something) & 8388607; /* lon pole stretching*/
	 if(sign)                                           /* negative value */
	   gds->llg.lPole_lon = - gds->llg.lPole_lon;      /* multiply by -1 */
	 
	 gbyte(in,&something,&skip,24);
	 DPRINT0 ("gds->llg.lStretch\n");
	 gds->llg.lStretch = (long) something;
       }

/*
 *                FUNCTION gbyte !get number of columns in each row
 */
       if (gds->llg.usNi == 65535) {
	 if (gds->head.thin == NULL) {
	   gds->head.thin = (int *)malloc(gds->llg.usNj*sizeof(int));
	 } else {
	   gds->head.thin = (int *)realloc(gds->head.thin,
					   gds->llg.usNj*sizeof(int));
	 }
	 if (gds->head.thin == NULL) {
	   sprintf(errmsg,
		   "%s: failed to create array[%d] for thinned grid information", 
		   func, gds->head.thin);
	   goto BYE;
	 }
	 for (i = 0; i<gds->llg.usNj; i++) {
	   gbyte(in,&something,&skip,16);
	   gds->head.thin[i] = (short)something;
	 }
       } else {
	 gds->head.thin = NULL;
       }
       break;

    case MERC_PRJ:    /* Mercator Projection Grid               */
/*
*           case Mercator Projection Grid:
*                 Mark the Projection type 
*                 FUNCTION gbyte !get Number of Columns
*/
       gds->merc.usData_type = gds->head.usData_type;

       gbyte(in,&something,&skip,16); 
       DPRINT0 ("gds->merc.cols\n");
       gds->merc.cols = (int) something;                  /* get Ni */
/*
*                 FUNCTION gbyte !get Number of Rows
*/
       gbyte(in,&something,&skip,16); 
       DPRINT0 ("gds->merc.rows\n");
       gds->merc.rows = (int) something;                  /* get Nj */

/*
*                 FUNCTION gbyte !get Latitude of First Point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->merc.first_lat\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->merc.first_lat = (long) (something) & 8388607;  /* get La1 */
       if(sign)                                             /* negative value */
          gds->merc.first_lat = - gds->merc.first_lat;      /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Longitude of First Point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->merc.first_lon\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->merc.first_lon = (long) (something) & 8388607;  /* get Lo1 */
       if(sign)                                             /* negative value */
          gds->merc.first_lon = - gds->merc.first_lon;      /* multiply by -1 */

/*
*                 FUNCTION gbyte !get resolution & comp flag
*/
       gbyte(in,&something,&skip,8); 
       DPRINT0 ("gds->merc.usRes_flag\n");
       gds->merc.usRes_flag = (short) something;  /* resolution & comp flags */

/*
*                 FUNCTION gbyte !get Latitude of Last point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->merc.La2\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->merc.La2 = (long) (something) & 8388607;        /* get La2 */
       if(sign)                                             /* negative value */
          gds->merc.La2 = - gds->merc.La2;                  /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Longitude of Last point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->merc.Lo2\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->merc.Lo2 = (long) (something) & 8388607;        /* get Lo2 */
       if(sign)                                             /* negative value */
          gds->merc.Lo2 = - gds->merc.Lo2;                  /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Latitude where projection intersects Earth
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->merc.latin\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->merc.latin = (long) (something) & 8388607;      /* get latin */
       if(sign)                                             /* negative value */
          gds->merc.latin = - gds->merc.latin;              /* multiply by -1 */

       skip += 8;      /* skip over the reserved octet */

/*
*                 FUNCTION gbyte !get scanning mode flag
*/
       gbyte(in,&something,&skip,8);
       DPRINT0 ("gds->merc.usScan_mode\n");
       gds->merc.usScan_mode = (short) something;            /* get scaning mode flag */

/*
*                 FUNCTION gbyte !get Longitudinal Increment
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("gds->merc.lon_inc\n");
       gds->merc.lon_inc = (float) something;               /* get Di */

/*
*                 FUNCTION gbyte !get Latitudinal Increment
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("gds->merc.lat_inc\n");
       gds->merc.lat_inc = (float) something;               /* get Dj */

       gbyte(in,&something,&skip,32);
       DPRINT0 ("gds->merc.usZero\n");
       gds->merc.usZero = (long) something;

       if (gds->merc.cols == 65535) {
	 gds->head.thin = (int *)calloc(gds->merc.rows,sizeof(int));
	 if (gds->head.thin == NULL) {
	   sprintf(errmsg,
		   "%s: failed to create array[%d] for thinned grid information", 
		   func, gds->head.thin);
	   goto BYE;
	 }
	 for (i = 0; i<gds->merc.rows; i++) {
	   gbyte(in,&something,&skip,16);
	   gds->head.thin[i] = (short)something;
	 }
       } else {
	 gds->head.thin = NULL;
       }

       break;

    case POLAR_PRJ:    /* Polar Stereographic Projection Grid    */
/*
*           case Polar Stereographic Projection Grid:
*                 Mark the Projection type 
*                 FUNCTION gbyte !get Number of Columns
*/
       gds->pol.usData_type = gds->head.usData_type;

       gbyte(in,&something,&skip,16); 
       DPRINT0 ("gds->pol.usNx\n");
       gds->pol.usNx = (short) something;                   /* get Nx */

/*
*                 FUNCTION gbyte !get Number of Rows
*/
       gbyte(in,&something,&skip,16); 
       DPRINT0 ("gds->pol.usNy\n");
       gds->pol.usNy = (short) something;                   /* get Ny */

/*
*                 FUNCTION gbyte !get Latitude of First point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->pol.lLat1\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->pol.lLat1 = (long)  (something) & 8388607;      /* get La1 */
       if(sign)                                             /* negative value */
          gds->pol.lLat1 = - gds->pol.lLat1;                /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Longitude of First point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->pol.lLon1\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->pol.lLon1 = (long) (something) & 8388607;       /* get Lo1 */
       if(sign)                                             /* negative value */
          gds->pol.lLon1 = - gds->pol.lLon1;                /* multiply by -1 */

/*
*                 FUNCTION gbyte !get resolution & comp flag
*/
       gbyte(in,&something,&skip,8); 
       DPRINT0 ("gds->pol.usRes_flag\n");
       gds->pol.usRes_flag = (short) something;             /* get resolution & comp flags */

/*
*                 FUNCTION gbyte !get Orientation Longitude
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->pol.lLon_orient\n");
       sign = (int)(something >> 23) & 1;                    /* get sign */
       gds->pol.lLon_orient = (long) (something) & 8388607;  /* Orientation */
       if(sign)      /* negative value , multiply by -1 */
          gds->pol.lLon_orient = - gds->pol.lLon_orient;

/*
*                 FUNCTION gbyte !get Increment along a Row
*/
       gbyte(in,&something,&skip,24);
       DPRINT0 ("gds->pol.ulDx\n");
       gds->pol.ulDx = (float) something;                   /* get Dx */

/*
*                 FUNCTION gbyte !get Increment along a Column
*/
       gbyte(in,&something,&skip,24);
       DPRINT0 ("gds->pol.ulDy\n");
       gds->pol.ulDy = (float) something;                   /* get Dy */

/*
*                 FUNCTION gbyte !get projection center flag
*/
       gbyte(in,&something,&skip,8);
       DPRINT0 ("gds->pol.usProj_flag\n");
       gds->pol.usProj_flag = (short) something;  /* Projection center flag */

/*
*                 FUNCTION gbyte !get scanning mode
*/
       gbyte(in,&something,&skip,8);
       DPRINT0 ("gds->pol.usScan_mode\n");
       gds->pol.usScan_mode = (short) something;     /* get scaning mode flag */

/*
*                 FUNCTION gbyte !reserved zero
*/
       gbyte(in,&something,&skip,32);
       DPRINT0 ("gds->pol.usZero\n");
       gds->pol.usZero = (int) something;            /* get Reserved zero */

       if (gds->pol.usNx == 65535) {
	 gds->head.thin = (int *)calloc(gds->pol.usNy,sizeof(int));
	 if (gds->head.thin == NULL) {
	   sprintf(errmsg,
		   "%s: failed to create array[%d] for thinned grid information", 
		   func, gds->head.thin);
	   goto BYE;
	 }
	 for (i = 0; i<gds->pol.usNy; i++) {
	   gbyte(in,&something,&skip,16);
	   gds->head.thin[i] = (short)something;
	 }
       } else {
	 gds->head.thin = NULL;
       }

       break;

   case LAMB_PRJ:  		/* Lambert Conformal */
   case ALBERS_PRJ:  		/* Albers equal-area */
   case OBLIQ_LAMB_PRJ: 	/* Oblique Lambert Conformal */
/*
*           case Lambert conformal, secant or tangent, conical or bipolar:
*           case Albers equal-area, secant or tangent, conical or bipolar:
*           case Oblique Lambert conformal:
*                 Mark the Projection type 
*                 FUNCTION gbyte !get Number of Columns
*/
       gds->lam.usData_type = gds->head.usData_type;

       gbyte(in,&something,&skip,16); 
       DPRINT0 ("gds->lam.iNx\n");
       gds->lam.iNx = (int) something;                      /* get Nx */

/*
*                 FUNCTION gbyte !get Number of Rows
*/
       gbyte(in,&something,&skip,16); 
       DPRINT0 ("gds->lam.iNy\n");
       gds->lam.iNy = (int) something;                      /* get Ny */

/*
*                 FUNCTION gbyte !get Latitude of First Point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->lam.lLat1\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->lam.lLat1 = (long)  (something) & 8388607;      /* get La1 */
       if(sign)                                             /* negative value */
          gds->lam.lLat1 = - gds->lam.lLat1;                /* multiply by -1 */

/*
*                 FUNCTION gbyte !get Longitude of First Point
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->lam.lLon1)\n");
       sign = (int)(something >> 23) & 1;                   /* get sign */
       gds->lam.lLon1 = (long) (something) & 8388607;       /* get Lo1 */
       if(sign)                                             /* negative value */
          gds->lam.lLon1 = - gds->lam.lLon1;                /* multiply by -1 */

/*
*                 FUNCTION gbyte !get resolution & comp flag
*/
       gbyte(in,&something,&skip,8); 
       DPRINT0 ("gds->lam.usRes_flag\n");
       gds->lam.usRes_flag = (short) something;  /* resolution & comp flags */

/*
*                 FUNCTION gbyte !get Orientation Longitude
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->lam.lLon_orient)\n");
       sign = (int)(something >> 23) & 1;                    /* get sign */
       gds->lam.lLon_orient = (long) (something) & 8388607;  /* Orientation */
       if(sign)              /* negative value , multiply by -1 */
          gds->lam.lLon_orient = - gds->lam.lLon_orient;     

/*
*                 FUNCTION gbyte !get Increment along a Row
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("gds->lam.ulDx\n");
       gds->lam.ulDx = (float) something;                    /* get Dx */

/*
*                 FUNCTION gbyte !get Increment along a Column
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("gds->lam.ulDy\n");
       gds->lam.ulDy = (float) something;                    /* get Dy */

/*
*                 FUNCTION gbyte !get Projection Center
*/
       gbyte(in,&something,&skip,8); 
       DPRINT0 ("gds->lam.usProj_flag\n");
       gds->lam.usProj_flag= (short) something;  /* Projection center flag */

/*
*                 FUNCTION gbyte !get scanning mode flag
*/
       gbyte(in,&something,&skip,8); 
       DPRINT0 ("gds->usScan_mode\n");
       gds->lam.usScan_mode = (short) something;    /* get scaning mode flag */

/*
*                 FUNCTION gbyte !get First lat from pole that intersects Earth
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("gds->lLat_cut1\n");
       gds->lam.lLat_cut1 = (long) something;                /* get latin_1 */

/*
*                 FUNCTION gbyte !get Second lat from pole that intersects Earth
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("gds->lLat_cut2\n");
       gds->lam.lLat_cut2 = (long) something;                /* get latin_2 */

/*
*                 FUNCTION gbyte !get lat of south pole
*/
       gbyte(in,&something,&skip,24);
       DPRINT0 ("Sign & gds->lLat_southpole\n");
       sign = (int)(something >> 23) & 1;                        /* get sign */
       gds->lam.lLat_southpole = (long) (something) & 8388607;   /* lat S.pole*/
       if(sign)      /* negative value , multiply by -1 */ 
           gds->lam.lLat_southpole = - gds->lam.lLat_southpole;

/*
*                 FUNCTION gbyte !get lon of South pole
*/
       gbyte(in,&something,&skip,24); 
       DPRINT0 ("Sign & gds->lLon_southpole\n");
       sign = (int)(something >> 23) & 1;                        /* get sign */
       gds->lam.lLon_southpole = (long) (something) & 8388607;/* lon S.pole */
       if(sign)     /* negative value, multiply by -1 */ 
           gds->lam.lLon_southpole = - gds->lam.lLon_southpole;

/*
*                 FUNCTION gbyte !get Reserved zero
*/
       gbyte(in,&something,&skip,16); 
       DPRINT0 ("gds->lam.usZero\n");
       gds->lam.usZero = (int) something;                    /* Reserved zero */

       gds->head.thin = NULL;

       if (gds->lam.iNx == 65535) {
	 gds->head.thin = (int *)calloc(gds->lam.iNy,sizeof(int));
	 if (gds->head.thin == NULL) {
	   sprintf(errmsg,
		   "%s: failed to create array[%d] for thinned grid information", 
		   func, gds->head.thin);
	   goto BYE;
	 }
	 for (i = 0; i<gds->lam.iNy; i++) {
	   gbyte(in,&something,&skip,16);
	   gds->head.thin[i] = (short)something;
	 }
       } else {
	 gds->head.thin = NULL;
       }

       break;

    default :             /* other cases not implemented in this version */
/*
*             default:   ! unsupported data types
*                 SET Status to bad
*/
       DPRINT2 ("%s:  unknown datatype=%d\n",func, gds->head.usData_type);
       sprintf(errmsg,"%s:  unknown datatype=%d\n",func, gds->head.usData_type);
       status=1;         /* set status to failure */
       break;
/*
*
* A.5      ENDSWITCH
*/
  }  /* end switch on data type */

/*
*
* A.6      DEBUG Print
*
* A.7      RETURN (status)
*/
 BYE:
  DPRINT2 ("Exiting %s, stat=%d\n", func,status);
  return(status);
/*
*  END OF FUNCTION 
*
*/
}  
