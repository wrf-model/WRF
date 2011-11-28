/* Program    : make_grib_log (was printer.c)
   Programmer : Todd J. Kienitz, SAIC
   Date       : January 10, 1996
   Purpose    : To produce the information file output of the GRIB message.
   Revisions  : 
   04/17/96 Steve Lowe, SAIC:  modified data print-out
   04/22/96 Alice Nakajima (ATN), SAIC:  added BMS summary
   12/12/96 ATN:  implement combined Decoder/Encdoer structs
	    replaced  (table2 tab2[], table3 tab3[], tables mgotab); 
   06/14/97 ATN:  print upto encoded pricision.
   02/22/98 ATN:  replace projection id with constants, add printing for
     prjns: Rotated Lat/Lon, Stretched Lat/Lon, Stretched Rotated Lat/Lon,
     Rotated Gaussian, Stretched Gauss, Stretched Rotated Gaussian ,
     Oblique Lambert, and for Albers equal-area.
   09/10/98 ATN:  extension flag printing.
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "grib_lookup.h"	/* combined encoder/decoder structs */
#include "dprints.h"		/* for debug printing */
#include "gribfuncs.h"		/* prototypes */

/* 
**********************************************************************
* A. FUNCTION: make_grib_log
*      Produces debug file GRIB.log from the GRIB message in the Grib Header 
*
*    INTERFACE:
*      int   make_grib_log (input_fn, lookup_fn, msg_length, offset,
*                           pds, gds, bds, bms, grib_data, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) char *input_fn;            name of input GRIB file 
*      (I) char *lookup_fn;           name of Lookup file, nil if not used 
*      (I) unsigned long msg_length;  total length of GRIB message
*      (I) long offset;               starting location of GRIB message in bytes
*      (I) PDS_INPUT pds;             product definition section structure
*      (I) grid_desc_sec gds;         grid description section structure
*      (I) BDS_HEAD_INPUT bds;        binary data section header structure
*      (I) BMS_INPUT bms;             bit map definition section structure
*      (I) float *grib_data;          array of decoded data
*
*     ACCESSES GLOBAL VARS:
*        int UseTables;
*            set to one if lookup table used
*        CTRS_DEFN  db_ctr_tbl[NCTRS];
*            predefined array holding Originating Center info
*        PARM_DEFN db_parm_tbl [MAX_PARM_TBLS * NPARM];
*            predefined arr of Parameter info 
*        LVL_DEFN db_lvl_tbl [NLVL];
*            predefined arr of Level info struct
*        MODEL_DEFN db_mdl_tbl [NMODEL];
*            predefined arr of Model info struct
*        GEOM_DEFN db_geom_tbl [NGEOM];
*            predefined arr of Geometry info struct
*
*     RETURN CODE:  
*       0> no errors; file GRIB.log has been created;
*       1> error, errmsg filled;
**********************************************************************
*/
extern int 	  UseTables; 	 /* set means use lookup tbl defns */
extern PARM_DEFN  db_parm_tbl[]; /* parameter conversion info */
extern MODEL_DEFN db_mdl_tbl[];  /* model conversion info */
extern LVL_DEFN   db_lvl_tbl[];  /* level conversion info */
extern GEOM_DEFN  db_geom_tbl[]; /* Geom conversion info  */
extern CTR_DEFN   db_ctr_tbl[];  /* Ctr conversion info  */

#if PROTOTYPE_NEEDED
int   make_grib_log ( 	char *input_fn, 
			char *lookup_fn, 
			unsigned long msg_length, 
			long offset, 
			PDS_INPUT pds, 
			grid_desc_sec gds,
     		       	BDS_HEAD_INPUT bds, 
			BMS_INPUT bms, 
			float *grib_data, 
			char *errmsg)
#else
int   make_grib_log (input_fn, lookup_fn, msg_length, offset,
		     pds, gds, bds, bms, grib_data,errmsg)
	char *input_fn;
	char *lookup_fn; 
	unsigned long msg_length; 
	long offset; 
	PDS_INPUT pds; 
	grid_desc_sec gds;
	BDS_HEAD_INPUT bds; 
	BMS_INPUT bms; 
	float *grib_data; 
	char *errmsg;
#endif

{
  char *func="make_grib_log";
  int i, indx, k, fd, numpts=100;
  float dsf, res, min, max;
  FILE *fp;

/*
*
* A.0   DEBUG printing
*/
  DPRINT1 ("Entering %s\n", func);

/* 
*
* A.1   OPEN file "GRIB.log" in APPEND mode
*/
  fp=fopen ("GRIB.log", "a+");
  if (!fp) {
	DPRINT1("%s:  failed to open 'GRIB.log' for appending, skip logfile\n",
	func); 
	sprintf (errmsg, "%s:  failed to open 'GRIB.log'\n", func); 
	return (1);
	}

/*
*
* A.2   WRITE Indicator Section information to file
*       !message length
*       !GRIB Edition number
*/
  fseek(fp, 0L, 2);
  if (ftell(fp) == 0L)
  fprintf (fp, 	"%s: InFile= %s\n%s: Lookup=%d, fn='%s'\n\n" ,
	   func, input_fn, func,UseTables, lookup_fn);

  fprintf (fp, "**** VALID MESSAGE FOUND AT %ld BYTES ****\n" , offset);

  fprintf(fp, "\n********* SECTION 0 IDS *********\n" );
  fprintf(fp, "Total Message length = %ld\nEdition Number = %d\n",
	   msg_length, pds.usEd_num);
/*
*
* A.3   WRITE Product Definition Section information to file
*       !Section length
*       !Parameter Table version
*       !Parameter Sub-Table version if defined and flagged by Extension flag
*       !Tracking id if defined and flagged by Extension flag
*/
  fprintf(fp, "\n********* SECTION 1 PDS *********\n" \
  	"Section length = %d\nTable version = %d\n",
	 pds.uslength, pds.usParm_tbl);

  if (pds.usExt_flag == (unsigned short)EXTENSION_FLAG ) 
   {
    if (pds.usSub_tbl != 0) 
        fprintf(fp,"Local Table version = %d\n",pds.usSub_tbl);
    if(pds.usTrack_num  != 0) 
	fprintf(fp,"Tracking ID = %d\n",pds.usTrack_num);
   }
   
/*
*       !Originating Center id
*       !IF (using tables) Name of Originating Center
*/
  fprintf(fp,"Originating Center id = %d\n",pds.usCenter_id);
  if (UseTables)
     if ( db_ctr_tbl[pds.usCenter_id].ctr_dsc[0] )
        fprintf(fp,"Originating Center = %s\n",
	db_ctr_tbl[pds.usCenter_id].ctr_dsc);
     else
        fprintf(fp,"Originating Center ID %d not defined in current table.\n",
	pds.usCenter_id);

/*
*       !Sub-Table Entry for Originating Center if non-zero and if
*       !extension flag is set
*/
  if (pds.usExt_flag == (unsigned short)EXTENSION_FLAG && 
	pds.usCenter_sub != 0) 
     fprintf(fp,"Sub-Table Entry Originating Center = %d\n",pds.usCenter_sub);

/*
*       !Extension flag
*/
  fprintf(fp,"Extension flag = %d (extensions %s)\n", pds.usExt_flag,
  (pds.usExt_flag == (unsigned short)EXTENSION_FLAG ? "used" : "not used"));

/*
*       !Model Identification
*       !IF (using tables) Model Description
*/
  fprintf(fp,"Model id = %d\n",pds.usProc_id);
  if (UseTables)
     if ( db_mdl_tbl[pds.usProc_id].grib_dsc[0] )
        fprintf(fp,"Model Description = %s\n",
	db_mdl_tbl[pds.usProc_id].grib_dsc);
     else
        fprintf(fp,"Model ID %d not defined in current table.\n",
	pds.usProc_id);

/*
*       !Grid Identification
*       !IF (using tables) Grid Description
*/
  fprintf(fp,"Grid id = %d\n",pds.usGrid_id);
  if (UseTables) 
     if ( db_geom_tbl[pds.usGrid_id].grib_dsc[0] )
        fprintf(fp,"Grid Description = %s\n",
	db_geom_tbl[pds.usGrid_id].grib_dsc);
     else
        fprintf(fp,"Grid ID %d not defined in current table.\n",
	pds.usGrid_id);

/*
*       !Parameter Identification
*/
  fprintf(fp,"Parameter id = %d\n",pds.usParm_id);

/*
*       !IF (usExt_flag is set AND
*       !    (Parm id between 250 and 254) AND (Sub Parm ID defined)))
*       !    PRINT Parm_sub 
*       !ENDIF
*/
  if (pds.usExt_flag == (unsigned short)EXTENSION_FLAG &&
	  pds.usParm_id>=250 && pds.usParm_id<=254 && pds.usParm_sub!=0)
          fprintf(fp,"Parameter sub-id = %d\n",pds.usParm_sub);


/*
*       !IF (using lookup table) THEN
*       !  CALCULATE index in Parm Conversion Array to use
*       !  let index= (usParm_Id - 249)*256 + usParm_sub;
*       !
*       !   IF this index in Parm Conversion Array is defined THEN
*       !       PRINT its grib_dsc and grib_unit_dsc
*       !   ELSE 
*       !       PRINT it's not defined mesage
*       !   ENDIF
*       !ENDIF
*/
  if(UseTables) 
   {
      indx = PARMTBL_INDX (pds.usParm_id, pds.usParm_sub);

      if ( db_parm_tbl[indx].grib_dsc[0] ) {
         fprintf(fp,"Parameter name = %s\n",db_parm_tbl[indx].grib_dsc);
         fprintf(fp,"Parameter units = %s\n",db_parm_tbl[indx].grib_unit_dsc);
          }
      else fprintf(fp,"Parameter ID %d not defined in current table.\n",
	 pds.usParm_id);
    }

/*
*       !Level Id 
*       !IF (using tables)
*       !  Level description
*       !  SWITCH (number of octets to store Height1)
*       !     2: Level = Height1
*       !     1: Bottom of Layer = Height1
*       !        Top of Layer = Height2
*       !     0: (no Height value required)
*       !     default: (corrupt table entry or message)
*       !  ENDSWITCH
*       !ELSE (not using tables)
*       !  Level = Height1  (Level assumed)
*       !ENDIF
*/
  fprintf(fp,"Level_type = %d\n",pds.usLevel_id);
  if(UseTables) {
    if ( db_lvl_tbl[pds.usLevel_id].grib_dsc[0] ) {
       fprintf(fp,"Level description = %s\n", 
		db_lvl_tbl[pds.usLevel_id].grib_dsc);
       switch(db_lvl_tbl[pds.usLevel_id].num_octets){
         case 2:
           fprintf(fp,"%s = %u\n",
  	   db_lvl_tbl[pds.usLevel_id].lvl_name_1, pds.usHeight1);
           break;
         case 1:
           fprintf(fp,"%s = %u\n%s = %u\n",
  	   db_lvl_tbl[pds.usLevel_id].lvl_name_1, pds.usHeight1,
  	   db_lvl_tbl[pds.usLevel_id].lvl_name_2, pds.usHeight2);
           break;
         case 0:
           break;
         default:
           fprintf(fp,"***Number of octets for table 3 undefined - possibly "
                   "corrupt dataset.***\n");
       }
    }else
       fprintf(fp,"Level ID %d not defined in current table.\n",
	pds.usLevel_id);
  } /* end UseTables 'if' statement */
  else fprintf(fp,"Level = %u\n",pds.usHeight1);

/*
*       !Reference Date/Time:
*       !  Century
*       !  Year
*       !  Month
*       !  Day
*       !  Hour
*       !  Minute
*       !  Second if defined
*/
  fprintf(fp,
     "Reference Date/Time of Data Set:\n" \
     "   Century = %d\n   Year = %d\n   Month = %d\n   Day = %d\n"\
     "   Hour = %d\n   Minute = %d\n",
     pds.usCentury,pds.usYear,pds.usMonth,pds.usDay,pds.usHour,pds.usMinute);

  if(pds.usExt_flag == (unsigned short)EXTENSION_FLAG)
	fprintf(fp,"   Second = %d\n",pds.usSecond);

/*
*       !Forecast Time Unit
*       !  Forecast Period 1
*       !  Forecast Period 2
*/
  switch(pds.usFcst_unit_id){
    case 0: fprintf(fp,"Forecast Time Unit = Minute\n"); break;
    case 1: fprintf(fp,"Forecast Time Unit = Hour\n"); break;
    case 2: fprintf(fp,"Forecast Time Unit = Day\n"); break;
    case 3: fprintf(fp,"Forecast Time Unit = Month\n"); break;
    case 4: fprintf(fp,"Forecast Time Unit = Year\n"); break;
    case 5: fprintf(fp,"Forecast Time Unit = Decade (10 years)\n"); break;
    case 6: fprintf(fp,"Forecast Time Unit = Normal (30 years)\n"); break;
    case 7: fprintf(fp,"Forecast Time Unit = Century (100 years)\n"); break;
    case 254: fprintf(fp,"Forecast Time Unit = Second\n"); break;
    default: fprintf(fp,"Forecast Time Unit = UNDEFINED!!\n");
  }
  fprintf(fp,"   Forecast Period 1 = %d\n",pds.usP1);
  fprintf(fp,"   Forecast Period 2 = %d\n",pds.usP2);

/*
*       !Time Range Indicator
*       !Number in Average
*       !Number Missing
*/
  fprintf(fp,"Time Range = %d\n",pds.usTime_range);
  fprintf(fp,"Number in Average = %d\n",pds.usTime_range_avg);
  fprintf(fp,"Number Missing = %d\n",pds.usTime_range_mis);

/*
*       !Decimal Scale Factor
*/
  fprintf(fp,"Decimal Scale Factor = %d\n",pds.sDec_sc_fctr);

/*
*
* A.4   IF (GDS included) THEN
* A.4.1    WRITE Grid Definition Section information to file
*            !Section length
*            !Parm_nv
*            !Parm_pv_pl
*            !Data type
*/
  if(pds.usGds_bms_id >> 7 & 1) {

     fprintf(fp,"\n********* SECTION 2 GDS *********\n");
     fprintf(fp,"Section length = %d\n",gds.head.uslength);
     fprintf(fp,"Parm_nv = %d\n",gds.head.usNum_v);
     fprintf(fp,"Parm_pv_pl = %d\n",gds.head.usPl_Pv);
     fprintf(fp,"Data_type = %d\n",gds.head.usData_type);
   
/*
* A.4.2    SWITCH (Data Type, Table 6)
*                 !  For each Data Type, write the following to file:
*                 !     Number of points along rows/columns of grid
*                 !     Reference Lat/Lon information
*                 !     Resolution and Component Flags (Table 7)
*                 !       Direction increments if given
*                 !       Assumption of Earth shape
*                 !       U&V component orientation
*                 !     Scanning mode flags (Table 8)
*              Default: Projection not supported, exit;
*/
     fprintf(fp,"Projection = %s\n", prjn_name[gds.head.usData_type]);
     switch(gds.head.usData_type)
     {

/*
*               Case  0: Lat/Lon projection
*               Case 10: Rotated Lat/Lon projection
*               Case 20: Stretched Lat/Lon projection
*               Case 30: Stretched Rotated Lat/Lon projection
*/
    case LATLON_PRJ:    	/* Lat/Lon Grid */
    case ROT_LATLON_PRJ:   	/* Rotated Lat/Lon */
    case STR_LATLON_PRJ:   	/* Stretched Lat/Lon */
    case STR_ROT_LATLON_PRJ :  /* Stretched and Rotated Lat/Lon */

      fprintf(fp,"Number of points along a parallel = %d\n",gds.llg.usNi);
      fprintf(fp,"Number of points along a meridian = %d\n",gds.llg.usNj);
      fprintf(fp,"Latitude of first grid point = %.3f deg\n",
      ((float)gds.llg.lLat1)/1000.);
      fprintf(fp,"Longitude of first grid point = %.3f deg\n",
      ((float)gds.llg.lLon1)/1000.);
      fprintf(fp,"Latitude of last grid point = %.3f deg\n",
      ((float)gds.llg.lLat2)/1000.);
      fprintf(fp,"Longitude of last grid point = %.3f deg\n",
      ((float)gds.llg.lLon2)/1000.);

      fprintf(fp,"Resolution and Component Flags: \n");
      if ((gds.llg.usRes_flag >> 7) & 1) {
         fprintf(fp,"    Longitudinal increment = %f deg\n",
	((float)gds.llg.iDi)/1000.);
         fprintf(fp,"    Latitudinal increment = %f deg\n",
	((float)gds.llg.iDj)/1000.);
      }else fprintf(fp,"    Direction increments not given.\n");
      if ((gds.llg.usRes_flag >> 6) & 1)
           fprintf(fp,"    Earth assumed oblate spherical.\n");
      else fprintf(fp,"    Earth assumed spherical.\n");
      if ((gds.llg.usRes_flag >> 3) & 1)
           fprintf(fp,"    U&V components resolved relative to +I and "
                   "+J\n");
      else fprintf(fp,"    U&V components resolved relative to east "
                   "and north.\n");

      fprintf(fp,"Scanning Mode Flags: \n");
      if ((gds.llg.usScan_mode >> 7) & 1)
           fprintf(fp,"    Points scan in -I direction.\n");
      else fprintf(fp,"    Points scan in +I direction.\n");
      if ((gds.llg.usScan_mode >> 6) & 1)
           fprintf(fp,"    Points scan in +J direction.\n");
      else fprintf(fp,"    Points scan in -J direction.\n");
      if ((gds.llg.usScan_mode >> 5) & 1)
           fprintf(fp,"    Adjacent points in J direction are "
                   "consecutive.\n");
      else fprintf(fp,"    Adjacent points in I direction are "
                   "consecutive.\n");

      /* added 02/98
	This code pertains only to the Stretch/Rotate grids,
	so skip over if it's a LATLON_PRJN type;
	*/
      if (gds.head.usData_type != LATLON_PRJ) 
	{
	fprintf(fp,"    Latitude of southern pole = %.3f deg\n",
	((float)gds.llg.lLat_southpole)/1000.);
	fprintf(fp,"    Longitude of southern pole = %.3f deg\n",
	((float)gds.llg.lLon_southpole)/1000.);

	/* conv from 'saaaaaaa bbbbbbbb bbbbbbbb bbbbbbbb' representation
	   a single precision floating point value */
	fprintf(fp,"    Angle of rotation = %.3f\n", 
	grib_ibm_local ((unsigned long) gds.llg.lRotate));

	fprintf(fp,"    Latitude of pole of stretching = %.3f deg\n",
	((float)gds.llg.lPole_lat)/1000.);
	fprintf(fp,"    Longitude of pole of stretching = %.3f deg\n",
	((float)gds.llg.lPole_lon)/1000.);

	/* conv from 'saaaaaaa bbbbbbbb bbbbbbbb bbbbbbbb' representation
           a single precision floating point value */
	fprintf(fp,"    Stretching factor = %.3f\n", 
	grib_ibm_local ((unsigned long)gds.llg.lStretch));
	}
      break;

/*
*               Case 1: Mercator Projection
*/
    case MERC_PRJ:    /* Mercator Projection Grid    */

      fprintf(fp,"Number of points along a parallel = %d\n",gds.merc.cols);
      fprintf(fp,"Number of points along a meridian = %d\n",gds.merc.rows);
      fprintf(fp,"Latitude of first grid point = %.3f deg\n",
      ((float)gds.merc.first_lat)/1000.);
      fprintf(fp,"Longitude of first grid point = %.3f deg\n",
      ((float)gds.merc.first_lon)/1000.);
      fprintf(fp,"Latitude of last grid point = %.3f deg\n",
      ((float)gds.merc.La2)/1000.);
      fprintf(fp,"Longitude of last grid point = %.3f deg\n",
      ((float)gds.merc.Lo2)/1000.);
      fprintf(fp,"Latitude of intersection with Earth = %.3f deg\n",
      ((float)gds.merc.latin)/1000.);

      fprintf(fp,"Resolution and Component Flags: \n");
      if ((gds.merc.usRes_flag >> 7) & 1) {
         fprintf(fp,"    Longitudinal increment = %f deg\n",
	 ((float)gds.merc.lon_inc)/1000.);
         fprintf(fp,"    Latitudinal increment = %f deg\n",
	 ((float)gds.merc.lat_inc)/1000.);
      }else fprintf(fp,"    Direction increments not given.\n");
      if ((gds.merc.usRes_flag >> 6) & 1)
           fprintf(fp,"    Earth assumed oblate spherical.\n");
      else fprintf(fp,"    Earth assumed spherical.\n");
      if ((gds.merc.usRes_flag >> 3) & 1)
           fprintf(fp,"    U&V components resolved relative to +I and "
                   "+J\n");
      else fprintf(fp,"    U&V components resolved relative to east "
                   "and north.\n");

      fprintf(fp,"Scanning Mode Flags: \n");
      if ((gds.merc.usScan_mode >> 7) & 1)
           fprintf(fp,"    Points scan in -I direction.\n");
      else fprintf(fp,"    Points scan in +I direction.\n");
      if ((gds.merc.usScan_mode >> 6) & 1)
           fprintf(fp,"    Points scan in +J direction.\n");
      else fprintf(fp,"    Points scan in -J direction.\n");
      if ((gds.merc.usScan_mode >> 5) & 1)
           fprintf(fp,"    Adjacent points in J direction are "
                   "consecutive.\n");
      else fprintf(fp,"    Adjacent points in I direction are "
                   "consecutive.\n");
      break;

/*
*               Case  3: Lambert Conformal Projection
*               Case 13: Oblique Lambert Conformal Projection
*               Case  8: Alberts equal-area secant/tangent conic/bipolar Prj
*/
    case LAMB_PRJ:  		/* Lambert Conformal */
    case OBLIQ_LAMB_PRJ:  	/* Oblique Lambert Conformal */
    case ALBERS_PRJ:  		/* Albers equal-area */
                  
      fprintf(fp,"Number of points along X-axis = %d\n",gds.lam.iNx);
      fprintf(fp,"Number of points along Y-axis = %d\n",gds.lam.iNy);
      fprintf(fp,"Latitude of first grid point = %.3f deg\n",
      ((float)gds.lam.lLat1)/1000.);
      fprintf(fp,"Longitude of first grid point = %.3f deg\n",
      ((float)gds.lam.lLon1)/1000.);
      fprintf(fp,"Orientation of grid = %.3f deg\n",
      ((float)gds.lam.lLon_orient)/1000.);
      fprintf(fp,"First Latitude Cut = %.3f deg\n",
      ((float)gds.lam.lLat_cut1)/1000.);
      fprintf(fp,"Second Latitude Cut = %.3f deg\n",
      ((float)gds.lam.lLat_cut2)/1000.);

      fprintf(fp,"Resolution and Component Flags: \n");
      if ((gds.lam.usRes_flag >> 7) & 1) {
            fprintf(fp,"    X-direction increment = %d meters\n",
            gds.lam.ulDx);
            fprintf(fp,"    Y-direction increment = %d meters\n",
            gds.lam.ulDy);
      }else fprintf(fp,"    Direction increments not given.\n");
      if ((gds.lam.usRes_flag >> 6) & 1)
           fprintf(fp,"    Earth assumed oblate spherical.\n");
      else fprintf(fp,"    Earth assumed spherical.\n");
      if ((gds.lam.usRes_flag >> 3) & 1)
           fprintf(fp,"    U&V components resolved relative to +I and "
                   "+J\n");
      else fprintf(fp,"    U&V components resolved relative to east "
                   "and north.\n");

      fprintf(fp,"Scanning Mode Flags: \n");
      if ((gds.lam.usScan_mode >> 7) & 1)
           fprintf(fp,"    Points scan in -I direction.\n");
      else fprintf(fp,"    Points scan in +I direction.\n");
      if ((gds.lam.usScan_mode >> 6) & 1)
           fprintf(fp,"    Points scan in +J direction.\n");
      else fprintf(fp,"    Points scan in -J direction.\n");
      if ((gds.lam.usScan_mode >> 5) & 1)
           fprintf(fp,"    Adjacent points in J direction are "
                   "consecutive.\n");
      else fprintf(fp,"    Adjacent points in I direction are "
                   "consecutive.\n");

      /* 02/98 This code pertains only to the Albers projection */
      if (gds.head.usData_type == ALBERS_PRJ) 
	{
	fprintf(fp,"    Latitude of the southern pole = %.3f\n",
        ((float)gds.lam.lLat_southpole)/1000.);
	fprintf(fp,"    Longitude of the southern pole = %.3f\n",
        ((float)gds.lam.lLon_southpole)/1000.);
      	}
      break;

/*
*               Case  4: Gaussian Lat/Lon Projection
*               Case 14: Rotated Gaussian Lat/Lon Projection
*               Case 24: Stretched Gaussian Lat/Lon Projection
*               Case 34: Stretched Rotated Gaussian Lat/Lon Projection
*/
    case GAUSS_PRJ:    		/* Gaussian Latitude/Longitude Grid */
    case ROT_GAUSS_PRJ:   	/* Rotated Gaussian */
    case STR_GAUSS_PRJ :   	/* Stretched Gaussian */
    case STR_ROT_GAUSS_PRJ :   	/* Stretched and Rotated Gaussian */

      fprintf(fp,"Number of points along a parallel = %d\n",gds.llg.usNi);
      fprintf(fp,"Number of points along a meridian = %d\n",gds.llg.usNj);
      fprintf(fp,"Latitude of first grid point = %.3f deg\n",
      ((float)gds.llg.lLat1)/1000.);
      fprintf(fp,"Longitude of first grid point = %.3f deg\n",
      ((float)gds.llg.lLon1)/1000.);
      fprintf(fp,"Latitude of last grid point = %.3f deg\n",
      ((float)gds.llg.lLat2)/1000.);
      fprintf(fp,"Longitude of last grid point = %.3f deg\n",
      ((float)gds.llg.lLon2)/1000.);

      fprintf(fp,"Resolution and Component Flags: \n");
      if ((gds.llg.usRes_flag >> 7) & 1) {
         fprintf(fp,"    i direction increment = %f deg\n",
	((float)gds.llg.iDi)/1000.);
         fprintf(fp,
	"    Number of parallels between pole and equator = %d\n",
	gds.llg.iDj);
      }else fprintf(fp,"    Direction increments not given.\n");
      if ((gds.llg.usRes_flag >> 6) & 1)
           fprintf(fp,"    Earth assumed oblate spherical.\n");
      else fprintf(fp,"    Earth assumed spherical.\n");
      if ((gds.llg.usRes_flag >> 3) & 1)
           fprintf(fp,"    U&V components resolved relative to +I and "
                   "+J\n");
      else fprintf(fp,"    U&V components resolved relative to east "
                   "and north.\n");

      fprintf(fp,"Scanning Mode Flags: \n");
      if ((gds.llg.usScan_mode >> 7) & 1)
           fprintf(fp,"    Points scan in -I direction.\n");
      else fprintf(fp,"    Points scan in +I direction.\n");
      if ((gds.llg.usScan_mode >> 6) & 1)
           fprintf(fp,"    Points scan in +J direction.\n");
      else fprintf(fp,"    Points scan in -J direction.\n");
      if ((gds.llg.usScan_mode >> 5) & 1)
           fprintf(fp,"    Adjacent points in J direction are "
                   "consecutive.\n");
      else fprintf(fp,"    Adjacent points in I direction are "
                   "consecutive.\n");

      /* added 02/98
	This code pertains only to the Stretch/Rotate grids
	*/
      if (gds.head.usData_type != GAUSS_PRJ) 
	{
	fprintf(fp,"    Latitude of southern pole = %.3f deg\n",
	((float)gds.llg.lLat_southpole)/1000.);
	fprintf(fp,"    Longitude of southern pole = %.3f deg\n",
	((float)gds.llg.lLon_southpole)/1000.);
	
       /* conv from 'saaaaaaa bbbbbbbb bbbbbbbb bbbbbbbb' representation
           a single precision floating point value */
        fprintf(fp,"    Angle of rotation = %.3f\n",
        grib_ibm_local ((unsigned long) gds.llg.lRotate));

	fprintf(fp,"    Latitude of pole of stretching = %.3f deg\n",
	(float)gds.llg.lPole_lat);
	fprintf(fp,"    Longitude of pole of stretching = %.3f deg\n",
	(float)gds.llg.lPole_lon);
	
	/* conv from 'saaaaaaa bbbbbbbb bbbbbbbb bbbbbbbb' representation
           a single precision floating point value */
	fprintf(fp,"    Stretching factor = %.3f\n", 
	grib_ibm_local ((unsigned long)gds.llg.lStretch));
	}
      break;

/*
*               Case 5: Polar Sterographic Projection
*/
    case POLAR_PRJ:    /* Polar Stereographic Projection Grid    */
      fprintf(fp,"Number of points along X-axis = %d\n",gds.pol.usNx);
      fprintf(fp,"Number of points along Y-axis = %d\n",gds.pol.usNy);
      fprintf(fp,"Latitude of first grid point = %.3f deg\n",
	((float)gds.pol.lLat1)/1000.);
      fprintf(fp,"Longitude of first grid point = %.3f deg\n",
	((float)gds.pol.lLon1)/1000.);
      fprintf(fp,"Orientation of grid = %.3f deg\n",
	((float)gds.pol.lLon_orient)/1000.);
      fprintf(fp,"Projection Center: ");
      if ((gds.pol.usProj_flag >> 7) & 1)
           fprintf(fp,"South Pole\n");
      else fprintf(fp,"North Pole\n");

      fprintf(fp,"Resolution and Component Flags: \n");
      if ((gds.pol.usRes_flag >> 7) & 1) {
         fprintf(fp,"    X-direction grid length = %d meters\n",gds.pol.ulDx);
         fprintf(fp,"    Y-direction grid length = %d meters\n",gds.pol.ulDy);
      }else fprintf(fp,"    Direction increments not given.\n");
      if ((gds.pol.usRes_flag >> 6) & 1)
           fprintf(fp,"    Earth assumed oblate spherical.\n");
      else fprintf(fp,"    Earth assumed spherical.\n");
      if ((gds.pol.usRes_flag >> 3) & 1)
           fprintf(fp,"    U&V components resolved relative to +I and "
                   "+J\n");
      else fprintf(fp,"    U&V components resolved relative to east "
                   "and north.\n");

      fprintf(fp,"Scanning Mode Flags: \n");
      if ((gds.pol.usScan_mode >> 7) & 1)
           fprintf(fp,"    Points scan in -I direction.\n");
      else fprintf(fp,"    Points scan in +I direction.\n");
      if ((gds.pol.usScan_mode >> 6) & 1)
           fprintf(fp,"    Points scan in +J direction.\n");
      else fprintf(fp,"    Points scan in -J direction.\n");
      if ((gds.pol.usScan_mode >> 5) & 1)
           fprintf(fp,"    Adjacent points in J direction are "
                   "consecutive.\n");
      else fprintf(fp,"    Adjacent points in I direction are "
                   "consecutive.\n");
      break;

    default:   /* Bad projection:  ignore & continue */ 
      fprintf(stdout, "\n\n***%s WARNING ***:\nProjection %d is INVALID;\n\n",
      func, gds.head.usData_type);

      fprintf(fp,"================================================\n"\
	         "%s:  projection %d is not currently implemented\n"\
      		"================================================\n",
      func, gds.head.usData_type);
      break;

/*
* A.4.2    ENDSWITCH (Data Type)
*/
     } /* Switch */

  } /* gds included */
/*
*
* A.4     ELSE 
*             PRINT no Gds message
* A.4     ENDIF
*/
  else fprintf(fp,"\n******* NO SECTION 2 GDS *********\n" );


/*
*
* A.5   IF (Bitmap Section is present)
*       THEN
*          WRITE Bitmap Section information to file
*       ELSE
*          PRINT no bms mesg
*       ENDIF
*/
  if(pds.usGds_bms_id >> 6 & 1) {
    fprintf(fp,"\n********* SECTION 3 BMS **********\n" );
    fprintf(fp,"Section length = %ld\n", bms.uslength);
    if (bms.uslength <= 6)
      fprintf(fp,"Bitmap is predefined (Not in message).\n");
    else fprintf(fp,"Bitmap is included with message.\n");
    fprintf(fp,"Bitmap ID = %d \n", bms.usBMS_id);
    fprintf(fp,"Number of unused bits = %d\n", bms.usUnused_bits);
    fprintf(fp,"Number of datapoints set = %ld\n", bms.ulbits_set);
  }else{
    fprintf(fp,"\n******* NO SECTION 3 BMS *********\n" );
  }

/*
*
* A.6   WRITE out Binary Data Section Information to file 
*       !Section Length
*/
  fprintf(fp,"\n********* SECTION 4 BDS *********\n" );
  fprintf(fp,"Section length = %ld\n",bds.length);

/*
*       !Table 11 Flags
*/       
  fprintf(fp,"Table 11 Flags:\n");
  if ((bds.usBDS_flag >> 7) & 1)
       fprintf(fp,"    Spherical harmonic coefficients.\n");
  else fprintf(fp,"    Grid-point data.\n");
  if ((bds.usBDS_flag >> 6) & 1)
       fprintf(fp,"    Second-order packing.\n");
  else fprintf(fp,"    Simple Packing.\n");
  if ((bds.usBDS_flag >> 5) & 1)
       fprintf(fp,"    Integer values.\n");
  else fprintf(fp,"    Floating point values.\n");
  if ((bds.usBDS_flag >> 4) & 1)
       fprintf(fp,"    Octet 14 contains additional flag bits.\n");
  else fprintf(fp,"    No additional flags at octet 14.\n");

/*
*       !Decimal Scale Factor (Repeated from PDS)
*/
  fprintf(fp,"\nDecimal Scale Factor = %d\n",pds.sDec_sc_fctr);

/*
*       !Binary Scale Factor
*       !Bit Width
*       !Number of Data Points
*/
  fprintf(fp,"Binary scale factor = %d\n", bds.Bin_sc_fctr);
  fprintf(fp,"Bit width = %d\n", bds.usBit_pack_num);
  fprintf(fp,"Number of data points = %ld\n",bds.ulGrid_size);

/*
* A.6.1   WRITE Data Summary to file
*         !Compute Data Min/Max and Resolution
*/
  dsf = (float) pow( (double) 10, (double) pds.sDec_sc_fctr);
  res = (float) pow((double)2,(double)bds.Bin_sc_fctr) / dsf;
  min = bds.fReference / dsf;
  max = (float) (pow((double)2, (double)bds.usBit_pack_num) - 1);
  max = min + max * res;
  fprintf(fp,"Data Minimum = %f\n", min );
  fprintf(fp,"Data Maximum = %f\n", max );
  fprintf(fp,"Resolution = %f\n",res );

/*
*         !Compute Format Specifier for printing Data
*/
  fd = (int) -1 * (float) log10((double) res) + .5; 
  if (fd <= 0)
  {
    fd = 0;
    fprintf(fp,"DATA will be displayed as integers (res > 0.1).\n");
  }

/*
*         !WRITE First 100 Data Points to file up to Encoded Precision
*/
  if (bds.ulGrid_size > 1) {
  if (bds.ulGrid_size < 100) numpts = bds.ulGrid_size;
  fprintf(fp,"\nDATA ARRAY: (first %d)\n",numpts);
  if (fd > 0)  {
	for (i=0; i<numpts; i=i+5)
	  if (i + 5 < numpts) 
 	      fprintf(fp, "%03d-  %.*f  %.*f  %.*f  %.*f  %.*f\n",
              i,fd,grib_data[i],fd,grib_data[i+1],fd,
              grib_data[i+2],fd,grib_data[i+3],fd,grib_data[i+4] );
	  else  {
	      fprintf(fp,"%03d-", i); 
	      while (i < numpts) fprintf(fp,"  %.*f", fd, grib_data[i++]);
	      fprintf(fp,"\n");
	    }
	}
  else  {
	for (i=0; i<numpts; i=i+5)
	  if (i + 5 < numpts)
    	      fprintf(fp, "%03d-  %.0f  %.0f  %.0f  %.0f  %.0f\n",
              i,grib_data[i],grib_data[i+1],
              grib_data[i+2],grib_data[i+3],grib_data[i+4] );
	  else  {
	      fprintf(fp,"%03d-", i); 
	      while (i < numpts) fprintf(fp,"  %.0f", grib_data[i++]);
	      fprintf(fp,"\n");
	    }
	}
  
  }

  fprintf (fp,"\n******** END OF MESSAGE *******\n\n");

/*
* 
* A.7   CLOSE file
*/
  fclose (fp);

/*
*
* A.8   DEBUG printing
*/
  DPRINT1 ("Leaving  %s, no errors\n", func);
  return (0);

/*
* END OF FUNCTION
*
*
*/
}
