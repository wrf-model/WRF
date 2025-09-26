#include <stdio.h>
#include <stdlib.h>
#include "gribfuncs.h"
/*
**************************************************************************
* A.  FUNCTION:  prt_badmsg
*     Print out as much information as possible from the GRIB message
*     currently in GRIB_HDR structure.  This may be an erroneous or
*     a partial message.
*
*    INTERFACE:
*      int  prt_badmsg (gh, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  GRIB_HDR *gh;
*           pointer to Grib header structure.
*      (O)  char *errmsg;
*           Empty array, Returned filled if error is found;
*
*     RETURN CODE:
*        0> decoded message without any errors; 
*        1> error, errmsg buffer filled;
***********************************************************************
*/
#if PROTOTYPE_NEEDED
int       prt_badmsg (GRIB_HDR *gh, char *errmsg)
#else
int       prt_badmsg (gh, errmsg)
GRIB_HDR  *gh; 	                   /*input= Grib Header struct */
char      *errmsg;                 /* output= empty unless Error happens */
#endif
{
  PDS_INPUT     pds;           /* local PDS struct  */
  grid_desc_sec gds;           /* local GDS struct  */
  BDS_HEAD_INPUT bds_head;     /* local BDS header struct  */
  BMS_INPUT     bms;           /* local bitmap section struct  */
  float         *grib_data;    /* local ptr to float data  */
  PDS_INPUT     *Tpds = NULL;  /* Null until valid PDS found*/
  grid_desc_sec *Tgds = NULL;  /* Null until valid GDS found*/
  BDS_HEAD_INPUT *Tbds= NULL;  /* Null until valid BDS found*/
  BMS_INPUT     *Tbms = NULL;  /* Null until valid BMS found*/
  char *func="prt_badmsg";     /* Function name */
  char *curr_ptr;  		/* pts to beginning of GRIB message */
  unsigned long lMessageSize;       /* message length  */
  unsigned long  edition;                     /* GRIB edition number */
  int bms_flag = 0;                 /* set if Bms present*/
  int gds_flag = 0;                 /* set if Gds present */
  int nReturn = 1;		    /* status, default is bad  */
  unsigned long skip;

/*
*
* A.1     CLEAR out local structures
*/
 fprintf(stdout,"\nEntering %s:  getting info from GRIB Header\n", func);
 memset ((void *)&pds, '\0', sizeof(PDS_INPUT));
 memset ((void *)&gds, '\0', sizeof(grid_desc_sec));
 memset ((void *)&bds_head, '\0', sizeof(BDS_HEAD_INPUT));
 memset ((void *)&bms, '\0', sizeof(BMS_INPUT));
 grib_data = (float *)NULL;

/*
*
* A.2     IF (incoming pointer is not at 'GRIB') 
*            RETURN 1  !errmsg filled
*         ENDIF
*/
curr_ptr = (char *)gh->entire_msg;  
if(strncmp(curr_ptr,"GRIB",4) != 0) {
  sprintf(errmsg,"%s:  no 'GRIB' at beg. of this msg.  Cannot continue.\n", 
  func);
  goto BYE;
 }
fprintf(stdout,"See 'GRIB'\n");

/*
*
* A.3     FUNCTION gbyte   !get total message length from IDS 
*/
skip=32;
gbyte(curr_ptr,&lMessageSize,&skip,24);
if (lMessageSize <= 8) {
	sprintf(errmsg,"Message length too short (%ld), cannot continue\n",
	lMessageSize);
	goto BYE;
	}
fprintf(stdout,"Message Length = %ld\n", lMessageSize);

/*
*
* A.4     PRINT warning message if Message length > Buffer size
*/
if (lMessageSize > gh->abs_size) 
	fprintf(stdout,
	"*** Messagelen (%ld) > buffersize (%ld), MAY BE CORRUPTED ***\n",
	lMessageSize, gh->abs_size);

/*
*
* A.5     EXTRACT the  GRIB edition out of Section 0
*/
gbyte (curr_ptr,&edition, &skip,8);
fprintf(stdout,"Edition = %ld\n", edition);

/*
*
* A.6     MOVE pointer to the Product Definition section
*/
curr_ptr = curr_ptr + 8;
fprintf(stdout,"Expect PDS to start at offset %ld\n", 
(long)curr_ptr - (long)gh->entire_msg);

/*
*
* A.7     FUNCTION gribgetpds  !decode the PDS
*         RETURN error code if fails  !errmsg filled
*         SAVE pointer to PDS block for printing later
*/
if( nReturn= gribgetpds(curr_ptr, &pds, errmsg)) {
   fprintf(stdout,"%s:  %s;\n", func, errmsg);
   goto BYE;
  }
fprintf(stdout,"got PDS\n");
Tpds = &pds;

/*
*
* A.8     PRINT warning if PDS length < 28 bytes
*/
if (pds.uslength < 28) 
	fprintf(stdout,"*** PDS (%ld) < 28 bytes, MAY BE CORRUPTED ***\n",
	pds.uslength);

/*
*
* A.9     MOVE pointer to the end of PDS
*/
curr_ptr += pds.uslength;
if ((long)curr_ptr > (long)gh->entire_msg + (gh->abs_size -1L)) {
     fprintf(stdout,"PDS size is much too big, cannot step past it\n");
     goto BYE;	
   }	
fprintf(stdout,"Expect next section to start at offset %ld\n", 
(long)curr_ptr - (long)gh->entire_msg);

/*
*
* A.10    IF (GDS is present)
*/
if ((gds_flag = pds.usGds_bms_id >> 7 & 1)) 
  {
/*
* A.10.1      FUNCTION gribgetgds   !Exit on error
*             SAVE pointer to GDS block for printing later
*/
   if ((nReturn=gribgetgds(curr_ptr, &gds, errmsg)) != 0) goto BYE;
   fprintf(stdout,"got GDS\n");
   Tgds = &gds;

/*
* A.10.2      SET ulGrid_size based on Projection type
*/
   switch(gds.head.usData_type)
     {
     case LATLON_PRJ:    	/* Lat/Lon Grid */
     case GAUSS_PRJ:    	/* Gaussian Latitude/Longitude grid */
     case ROT_LATLON_PRJ:   	/* Rotated Lat/Lon */
     case ROT_GAUSS_PRJ:   	/* Rotated Gaussian */
     case STR_LATLON_PRJ:   	/* Stretched Lat/Lon */
     case STR_GAUSS_PRJ :   	/* Stretched Gaussian */
     case STR_ROT_LATLON_PRJ :  /* Stretched and Rotated Lat/Lon */
     case STR_ROT_GAUSS_PRJ :   /* Stretched and Rotated Gaussian */
        bds_head.ulGrid_size = gds.llg.usNi * gds.llg.usNj; break;

     case MERC_PRJ:  		/* Mercator Grid */
        bds_head.ulGrid_size = gds.merc.cols * gds.merc.rows; break;

     case LAMB_PRJ:  		/* Lambert Conformal */
     case ALBERS_PRJ:  		/* Albers equal-area */
     case OBLIQ_LAMB_PRJ: 	/* Oblique Lambert Conformal */
        bds_head.ulGrid_size = gds.lam.iNx * gds.lam.iNy; break;

     case POLAR_PRJ:  /* Polar Stereographic */
        bds_head.ulGrid_size = gds.pol.usNx * gds.pol.usNy; break;

     default: 
        fprintf(stdout,"%s: unsupported usData_type=%d\n",
	func, gds.head.usData_type);
        sprintf(errmsg,"%s: unsupported usData_type=%d\n",
        func, gds.head.usData_type);
	nReturn= (1); goto BYE; 
     }

/*
* A.10.3      PRINT warning if GDS length < 32 bytes
*/
   if (gds.head.uslength < 32) 
	fprintf(stdout,"*** GDS (%d bytes) < 32 bytes, MAY BE CORRUPTED ***\n",
	gds.head.uslength);

/*
* A.10.4      MOVE the cursor to the next section (either BMS/BDS)
*/
     curr_ptr += gds.head.uslength;
     if ((long)curr_ptr > (long)gh->entire_msg + (gh->abs_size -1L)) {
	fprintf(stdout,"GDS size is much too big, cannot step past it\n");
	goto BYE;
     }
/*
* A.10     ENDIF (GDS is present)
*/
  } /* gds present */
 else fprintf(stdout,"Flag shows NO Grid Defn Sect included\n");


fprintf(stdout,"Expect next section to start at offset %ld\n", 
(long)curr_ptr - (long)gh->entire_msg);
bms_flag = pds.usGds_bms_id >> 6 & 1;
/*
*
* A.11     IF (bitmap Section is present)
*/
if(bms_flag)  /* bit map section present */
  {
/*
* A.11.1      FUNCTION gribgetbms   !decode BMS
*             RETURN error code if fails  !errmsg filled
*             SAVE pointer to BMS block for printing later
*/
   if( nReturn=
	gribgetbms(curr_ptr,&bms,gds_flag, bds_head.ulGrid_size,errmsg))
   {
     fprintf(stdout,"%s:  error=%d  in grib get BMS;\n",func,nReturn);
     goto BYE;
   }
   fprintf(stdout,"got BMS\n");
   Tbms = &bms;

/*
* A.11.2      PRINT warning if BMS length < 7 bytes
*/
   if (bms.uslength < 7) 
	fprintf(stdout,"*** BMS (%d bytes) < 7 bytes, MAY BE CORRUPTED ***\n",
	bms.uslength);

/*
* A.11.3      MOVE the cursor to beginning of Binary Data Section
*/
     curr_ptr += bms.uslength;
     if ((long)curr_ptr > (long)gh->entire_msg + (gh->abs_size -1L)) {
	fprintf(stdout,"BMS size is much too big, cannot step past it\n");
	goto BYE;
     }
/*
* A.11     ENDIF  !bms present
*/
  } /* Bms present */
 else fprintf(stdout,"Flag shows NO Bit Map Section included\n");


fprintf(stdout,"Expect BDS to start at offset %ld\n", 
(long)curr_ptr - (long)gh->entire_msg);
/*
*
* A.12    FUNCTION  gribgetbds()
*         RETURN error code if failed  !errmsg filled
*         SAVE pointer to BDS for printing later
*/
  if(nReturn=gribgetbds(curr_ptr, pds.sDec_sc_fctr, &bms, &gds, &grib_data,
                &bds_head, errmsg))
   { fprintf(stdout,"%s:  error=%d  in grib get BDS;\n",func,nReturn);
     goto BYE;
    }
  fprintf(stdout,"got BDS\n");
  Tbds= &bds_head;
   
/*
* A.13    PRINT warning if BDS < 11 bytes
*/
   if (bds_head.length < 11) 
	fprintf(stdout,"*** BDS (%d bytes) < 11 bytes, MAY BE CORRUPTED ***\n",
	bds_head.length);

/*
* A.14    BUMP pointer to next section  !return on failure
*/
  curr_ptr += bds_head.length;
     if ((long)curr_ptr > (long)gh->entire_msg + (gh->abs_size -1L)) {
	fprintf(stdout,"BDS size is much too big, cannot step past it\n");
	goto BYE;
     }

/*
* A.15    CHECK for '7777' string 
*         SET return code to 0 if found string 
*/
  fprintf(stdout,"Expect 7777 to start at offset %ld\n", 
  (long)curr_ptr - (long)gh->entire_msg);
  if (strncmp (curr_ptr, "7777", 4))
	fprintf(stdout,"'7777' is NOT at expected location\n");
  else  {
	fprintf(stdout,"see '7777' at offset %ld\n",
	(long)curr_ptr - (long)gh->entire_msg);
  	nReturn = 0;
	}


BYE:
/*
*
* A.16    FUNCTION prt_inp_struct !print as many sections as possible
*/
  fprintf(stdout,"\nNow will print all avail sections=\n");
  prt_inp_struct (Tpds, Tgds, Tbms, Tbds, &grib_data);

/*
*
* A.17    FREE data array
*/
 if (grib_data != NULL) free (grib_data);

/*
*
* A.18    RETURN with exit status
*/
  fprintf(stdout,"Exiting %s\n", func); 
  return (nReturn);
}
