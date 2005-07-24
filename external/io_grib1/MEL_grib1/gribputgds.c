#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include "dprints.h"		/* for dprints */
#include "gribfuncs.h"		/* prototypes */
#include <math.h>

/*
************************************************************************
* A. FUNCTION  gribputgds
*      used to decode Grib's Grid Defn Section.  It returns with both
*      internal structures GDS_HEAD_INPUT and VOID* projblock filled,
*      and also with true GDS already appended to GribHeader's Entire_Msg;
*
*    INTERFACE:
*      int   gribputgds (Geom_In, pGDS_Head_Input, ppvGDS_Proj_Input,
*                        ppgrib_hdr, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I)  GEOM_IN  Geom_In
*           Geometry information used as input;
*      (O)  GDS_HEAD_INPUT *pGDS_Head_Input
*           this is the internal GDS structure.  Attributes (uslength, 
*           usData_type, chData_type, usNum_v and usPl_Pv) gets updated;
*      (O)  void  **ppvGDS_Proj_Input;  
*           This is a pre-alloced storage of type Void and has length of
*           MAX_INP_PROJ_SIZE bytes long.  How this block is filled depends 
*           on the type of Projection it is.  Projections currently supported 
*           are Spherical, Lambert, and Polar Stereographic. 
*     (I&O) GRIB_HDR  **ppgrib_hdr
*           may already have one or more of the other Grib Sections
*           (IDS, PDS, BMS, BDS, or EDS).   Upon successful exit, will
*           also contain a GDS section.
*      (O)  char  *errmsg               
*           empty array, returned filled if error occurred;
*
*      RETURN CODE:
*      0>  no errors;  GDS appended to GRIB_HDR's entire_msg, its info 
*          stored in bds_len & bds_ptr;  msg_length updated too;
*      1>  error, grib hdr is null; errmsg filled;
************************************************************************
*/

#ifdef NCARIBM_NOC99
/*
NCAR AIX does not have lrint, make one up.
*/
long lrint(double value)
{
  long retval;
  retval=(long) rint(value);
  return retval;
}
#endif

#if PROTOTYPE_NEEDED
int   gribputgds ( GEOM_IN  Geom_In, GDS_HEAD_INPUT *pGDS_Head_Input,
		void  **ppvGDS_Proj_Input, GRIB_HDR  **ppgrib_hdr,
		char  *errmsg)
#else
int   gribputgds ( Geom_In, pGDS_Head_Input, ppvGDS_Proj_Input, 
						ppgrib_hdr, errmsg)
		GEOM_IN  Geom_In; 
		GDS_HEAD_INPUT *pGDS_Head_Input;
		void  **ppvGDS_Proj_Input; 
		GRIB_HDR  **ppgrib_hdr;
		char  *errmsg;
#endif
{
/*
* A.0       DEFAULT to err stat 1
*/
  char		  *func= "gribputgds";
  char		  *pgds=0;	 /* true grib, GDS_HEAD + Proj block */
  GDS_HEAD 	  *pGDS_Head=0;	 /* first 6 bytes of PGDS */
  void 		  *pvGDS_Proj=0;  /* projection info, PGDS 7th byte and on... */
  long 		  lProj_sz ;	 /* size of True-Grib projection block  */
  long		  new_msgsz;	 /* size after adding GDS */
  GRIB_HDR	  *gh=0;	 /* temp ptr to struct */
  unsigned char   ucflag;
  int		  tempsz, stat=  1;

GDS_LATLON_INPUT *mp;


  DPRINT0 ("\nEntering  gribputgds .....\n");
/*
*
* A.1       IF (Grib Hdr is null) THEN
*               RETURN error Stat !null ptrs msg in errmsg
*           ENDIF
*/
   gh = *ppgrib_hdr; 
   if (!gh || !gh->entire_msg) {
 	DPRINT1("%s: grib header is null\n", func);
 	sprintf(errmsg,"%s: grib header is null\n", func);
	goto BYE;
	}

/*
*
* A.3       ALLOCATE space for True Grib Structs GDS_HEAD & VOID *proj;
*           IF (fails) THEN
*              RETURN with bad Stat !errmsg filled
*           ELSE  
*              CLEAR out structs
*           ENDIF
*/

   if (! (pgds= (char *) malloc(sizeof (GDS_HEAD) + MAX_PROJ_SIZE))) { 
	DPRINT1 ("%s: MALloced true Grib struct failed\n",func);
	sprintf(errmsg,"%s: MALloced true Grib struct failed\n",func);
	goto BYE; 
	}
   else memset ((void *)pgds, '\0', sizeof(GDS_HEAD) + MAX_PROJ_SIZE);

/*
*
* A.4       ASSIGN (GDS_HEAD *pGDS_Head) to be beginning of local PGDS block 
*           ASSIGN (void *pvGDS_Proj) to byte #7 of local PGDS block
*/
   pGDS_Head  = (GDS_HEAD *) pgds;
   pvGDS_Proj = (void *) (pgds + sizeof(GDS_HEAD));
	 
/*
*
* A.5       INIT some fields of GDS_HEAD & GDS_HEAD_INPUT structs
*/
   pGDS_Head->chNV 	    = ( unsigned char ) 0;
   pGDS_Head->chPV 	    = ( unsigned char ) 255;
   pGDS_Head_Input->usNum_v = 0;    /* INPUT NOT USED AT THIS TIME */
   pGDS_Head_Input->usPl_Pv = 255;    

 
/*
*
*           !now fill true GRIB Grid Defn Sect depending on Projection type
* A.6.a     IF (projection is Spherical) THEN
*/
   if ((strcmp(Geom_In.prjn_name,"spherical")==0) || 
       (strcmp(Geom_In.prjn_name,"gaussian") == 0)){
/*
* A.6.a.1      FUNCTION create_inpLatlon !create internal Latlon struct 
*                                        !using GEOM_IN & USER_INPUT
* A.6.a.2      FUNCTION inp2grib_Latlon  !use internal Latlon struct to 
*                                        !make true Latlon Grib Gds
* A.6.a.3      IF (either failed) THEN
*                  FUNCTION upd_child_errmsg   !tack funcname to errmsg
*                  RETURN with error     !errmsg filled
*              ENDIF
*/

     if ( create_inpLatlon(Geom_In, ppvGDS_Proj_Input,errmsg)
          || inp2grib_Latlon (ppvGDS_Proj_Input, 
		       (LATLON *)(pgds+sizeof(GDS_HEAD)),&lProj_sz, errmsg))
	 { 
	  upd_child_errmsg (func, errmsg); goto BYE; 
	 }

/*
* A.6.a.4      STORE Gds len, DataType=0 into internal GDS struct
*/
     pGDS_Head_Input->uslength = sizeof(GDS_HEAD) + lProj_sz;
     if (strcmp(Geom_In.prjn_name,"spherical")==0) {
       pGDS_Head_Input->usData_type = LATLON_PRJ;
       pGDS_Head->chData_type = 0;
     } else {
       pGDS_Head_Input->usData_type = GAUSS_PRJ;
       pGDS_Head->chData_type = 4;
     }
   }

/*
* A.6.b     ELSE IF (projection is Lambert) THEN
*/
   else if (strcmp(Geom_In.prjn_name,"lambert")==0)
   {
 /*
* A.6.b.1      FUNCTION create_inpLambert !create internal Lambert struct 
*                                         !using GEOM_IN & USER_INPUT
* A.6.b.2      FUNCTION inp2grib_Lambert  !use internal Lambert struct to 
*                                         !make true Lambert Grib Gds
* A.6.b.3      IF (either failed) THEN
*                  FUNCTION upd_child_errmsg   !tack funcname to errmsg
*                  RETURN with error           !errmsg filled
*              ENDIF
*/
    if ( create_inpLambert(Geom_In,ppvGDS_Proj_Input,errmsg)
        || inp2grib_Lambert( ppvGDS_Proj_Input, 
		    (LAMBERT *)(pgds+sizeof(GDS_HEAD)), &lProj_sz, errmsg))
	 { 
	   upd_child_errmsg (func, errmsg); goto BYE; 
	 }

/*
* A.6.b.4      STORE Gds len, DataType=3 into internal GDS struct
*/
     pGDS_Head_Input->uslength = sizeof(GDS_HEAD) + lProj_sz;
     pGDS_Head_Input->usData_type = LAMB_PRJ; 
     pGDS_Head->chData_type = 3;
   }

/*
* A.6.c     ELSE if (projection is Polar_Stereo) THEN
*/
   else if (strcmp(Geom_In.prjn_name,"polar_stereo")==0)
   {
/*
* A.6.c.1      FUNCTION create_inpPolar 
*              !create internal Polar struct using GEOM_IN & USER_INPUT
* A.6.c.2      FUNCTION inp2grib_PolarSt
*              !use internal PolarSt struct to make true PolarSt Grib Gds
* A.6.c.3      IF (either failed) THEN
*                  FUNCTION upd_child_errmsg   !tack funcname to errmsg
*                  RETURN with error           !errmsg filled
*              ENDIF
*/
/* make True Grib PPVGDS_PROJ & SIZE using internal ppvGds_proj_input :  */

     if (create_inpPolar(Geom_In, ppvGDS_Proj_Input,errmsg)
         || inp2grib_PolarSt(ppvGDS_Proj_Input, 
	     	(void *)(pgds+sizeof(GDS_HEAD)),&lProj_sz, errmsg) )
	 { 
	   upd_child_errmsg (func, errmsg); goto BYE; 
	 }

/*
* A.6.c.4      STORE Gds len, DataType=5 into internal GDS struct
*/
     pGDS_Head_Input->uslength = sizeof(GDS_HEAD) + lProj_sz;
     pGDS_Head_Input->usData_type = POLAR_PRJ; 
     pGDS_Head->chData_type = 5;
   }

/*
* A.6.c     ELSE if (projection is Mercator) THEN
*/
   else if (strcmp(Geom_In.prjn_name,"mercator")==0)
   {
/*
* A.6.c.1      FUNCTION create_inpMercator
*              !create internal Mercator struct using GEOM_IN & USER_INPUT
* A.6.c.2      FUNCTION inp2grib_Mercator
*              !use internal Mercator struct to make true PolarSt Grib Gds
* A.6.c.3      IF (either failed) THEN
*                  FUNCTION upd_child_errmsg   !tack funcname to errmsg
*                  RETURN with error           !errmsg filled
*              ENDIF
*/
/* make True Grib PPVGDS_PROJ & SIZE using internal ppvGds_proj_input :  */

     if (create_inpMercator(Geom_In, ppvGDS_Proj_Input,errmsg)
         || inp2grib_Mercator(ppvGDS_Proj_Input, 
	     	(void *)(pgds+sizeof(GDS_HEAD)),&lProj_sz, errmsg) )
	 { 
	   upd_child_errmsg (func, errmsg); goto BYE; 
	 }

/*
* A.6.c.4      STORE Gds len, DataType=5 into internal GDS struct
*/
     pGDS_Head_Input->uslength = sizeof(GDS_HEAD) + lProj_sz;
     pGDS_Head_Input->usData_type = MERC_PRJ; 
     pGDS_Head->chData_type = 1;
   }

/*
* A.6.d     ELSE   ! Projection unknown
*/
   else {
/*
*              RETURN with error           !errmsg filled
*/
     DPRINT2 ("%s: Projection '%s' unknown\n",func,Geom_In.prjn_name);
     sprintf (errmsg,"%s: Projection '%s' unknown\n",func,Geom_In.prjn_name);
     goto BYE;
/*
* A.6.d     ENDIF
*/
   }

/*
*
* A.7       STORE ptr to Gds and its Length in Grib hdr
*/
  gh->gds_ptr     = gh->entire_msg + gh->msg_length;
  gh->gds_len     = sizeof(GDS_HEAD) + lProj_sz;
  DPRINT3 ("Gds length= (%ld + %ld)= %ld \n", 
  sizeof(GDS_HEAD), lProj_sz, gh->gds_len);

/*
*
* A.8       STORE Gds length in the True Grib GDS block too
*/
#if ( BYTE_ORDER == LITTLE_ENDIAN )
  tempsz = (htonl(gh->gds_len)) >> 8;
#else
  tempsz = gh->gds_len << 8;
#endif
  memcpy ((void*)pGDS_Head->achGDS_length, (void *)&tempsz, 3);

/*
*
* A.9       IF gribhdr's buffer is too small AND
*               FUCTION Expand_gribhdr failed 
*           THEN
*               RETURN with error   !errmsg filled
*           ENDIF
*/
  new_msgsz= gh->msg_length + gh->gds_len;

      if (new_msgsz  > gh->abs_size
        && Expand_gribhdr (gh, new_msgsz, errmsg) !=0)
        {
        upd_child_errmsg (func, errmsg);
        goto BYE;
        }

/*
*
* A.10      UPDATE Grib Header Struct
*           !copy true BDS block into Grib Header's Entire_Msg array;
*           !add gds length to Message length 
*/
   memcpy ((void *)gh->gds_ptr, (void *)pgds, gh->gds_len);
   gh->msg_length += gh->gds_len;
   DPRINT1 ("copying %ld bytes from PGDS to gh->GDS_PTR \n", gh->gds_len);
/*
* 
* A.11      CHANGE return Status to no errors
*/
   stat = 0;

BYE:
/*
*
* A.12      FREE up storage
*
* A.13      RETURN Status
*/
   if (pgds) free (pgds);
   DPRINT3 ("Leaving %s, stat=%d, errmsg='%s'\n", func,stat,errmsg);
   return stat;
/*
*
* END OF FUNCTION
*
*
*/
}

/*
*
*********************************************************************
* B. FUNCTION:  create_inpLambert
*       Fills Lambert Projection structure. 
*
*    INTERFACE:
*       int create_inpLambert ( geom_in, ppvGDS_Proj_Input, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*     (I)  GEOM_IN geom_in            Holds info to fill local Lambert block
*     (O)  void **ppvGDS_Proj_Input   pre-allocated block to be filled;
*     (O)  char *errmsg               returns filled if error occurred
*
*     RETURN CODE
*      0> success, ppvGDS_Proj_Input holds Lambert projection information;
*      1> the input pre-MAlloced projection block is null;  errmsg filled;
**********************************************************************/

#if PROTOTYPE_NEEDED
int create_inpLambert (GEOM_IN geom_in, void **ppvGDS_Proj_Input, char *errmsg)
#else
int create_inpLambert (geom_in, ppvGDS_Proj_Input, errmsg)
		GEOM_IN geom_in; void **ppvGDS_Proj_Input; char *errmsg;
#endif
{
char *func= "create_inpLambert";
/*
*
* B.1       DEFAULT status of 0
*/
   int            nStatus = 0;
   double	  cut1, cut2, tmp;
   GDS_LAM_INPUT  *pGDS_Lam_Input;

   DPRINT1 ( "   Entering %s.....\n", func );
/*
*
* B.2       IF (incoming projection block is Null)
*           THEN
*              FILL errmsg
*              SET return status to error
*/
   if (!(pGDS_Lam_Input= (GDS_LAM_INPUT *) *ppvGDS_Proj_Input) )
     {
      DPRINT1 ( "%s: ppvGDS_Proj_Input is null\n", func);
      sprintf(errmsg, "%s: ppvGDS_Proj_Input is null\n", func);
      nStatus = 1;
      }
/*
* B.2.b     ELSE
*               USE info from GEOM_IN to fill the Lambert GDS struct
*/
   else
   {
   pGDS_Lam_Input->usData_type = LAMB_PRJ; /* data type flag (Tbl  ) */
   pGDS_Lam_Input->iNx = (int) geom_in.nx; /* #pts along x-axis */
   pGDS_Lam_Input->iNy = (int) geom_in.ny;/* #pts along y-axis */  
  /* latitude & lon of 1st grid point */
   pGDS_Lam_Input->lLat1 = lrint(geom_in.first_lat *1000.); 
   pGDS_Lam_Input->lLon1 = lrint((geom_in.first_lon) *1000.); 
   pGDS_Lam_Input->usRes_flag = geom_in.usRes_flag;/*Resolution flags Tbl7*/
   pGDS_Lam_Input->lLon_orient=lrint(geom_in.parm_3 *1000.);/*grid orient */
   pGDS_Lam_Input->ulDx=(unsigned long)lrint(geom_in.x_int_dis*1000.);/*Xdir gridlen*/
   pGDS_Lam_Input->ulDy=(unsigned long)lrint(geom_in.y_int_dis*1000.);/*Ydir gridlen*/
   if ((geom_in.y_int_dis != 0) && (geom_in.x_int_dis != 0))
         pGDS_Lam_Input->usRes_flag = pGDS_Lam_Input->usRes_flag + 0x80;
   if (geom_in.parm_1 > 0) 
     pGDS_Lam_Input->usProj_flag = 0;              /* projection flag */
   else
     pGDS_Lam_Input->usProj_flag = 1<<7;              /* projection flag */
   pGDS_Lam_Input->usScan_mode = geom_in.scan; /* order of grid points (Tbl8)*/
  /*  Make sure CUT1 is closest to Pole  */
   cut1 = geom_in.parm_1;
   cut2 = geom_in.parm_2;
   if (cut1 >= 0.) {
      if (cut2 > cut1) { tmp = cut1; cut1 = cut2; cut2 = tmp; }
      }
   else {
      if (cut2 < cut1) { tmp = cut1; cut1 = cut2; cut2 = tmp; }
      }
    
   pGDS_Lam_Input->lLat_cut1=lrint(cut1 *1000.);/* 1stlat fr pole secant cuts*/
   pGDS_Lam_Input->lLat_cut2=lrint(cut2 *1000.);/* 2ndlat fr pole secant cuts*/
   pGDS_Lam_Input->lLat_southpole = 0; /* lat of southern pole (millidegrees) */
   pGDS_Lam_Input->lLon_southpole = 0; /* lon of souther pole (millidegrees) */
   pGDS_Lam_Input->usZero = 0;         /* filler zeroes */

/*
*               DEBUG print
*/
   DPRINT3("\t%s: usData_type = %u (%s)\n",
   func,pGDS_Lam_Input->usData_type, prjn_name[pGDS_Lam_Input->usData_type] );
   DPRINT2("\t%s: iNx = %d\n", func,pGDS_Lam_Input->iNx );
   DPRINT2("\t%s: iNy = %d\n", func,pGDS_Lam_Input->iNy );
   DPRINT2("\t%s: lLat1 = %d\n", func,pGDS_Lam_Input->lLat1 );
   DPRINT2("\t%s: lLon1 = %d\n", func,pGDS_Lam_Input->lLon1 );
   DPRINT2("\t%s: lLon_orient = %d\n", func,pGDS_Lam_Input->lLon_orient);
   DPRINT2("\t%s: ulDx = %u\n", func, pGDS_Lam_Input->ulDx );
   DPRINT2("\t%s: ulDy = %u\n", func, pGDS_Lam_Input->ulDy );
   DPRINT2("\t%s: lLat_cut1 = %d\n", func, pGDS_Lam_Input->lLat_cut1);
   DPRINT2("\t%s: lLat_cut2 = %d\n", func, pGDS_Lam_Input->lLat_cut2);
   DPRINT2("\t%s: usRes_flag = %u\n", func,pGDS_Lam_Input->usRes_flag);
   DPRINT2("\t%s: usProj_flag = %u\n", func,pGDS_Lam_Input->usProj_flag);
   DPRINT2("\t%s: usScan_mode = %u\n", func,pGDS_Lam_Input->usScan_mode);
/*
* B.2       ENDIF
*/
   }

   DPRINT1("   Exiting %s.......\n" ,func);  
/*
*
* B.3       RETURN status
*/
   return ( nStatus );
/*
*
* END OF FUNCTION
*
*
*/
}

/*
*
*********************************************************************
* C. FUNCTION:  create_inpPolar
*      Fills Polar Stereographic Projection structure.  
*
*    INTERFACE:
*      int create_inpPolar (geom_in, ppvGDS_Proj_Input, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) GEOM_IN geom_in          holds info to fill local Polar block
*      (O) void **ppvGDS_Proj_Input  block to filled with Polar Stereo info
*      (O) char *errmsg              empty array filled if error occurs
*
*    RETURN CODE:
*       0> success, ppvGDS_Proj_Input holds Polar projection info;
*       1> the input pre-Malloced projection block is null; errmsg filled;
**********************************************************************/

#if PROTOTYPE_NEEDED
int create_inpPolar (GEOM_IN geom_in, void **ppvGDS_Proj_Input, char *errmsg)
#else
int create_inpPolar (geom_in, ppvGDS_Proj_Input, errmsg)
		GEOM_IN geom_in; void **ppvGDS_Proj_Input; char *errmsg;
#endif
{
char	*func="create_inpPolar";
/*
*
* C.1       DEFAULT status of 0
*/
   GDS_PS_INPUT   *pGDS_PS_Input;
   int            nStatus = 0;

   DPRINT1 ("   Entering %s.....\n",func );
/*
*
* C.2       IF (incoming projection block is Null)
* C.2.a     THEN
*              FILL errmsg
*              CHANGE return Status to error
*/
   if (!(pGDS_PS_Input= (GDS_PS_INPUT *) *ppvGDS_Proj_Input) )
     {
      DPRINT1  ("%s: ppvGDS_Proj_Input is null\n", func);
      sprintf(errmsg,"%s: ppvGDS_Proj_Input is null\n", func);
      nStatus = 1;
      }

   else {
/* 
* C.2.b     ELSE
* C.2.b.1      FILL elements of Polar Stereo structure 
*/
   pGDS_PS_Input->usData_type = POLAR_PRJ;   /* data type flag (Tbl  ) */
   pGDS_PS_Input->usNx = (unsigned short) geom_in.nx;/* #pts along x-axis*/
   pGDS_PS_Input->usNy = (unsigned short) geom_in.ny;/* #pts along y-axiz*/  
   pGDS_PS_Input->lLat1 = lrint(geom_in.first_lat *1000.);/*lat of 1st gridpt*/
   pGDS_PS_Input->lLon1 = lrint(geom_in.first_lon *1000.);/*lon of 1st gridpt*/
   pGDS_PS_Input->usRes_flag = geom_in.usRes_flag;/* resolution flags Tbl7 */
   pGDS_PS_Input->lLon_orient = lrint(geom_in.parm_2 *1000.);/*grid orient*/
   pGDS_PS_Input->ulDx=(unsigned long)lrint(geom_in.x_int_dis*1000.);/*Xdir gridlen*/
   pGDS_PS_Input->ulDy=(unsigned long)lrint(geom_in.y_int_dis*1000.);/*Ydir gridlen*/
   if ((geom_in.y_int_dis != 0) && (geom_in.x_int_dis != 0))
         pGDS_PS_Input->usRes_flag = pGDS_PS_Input->usRes_flag + 0x80;
   if (geom_in.first_lat > 0) 
     pGDS_PS_Input->usProj_flag = 0;  /* projection flag */
   else 
     pGDS_PS_Input->usProj_flag = 1<<7;  /* projection flag */

   pGDS_PS_Input->usScan_mode = geom_in.scan; /* order of grid points (Tbl 8) */
   pGDS_PS_Input->usZero = 0;       /* filler zeroes */

/* 
* C.2.b.2      DEBUG print
*/
   DPRINT3 ("\t%s: usData_type = %u (%s)\n",func,pGDS_PS_Input->usData_type,
   prjn_name [pGDS_PS_Input->usData_type] );
   DPRINT2("\t%s: usNx = %u\n", func,pGDS_PS_Input->usNx );
   DPRINT2("\t%s: usNy = %u\n", func,pGDS_PS_Input->usNy );
   DPRINT2("\t%s: lLat1 = %d\n", func,pGDS_PS_Input->lLat1 );
   DPRINT2("\t%s: lLon1 = %d\n", func,pGDS_PS_Input->lLon1 );
   DPRINT2("\t%s: lLon_orient = %d\n", func,pGDS_PS_Input->lLon_orient);
   DPRINT2("\t%s: ulDx = %u\n", func,pGDS_PS_Input->ulDx);
   DPRINT2("\t%s: ulDy = %u\n", func,pGDS_PS_Input->ulDy);
   DPRINT2("\t%s: usRes_flag = %u\n", func,pGDS_PS_Input->usRes_flag);
   DPRINT2("\t%s: usProj_flag = %u\n", func,pGDS_PS_Input->usProj_flag);
   DPRINT2("\t%s: usScan_mode = %u\n", func,pGDS_PS_Input->usScan_mode);
/*
* C.2.b     ENDIF
*/
   }

   DPRINT1("   Exiting %s.......\n" ,func);  
/*
*
* C.3       RETURN status
*/
   return ( nStatus );
/*
*
* END OF FUNCTION
*
*
*/
}


/*
*
*********************************************************************
* D. FUNCTION:  create_inpLatlon
*       Fills Latitude Longitude Projection structure.  
*
*    INTERFACE:
*       int create_inpLatlon ( geom_in, ppvGDS_Proj_Input, errmsg) 
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) GEOM_IN geom_in           holds geom info to fill local Lat/Lon block
*      (O) void **ppvGDS_Proj_Input  to be filled with LatLon projection info
*      (O) char *errmsg              empty array, filled if error occurred
*
*     OUTPUT:
*       0> success, ppGDS_Proj_Input filled with Lat/Lon projection info
*       1> pre-Malloced Projection block is null;  errmsg filled;
**********************************************************************/

#if PROTOTYPE_NEEDED
int create_inpLatlon (GEOM_IN geom_in, void **ppvGDS_Proj_Input, char *errmsg)
#else
int create_inpLatlon (geom_in, ppvGDS_Proj_Input, errmsg)
		GEOM_IN geom_in; void **ppvGDS_Proj_Input; char *errmsg;
#endif
{
char *func= "Create_InpLatLon";
/*
*
* D.0       DEFAULT to return status of 0
*/
   GDS_LATLON_INPUT  *pGDS_Latlon_Input;
   int            nStatus = 0;

   DPRINT1 ("  Entering %s......\n" ,func);
/*
*
* D.2       IF (incoming projection block is Null)
*           THEN
*              FILL errmsg
*              CHANGE stat to 1
*/
   if (!(pGDS_Latlon_Input= (GDS_LATLON_INPUT *) *ppvGDS_Proj_Input) )
     {
      DPRINT1 (" %s: ppvGDS_Proj_Input is null\n", func);
      sprintf(errmsg," %s: ppvGDS_Proj_Input is null\n", func);
      nStatus = 1; 
	} 
/* 
* D.2.b     ELSE 
* D.2.b.1      FILL elements of the Lat/Lon GDS block 
*/
   else
   {
   
   pGDS_Latlon_Input->usData_type = LATLON_PRJ;  /* data type flag (Tbl  )*/
   pGDS_Latlon_Input->usNi=(unsigned short)geom_in.nx;/*#pts along x-axis 109*/
   pGDS_Latlon_Input->usNj=(unsigned short)geom_in.ny;/* #pts along y-axiz 82*/
   pGDS_Latlon_Input->lLat1=lrint(geom_in.first_lat*1000.);/*lat of 1stgridpt*/
   pGDS_Latlon_Input->lLon1=lrint(geom_in.first_lon*1000.);/*lon of 1stgridpt*/
   pGDS_Latlon_Input->usRes_flag=geom_in.usRes_flag;/*resolution flags Tbl7*/
   pGDS_Latlon_Input->lLat2 =lrint(geom_in.last_lat*1000.);/*lat of 2ndgridpt*/
   pGDS_Latlon_Input->lLon2 =lrint(geom_in.last_lon*1000.);/*lon of 2ndgridpt*/
   pGDS_Latlon_Input->iDi = lrint(geom_in.parm_2 *1000.);/* i-dir incr*/
   pGDS_Latlon_Input->iDj = lrint(geom_in.parm_1 *1000.);/* j-dir incr*/
   if ((geom_in.parm_1 != 0) && (geom_in.parm_2 != 0))
         pGDS_Latlon_Input->usRes_flag = pGDS_Latlon_Input->usRes_flag + 0x80;
   pGDS_Latlon_Input->usScan_mode = geom_in.scan; /* order ofgridpts (Tbl 8)*/
   pGDS_Latlon_Input->usZero = 0;  /* filler zeroes*/
   pGDS_Latlon_Input->lLat_southpole= 0;/* lat of southern pole (millidegrees)*/
   pGDS_Latlon_Input->lLon_southpole= 0;/* lon of southern pole (millidegrees)*/
   pGDS_Latlon_Input->lRotate = 0;/* angle of rotation*/
   pGDS_Latlon_Input->lPole_lat = 0;/* lat of pole of stretching (mdeg)*/
   pGDS_Latlon_Input->lPole_lon = 0; /* lon of pole of stretching*/
   pGDS_Latlon_Input->lStretch = 0;/* stretching factor*/

/*
* D.2.b.2      DEBUG print
*/
   DPRINT3("\t%s: usData_type = %u (%s)\n", func,pGDS_Latlon_Input->usData_type,
	prjn_name[pGDS_Latlon_Input->usData_type] );
   DPRINT2("\t%s: usNi = %u\n",func,pGDS_Latlon_Input->usNi );
   DPRINT2("\t%s: usNj = %u\n",func,pGDS_Latlon_Input->usNj );
   DPRINT2("\t%s: lLat1 = %d\n",func,pGDS_Latlon_Input->lLat1 );
   DPRINT2("\t%s: lLon1 = %d\n",func,pGDS_Latlon_Input->lLon1 );
   DPRINT2("\t%s: lLat2 = %d\n",func,pGDS_Latlon_Input->lLat2 );
   DPRINT2("\t%s: lLon2 = %d\n",func,pGDS_Latlon_Input->lLon2 );
   DPRINT2("\t%s: iDi = %u\n",func,pGDS_Latlon_Input->iDi );
   DPRINT2("\t%s: iDj = %u\n",func,pGDS_Latlon_Input->iDj );
   DPRINT2("\t%s: usRes_flag = %u\n",func,pGDS_Latlon_Input->usRes_flag );
   DPRINT2("\t%s: usScan_mode = %u\n",func,pGDS_Latlon_Input->usScan_mode );
   DPRINT2("\t%s: lLat_southpole = %ld\n",func,pGDS_Latlon_Input->lLat_southpole);
   DPRINT2("\t%s: lLon_southpole = %ld\n",func,pGDS_Latlon_Input->lLon_southpole);
   DPRINT2("\t%s: lRotate = %ld\n",func,pGDS_Latlon_Input->lRotate );
   DPRINT2("\t%s: lPole_lat = %ld\n",func,pGDS_Latlon_Input->lPole_lat );
   DPRINT2("\t%s: lPole_lon = %ld\n",func,pGDS_Latlon_Input->lPole_lon );
   DPRINT2("\t%s: lStretch = %ld\n",func,pGDS_Latlon_Input->lStretch );
/*
* D.2.b    ENDIF
*/
   }
/*
*
* D.3      RET2URN status
*/
   DPRINT1("   Exiting %s.......\n" ,func);  
   return ( nStatus );
/*
* END OF FUNCTION
*
*/ 
}

/*
*
*********************************************************************
* FUNCTION:  create_inpMercator
*      Fills Mercator Projection structure.  
*
*    INTERFACE:
*      int create_inpMercator (geom_in, ppvGDS_Proj_Input, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) GEOM_IN geom_in          holds info to fill local Mercator block
*      (O) void **ppvGDS_Proj_Input  block to filled with Mercator info
*      (O) char *errmsg              empty array filled if error occurs
*
*    RETURN CODE:
*       0> success, ppvGDS_Proj_Input holds Mercator projection info;
*       1> the input pre-Malloced projection block is null; errmsg filled;
**********************************************************************/

#if PROTOTYPE_NEEDED
int create_inpMercator (GEOM_IN geom_in, void **ppvGDS_Proj_Input, char *errmsg)
#else
int create_inpMercator (geom_in, ppvGDS_Proj_Input, errmsg)
		GEOM_IN geom_in; void **ppvGDS_Proj_Input; char *errmsg;
#endif
{
char	*func="create_inpMercator";
/*
*
* C.1       DEFAULT status of 0
*/
   mercator       *pGDS_mercator_Input;
   int            nStatus = 0;

   DPRINT1 ("   Entering %s.....\n",func );
/*
*
* C.2       IF (incoming projection block is Null)
* C.2.a     THEN
*              FILL errmsg
*              CHANGE return Status to error
*/
   if (!(pGDS_mercator_Input= (mercator *) *ppvGDS_Proj_Input) )
     {
      DPRINT1  ("%s: ppvGDS_Proj_Input is null\n", func);
      sprintf(errmsg,"%s: ppvGDS_Proj_Input is null\n", func);
      nStatus = 1;
      }

   else {
/* 
* C.2.b     ELSE
* C.2.b.1      FILL elements of Polar Stereo structure 
*/
   pGDS_mercator_Input->usData_type = MERC_PRJ;   /* data type flag (Tbl  ) */
   pGDS_mercator_Input->cols = (unsigned short) geom_in.nx;/* #pts along x-axis*/
   pGDS_mercator_Input->rows = (unsigned short) geom_in.ny;/* #pts along y-axiz*/  
   pGDS_mercator_Input->first_lat = lrint(geom_in.first_lat *1000.);/*lat of 1st gridpt*/
   pGDS_mercator_Input->first_lon = lrint(geom_in.first_lon *1000.);/*lon of 1st gridpt*/
   pGDS_mercator_Input->usRes_flag = geom_in.usRes_flag;/* resolution flags Tbl7 */
   pGDS_mercator_Input->La2 = lrint(geom_in.last_lat *1000.);/*lat of last gridpt*/
   pGDS_mercator_Input->Lo2 = lrint(geom_in.last_lon *1000.);/*lon of last gridpt*/
   pGDS_mercator_Input->latin = lrint(geom_in.parm_1 *1000.);/*reference latitude*/
   pGDS_mercator_Input->usZero1 = 0; /* filler zeroes */
   pGDS_mercator_Input->usScan_mode = geom_in.scan; /* order of grid points (Tbl 8) */
   pGDS_mercator_Input->lon_inc = lrint(geom_in.parm_2 *1000.);/*longitude increment*/
   pGDS_mercator_Input->lat_inc = lrint(geom_in.parm_3 *1000.);/*latitude increment*/
   pGDS_mercator_Input->usZero = 0;  /* filler zeroes */

/* 
* C.2.b.2      DEBUG print
*/
   DPRINT3 ("\t%s: usData_type = %u (%s)\n",func,pGDS_mercator_Input->usData_type,
   prjn_name [pGDS_mercator_Input->usData_type] );
   DPRINT2("\t%s: cols = %u\n", func,pGDS_mercator_Input->cols );
   DPRINT2("\t%s: rows = %u\n", func,pGDS_mercator_Input->rows );
   DPRINT2("\t%s: first_lat = %d\n", func,pGDS_mercator_Input->first_lat );
   DPRINT2("\t%s: first_lon = %d\n", func,pGDS_mercator_Input->first_lon );
   DPRINT2("\t%s: usRes_flag = %d\n", func,pGDS_mercator_Input->usRes_flag);
   DPRINT2("\t%s: La2 = %d\n", func,pGDS_mercator_Input->La2);
   DPRINT2("\t%s: Lo2 = %d\n", func,pGDS_mercator_Input->Lo2);
   DPRINT2("\t%s: latin = %d\n", func,pGDS_mercator_Input->latin);
   DPRINT2("\t%s: usZero1 = %d\n", func,pGDS_mercator_Input->usZero1);
   DPRINT2("\t%s: usScan_mode = %d\n", func,pGDS_mercator_Input->usScan_mode);
   DPRINT2("\t%s: lon_inc = %f\n", func,pGDS_mercator_Input->lon_inc);
   DPRINT2("\t%s: lat_inc = %f\n", func,pGDS_mercator_Input->lat_inc);
/*
* C.2.b     ENDIF
*/
   }

   DPRINT1("   Exiting %s.......\n" ,func);  
/*
*
* C.3       RETURN status
*/
   return ( nStatus );
/*
*
* END OF FUNCTION
*
*
*/
}








/*
*
****************************************************************************
* E. FUNCTION:  inp2gribLambert
*      This routine fills the special Lambert Projection structure for
*      the GDS.
*
*    INTERFACE:
*       int inp2grib_Lambert (ppvGDS_Proj_Input, pLambert, lProj_size, errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) void  **ppvGDS_Proj_Input; 
*          pointer to struct holds Input Projection data 
*      (O) LAMBERT *pLambert;
*          block to be filled with Lambert Projection information
*      (O) long  *lProj_size;
*          to be filled with size of LAMBERT struct;
*      (O) char *errmsg;
*          empty array, filled if error occurred;
*
*    RETURN CODE:
*      0> success,  pLambert and lProj_size filled;
*      1> got null pointers; errmsg filled;
****************************************************************************/

#if PROTOTYPE_NEEDED
int   inp2grib_Lambert (void **ppvGDS_Proj_Input, LAMBERT  *pLambert,
			long *lProj_size, char *errmsg)
#else
int   inp2grib_Lambert (ppvGDS_Proj_Input, pLambert, lProj_size, errmsg)
			void **ppvGDS_Proj_Input; LAMBERT  *pLambert;
			long *lProj_size; char *errmsg;
#endif
{
/*
* E.1       INIT status to success
*
* E.2       DEBUG printing
*/
   GDS_LAM_INPUT     *vProjInp = 0;
   int               lTemp = 0;
   int               nStatus = 0;
   char		     *func= "inp2grib_Lambert";
   long              tmp_byte4;
   DPRINT1 ("   Entering %s.....\n",func);

/*
* 
* E.3       MAKE local ptr vProjInp point to Input Projection data block arg
*/
   vProjInp = ( GDS_LAM_INPUT * ) *ppvGDS_Proj_Input;    /* read fr this */

/*
* 
* E.4       IF (either of the user's struct pointers are NUL) THEN
*              SET status = 1
*              RETURN
*           ENDIF
*/
   if (!vProjInp || !pLambert)   {
	DPRINT1 ("%s: the VOID *ppvGDS_Proj_Input block is null\n",func);
	sprintf(errmsg, "%s: the VOID *ppvGDS_Proj_Input block is null\n",func);
	nStatus=  1; 
	goto BYE; 
	}

/*
* E.5       FILL local block type LAMBERT
*/
  
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(vProjInp->iNx)) >> 16;
#else
      lTemp = vProjInp->iNx << 16;
#endif
      memcpy ((void *) pLambert->achNx, (void *)&lTemp, 2);
      
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(vProjInp->iNy) >> 16);
#else
      lTemp = vProjInp->iNy << 16;
#endif
      memcpy ((void *) pLambert->achNy, (void *)&lTemp, 2);

/* convert lLat1 to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(vProjInp->lLat1))) >> 8;
#else
      lTemp = abs ( vProjInp->lLat1 ) << 8;
#endif
      memcpy ((void *)  pLambert->achLat1, (void *)&lTemp, 3 );
      if ( vProjInp->lLat1 < 0 ) pLambert->achLat1[0] |= 0x0080;

/* convert lLon1 to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(vProjInp->lLon1))) >> 8;
#else
      lTemp = abs ( vProjInp->lLon1 ) << 8;
#endif
      memcpy ((void *) pLambert->achLon1, (void *)&lTemp, 3 );
      if ( vProjInp->lLon1 < 0 ) pLambert->achLon1[0] |= 0x0080;

      pLambert->chRes_flag = ( unsigned char ) vProjInp->usRes_flag;

/* convert lLon_orient to 3 bytes */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(vProjInp->lLon_orient))) >> 8;
#else
      lTemp = abs(vProjInp->lLon_orient) << 8;
#endif
      memcpy ((void *) pLambert->achLon_orient, (void *)&lTemp, 3 );
      if ( vProjInp->lLon_orient < 0 ) pLambert->achLon_orient[0] |= 0x0080;

/* convert ulDx to 3 bytes */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(vProjInp->ulDx)) >> 8;
#else
      lTemp=   vProjInp->ulDx  << 8;
#endif
      memcpy ((void *) pLambert->achDx, (void *)&lTemp, 3 );
   
/* convert ulDy to 3 bytes */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(vProjInp->ulDy)) >> 8;
#else
      lTemp= vProjInp->ulDy  << 8;
#endif
      memcpy ((void *) pLambert->achDy, (void *)&lTemp, 3 );

      pLambert->chProj_flag = ( unsigned char ) vProjInp->usProj_flag;
      pLambert->chScan_mode = ( unsigned char ) vProjInp->usScan_mode;

/* convert lLat_cut1 to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(vProjInp->lLat_cut1))) >> 8;
#else
      lTemp = abs (vProjInp->lLat_cut1) << 8;
#endif
      memcpy ((void *) pLambert->achLat_cut1, (void *)&lTemp, 3 );
      if ( vProjInp->lLat_cut1 < 0 ) pLambert->achLat_cut1[0] |= 0x0080;

/* convert lLat_cut2 to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(vProjInp->lLat_cut2))) >> 8;
#else
      lTemp = abs ( vProjInp->lLat_cut2 ) << 8;
#endif
      memcpy ((void *) pLambert->achLat_cut2, (void *)&lTemp, 3 );
      if ( vProjInp->lLat_cut2 < 0 ) pLambert->achLat_cut2[0] |= 0x0080;

/* convert lLat_southpole to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(vProjInp->lLat_southpole))) >> 8;
#else
      lTemp = abs ( vProjInp->lLat_southpole ) << 8;
#endif
      memcpy ((void *) pLambert->achLat_southpole, (void *)&lTemp, 3 );
      if ( vProjInp->lLat_southpole < 0 ) 
		pLambert->achLat_southpole[0] |= 0x0080;

/* convert lLon_southpole to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(vProjInp->lLon_southpole))) >> 8;
#else
      lTemp = abs ( vProjInp->lLon_southpole ) << 8;
#endif
      memcpy ((void *) pLambert->achLon_southpole, (void *)&lTemp, 3 );
      if ( vProjInp->lLon_southpole < 0 )
         	pLambert->achLon_southpole[0] |= 0x0080;

#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(vProjInp->usZero)) >> 16;
#else
      lTemp = vProjInp->usZero << 16;      /* usZero is type INT */
#endif
      memcpy ((void *) pLambert->achZero, (void *)&lTemp, 2);

/*
*
* E.6       DEBUG print Grib LAMBERT block
*/
  DPRINT3("\t%s: achNx [%02d,%02d]\n", func,
  pLambert->achNx[0],pLambert->achNx[1]);
  DPRINT3("\t%s: achNy [%02d,%02d]\n", func, 
  pLambert->achNy[0],pLambert->achNy[1]);
  DPRINT4("\t%s: achLat1 [%02d,%02d,%02d]\n", func, 
  pLambert->achLat1[0], pLambert->achLat1[1], pLambert->achLat1[2]);
  DPRINT4("\t%s: achLon1 [%02d,%02d,%02d]\n", func,
  pLambert->achLon1[0], pLambert->achLon1[1], pLambert->achLon1[2]);
  DPRINT2("\t%s: chRes_flag [%02d]\n", func, pLambert->chRes_flag);
  DPRINT4("\t%s: achLon_orient [%02d,%02d,%02d]\n", func, 
  pLambert->achLon_orient[0], pLambert->achLon_orient[1], 
  pLambert->achLon_orient[2]);
  DPRINT4("\t%s: achDx [%02d,%02d,%02d]\n", func,
  pLambert->achDx[0], pLambert->achDx[1], pLambert->achDx[2]);
  DPRINT4("\t%s: achDy [%02d,%02d,%02d]\n", func, 
  pLambert->achDy[0], pLambert->achDy[1], pLambert->achDy[2]);
  DPRINT2("\t%s: chProj_flag [%02d]\n", func, pLambert->chProj_flag);
  DPRINT2("\t%s: chScan_mode [%02d]\n", func, pLambert->chScan_mode);
  DPRINT4("\t%s: achLat_cut1 [%02d,%02d,%02d]\n", func, 
  pLambert->achLat_cut1[0], 
  pLambert->achLat_cut1[1], pLambert->achLat_cut1[2]);
  DPRINT4("\t%s: achLat_cut2 [%02d,%02d,%02d]\n", func, 
  pLambert->achLat_cut2[0], 
  pLambert->achLat_cut2[1], pLambert->achLat_cut2[2]);
  DPRINT4("\t%s: achLat_southpole [%02d,%02d,%02d]\n",func,
  pLambert->achLat_southpole[0], 
  pLambert->achLat_southpole[1], pLambert->achLat_southpole[2] );
  DPRINT4("\t%s: achLon_southpole [%02d,%02d,%02d]\n",func,
  pLambert->achLon_southpole[0], 
  pLambert->achLon_southpole[1], pLambert->achLon_southpole[2] );
  DPRINT3("\t%s: achZero [%02d,%02d]\n", func, 
  pLambert->achZero[0], pLambert->achZero[1]);
/*******/

/*
*
* E.7       STORE proj size of LAMBERT struct in  lProj_size 
*/
      
      *lProj_size = sizeof (LAMBERT);

BYE:
      DPRINT3 ("   Exiting %s (lProj_size=%ld), stat=%d\n", func,
      *lProj_size, nStatus);
/*
*
* E.9       RETURN status
*/
   return ( nStatus );
/* 
* 
* END OF FUNCTION
*/ 
}

/*
*
****************************************************************************
* F. FUNCTION:  inp2grib_PolarSt
*      This routine fills the special Polar Stereo Projection structure for
*      the GDS.
*
*    INTERFACE:
*       int inp2grib_PolarSt ( ppvGDS_Proj_Input, Polar, lProj_size ,errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) void **ppvGDS_Proj_Input;  
*          holds input projection data
*      (O) POLAR *Polar;		
*          to be filled with Polar Stereographic projection info
*      (O) long *lProj_size;        
*          to be filled with size of structure POLAR 
*      (O) char *errmsg              
*          empty array, filled if error occurred
*
*   RETURN CODE:
*      0> success, Polar and lProj_size filled;
*      1> pointers are null, errmsg filled;
****************************************************************************/

#if PROTOTYPE_NEEDED
int   inp2grib_PolarSt  (void **ppvGDS_Proj_Input, POLAR *Polar, 
			long *lProj_size , char *errmsg)
#else
int   inp2grib_PolarSt  (ppvGDS_Proj_Input, Polar, lProj_size , errmsg)
			void **ppvGDS_Proj_Input; POLAR *Polar; 
			long *lProj_size ; char *errmsg;
#endif
{
/*
* 
* F.1       INIT variables !default stat=good
*/
   GDS_PS_INPUT      *pProjInp = 0;
   long              lTemp = 0;
   int               nStatus = 0;
   char		     *func="inp2grib_PolarSt";

   DPRINT1 ("\t Entering %s.....\n", func);
/*
*
* F.2       POINT local pProjInp to incoming ppvGDS_Proj_Input
*/
   pProjInp = ( GDS_PS_INPUT *) *ppvGDS_Proj_Input;

/*
*
* F.3       IF (true grib Polar proj block OR input Polar block is null) THEN
*               SET Status=  1 
*               RETURN;
*           ENDIF
*/
    if (!Polar || !pProjInp ) 
	{
        DPRINT1 ( "%s:  Polar or pProjInp is null\n", func);
        sprintf(errmsg,"%s:  Polar or pProjInp is null\n", func);
	nStatus=  1; goto BYE; 
	}

/*
*
* F.4        FILL local struct from pProjInp
*/
/* convert usNx to 2 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(pProjInp->usNx))) >> 16;
#else
      lTemp= abs (pProjInp->usNx) << 16;
#endif
      memcpy ((void*) Polar->achNx, (void*)&lTemp, 2);
   
/* convert usNy to 2 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(pProjInp->usNy))) >> 16;
#else
      lTemp= abs(pProjInp->usNy) << 16;
#endif
      memcpy ((void*) Polar->achNy, (void*)&lTemp, 2);

/* convert lLat1 to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(pProjInp->lLat1))) >> 8;
#else
      lTemp = abs ( pProjInp->lLat1 ) << 8;
#endif
      memcpy ((void *)  Polar->achLat1, (void *)&lTemp, 3 );
      if ( pProjInp->lLat1 < 0 ) Polar->achLat1[0] |= 0x0080;

/* convert lLon1 to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(pProjInp->lLon1))) >> 8;
#else
      lTemp = abs ( pProjInp->lLon1 ) << 8;
#endif
      memcpy ((void *) Polar->achLon1, (void *)&lTemp, 3 );
      if ( pProjInp->lLon1 < 0 ) Polar->achLon1[0] |= 0x0080;

      Polar->chRes_flag = ( unsigned char ) pProjInp->usRes_flag;

/* convert lLon_orient to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(pProjInp->lLon_orient))) >> 8;
#else
      lTemp = abs ( pProjInp->lLon_orient ) << 8;
#endif
      memcpy ((void *)  Polar->achLon_orient, (void *)&lTemp, 3 );
      if ( pProjInp->lLon_orient < 0 )  Polar->achLon_orient[0] |= 0x0080;

/* convert ulDx to 3 char */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(pProjInp->ulDx)) >> 8;
#else
      lTemp= ( pProjInp->ulDx << 8);
#endif
      memcpy ((void *)  Polar->achDx, (void *)&lTemp, 3 );
   
/* convert ulDy to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(pProjInp->ulDy)) >> 8;
#else
      lTemp=  ( pProjInp->ulDy << 8);
#endif
      memcpy ((void *)  Polar->achDy, (void *)&lTemp, 3 );

      Polar->chProj_flag = ( unsigned char ) pProjInp->usProj_flag;
      Polar->chScan_mode = ( unsigned char ) pProjInp->usScan_mode;

/* 4 bytes of zero */
      memset((void*) Polar->achZero, '\0', 4);

/* 
*
* F.5       DEBUG print GRIB Projection block
*/
  DPRINT3("\t%s: achNx [%02d,%02d]\n",func, Polar->achNx[0],Polar->achNx[1]);
  DPRINT3("\t%s: achNy [%02d,%02d]\n",func, Polar->achNy[0],Polar->achNy[1]);
  DPRINT4("\t%s: achLat1 [%02d,%02d,%02d]\n",func, Polar->achLat1[0],
  Polar->achLat1[1], Polar->achLat1[2]);
  DPRINT4("\t%s: achLon1 [%02d,%02d,%02d]\n",func, Polar->achLon1[0],
   Polar->achLon1[1] , Polar->achLon1[2]);
  DPRINT2("\t%s: chRes_flag [%02d]\n",func, Polar->chRes_flag);
  DPRINT4("\t%s: achLon_orient [%02d,%02d,%02d]\n",func, 
  Polar->achLon_orient[0], Polar->achLon_orient[1], Polar->achLon_orient[2]);
  DPRINT4("\t%s: achDx [%02d,%02d,%02d]\n",func, Polar->achDx[0],
  Polar->achDx[1], Polar->achDx[2]);
  DPRINT4("\t%s: achDy [%02d,%02d,%02d]\n",func, Polar->achDy[0],
  Polar->achDy[1], Polar->achDy[2]);
  DPRINT2("\t%s: chProj_flag [%02d]\n",func, Polar->chProj_flag);
  DPRINT2("\t%s: chScan_mode [%02d]\n",func, Polar->chScan_mode);
  DPRINT5("\t%s: achZero [%02d,%02d,%02d,%02d]\n",func, Polar->achZero[0],
  Polar->achZero[1],  Polar->achZero[2],  Polar->achZero[3]);
/*******/

/*
*
* F.7        STORE size of POLAR struct in lProj_size
*/
      *lProj_size = sizeof (POLAR);

BYE:
   DPRINT3 ("   Exiting %s (lProj_size=%ld), stat=%d\n", func,
      *lProj_size, nStatus);
/*
*
* F.8       RETURN Stat  ! 0 or  1
*/
   return ( nStatus );
/* 
* 
* END OF FUNCTION
*
*
*/ 
}

/*
*
****************************************************************************
* G. FUNCTION:  inp2grib_Latlon
*      This routine fills the Latitude Longitude Projection structure for
*      the GDS.
*
*    INTERFACE:
*       int inp2grib_Latlon ( ppvGDS_Proj_Input, pLatlon, lProj_size ,errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) void **ppvGDS_Proj_Input;  
*          holds input projection data
*      (O) LATLON *pLatlon;
*          to be filled with Lat/Lon projection info
*      (O) long *lProj_size;        
*          to be filled with size of structure LATLON
*      (O) char *errmsg;
*          empty array, filled if error occurred
*
*    RETURN CODE:
*      0>  success, pLatlon and lProj_size filled;
*      1>  got null pointers, errmsg filled;
****************************************************************************/
#if PROTOTYPE_NEEDED
int    inp2grib_Latlon  (void **ppvGDS_Proj_Input, LATLON *pLatlon,
			long *lProj_size, char *errmsg)
#else
int    inp2grib_Latlon  (ppvGDS_Proj_Input, pLatlon, lProj_size, errmsg)
			void **ppvGDS_Proj_Input; LATLON *pLatlon;
			long *lProj_size; char *errmsg;
#endif
{
   GDS_LATLON_INPUT  *Inp = 0;
   long              lTemp = 0;
   char 	*func= "inp2grib_Latlon";
/*
*
* G.1       INIT status to success
*/
   int               nStatus = 0;
   DPRINT1 ( "   Entering %s.....\n", func );
/*
*
* G.2        ASSIGN arguments to local pointers
*/
  Inp  = (GDS_LATLON_INPUT *) *ppvGDS_Proj_Input;
/*
   DPRINT3("\n%s: usData_type = %u (%s)\n", func, Inp->usData_type,
   prjn_name[Inp->usData_type] );
   DPRINT2("\t%s: usNi = %u\n",func, Inp->usNi );
   DPRINT2("\t%s: usNj = %u\n",func, Inp->usNj );
   DPRINT2("\t%s: lLat1 = %d\n",func, Inp->lLat1 );
   DPRINT2("\t%s: lLon1 = %d\n",func, Inp->lLon1 );
   DPRINT2("\t%s: lLat2 = %d\n",func, Inp->lLat2 );
   DPRINT2("\t%s: lLon2 = %d\n",func, Inp->lLon2 );
   DPRINT2("\t%s: iDi = %u\n",func, Inp->iDi );
   DPRINT2("\t%s: iDj = %u\n",func, Inp->iDj );
   DPRINT2("\t%s: usRes_flag = %u\n",func, Inp->usRes_flag );
   DPRINT2("\t%s: usScan_mode = %u\n",func, Inp->usScan_mode );
   DPRINT2("\t%s: lLat_southpole = %ld\n",func, Inp->lLat_southpole);
   DPRINT2("\t%s: lLon_southpole = %ld\n",func, Inp->lLon_southpole);
   DPRINT2("\t%s: lRotate = %ld\n",func, Inp->lRotate );
   DPRINT2("\t%s: lPole_lat = %ld\n",func, Inp->lPole_lat );
   DPRINT2("\t%s: lPole_lon = %ld\n",func, Inp->lPole_lon );
   DPRINT2("\t%s: lStretch = %ld\n",func, Inp->lStretch );
*/

/*
*
* G.3        IF (pointers passed in are null) THEN
*                SET status to  1
*                RETURN
*            ENDIF
*/
  if ( !Inp || !pLatlon) { 
	   DPRINT1 ("%s:  lLatlon_inp || pLatlon is null\n",func);
	   sprintf(errmsg, "%s:  lLatlon_inp || pLatlon is null\n", func);
	   nStatus =  1; 
	   goto BYE; 
	 }

/*
*
* G.4       FILL local struct from Inp
*/
/* convert usNi & usNj to 2 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(Inp->usNi)) >> 16;
#else
      lTemp= Inp->usNi << 16;
#endif
      memcpy ((void*) pLatlon->achNi, (void*)&lTemp, 2);

#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(Inp->usNj)) >> 16;
#else
      lTemp = Inp->usNj << 16;
#endif
      memcpy ((void*) pLatlon->achNj, (void*)&lTemp, 2);
   
/* convert lLat1 to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lLat1))) >> 8;
#else
      lTemp=  abs ( Inp->lLat1 )<< 8;
#endif
      memcpy ((void *) pLatlon->achLat1, (void *)&lTemp, 3 );
      if ( Inp->lLat1 < 0 ) pLatlon->achLat1[0] |= 0x0080;

/* convert lLon1 to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lLon1))) >> 8;
#else
      lTemp = abs ( Inp->lLon1 ) << 8;
#endif
      memcpy ((void *) pLatlon->achLon1, (void *)&lTemp, 3 );
      if ( Inp->lLon1 < 0 ) pLatlon->achLon1[0] |= 0x0080;

      pLatlon->chRes_flag = ( unsigned char ) Inp->usRes_flag;

/* convert lLat2 to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lLat2))) >> 8;
#else
      lTemp = abs ( Inp->lLat2 ) << 8;
#endif
      memcpy ((void *) pLatlon->achLat2, (void *)&lTemp, 3 );
      if ( Inp->lLat2 < 0 ) pLatlon->achLat2[0] |= 0x0080;

/* convert lLon2 to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lLon2))) >> 8;
#else
      lTemp = abs ( Inp->lLon2 ) << 8;
#endif
      memcpy ((void *) pLatlon->achLon2, (void *)&lTemp, 3 );
      if ( Inp->lLon2 < 0 ) pLatlon->achLon2[0] |= 0x0080;

/* convert lon increment to 2chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(Inp->iDi)) >> 16;
#else
      lTemp = Inp->iDi  << 16;
#endif
      memcpy ((void *) pLatlon->achDi, (void *)&lTemp, 2);

/* convert lat increment to 2chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(Inp->iDj)) >> 16;
#else
      lTemp = Inp->iDj << 16;
#endif
      memcpy ((void *)  pLatlon->achDj, (void *)&lTemp, 2);

/* 1 byte scan mode */
      pLatlon->chScan_mode = ( unsigned char ) Inp->usScan_mode;

/* 4 bytes of reserved zero */
      memset ((void*)pLatlon->achZero, '\0', 4);

/* convert lLat_southpole to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lLat_southpole))) >> 8;
#else
      lTemp = abs ( Inp->lLat_southpole ) << 8;
#endif
      memcpy ((void *) pLatlon->achLat_southpole, (void *)&lTemp, 3 );
      if ( Inp->lLat_southpole < 0) pLatlon->achLat_southpole[0] |=0x080;

/* convert lLon_southpole to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lLon_southpole))) >> 8;
#else
      lTemp = abs ( Inp->lLon_southpole ) << 8;
#endif
      memcpy ((void *)  pLatlon->achLon_southpole, (void *)&lTemp, 3 );
      if ( Inp->lLon_southpole < 0) pLatlon->achLon_southpole[0] |=0x080;

/* convert lRotate to 4chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lRotate)));
#else
      lTemp = abs ( Inp->lRotate );
#endif
      memcpy ((void *) pLatlon->achRotate, (void *)&lTemp, 4 );

/* convert lPole_lat to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lPole_lat))) >> 8;
#else
      lTemp = abs ( Inp->lPole_lat ) << 8;
#endif
      memcpy ((void *) pLatlon->achPole_lat, (void *)&lTemp, 3 );
      if (  Inp->lPole_lat < 0) pLatlon->achPole_lat[0] |= 0x0080;

/* convert lPole_lon to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(Inp->lPole_lon))) >> 8;
#else
      lTemp = abs ( Inp->lPole_lon ) << 8;
#endif
      memcpy ((void *) pLatlon->achPole_lon, (void *)&lTemp, 3 );
      if (  Inp->lPole_lon < 0) pLatlon->achPole_lon[0] |= 0x0080;

/* convert lStretch to 4 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(Inp->lStretch));
#else
      lTemp = Inp->lStretch;
#endif
      memcpy ((void *) pLatlon->achStretch, (void*)&lTemp, 4 );

/*
*
* G.5       DEBUG print Input Proj Block & their equivalence in the Char array;
*/
  DPRINT3("\t%s: achNi [%02d,%02d]\n",func,pLatlon->achNi[0],pLatlon->achNi[1]);
  DPRINT3("\t%s: achNj [%02d,%02d]\n",func,pLatlon->achNj[0],pLatlon->achNj[1]);
  DPRINT4("\t%s: achLat1 [%02d,%02d,%02d]\n", 
	func, pLatlon->achLat1[0],pLatlon->achLat1[1],pLatlon->achLat1[2]);
  DPRINT4("\t%s: achLon1 [%02d,%02d,%02d]\n", func, 
	pLatlon->achLon1[0],pLatlon->achLon1[1],pLatlon->achLon1[2]);
  DPRINT2("\t%s: chRes_flag [%02d]\n", func, pLatlon->chRes_flag	);
  DPRINT4("\t%s: achLat2 [%02d,%02d,%02d]\n", 
	func, pLatlon->achLat2[0], pLatlon->achLat2[1], pLatlon->achLat2[2]);
  DPRINT4("\t%s: achLon2 [%02d,%02d,%02d]\n", 
	func, pLatlon->achLon2[0], pLatlon->achLon2[1], pLatlon->achLon2[2]);
  DPRINT3("\t%s: achDi [%02d,%02d]\n",func,pLatlon->achDi[0],pLatlon->achDi[1]);
  DPRINT3("\t%s: achDj [%02d,%02d]\n",func,pLatlon->achDj[0],pLatlon->achDj[1]);
  DPRINT2("\t%s: chScan_mode [%02d]\n", func, pLatlon->chScan_mode);
  DPRINT5("\t%s: achZero [%02d,%02d,%02d,%02d]\n", 
	func, pLatlon->achZero[0],pLatlon->achZero[1],pLatlon->achZero[2],
	pLatlon->achZero[3]);
  DPRINT4("\t%s achLat_southpole [%02d,%02d,%02d]\n",
        func, pLatlon->achLat_southpole[0],pLatlon->achLat_southpole[1],
        pLatlon->achLat_southpole[2]);
  DPRINT4("\t%s achLon_southpole [%02d,%02d,%02d]\n",
        func, pLatlon->achLon_southpole[0],pLatlon->achLon_southpole[1],
        pLatlon->achLon_southpole[2]);
  DPRINT5("\t%s achRotate [%02d,%02d,%02d,%02d]\n",
        func, pLatlon->achRotate[0],pLatlon->achRotate[1],
        pLatlon->achRotate[2], pLatlon->achRotate[3]);
  DPRINT4("\t%s achPole_lat [%02d,%02d,%02d]\n",
        func, pLatlon->achPole_lat[0],pLatlon->achPole_lat[1],
        pLatlon->achPole_lat[2]);
  DPRINT4("\t%s achPole_lon [%02d,%02d,%02d]\n",
        func, pLatlon->achPole_lon[0],pLatlon->achPole_lon[1],
        pLatlon->achPole_lon[2]);
  DPRINT5("\t%s achStretch [%02d,%02d,%02d,%02d]\n",
    	func, pLatlon->achStretch[0],pLatlon->achStretch[1],
	pLatlon->achStretch[2], pLatlon->achStretch[3]);
/*******/
/*
*
* G.6       STORE size of LATLON struct in lProj_size
*/
      *lProj_size = sizeof (LATLON);


BYE:
   DPRINT3 ("   Exiting %s (lProj_size=%ld), stat=%d\n", func,
      *lProj_size, nStatus);
/*
*
* G.7       RETURN stat 
*/
   return ( nStatus );
/* 
* 
* END OF FUNCTION
*
*
*/ 
}
/*
*
****************************************************************************
* F. FUNCTION:  inp2grib_Mercator
*      This routine fills the special Mercator Projection structure for
*      the GDS.
*
*    INTERFACE:
*       int inp2grib_Mercator ( ppvGDS_Proj_Input, Polar, lProj_size ,errmsg)
*
*    ARGUMENTS (I=input, O=output, I&O=input and output):
*      (I) void **ppvGDS_Proj_Input;  
*          holds input projection data
*      (O) MERCATOR *Mercator;		
*          to be filled with Polar Stereographic projection info
*      (O) long *lProj_size;        
*          to be filled with size of structure POLAR 
*      (O) char *errmsg              
*          empty array, filled if error occurred
*
*   RETURN CODE:
*      0> success, Mercator and lProj_size filled;
*      1> pointers are null, errmsg filled;
****************************************************************************/

#if PROTOTYPE_NEEDED
int   inp2grib_Mercator (void **ppvGDS_Proj_Input, MERCATOR *Mercator, 
			long *lProj_size , char *errmsg)
#else
int   inp2grib_Mercator (ppvGDS_Proj_Input, Mercator, lProj_size , errmsg)
			void **ppvGDS_Proj_Input; MERCATOR *Mercator; 
			long *lProj_size ; char *errmsg;
#endif
{
/*
* 
* F.1       INIT variables !default stat=good
*/
   mercator          *ProjInp = 0;
   long              lTemp = 0;
   int               nStatus = 0;
   char		     *func="inp2grib_PolarSt";

   DPRINT1 ("\t Entering %s.....\n", func);
/*
*
* F.2       POINT local pProjInp to incoming ppvGDS_Proj_Input
*/
   ProjInp = ( mercator *) *ppvGDS_Proj_Input;

/*
*
* F.3       IF (true grib Mercator proj block OR input Polar block is null) THEN
*               SET Status=  1 
*               RETURN;
*           ENDIF
*/
    if (!Mercator || !ProjInp ) 
	{
        DPRINT1 ( "%s:  Mercator or ProjInp is null\n", func);
        sprintf(errmsg,"%s: Mercator or ProjInp is null\n", func);
	nStatus=  1; goto BYE; 
	}

/*
*
* F.4        FILL local struct from pProjInp
*/
/* convert cols to 2 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(ProjInp->cols))) >> 16;
#else
      lTemp= abs (ProjInp->cols) << 16;
#endif
      memcpy ((void*) Mercator->achNi, (void*)&lTemp, 2);
   
/* convert rows to 2 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(ProjInp->rows))) >> 16;
#else
      lTemp= abs(ProjInp->rows) << 16;
#endif
      memcpy ((void*) Mercator->achNj, (void*)&lTemp, 2);

/* convert first_lat to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(ProjInp->first_lat))) >> 8;
#else
      lTemp = abs ( ProjInp->first_lat ) << 8;
#endif
      memcpy ((void *)  Mercator->achLat1, (void *)&lTemp, 3 );
      if ( ProjInp->first_lat < 0 ) Mercator->achLat1[0] |= 0x0080;

/* convert first_lon to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(ProjInp->first_lon))) >> 8;
#else
      lTemp = abs ( ProjInp->first_lon ) << 8;
#endif
      memcpy ((void *) Mercator->achLon1, (void *)&lTemp, 3 );
      if ( ProjInp->first_lon < 0 ) Mercator->achLon1[0] |= 0x0080;

      Mercator->chRes_flag = ( unsigned char ) ProjInp->usRes_flag;

/* convert La2 to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(ProjInp->La2))) >> 8;
#else
      lTemp = abs ( ProjInp->La2 ) << 8;
#endif
      memcpy ((void *)  Mercator->achLat2, (void *)&lTemp, 3 );
      if ( ProjInp->La2 < 0 ) Mercator->achLat2[0] |= 0x0080;

/* convert Lo2 to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(ProjInp->Lo2))) >> 8;
#else
      lTemp = abs ( ProjInp->Lo2 ) << 8;
#endif
      memcpy ((void *) Mercator->achLon2, (void *)&lTemp, 3 );
      if ( ProjInp->Lo2 < 0 ) Mercator->achLon2[0] |= 0x0080;

/* convert lLon_orient to 3 chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl(abs(ProjInp->latin))) >> 8;
#else
      lTemp = abs ( ProjInp->latin ) << 8;
#endif
      memcpy ((void *)  Mercator->achLatin, (void *)&lTemp, 3 );
      if ( ProjInp->latin < 0 )  Mercator->achLatin[0] |= 0x0080;

/* convert zero fill */
      Mercator->achZero1 = ( unsigned char ) ProjInp->usZero1;

      Mercator->chScan_mode = ( unsigned char ) ProjInp->usScan_mode;

/* convert ulDx to 3 char */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl((long)ProjInp->lon_inc)) >> 8;
#else
      lTemp= ( (long)ProjInp->lon_inc << 8);
#endif
      memcpy ((void *)  Mercator->achDi, (void *)&lTemp, 3 );

/* convert ulDy to 3chars */
#if ( BYTE_ORDER == LITTLE_ENDIAN )
      lTemp = (htonl((long)ProjInp->lat_inc)) >> 8;
#else
      lTemp=  ( (long)ProjInp->lat_inc << 8);
#endif
      memcpy ((void *)  Mercator->achDj, (void *)&lTemp, 3 );

      Mercator->chScan_mode = ( unsigned char ) ProjInp->usScan_mode;

/* 8 bytes of zero */
      memset((void*) Mercator->achZero2, '\0', 8);

/* 
*
* F.5       DEBUG print GRIB Projection block
*/
  DPRINT3("\t%s: achNi [%02d,%02d]\n",func,Mercator->achNi[0],Mercator->achNi[1]);
  DPRINT3("\t%s: achNj [%02d,%02d]\n",func, Mercator->achNj[0],Mercator->achNj[1]);
  DPRINT4("\t%s: achLat1 [%02d,%02d,%02d]\n",func, Mercator->achLat1[0],
  Mercator->achLat1[1], Mercator->achLat1[2]);
  DPRINT4("\t%s: achLon1 [%02d,%02d,%02d]\n",func, Mercator->achLon1[0],
   Mercator->achLon1[1] , Mercator->achLon1[2]);
  DPRINT2("\t%s: chRes_flag [%02d]\n",func, Mercator->chRes_flag);
  DPRINT4("\t%s: achLatint [%02d,%02d,%02d]\n",func, 
  Mercator->achLatin[0], Mercator->achLatin[1], Mercator->achLatin[2]);
  DPRINT4("\t%s: achDi [%02d,%02d,%02d]\n",func, Mercator->achDi[0],
  Mercator->achDi[1], Mercator->achDi[2]);
  DPRINT4("\t%s: achDj [%02d,%02d,%02d]\n",func, Mercator->achDj[0],
  Mercator->achDj[1], Mercator->achDj[2]);
  DPRINT5("\t%s: achZero2 [%02d,%02d,%02d,%02d]\n",func, Mercator->achZero2[0],
  Mercator->achZero2[1],  Mercator->achZero2[2],  Mercator->achZero2[3]);
/*******/

/*
*
* F.7        STORE size of POLAR struct in lProj_size
*/
      *lProj_size = sizeof (MERCATOR);

BYE:
   DPRINT3 ("   Exiting %s (lProj_size=%ld), stat=%d\n", func,
      *lProj_size, nStatus);
/*
*
* F.8       RETURN Stat  ! 0 or  1
*/
   return ( nStatus );
/* 
* 
* END OF FUNCTION
*
*
*/ 
}
/*
Old round--this is different from standard gnu round (gnu round returns a 
float).  Depending on compile options, sometimes gnu round was used, other
times this function was used.  Removed and replaced by lrint by T. Hutchinson,
WSI.  4/14/05.
long round(double value)
{
  long retval;
  retval=lrint(value);
  return retval;
}
*/
