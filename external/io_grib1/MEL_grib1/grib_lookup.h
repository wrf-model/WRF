/* FILE:  grib_lookup.h   
 Decoder Lookup Table (ie. g1tab_128_2.2) shows how to convert from GRIB
 units to DB units;
 Encoder Lookup Table (ie. neons2grib.2.2) shows how to convert from DB
 units to GRIB units;

Revisions:
12/96 A.Nakajima +debug print
07/97 atn: +LPRINT, +List_xxx;
...............................................................*/
#include <stdio.h>
#include <limits.h>
#include <float.h>

#ifndef  TABLES_H
#define  TABLES_H
#define  ADD_DEC   	1L
#define  ADD_ENC   	2L
#define  NEW_ENTRY 	3L
#define  NO_NUM	   	INT_MAX	/* +32767 */
#define  NO_FNUM   	FLT_MAX   	/*  1E+37 */
#define  LPRINT 	if (logfile!=NULL) fprintf

enum { 
   NOTFOUND, FOUND, NULL_OK, NO_NULL, NO_SPACE, SPACE_OK, YES, NO, 
   OK, SKIP, ABORT 
   };

/* 
*   Use this to index the db_tbl_name[] array, so any changes would
*   require updating db_tbl_name[] also.  
*  To add new Parm tables, insert it before the Model_Type;
*/
enum struct_types {
    	Parm0_Type=0, ParmA_Type, ParmB_Type,ParmC_Type, ParmD_Type, ParmE_Type,
    	Model_Type, Level_Type, Geom_Type
  };
#define  MAX_PARM_TBLS   	(ParmE_Type-Parm0_Type + 1)

static char *db_tbl_name[]= {
    	"PARMTBL-0","PARMTBL-A","PARMTBL-B","PARMTBL-C","PARMTBL-D","PARMTBL-E",
    	"MODEL-TBL","LEVEL-TBL","GEOM-TBL"
   };

#define  NPARM 	   256
#define  NLEV 	   256
#define  NGEOM 	   256
#define  NMODEL    256
#define  NOCTR 	   256
#define  NCTRS     256

#define  EMPTY_PARM(x)  	(!(x)->grib_dsc[0] && !(x)->db_name[0])
#define  EMPTY_LEVEL(x) 	(!(x)->grib_dsc[0]  && !(x)->db_name[0])
#define  EMPTY_MODEL(x) 	(!(x)->grib_dsc[0] && !(x)->db_name[0])
#define  EMPTY_GEOM(x)  	(!(x)->grib_dsc[0] && !(x)->db_name[0])

#define  PARMTBL_INDX(Parm_id, Parm_sub)   \
	(Parm_id>249 && Parm_sub!=0 ? ((Parm_id-249)*NPARM+Parm_sub):Parm_id)
#define  LIST_GEOM(unit,cell)  	\
	 fprintf((unit),"Geom=%03d:\n    grib_dsc='%s'\n    db_name='%s'\n\n",\
	 	(cell)->usGeom_id, (cell)->grib_dsc, (cell)->db_name);
#define  LIST_PARM(unit,curr_Type,cell)	\
	 fprintf ((unit),						\
     		"%s  Parm_id=%d,  Parm_sub=%d  (Index= %d):\n" 		\
     		"    Descr='%s' Unit='%s'\n"				\
     		"    DBName='%s' Scl=%.3f Offs=%.3f DSF=%d\n\n",	\
		db_tbl_name[curr_Type],					\
     		(cell)->usParm_id,  (cell)->usParm_sub,			\
     		PARMTBL_INDX ((cell)->usParm_id,(cell)->usParm_sub),	\
     		(cell)->grib_dsc, (cell)->grib_unit_dsc, (cell)->db_name, \
     		(cell)->fScale, (cell)->fOffset, (cell)->sDSF);
#define  LIST_LVL(unit,cell)		\
	fprintf ((unit), 						\
		"Level=%03d: '%s' %d octs\n   name1='%s'\n"		\
		"   name2='%s'\n   db_name='%s' Scl=%.3f Offs=%.3f\n\n",  \
        	(cell)->usLevel_id, (cell)->grib_dsc, (cell)->num_octets, \
		(cell)->lvl_name_1, (cell)->lvl_name_2, (cell)->db_name, \
		(cell)->fScale, (cell)->fOffset);
#define  LIST_MODEL(unit,cell)		\
	fprintf ((unit),		\
		"Model=%03d:\n    grib_dscr='%s'\n    db_name='%s'\n\n",\
   		(cell)->usModel_id, (cell)->grib_dsc, (cell)->db_name);

/******************************************************************
 The following structs hold Parameter, Level, Model and Geom info;
 They are loaded from the external 'lookup tables';
 The following structs are used as ARRAY of structures, where # elements
 depends on what type of structs they are (usually 256 as defined in 
 the # defines lines above;
 ******************************************************************/

typedef struct parm_defn  { /****  PARAMETER:   GRIB vs. Neons */
     /* for a given ParmId & ParmSub, get Index within Db_Parm_Tbl
	via PARMTBL_INDX(Parm_id, Parm_sub)
	Index 249 through 255 denote Sub-Tables Defs only when Parm_sub > 0;
	In that case usParm_id tells which Sub-Tbl we're in and usParm_sub
	tells which element of that Table to access;
	SubTbl-A  usParm_id 249  usParm_sub

	and
        Db_Parm_Tbl[0-249,255],
		{  usParm_id = usGribCode (1-255); SubParm=0; }
        Db_Parm_Tbl[250-254],
		{  usParm_id = 249/250/251/252/253/254; SubParm= usGribCode; }
        for Main Parameter defns (Db_Parm_Tbl[0-255],
		{  usParm_id= usGribCode (1-255); SubParm=0; }
	Sub-Tbls A/B/C/D/E defns (Db_Parm_Tbl[256-511], [512-767] ...)
		{  usParm_id= 250/251/252/253/254; Parmsub= usGribCode(1-255);}
	where usGribCode is between 0-255 (DECODR: 1st col, ENCODR= 3rd col)
     */

  unsigned short usParm_id; /* see above */
  unsigned short usParm_sub;/* see above */

  char   grib_dsc[75];      /* DECODR: field parameter -   2nd col */
  char   grib_unit_dsc[25]; /* DECODR: units -             3rd col */

  char   db_name[31];       /* ENCODR: neons field name    1st col */
  /*char   chTable_code;    /- ENCODR: 0/a/b/c/d/e table 2nd col   */
  float  fScale;  	    /* ENCODR: binary Scale Fctor  4th col */
  float  fOffset;           /* ENCODR: Unit offset         5th col */
  short  sDSF;              /* ENCODR: Decimal scalefactor 6th col */
} PARM_DEFN;


typedef struct lvl_defn {   /****  LEVEL:  GRIB vs. Neons  */
  unsigned short    usLevel_id;	  /* DECDR: line1 col1 &  ENCDR:  2nd col  */
  char   grib_dsc[100];      /* DECDR: meaning of code figure line1: 2nd col*/
  int    num_octets;         /* DECDR: #octets for contents- line1: 3rd col*/
  char   lvl_name_1[100];    /* DECDR: contents of octets 11 & 12  : line2 */
  char   lvl_name_2[100];    /* DECDR: contents of octets 11 & 12  : line3 */

  char   db_name[31];        /* ENCODR: db db_name      1st col */
  float  fScale;  	    /* ENCODR: binary Scale Fctor  3rd col */
  float  fOffset;           /* ENCODR: offset              4th col */
}  LVL_DEFN;

typedef struct mdl_defn { /****  MODEL:    GRIB vs. Neons */
  unsigned short  usModel_id;/* ENCDR & DECDR: 1st col */
  char grib_dsc[61];         /* DECDR:   2nd col */
  char db_name[31];          /* ENCDR:   2nd col */
} MODEL_DEFN;

typedef struct geom_defn {   /***  GEOM :   Grib vs. Neons ****/
  unsigned short  usGeom_id; /* ENCDR & DECODR TBL: 1st col */
  char grib_dsc[61];         /* DECODR LOOKUP TBL:   2nd col */
  char db_name [31];         /* ENCDR LOOKUP TBL:   2nd col */
} GEOM_DEFN;
/*................................................................*/

typedef struct  ctr_defn {  /**** ORIGINATING CENTERS INFO *****/
  char ctr_dsc[100];	    /* orig_ctrs:  description of center */
}  CTR_DEFN;
#endif
